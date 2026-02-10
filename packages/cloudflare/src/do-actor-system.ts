/**
 * DOActorSystem — Abstract Durable Object base class wrapping ActorSystem
 *
 * Maps Durable Object lifecycle to ActorSystem:
 * - constructor: Creates ActorSystem + DOActorCheckpoint + WebSocketBridge
 * - fetch: Routes WebSocket upgrades and HTTP actor messages
 * - alarm: Delivers scheduled messages to actors
 * - webSocketMessage/webSocketClose: Delegates to WebSocketBridge
 *
 * Subclasses implement configure() to spawn domain actors and register transports.
 *
 * Usage:
 * ```typescript
 * export class BrainDO extends DOActorSystem<Env> {
 *   configure(system: ActorSystem) {
 *     system.spawn(triageBehavior, initialState, 'triage');
 *     this.scheduleAlarm({
 *       targetActor: 'briefing',
 *       messageType: 'GENERATE',
 *       nextRunAt: getNextBriefingTime(),
 *       interval: 24 * 60 * 60 * 1000,
 *     });
 *   }
 * }
 * ```
 */

import { ActorSystem, address, type ActorSystemConfig } from '@agentic-primer/actors';
import { DOActorCheckpoint } from './do-actor-checkpoint.ts';
import { WebSocketBridge } from './transports/websocket-bridge.ts';
import type { AlarmSchedule } from './types.ts';

/** Configuration for DOActorSystem */
export interface DOActorSystemConfig {
  /** Name for the ActorSystem instance */
  name: string;
  /** Optional ActorSystem configuration overrides */
  systemConfig?: Partial<ActorSystemConfig>;
}

/** Storage key for persisting alarm schedules in DO SQLite */
const ALARM_SCHEDULE_KEY = '_alarm_schedule';

export abstract class DOActorSystem<Env = unknown> implements DurableObject {
  protected readonly ctx: DurableObjectState;
  protected readonly env: Env;
  protected readonly actorSystem: ActorSystem;
  protected readonly checkpoint: DOActorCheckpoint;
  protected readonly wsBridge: WebSocketBridge;

  constructor(ctx: DurableObjectState, env: Env) {
    this.ctx = ctx;
    this.env = env;

    // Create actor system with DO name
    const config = this.getConfig();
    this.actorSystem = new ActorSystem({
      name: config.name,
      ...config.systemConfig,
    });

    // Set up DO SQLite actor checkpoint
    this.checkpoint = new DOActorCheckpoint(ctx.storage);

    // Set up WebSocket bridge
    this.wsBridge = new WebSocketBridge(ctx, this.actorSystem);

    // Initialize inside blockConcurrencyWhile to ensure
    // schema + actors are ready before handling requests
    ctx.blockConcurrencyWhile(async () => {
      await this.checkpoint.initialize();
      await this.configure(this.actorSystem);
    });
  }

  /**
   * Subclass hook: return configuration for this DOActorSystem.
   * Override to customize the system name and config.
   * Default implementation uses the class name as the system name.
   */
  protected getConfig(): DOActorSystemConfig {
    return { name: this.constructor.name };
  }

  /**
   * Subclass hook: configure the ActorSystem.
   * Spawn actors, register transports, schedule alarms, etc.
   */
  protected abstract configure(system: ActorSystem): void | Promise<void>;

  /**
   * Handle incoming HTTP requests.
   * - WebSocket upgrade: delegate to WebSocketBridge
   * - POST /actor-message: deserialize and route to ActorSystem
   */
  async fetch(request: Request): Promise<Response> {
    // WebSocket upgrade
    const upgradeHeader = request.headers.get('Upgrade');
    if (upgradeHeader === 'websocket') {
      return this.wsBridge.handleUpgrade(request);
    }

    // Actor message via HTTP
    const url = new URL(request.url);
    if (request.method === 'POST' && url.pathname === '/actor-message') {
      try {
        const data = await request.json() as Record<string, unknown>;

        if (data.to && data.type) {
          const to = data.to as string;
          const type = data.type as string;
          const payload = data.payload;

          this.actorSystem.send(address(to), type, payload);

          return new Response(JSON.stringify({ ok: true }), {
            status: 200,
            headers: { 'Content-Type': 'application/json' },
          });
        }

        return new Response(
          JSON.stringify({ error: 'Missing required fields: to, type' }),
          { status: 400, headers: { 'Content-Type': 'application/json' } }
        );
      } catch (error) {
        const message = error instanceof Error ? error.message : 'Unknown error';
        return new Response(
          JSON.stringify({ error: message }),
          { status: 500, headers: { 'Content-Type': 'application/json' } }
        );
      }
    }

    return new Response('Not Found', { status: 404 });
  }

  /**
   * Handle DO alarm.
   * Looks up the stored alarm schedule, delivers the message to the target actor,
   * and reschedules if the alarm is recurring.
   */
  async alarm(): Promise<void> {
    const schedule = this.getAlarmSchedule();
    if (!schedule) {
      return;
    }

    // Deliver the scheduled message
    this.actorSystem.send(
      address(schedule.targetActor),
      schedule.messageType,
      schedule.payload
    );

    // Reschedule if recurring
    if (schedule.interval) {
      const nextRunAt = Date.now() + schedule.interval;
      await this.scheduleAlarm({
        ...schedule,
        nextRunAt,
      });
    } else {
      // One-shot alarm: clear the schedule
      this.clearAlarmSchedule();
    }
  }

  /**
   * Handle incoming WebSocket message (Hibernatable WebSocket API).
   */
  async webSocketMessage(ws: WebSocket, message: string | ArrayBuffer): Promise<void> {
    this.wsBridge.handleMessage(ws, message);
  }

  /**
   * Handle WebSocket close (Hibernatable WebSocket API).
   */
  async webSocketClose(ws: WebSocket, _code: number, _reason: string, _wasClean: boolean): Promise<void> {
    this.wsBridge.handleClose(ws);
  }

  // --- Alarm Scheduling ---

  /**
   * Schedule an alarm that will deliver a message to an actor.
   * Stores the schedule in DO SQLite and sets the DO alarm.
   */
  protected async scheduleAlarm(schedule: AlarmSchedule): Promise<void> {
    this.saveAlarmSchedule(schedule);
    await this.ctx.storage.setAlarm(schedule.nextRunAt);
  }

  /**
   * Get the currently stored alarm schedule.
   */
  private getAlarmSchedule(): AlarmSchedule | null {
    const cursor = this.ctx.storage.sql.exec(
      'SELECT data FROM _actor_snapshots WHERE key = ?',
      ALARM_SCHEDULE_KEY
    );
    const rows = [...cursor];
    if (rows.length === 0) {
      return null;
    }
    try {
      const text = new TextDecoder().decode(rows[0].data as ArrayBuffer);
      return JSON.parse(text) as AlarmSchedule;
    } catch {
      return null;
    }
  }

  /**
   * Save an alarm schedule to DO SQLite.
   */
  private saveAlarmSchedule(schedule: AlarmSchedule): void {
    const data = new TextEncoder().encode(JSON.stringify(schedule));
    const buffer = data.buffer.slice(
      data.byteOffset,
      data.byteOffset + data.byteLength
    );
    this.ctx.storage.sql.exec(
      'INSERT OR REPLACE INTO _actor_snapshots (key, data) VALUES (?, ?)',
      ALARM_SCHEDULE_KEY,
      buffer
    );
  }

  /**
   * Clear the stored alarm schedule.
   */
  private clearAlarmSchedule(): void {
    this.ctx.storage.sql.exec(
      'DELETE FROM _actor_snapshots WHERE key = ?',
      ALARM_SCHEDULE_KEY
    );
  }
}
