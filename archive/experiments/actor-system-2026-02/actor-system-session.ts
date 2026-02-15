// =============================================================================
// Multi-Actor System with SessionActor (WebSocket Support)
// Combines existing CoordinatorActor and WorkerActor with new SessionActor
// =============================================================================

import { DurableObject } from "cloudflare:workers";
import { MessageTypes } from "./shared/messages.js";

// =============================================================================
// MESSAGE PROTOCOLS
// =============================================================================

interface Message {
  id: string;
  type: string;
  from?: string;
  to?: string;
  payload?: unknown;
  timestamp?: number;
}

type CoordinatorMessageType =
  | typeof MessageTypes.REGISTER_WORKER
  | typeof MessageTypes.ASSIGN_TASK
  | typeof MessageTypes.TASK_COMPLETE
  | typeof MessageTypes.GET_STATUS;

interface CoordinatorMessage extends Message {
  type: CoordinatorMessageType;
}

type WorkerMessageType =
  | typeof MessageTypes.PROCESS_TASK
  | typeof MessageTypes.REPORT_PROGRESS
  | typeof MessageTypes.GET_STATE;

interface WorkerMessage extends Message {
  type: WorkerMessageType;
  taskId?: string;
  progress?: number;
}

// =============================================================================
// COORDINATOR ACTOR (from actor-system.ts)
// =============================================================================

export class CoordinatorActor extends DurableObject {
  private workers: Set<string> = new Set();
  private taskQueue: string[] = [];
  private activeTasks: Map<string, string> = new Map();

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);

    this.ctx.blockConcurrencyWhile(async () => {
      const state = await this.ctx.storage.get<{
        workers: string[];
        taskQueue: string[];
        activeTasks: [string, string][];
      }>("state");

      if (state) {
        this.workers = new Set(state.workers);
        this.taskQueue = state.taskQueue;
        this.activeTasks = new Map(state.activeTasks);
      }
    });
  }

  async handleMessage(message: CoordinatorMessage): Promise<unknown> {
    switch (message.type) {
      case MessageTypes.REGISTER_WORKER: {
        const workerId = message.from!;
        this.workers.add(workerId);
        await this.persist();

        if (this.taskQueue.length > 0) {
          const taskId = this.taskQueue.shift()!;
          await this.assignTask(taskId, workerId);
        }

        return { status: "registered", workerId };
      }

      case MessageTypes.ASSIGN_TASK: {
        const taskId = message.payload as string;

        const availableWorker = this.findAvailableWorker();
        if (availableWorker) {
          // Assign immediately if worker is available
          await this.assignTask(taskId, availableWorker);
          return { status: "assigned", taskId, workerId: availableWorker };
        } else {
          // Queue the task if no worker is available
          this.taskQueue.push(taskId);
          await this.persist();
          return { status: "queued", taskId, position: this.taskQueue.length };
        }
      }

      case MessageTypes.TASK_COMPLETE: {
        const { taskId, workerId } = message.payload as {
          taskId: string;
          workerId: string;
        };
        this.activeTasks.delete(taskId);
        await this.persist();

        if (this.taskQueue.length > 0) {
          const nextTask = this.taskQueue.shift()!;
          await this.assignTask(nextTask, workerId);
        }

        return { status: "completed", taskId };
      }

      case MessageTypes.GET_STATUS: {
        return {
          workers: Array.from(this.workers),
          queuedTasks: this.taskQueue.length,
          activeTasks: Array.from(this.activeTasks.entries()),
        };
      }

      default:
        return { error: "Unknown message type" };
    }
  }

  private findAvailableWorker(): string | null {
    const busyWorkers = new Set(this.activeTasks.values());
    for (const workerId of this.workers) {
      if (!busyWorkers.has(workerId)) {
        return workerId;
      }
    }
    return null;
  }

  private async assignTask(taskId: string, workerId: string) {
    this.activeTasks.set(taskId, workerId);
    await this.persist();

    const workerStub = this.env.WORKER_ACTOR.get(
      this.env.WORKER_ACTOR.idFromName(workerId)
    );

    // Don't await - send the message and let it process asynchronously
    // The worker will call TASK_COMPLETE when done
    const promise = workerStub.handleMessage({
      id: crypto.randomUUID(),
      type: MessageTypes.PROCESS_TASK,
      from: "coordinator",
      to: workerId,
      taskId,
      timestamp: Date.now(),
    });

    // Ensure the promise completes even if assignTask() finishes first
    this.ctx.waitUntil(promise);
  }

  private async persist() {
    await this.ctx.storage.put("state", {
      workers: Array.from(this.workers),
      taskQueue: this.taskQueue,
      activeTasks: Array.from(this.activeTasks.entries()),
    });
  }
}

// =============================================================================
// WORKER ACTOR (from actor-system.ts)
// =============================================================================

export class WorkerActor extends DurableObject {
  private workerId: string;
  private currentTask: string | null = null;
  private processedCount: number = 0;

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    this.workerId = ctx.id.toString();

    this.ctx.blockConcurrencyWhile(async () => {
      const state = await this.ctx.storage.get<{
        currentTask: string | null;
        processedCount: number;
      }>("state");

      if (state) {
        this.currentTask = state.currentTask;
        this.processedCount = state.processedCount;
      }
    });
  }

  async handleMessage(message: WorkerMessage): Promise<unknown> {
    switch (message.type) {
      case MessageTypes.PROCESS_TASK: {
        const taskId = message.taskId!;
        this.currentTask = taskId;
        await this.persist();

        await this.processTask(taskId);

        this.currentTask = null;
        this.processedCount++;
        await this.persist();

        const coordinatorStub = this.env.COORDINATOR_ACTOR.get(
          this.env.COORDINATOR_ACTOR.idFromName("main")
        );

        await coordinatorStub.handleMessage({
          id: crypto.randomUUID(),
          type: MessageTypes.TASK_COMPLETE,
          from: this.workerId,
          payload: { taskId, workerId: this.workerId },
          timestamp: Date.now(),
        });

        return { status: "completed", taskId };
      }

      case MessageTypes.GET_STATE: {
        return {
          workerId: this.workerId,
          currentTask: this.currentTask,
          processedCount: this.processedCount,
        };
      }

      default:
        return { error: "Unknown message type" };
    }
  }

  private async processTask(taskId: string): Promise<void> {
    await new Promise((resolve) => setTimeout(resolve, 1000));
    console.log(`Worker ${this.workerId} processed task ${taskId}`);
  }

  private async persist() {
    await this.ctx.storage.put("state", {
      currentTask: this.currentTask,
      processedCount: this.processedCount,
    });
  }
}

// =============================================================================
// SESSION ACTOR (WebSocket Handler)
// =============================================================================

interface WireMessage {
  to: string;
  from: string;
  type: string;
  payload: unknown;
  id: string;
  timestamp: number;
  replyTo?: string;
}

export class SessionActor extends DurableObject {
  private sessionId: string;

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    this.sessionId = ctx.id.toString();
  }

  async fetch(request: Request): Promise<Response> {
    if (request.headers.get("Upgrade") !== "websocket") {
      return new Response("Expected WebSocket", { status: 400 });
    }

    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);

    // Accept WebSocket and let Cloudflare manage it via hibernation API
    // No need to manually track this.ws - use ctx.getWebSockets() instead
    this.ctx.acceptWebSocket(server);

    // Send SessionEstablished message to the specific connection
    // Pass the server WebSocket directly to ensure it's sent immediately
    this.sendToClient({
      type: MessageTypes.SESSION_ESTABLISHED,
      payload: { sessionId: this.sessionId },
    }, server);

    return new Response(null, {
      status: 101,
      webSocket: client,
    });
  }

  async webSocketMessage(ws: WebSocket, data: string | ArrayBuffer) {
    if (typeof data !== "string") {
      console.warn("SessionActor: Received non-text message, ignoring");
      return;
    }

    try {
      const wireMsg: WireMessage = JSON.parse(data);
      await this.receiveFromClient(wireMsg, ws);
    } catch (error) {
      console.error("SessionActor: Failed to parse message", error);
      this.sendToClient({
        type: MessageTypes.ERROR,
        payload: { error: "Invalid message format" },
      }, ws);
    }
  }

  async webSocketClose(ws: WebSocket, code: number, reason: string, wasClean: boolean) {
    console.log(`SessionActor: WebSocket closed (code: ${code}, reason: ${reason}, clean: ${wasClean})`);
    // Hibernation API manages WebSocket lifecycle - no manual cleanup needed
  }

  async webSocketError(ws: WebSocket, error: unknown) {
    console.error("SessionActor: WebSocket error", error);
    // Hibernation API manages WebSocket lifecycle - no manual cleanup needed
  }

  private async receiveFromClient(wireMsg: WireMessage, ws: WebSocket) {
    const [actorType, actorId] = wireMsg.to.split(":");

    if (!actorType || !actorId) {
      this.sendToClient({
        type: MessageTypes.ERROR,
        payload: { error: `Invalid 'to' field: ${wireMsg.to}` },
        replyTo: wireMsg.id,
      }, ws);
      return;
    }

    try {
      switch (actorType) {
        case "coordinator": {
          await this.routeToCoordinator(wireMsg, actorId, ws);
          break;
        }

        case "worker": {
          await this.routeToWorker(wireMsg, actorId, ws);
          break;
        }

        default: {
          this.sendToClient({
            type: MessageTypes.ERROR,
            payload: { error: `Unknown actor type: ${actorType}` },
            replyTo: wireMsg.id,
          }, ws);
        }
      }
    } catch (error) {
      console.error(`SessionActor: Error routing message to ${wireMsg.to}`, error);
      this.sendToClient({
        type: MessageTypes.ERROR,
        payload: {
          error: error instanceof Error ? error.message : "Unknown error"
        },
        replyTo: wireMsg.id,
      }, ws);
    }
  }

  private async routeToCoordinator(wireMsg: WireMessage, actorId: string, ws: WebSocket) {
    const stub = this.env.COORDINATOR_ACTOR.get(
      this.env.COORDINATOR_ACTOR.idFromName(actorId)
    );

    // For register_worker, preserve the original 'from' field as it's used as workerId
    // For other messages, use session ID
    const isRegisterWorker = wireMsg.type === MessageTypes.REGISTER_WORKER;
    const fromField = isRegisterWorker ? wireMsg.from : `session:${this.sessionId}`;

    console.log(`SessionActor: Routing ${wireMsg.type}, isRegisterWorker=${isRegisterWorker}, from=${fromField}`);

    const result = await stub.handleMessage({
      type: wireMsg.type,
      payload: wireMsg.payload,
      from: fromField,
      id: wireMsg.id,
      timestamp: wireMsg.timestamp,
    });

    console.log(`SessionActor: Coordinator returned:`, JSON.stringify(result));

    this.sendToClient({
      type: `${wireMsg.type}Response`,
      payload: result,
      replyTo: wireMsg.id,
      from: `session:${this.sessionId}`,
    }, ws);
  }

  private async routeToWorker(wireMsg: WireMessage, actorId: string, ws: WebSocket) {
    const stub = this.env.WORKER_ACTOR.get(
      this.env.WORKER_ACTOR.idFromName(actorId)
    );

    const result = await stub.handleMessage({
      type: wireMsg.type,
      payload: wireMsg.payload,
      from: `session:${this.sessionId}`,
      id: wireMsg.id,
      timestamp: wireMsg.timestamp,
    });

    this.sendToClient({
      type: `${wireMsg.type}Response`,
      payload: result,
      replyTo: wireMsg.id,
      from: `session:${this.sessionId}`,
    }, ws);
  }

  private sendToClient(msg: {
    type: string;
    payload: unknown;
    replyTo?: string;
    from?: string;
  }, targetWs?: WebSocket) {
    // Route responses to appropriate client-side actor based on message type
    // This implements the routing rules from MESSAGE_FLOWS.yaml
    let targetActor = "client:ui"; // Default fallback
    const msgType = msg.type.toLowerCase();

    // Coordinator responses go to coordinator-proxy
    if (msgType.includes("worker") || msgType.includes("task") || msgType.includes("status")) {
      // Check if it's a worker-specific message
      if (msgType.includes(MessageTypes.GET_STATE.toLowerCase()) ||
          msgType.includes(MessageTypes.PROCESS_TASK.toLowerCase()) ||
          msgType.includes(MessageTypes.REPORT_PROGRESS.toLowerCase())) {
        targetActor = "client:worker-proxy";
      } else {
        targetActor = "client:coordinator-proxy";
      }
    }

    const fromField = msg.from || `session:${this.sessionId}`;

    const wireMsg: WireMessage = {
      to: targetActor,
      from: fromField,
      type: msg.type,
      payload: msg.payload,
      id: crypto.randomUUID(),
      timestamp: Date.now(),
      ...(msg.replyTo && { replyTo: msg.replyTo }),
    };

    const messageStr = JSON.stringify(wireMsg);

    // If a specific WebSocket was provided, use it (e.g., for responding to a message)
    if (targetWs) {
      try {
        targetWs.send(messageStr);
      } catch (error) {
        console.error("SessionActor: Failed to send message to client", error);
      }
      return;
    }

    // Otherwise, broadcast to all active WebSocket connections
    const sockets = this.ctx.getWebSockets();
    if (sockets.length === 0) {
      console.warn("SessionActor: No active WebSocket connections");
      return;
    }

    for (const ws of sockets) {
      try {
        ws.send(messageStr);
      } catch (error) {
        console.error("SessionActor: Failed to send message to client", error);
      }
    }
  }
}

// =============================================================================
// ROUTER (Worker Entry Point)
// =============================================================================

// =============================================================================
// PURE ACTOR ARCHITECTURE - WebSocket Only
// =============================================================================
// All actor communication happens via WebSocket message passing.
// HTTP endpoints are only for:
// - WebSocket upgrades (/ws)
//
// See ACTOR_ARCHITECTURE.md for design rationale.
// No REST endpoints (POST /coordinator, POST /worker) - removed in favor of
// WebSocket communication via SessionActor.
// =============================================================================

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);
    const path = url.pathname;

    // WebSocket endpoint - route to SessionActor
    if (path === "/ws") {
      const sessionId = url.searchParams.get("session") ?? crypto.randomUUID();
      const stub = env.SESSION_ACTOR.get(
        env.SESSION_ACTOR.idFromName(sessionId)
      );
      return stub.fetch(request);
    }

    // Serve static assets (dashboard HTML, JS, CSS)
    if (path === "/" || path.startsWith("/dashboard") || path.startsWith("/src/")) {
      return env.ASSETS.fetch(request);
    }

    return new Response("Not found", { status: 404 });
  },
};

// =============================================================================
// TYPES
// =============================================================================

interface Env {
  COORDINATOR_ACTOR: DurableObjectNamespace<CoordinatorActor>;
  WORKER_ACTOR: DurableObjectNamespace<WorkerActor>;
  SESSION_ACTOR: DurableObjectNamespace<SessionActor>;
  ASSETS: Fetcher;
}
