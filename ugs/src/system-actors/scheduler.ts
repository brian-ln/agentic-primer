#!/usr/bin/env bun
/**
 * SchedulerActor - Time-based message scheduling with real and virtual time
 *
 * Provides:
 * - One-time delayed messages (schedule)
 * - Recurring messages (scheduleRecurring)
 * - Schedule cancellation
 * - Virtual time for tests (instant execution)
 * - Real time for production
 */

import { Actor, createResponse, createMessage } from '@agentic-primer/actors';
import type { Message, MessageResponse, Address, IMessageRouter } from '@agentic-primer/actors';

/**
 * Clock interface - abstracts time for testability
 */
export interface Clock {
  now(): number;
  setTimeout(callback: () => void, delay: number): any;
  setInterval(callback: () => void, interval: number): any;
  clearTimeout(id: any): void;
  clearInterval(id: any): void;
}

/**
 * Real clock - uses actual system time
 */
export class RealClock implements Clock {
  now(): number {
    return Date.now();
  }

  setTimeout(callback: () => void, delay: number): any {
    return globalThis.setTimeout(callback, delay);
  }

  setInterval(callback: () => void, interval: number): any {
    return globalThis.setInterval(callback, interval);
  }

  clearTimeout(id: any): void {
    globalThis.clearTimeout(id);
  }

  clearInterval(id: any): void {
    globalThis.clearInterval(id);
  }
}

/**
 * Virtual clock - controllable time for tests
 */
export class VirtualClock implements Clock {
  private currentTime = 0;
  private timers = new Map<number, { callback: () => void; time: number; recurring?: number }>();
  private nextId = 1;

  now(): number {
    return this.currentTime;
  }

  setTimeout(callback: () => void, delay: number): any {
    const id = this.nextId++;
    const time = this.currentTime + delay;
    this.timers.set(id, { callback, time });
    return id;
  }

  setInterval(callback: () => void, interval: number): any {
    const id = this.nextId++;
    const time = this.currentTime + interval;
    this.timers.set(id, { callback, time, recurring: interval });
    return id;
  }

  clearTimeout(id: any): void {
    this.timers.delete(id);
  }

  clearInterval(id: any): void {
    this.timers.delete(id);
  }

  /**
   * Advance virtual time and execute due callbacks
   */
  async advance(ms: number): Promise<void> {
    const targetTime = this.currentTime + ms;

    // Process all timers up to target time
    while (true) {
      // Find next timer that should fire
      let nextTimer: { id: number; timer: { callback: () => void; time: number; recurring?: number } } | null = null;

      for (const [id, timer] of this.timers.entries()) {
        if (timer.time <= targetTime) {
          if (!nextTimer || timer.time < nextTimer.timer.time) {
            nextTimer = { id, timer };
          }
        }
      }

      if (!nextTimer) break;

      // Advance to this timer's time
      this.currentTime = nextTimer.timer.time;

      // Execute callback
      await Promise.resolve(nextTimer.timer.callback());

      // Handle recurring timers
      if (nextTimer.timer.recurring !== undefined) {
        // Reschedule for next interval
        nextTimer.timer.time = this.currentTime + nextTimer.timer.recurring;
      } else {
        // One-time timer, remove it
        this.timers.delete(nextTimer.id);
      }
    }

    // Advance to target time
    this.currentTime = targetTime;
  }

  /**
   * Fast-forward to next timer (for tests)
   */
  async runNext(): Promise<boolean> {
    if (this.timers.size === 0) return false;

    // Find earliest timer
    let earliest: { id: number; timer: { callback: () => void; time: number; recurring?: number } } | null = null;
    for (const [id, timer] of this.timers.entries()) {
      if (!earliest || timer.time < earliest.timer.time) {
        earliest = { id, timer };
      }
    }

    if (!earliest) return false;

    // Advance to that time
    await this.advance(earliest.timer.time - this.currentTime);
    return true;
  }

  /**
   * Run all pending timers
   */
  async runAll(): Promise<void> {
    while (await this.runNext()) {
      // Keep running until no more timers
    }
  }

  /**
   * Reset clock state
   */
  reset(): void {
    this.currentTime = 0;
    this.timers.clear();
    this.nextId = 1;
  }
}

/**
 * Schedule entry
 */
interface ScheduleEntry {
  id: string;
  message: {
    to: Address;
    type: string;
    payload?: any;
  };
  timerId: any;
  recurring?: number;
  createdAt: number;
}

/**
 * SchedulerActor - Message-based time scheduling
 *
 * Message types:
 * - 'scheduler.schedule' - Schedule one-time message
 * - 'scheduler.recurring' - Schedule recurring message
 * - 'scheduler.cancel' - Cancel schedule
 * - 'scheduler.list' - List active schedules
 */
export class SchedulerActor extends Actor {
  private clock: Clock;
  private schedules = new Map<string, ScheduleEntry>();
  private nextScheduleId = 1;

  constructor(
    router: IMessageRouter,
    options: {
      clock?: 'real' | 'virtual' | Clock;
    } = {}
  ) {
    super('/system/scheduler', router);

    // Initialize clock
    if (!options.clock || options.clock === 'real') {
      this.clock = new RealClock();
    } else if (options.clock === 'virtual') {
      this.clock = new VirtualClock();
    } else {
      this.clock = options.clock;
    }
  }

  /**
   * Get clock instance (for tests)
   */
  getClock(): Clock {
    return this.clock;
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    switch (type) {
      case 'scheduler.schedule':
        return this.handleSchedule(message, payload);

      case 'scheduler.recurring':
        return this.handleRecurring(message, payload);

      case 'scheduler.cancel':
        return this.handleCancel(message, payload);

      case 'scheduler.list':
        return this.handleList(message);

      default:
        return {
          ...createResponse(message, {}),
          success: false,
          error: `Unknown message type: ${type}`,
        };
    }
  }

  /**
   * Schedule one-time message
   */
  private async handleSchedule(
    message: Message,
    payload: {
      delay: number;
      message: { to: Address; type: string; payload?: any };
    }
  ): Promise<MessageResponse> {
    const scheduleId = `schedule-${this.nextScheduleId++}`;

    const timerId = this.clock.setTimeout(async () => {
      // Send the scheduled message
      const msg = createMessage(
        payload.message.to,
        payload.message.type,
        payload.message.payload,
        {
          pattern: 'tell',
          from: this.address,
        }
      );

      await this.router.tell(msg).catch((error) => {
        this.logError('Failed to send scheduled message', {
          scheduleId,
          error: error.message,
        });
      });

      // Remove from schedules
      this.schedules.delete(scheduleId);
    }, payload.delay);

    // Store schedule
    this.schedules.set(scheduleId, {
      id: scheduleId,
      message: payload.message,
      timerId,
      createdAt: this.clock.now(),
    });

    this.logDebug('Scheduled message', {
      scheduleId,
      delay: payload.delay,
      to: payload.message.to,
      type: payload.message.type,
    });

    return {
      ...createResponse(message, { scheduleId }),
      success: true,
    };
  }

  /**
   * Schedule recurring message
   */
  private async handleRecurring(
    message: Message,
    payload: {
      interval: number;
      message: { to: Address; type: string; payload?: any };
    }
  ): Promise<MessageResponse> {
    const scheduleId = `recurring-${this.nextScheduleId++}`;

    const timerId = this.clock.setInterval(async () => {
      // Send the recurring message
      const msg = createMessage(
        payload.message.to,
        payload.message.type,
        payload.message.payload,
        {
          pattern: 'tell',
          from: this.address,
        }
      );

      await this.router.tell(msg).catch((error) => {
        this.logError('Failed to send recurring message', {
          scheduleId,
          error: error.message,
        });
      });
    }, payload.interval);

    // Store schedule
    this.schedules.set(scheduleId, {
      id: scheduleId,
      message: payload.message,
      timerId,
      recurring: payload.interval,
      createdAt: this.clock.now(),
    });

    this.logDebug('Scheduled recurring message', {
      scheduleId,
      interval: payload.interval,
      to: payload.message.to,
      type: payload.message.type,
    });

    return {
      ...createResponse(message, { scheduleId }),
      success: true,
    };
  }

  /**
   * Cancel schedule
   */
  private async handleCancel(
    message: Message,
    payload: { scheduleId: string }
  ): Promise<MessageResponse> {
    const schedule = this.schedules.get(payload.scheduleId);

    if (!schedule) {
      return {
        ...createResponse(message, {}),
        success: false,
        error: `Schedule not found: ${payload.scheduleId}`,
      };
    }

    // Clear timer
    if (schedule.recurring !== undefined) {
      this.clock.clearInterval(schedule.timerId);
    } else {
      this.clock.clearTimeout(schedule.timerId);
    }

    // Remove schedule
    this.schedules.delete(payload.scheduleId);

    this.logDebug('Cancelled schedule', {
      scheduleId: payload.scheduleId,
    });

    return {
      ...createResponse(message, {}),
      success: true,
    };
  }

  /**
   * List active schedules
   */
  private async handleList(message: Message): Promise<MessageResponse> {
    const schedules = Array.from(this.schedules.values()).map((s) => ({
      id: s.id,
      to: s.message.to,
      type: s.message.type,
      recurring: s.recurring !== undefined,
      interval: s.recurring,
      createdAt: s.createdAt,
    }));

    return {
      ...createResponse(message, { schedules, count: schedules.length }),
      success: true,
    };
  }

  /**
   * Shutdown - cancel all schedules
   */
  async shutdown(): Promise<void> {
    for (const schedule of this.schedules.values()) {
      if (schedule.recurring !== undefined) {
        this.clock.clearInterval(schedule.timerId);
      } else {
        this.clock.clearTimeout(schedule.timerId);
      }
    }
    this.schedules.clear();
  }
}
