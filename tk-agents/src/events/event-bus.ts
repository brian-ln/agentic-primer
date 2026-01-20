/**
 * EventBus - Publish/Subscribe event system for task automation
 *
 * Provides a simple event bus for decoupling event producers (EventLog)
 * from event consumers (hooks, workflows, listeners).
 *
 * Design:
 * - EventLog emits events to EventBus
 * - Listeners subscribe to event types
 * - EventBus dispatches events to all matching subscribers
 * - Subscribers can be added/removed dynamically
 */

import type { Event } from "../persistence/event-log.ts";
import type { TaskEvent } from "./task-events.ts";

/**
 * Event listener callback function
 */
export type EventListener<E extends Event = Event> = (event: E) => void | Promise<void>;

/**
 * Subscription handle for managing subscriptions
 */
export interface Subscription {
  /** Unique subscription ID */
  id: string;
  /** Event type this subscription listens to */
  eventType: string;
  /** Unsubscribe function */
  unsubscribe: () => void;
}

/**
 * EventBus implementation
 *
 * Simple publish/subscribe pattern for event-driven automation.
 */
export class EventBus {
  private listeners: Map<string, Set<{ id: string; listener: EventListener }>>;
  private subscriptionCounter: number;

  constructor() {
    this.listeners = new Map();
    this.subscriptionCounter = 0;
  }

  /**
   * Subscribe to an event type
   *
   * @param eventType Event type to listen for (e.g., "task_created")
   * @param listener Callback function to invoke when event occurs
   * @returns Subscription handle with unsubscribe function
   */
  subscribe<E extends Event = Event>(
    eventType: string,
    listener: EventListener<E>
  ): Subscription {
    // Get or create listener set for this event type
    if (!this.listeners.has(eventType)) {
      this.listeners.set(eventType, new Set());
    }

    const listenerSet = this.listeners.get(eventType)!;
    const subscriptionId = `sub_${++this.subscriptionCounter}`;

    // Add listener with ID
    const listenerEntry = {
      id: subscriptionId,
      listener: listener as EventListener,
    };
    listenerSet.add(listenerEntry);

    // Return subscription handle
    return {
      id: subscriptionId,
      eventType,
      unsubscribe: () => {
        listenerSet.delete(listenerEntry);
        // Clean up empty sets
        if (listenerSet.size === 0) {
          this.listeners.delete(eventType);
        }
      },
    };
  }

  /**
   * Subscribe to multiple event types with a single listener
   *
   * @param eventTypes Array of event types
   * @param listener Callback function
   * @returns Array of subscription handles
   */
  subscribeMany<E extends Event = Event>(
    eventTypes: string[],
    listener: EventListener<E>
  ): Subscription[] {
    return eventTypes.map((type) => this.subscribe(type, listener));
  }

  /**
   * Publish an event to all subscribers
   *
   * @param event Event to publish
   */
  async publish(event: Event): Promise<void> {
    const listenerSet = this.listeners.get(event.type);

    if (!listenerSet || listenerSet.size === 0) {
      // No listeners for this event type
      return;
    }

    // Call all listeners (in parallel for async listeners)
    const promises: Promise<void>[] = [];

    for (const { listener } of listenerSet) {
      try {
        const result = listener(event);
        if (result instanceof Promise) {
          promises.push(result);
        }
      } catch (error) {
        // Log error but don't break event dispatch
        console.error(`EventBus listener error for ${event.type}:`, error);
      }
    }

    // Wait for all async listeners to complete
    if (promises.length > 0) {
      await Promise.allSettled(promises);
    }
  }

  /**
   * Get count of subscribers for an event type
   *
   * @param eventType Event type to check
   * @returns Number of subscribers
   */
  getSubscriberCount(eventType: string): number {
    return this.listeners.get(eventType)?.size || 0;
  }

  /**
   * Get all event types that have subscribers
   *
   * @returns Array of event types with active subscriptions
   */
  getActiveEventTypes(): string[] {
    return Array.from(this.listeners.keys());
  }

  /**
   * Clear all subscriptions
   */
  clear(): void {
    this.listeners.clear();
  }

  /**
   * Remove all subscribers for a specific event type
   *
   * @param eventType Event type to clear
   */
  clearEventType(eventType: string): void {
    this.listeners.delete(eventType);
  }
}

/**
 * Global EventBus instance
 *
 * Use this singleton instance for application-wide event handling.
 */
export const globalEventBus = new EventBus();

/**
 * Convenience function to subscribe to task events
 *
 * @param eventType Event type to subscribe to
 * @param listener Listener callback
 * @returns Subscription handle
 */
export function onTaskEvent<E extends TaskEvent = TaskEvent>(
  eventType: E["type"],
  listener: EventListener<E>
): Subscription {
  return globalEventBus.subscribe(eventType, listener);
}

/**
 * Convenience function to emit task events
 *
 * @param event Event to emit
 */
export async function emitTaskEvent(event: TaskEvent): Promise<void> {
  return globalEventBus.publish(event);
}
