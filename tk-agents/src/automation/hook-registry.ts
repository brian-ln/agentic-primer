/**
 * HookRegistry - Manages lifecycle hook registration and execution
 *
 * Central registry for all lifecycle hooks. Handles:
 * - Hook registration/unregistration
 * - Hook discovery by event type
 * - Priority-ordered execution
 * - Error isolation
 */

import type { Event } from "../persistence/event-log.ts";
import type { LifecycleHook, HookContext, HookAction } from "./hooks.ts";

/**
 * Hook registry for managing lifecycle hooks
 */
export class HookRegistry {
  private hooks: Map<string, LifecycleHook>;

  constructor() {
    this.hooks = new Map();
  }

  /**
   * Register a lifecycle hook
   *
   * @param hook Hook to register
   * @throws Error if hook with same name already exists
   */
  register(hook: LifecycleHook): void {
    if (this.hooks.has(hook.name)) {
      throw new Error(`Hook with name "${hook.name}" already registered`);
    }

    // Validate hook
    if (!hook.name || hook.name.trim() === "") {
      throw new Error("Hook name cannot be empty");
    }

    if (!hook.eventTypes || hook.eventTypes.length === 0) {
      throw new Error(`Hook "${hook.name}" must subscribe to at least one event type`);
    }

    if (typeof hook.handler !== "function") {
      throw new Error(`Hook "${hook.name}" handler must be a function`);
    }

    this.hooks.set(hook.name, hook);
  }

  /**
   * Unregister a hook by name
   *
   * @param name Hook name to remove
   * @returns True if hook was removed, false if not found
   */
  unregister(name: string): boolean {
    return this.hooks.delete(name);
  }

  /**
   * Get all hooks subscribed to an event type
   *
   * @param eventType Event type to query
   * @returns Hooks in priority order (low to high)
   */
  getHooksForEvent(eventType: string): LifecycleHook[] {
    return Array.from(this.hooks.values())
      .filter((h) => h.enabled !== false && h.eventTypes.includes(eventType))
      .sort((a, b) => (a.priority || 100) - (b.priority || 100));
  }

  /**
   * Execute all hooks for an event
   *
   * Hooks are executed sequentially in priority order.
   * Errors in one hook don't prevent execution of other hooks.
   *
   * @param event Event to process
   * @param context Hook execution context
   * @returns Aggregated actions from all hooks
   */
  async executeHooks(event: Event, context: HookContext): Promise<HookAction[]> {
    const hooks = this.getHooksForEvent(event.type);
    const allActions: HookAction[] = [];

    for (const hook of hooks) {
      try {
        const actions = await hook.handler(event, context);
        allActions.push(...actions);
      } catch (error) {
        // Log error but continue executing other hooks
        console.error(`Hook "${hook.name}" failed for event ${event.type}:`, error);

        // Optionally emit error event
        if (context.eventLog) {
          try {
            context.eventLog.append({
              timestamp: new Date().toISOString(),
              type: "hook_error",
              nodeId: event.nodeId,
              data: {
                hookName: hook.name,
                error: error instanceof Error ? error.message : String(error),
                originalEventType: event.type,
              },
            });
          } catch {
            // Silently ignore if we can't log the error
          }
        }
      }
    }

    return allActions;
  }

  /**
   * List all registered hooks
   *
   * @returns Array of all hooks
   */
  listHooks(): LifecycleHook[] {
    return Array.from(this.hooks.values());
  }

  /**
   * Get hook by name
   *
   * @param name Hook name
   * @returns Hook or undefined if not found
   */
  getHook(name: string): LifecycleHook | undefined {
    return this.hooks.get(name);
  }

  /**
   * Get count of registered hooks
   *
   * @returns Number of hooks
   */
  getHookCount(): number {
    return this.hooks.size;
  }

  /**
   * Get count of enabled hooks
   *
   * @returns Number of enabled hooks
   */
  getEnabledHookCount(): number {
    return Array.from(this.hooks.values()).filter((h) => h.enabled !== false).length;
  }

  /**
   * Enable a hook by name
   *
   * @param name Hook name
   * @returns True if hook was found and enabled
   */
  enableHook(name: string): boolean {
    const hook = this.hooks.get(name);
    if (hook) {
      hook.enabled = true;
      return true;
    }
    return false;
  }

  /**
   * Disable a hook by name
   *
   * @param name Hook name
   * @returns True if hook was found and disabled
   */
  disableHook(name: string): boolean {
    const hook = this.hooks.get(name);
    if (hook) {
      hook.enabled = false;
      return true;
    }
    return false;
  }

  /**
   * Clear all hooks
   */
  clear(): void {
    this.hooks.clear();
  }

  /**
   * Get all unique event types that have hooks
   *
   * @returns Array of event types
   */
  getActiveEventTypes(): string[] {
    const eventTypes = new Set<string>();
    for (const hook of this.hooks.values()) {
      if (hook.enabled !== false) {
        for (const eventType of hook.eventTypes) {
          eventTypes.add(eventType);
        }
      }
    }
    return Array.from(eventTypes);
  }
}

/**
 * Global hook registry instance
 *
 * Use this singleton instance for application-wide hook management.
 */
export const globalHookRegistry = new HookRegistry();
