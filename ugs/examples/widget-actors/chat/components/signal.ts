/**
 * Minimal TC39 Signal Implementation
 *
 * Provides reactive state management for Widget Actors.
 * Compatible with TC39 Signals proposal interface.
 */

/**
 * Signal class for reactive state.
 * Notifies subscribers when value changes.
 */
export class Signal<T> {
  private value: T;
  private subscribers = new Set<(val: T) => void>();

  constructor(initial: T) {
    this.value = initial;
  }

  /**
   * Get the current signal value.
   */
  get(): T {
    return this.value;
  }

  /**
   * Set a new signal value.
   * Notifies all subscribers if value changed.
   */
  set(newValue: T): void {
    if (this.value !== newValue) {
      this.value = newValue;
      this.subscribers.forEach((fn) => fn(newValue));
    }
  }

  /**
   * Subscribe to value changes.
   * Returns unsubscribe function.
   */
  subscribe(fn: (val: T) => void): () => void {
    this.subscribers.add(fn);
    return () => this.subscribers.delete(fn);
  }
}

/**
 * Create a new signal with initial value.
 */
export function signal<T>(initial: T): Signal<T> {
  return new Signal(initial);
}

/**
 * Run an effect function.
 * Simple implementation just runs immediately.
 */
export function effect(fn: () => void): void {
  fn();
}
