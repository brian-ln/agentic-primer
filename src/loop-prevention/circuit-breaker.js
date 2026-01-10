/**
 * Circuit Breaker - Mechanism 4
 *
 * Per-function rate limiting to prevent runaway functions.
 * Tracks execution count per time window.
 */

export const DEFAULT_THRESHOLD = 100;
export const DEFAULT_WINDOW_MS = 60000; // 1 minute

export class CircuitBreaker {
  constructor(threshold = DEFAULT_THRESHOLD, windowMs = DEFAULT_WINDOW_MS) {
    this.threshold = threshold;
    this.windowMs = windowMs;
    this.counts = new Map(); // functionId -> count
    this.windows = new Map(); // functionId -> window start time
    this.state = new Map(); // functionId -> 'closed' | 'open' | 'half-open'
    this.trips = 0;
  }

  /**
   * Check if function is allowed to execute
   */
  check(functionId) {
    if (!functionId) {
      return { allowed: true };
    }

    const now = Date.now();
    const state = this.state.get(functionId) || 'closed';

    // If circuit is open (tripped), reject
    if (state === 'open') {
      const windowStart = this.windows.get(functionId);
      const timeSinceTrip = now - windowStart;

      // Auto-reset after window expires
      if (timeSinceTrip > this.windowMs) {
        this.state.set(functionId, 'half-open');
        this.counts.set(functionId, 0);
        this.windows.set(functionId, now);
        return { allowed: true, state: 'half-open' };
      }

      return {
        allowed: false,
        reason: `Circuit breaker open for ${functionId} (${this.counts.get(functionId)}/${this.threshold})`,
        mechanism: 'circuit-breaker',
        functionId,
        count: this.counts.get(functionId),
        threshold: this.threshold,
        timeRemaining: this.windowMs - timeSinceTrip
      };
    }

    // Check if window expired
    const windowStart = this.windows.get(functionId);
    if (windowStart && (now - windowStart) > this.windowMs) {
      // Reset window
      this.counts.set(functionId, 0);
      this.windows.set(functionId, now);
      this.state.set(functionId, 'closed');
    }

    const count = this.counts.get(functionId) || 0;

    // Check threshold
    if (count >= this.threshold) {
      // Trip circuit
      this.state.set(functionId, 'open');
      this.trips++;

      return {
        allowed: false,
        reason: `Circuit breaker tripped for ${functionId} (${count}/${this.threshold})`,
        mechanism: 'circuit-breaker',
        functionId,
        count,
        threshold: this.threshold
      };
    }

    return { allowed: true, state, count };
  }

  /**
   * Increment execution count
   */
  increment(functionId) {
    if (!functionId) return;

    const count = this.counts.get(functionId) || 0;
    this.counts.set(functionId, count + 1);

    if (!this.windows.has(functionId)) {
      this.windows.set(functionId, Date.now());
    }
  }

  /**
   * Reset circuit breaker for function
   */
  reset(functionId) {
    this.counts.delete(functionId);
    this.windows.delete(functionId);
    this.state.set(functionId, 'closed');
  }

  /**
   * Reset all circuit breakers
   */
  resetAll() {
    this.counts.clear();
    this.windows.clear();
    this.state.clear();
    this.trips = 0;
  }

  /**
   * Get circuit breaker state
   */
  getState(functionId) {
    return {
      state: this.state.get(functionId) || 'closed',
      count: this.counts.get(functionId) || 0,
      threshold: this.threshold,
      window: this.windowMs
    };
  }

  /**
   * Get statistics
   */
  getStats() {
    const openCircuits = Array.from(this.state.values()).filter(s => s === 'open').length;

    return {
      totalCircuits: this.state.size,
      openCircuits,
      closedCircuits: this.state.size - openCircuits,
      totalTrips: this.trips,
      threshold: this.threshold,
      windowMs: this.windowMs
    };
  }
}
