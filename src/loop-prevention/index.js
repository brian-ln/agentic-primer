/**
 * Loop Prevention Coordinator
 *
 * Integrates all 4 loop prevention mechanisms:
 * 1. Depth Counter - Limits event chain depth
 * 2. Fingerprinting - Detects duplicate events
 * 3. Ancestry Chain - Detects cycles in causation
 * 4. Circuit Breaker - Per-function rate limiting
 */

import { DepthCounter } from './depth-counter.js';
import { FingerprintDetector } from './fingerprinting.js';
import { AncestryTracker } from './ancestry-chain.js';
import { CircuitBreaker } from './circuit-breaker.js';

export class LoopPreventionCoordinator {
  constructor(config = {}) {
    this.depthCounter = new DepthCounter(config.maxDepth);
    this.fingerprintDetector = new FingerprintDetector(config.fingerprintCacheSize);
    this.ancestryTracker = new AncestryTracker();
    this.circuitBreaker = new CircuitBreaker(
      config.circuitBreakerThreshold,
      config.circuitBreakerWindow
    );

    this.preventedCount = 0;
    this.preventedByMechanism = {
      'depth-counter': 0,
      'fingerprinting': 0,
      'ancestry-chain': 0,
      'circuit-breaker': 0
    };
  }

  /**
   * Check if event should be allowed
   * Runs all 4 mechanisms in sequence
   */
  checkEvent(event) {
    // Mechanism 1: Depth Counter
    const depthCheck = this.depthCounter.check(event);
    if (!depthCheck.allowed) {
      this.preventedCount++;
      this.preventedByMechanism['depth-counter']++;
      return {
        allowed: false,
        ...depthCheck,
        preventedAt: Date.now()
      };
    }

    // Mechanism 2: Fingerprinting
    const fingerprintCheck = this.fingerprintDetector.check(event);
    if (!fingerprintCheck.allowed) {
      this.preventedCount++;
      this.preventedByMechanism['fingerprinting']++;
      return {
        allowed: false,
        ...fingerprintCheck,
        preventedAt: Date.now()
      };
    }

    // Mechanism 3: Ancestry Chain
    const ancestryCheck = this.ancestryTracker.check(event);
    if (!ancestryCheck.allowed) {
      this.preventedCount++;
      this.preventedByMechanism['ancestry-chain']++;
      return {
        allowed: false,
        ...ancestryCheck,
        preventedAt: Date.now()
      };
    }

    // Mechanism 4: Circuit Breaker (for function execution)
    if (event.function_id) {
      const circuitCheck = this.circuitBreaker.check(event.function_id);
      if (!circuitCheck.allowed) {
        this.preventedCount++;
        this.preventedByMechanism['circuit-breaker']++;
        return {
          allowed: false,
          ...circuitCheck,
          preventedAt: Date.now()
        };
      }

      // Increment circuit breaker count if allowed
      this.circuitBreaker.increment(event.function_id);
    }

    // All checks passed
    return {
      allowed: true,
      depth: depthCheck.depth,
      fingerprint: fingerprintCheck.fingerprint,
      ancestry: ancestryCheck.ancestry
    };
  }

  /**
   * Enrich event with metadata
   */
  enrichEvent(event, parentEvent = null) {
    // Add depth
    if (parentEvent) {
      event.depth = this.depthCounter.incrementDepth(parentEvent);
      event.parent_id = parentEvent.id;
    } else {
      event.depth = 0;
    }

    // Add timestamp if missing
    if (!event.timestamp) {
      event.timestamp = new Date().toISOString();
    }

    return event;
  }

  /**
   * Periodic maintenance
   */
  maintain() {
    // Prune old ancestry chains (every 5 minutes)
    this.ancestryTracker.prune();
  }

  /**
   * Reset all mechanisms
   */
  reset() {
    this.fingerprintDetector.clear();
    this.ancestryTracker.clear();
    this.circuitBreaker.resetAll();
    this.preventedCount = 0;
    this.preventedByMechanism = {
      'depth-counter': 0,
      'fingerprinting': 0,
      'ancestry-chain': 0,
      'circuit-breaker': 0
    };
  }

  /**
   * Get comprehensive statistics
   */
  getStats() {
    return {
      totalPrevented: this.preventedCount,
      preventedByMechanism: { ...this.preventedByMechanism },
      depthCounter: this.depthCounter.getConfig(),
      fingerprinting: this.fingerprintDetector.getStats(),
      ancestryChain: this.ancestryTracker.getStats(),
      circuitBreaker: this.circuitBreaker.getStats()
    };
  }
}

// Export individual mechanisms as well
export { DepthCounter } from './depth-counter.js';
export { FingerprintDetector, generateFingerprint } from './fingerprinting.js';
export { AncestryTracker } from './ancestry-chain.js';
export { CircuitBreaker } from './circuit-breaker.js';
