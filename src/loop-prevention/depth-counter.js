/**
 * Depth Counter - Mechanism 1
 *
 * Prevents infinite loops by limiting event chain depth.
 * Configurable threshold (default: 50)
 */

export const DEFAULT_MAX_DEPTH = 50;

export class DepthCounter {
  constructor(maxDepth = DEFAULT_MAX_DEPTH) {
    this.maxDepth = maxDepth;
  }

  /**
   * Check if event exceeds depth limit
   */
  check(event) {
    const depth = event.depth || 0;

    if (depth >= this.maxDepth) {
      return {
        allowed: false,
        reason: `Depth limit exceeded (${depth} >= ${this.maxDepth})`,
        mechanism: 'depth-counter',
        depth
      };
    }

    return { allowed: true, depth };
  }

  /**
   * Increment depth for child event
   */
  incrementDepth(parentEvent) {
    return (parentEvent.depth || 0) + 1;
  }

  /**
   * Update max depth configuration
   */
  setMaxDepth(maxDepth) {
    this.maxDepth = maxDepth;
  }

  /**
   * Get current configuration
   */
  getConfig() {
    return {
      maxDepth: this.maxDepth
    };
  }
}
