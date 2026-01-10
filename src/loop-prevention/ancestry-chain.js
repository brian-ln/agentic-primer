/**
 * Ancestry Chain Tracking - Mechanism 3
 *
 * Detects cycles in event causation chains using parent_id.
 * Tracks ancestry to prevent events from triggering themselves.
 */

export class AncestryTracker {
  constructor() {
    // Map of event_id -> parent chain
    this.chains = new Map();
  }

  /**
   * Build ancestry chain for event
   */
  buildChain(event) {
    const chain = [];
    let current = event;

    // Traverse up the parent chain
    while (current && current.parent_id) {
      chain.push(current.parent_id);

      // Look up parent in chains map
      const parentChain = this.chains.get(current.parent_id);
      if (parentChain) {
        chain.push(...parentChain);
        break;
      }

      current = null; // No more parents in cache
    }

    return chain;
  }

  /**
   * Check if event creates a cycle
   */
  check(event) {
    if (!event.parent_id) {
      // Root event, no cycle possible
      return { allowed: true, ancestry: [] };
    }

    // Build ancestry chain
    const ancestry = this.buildChain(event);

    // Check if event's own ID appears in ancestry (cycle!)
    if (event.id && ancestry.includes(event.id)) {
      return {
        allowed: false,
        reason: `Cycle detected: event ${event.id} found in ancestry chain`,
        mechanism: 'ancestry-chain',
        ancestry,
        cycleDepth: ancestry.indexOf(event.id)
      };
    }

    // Check if any function/pattern in ancestry repeats
    if (event.triggered_by) {
      const triggeredByInChain = ancestry.filter(id => {
        const parent = this.chains.get(id);
        return parent && parent.triggered_by === event.triggered_by;
      });

      if (triggeredByInChain.length > 0) {
        return {
          allowed: false,
          reason: `Same trigger detected in ancestry: ${event.triggered_by}`,
          mechanism: 'ancestry-chain',
          ancestry,
          repeatedTrigger: event.triggered_by
        };
      }
    }

    // Store chain for this event
    if (event.id) {
      this.chains.set(event.id, {
        ancestry,
        triggered_by: event.triggered_by,
        timestamp: Date.now()
      });
    }

    return { allowed: true, ancestry };
  }

  /**
   * Clear old chains (memory management)
   */
  prune(maxAge = 300000) { // 5 minutes default
    const now = Date.now();

    for (const [id, data] of this.chains.entries()) {
      if (now - data.timestamp > maxAge) {
        this.chains.delete(id);
      }
    }
  }

  /**
   * Clear all chains
   */
  clear() {
    this.chains.clear();
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      trackedChains: this.chains.size,
      oldestChain: this._getOldestChainAge()
    };
  }

  _getOldestChainAge() {
    let oldest = null;

    for (const data of this.chains.values()) {
      if (!oldest || data.timestamp < oldest) {
        oldest = data.timestamp;
      }
    }

    return oldest ? Date.now() - oldest : 0;
  }
}
