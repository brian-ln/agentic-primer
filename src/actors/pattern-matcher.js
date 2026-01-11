/**
 * PatternMatcherActor
 *
 * Evaluates events against JavaScript predicates and returns matching patterns.
 * Uses safe predicate execution with error handling and priority sorting.
 *
 * Pattern structure:
 * {
 *   id: string,           // Unique pattern identifier
 *   predicate: string,    // JavaScript expression returning boolean
 *   priority: number,     // Higher = evaluated first (default: 0)
 *   metadata: object      // Optional additional data
 * }
 */

/**
 * Create a PatternMatcherActor instance
 */
export function createPatternMatcher() {
  const patterns = new Map();
  let isRunning = false;

  return {
    /**
     * Start the actor - initialize pattern matcher
     */
    async start() {
      if (isRunning) {
        return {
          success: false,
          error: 'PatternMatcherActor is already running'
        };
      }

      try {
        isRunning = true;
        return {
          success: true,
          message: 'PatternMatcherActor started successfully'
        };
      } catch (error) {
        return {
          success: false,
          error: `Failed to start PatternMatcherActor: ${error.message}`
        };
      }
    },

    /**
     * Stop the actor - cleanup resources
     */
    async stop() {
      if (!isRunning) {
        return {
          success: true,
          message: 'PatternMatcherActor was not running'
        };
      }

      try {
        // No cleanup needed for now, but could add resource cleanup here
        isRunning = false;
        return {
          success: true,
          message: 'PatternMatcherActor stopped successfully'
        };
      } catch (error) {
        return {
          success: false,
          error: `Failed to stop PatternMatcherActor: ${error.message}`
        };
      }
    },

    /**
     * Get actor status
     */
    getStatus() {
      return {
        isRunning,
        patternCount: patterns.size
      };
    },
    /**
     * Register a new pattern
     *
     * @param {object} pattern - Pattern to register
     * @param {string} pattern.id - Unique pattern identifier
     * @param {string} pattern.predicate - JavaScript predicate expression
     * @param {number} [pattern.priority=0] - Pattern priority (higher = first)
     * @param {object} [pattern.metadata] - Optional metadata
     * @returns {object} Result with success status
     */
    registerPattern(pattern) {
      // Validate pattern structure
      if (!pattern || typeof pattern !== 'object') {
        return {
          success: false,
          error: "Pattern must be an object"
        };
      }

      if (!pattern.id || typeof pattern.id !== 'string') {
        return {
          success: false,
          error: "Pattern must have an id string"
        };
      }

      if (!pattern.predicate || typeof pattern.predicate !== 'string') {
        return {
          success: false,
          error: "Pattern must have a predicate string"
        };
      }

      // Check for duplicate ID
      if (patterns.has(pattern.id)) {
        return {
          success: false,
          error: `Pattern with id '${pattern.id}' already exists`
        };
      }

      // Validate predicate syntax by attempting compilation
      try {
        // Test compilation (don't execute yet)
        new Function('event', `return (${pattern.predicate});`);
      } catch (error) {
        return {
          success: false,
          error: `Invalid predicate syntax: ${error.message}`
        };
      }

      // Store pattern with defaults
      const normalizedPattern = {
        id: pattern.id,
        predicate: pattern.predicate,
        priority: typeof pattern.priority === 'number' ? pattern.priority : 0,
        metadata: pattern.metadata || {}
      };

      patterns.set(pattern.id, normalizedPattern);

      return {
        success: true,
        pattern: normalizedPattern
      };
    },

    /**
     * Unregister a pattern by ID
     *
     * @param {string} id - Pattern ID to remove
     * @returns {object} Result with success status
     */
    unregisterPattern(id) {
      if (!id || typeof id !== 'string') {
        return {
          success: false,
          error: "Pattern ID must be a string"
        };
      }

      if (!patterns.has(id)) {
        return {
          success: false,
          error: `Pattern '${id}' not found`
        };
      }

      patterns.delete(id);

      return {
        success: true,
        id
      };
    },

    /**
     * List all registered patterns
     *
     * @param {object} [options] - Query options
     * @param {boolean} [options.sortByPriority=false] - Sort by priority
     * @returns {Array} Array of patterns
     */
    listPatterns(options = {}) {
      const patternList = Array.from(patterns.values());

      if (options.sortByPriority) {
        // Sort by priority (descending), then by id (ascending) for stability
        patternList.sort((a, b) => {
          if (a.priority !== b.priority) {
            return b.priority - a.priority; // Higher priority first
          }
          return a.id.localeCompare(b.id);
        });
      }

      return patternList;
    },

    /**
     * Match an event against all registered patterns
     *
     * @param {object} event - Event to match against patterns
     * @returns {object} Result containing matches and errors
     */
    matchEvent(event) {
      if (!event || typeof event !== 'object') {
        return {
          success: false,
          error: "Event must be an object",
          matches: [],
          errors: []
        };
      }

      const matches = [];
      const errors = [];

      // Get patterns sorted by priority
      const sortedPatterns = this.listPatterns({ sortByPriority: true });

      // Evaluate each pattern
      for (const pattern of sortedPatterns) {
        try {
          // Create predicate function with event in scope
          const predicateFn = new Function('event', `return (${pattern.predicate});`);

          // Execute predicate
          const result = predicateFn(event);

          // If predicate returns truthy value, it's a match
          if (result) {
            matches.push({
              patternId: pattern.id,
              priority: pattern.priority,
              metadata: pattern.metadata
            });
          }
        } catch (error) {
          // Capture predicate execution errors without stopping
          errors.push({
            patternId: pattern.id,
            error: error.message,
            stack: error.stack
          });
        }
      }

      return {
        success: true,
        matches,
        errors,
        eventMatched: event
      };
    },

    /**
     * Get pattern by ID
     *
     * @param {string} id - Pattern ID
     * @returns {object|null} Pattern or null if not found
     */
    getPattern(id) {
      return patterns.get(id) || null;
    },

    /**
     * Get pattern count
     *
     * @returns {number} Number of registered patterns
     */
    getPatternCount() {
      return patterns.size;
    },

    /**
     * Clear all patterns
     *
     * @returns {object} Result with count of cleared patterns
     */
    clearPatterns() {
      const count = patterns.size;
      patterns.clear();
      return {
        success: true,
        clearedCount: count
      };
    }
  };
}
