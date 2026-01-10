/**
 * FunctionRegistryActor
 *
 * Responsibility: Track available functions and their metadata
 *
 * Manages a catalog of functions that can be executed in response to events.
 * Supports both "code" (.js files) and "agent" (Claude CLI) function types.
 * Provides auto-discovery by scanning directories for function files.
 */

import { readdir, stat } from 'node:fs/promises';
import { join, extname, basename } from 'node:path';
import { PROTOCOLS, ACTIONS, createMessage, validateMessage } from '../protocol.js';

/**
 * FunctionRegistryActor state
 */
class FunctionRegistryActor {
  constructor() {
    // Map<functionId, functionMetadata>
    this.functions = new Map();
  }

  /**
   * Register a function in the registry
   *
   * @param {string} functionId - Unique identifier for the function
   * @param {Object} metadata - Function metadata
   * @param {string} metadata.type - "code" or "agent"
   * @param {string} metadata.path - File path for code functions
   * @param {string} [metadata.agentCommand] - Command for agent functions
   * @param {number} [metadata.maxStackDepth] - Override default stack depth
   * @param {Object} [metadata.metadata] - Additional metadata (name, description, author)
   * @returns {Object} UAP response message
   */
  registerFunction(functionId, metadata) {
    // Validate required fields
    if (!functionId || typeof functionId !== 'string') {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: 'functionId is required and must be a string' }
      );
    }

    if (!metadata || typeof metadata !== 'object') {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: 'metadata is required and must be an object' }
      );
    }

    if (!metadata.type || !['code', 'agent'].includes(metadata.type)) {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: 'metadata.type must be "code" or "agent"' }
      );
    }

    if (metadata.type === 'code' && !metadata.path) {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: 'metadata.path is required for code functions' }
      );
    }

    if (metadata.type === 'agent' && !metadata.agentCommand) {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: 'metadata.agentCommand is required for agent functions' }
      );
    }

    // Store function with full metadata
    this.functions.set(functionId, {
      type: metadata.type,
      path: metadata.path || null,
      agentCommand: metadata.agentCommand || null,
      maxStackDepth: metadata.maxStackDepth || null,
      metadata: {
        name: metadata.metadata?.name || functionId,
        description: metadata.metadata?.description || '',
        author: metadata.metadata?.author || ''
      },
      registeredAt: new Date().toISOString()
    });

    return createMessage(
      PROTOCOLS.REGISTRY,
      ACTIONS.REGISTER,
      {
        functionId,
        success: true,
        message: `Function '${functionId}' registered successfully`
      }
    );
  }

  /**
   * Unregister a function from the registry
   *
   * @param {string} functionId - Function ID to unregister
   * @returns {Object} UAP response message
   */
  unregisterFunction(functionId) {
    if (!functionId || typeof functionId !== 'string') {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: 'functionId is required and must be a string' }
      );
    }

    const existed = this.functions.has(functionId);
    this.functions.delete(functionId);

    return createMessage(
      PROTOCOLS.REGISTRY,
      ACTIONS.UNREGISTER,
      {
        functionId,
        success: true,
        existed,
        message: existed
          ? `Function '${functionId}' unregistered successfully`
          : `Function '${functionId}' was not registered`
      }
    );
  }

  /**
   * Get function metadata by ID
   *
   * @param {string} functionId - Function ID to retrieve
   * @returns {Object} UAP response message with function metadata
   */
  getFunction(functionId) {
    if (!functionId || typeof functionId !== 'string') {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: 'functionId is required and must be a string' }
      );
    }

    const functionData = this.functions.get(functionId);

    if (!functionData) {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        {
          error: `Function '${functionId}' not found`,
          functionId
        }
      );
    }

    return createMessage(
      PROTOCOLS.REGISTRY,
      ACTIONS.RESPONSE,
      {
        functionId,
        function: functionData
      }
    );
  }

  /**
   * List all registered functions
   *
   * @param {Object} [filters] - Optional filters
   * @param {string} [filters.type] - Filter by type ("code" or "agent")
   * @returns {Object} UAP response message with list of functions
   */
  listFunctions(filters = {}) {
    let functions = Array.from(this.functions.entries()).map(([id, data]) => ({
      functionId: id,
      ...data
    }));

    // Apply type filter if provided
    if (filters.type && ['code', 'agent'].includes(filters.type)) {
      functions = functions.filter(f => f.type === filters.type);
    }

    return createMessage(
      PROTOCOLS.REGISTRY,
      ACTIONS.LIST,
      {
        functions,
        count: functions.length,
        filters: filters
      }
    );
  }

  /**
   * Scan a directory for function files and auto-register them
   *
   * Discovers .js files (code functions) and .agent.js files (agent functions)
   *
   * @param {string} directoryPath - Directory to scan
   * @param {Object} [options] - Scan options
   * @param {boolean} [options.recursive] - Scan subdirectories (default: false)
   * @param {boolean} [options.overwrite] - Overwrite existing registrations (default: false)
   * @returns {Object} UAP response message with discovered functions
   */
  async scanDirectory(directoryPath, options = {}) {
    const { recursive = false, overwrite = false } = options;
    const discovered = [];
    const errors = [];
    const skipped = [];

    try {
      const entries = await readdir(directoryPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(directoryPath, entry.name);

        // Recursively scan subdirectories if requested
        if (entry.isDirectory() && recursive) {
          try {
            const subResult = await this.scanDirectory(fullPath, options);
            if (subResult.data.discovered) {
              discovered.push(...subResult.data.discovered);
            }
            if (subResult.data.errors) {
              errors.push(...subResult.data.errors);
            }
            if (subResult.data.skipped) {
              skipped.push(...subResult.data.skipped);
            }
          } catch (err) {
            errors.push({
              path: fullPath,
              error: err.message
            });
          }
          continue;
        }

        // Only process .js files
        if (!entry.isFile() || extname(entry.name) !== '.js') {
          continue;
        }

        // Determine function type based on filename
        const isAgentFunction = entry.name.endsWith('.agent.js');
        const functionId = isAgentFunction
          ? basename(entry.name, '.agent.js')
          : basename(entry.name, '.js');

        // Skip if already registered and not overwriting
        if (this.functions.has(functionId) && !overwrite) {
          skipped.push({
            functionId,
            path: fullPath,
            reason: 'already registered'
          });
          continue;
        }

        // Register the function
        const metadata = {
          type: isAgentFunction ? 'agent' : 'code',
          path: fullPath,
          agentCommand: isAgentFunction ? 'claude' : undefined,
          metadata: {
            name: functionId,
            description: `Auto-discovered ${isAgentFunction ? 'agent' : 'code'} function`,
            author: 'auto-discovery'
          }
        };

        const result = this.registerFunction(functionId, metadata);

        if (result.action === ACTIONS.REGISTER) {
          discovered.push({
            functionId,
            type: metadata.type,
            path: fullPath
          });
        } else {
          errors.push({
            functionId,
            path: fullPath,
            error: result.data.error
          });
        }
      }

      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.RESPONSE,
        {
          directoryPath,
          discovered,
          discoveredCount: discovered.length,
          errors,
          errorCount: errors.length,
          skipped,
          skippedCount: skipped.length,
          success: true
        }
      );

    } catch (err) {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        {
          error: `Failed to scan directory: ${err.message}`,
          directoryPath,
          discovered,
          discoveredCount: discovered.length,
          errors,
          errorCount: errors.length
        }
      );
    }
  }

  /**
   * Handle incoming UAP messages
   *
   * @param {Object} message - UAP message
   * @returns {Object} UAP response message
   */
  async handleMessage(message) {
    const validation = validateMessage(message);
    if (!validation.valid) {
      return createMessage(
        PROTOCOLS.REGISTRY,
        ACTIONS.ERROR,
        { error: validation.error }
      );
    }

    switch (message.action) {
      case ACTIONS.REGISTER:
        return this.registerFunction(
          message.data.functionId,
          message.data.metadata
        );

      case ACTIONS.UNREGISTER:
        return this.unregisterFunction(message.data.functionId);

      case ACTIONS.LIST:
        return this.listFunctions(message.data.filters);

      case 'get':
        return this.getFunction(message.data.functionId);

      case 'scan':
        return await this.scanDirectory(
          message.data.directoryPath,
          message.data.options
        );

      default:
        return createMessage(
          PROTOCOLS.REGISTRY,
          ACTIONS.ERROR,
          { error: `Unknown action: ${message.action}` }
        );
    }
  }

  /**
   * Get registry statistics
   *
   * @returns {Object} Registry statistics
   */
  getStats() {
    const functions = Array.from(this.functions.values());
    const codeCount = functions.filter(f => f.type === 'code').length;
    const agentCount = functions.filter(f => f.type === 'agent').length;

    return {
      totalFunctions: this.functions.size,
      codeFunctions: codeCount,
      agentFunctions: agentCount,
      functions: Array.from(this.functions.keys())
    };
  }

  /**
   * Clear all registered functions (for testing)
   */
  clear() {
    this.functions.clear();
  }
}

export default FunctionRegistryActor;
