/**
 * FunctionExecutorActor
 *
 * Responsibility: Execute individual function invocations
 *
 * Features:
 * - Dynamic ES module loading from filesystem (Bun)
 * - Execution context injection {emit, logger, config}
 * - Return value capture
 * - Graceful error handling
 * - Event emission (function.executed, function.error)
 */

import { resolve } from 'node:path';
import { PROTOCOLS, ACTIONS, createMessage, validateMessage } from '../protocol.js';

/**
 * Simple logger implementation for function context
 */
class Logger {
  constructor(functionId) {
    this.functionId = functionId;
  }

  info(...args) {
    console.log(`[${this.functionId}]`, ...args);
  }

  warn(...args) {
    console.warn(`[${this.functionId}]`, ...args);
  }

  error(...args) {
    console.error(`[${this.functionId}]`, ...args);
  }

  debug(...args) {
    if (process.env.DEBUG) {
      console.log(`[${this.functionId}][DEBUG]`, ...args);
    }
  }
}

/**
 * FunctionExecutorActor class
 */
export class FunctionExecutorActor {
  constructor(config = {}) {
    this.config = config;
    this.emitCallback = null; // Will be set via setEmitCallback()
    this.isRunning = false;
  }

  /**
   * Start the actor - initialize function executor
   */
  async start() {
    if (this.isRunning) {
      return {
        success: false,
        error: 'FunctionExecutorActor is already running'
      };
    }

    try {
      this.isRunning = true;
      return {
        success: true,
        message: 'FunctionExecutorActor started successfully'
      };
    } catch (error) {
      return {
        success: false,
        error: `Failed to start FunctionExecutorActor: ${error.message}`
      };
    }
  }

  /**
   * Stop the actor - cleanup resources
   */
  async stop() {
    if (!this.isRunning) {
      return {
        success: true,
        message: 'FunctionExecutorActor was not running'
      };
    }

    try {
      // No cleanup needed for now, but could add resource cleanup here
      this.isRunning = false;
      return {
        success: true,
        message: 'FunctionExecutorActor stopped successfully'
      };
    } catch (error) {
      return {
        success: false,
        error: `Failed to stop FunctionExecutorActor: ${error.message}`
      };
    }
  }

  /**
   * Get actor status
   */
  getStatus() {
    return {
      isRunning: this.isRunning,
      hasEmitCallback: this.emitCallback !== null
    };
  }

  /**
   * Set the emit callback for emitting events from functions
   *
   * @param {Function} callback - Function that accepts (event) and emits it
   */
  setEmitCallback(callback) {
    this.emitCallback = callback;
  }

  /**
   * Execute a function with the given event and metadata
   *
   * @param {Object} params - Execution parameters
   * @param {string} params.functionId - Function identifier
   * @param {string} params.functionPath - Absolute or relative path to function file
   * @param {string} [params.functionType] - Function type: 'code' or 'agent'
   * @param {string} [params.agentCommand] - Agent command (for agent functions)
   * @param {Object} params.event - Event object to pass to function
   * @param {Object} [params.config] - Additional config to pass to function
   * @returns {Object} UAP response message
   */
  async execute({ functionId, functionPath, functionType = 'code', agentCommand, event, config = {} }) {
    // Route to agent executor if function type is 'agent'
    if (functionType === 'agent') {
      return this.executeAgent({ functionId, functionPath, agentCommand, event, config });
    }
    const startTime = Date.now();
    const logger = new Logger(functionId);

    try {
      // Validate inputs
      if (!functionId) {
        return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
          error: 'functionId is required',
          functionId
        });
      }

      if (!functionPath) {
        return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
          error: 'functionPath is required',
          functionId
        });
      }

      if (!event) {
        return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
          error: 'event is required',
          functionId
        });
      }

      logger.info(`Executing function: ${functionId}`);
      logger.debug(`Function path: ${functionPath}`);
      logger.debug(`Event type: ${event.type}`);

      // Resolve absolute path
      const absolutePath = resolve(functionPath);

      // Dynamic import using Bun
      let module;
      try {
        module = await import(absolutePath);
      } catch (importError) {
        logger.error(`Failed to load module: ${importError.message}`);

        // Emit error event if callback is set
        if (this.emitCallback) {
          await this.emitCallback({
            type: 'function.error',
            data: {
              functionId,
              error: `Import error: ${importError.message}`,
              event,
              phase: 'import'
            },
            metadata: {
              source: 'FunctionExecutorActor',
              triggeredBy: event.id,
              depth: (event.metadata?.depth || 0) + 1
            }
          });
        }

        return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
          error: `Failed to load function: ${importError.message}`,
          functionId,
          functionPath: absolutePath,
          phase: 'import'
        });
      }

      // Get the default export (the function)
      const fn = module.default;

      if (typeof fn !== 'function') {
        const error = 'Module does not export a default function';
        logger.error(error);

        if (this.emitCallback) {
          await this.emitCallback({
            type: 'function.error',
            data: {
              functionId,
              error,
              event,
              phase: 'validation'
            },
            metadata: {
              source: 'FunctionExecutorActor',
              triggeredBy: event.id,
              depth: (event.metadata?.depth || 0) + 1
            }
          });
        }

        return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
          error,
          functionId,
          functionPath: absolutePath,
          phase: 'validation'
        });
      }

      // Create execution context
      const context = {
        emit: async (newEvent) => {
          if (this.emitCallback) {
            // Add depth tracking
            const eventWithMetadata = {
              ...newEvent,
              metadata: {
                ...newEvent.metadata,
                source: functionId,
                triggeredBy: event.id,
                depth: (event.metadata?.depth || 0) + 1
              }
            };
            await this.emitCallback(eventWithMetadata);
          } else {
            logger.warn('Emit called but no emit callback is set');
          }
        },
        logger,
        config: {
          ...this.config,
          ...config
        }
      };

      // Execute the function
      logger.debug('Invoking function...');
      let result;
      try {
        result = await fn(event, context);
      } catch (executionError) {
        logger.error(`Function execution error: ${executionError.message}`);
        logger.debug(`Stack trace: ${executionError.stack}`);

        // Emit error event
        if (this.emitCallback) {
          await this.emitCallback({
            type: 'function.error',
            data: {
              functionId,
              error: executionError.message,
              stack: executionError.stack,
              event,
              phase: 'execution'
            },
            metadata: {
              source: 'FunctionExecutorActor',
              triggeredBy: event.id,
              depth: (event.metadata?.depth || 0) + 1
            }
          });
        }

        return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
          error: `Function execution error: ${executionError.message}`,
          stack: executionError.stack,
          functionId,
          functionPath: absolutePath,
          event,
          phase: 'execution'
        });
      }

      const executionTime = Date.now() - startTime;
      logger.info(`Function completed in ${executionTime}ms`);

      // Emit success event
      if (this.emitCallback) {
        await this.emitCallback({
          type: 'function.executed',
          data: {
            functionId,
            result,
            executionTime,
            event
          },
          metadata: {
            source: 'FunctionExecutorActor',
            triggeredBy: event.id,
            depth: (event.metadata?.depth || 0) + 1
          }
        });
      }

      return createMessage(PROTOCOLS.FUNCTION, ACTIONS.COMPLETE, {
        functionId,
        functionPath: absolutePath,
        result,
        executionTime,
        success: true
      });

    } catch (error) {
      const executionTime = Date.now() - startTime;
      logger.error(`Unexpected error: ${error.message}`);

      // Emit error event
      if (this.emitCallback) {
        try {
          await this.emitCallback({
            type: 'function.error',
            data: {
              functionId,
              error: error.message,
              stack: error.stack,
              event,
              phase: 'unexpected'
            },
            metadata: {
              source: 'FunctionExecutorActor',
              triggeredBy: event?.id,
              depth: (event?.metadata?.depth || 0) + 1
            }
          });
        } catch (emitError) {
          logger.error(`Failed to emit error event: ${emitError.message}`);
        }
      }

      return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
        error: `Unexpected error: ${error.message}`,
        stack: error.stack,
        functionId,
        executionTime,
        phase: 'unexpected'
      });
    }
  }

  /**
   * Execute an agent function (calls Claude CLI as subprocess)
   *
   * @param {Object} params - Execution parameters
   * @param {string} params.functionId - Function identifier
   * @param {string} params.functionPath - Path to agent function config
   * @param {string} [params.agentCommand] - Agent command (default: 'claude')
   * @param {Object} params.event - Event object to pass to agent
   * @param {Object} [params.config] - Additional config
   * @returns {Object} UAP response message
   */
  async executeAgent({ functionId, functionPath, agentCommand = 'claude', event, config = {} }) {
    const startTime = Date.now();
    const logger = new Logger(functionId);

    try {
      logger.info(`Executing agent function: ${functionId}`);
      logger.debug(`Agent command: ${agentCommand}`);

      // Build prompt from event data
      const prompt = this._buildAgentPrompt(event, config);
      logger.debug(`Prompt length: ${prompt.length} chars`);

      // Spawn Claude CLI as subprocess
      const proc = Bun.spawn([agentCommand], {
        stdin: 'pipe',
        stdout: 'pipe',
        stderr: 'pipe'
      });

      // Write prompt to stdin
      proc.stdin.write(prompt);
      proc.stdin.end();

      // Capture stdout
      const stdout = await new Response(proc.stdout).text();
      const stderr = await new Response(proc.stderr).text();

      await proc.exited;

      if (proc.exitCode !== 0) {
        logger.error(`Agent exited with code ${proc.exitCode}`);
        logger.debug(`Stderr: ${stderr}`);

        // Emit error event
        if (this.emitCallback) {
          await this.emitCallback({
            type: 'function.error',
            data: {
              functionId,
              error: `Agent failed with exit code ${proc.exitCode}`,
              stderr,
              event,
              phase: 'agent-execution'
            },
            metadata: {
              source: 'FunctionExecutorActor',
              triggeredBy: event.id,
              depth: (event.metadata?.depth || 0) + 1
            }
          });
        }

        return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
          error: `Agent failed with exit code ${proc.exitCode}`,
          stderr,
          functionId,
          phase: 'agent-execution'
        });
      }

      const executionTime = Date.now() - startTime;
      logger.info(`Agent completed in ${executionTime}ms`);

      // Parse response
      const result = {
        response: stdout.trim(),
        stderr: stderr.trim()
      };

      // Emit success event
      if (this.emitCallback) {
        await this.emitCallback({
          type: 'function.executed',
          data: {
            functionId,
            functionType: 'agent',
            result,
            executionTime,
            event
          },
          metadata: {
            source: 'FunctionExecutorActor',
            triggeredBy: event.id,
            depth: (event.metadata?.depth || 0) + 1
          }
        });
      }

      return createMessage(PROTOCOLS.FUNCTION, ACTIONS.COMPLETE, {
        functionId,
        functionType: 'agent',
        result,
        executionTime,
        success: true
      });

    } catch (error) {
      const executionTime = Date.now() - startTime;
      logger.error(`Agent execution error: ${error.message}`);

      // Emit error event
      if (this.emitCallback) {
        try {
          await this.emitCallback({
            type: 'function.error',
            data: {
              functionId,
              functionType: 'agent',
              error: error.message,
              stack: error.stack,
              event,
              phase: 'agent-error'
            },
            metadata: {
              source: 'FunctionExecutorActor',
              triggeredBy: event?.id,
              depth: (event?.metadata?.depth || 0) + 1
            }
          });
        } catch (emitError) {
          logger.error(`Failed to emit error event: ${emitError.message}`);
        }
      }

      return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
        error: `Agent execution error: ${error.message}`,
        stack: error.stack,
        functionId,
        executionTime,
        phase: 'agent-error'
      });
    }
  }

  /**
   * Build prompt for agent from event data
   * @private
   */
  _buildAgentPrompt(event, config) {
    const parts = [];

    // Add system context if provided
    if (config.systemPrompt) {
      parts.push(config.systemPrompt);
      parts.push('');
    }

    // Add event information
    parts.push('Event to analyze:');
    parts.push(JSON.stringify(event, null, 2));

    // Add task if provided
    if (config.task) {
      parts.push('');
      parts.push('Task:');
      parts.push(config.task);
    }

    return parts.join('\n');
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
        PROTOCOLS.FUNCTION,
        ACTIONS.ERROR,
        { error: validation.error }
      );
    }

    if (message.protocol !== PROTOCOLS.FUNCTION) {
      return createMessage(
        PROTOCOLS.FUNCTION,
        ACTIONS.ERROR,
        {
          error: `Invalid protocol: ${message.protocol}, expected ${PROTOCOLS.FUNCTION}`
        }
      );
    }

    switch (message.action) {
      case ACTIONS.EXECUTE:
        return await this.execute(message.data);

      default:
        return createMessage(
          PROTOCOLS.FUNCTION,
          ACTIONS.ERROR,
          { error: `Unknown action: ${message.action}` }
        );
    }
  }
}

/**
 * Factory function to create FunctionExecutorActor instance
 */
export function createFunctionExecutor(config) {
  return new FunctionExecutorActor(config);
}

export default FunctionExecutorActor;
