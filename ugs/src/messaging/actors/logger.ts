#!/usr/bin/env bun
/**
 * Logger Actor - Message-based logging infrastructure
 *
 * Provides async, testable, routable logging via actor messages.
 * Supports multiple log levels and can be easily mocked in tests.
 */

import { Actor } from '../actor.ts';
import type { Message, MessageResponse } from '@agentic-primer/actors';
import { createResponse } from '@agentic-primer/actors';
import type { MessageRouter } from '../router.ts';

/**
 * Log levels
 */
export type LogLevel = 'debug' | 'info' | 'warn' | 'error';

/**
 * Log message payload
 */
export interface LogMessage {
  level: LogLevel;
  message: string;
  context?: Record<string, any>;
  timestamp?: number;
}

/**
 * LoggerActor - Receives and processes log messages
 *
 * Message types:
 * - 'log.debug' - Debug-level messages
 * - 'log.info' - Info-level messages
 * - 'log.warn' - Warning messages
 * - 'log.error' - Error messages
 */
export class LoggerActor extends Actor {
  private minLevel: LogLevel;
  private levelPriority: Record<LogLevel, number> = {
    debug: 0,
    info: 1,
    warn: 2,
    error: 3,
  };

  constructor(
    router: MessageRouter,
    options: {
      minLevel?: LogLevel;
    } = {}
  ) {
    super('logger', router);
    this.minLevel = options.minLevel || 'debug';
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    // Handle log messages
    if (type.startsWith('log.')) {
      const level = type.slice(4) as LogLevel;
      const logMsg: LogMessage = {
        level,
        message: payload.message || payload,
        context: payload.context,
        timestamp: payload.timestamp || Date.now(),
      };

      await this.log(logMsg);

      return createResponse(message, { success: true });
    }

    return createResponse(message, {
      success: false,
      error: `Unknown message type: ${type}`
    });
  }

  /**
   * Process log message
   */
  private async log(logMsg: LogMessage): Promise<void> {
    // Filter by minimum level
    if (this.levelPriority[logMsg.level] < this.levelPriority[this.minLevel]) {
      return;
    }

    // Format message
    const timestamp = new Date(logMsg.timestamp!).toISOString();
    const levelStr = logMsg.level.toUpperCase().padEnd(5);
    const contextStr = logMsg.context
      ? ` ${JSON.stringify(logMsg.context)}`
      : '';

    const formatted = `[${timestamp}] ${levelStr} ${logMsg.message}${contextStr}`;

    // Output to console
    switch (logMsg.level) {
      case 'debug':
        console.debug(formatted);
        break;
      case 'info':
        console.info(formatted);
        break;
      case 'warn':
        console.warn(formatted);
        break;
      case 'error':
        console.error(formatted);
        break;
    }
  }
}

/**
 * SilentLoggerActor - Discards all log messages (for tests)
 */
export class SilentLoggerActor extends Actor {
  constructor(router: MessageRouter) {
    super('logger', router);
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Silently discard all log messages
    return createResponse(message, { success: true });
  }
}
