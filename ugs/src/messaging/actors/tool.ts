#!/usr/bin/env bun
/**
 * ToolActor - Executes tools (bash, file ops, etc.)
 *
 * Each tool is an actor that can execute operations.
 * Examples: @(tool-bash), @(tool-read), @(tool-write)
 */

import { exec } from 'child_process';
import { promisify } from 'util';
import { readFile, writeFile } from 'fs/promises';
import { MessageRouter } from '../router.ts';
import { Actor } from '../actor.ts';
import {
  type Message,
  type MessageResponse,
} from '@agentic-primer/actors';

const execAsync = promisify(exec);

/**
 * BashToolActor - Executes bash commands
 */
export class BashToolActor extends Actor {
  constructor(router: MessageRouter) {
    super('tool-bash', router);
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type !== 'execute') {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: `Expected message type 'execute', got '${message.type}'`,
        timestamp: Date.now(),
      };
    }

    try {
      const { command, timeout = 30000 } = message.payload;

      if (!command) {
        throw new Error('No command provided');
      }

      const { stdout, stderr } = await execAsync(command, {
        timeout,
        maxBuffer: 1024 * 1024 * 10, // 10MB
      });

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: {
          stdout: stdout.trim(),
          stderr: stderr.trim(),
          exitCode: 0,
        },
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: error.message,
        payload: {
          stdout: error.stdout?.trim() || '',
          stderr: error.stderr?.trim() || error.message,
          exitCode: error.code || 1,
        },
        timestamp: Date.now(),
      };
    }
  }
}

/**
 * ReadToolActor - Reads files
 */
export class ReadToolActor extends Actor {
  constructor(router: MessageRouter) {
    super('tool-read', router);
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type !== 'read') {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: `Expected message type 'read', got '${message.type}'`,
        timestamp: Date.now(),
      };
    }

    try {
      const { path, encoding = 'utf-8' } = message.payload;

      if (!path) {
        throw new Error('No path provided');
      }

      const content = await readFile(path, encoding);

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: {
          path,
          content,
          size: Buffer.byteLength(content, encoding),
        },
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }
}

/**
 * WriteToolActor - Writes files
 */
export class WriteToolActor extends Actor {
  constructor(router: MessageRouter) {
    super('tool-write', router);
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type !== 'write') {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: `Expected message type 'write', got '${message.type}'`,
        timestamp: Date.now(),
      };
    }

    try {
      const { path, content, encoding = 'utf-8' } = message.payload;

      if (!path) {
        throw new Error('No path provided');
      }

      if (content === undefined) {
        throw new Error('No content provided');
      }

      await writeFile(path, content, encoding);

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: {
          path,
          size: Buffer.byteLength(content, encoding),
        },
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }
}
