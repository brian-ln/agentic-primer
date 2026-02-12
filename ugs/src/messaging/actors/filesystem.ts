#!/usr/bin/env bun
/**
 * FileSystemActor - Safe file system operations with path validation
 *
 * Provides read, write, list, and delete operations with directory traversal protection.
 * All paths are validated against a base directory to prevent unauthorized access.
 */

import { readdir, unlink } from 'node:fs/promises';
import { realpathSync, lstatSync, readlinkSync } from 'node:fs';
import { resolve, relative, join } from 'node:path';
import { MessageRouter } from '../router.ts';
import { Actor } from '../actor.ts';
import {
  type Message,
  type MessageResponse,
} from '@agentic-primer/actors';

/**
 * FileSystemActor - Manages file operations within a sandboxed directory
 */
export class FileSystemActor extends Actor {
  private basePath: string;

  constructor(router: MessageRouter, basePath: string = './data') {
    super('filesystem', router);
    this.basePath = resolve(basePath);
  }

  /**
   * Validate and resolve a path to ensure it's within the base directory
   * Prevents directory traversal attacks and symlink escapes
   *
   * Security: Uses realpathSync to resolve symlinks before validation (FS-001, FS-002, FS-003)
   */
  private validatePath(path: string): string {
    const resolved = resolve(this.basePath, path);

    // Check if path is a symlink (even if target doesn't exist)
    let realPath: string;
    try {
      const stats = lstatSync(resolved);
      if (stats.isSymbolicLink()) {
        // Symlink exists - resolve its target
        const target = readlinkSync(resolved);
        // If target is relative, resolve it relative to the symlink's directory
        realPath = resolve(resolve(resolved, '..'), target);
      } else {
        // Not a symlink - use realpath to handle any parent symlinks
        realPath = realpathSync(resolved);
      }
    } catch (error) {
      // File/symlink doesn't exist yet
      // Use realpath on existing parent directory to catch parent symlinks
      try {
        const parent = resolve(resolved, '..');
        const basename = resolve(resolved).split('/').pop() || '';
        const realParent = realpathSync(parent);
        realPath = join(realParent, basename);
      } catch {
        // Parent doesn't exist either - just validate resolved path
        realPath = resolved;
      }
    }

    if (!realPath.startsWith(this.basePath)) {
      throw new Error('Path outside allowed directory');
    }
    return realPath;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      switch (message.type) {
        case 'read_file':
          return await this.handleReadFile(message);
        case 'write_file':
          return await this.handleWriteFile(message);
        case 'list_dir':
          return await this.handleListDir(message);
        case 'delete_file':
          return await this.handleDeleteFile(message);
        default:
          return {
            id: message.id + '_response',
            correlationId: message.correlationId || message.id,
            from: this.address,
            to: message.from || this.address,
            success: false,
            error: `Unknown message type: ${message.type}`,
            timestamp: Date.now(),
          };
      }
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

  /**
   * Read a file's contents
   */
  private async handleReadFile(message: Message): Promise<MessageResponse> {
    const { path, encoding = 'utf-8' } = message.payload;

    if (!path) {
      throw new Error('No path provided');
    }

    const validatedPath = this.validatePath(path);
    const file = Bun.file(validatedPath);

    // Check if file exists
    if (!await file.exists()) {
      throw new Error(`File not found: ${path}`);
    }

    const content = await file.text();

    return {
      id: message.id + '_response',
      correlationId: message.correlationId || message.id,
      from: this.address,
      to: message.from || this.address,
      success: true,
      payload: {
        path,
        content,
        size: file.size,
      },
      timestamp: Date.now(),
    };
  }

  /**
   * Write content to a file
   */
  private async handleWriteFile(message: Message): Promise<MessageResponse> {
    const { path, content, encoding = 'utf-8' } = message.payload;

    if (!path) {
      throw new Error('No path provided');
    }

    if (content === undefined) {
      throw new Error('No content provided');
    }

    const validatedPath = this.validatePath(path);
    await Bun.write(validatedPath, content);

    const file = Bun.file(validatedPath);

    return {
      id: message.id + '_response',
      correlationId: message.correlationId || message.id,
      from: this.address,
      to: message.from || this.address,
      success: true,
      payload: {
        path,
        size: file.size,
      },
      timestamp: Date.now(),
    };
  }

  /**
   * List directory contents
   */
  private async handleListDir(message: Message): Promise<MessageResponse> {
    const { path = '.' } = message.payload;

    const validatedPath = this.validatePath(path);
    const entries = await readdir(validatedPath, { withFileTypes: true });

    const files = entries.map(entry => ({
      name: entry.name,
      type: entry.isDirectory() ? 'directory' : 'file',
      path: join(path, entry.name),
    }));

    return {
      id: message.id + '_response',
      correlationId: message.correlationId || message.id,
      from: this.address,
      to: message.from || this.address,
      success: true,
      payload: {
        path,
        entries: files,
      },
      timestamp: Date.now(),
    };
  }

  /**
   * Delete a file
   */
  private async handleDeleteFile(message: Message): Promise<MessageResponse> {
    const { path } = message.payload;

    if (!path) {
      throw new Error('No path provided');
    }

    const validatedPath = this.validatePath(path);

    // Check if file exists
    const file = Bun.file(validatedPath);
    if (!await file.exists()) {
      throw new Error(`File not found: ${path}`);
    }

    await unlink(validatedPath);

    return {
      id: message.id + '_response',
      correlationId: message.correlationId || message.id,
      from: this.address,
      to: message.from || this.address,
      success: true,
      payload: {
        path,
        deleted: true,
      },
      timestamp: Date.now(),
    };
  }
}
