#!/usr/bin/env bun
/**
 * FileSystemCapability - Scoped filesystem access
 *
 * Provides path-level and operation-level scoped access to filesystem.
 * Enforces sandboxing before routing to system filesystem actors.
 *
 * @example
 * ```typescript
 * const fs = new FileSystemCapability(router, {
 *   namespace: '/workflows',
 *   allowedPaths: ['/workflows/config', '/workflows/data'],
 *   operations: ['read', 'write']
 * });
 *
 * // ✅ Allowed
 * await fs.read('/workflows/config/settings.json');
 *
 * // ❌ Denied
 * await fs.read('/etc/passwd'); // Path not in allowedPaths
 * ```
 */

import * as path from 'path';
import type { MessageRouter } from '../router.ts';
import { type Address, address, createMessage } from '../message.ts';
import { CapabilityError } from './storage.ts';

/**
 * FileSystemCapability - Scoped filesystem access
 */
export class FileSystemCapability {
  private router: MessageRouter;
  private fsAddress: Address;
  private config: FileSystemCapabilityConfig;

  constructor(router: MessageRouter, config: FileSystemCapabilityConfig) {
    this.router = router;
    this.config = config;
    this.fsAddress = address(`${config.namespace}/system/fs`);
  }

  /**
   * Read file contents
   * Requires: 'read' operation
   * Enforces: allowedPaths restriction
   */
  async read(
    filePath: string,
    options?: { encoding?: 'utf-8' | 'binary' }
  ): Promise<string | Buffer> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(filePath);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(
        this.fsAddress,
        'fs.read',
        {
          path: filePath,
          encoding: options?.encoding || 'utf-8',
        },
        { pattern: 'ask', from: address('capability/filesystem') }
      )
    );

    if (!response.success) {
      throw new Error(`Filesystem read failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Write file contents (atomic)
   * Requires: 'write' operation
   * Enforces: allowedPaths restriction
   */
  async write(
    filePath: string,
    content: string | Buffer,
    options?: { atomic?: boolean }
  ): Promise<void> {
    // Check operation permission
    if (!this.config.operations.includes('write')) {
      throw new CapabilityError('write', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(filePath);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(
        this.fsAddress,
        'fs.write',
        {
          path: filePath,
          content,
          atomic: options?.atomic ?? true, // Atomic by default
        },
        { pattern: 'ask', from: address('capability/filesystem') }
      )
    );

    if (!response.success) {
      throw new Error(`Filesystem write failed: ${response.error}`);
    }
  }

  /**
   * Delete file
   * Requires: 'delete' operation
   * Enforces: allowedPaths restriction
   */
  async delete(filePath: string): Promise<void> {
    // Check operation permission
    if (!this.config.operations.includes('delete')) {
      throw new CapabilityError('delete', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(filePath);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(
        this.fsAddress,
        'fs.delete',
        { path: filePath },
        { pattern: 'ask', from: address('capability/filesystem') }
      )
    );

    if (!response.success) {
      throw new Error(`Filesystem delete failed: ${response.error}`);
    }
  }

  /**
   * List directory contents
   * Requires: 'read' operation
   * Enforces: allowedPaths restriction
   */
  async list(
    dirPath: string,
    options?: { recursive?: boolean; pattern?: string }
  ): Promise<string[]> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(dirPath);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(
        this.fsAddress,
        'fs.list',
        {
          path: dirPath,
          recursive: options?.recursive ?? false,
          pattern: options?.pattern,
        },
        { pattern: 'ask', from: address('capability/filesystem') }
      )
    );

    if (!response.success) {
      throw new Error(`Filesystem list failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Get file metadata (stat)
   * Requires: 'read' operation
   * Enforces: allowedPaths restriction
   */
  async stat(filePath: string): Promise<FileStats> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(filePath);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(
        this.fsAddress,
        'fs.stat',
        { path: filePath },
        { pattern: 'ask', from: address('capability/filesystem') }
      )
    );

    if (!response.success) {
      throw new Error(`Filesystem stat failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Watch file/directory for changes
   * Requires: 'watch' operation
   * Enforces: allowedPaths restriction
   */
  async watch(
    watchPath: string,
    options: {
      pattern?: string;
      onChange: (event: FileChangeEvent) => void;
    }
  ): Promise<{ unwatch: () => void }> {
    // Check operation permission
    if (!this.config.operations.includes('watch')) {
      throw new CapabilityError('watch', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(watchPath);

    // Route to filesystem actor
    // Note: In real implementation, need to create callback mechanism
    const response = await this.router.ask(
      createMessage(
        this.fsAddress,
        'fs.watch',
        {
          path: watchPath,
          pattern: options.pattern,
          // Callback mechanism TBD in Phase 3
        },
        { pattern: 'ask', from: address('capability/filesystem') }
      )
    );

    if (!response.success) {
      throw new Error(`Filesystem watch failed: ${response.error}`);
    }

    return {
      unwatch: () => {
        this.router
          .tell(
            createMessage(
              this.fsAddress,
              'fs.unwatch',
              { watchId: response.payload.watchId },
              { pattern: 'tell', from: address('capability/filesystem') }
            )
          )
          .catch(() => {}); // Ignore errors
      },
    };
  }

  /**
   * Validate path against allowedPaths
   *
   * Checks if path is within one of the allowed directories.
   * Resolves relative paths and prevents ../ escapes.
   */
  private validatePath(filePath: string): void {
    // Resolve to absolute path (prevents ../ escapes)
    const resolved = path.resolve(filePath);

    // Check against allowedPaths
    const isAllowed = this.config.allowedPaths.some(allowedPath => {
      const resolvedAllowed = path.resolve(allowedPath);
      return resolved.startsWith(resolvedAllowed);
    });

    if (!isAllowed) {
      throw new CapabilityError(
        'path-access',
        `Path '${filePath}' not in allowedPaths: [${this.config.allowedPaths.join(', ')}]`
      );
    }
  }
}

/**
 * FileSystemCapability configuration
 */
export interface FileSystemCapabilityConfig {
  /** Actor namespace (e.g., '/workflows') */
  namespace: string;

  /** Allowed paths (sandboxed directories) */
  allowedPaths: string[];

  /** Allowed operations */
  operations: FileSystemOperation[];
}

export type FileSystemOperation = 'read' | 'write' | 'delete' | 'watch';

export interface FileStats {
  size: number;
  isFile: boolean;
  isDirectory: boolean;
  mtime: number;
  ctime: number;
}

export interface FileChangeEvent {
  type: 'created' | 'modified' | 'deleted';
  path: string;
  timestamp: number;
}
