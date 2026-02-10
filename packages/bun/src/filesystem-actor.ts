/**
 * FileSystemActor - Filesystem access with path sandboxing
 *
 * Provides sandboxed filesystem operations as an actor.
 * Uses node:fs APIs (compatible with Bun and Node).
 *
 * Features:
 * - Path-level access control (allowedPaths)
 * - Operation-level permissions (read, write, delete, watch)
 * - Path normalization and sandboxing
 * - Atomic writes by default
 * - File watching support
 */

import * as fs from 'node:fs/promises';
import * as fsSync from 'node:fs';
import * as path from 'node:path';
import {
  Actor,
  type Message,
  type MessageResponse,
  type IMessageRouter,
  createResponse,
  createErrorResponse,
} from '@agentic-primer/actors';

export type FileSystemOperation = 'read' | 'write' | 'delete' | 'watch';

export interface FileSystemActorConfig {
  allowedPaths: string[];
  operations: FileSystemOperation[];
}

export class FileSystemActor extends Actor {
  private allowedPaths: string[];
  private operations: Set<FileSystemOperation>;
  private watchers = new Map<string, fsSync.FSWatcher>();

  constructor(id: string, router: IMessageRouter, config: FileSystemActorConfig) {
    super(id, router);
    this.allowedPaths = config.allowedPaths.map(p => path.resolve(p));
    this.operations = new Set(config.operations);
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'fs.read': return await this.handleRead(message, payload);
        case 'fs.write': return await this.handleWrite(message, payload);
        case 'fs.append': return await this.handleAppend(message, payload);
        case 'fs.delete': return await this.handleDelete(message, payload);
        case 'fs.list': return await this.handleList(message, payload);
        case 'fs.exists': return await this.handleExists(message, payload);
        case 'fs.stat': return await this.handleStat(message, payload);
        case 'fs.watch': return await this.handleWatch(message, payload);
        case 'fs.unwatch': return await this.handleUnwatch(message, payload);
        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      this.logError('Filesystem operation failed', { type, error: error.message });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleRead(
    message: Message,
    payload: { path: string; encoding?: 'utf-8' | 'binary' }
  ): Promise<MessageResponse> {
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    const content = await fs.readFile(
      resolved,
      payload.encoding === 'binary' ? null : 'utf-8'
    );

    return createResponse(message, { content });
  }

  private async handleWrite(
    message: Message,
    payload: { path: string; content: string | Buffer; atomic?: boolean }
  ): Promise<MessageResponse> {
    if (!this.operations.has('write')) {
      return createErrorResponse(message, 'Write operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    const dir = path.dirname(resolved);
    await fs.mkdir(dir, { recursive: true });

    if (payload.atomic !== false) {
      const tempPath = `${resolved}.tmp`;
      await fs.writeFile(tempPath, payload.content);
      await fs.rename(tempPath, resolved);
    } else {
      await fs.writeFile(resolved, payload.content);
    }

    return createResponse(message, { written: true });
  }

  private async handleAppend(
    message: Message,
    payload: { path: string; content: string | Buffer }
  ): Promise<MessageResponse> {
    if (!this.operations.has('write')) {
      return createErrorResponse(message, 'Write operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    const dir = path.dirname(resolved);
    await fs.mkdir(dir, { recursive: true });
    await fs.appendFile(resolved, payload.content);

    return createResponse(message, { appended: true });
  }

  private async handleDelete(
    message: Message,
    payload: { path: string }
  ): Promise<MessageResponse> {
    if (!this.operations.has('delete')) {
      return createErrorResponse(message, 'Delete operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    await fs.unlink(resolved);
    return createResponse(message, { deleted: true });
  }

  private async handleList(
    message: Message,
    payload: { path: string; recursive?: boolean; pattern?: string }
  ): Promise<MessageResponse> {
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    const entries = await fs.readdir(resolved, { withFileTypes: true });
    let files = entries.map(e => e.name);

    if (payload.pattern) {
      const regex = this.patternToRegex(payload.pattern);
      files = files.filter(f => regex.test(f));
    }

    if (payload.recursive) {
      const subdirs = entries.filter(e => e.isDirectory());
      for (const subdir of subdirs) {
        const subPath = path.join(resolved, subdir.name);
        const subResponse = await this.handleList(message, {
          path: subPath,
          recursive: true,
          pattern: payload.pattern,
        });
        if (subResponse.success) {
          files.push(
            ...subResponse.payload.files.map((f: string) =>
              path.join(subdir.name, f)
            )
          );
        }
      }
    }

    return createResponse(message, { files });
  }

  private async handleExists(
    message: Message,
    payload: { path: string }
  ): Promise<MessageResponse> {
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    try {
      await fs.access(resolved);
      return createResponse(message, { exists: true });
    } catch {
      return createResponse(message, { exists: false });
    }
  }

  private async handleStat(
    message: Message,
    payload: { path: string }
  ): Promise<MessageResponse> {
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    const stats = await fs.stat(resolved);
    return createResponse(message, {
      size: stats.size,
      isFile: stats.isFile(),
      isDirectory: stats.isDirectory(),
      mtime: stats.mtimeMs,
      ctime: stats.ctimeMs,
    });
  }

  private async handleWatch(
    message: Message,
    payload: { path: string; pattern?: string }
  ): Promise<MessageResponse> {
    if (!this.operations.has('watch')) {
      return createErrorResponse(message, 'Watch operation not permitted');
    }

    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) return createErrorResponse(message, violation);

    const watchId = `watch-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const pattern = payload.pattern ? this.patternToRegex(payload.pattern) : null;

    const watcher = fsSync.watch(resolved, { recursive: true }, (eventType, filename) => {
      if (filename && pattern && !pattern.test(filename)) return;

      if (message.from) {
        this.tell(message.from, 'fs.change', {
          watchId,
          event: {
            type: eventType === 'rename' ? 'modified' : 'modified',
            path: path.join(resolved, filename || ''),
            timestamp: Date.now(),
          },
        }).catch(() => {});
      }
    });

    this.watchers.set(watchId, watcher);
    return createResponse(message, { watchId });
  }

  private async handleUnwatch(
    message: Message,
    payload: { watchId: string }
  ): Promise<MessageResponse> {
    const watcher = this.watchers.get(payload.watchId);
    if (!watcher) {
      return createErrorResponse(message, `Watch not found: ${payload.watchId}`);
    }

    watcher.close();
    this.watchers.delete(payload.watchId);
    return createResponse(message, { unwatched: true });
  }

  private resolvePath(filePath: string): string {
    return path.resolve(filePath);
  }

  private validatePath(resolved: string): string | null {
    const isAllowed = this.allowedPaths.some(allowedPath =>
      resolved.startsWith(allowedPath)
    );
    if (!isAllowed) {
      return `Path access denied: '${resolved}' not in allowedPaths: [${this.allowedPaths.join(', ')}]`;
    }
    return null;
  }

  private patternToRegex(pattern: string): RegExp {
    const escaped = pattern.replace(/[.+^${}()|[\]\\]/g, '\\$&');
    const regex = escaped.replace(/\*/g, '.*').replace(/\?/g, '.');
    return new RegExp(`^${regex}$`);
  }

  async shutdown(): Promise<void> {
    for (const watcher of this.watchers.values()) {
      watcher.close();
    }
    this.watchers.clear();
  }
}
