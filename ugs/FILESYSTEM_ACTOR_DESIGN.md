# FileSystemActor Design (Pure Actor Model)

**Date:** 2026-02-06
**Status:** Design Phase
**Branch:** feature/path-addressing
**Bead:** simplify-0u7.2

---

## Overview

FileSystemActor is a **system actor** that provides filesystem access with internal path sandboxing. Like StorageActor, it enforces its own access control - no helper classes needed.

**Core Principle:** FileSystemActor validates paths internally. Access control = routing decisions.

---

## Pure Actor Model Architecture

### What Changed

**Old Approach (Helper Classes):**
```typescript
// Helper class validates paths
class FileSystemCapability {
  private router: MessageRouter;
  private allowedPaths: string[];

  async read(path: string) {
    this.validatePath(path); // Validation in helper
    return await this.router.ask(fsActor, 'fs.read', { path });
  }
}

// Actors compose helpers
class TaskActor extends Actor {
  private fs: FileSystemCapability; // Helper instance
}
```

**New Approach (Pure Actor):**
```typescript
// FileSystemActor validates internally
class FileSystemActor extends Actor {
  private allowedPaths: string[];

  async receive(message: Message) {
    if (message.type === 'fs.read') {
      if (!this.isPathAllowed(message.payload.path)) {
        return createErrorResponse(message, 'Path access denied');
      }
      // Read file
    }
  }
}

// Access control = routing
router.registerActor('/workflows/system/fs', workflowsFs);
// NOT registered for /domain → access denied by absence
```

---

## Design Principles

### 1. System Actors ARE Capabilities

FileSystemActor is not a wrapper - it IS the filesystem capability. Path sandboxing happens at construction.

```typescript
const workflowsFs = new FileSystemActor('workflows-fs', router, {
  allowedPaths: ['/workflows/config', '/workflows/data'],
  operations: ['read', 'write']
});
```

### 2. Routing Determines Access

If an actor path is registered, access is granted. If not registered, access is implicitly denied.

```typescript
// Workflows can access their filesystem
router.registerActor('/workflows/system/fs', workflowsFs);

// Domain has its own filesystem (different paths)
router.registerActor('/domain/system/fs', domainFs);

// No filesystem for /untrusted → messages fail with "Actor not found"
```

### 3. Internal Path Validation

FileSystemActor validates every message against its allowedPaths before execution.

```typescript
async receive(message: Message): Promise<MessageResponse> {
  if (message.type === 'fs.read') {
    const path = message.payload.path;

    // Internal validation (resolve symlinks, prevent ../ escapes)
    const resolved = this.resolvePath(path);
    if (!this.isPathAllowed(resolved)) {
      return createErrorResponse(message,
        `Path access denied: '${path}' not in allowedPaths`
      );
    }

    // Read file
    const content = await fs.readFile(resolved, 'utf-8');
    return createSuccessResponse(message, { content });
  }
}
```

### 4. Message-Based Protocol

All filesystem operations are messages. No direct method calls.

```typescript
// Read
await actor.ask(address('/workflows/system/fs'), 'fs.read', {
  path: '/workflows/config/settings.json'
});

// Write
await actor.ask(address('/workflows/system/fs'), 'fs.write', {
  path: '/workflows/data/state.json',
  content: JSON.stringify(state)
});

// List
await actor.ask(address('/workflows/system/fs'), 'fs.list', {
  path: '/workflows/data',
  recursive: true
});
```

---

## FileSystemActor Interface

### Constructor

```typescript
export class FileSystemActor extends Actor {
  private allowedPaths: string[];
  private operations: Set<FileSystemOperation>;

  constructor(
    id: string,
    router: MessageRouter,
    config: FileSystemActorConfig
  ) {
    super(id, router);
    this.allowedPaths = config.allowedPaths.map(p => path.resolve(p));
    this.operations = new Set(config.operations);
  }
}
```

### Configuration

```typescript
export interface FileSystemActorConfig {
  /** Allowed paths (sandboxed directories) */
  allowedPaths: string[];

  /** Allowed operations */
  operations: FileSystemOperation[];
}

export type FileSystemOperation = 'read' | 'write' | 'delete' | 'watch';
```

### Message Protocol

**fs.read** (Read file)
```typescript
{
  type: 'fs.read',
  payload: {
    path: '/workflows/config/settings.json',
    encoding: 'utf-8'
  }
}

// Response
{
  success: true,
  payload: {
    content: '{ "version": "1.0" }'
  }
}
```

**fs.write** (Write file, atomic by default)
```typescript
{
  type: 'fs.write',
  payload: {
    path: '/workflows/data/state.json',
    content: '{ "status": "running" }',
    atomic: true
  }
}

// Response
{
  success: true,
  payload: { written: true }
}
```

**fs.delete** (Delete file)
```typescript
{
  type: 'fs.delete',
  payload: {
    path: '/workflows/data/temp.json'
  }
}

// Response
{
  success: true,
  payload: { deleted: true }
}
```

**fs.list** (List directory)
```typescript
{
  type: 'fs.list',
  payload: {
    path: '/workflows/data',
    recursive: false,
    pattern: '*.json'
  }
}

// Response
{
  success: true,
  payload: {
    files: ['state.json', 'config.json']
  }
}
```

**fs.stat** (Get file metadata)
```typescript
{
  type: 'fs.stat',
  payload: {
    path: '/workflows/config/settings.json'
  }
}

// Response
{
  success: true,
  payload: {
    size: 1024,
    isFile: true,
    isDirectory: false,
    mtime: 1706304000000,
    ctime: 1706304000000
  }
}
```

**fs.watch** (Watch for changes)
```typescript
{
  type: 'fs.watch',
  payload: {
    path: '/workflows/data',
    pattern: '*.json'
  }
}

// Response
{
  success: true,
  payload: {
    watchId: 'watch-abc123'
  }
}

// Change events sent to caller as messages
{
  type: 'fs.change',
  payload: {
    watchId: 'watch-abc123',
    event: {
      type: 'modified',
      path: '/workflows/data/state.json',
      timestamp: 1706304100000
    }
  }
}
```

---

## Implementation

### Core Logic

```typescript
import * as fs from 'node:fs/promises';
import * as fsSync from 'node:fs';
import * as path from 'node:path';
import { Actor } from './actor.ts';
import { type Message, type MessageResponse, createSuccessResponse, createErrorResponse } from './message.ts';

export class FileSystemActor extends Actor {
  private allowedPaths: string[];
  private operations: Set<FileSystemOperation>;
  private watchers = new Map<string, fsSync.FSWatcher>();

  constructor(id: string, router: MessageRouter, config: FileSystemActorConfig) {
    super(id, router);
    // Resolve all paths at construction (prevents escapes)
    this.allowedPaths = config.allowedPaths.map(p => path.resolve(p));
    this.operations = new Set(config.operations);
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      if (type === 'fs.read') {
        return await this.handleRead(message, payload);
      }

      if (type === 'fs.write') {
        return await this.handleWrite(message, payload);
      }

      if (type === 'fs.delete') {
        return await this.handleDelete(message, payload);
      }

      if (type === 'fs.list') {
        return await this.handleList(message, payload);
      }

      if (type === 'fs.stat') {
        return await this.handleStat(message, payload);
      }

      if (type === 'fs.watch') {
        return await this.handleWatch(message, payload);
      }

      if (type === 'fs.unwatch') {
        return await this.handleUnwatch(message, payload);
      }

      return createErrorResponse(message, `Unknown message type: ${type}`);
    } catch (error: any) {
      this.logError('Filesystem operation failed', {
        type,
        error: error.message
      });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleRead(
    message: Message,
    payload: { path: string; encoding?: 'utf-8' | 'binary' }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    // Validate path
    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Read file
    const content = await fs.readFile(
      resolved,
      payload.encoding === 'binary' ? null : 'utf-8'
    );

    return createSuccessResponse(message, { content });
  }

  private async handleWrite(
    message: Message,
    payload: { path: string; content: string | Buffer; atomic?: boolean }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('write')) {
      return createErrorResponse(message, 'Write operation not permitted');
    }

    // Validate path
    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Write file (atomic by default)
    if (payload.atomic !== false) {
      const tempPath = `${resolved}.tmp`;
      await fs.writeFile(tempPath, payload.content);
      await fs.rename(tempPath, resolved);
    } else {
      await fs.writeFile(resolved, payload.content);
    }

    return createSuccessResponse(message, { written: true });
  }

  private async handleDelete(
    message: Message,
    payload: { path: string }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('delete')) {
      return createErrorResponse(message, 'Delete operation not permitted');
    }

    // Validate path
    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Delete file
    await fs.unlink(resolved);

    return createSuccessResponse(message, { deleted: true });
  }

  private async handleList(
    message: Message,
    payload: { path: string; recursive?: boolean; pattern?: string }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    // Validate path
    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // List directory
    const entries = await fs.readdir(resolved, { withFileTypes: true });
    let files = entries.map(e => e.name);

    // Apply pattern filter if specified
    if (payload.pattern) {
      const regex = this.patternToRegex(payload.pattern);
      files = files.filter(f => regex.test(f));
    }

    // Recursive listing
    if (payload.recursive) {
      const subdirs = entries.filter(e => e.isDirectory());
      for (const subdir of subdirs) {
        const subPath = path.join(resolved, subdir.name);
        const subResponse = await this.handleList(message, {
          path: subPath,
          recursive: true,
          pattern: payload.pattern
        });
        if (subResponse.success) {
          files.push(...subResponse.payload.files.map((f: string) =>
            path.join(subdir.name, f)
          ));
        }
      }
    }

    return createSuccessResponse(message, { files });
  }

  private async handleStat(
    message: Message,
    payload: { path: string }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    // Validate path
    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Get file stats
    const stats = await fs.stat(resolved);

    return createSuccessResponse(message, {
      size: stats.size,
      isFile: stats.isFile(),
      isDirectory: stats.isDirectory(),
      mtime: stats.mtimeMs,
      ctime: stats.ctimeMs
    });
  }

  private async handleWatch(
    message: Message,
    payload: { path: string; pattern?: string }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('watch')) {
      return createErrorResponse(message, 'Watch operation not permitted');
    }

    // Validate path
    const resolved = this.resolvePath(payload.path);
    const violation = this.validatePath(resolved);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Create watcher
    const watchId = `watch-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const pattern = payload.pattern ? this.patternToRegex(payload.pattern) : null;

    const watcher = fsSync.watch(resolved, { recursive: true }, (eventType, filename) => {
      if (filename && pattern && !pattern.test(filename)) {
        return; // Filtered out by pattern
      }

      // Send change event to caller
      this.tell(message.from, 'fs.change', {
        watchId,
        event: {
          type: eventType === 'rename' ? 'modified' : 'modified',
          path: path.join(resolved, filename || ''),
          timestamp: Date.now()
        }
      }).catch(() => {}); // Ignore send failures
    });

    this.watchers.set(watchId, watcher);

    return createSuccessResponse(message, { watchId });
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

    return createSuccessResponse(message, { unwatched: true });
  }

  /**
   * Resolve path to absolute (prevents ../ escapes)
   */
  private resolvePath(filePath: string): string {
    return path.resolve(filePath);
  }

  /**
   * Validate path against allowedPaths
   * @returns Error message if validation fails, null if OK
   */
  private validatePath(resolved: string): string | null {
    const isAllowed = this.allowedPaths.some(allowedPath =>
      resolved.startsWith(allowedPath)
    );

    if (!isAllowed) {
      return `Path access denied: '${resolved}' not in allowedPaths: [${this.allowedPaths.join(', ')}]`;
    }

    return null;
  }

  /**
   * Check if path is allowed
   */
  private isPathAllowed(resolved: string): boolean {
    return this.allowedPaths.some(allowedPath =>
      resolved.startsWith(allowedPath)
    );
  }

  /**
   * Convert glob pattern to regex
   */
  private patternToRegex(pattern: string): RegExp {
    const escaped = pattern.replace(/[.+^${}()|[\]\\]/g, '\\$&');
    const regex = escaped.replace(/\*/g, '.*').replace(/\?/g, '.');
    return new RegExp(`^${regex}$`);
  }
}

export type FileSystemOperation = 'read' | 'write' | 'delete' | 'watch';

export interface FileSystemActorConfig {
  allowedPaths: string[];
  operations: FileSystemOperation[];
}
```

---

## Usage Examples

### Setup: Register FileSystemActors

```typescript
const router = new MessageRouter(store, programManager);

// Workflows namespace filesystem
const workflowsFs = new FileSystemActor('workflows-fs', router, {
  allowedPaths: ['/workflows/config', '/workflows/data'],
  operations: ['read', 'write', 'delete']
});
router.registerActor('/workflows/system/fs', workflowsFs);

// Domain namespace filesystem (read-only)
const domainFs = new FileSystemActor('domain-fs', router, {
  allowedPaths: ['/domain/config'],
  operations: ['read'] // Read-only
});
router.registerActor('/domain/system/fs', domainFs);

// Session knowledge filesystem
const knowledgeFs = new FileSystemActor('knowledge-fs', router, {
  allowedPaths: ['/session-logs'],
  operations: ['read', 'watch'] // Read + watch, no write
});
router.registerActor('/session-knowledge/system/fs', knowledgeFs);
```

### Actors Use FileSystemActor via Messages

```typescript
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'save-state') {
      // Send message to FileSystemActor
      const response = await this.ask(
        address('/workflows/system/fs'),
        'fs.write',
        {
          path: '/workflows/data/state.json',
          content: JSON.stringify({ status: 'running' })
        }
      );

      if (!response.success) {
        return createErrorResponse(message, response.error);
      }

      return createSuccessResponse(message, { saved: true });
    }
  }
}
```

### Path Sandboxing in Action

```typescript
// ✅ Allowed: Path within allowedPaths
await actor.ask(address('/workflows/system/fs'), 'fs.read', {
  path: '/workflows/config/settings.json'
});
// Success: File read

// ❌ Denied: Path outside allowedPaths
await actor.ask(address('/workflows/system/fs'), 'fs.read', {
  path: '/etc/passwd'
});
// Error: Path access denied: '/etc/passwd' not in allowedPaths

// ❌ Denied: Escape attempt via ../
await actor.ask(address('/workflows/system/fs'), 'fs.read', {
  path: '/workflows/config/../../../etc/passwd'
});
// Error: Path access denied: '/etc/passwd' not in allowedPaths
// (path.resolve() normalizes to /etc/passwd)
```

---

## Error Handling

### Access Denied Errors

```typescript
// Path not in allowedPaths
{
  success: false,
  error: "Path access denied: '/etc/passwd' not in allowedPaths: [/workflows/config, /workflows/data]"
}

// Operation not permitted
{
  success: false,
  error: "Write operation not permitted"
}

// Actor not found (routing denial)
{
  success: false,
  error: "Actor not found: /workflows/system/fs"
}
```

### Clear Error Messages

All errors include:
- **What was denied:** Full resolved path
- **Why it was denied:** Not in allowedPaths or operation not permitted
- **What is allowed:** List of allowedPaths or operations

---

## Security Features

### 1. Path Normalization

All paths are resolved to absolute paths using `path.resolve()`, preventing:
- Relative path escapes (`../../../etc/passwd`)
- Symlink following (resolved to target)
- Current directory ambiguity (`./ vs no prefix`)

### 2. Sandboxing

Paths are checked against allowedPaths prefix matching:
```typescript
const resolved = path.resolve(requestedPath);
const isAllowed = this.allowedPaths.some(allowedPath =>
  resolved.startsWith(allowedPath)
);
```

This ensures paths stay within sandboxed directories.

### 3. Atomic Writes

By default, writes are atomic (write to temp file, then rename):
```typescript
await fs.writeFile(`${path}.tmp`, content);
await fs.rename(`${path}.tmp`, path);
```

This prevents partial writes from corrupting files.

---

## Comparison: Old vs New

### Old Design (Helper Classes)

```typescript
// ❌ Complexity: Two layers of validation
class FileSystemCapability {
  async read(path: string) {
    this.validatePath(path); // Layer 1: Helper validation
    await this.router.ask(fsActor, ...); // Layer 2: Actor receives pre-validated
  }
}

// ❌ Coupling: Actors depend on capability helpers
class TaskActor extends Actor {
  private fs: FileSystemCapability; // Dependency on helper class
}
```

### New Design (Pure Actors)

```typescript
// ✅ Simplicity: Single layer of validation (in actor)
class FileSystemActor extends Actor {
  async receive(message: Message) {
    const resolved = this.resolvePath(message.payload.path);
    this.validatePath(resolved); // Single validation point
    // Execute
  }
}

// ✅ No coupling: Actors use standard messaging
class TaskActor extends Actor {
  async receive(message: Message) {
    const response = await this.ask(
      address('/workflows/system/fs'),
      'fs.read',
      { path: '...' }
    ); // Standard actor messaging
  }
}
```

---

## Benefits

### 1. Simpler Architecture
- No helper classes
- Single validation point (in actor)
- Standard actor messaging throughout

### 2. Clearer Access Control
- Access = routing registration
- No access = actor not registered
- Explicit and auditable

### 3. Stronger Security
- Path normalization at actor level
- Sandboxing enforced by actor
- Atomic writes built-in

### 4. Better Testability
- Mock router, not helper classes
- Test actors in isolation
- No capability configuration needed in tests

---

## Next Steps

1. ✅ Design approved (this document)
2. Implement FileSystemActor (simplify-0u7.4)
3. Create tests for path sandboxing
4. Document migration from helper classes
5. Update existing code to use FileSystemActor

---

**Document End**
