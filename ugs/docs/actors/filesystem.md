# FileSystemActor

Safe file system operations with path validation and directory traversal protection.

## Overview

The `FileSystemActor` provides a secure interface for file operations within a sandboxed directory. All paths are validated to prevent directory traversal attacks and unauthorized access to files outside the designated base path.

## Constructor

```typescript
new FileSystemActor(router: MessageRouter, basePath: string = './data')
```

**Parameters:**
- `router`: MessageRouter instance for message handling
- `basePath`: Base directory for all file operations (default: './data')

**Address:** `@(filesystem)`

## Operations

### read_file

Read the contents of a file.

**Message Type:** `read_file`

**Payload:**
```typescript
{
  path: string;        // Relative path within base directory
  encoding?: string;   // File encoding (default: 'utf-8')
}
```

**Response Payload:**
```typescript
{
  path: string;    // Original path
  content: string; // File contents
  size: number;    // File size in bytes
}
```

**Example:**
```typescript
const response = await fsActor.receive({
  type: 'read_file',
  payload: {
    path: 'documents/readme.txt'
  }
});
```

### write_file

Write content to a file (creates or overwrites).

**Message Type:** `write_file`

**Payload:**
```typescript
{
  path: string;        // Relative path within base directory
  content: string;     // Content to write
  encoding?: string;   // File encoding (default: 'utf-8')
}
```

**Response Payload:**
```typescript
{
  path: string;  // Original path
  size: number;  // Written file size in bytes
}
```

**Example:**
```typescript
const response = await fsActor.receive({
  type: 'write_file',
  payload: {
    path: 'output.txt',
    content: 'Hello, World!'
  }
});
```

### list_dir

List contents of a directory.

**Message Type:** `list_dir`

**Payload:**
```typescript
{
  path?: string;  // Relative directory path (default: '.')
}
```

**Response Payload:**
```typescript
{
  path: string;
  entries: Array<{
    name: string;              // Entry name
    type: 'file' | 'directory'; // Entry type
    path: string;              // Relative path
  }>;
}
```

**Example:**
```typescript
const response = await fsActor.receive({
  type: 'list_dir',
  payload: {
    path: 'documents'
  }
});
```

### delete_file

Delete a file.

**Message Type:** `delete_file`

**Payload:**
```typescript
{
  path: string;  // Relative path within base directory
}
```

**Response Payload:**
```typescript
{
  path: string;    // Original path
  deleted: boolean; // Always true on success
}
```

**Example:**
```typescript
const response = await fsActor.receive({
  type: 'delete_file',
  payload: {
    path: 'temp/cache.json'
  }
});
```

## Security Considerations

### Path Validation

All paths are validated using the `validatePath()` method:

```typescript
private validatePath(path: string): string {
  const resolved = resolve(this.basePath, path);
  if (!resolved.startsWith(this.basePath)) {
    throw new Error('Path outside allowed directory');
  }
  return resolved;
}
```

This prevents directory traversal attacks such as:
- `../../../etc/passwd`
- `../../secret.key`
- `/absolute/path/to/file`

### What's Protected

- **Directory Traversal:** Attempts to access files outside basePath are blocked
- **Absolute Paths:** All paths are resolved relative to basePath
- **Symbolic Links:** Resolution follows symlinks, but final path must be within basePath

### What's Not Protected

- **File Overwrites:** write_file will overwrite existing files without warning
- **Concurrent Access:** No file locking mechanism for concurrent operations
- **Disk Space:** No quota enforcement on write operations

### Known Vulnerabilities (2026-02-03)

⚠️ **CRITICAL: Symlink Path Traversal Vulnerability**

**Status:** [VERIFIED] via automated security testing

The current implementation is **vulnerable to symlink-based path traversal attacks**.
Symlinks inside basePath pointing outside basePath will bypass validation, allowing
unauthorized file access.

**Vulnerability Details:**
- **FS-001:** Symlink to file outside basePath (CRITICAL)
- **FS-002:** Symlink in subdirectory (CRITICAL)
- **FS-003:** Write via symlink to arbitrary location (CRITICAL)

**Example Attack:**
```bash
# Attacker creates symlink inside allowed directory
ln -s /etc/passwd ./data/evil-link

# FileSystemActor reads /etc/passwd via 'evil-link'
const response = await fsActor.receive({
  type: 'read_file',
  payload: { path: 'evil-link' }
});
// SUCCESS: Returns contents of /etc/passwd
```

**Root Cause:**
Path validation checks the symlink path, not the symlink target:
```typescript
const resolved = resolve(this.basePath, path);  // Resolves to ./data/evil-link
if (!resolved.startsWith(this.basePath)) {      // ✓ Passes check
  throw new Error('Path outside allowed directory');
}
// But Bun.file() follows symlink to actual target!
```

**Recommendation:**
- **DO NOT USE WITH UNTRUSTED PATHS** until patched
- See SECURITY_FINDINGS.md for full details and remediation plan

**Planned Fix:**
```typescript
import { realpathSync } from 'node:fs';

private validatePath(path: string): string {
  const resolved = resolve(this.basePath, path);

  // Resolve symlinks BEFORE validation
  let realPath: string;
  try {
    realPath = realpathSync(resolved);
  } catch {
    realPath = resolved;  // File doesn't exist - allow for write operations
  }

  if (!realPath.startsWith(this.basePath)) {
    throw new Error('Path outside allowed directory');
  }
  return realPath;
}
```

**Tracking:** See SECURITY_FINDINGS.md for complete vulnerability report and test results.

## Usage Examples

### Basic Usage

```typescript
import { MessageRouter } from './src/messaging/router.ts';
import { FileSystemActor } from './src/messaging/actors/filesystem.ts';

const router = new MessageRouter(store, programManager);
const fsActor = new FileSystemActor(router, './data');
router.registerActor('filesystem', fsActor);

// Write a file
await fsActor.receive({
  type: 'write_file',
  payload: { path: 'config.json', content: '{}' }
});

// Read it back
const result = await fsActor.receive({
  type: 'read_file',
  payload: { path: 'config.json' }
});
console.log(result.payload.content); // '{}'
```

### Error Handling

```typescript
try {
  const response = await fsActor.receive({
    type: 'read_file',
    payload: { path: 'nonexistent.txt' }
  });

  if (!response.success) {
    console.error('Error:', response.error);
    // Error: File not found: nonexistent.txt
  }
} catch (error) {
  console.error('Unexpected error:', error);
}
```

### Directory Operations

```typescript
// Create directory structure by writing files
await fsActor.receive({
  type: 'write_file',
  payload: {
    path: 'projects/my-app/index.ts',
    content: 'console.log("Hello");'
  }
});

// List directory contents
const listing = await fsActor.receive({
  type: 'list_dir',
  payload: { path: 'projects' }
});

listing.payload.entries.forEach(entry => {
  console.log(`${entry.name} (${entry.type})`);
});
```

### Security Example

```typescript
// This will be blocked by path validation
const maliciousResponse = await fsActor.receive({
  type: 'read_file',
  payload: { path: '../../../etc/passwd' }
});

console.log(maliciousResponse.success); // false
console.log(maliciousResponse.error);   // 'Path outside allowed directory'
```

## Integration with Actor System

```typescript
// Using ask pattern through router
const response = await router.ask({
  to: '@(filesystem)',
  type: 'read_file',
  payload: { path: 'data.json' }
});

// Using actor's ask method
const myActor = new Actor('my-actor', router);
const result = await myActor.ask(
  '@(filesystem)',
  'write_file',
  { path: 'output.txt', content: 'data' }
);
```

## Error Messages

Common error messages:

- `No path provided` - Missing path in payload
- `No content provided` - Missing content in write_file payload
- `File not found: <path>` - File doesn't exist for read/delete operations
- `Path outside allowed directory` - Directory traversal attempt blocked
- `Unknown message type: <type>` - Unsupported operation

## Performance Considerations

- **File Size:** No explicit limits on file size, constrained by available memory
- **Concurrent Operations:** Each message is handled sequentially
- **Directory Listing:** Performance degrades with large directories
- **Path Resolution:** Minimal overhead from path validation

## Best Practices

1. **Use relative paths:** Always provide paths relative to basePath
2. **Check response.success:** Always verify operation success before using payload
3. **Handle errors gracefully:** File operations can fail for many reasons
4. **Avoid sensitive paths:** Don't point basePath to system directories
5. **Consider permissions:** Ensure the process has appropriate file system permissions

## See Also

- [Actor System Documentation](../actor.md)
- [Message Protocol](../message.md)
- [CodeExecutionActor](./code-execution.md)
