# SubprocessCodeExecutionActor

**Production-ready code execution with maximum isolation**

## Quick Start

```typescript
import { MessageRouter } from '../router.ts';
import { SubprocessCodeExecutionActor } from './subprocess-code-execution.ts';

// Create actor
const router = new MessageRouter();
const actor = new SubprocessCodeExecutionActor(router, 5000);

// Execute code
const response = await actor.receive({
  id: 'req-1',
  from: 'user',
  to: 'code-execution',
  type: 'execute',
  payload: {
    code: '1 + 1',
    language: 'javascript',
    timeout: 1000
  },
  timestamp: Date.now()
});

console.log(response.payload.result); // 2
```

## Why Use This Actor?

✅ **Maximum Security** - Complete process isolation
✅ **Handles Infinite Loops** - Can kill synchronous `while(true)` loops
✅ **Production Ready** - Safe for untrusted code execution
✅ **Drop-in Replacement** - Same protocol as UnsafeCodeExecutionActor

## Key Advantage: Infinite Loop Protection

```typescript
// This code WILL BE KILLED after timeout
const response = await actor.receive({
  type: 'execute',
  payload: {
    code: 'while(true) {}',  // Infinite loop
    timeout: 1000
  }
});

// Result:
// success: false
// error: "Execution timed out after 1000ms (process killed)"
// killedByTimeout: true
```

## Features

- **Process Isolation**: Each execution runs in separate Bun subprocess
- **Timeout Protection**: Kills process after timeout (handles sync loops!)
- **Console Capture**: Captures console.log, error, warn, info
- **TypeScript Support**: Strips type annotations automatically
- **Error Handling**: Clean error messages, no path disclosure
- **Resource Cleanup**: Process terminates cleanly, no leaks

## Testing

```bash
# Run tests
bun run test:subprocess

# Run demo
bun run demo:subprocess

# Or directly
bun test src/messaging/actors/subprocess-code-execution.test.ts
bun run src/messaging/actors/demo-subprocess.ts
```

## Performance

- **Overhead**: ~10ms per execution (process spawn + IPC)
- **Memory**: ~30MB per subprocess
- **Suitable for**: Production with untrusted code
- **Not suitable for**: High-frequency microbenchmarks (<1ms)

## Security

All known vulnerabilities addressed:
- ✅ CE-001: Constructor chain escapes (blocked)
- ✅ CE-002: Function.constructor escapes (blocked)
- ✅ CE-003: Prototype pollution (impossible)
- ✅ CE-004: Global scope access (blocked)
- ✅ CE-005: Error information leaks (sanitized)

Dangerous globals blocked:
- `process`, `Bun`, `fetch`, `require`, `Buffer`, etc.

## Files

- `subprocess-code-execution.ts` - Main actor
- `workers/code-executor-subprocess.ts` - Subprocess worker
- `subprocess-code-execution.test.ts` - Test suite (13 tests)
- `demo-subprocess.ts` - Interactive demo

## Documentation

- `../../../CODE_EXECUTION_ACTORS_COMPARISON.md` - Compare all actors
- `../../../SECURITY_FINDINGS.md` - Security analysis
- `../../../IMPLEMENTATION_SUMMARY_SUBPROCESS.md` - Implementation details

## When to Use

**Use SubprocessCodeExecutionActor when:**
- Code comes from untrusted sources (users, plugins)
- Code might contain infinite loops
- Security is important
- Production deployment

**Use UnsafeCodeExecutionActor when:**
- Internal testing with fully trusted code only
- Performance benchmarking
- **NEVER** with user-provided code

**Use WorkerCodeExecutionActor when:**
- Performance critical (<5ms required)
- Code is trusted
- Can guarantee no infinite loops

## Message Protocol

### Request
```typescript
{
  type: 'execute',
  payload: {
    code: 'const x = 1 + 1; x',
    language: 'javascript', // or 'typescript'
    timeout: 5000 // optional, default: 5000ms
  }
}
```

### Success Response
```typescript
{
  success: true,
  payload: {
    result: 2,
    logs: ['console output'],
    executionTime: 10,
    language: 'javascript'
  }
}
```

### Error Response
```typescript
{
  success: false,
  error: 'Execution timed out after 5000ms (process killed)',
  payload: {
    logs: ['console output before error'],
    executionTime: 5000,
    killedByTimeout: true
  }
}
```

## Examples

### Basic Execution
```typescript
const response = await actor.receive({
  type: 'execute',
  payload: { code: '2 + 2' }
});
// result: 4
```

### Console Output
```typescript
const response = await actor.receive({
  type: 'execute',
  payload: {
    code: `
      console.log('Starting...');
      const result = 42;
      console.log('Result:', result);
      result
    `
  }
});
// result: 42
// logs: ['Starting...', 'Result: 42']
```

### TypeScript
```typescript
const response = await actor.receive({
  type: 'execute',
  payload: {
    code: `
      const greet = (name: string): string => {
        return 'Hello, ' + name;
      };
      greet('World')
    `,
    language: 'typescript'
  }
});
// result: "Hello, World"
```

### Error Handling
```typescript
const response = await actor.receive({
  type: 'execute',
  payload: {
    code: 'throw new Error("Something failed")'
  }
});
// success: false
// error: "Something failed"
```

### Timeout Protection (CRITICAL)
```typescript
const response = await actor.receive({
  type: 'execute',
  payload: {
    code: 'while(true) {}',
    timeout: 1000
  }
});
// success: false
// error: "Execution timed out after 1000ms (process killed)"
// killedByTimeout: true
```

## Implementation Details

### How It Works

1. **Spawn subprocess** using `Bun.spawn()`
2. **Send code** via stdin as JSON: `{ code, language, timeout }`
3. **Set timeout** using `setTimeout(() => proc.kill(), timeout)`
4. **Read result** from stdout as JSON
5. **Clean up** subprocess automatically terminates

### Architecture

```
Parent Process (Actor)
  |
  ├─> Spawn subprocess via Bun.spawn()
  ├─> Send JSON to stdin: { code, language, timeout }
  ├─> Set timeout with proc.kill()
  ├─> Read JSON from stdout: { success, result, logs }
  └─> Process terminates (clean resource cleanup)

Subprocess (Worker)
  |
  ├─> Read stdin for JSON
  ├─> Parse { code, language }
  ├─> Execute code in isolated process
  ├─> Capture console output
  └─> Write JSON to stdout: { success, result, logs }
```

### Communication Protocol

**Parent → Subprocess (stdin):**
```json
{
  "code": "const x = 1 + 1; x",
  "language": "javascript",
  "timeout": 5000
}
```

**Subprocess → Parent (stdout):**
```json
{
  "success": true,
  "result": 2,
  "logs": []
}
```

## Migration Guide

### From UnsafeCodeExecutionActor

```typescript
// Before (UNSAFE)
import { UnsafeCodeExecutionActor } from './unsafe-code-execution.ts';
const actor = new UnsafeCodeExecutionActor(router, 5000);

// After (SAFE)
import { SubprocessCodeExecutionActor } from './subprocess-code-execution.ts';
const actor = new SubprocessCodeExecutionActor(router, 5000);

// Message protocol is IDENTICAL - no other changes needed!
```

### Performance Consideration

- Unsafe: ~0.1ms overhead
- Subprocess: ~10ms overhead
- **Trade-off**: 100x slower, but infinitely more secure
- **Verdict**: ~10ms is acceptable for executing untrusted code

## Limitations

1. **Not suitable for high-frequency microbenchmarks**
   - ~10ms overhead per execution
   - Process spawn + IPC overhead
   - Use WorkerCodeExecutionActor if <5ms required

2. **Memory overhead**
   - ~30MB per subprocess (Bun runtime)
   - Not ideal for thousands of concurrent executions
   - Fine for typical use cases (<100 concurrent)

3. **No persistent state**
   - Each execution is independent
   - No shared state between executions
   - This is by design for security

## Security Status

**Production-Ready**: ✅ SAFE for untrusted code

All critical vulnerabilities addressed:
- Constructor chain escapes: **FIXED**
- Function.constructor escapes: **FIXED**
- Prototype pollution: **FIXED**
- Global scope access: **FIXED**
- Infinite loop DoS: **FIXED**
- Error information leaks: **FIXED**

**Recommendation**: Use this actor for all production deployments involving untrusted code.

## Contributing

When modifying this actor:
1. Run tests: `bun run test:subprocess`
2. Run demo: `bun run demo:subprocess`
3. Verify infinite loop test passes
4. Update documentation if protocol changes
5. Add new test cases for new features

## License

MIT (same as parent project)

---

**Status**: Production-Ready ✅
**Security Level**: Maximum 🔒
**Last Updated**: 2026-02-04
