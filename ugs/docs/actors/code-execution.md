# CodeExecutionActor

Safe code execution with sandboxing and timeout protection for JavaScript and TypeScript.

## Overview

The `CodeExecutionActor` provides a secure environment for executing JavaScript and TypeScript code with strict sandboxing and timeout enforcement. It prevents access to file systems, networks, and process APIs while capturing console output.

## Constructor

```typescript
new CodeExecutionActor(router: MessageRouter, defaultTimeout: number = 5000)
```

**Parameters:**
- `router`: MessageRouter instance for message handling
- `defaultTimeout`: Default execution timeout in milliseconds (default: 5000ms)

**Address:** `@(code-execution)`

## Operations

### execute

Execute JavaScript or TypeScript code in a sandboxed environment.

**Message Type:** `execute`

**Payload:**
```typescript
{
  code: string;                           // Code to execute
  language?: 'javascript' | 'typescript'; // Language (default: 'javascript')
  timeout?: number;                       // Timeout in ms (default: actor's defaultTimeout)
}
```

**Response Payload (Success):**
```typescript
{
  result: any;           // Return value from executed code
  logs: string[];        // Captured console.log output
  executionTime: number; // Execution time in milliseconds
  language: string;      // Language used
}
```

**Response Payload (Error):**
```typescript
{
  logs: string[];  // Captured console output before error
}
```

**Example:**
```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: 'return Math.sqrt(16) + Math.pow(2, 3);',
    language: 'javascript',
    timeout: 3000
  }
});
```

## Safety Features

### Sandboxing

The sandbox restricts access to dangerous APIs:

**Allowed:**
- `Math` - Mathematical operations
- `Date` - Date/time operations
- `JSON` - JSON parsing and stringification
- `Array`, `Object`, `String`, `Number`, `Boolean` - Built-in types
- `Promise` - Promise handling (but not async I/O)
- `console` - Output capture (log, error, warn, info)

**Blocked:**
- `require()` - Module loading
- `import` - Dynamic imports
- `fetch()` - Network requests
- `process` - Process access
- `Bun` - Bun runtime APIs
- `Buffer` - Buffer manipulation
- `global`, `globalThis` - Global scope access
- `setTimeout`, `setInterval` - Async timers (could bypass timeout)
- File system APIs

**Implementation:**
```typescript
private async executeInSandbox(code: string, language: string): Promise<any> {
  const sandbox = {
    console: { log: (...args) => this.captureOutput(...args) },
    Math, Date, JSON, Array, Object, String, Number, Boolean, Promise,
    setTimeout: undefined,
    fetch: undefined,
    require: undefined,
    import: undefined,
    process: undefined,
    Bun: undefined,
    // ... other blocked APIs
  };

  const wrappedCode = `
    with(sandbox) {
      return (function() { ${code} })();
    }
  `;
  const fn = new Function('sandbox', wrappedCode);
  return fn(sandbox);
}
```

### Timeout Protection

Code execution includes timeout protection using Promise.race:

```typescript
return await Promise.race([
  this.executeInSandbox(code, language, timeout),
  this.timeoutPromise(timeout)
]);
```

**Important Limitation:** Timeout only works for:
- Asynchronous operations (Promise-based code)
- Finite synchronous operations that yield control

**Timeout does NOT work for:**
- Synchronous infinite loops (e.g., `while(true) {}`)
- Blocking synchronous operations

This is a JavaScript limitation - synchronous code cannot be interrupted. For production use with untrusted code, consider:
- Using Worker threads with termination capability
- Running code in a separate process
- Static code analysis to detect infinite loops before execution

If code completes within timeout:
- Success response with result is returned
- Console output is captured
- Execution time is measured

### Console Output Capture

Console methods are intercepted to capture output:

```typescript
console: {
  log: (...args) => this.captureOutput(...args),
  error: (...args) => this.captureOutput('ERROR:', ...args),
  warn: (...args) => this.captureOutput('WARN:', ...args),
  info: (...args) => this.captureOutput('INFO:', ...args),
}
```

Output is formatted:
- Objects are JSON-stringified
- Multiple arguments are joined with spaces
- All output is stored in the `logs` array

## Usage Examples

### Basic JavaScript Execution

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      const sum = [1, 2, 3, 4, 5].reduce((a, b) => a + b, 0);
      console.log('Sum:', sum);
      return sum;
    `,
    language: 'javascript'
  }
});

console.log(response.payload.result);      // 15
console.log(response.payload.logs);        // ['Sum: 15']
console.log(response.payload.executionTime); // e.g., 2
```

### TypeScript Execution

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      const multiply = (a: number, b: number): number => {
        return a * b;
      };
      return multiply(6, 7);
    `,
    language: 'typescript'
  }
});

console.log(response.payload.result); // 42
```

### Math Operations

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      const pi = Math.PI;
      const area = pi * Math.pow(5, 2);
      console.log('Circle area (r=5):', area);
      return area;
    `
  }
});
```

### Date Operations

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      const now = new Date();
      const year = now.getFullYear();
      const month = now.getMonth() + 1;
      console.log(\`Current date: \${year}-\${month}\`);
      return { year, month };
    `
  }
});
```

### With Custom Timeout

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      let sum = 0;
      for (let i = 0; i < 1000000; i++) {
        sum += i;
      }
      return sum;
    `,
    timeout: 10000  // 10 second timeout
  }
});
```

### Error Handling

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      console.log('Starting calculation...');
      throw new Error('Something went wrong');
    `
  }
});

if (!response.success) {
  console.error('Execution failed:', response.error);
  console.log('Output before error:', response.payload?.logs);
}
```

## Security Examples

### Blocked: File System Access

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `const fs = require('fs'); return fs.readFileSync('/etc/passwd');`
  }
});

console.log(response.success); // false
console.log(response.error);   // 'Execution error: require is not defined'
```

### Blocked: Network Access

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `const data = await fetch('https://example.com'); return data;`
  }
});

console.log(response.success); // false
console.log(response.error);   // Contains 'fetch is not defined'
```

### Blocked: Process Access

```typescript
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `return process.env.HOME;`
  }
});

console.log(response.success); // false
console.log(response.error);   // Contains 'process is not defined'
```

### Note on Timeout Limitations

Due to JavaScript limitations, synchronous infinite loops cannot be interrupted:

```typescript
// This will hang indefinitely - timeout cannot interrupt synchronous loops
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `while (true) {}`,  // Cannot be interrupted
    timeout: 1000
  }
});

// Timeout works for async operations:
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      await new Promise(resolve => {
        // This would timeout if we had setTimeout
      });
    `,
    timeout: 1000  // This would work if async I/O was enabled
  }
});

// Timeout works for finite operations that complete:
const response = await codeActor.receive({
  type: 'execute',
  payload: {
    code: `
      let sum = 0;
      for (let i = 0; i < 10000000; i++) sum += i;
      return sum;
    `,
    timeout: 5000  // Completes before timeout
  }
});
```

## Known Vulnerabilities (2026-02-03)

⚠️ **CRITICAL: Sandbox Escape Vulnerabilities**

**Status:** [VERIFIED] via automated security testing

The current sandbox implementation **can be completely bypassed** through multiple methods.
**DO NOT USE WITH UNTRUSTED CODE.**

### Confirmed Vulnerabilities

**CE-001: Constructor Chain Escape (CRITICAL)**
```javascript
// Attacker can access process via constructor chain
const AsyncFunction = (async function(){}).constructor;
const func = AsyncFunction('return process');
return func(); // Full access to process, env, filesystem!
```

**CE-002: Function Constructor Escape (CRITICAL)**
```javascript
// Attacker can access global scope
const FunctionConstructor = (function(){}).constructor.constructor;
return FunctionConstructor('return this')(); // Returns globalThis with all APIs
```

**CE-003: Prototype Pollution (HIGH)**
```javascript
// Pollution persists across executions
Object.prototype.polluted = 'EXPLOITED';
// Later execution:
const test = {};
return test.polluted; // Returns 'EXPLOITED' - pollution survived!
```

**CE-004: Global Scope Access via this (CRITICAL)**
```javascript
// Non-strict mode allows global access
return (function() { return this; })(); // Returns globalThis
```

**CE-005: Error Stack Information Leak (MEDIUM)**
```javascript
// Error stacks reveal file system paths
try {
  throw new Error('test');
} catch (e) {
  return e.stack; // Reveals: file:///Users/.../project/...
}
```

### Timeout Limitation (KNOWN LIMITATION)

**CE-L001: Synchronous Infinite Loops Cannot Be Interrupted (INFO)**

The timeout mechanism **CANNOT** stop synchronous infinite loops:

```javascript
// This will run for full 3 seconds, timeout has NO effect
const start = Date.now();
while (Date.now() - start < 3000) {
  // Busy wait
}
return 'Loop completed'; // Completes after 3000ms, not 1000ms timeout
```

**Why:** JavaScript is single-threaded. `Promise.race()` only works if code yields
control (async operations). Synchronous loops never yield, so the timeout promise
never gets a chance to reject.

**Impact:** Denial of service through infinite loops that hang the process.

### Test Results Summary

```
Total Tests:     10
Passed:          4  (import.meta, eval, Symbol, Proxy)
Vulnerabilities: 5  (3 CRITICAL, 1 HIGH, 1 MEDIUM)
Limitations:     1  (Timeout ineffective)

Security Status: SANDBOX CAN BE ESCAPED
```

### Recommendations

1. **DO NOT USE WITH UNTRUSTED CODE** until architectural fix is implemented
2. **Use Worker threads** for production (complete isolation, can terminate sync loops)
3. **Static analysis** to detect dangerous patterns before execution
4. **Rate limiting** to prevent resource exhaustion

### Planned Remediation

**Short-term:**
- Add strict mode: `'use strict';` (blocks CE-004)
- Sanitize error stacks (fixes CE-005)
- Document limitations clearly

**Long-term:**
- Migrate to Worker threads (fixes CE-001, CE-002, CE-003, CE-L001)
- Separate process per execution (complete isolation)
- Termination support for infinite loops

**Tracking:** See SECURITY_FINDINGS.md for complete vulnerability report, proof-of-concepts,
and automated test results.

---

## Limitations

### No External Modules

Code cannot import or require external modules:

```typescript
// These will fail:
import axios from 'axios';     // Not supported
const lodash = require('lodash'); // Not supported
```

### No Async I/O

While Promises are available, async I/O operations are blocked:

```typescript
// Promises work:
const result = await Promise.resolve(42); // ✓ Works

// But I/O is blocked:
await fetch('https://api.example.com');   // ✗ Fails
await Bun.file('data.txt').text();        // ✗ Fails
```

### No Persistent State

Each execution starts with a fresh sandbox:

```typescript
// First execution
await codeActor.receive({
  type: 'execute',
  payload: { code: 'let x = 42;' }
});

// Second execution - x is not defined
await codeActor.receive({
  type: 'execute',
  payload: { code: 'return x;' }  // Error: x is not defined
});
```

### TypeScript Limitations

TypeScript support is basic - type annotations are stripped but:
- No type checking is performed
- Complex TypeScript features may not work
- Consider transpiling complex TS code before submission

## Integration with Actor System

### Using Router

```typescript
const response = await router.ask({
  to: '@(code-execution)',
  type: 'execute',
  payload: {
    code: 'return Math.random();'
  }
});
```

### Using Actor's Ask Method

```typescript
const myActor = new Actor('my-actor', router);
const result = await myActor.ask(
  '@(code-execution)',
  'execute',
  {
    code: 'return [1, 2, 3].reduce((a, b) => a + b);',
    timeout: 2000
  }
);
```

## Error Messages

Common error messages:

- `No code provided` - Missing code in payload
- `Unsupported language: <lang>` - Language not 'javascript' or 'typescript'
- `Execution timed out after ${timeout}ms` - Code exceeded timeout
- `Execution error: <error>` - Runtime error during execution
- `Expected message type 'execute', got '<type>'` - Wrong message type

## Performance Considerations

- **Execution Overhead:** Each execution creates a new sandbox (~1-5ms overhead)
- **Memory:** Returned values must fit in memory
- **Timeout Default:** 5000ms default - adjust based on expected workload
- **Console Output:** Large console output increases memory usage
- **No Optimization:** Code is not compiled/optimized before execution

## Best Practices

1. **Set appropriate timeouts:** Use shorter timeouts for simple operations
2. **Validate code length:** Consider limiting input code size
3. **Handle errors gracefully:** Always check `response.success`
4. **Capture console output:** Use logs for debugging and monitoring
5. **Return serializable data:** Avoid returning functions or complex objects
6. **Test security:** Regularly verify sandbox restrictions are enforced
7. **Document limitations:** Make users aware of what's not supported

## Use Cases

### Safe User Scripts

Allow users to write custom logic without security risks:

```typescript
const userScript = getUserScript(); // From user input
const result = await codeActor.receive({
  type: 'execute',
  payload: { code: userScript, timeout: 3000 }
});
```

### Formula Evaluation

Evaluate mathematical expressions safely:

```typescript
const formula = 'Math.sqrt(a * a + b * b)'; // User-provided
const code = `const a = 3; const b = 4; return ${formula};`;
const result = await codeActor.receive({
  type: 'execute',
  payload: { code }
});
```

### Data Transformation

Transform data with user-defined logic:

```typescript
const data = [1, 2, 3, 4, 5];
const transformCode = `
  const data = ${JSON.stringify(data)};
  return data.map(x => x * 2).filter(x => x > 5);
`;
const result = await codeActor.receive({
  type: 'execute',
  payload: { code: transformCode }
});
```

## See Also

- [Actor System Documentation](../actor.md)
- [Message Protocol](../message.md)
- [FileSystemActor](./filesystem.md)
