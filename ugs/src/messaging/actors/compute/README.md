# Compute Actors

Compute actors are a specialized category of actors that execute computational workloads with varying levels of isolation and security.

## Overview

The compute actor pattern provides a unified interface for executing code, formulas, rules, and other computational tasks with configurable security and isolation levels.

## Current Implementations

### Code Execution Actors

Three implementations of code execution with different isolation strategies:

#### 1. `UnsafeCodeComputeActor` (`unsafe-code.ts`)

**Security Level:** ⚠️ UNSAFE - Known vulnerabilities

**Use Cases:**
- Demo/testing with trusted code only
- Internal tools where code source is controlled
- High-frequency executions where speed is critical (0.1ms overhead)

**Known Vulnerabilities:**
- CE-001: Constructor chain escapes
- CE-002: Function.constructor escapes
- CE-003: Prototype pollution persistence
- CE-004: Global scope access via 'this'
- Cannot interrupt synchronous infinite loops

**Configuration:**
```typescript
import { UnsafeCodeComputeActor } from './actors/compute/unsafe-code.ts';

const actor = new UnsafeCodeComputeActor(router, {
  timeout: 5000,
  iUnderstandThisIsUnsafe: true,  // Required acknowledgment
  staticAnalysis: 'strict',        // Optional: 'off' | 'warn' | 'strict'
});
```

#### 2. `WorkerCodeComputeActor` (`worker-code.ts`)

**Security Level:** 🟡 MODERATE - Thread-level isolation

**Use Cases:**
- Protecting against infinite loops (can terminate worker threads)
- Preventing prototype pollution (worker terminates after execution)
- Better resource management than unsafe mode
- Trusted code with some isolation needs

**Advantages:**
- ✅ Can terminate infinite loops
- ✅ No prototype pollution persistence
- ✅ Better resource isolation (~10-50ms overhead)

**Limitations:**
- ⚠️ Bun Workers may share process space with parent
- ⚠️ Constructor escapes may still be possible in Bun

**Configuration:**
```typescript
import { WorkerCodeComputeActor } from './actors/compute/worker-code.ts';

const actor = new WorkerCodeComputeActor(router, 5000); // timeout in ms
```

#### 3. `SubprocessCodeComputeActor` (`subprocess-code.ts`)

**Security Level:** ✅ MAXIMUM - Process-level isolation (Production-Ready)

**Use Cases:**
- Production environments with untrusted code
- Multi-tenant platforms
- Maximum security requirements
- Long-running code execution

**Advantages:**
- ✅ Complete process isolation (no shared memory)
- ✅ Can kill synchronous infinite loops via process termination
- ✅ No constructor chain escapes possible
- ✅ No prototype pollution possible
- ✅ Clean resource cleanup on timeout
- ✅ Independent memory space

**Trade-offs:**
- ~10-50ms overhead (process spawn + IPC)
- Additional process memory (~30MB per execution)

**Configuration:**
```typescript
import { SubprocessCodeComputeActor } from './actors/compute/subprocess-code.ts';

const actor = new SubprocessCodeComputeActor(router, 5000); // timeout in ms
```

## Choosing the Right Actor

| Requirement | Recommended Actor |
|------------|------------------|
| Trusted code, maximum speed | `UnsafeCodeComputeActor` |
| Infinite loop protection | `WorkerCodeComputeActor` or `SubprocessCodeComputeActor` |
| Untrusted code | `SubprocessCodeComputeActor` |
| Production with user-generated code | `SubprocessCodeComputeActor` |
| Internal tools, known code sources | `UnsafeCodeComputeActor` with `staticAnalysis: 'strict'` |

## Worker Scripts

The `workers/` subdirectory contains the execution environments:

- `code-executor-worker.ts` - Web Worker execution environment
- `code-executor-subprocess.ts` - Subprocess execution environment

These are spawned by their respective actor implementations and handle the actual code execution in isolated contexts.

## Future Extensibility

The compute actor pattern is designed to support additional computational workload types:

### Planned Implementations

#### Formula Compute Actors
Execute spreadsheet-style formulas with cell references and functions:
```typescript
// Future: FormulaComputeActor
const formulaActor = new FormulaComputeActor(router);
// Execute: =SUM(A1:A10) + IF(B1>100, "High", "Low")
```

#### Rules Compute Actors
Execute business rules and decision logic:
```typescript
// Future: RulesComputeActor
const rulesActor = new RulesComputeActor(router);
// Execute: IF customer.age >= 18 AND customer.credit_score > 700 THEN approve_loan
```

#### Expression Compute Actors
Evaluate mathematical and logical expressions safely:
```typescript
// Future: ExpressionComputeActor
const exprActor = new ExpressionComputeActor(router);
// Execute: (x + y) * 2 where x=5, y=10
```

#### Query Compute Actors
Execute database-style queries over in-memory data:
```typescript
// Future: QueryComputeActor
const queryActor = new QueryComputeActor(router);
// Execute: SELECT * FROM users WHERE age > 25 ORDER BY name
```

## Adding New Compute Types

To add a new compute actor type:

1. **Create the actor file** in `src/messaging/actors/compute/`:
   ```typescript
   // Example: formula-compute.ts
   export class FormulaComputeActor extends Actor {
     constructor(router: MessageRouter, config?: FormulaConfig) {
       super('formula-execution', router);
       // ...
     }

     async receive(message: Message): Promise<MessageResponse> {
       // Handle formula execution
     }
   }
   ```

2. **Follow the naming pattern**: `*ComputeActor` (e.g., `FormulaComputeActor`)

3. **Implement the Actor interface**:
   - Constructor accepts `MessageRouter` and optional config
   - Implement `receive(message: Message)` method
   - Return `MessageResponse` with execution results

4. **Add worker scripts** (if needed) in `workers/` subdirectory

5. **Export from index** (if creating a public API)

6. **Add tests** following existing patterns (`test-*.ts`)

7. **Update this README** with usage examples

## Architecture Pattern

All compute actors follow this pattern:

```text
┌─────────────────────────────────────────────────────┐
│              Compute Actor                          │
│  (UnsafeCode/Worker/Subprocess/Formula/Rules)       │
├─────────────────────────────────────────────────────┤
│                                                     │
│  receive(message: Message)                          │
│    ├─ Validate message type                        │
│    ├─ Extract payload (code/formula/rules)          │
│    ├─ Choose execution strategy                     │
│    │   ├─ Direct execution (unsafe)                 │
│    │   ├─ Worker thread (worker)                    │
│    │   └─ Subprocess (subprocess)                   │
│    ├─ Execute with timeout protection               │
│    ├─ Capture output/logs                          │
│    └─ Return MessageResponse                        │
│                                                     │
└─────────────────────────────────────────────────────┘
```

## Message Protocol

All compute actors use a consistent message protocol:

**Request Message:**
```typescript
{
  id: string,
  from: string,
  to: string,
  type: 'execute',
  payload: {
    code: string,        // or formula, rule, expression, etc.
    language?: string,   // 'javascript' | 'typescript' | etc.
    timeout?: number,    // milliseconds
    // ... type-specific fields
  },
  timestamp: number
}
```

**Response Message:**
```typescript
{
  id: string,
  correlationId: string,
  from: string,
  to: string,
  success: boolean,
  payload?: {
    result: any,           // execution result
    logs: string[],        // captured console output
    executionTime: number, // ms
    // ... type-specific fields
  },
  error?: string,          // if success === false
  timestamp: number
}
```

## Testing

Each compute actor should have comprehensive tests:

- Basic execution (happy path)
- Error handling
- Timeout enforcement
- Security vulnerability tests (where applicable)
- Performance benchmarks
- Edge cases

See existing test files for patterns:
- `test-code-execution-security.ts`
- `test-worker-code-execution.ts`
- `subprocess-code-execution.test.ts`

## Performance Benchmarks

See `src/messaging/benchmarks/tool-actors.bench.ts` for performance testing patterns.

## Security Considerations

When implementing new compute actors:

1. **Default to secure** - Start with the most secure isolation level
2. **Explicit opt-in for unsafe** - Require acknowledgment for reduced security
3. **Timeout all executions** - Always enforce timeout limits
4. **Sanitize errors** - Remove internal paths and implementation details
5. **Validate inputs** - Check message types and payload structure
6. **Document trade-offs** - Clearly explain security vs performance
7. **Test attack vectors** - Verify protection against common exploits

## References

- Security findings: See `SECURITY_FINDINGS.md` in project root
- Actor pattern: See `src/messaging/actor.ts`
- Message router: See `src/messaging/router.ts`
- Message protocol: See `src/messaging/message.ts`
