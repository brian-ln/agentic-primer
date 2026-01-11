# FunctionExecutorActor - FIT Decision Tables

FIT-style decision tables for testing FunctionExecutorActor behavior declaratively.

## Table 1: Lifecycle State Transitions

Tests the core lifecycle methods: start(), stop(), getStatus()

| Initial State | Action | Expected State | Expected Success | Expected Error/Message |
|--------------|--------|----------------|------------------|------------------------|
| stopped | start() | running | true | FunctionExecutorActor started |
| stopped | stop() | stopped | true | was not running |
| stopped | getStatus() | stopped | - | isRunning=false, hasEmitCallback=false |
| running | start() | running | false | already running |
| running | stop() | stopped | true | FunctionExecutorActor stopped |
| running | getStatus() | running | - | isRunning=true |

## Table 2: Execute Code Function - Valid Inputs

Tests successful execution of code-based functions.

| Function Path | Event | Function Returns | Expected Success | Expected Result |
|--------------|-------|------------------|------------------|-----------------|
| /valid/func.js | {type: 'test'} | "success" | true | "success" |
| /valid/func.js | {type: 'test', data: {}} | {processed: true} | true | {processed: true} |
| /valid/func.js | {type: 'test'} | null | true | null |
| /valid/func.js | {type: 'test'} | undefined | true | undefined |
| /valid/async.js | {type: 'test'} | Promise<"async"> | true | "async" |

## Table 3: Execute Code Function - Input Validation

Tests input validation for function execution.

| Function ID | Function Path | Event | Expected Success | Expected Error Contains |
|------------|--------------|-------|------------------|------------------------|
| null | /valid/func.js | {type: 'test'} | false | functionId is required |
| "valid" | null | {type: 'test'} | false | functionPath is required |
| "valid" | /valid/func.js | null | false | event is required |
| "" | /valid/func.js | {type: 'test'} | false | functionId is required |
| "valid" | "" | {type: 'test'} | false | functionPath is required |

## Table 4: Execute Code Function - Module Loading Errors

Tests error handling for module import failures.

| Function Path | File Exists | Module Valid | Expected Success | Expected Error Phase |
|--------------|-------------|--------------|------------------|---------------------|
| /valid/func.js | yes | yes | true | - |
| /missing/func.js | no | - | false | import |
| /invalid/syntax.js | yes | no | false | import |
| /not/js/file.txt | yes | no | false | import |
| relative/path.js | yes | yes | true | - (resolved) |

## Table 5: Execute Code Function - Module Export Validation

Tests validation of module exports.

| Module Export | Expected Success | Expected Error Phase |
|--------------|------------------|---------------------|
| default: function | true | - |
| default: async function | true | - |
| default: arrow function | true | - |
| default: null | false | validation |
| default: undefined | false | validation |
| default: object | false | validation |
| default: string | false | validation |
| (no default export) | false | validation |

## Table 6: Execute Code Function - Execution Context

Tests that execution context is correctly provided to functions.

| Context Field | Expected Available | Expected Type |
|--------------|-------------------|--------------|
| emit | yes | function |
| logger | yes | object with info/warn/error/debug |
| config | yes | object (merged) |
| event | yes (as parameter) | object |

## Table 7: Execute Code Function - Emit Callback

Tests event emission from within functions.

| Emit Callback Set | Function Calls emit() | Expected Behavior |
|------------------|----------------------|-------------------|
| yes | yes | Event emitted with metadata |
| yes | no | No events emitted |
| no | yes | Warning logged, no emission |
| no | no | No events emitted |

## Table 8: Execute Code Function - Execution Errors

Tests error handling during function execution.

| Function Behavior | Expected Success | Expected Error Phase | Error Event Emitted |
|------------------|------------------|---------------------|-------------------|
| throws Error | false | execution | yes |
| throws async Error | false | execution | yes |
| returns normally | true | - | no |
| infinite loop (timeout) | false | execution | yes |
| references undefined | false | execution | yes |

## Table 9: Execute Code Function - Success Events

Tests that success events are emitted after successful execution.

| Function Executes | Emit Callback Set | Expected Event Type | Expected Event Data |
|------------------|------------------|---------------------|-------------------|
| successfully | yes | function.executed | functionId, result, executionTime, event |
| successfully | no | none | - |
| with error | yes | function.error | functionId, error, stack, event, phase |

## Table 10: Execute Agent Function - Valid Execution

Tests successful execution of agent-based functions.

| Agent Command | Event | Agent Exit Code | Agent Stdout | Expected Success | Expected Result |
|--------------|-------|----------------|--------------|------------------|-----------------|
| claude | {type: 'test'} | 0 | "Analysis complete" | true | {response: "Analysis complete"} |
| gpt | {type: 'test'} | 0 | "Response text" | true | {response: "Response text"} |
| custom-cli | {type: 'test'} | 0 | "Done" | true | {response: "Done"} |

## Table 11: Execute Agent Function - Agent Failures

Tests error handling for agent execution failures.

| Agent Command | Agent Exit Code | Agent Stderr | Expected Success | Expected Error Phase |
|--------------|----------------|--------------|------------------|---------------------|
| claude | 1 | "API error" | false | agent-execution |
| claude | 2 | "Rate limit" | false | agent-execution |
| missing-cli | - | - | false | agent-error |
| claude | 127 | "Command not found" | false | agent-execution |

## Table 12: Execute Agent Function - Prompt Building

Tests that prompts are correctly built for agent execution.

| System Prompt | Task | Event | Expected Prompt Contains |
|--------------|------|-------|-------------------------|
| "You are a helper" | "Analyze this" | {type: 'test'} | system prompt, event JSON, task |
| null | "Analyze this" | {type: 'test'} | event JSON, task |
| "You are a helper" | null | {type: 'test'} | system prompt, event JSON |
| null | null | {type: 'test'} | event JSON only |

## Table 13: Execute Agent Function - Event Emission

Tests that agent execution emits appropriate events.

| Agent Exit Code | Emit Callback Set | Expected Event Type | Event Data Contains |
|----------------|------------------|---------------------|-------------------|
| 0 | yes | function.executed | functionId, functionType=agent, result, executionTime |
| 1 | yes | function.error | functionId, functionType=agent, error, stderr, phase |
| 0 | no | none | - |

## Table 14: Function Type Routing

Tests that execution is routed to correct handler based on function type.

| Function Type | Expected Handler | Expected Execution Path |
|--------------|-----------------|------------------------|
| code | execute() | Module loading → execution |
| agent | executeAgent() | Prompt building → subprocess |
| undefined | execute() (default) | Module loading → execution |
| invalid | execute() | Module loading → execution |

## Table 15: Handle Message - Protocol Validation

Tests UAP message handling and protocol validation.

| Protocol | Action | Expected Success | Expected Behavior |
|----------|--------|------------------|-------------------|
| FUNCTION | EXECUTE | varies | Calls execute() |
| WRONG | EXECUTE | false | Invalid protocol error |
| FUNCTION | INVALID | false | Unknown action error |

## Table 16: Handle Message - Execute Action

Tests message-based function execution.

| Message Data | Expected Success | Expected Behavior |
|-------------|------------------|-------------------|
| {functionId: "f1", functionPath: "/test.js", event: {...}} | varies | Code execution |
| {functionId: "f1", functionPath: "/test.js", functionType: "agent", event: {...}} | varies | Agent execution |
| {functionPath: "/test.js", event: {...}} | false | functionId required |
| {functionId: "f1", event: {...}} | false | functionPath required |

## Table 17: Execution Time Tracking

Tests that execution time is measured correctly.

| Function Type | Expected executionTime Present | Expected executionTime > 0 |
|--------------|-------------------------------|---------------------------|
| code (fast) | yes | yes |
| code (slow) | yes | yes |
| agent | yes | yes |
| error during execution | yes | yes |

## Table 18: Depth Tracking in Emitted Events

Tests that event depth is correctly tracked for emitted events.

| Incoming Event Depth | Function emits() Event | Expected Emitted Event Depth |
|---------------------|----------------------|----------------------------|
| 0 | yes | 1 |
| 1 | yes | 2 |
| 5 | yes | 6 |
| undefined | yes | 1 (default 0 + 1) |
| 0 | no | - |

## Table 19: Metadata in Emitted Events

Tests that emitted events have correct metadata.

| Function ID | Triggering Event ID | Expected Emitted Metadata |
|------------|-------------------|--------------------------|
| myFunc | evt_123 | source=myFunc, triggeredBy=evt_123, depth=(depth+1) |
| testFunc | evt_456 | source=testFunc, triggeredBy=evt_456, depth=(depth+1) |

## Table 20: Error Phase Tracking

Tests that error phase is correctly identified in error responses.

| Error Occurs During | Expected Error Phase |
|-------------------|---------------------|
| Module import | import |
| Export validation | validation |
| Function execution | execution |
| Agent spawn | agent-error |
| Agent exit code != 0 | agent-execution |
| Unexpected error | unexpected |

## Table 21: Logger Functionality

Tests that logger is correctly provided and functions.

| Logger Method | Function Calls | Expected Behavior |
|--------------|---------------|-------------------|
| logger.info() | yes | Logs to console with [functionId] prefix |
| logger.warn() | yes | Warns to console with [functionId] prefix |
| logger.error() | yes | Errors to console with [functionId] prefix |
| logger.debug() | yes (DEBUG=true) | Logs debug info |
| logger.debug() | yes (DEBUG=false) | No output |

## Table 22: Edge Cases

Tests boundary conditions and edge cases.

| Test Case | Expected Behavior |
|-----------|------------------|
| Function returns very large object | Success, object captured |
| Function with very long execution (10s) | Success, time tracked |
| Function emits 100 events | All emitted with correct metadata |
| Agent with very long output | Success, output captured |
| Agent with binary output | Success, captured as-is |
| Concurrent function executions | All execute independently |
| Execute same function 100 times | All succeed independently |
| Function path with spaces | Resolved and executed |
| Function path with unicode | Resolved and executed |

## Table 23: Config Merging

Tests that config is correctly merged for function context.

| Actor Config | Execute Config | Expected Context Config |
|-------------|---------------|------------------------|
| {a: 1, b: 2} | {b: 3, c: 4} | {a: 1, b: 3, c: 4} |
| {} | {a: 1} | {a: 1} |
| {a: 1} | {} | {a: 1} |
| {} | {} | {} |

## Implementation Notes

### Table Format
Each table represents a specific test scenario or behavior category. Tables can be executed by:
1. Setting up the actor and dependencies
2. Calling execute() or executeAgent()
3. Verifying expected outcomes

### Execution Strategy
- Table 1: Lifecycle - must pass first
- Tables 2-9: Code function execution - core functionality
- Tables 10-13: Agent function execution - subprocess handling
- Tables 14-23: Integration, messaging, edge cases

### Success Criteria
- All lifecycle transitions work correctly
- Code functions are loaded and executed correctly
- Agent functions are spawned and executed correctly
- Input validation catches invalid data
- Module loading errors are handled gracefully
- Execution errors are caught and reported
- Events are emitted with correct metadata
- Depth tracking works correctly
- Edge cases are handled properly
