# FunctionRegistryActor - FIT Decision Tables

FIT-style decision tables for testing FunctionRegistryActor behavior declaratively.

## Table 1: Lifecycle State Transitions

Tests the core lifecycle methods: start(), stop(), getStatus()

| Initial State | Action | Expected State | Expected Success | Expected Error/Message |
|--------------|--------|----------------|------------------|------------------------|
| stopped | start() | running | true | FunctionRegistryActor started |
| stopped | stop() | stopped | true | was not running |
| stopped | getStatus() | stopped | - | isRunning=false, functionCount=0 |
| running | start() | running | false | already running |
| running | stop() | stopped | true | FunctionRegistryActor stopped |
| running | getStatus() | running | - | isRunning=true, functionCount varies |

## Table 2: Register Function - Code Functions

Tests registration of code-based (.js) functions.

| Function ID | Type | Path | Expected Success | Expected Message Contains |
|------------|------|------|------------------|--------------------------|
| myFunc | code | /path/to/func.js | true | registered successfully |
| anotherFunc | code | /another/path.js | true | registered successfully |
| testFunc | code | ./relative/path.js | true | registered successfully |
| func123 | code | /valid/path.js | true | registered successfully |

## Table 3: Register Function - Agent Functions

Tests registration of agent-based functions.

| Function ID | Type | Agent Command | Path | Expected Success | Expected Message Contains |
|------------|------|---------------|------|------------------|--------------------------|
| agentFunc | agent | claude | /path/to/agent.js | true | registered successfully |
| gptFunc | agent | gpt | /path/to/gpt.js | true | registered successfully |
| customAgent | agent | custom-cli | /path/custom.js | true | registered successfully |

## Table 4: Register Function - Validation Errors

Tests input validation for function registration.

| Function ID | Metadata | Expected Success | Expected Error Contains |
|------------|----------|------------------|------------------------|
| null | {type: 'code', path: '/test.js'} | false | functionId is required |
| undefined | {type: 'code', path: '/test.js'} | false | functionId is required |
| "" | {type: 'code', path: '/test.js'} | false | functionId is required |
| 123 | {type: 'code', path: '/test.js'} | false | must be a string |
| "valid" | null | false | metadata is required |
| "valid" | undefined | false | metadata is required |
| "valid" | "string" | false | must be an object |
| "valid" | {type: 'invalid'} | false | must be "code" or "agent" |
| "valid" | {type: 'code'} | false | path is required for code |
| "valid" | {type: 'agent'} | false | agentCommand is required |

## Table 5: Register Function - Duplicate IDs

Tests duplicate function ID handling.

| Existing Functions | New Function ID | Expected Success | Expected Behavior |
|-------------------|----------------|------------------|-------------------|
| none | func1 | true | registers new |
| func1 | func1 | true | overwrites existing |
| func1, func2 | func1 | true | overwrites func1 |
| func1 | func2 | true | registers new |

## Table 6: Register Function - Metadata Fields

Tests storage of optional metadata fields.

| Function ID | Metadata Fields | Expected Stored Fields |
|------------|----------------|----------------------|
| func1 | {name: "MyFunc", description: "Test", author: "Alice"} | all fields stored |
| func2 | {name: "Func2"} | name stored, others default |
| func3 | {} | all defaults (functionId as name) |
| func4 | {maxStackDepth: 5} | maxStackDepth stored |
| func5 | {metadata: {custom: "data"}} | custom metadata stored |

## Table 7: Unregister Function

Tests function removal from registry.

| Existing Functions | Unregister ID | Expected Success | Expected Existed |
|-------------------|--------------|------------------|------------------|
| func1 | func1 | true | true |
| func1, func2 | func1 | true | true |
| none | func1 | true | false |
| func1 | func2 | true | false |
| func1 | null | false | - |
| func1 | undefined | false | - |

## Table 8: Get Function

Tests retrieving a specific function by ID.

| Registered Functions | Get Function ID | Expected Success | Expected Data Present |
|--------------------|----------------|------------------|---------------------|
| func1 | func1 | true | function metadata |
| func1, func2 | func1 | true | func1 metadata |
| func1 | func2 | false | error: not found |
| none | func1 | false | error: not found |
| func1 | null | false | error: functionId required |

## Table 9: List Functions - No Filters

Tests listing all registered functions without filters.

| Registered Functions | Expected Count | Expected Function IDs |
|--------------------|----------------|----------------------|
| none | 0 | [] |
| func1 | 1 | [func1] |
| func1, func2, func3 | 3 | [func1, func2, func3] |
| 10 functions | 10 | all 10 IDs |

## Table 10: List Functions - Type Filter

Tests filtering functions by type (code vs agent).

| Registered Functions | Filter Type | Expected Count | Expected Types |
|--------------------|-------------|----------------|----------------|
| 2 code, 1 agent | code | 2 | all code |
| 2 code, 1 agent | agent | 1 | all agent |
| 3 code | code | 3 | all code |
| 3 code | agent | 0 | none |
| 2 code, 2 agent | none | 4 | mixed |

## Table 11: Scan Directory - File Discovery

Tests auto-discovery of functions from filesystem.

| Directory Contents | Recursive | Expected Discovered | Expected Types |
|-------------------|-----------|---------------------|----------------|
| func1.js | false | 1 | code |
| func1.js, func2.js | false | 2 | code |
| agent1.agent.js | false | 1 | agent |
| func1.js, agent1.agent.js | false | 2 | 1 code, 1 agent |
| subdir/func1.js | false | 0 | none (not recursive) |
| subdir/func1.js | true | 1 | code |

## Table 12: Scan Directory - Overwrite Behavior

Tests overwrite behavior during directory scanning.

| Existing Functions | Discovered Files | Overwrite Option | Expected Behavior |
|-------------------|-----------------|------------------|-------------------|
| func1 | func1.js | false | skip (already registered) |
| func1 | func1.js | true | overwrite |
| func1 | func2.js | false | register func2 |
| func1 | func2.js | true | register func2 |
| none | func1.js | false | register |
| none | func1.js | true | register |

## Table 13: Scan Directory - Error Handling

Tests error handling during directory scanning.

| Directory Path | Expected Success | Expected Behavior |
|---------------|------------------|-------------------|
| /valid/path | true | scan succeeds |
| /nonexistent | false | error: failed to scan |
| /file/not/dir | false | error: not a directory |
| null | false | error: path required |

## Table 14: Get Stats

Tests registry statistics retrieval.

| Registered Functions (type) | Expected Total | Expected Code | Expected Agent |
|----------------------------|----------------|---------------|----------------|
| none | 0 | 0 | 0 |
| 3 code | 3 | 3 | 0 |
| 2 agent | 2 | 0 | 2 |
| 3 code, 2 agent | 5 | 3 | 2 |
| 10 code, 5 agent | 15 | 10 | 5 |

## Table 15: Handle Message - Protocol Validation

Tests UAP message handling and protocol validation.

| Protocol | Action | Expected Success | Expected Error |
|----------|--------|------------------|----------------|
| REGISTRY | REGISTER | varies | - |
| REGISTRY | UNREGISTER | varies | - |
| REGISTRY | LIST | true | - |
| REGISTRY | get | varies | - |
| REGISTRY | scan | varies | - |
| WRONG | REGISTER | false | Invalid protocol |
| REGISTRY | INVALID | false | Unknown action |

## Table 16: Handle Message - REGISTER Action

Tests message-based function registration.

| Message Data | Expected Success | Expected Behavior |
|-------------|------------------|-------------------|
| {functionId: "f1", metadata: {type: "code", path: "/test.js"}} | true | function registered |
| {functionId: "f1", metadata: {type: "agent", agentCommand: "claude"}} | true | agent registered |
| {functionId: null, metadata: {...}} | false | validation error |
| {metadata: {type: "code", path: "/test.js"}} | false | functionId required |

## Table 17: Handle Message - LIST Action

Tests message-based function listing.

| Message Data | Registered Functions | Expected Count | Expected Filters Applied |
|-------------|---------------------|----------------|------------------------|
| {} | 5 functions | 5 | none |
| {filters: {type: "code"}} | 3 code, 2 agent | 3 | type=code |
| {filters: {type: "agent"}} | 3 code, 2 agent | 2 | type=agent |
| {filters: {}} | 5 functions | 5 | none |

## Table 18: Clear Registry

Tests clearing all registered functions (for testing).

| Existing Function Count | After Clear Count | Expected Behavior |
|-----------------------|-------------------|-------------------|
| 0 | 0 | empty stays empty |
| 1 | 0 | cleared |
| 10 | 0 | all cleared |
| 100 | 0 | all cleared |

## Table 19: Function Metadata Storage

Tests that all metadata fields are stored correctly.

| Input Metadata | Expected Stored Fields |
|---------------|----------------------|
| {type: "code", path: "/test.js"} | type, path, agentCommand=null, maxStackDepth=null |
| {type: "agent", agentCommand: "claude"} | type, agentCommand, path=null, maxStackDepth=null |
| {type: "code", path: "/test.js", maxStackDepth: 5} | all fields including maxStackDepth |
| {type: "code", path: "/test.js", metadata: {name: "Test"}} | all fields plus custom metadata |

## Table 20: Edge Cases

Tests boundary conditions and edge cases.

| Test Case | Expected Behavior |
|-----------|------------------|
| Register 1000 functions | All stored successfully |
| Function ID with special chars | Stored as-is |
| Function ID with unicode | Stored as-is |
| Very long function ID (1000 chars) | Stored successfully |
| Very long path (1000 chars) | Stored successfully |
| Metadata with deeply nested objects | Stored successfully |
| Register same function 100 times | Overwrites each time |
| List after clear | Returns empty array |

## Implementation Notes

### Table Format
Each table represents a specific test scenario or behavior category. Tables can be executed by:
1. Setting up initial registry state
2. Performing the action (register, list, scan, etc.)
3. Verifying expected outcomes

### Execution Strategy
- Table 1: Lifecycle - must pass first
- Tables 2-7: Function management - registration and removal
- Tables 8-10: Function retrieval and listing
- Tables 11-13: Directory scanning
- Tables 14-20: Statistics, messaging, edge cases

### Success Criteria
- All lifecycle transitions work correctly
- Functions are registered and stored correctly
- Input validation catches invalid data
- Duplicate IDs are handled correctly (overwrite)
- Functions can be retrieved and listed
- Type filtering works correctly
- Directory scanning discovers functions
- UAP message handling works correctly
- Edge cases are handled properly
