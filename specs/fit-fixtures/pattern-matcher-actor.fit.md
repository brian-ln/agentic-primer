# PatternMatcherActor - FIT Decision Tables

FIT-style decision tables for testing PatternMatcherActor behavior declaratively.

## Table 1: Lifecycle State Transitions

Tests the core lifecycle methods: start(), stop(), getStatus()

| Initial State | Action | Expected State | Expected Success | Expected Error/Message |
|--------------|--------|----------------|------------------|------------------------|
| stopped | start() | running | true | PatternMatcherActor started |
| stopped | stop() | stopped | true | was not running |
| stopped | getStatus() | stopped | - | isRunning=false, patternCount=0 |
| running | start() | running | false | already running |
| running | stop() | stopped | true | PatternMatcherActor stopped |
| running | getStatus() | running | - | isRunning=true |

## Table 2: Register Pattern - Valid Patterns

Tests pattern registration with valid inputs.

| Pattern ID | Predicate | Priority | Metadata | Expected Success | Stored Priority |
|-----------|-----------|----------|----------|------------------|-----------------|
| pattern1 | event.type === 'user.created' | 0 | {} | true | 0 |
| pattern2 | event.data.amount > 100 | 10 | {name: "High Value"} | true | 10 |
| pattern3 | event.type.startsWith('user.') | -5 | {} | true | -5 |
| pattern4 | true | undefined | {} | true | 0 (default) |
| pattern5 | event.metadata?.source === 'api' | 5 | {desc: "API events"} | true | 5 |

## Table 3: Register Pattern - Invalid Patterns

Tests pattern registration error handling.

| Pattern ID | Predicate | Expected Success | Expected Error Contains |
|-----------|-----------|------------------|------------------------|
| null | "event.type === 'test'" | false | must have an id string |
| undefined | "event.type === 'test'" | false | must have an id string |
| "" | "event.type === 'test'" | false | must have an id string |
| 123 | "event.type === 'test'" | false | must have an id string |
| "valid" | null | false | must have a predicate string |
| "valid" | undefined | false | must have a predicate string |
| "valid" | "" | false | must have a predicate string |
| "valid" | 123 | false | must have a predicate string |
| "valid" | "invalid syntax )" | false | Invalid predicate syntax |
| "valid" | "event.type ===" | false | Invalid predicate syntax |

## Table 4: Register Pattern - Duplicate IDs

Tests duplicate pattern ID handling.

| Existing Patterns | New Pattern ID | Expected Success | Expected Error Contains |
|------------------|---------------|------------------|------------------------|
| none | pattern1 | true | - |
| pattern1 | pattern1 | false | already exists |
| pattern1, pattern2 | pattern1 | false | already exists |
| pattern1 | pattern2 | true | - |

## Table 5: Unregister Pattern

Tests pattern removal functionality.

| Existing Patterns | Unregister ID | Expected Success | Expected Error Contains |
|------------------|--------------|------------------|------------------------|
| pattern1 | pattern1 | true | - |
| pattern1, pattern2 | pattern1 | true | - |
| none | pattern1 | false | not found |
| pattern1 | pattern2 | false | not found |
| pattern1 | null | false | must be a string |
| pattern1 | undefined | false | must be a string |

## Table 6: List Patterns - Unsorted

Tests pattern listing without priority sorting.

| Registered Patterns | Expected Count | Expected Order |
|--------------------|----------------|----------------|
| none | 0 | empty array |
| pattern1 (p=0) | 1 | [pattern1] |
| pattern1 (p=10), pattern2 (p=5) | 2 | insertion order |
| pattern1, pattern2, pattern3 | 3 | insertion order |

## Table 7: List Patterns - Sorted by Priority

Tests pattern listing with priority sorting.

| Registered Patterns (id=priority) | Sort by Priority | Expected Order |
|----------------------------------|------------------|----------------|
| p1=10, p2=5, p3=0 | true | [p1, p2, p3] |
| p1=0, p2=10, p3=5 | true | [p2, p3, p1] |
| p1=5, p2=5, p3=5 | true | [p1, p2, p3] (stable) |
| p1=-10, p2=0, p3=10 | true | [p3, p2, p1] |
| p1=10, p2=5, p3=0 | false | insertion order |

## Table 8: Match Event - Simple Predicates

Tests event matching with simple predicate expressions.

| Pattern Predicate | Event | Expected Match | Match Count |
|------------------|-------|----------------|-------------|
| event.type === 'user.created' | {type: 'user.created'} | true | 1 |
| event.type === 'user.created' | {type: 'order.placed'} | false | 0 |
| event.data.amount > 100 | {type: 'test', data: {amount: 150}} | true | 1 |
| event.data.amount > 100 | {type: 'test', data: {amount: 50}} | false | 0 |
| true | {type: 'anything'} | true | 1 |
| false | {type: 'anything'} | false | 0 |

## Table 9: Match Event - Complex Predicates

Tests event matching with complex predicate expressions.

| Pattern Predicate | Event | Expected Match |
|------------------|-------|----------------|
| event.type.startsWith('user.') | {type: 'user.created'} | true |
| event.type.startsWith('user.') | {type: 'order.placed'} | false |
| event.data?.nested?.value > 10 | {data: {nested: {value: 20}}} | true |
| event.data?.nested?.value > 10 | {data: {nested: {value: 5}}} | false |
| event.metadata?.source === 'api' && event.type === 'user.created' | {type: 'user.created', metadata: {source: 'api'}} | true |
| event.metadata?.source === 'api' && event.type === 'user.created' | {type: 'user.created', metadata: {source: 'web'}} | false |

## Table 10: Match Event - Multiple Patterns

Tests event matching against multiple registered patterns.

| Registered Patterns | Event | Expected Match Count | Expected Pattern IDs |
|--------------------|-------|---------------------|---------------------|
| p1: event.type==='user.created', p2: event.type==='order.placed' | {type: 'user.created'} | 1 | [p1] |
| p1: event.type==='user.created', p2: true | {type: 'user.created'} | 2 | [p1, p2] |
| p1: event.data.amount>100, p2: event.data.amount>50 | {data: {amount: 150}} | 2 | [p1, p2] |
| p1: event.type==='user.created', p2: event.type==='order.placed' | {type: 'unknown'} | 0 | [] |

## Table 11: Match Event - Priority Order

Tests that matches are returned in priority order (highest first).

| Patterns (id=priority, predicate=true) | Event | Expected Match Order |
|---------------------------------------|-------|---------------------|
| p1=10, p2=5, p3=0 | {type: 'test'} | [p1, p2, p3] |
| p1=0, p2=10, p3=5 | {type: 'test'} | [p2, p3, p1] |
| p1=-5, p2=0, p3=5 | {type: 'test'} | [p3, p2, p1] |
| p1=5, p2=5, p3=5 | {type: 'test'} | [p1, p2, p3] (stable) |

## Table 12: Match Event - Predicate Errors

Tests error handling when predicates throw exceptions during evaluation.

| Pattern Predicate | Event | Expected Success | Expected Errors Count |
|------------------|-------|------------------|---------------------|
| event.data.nested.value > 10 | {type: 'test'} | true | 1 (TypeError) |
| event.data.method() | {type: 'test', data: {}} | true | 1 (not a function) |
| event.nonexistent.deeply.nested | {type: 'test'} | true | 1 |
| throw new Error('test') | {type: 'test'} | true | 1 |

## Table 13: Match Event - Invalid Event Input

Tests error handling for invalid event inputs.

| Event Input | Expected Success | Expected Error Contains |
|------------|------------------|------------------------|
| null | false | Event must be an object |
| undefined | false | Event must be an object |
| "string" | false | Event must be an object |
| 123 | false | Event must be an object |
| [] | true | - (arrays are objects) |
| {} | true | - |

## Table 14: Get Pattern

Tests retrieving a specific pattern by ID.

| Registered Patterns | Get Pattern ID | Expected Result |
|--------------------|---------------|-----------------|
| pattern1 | pattern1 | pattern object |
| pattern1, pattern2 | pattern1 | pattern1 object |
| pattern1 | pattern2 | null |
| none | pattern1 | null |

## Table 15: Get Pattern Count

Tests getting the count of registered patterns.

| Registered Patterns | Expected Count |
|--------------------|----------------|
| none | 0 |
| pattern1 | 1 |
| pattern1, pattern2 | 2 |
| pattern1, pattern2, pattern3, pattern4, pattern5 | 5 |

## Table 16: Clear Patterns

Tests clearing all registered patterns.

| Existing Pattern Count | After Clear Count | Expected Success |
|-----------------------|-------------------|------------------|
| 0 | 0 | true |
| 1 | 0 | true |
| 5 | 0 | true |
| 10 | 0 | true |

## Table 17: Edge Cases - Predicate Expressions

Tests edge cases in predicate expression evaluation.

| Pattern Predicate | Event | Expected Match | Notes |
|------------------|-------|----------------|-------|
| 1 + 1 === 2 | {type: 'test'} | true | constant expression |
| event | {type: 'test'} | true | truthy object |
| !event.data | {type: 'test'} | true | falsy undefined |
| event.data || event.type | {type: 'test'} | true | OR expression |
| event.data && event.type | {type: 'test', data: {}} | true | AND expression |
| event.type === 'test' ? true : false | {type: 'test'} | true | ternary |

## Table 18: Pattern Metadata

Tests that pattern metadata is stored and returned correctly.

| Pattern Metadata | Expected in List | Expected in Match |
|-----------------|-----------------|-------------------|
| {name: "Test", desc: "A test pattern"} | yes | yes |
| {} | yes (empty) | yes (empty) |
| {custom: {nested: {data: true}}} | yes | yes |
| undefined | yes (empty object) | yes (empty object) |

## Implementation Notes

### Table Format
Each table represents a specific test scenario or behavior category. Tables can be executed by:
1. Setting up initial patterns (Registered Patterns)
2. Performing the action (registerPattern, matchEvent, etc.)
3. Verifying expected outcomes (Expected columns)

### Execution Strategy
- Table 1: Lifecycle - must pass first
- Tables 2-7: Pattern management - registration, listing
- Tables 8-12: Pattern matching - core functionality
- Tables 13-18: Edge cases and error handling

### Success Criteria
- All lifecycle transitions work correctly
- Patterns are registered and stored correctly
- Pattern validation catches invalid inputs
- Duplicate IDs are rejected
- Events are matched against predicates correctly
- Matches are returned in priority order
- Predicate errors are caught and reported
- Invalid inputs are handled gracefully
- Edge cases are handled properly
