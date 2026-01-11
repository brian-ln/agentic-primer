# EventLogActor - FIT Decision Tables

FIT-style decision tables for testing EventLogActor behavior declaratively.

## Table 1: Lifecycle State Transitions

Tests the core lifecycle methods: start(), stop(), getStatus()

| Initial State | Action | Expected State | Expected Success | Expected Message/Error |
|--------------|--------|----------------|------------------|------------------------|
| stopped | start() | running | true | - |
| stopped | stop() | stopped | true | - |
| stopped | getStatus() | stopped | - | isRunning=false |
| running | start() | running | false | already running |
| running | stop() | stopped | true | - |
| running | getStatus() | running | - | isRunning=true |
| running | start() then stop() | stopped | true | - |
| stopped | start() twice | running | false on 2nd | already running |
| stopped | stop() twice | stopped | true both | - |

## Table 2: Append Event - Happy Path

Tests successful event appending with various input combinations.

| Event Type | Event Data | Has ID | Has Timestamp | Expected Success | EventCount Increments | ID Generated |
|-----------|-----------|--------|---------------|------------------|----------------------|--------------|
| user.created | {name: "Alice"} | no | no | true | yes | yes |
| user.created | {name: "Bob"} | yes | no | true | yes | no |
| user.created | {name: "Charlie"} | no | yes | true | yes | yes |
| user.created | {name: "David"} | yes | yes | true | yes | no |
| order.placed | {orderId: 123} | no | no | true | yes | yes |
| order.placed | {} | no | no | true | yes | yes |
| system.boot | null | no | no | true | yes | yes |

## Table 3: Append Event - Error Cases

Tests error handling for invalid event data.

| Event Type | Event Data | Expected Success | Expected Error Contains |
|-----------|-----------|------------------|------------------------|
| null | {data: "test"} | false | Event type is required |
| undefined | {data: "test"} | false | Event type is required |
| "" | {data: "test"} | false | Event type is required |
| "valid.type" | "not-an-object" | true | - |

## Table 4: Query Events - Basic Queries

Tests event querying with different filter and pagination options.

| Setup Events | Limit | Offset | Reverse | Expected Count | Expected Order |
|-------------|-------|--------|---------|----------------|----------------|
| 5 events | infinity | 0 | false | 5 | oldest first |
| 5 events | 3 | 0 | false | 3 | oldest first 3 |
| 5 events | 3 | 2 | false | 3 | skip first 2 |
| 5 events | infinity | 0 | true | 5 | newest first |
| 5 events | 2 | 0 | true | 2 | newest first 2 |
| 10 events | 5 | 5 | false | 5 | events 6-10 |
| 0 events | infinity | 0 | false | 0 | empty |

## Table 5: Query Events - Filtering

Tests event filtering by type and custom filters.

| Event Types Stored | Filter Type | Expected Matches | Expected Types |
|-------------------|-------------|------------------|----------------|
| user.created, user.updated, order.placed | user.created | 1 | user.created |
| user.created (x3), order.placed (x2) | user.created | 3 | user.created |
| user.created (x3), order.placed (x2) | order.placed | 2 | order.placed |
| user.created, user.updated | none | 2 | both |
| (empty) | user.created | 0 | none |

## Table 6: Checkpoint and Replay

Tests checkpoint creation and replay from checkpoint functionality.

| Initial EventCount | Action | Checkpoint Value | Events After Checkpoint | Replay From | Expected Replay Count |
|-------------------|--------|------------------|------------------------|-------------|----------------------|
| 0 | checkpoint() | 0 | 0 | 0 | 0 |
| 5 | checkpoint() | 5 | 0 | 5 | 0 |
| 5 | checkpoint() | 5 | 3 | 5 | 3 |
| 10 | checkpoint() | 10 | 5 | 0 | 15 |
| 10 | checkpoint() | 10 | 5 | 10 | 5 |
| 10 | checkpoint() | 10 | 0 | 5 | 5 |
| 0 | checkpoint() | 0 | 10 | 0 | 10 |

## Table 7: Replay Event Handler

Tests replay with custom event handlers.

| Events in Log | Replay From | Handler Function | Expected Result |
|--------------|-------------|------------------|-----------------|
| 5 events | 0 | count events | 5 events processed |
| 5 events | 2 | count events | 3 events processed |
| 5 events | 0 | transform event | 5 transformed events |
| 5 events | 0 | filter user.* | only user events |
| 10 events | 5 | collect IDs | IDs of events 6-10 |

## Table 8: Error Handling - File Operations

Tests error handling for file system operations.

| Scenario | Log Path | Expected Behavior |
|----------|----------|------------------|
| Directory doesn't exist | /new/path/events.jsonl | Create directory |
| File doesn't exist | events.jsonl | Create on first write |
| File exists with data | events.jsonl | Append to existing |
| Read non-existent file | missing.jsonl | Return empty results |
| Invalid JSON in file | corrupted.jsonl | Skip bad lines |

## Table 9: Message Protocol Handling

Tests UAP message handling with different protocols and actions.

| Protocol | Action | Data | Expected Success | Expected Response |
|----------|--------|------|------------------|-------------------|
| EVENT | APPEND | valid event | true | eventId + count |
| EVENT | QUERY | {} | true | events array |
| EVENT | CHECKPOINT | {} | true | checkpoint value |
| EVENT | replay | {fromCheckpoint: 0} | true | replayed events |
| WRONG | APPEND | valid event | false | Invalid protocol |
| EVENT | INVALID | {} | false | Unknown action |

## Table 10: Edge Cases and Boundaries

Tests boundary conditions and edge cases.

| Test Case | Input | Expected Behavior |
|-----------|-------|------------------|
| Very large event data | {data: "x".repeat(10000)} | Success |
| Unicode in event type | "用户.created" | Success |
| Nested event data | {user: {profile: {nested: true}}} | Success |
| Empty event data | {} | Success |
| Multiple rapid appends | 100 events in loop | All succeed |
| Query with limit=0 | limit: 0 | Empty results |
| Query with negative offset | offset: -5 | Treated as 0 |
| Checkpoint without events | No events appended | checkpoint=0 |

## Table 11: ULID Generation

Tests ULID generation for event IDs.

| Append Count | Expected ID Format | IDs Unique | IDs Sortable |
|-------------|-------------------|-----------|--------------|
| 1 | evt_[26 chars] | true | true |
| 10 | evt_[26 chars] | all unique | chronologically ordered |
| 100 | evt_[26 chars] | all unique | chronologically ordered |
| concurrent | evt_[26 chars] | all unique | mostly ordered |

## Implementation Notes

### Table Format
Each table represents a specific test scenario or behavior category. Tables can be executed by:
1. Setting up the initial state (Initial State column)
2. Performing the action (Action column)
3. Verifying expected outcomes (Expected columns)

### Execution Strategy
- Tables 1-3: Core functionality - must pass first
- Tables 4-7: Query and replay features - integration tests
- Tables 8-9: Protocol and error handling - robustness tests
- Tables 10-11: Edge cases and boundaries - comprehensive coverage

### Success Criteria
- All lifecycle transitions work correctly
- Events are persisted durably to JSONL
- Query filters and pagination work correctly
- Checkpoint/replay functionality is reliable
- Error cases are handled gracefully
- Edge cases and boundaries are handled properly
