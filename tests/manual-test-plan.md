# Event System MVP - Manual Test Plan

**Date**: 2026-01-10
**Version**: Phase 1 MVP
**Status**: Ready for Testing

---

## Test Environment Setup

```bash
# 1. Navigate to project
cd /Users/bln/play/agentic-primer/.wt/event-system

# 2. Verify Bun installed
bun --version

# 3. Install dependencies (if needed)
bun install
```

---

## Test Suite

### Test 1: Daemon Lifecycle

**Objective**: Verify daemon starts, stops, and reports status correctly

```bash
# Start daemon
./event-system daemon start
# Expected: ✅ Daemon started (PID: XXXX)

# Check status
./event-system daemon status
# Expected: Status: running, PID: XXXX

# Stop daemon
./event-system daemon stop
# Expected: ✅ Daemon stopped

# Verify stopped
./event-system daemon status
# Expected: Status: stopped
```

**Acceptance Criteria**:
- ✅ Daemon starts without errors
- ✅ Status shows correct state
- ✅ Daemon stops gracefully
- ✅ PID file cleaned up

---

### Test 2: Event Emission via CLI

**Objective**: Verify events can be emitted and persisted

```bash
# Start daemon
./event-system daemon start

# Emit test event
./event-system emit '{"type":"test.event","data":{"message":"hello world"}}'
# Expected: ✅ Event emitted: <event-id>

# Check events.jsonl file
cat events.jsonl
# Expected: JSON line with event data

# Stop daemon
./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Event emitted successfully
- ✅ Event written to events.jsonl
- ✅ Event has required fields (id, timestamp, type, data)
- ✅ Event depth = 0 (root event)

---

### Test 3: Event Log Persistence

**Objective**: Verify JSONL event storage and replay

```bash
# Start daemon
./event-system daemon start

# Emit multiple events
for i in {1..5}; do
  ./event-system emit "{\"type\":\"counter\",\"data\":{\"count\":$i}}"
  sleep 0.1
done

# Check events.jsonl
wc -l events.jsonl
# Expected: At least 5 lines

# Verify JSON format
cat events.jsonl | jq '.type'
# Expected: "counter" for all events

# Stop and restart daemon (test replay)
./event-system daemon stop
./event-system daemon start

# Check logs for replay
tail daemon.log | grep replay
# Expected: Events replayed from checkpoint

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Multiple events persisted
- ✅ Valid JSON Lines format
- ✅ Events replay on daemon restart
- ✅ Event order preserved

---

### Test 4: Function Execution (Code Functions)

**Objective**: Verify code-based functions execute correctly

```bash
# Start daemon
./event-system daemon start

# Register echo function (via HTTP or registry)
# Note: In MVP, functions are auto-discovered from functions/ directory

# Test echo function
./event-system emit '{"type":"echo.test","data":{"text":"Hello Functions"}}'

# Check events.jsonl for function.executed event
cat events.jsonl | grep "function.executed"
# Expected: Event showing echo function executed

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Function loaded from filesystem
- ✅ Function receives event and context
- ✅ Function executes without errors
- ✅ function.executed event emitted
- ✅ Return value captured

---

### Test 5: Pattern Matching

**Objective**: Verify JavaScript predicate matching works

```bash
# Start daemon
./event-system daemon start

# Emit event that matches pattern
./event-system emit '{"type":"error","data":{"severity":"high"}}'

# Check if pattern triggered
cat events.jsonl | grep -A 2 "error"
# Expected: error event + any triggered function events

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Patterns registered successfully
- ✅ Events matched against patterns
- ✅ Matched functions triggered
- ✅ Priority sorting works

---

### Test 6: Loop Prevention - Depth Limiting

**Objective**: Verify depth counter prevents infinite loops

```bash
# Start daemon
./event-system daemon start

# Create a function that emits events recursively
# (Would need to create recursive-test.js function)

# Emit trigger event
./event-system emit '{"type":"recursive.test","data":{}}'

# Check for loop.prevented event
cat events.jsonl | grep "loop.prevented"
# Expected: Loop prevented event after depth limit

# Or check daemon.log
tail daemon.log | grep "Depth limit"
# Expected: Depth limit exceeded message

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Depth limit enforced (default: 50)
- ✅ Loop prevention triggered
- ✅ loop.prevented event emitted
- ✅ System remains stable

---

### Test 7: Loop Prevention - Fingerprinting

**Objective**: Verify duplicate event detection

```bash
# Start daemon
./event-system daemon start

# Emit identical events
./event-system emit '{"type":"duplicate","data":{"id":123}}'
./event-system emit '{"type":"duplicate","data":{"id":123}}'

# Check for duplicate detection
cat events.jsonl | grep "duplicate"
# Expected: First event accepted, second prevented

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Fingerprint calculated correctly
- ✅ Duplicate events detected
- ✅ LRU cache working
- ✅ Only first occurrence allowed

---

### Test 8: HTTP API

**Objective**: Verify HTTP server endpoints

```bash
# Start daemon
./event-system daemon start

# Health check
curl http://localhost:3000/health
# Expected: {"status":"ok","timestamp":"..."}

# Emit event via POST
curl -X POST http://localhost:3000/events \
  -H "Content-Type: application/json" \
  -d '{"type":"http.test","data":{"source":"curl"}}'
# Expected: {"success":true,"eventId":"..."}

# Query events
curl http://localhost:3000/events
# Expected: JSON array of events

# List functions
curl http://localhost:3000/functions
# Expected: JSON array of registered functions

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ HTTP server binds to port 3000
- ✅ All endpoints respond correctly
- ✅ JSON parsing works
- ✅ Errors return proper status codes

---

### Test 9: Agent Functions (Claude CLI)

**Objective**: Verify agent function integration

**Prerequisites**: Claude CLI installed and accessible

```bash
# Start daemon
./event-system daemon start

# Emit error event to trigger agent analysis
./event-system emit '{"type":"error.occurred","data":{"error":"NullPointerException","stack":"..."}}'

# Check for agent function execution
cat events.jsonl | grep -A 5 "analyze-error"
# Expected: Agent function executed, response captured

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Agent function detected
- ✅ Claude CLI spawned as subprocess
- ✅ Prompt built from event data
- ✅ Response captured
- ✅ function.executed event emitted

---

### Test 10: Circuit Breaker

**Objective**: Verify per-function rate limiting

```bash
# Start daemon
./event-system daemon start

# Emit 150 events rapidly to same function
for i in {1..150}; do
  ./event-system emit '{"type":"rate.test","data":{"count":'$i'}}'
done

# Check for circuit breaker trip
cat events.jsonl | grep "circuit.breaker"
# Expected: Circuit breaker tripped after threshold (100)

./event-system daemon stop
```

**Acceptance Criteria**:
- ✅ Circuit breaker counts per function
- ✅ Threshold enforced (default: 100/min)
- ✅ Circuit opens when threshold exceeded
- ✅ Circuit auto-resets after window

---

### Test 11: Graceful Shutdown

**Objective**: Verify SIGINT/SIGTERM handling

```bash
# Start daemon
./event-system daemon start

# Get PID
PID=$(cat .daemon.pid)

# Send SIGTERM
kill -TERM $PID

# Check if daemon stopped gracefully
sleep 1
./event-system daemon status
# Expected: Status: stopped

# Check logs
tail daemon.log | grep "shutdown"
# Expected: Graceful shutdown message
```

**Acceptance Criteria**:
- ✅ SIGINT handled (Ctrl+C)
- ✅ SIGTERM handled
- ✅ Cleanup performed
- ✅ PID file removed
- ✅ No orphaned processes

---

## Test Results Summary

| Test | Status | Notes |
|------|--------|-------|
| 1. Daemon Lifecycle | ⏳ | |
| 2. Event Emission | ⏳ | |
| 3. Event Log Persistence | ⏳ | |
| 4. Function Execution | ⏳ | |
| 5. Pattern Matching | ⏳ | |
| 6. Loop Prevention - Depth | ⏳ | |
| 7. Loop Prevention - Fingerprint | ⏳ | |
| 8. HTTP API | ⏳ | |
| 9. Agent Functions | ⏳ | |
| 10. Circuit Breaker | ⏳ | |
| 11. Graceful Shutdown | ⏳ | |

---

## Known Issues

*To be filled during testing*

---

## Performance Benchmarks

*To be measured during testing*

- Event emission latency: TBD
- Function execution time: TBD
- HTTP API response time: TBD
- Memory usage: TBD
- Event throughput: TBD

---

## Next Steps

1. Execute all tests in sequence
2. Document results and issues
3. Fix any critical bugs
4. Re-test failed scenarios
5. Update this document with results
6. Sign off on MVP completion
