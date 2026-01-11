# HTTPServerActor State Machine Specification

## Overview

HTTPServerActor provides HTTP REST API endpoints for the event system with lifecycle-managed server resources.

**Actors**: HTTPServerActor
**States**: STOPPED, STARTING, RUNNING, STOPPING, ERROR
**Resources**: Bun HTTP server, network port
**File**: `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/http-server.js`

---

## State Definitions

### STOPPED (isRunning = false)
**Description**: Server is not running, no network resources are allocated.

**Invariants**:
- `isRunning === false`
- `server === null`
- `startTime === null`
- Port is available for binding

**Entry Actions**:
- Set `isRunning = false`
- Set `server = null`
- Set `startTime = null`

**Exit Actions**: None

**Valid Operations**:
- `start()` - Initialize and start server
- `getStatus()` - Report stopped status

---

### STARTING (intermediate state)
**Description**: Server is being initialized and starting up.

**Invariants**:
- `isRunning === false` (not yet fully running)
- `server` may be partially initialized
- Port binding in progress

**Entry Actions**:
1. Validate configuration (port, host)
2. Begin Bun.serve() initialization
3. Bind to network port

**Exit Actions**:
- On success: Transition to RUNNING
- On failure: Transition to ERROR

**Valid Operations**:
- None (transient state)

**Typical Duration**: Milliseconds

---

### RUNNING (isRunning = true)
**Description**: Server is operational and accepting HTTP requests.

**Invariants**:
- `isRunning === true`
- `server !== null` (active Bun server)
- `startTime !== null` (ISO timestamp)
- Port is bound and listening
- `server.port` contains actual port number

**Entry Actions**:
1. Set `isRunning = true`
2. Set `startTime = new Date().toISOString()`
3. Log server URL and port
4. Return success with URL and port

**Exit Actions**:
1. Prepare for shutdown
2. Transition to STOPPING state

**Valid Operations**:
- `handleRequest(request)` - Process HTTP requests
- `stop()` - Initiate shutdown
- `getStatus()` - Report running status with uptime
- `setActors()` - Inject actor references

---

### STOPPING (intermediate state)
**Description**: Server is shutting down gracefully.

**Invariants**:
- `isRunning === false` (marked for shutdown)
- `server !== null` (still exists, being closed)
- No new requests accepted
- Existing requests being completed

**Entry Actions**:
1. Set `isRunning = false`
2. Stop accepting new connections
3. Call `server.stop()`

**Exit Actions**:
- On success: Transition to STOPPED
- On failure: Transition to ERROR (but force cleanup)

**Valid Operations**:
- None (transient state)

**Typical Duration**: Milliseconds (waits for in-flight requests)

---

### ERROR
**Description**: Server encountered an error during startup or operation.

**Invariants**:
- `isRunning === false`
- `server` may be null or in inconsistent state
- Error details available

**Entry Actions**:
1. Set `isRunning = false`
2. Log error details
3. Clean up any partial resources

**Exit Actions**:
- Require manual recovery (restart)

**Valid Operations**:
- `stop()` - Cleanup and return to STOPPED
- `start()` - Retry startup
- `getStatus()` - Report error status

**Recovery**: Call `stop()` then `start()` to retry

---

## State Transition Table

| Current State | Event | Guards | Next State | Actions | Error Handling |
|---------------|-------|--------|------------|---------|----------------|
| STOPPED | `start()` called | None | STARTING | 1. Validate port and host<br>2. Begin server initialization | On error: → ERROR |
| STOPPED | `stop()` called | None | STOPPED | 1. Return `{ success: true }` (idempotent) | No error possible |
| STOPPED | `getStatus()` called | None | STOPPED | 1. Return `{ isRunning: false, port, url: null }` | No error possible |
| STARTING | Server binds successfully | Port available | RUNNING | 1. Store server reference<br>2. Set `isRunning = true`<br>3. Set `startTime`<br>4. Return `{ success: true, url, port }` | - |
| STARTING | Port already in use | Port unavailable | ERROR | 1. Return `{ success: false, error: "Port in use" }`<br>2. Set error state | Log error, cleanup |
| STARTING | Bun.serve() fails | Any error | ERROR | 1. Return `{ success: false, error: message }`<br>2. Set error state | Log error, cleanup |
| RUNNING | `start()` called | Already running | RUNNING | 1. Return `{ success: false, error: "Already running" }` | No state change |
| RUNNING | `stop()` called | None | STOPPING | 1. Set `isRunning = false`<br>2. Call `server.stop()` | On error: → ERROR |
| RUNNING | `getStatus()` called | None | RUNNING | 1. Calculate uptime<br>2. Return `{ isRunning: true, port, url, startTime, uptime }` | No error possible |
| RUNNING | `handleRequest()` called | Valid HTTP request | RUNNING | 1. Parse URL and method<br>2. Route to handler<br>3. Return HTTP response | On error: return 500 response |
| RUNNING | Request handler error | Exception in handler | RUNNING | 1. Log error<br>2. Return 500 response | Server continues running |
| RUNNING | Server crash | Unexpected error | ERROR | 1. Set `isRunning = false`<br>2. Log error | May require restart |
| STOPPING | Server stops successfully | None | STOPPED | 1. Set `server = null`<br>2. Set `startTime = null`<br>3. Return `{ success: true }` | - |
| STOPPING | Stop fails | Timeout or error | ERROR | 1. Force `server = null`<br>2. Return `{ success: false, error: message }` | Force cleanup anyway |
| ERROR | `stop()` called | None | STOPPED | 1. Force cleanup<br>2. Set `server = null`<br>3. Return `{ success: true }` | Best-effort cleanup |
| ERROR | `start()` called | None | STARTING | 1. Reset error state<br>2. Retry server initialization | On error: → ERROR |
| ERROR | `getStatus()` called | None | ERROR | 1. Return `{ isRunning: false, error: <details> }` | No error possible |

---

## State Diagram

```
                    ┌────────────────────────────────────────┐
                    │                                        │
                    │                                        ▼
               ┌─────────┐    start()         ┌──────────────────┐
            ┌──┤ STOPPED │───────────────────►│    STARTING      │
            │  └─────────┘                    │  (Bun.serve())   │
            │       ▲                         └────┬─────────┬───┘
            │       │                              │         │
            │       │                              │ success │ error
            │       │                              ▼         ▼
            │       │                         ┌─────────┐  ┌───────┐
            │       │                         │ RUNNING │  │ ERROR │
            │       │                         └────┬────┘  └───┬───┘
            │       │                              │           │
            │       │         stop()               │           │
            │       │        ◄─────────────────────┘           │
            │       │                                          │
            │  ┌──────────┐                                    │
            │  │ STOPPING │◄───────────────────────────────────┘
            │  │ (cleanup)│                    stop()
            │  └─────┬────┘
            │        │
            │        │ success
            └────────┘
```

---

## Transition Guards

### start() Guards
- **Not Already Running**: `isRunning === false`
- **Valid Configuration**: `port` is valid (1-65535)
- **Host is Valid**: `host` is valid hostname or IP
- **Port Available**: Port not already bound

### stop() Guards
- **No Guards**: Always safe to call (idempotent)

### handleRequest() Guards
- **Is Running**: `isRunning === true`
- **Valid Request**: Request object is valid
- **Actors Injected**: Required actors are available (optional check)

---

## Error States and Recovery

### Error Scenarios

1. **Port Already in Use**
   - **State**: STARTING → ERROR
   - **Recovery**:
     - Call `stop()` to cleanup
     - Change port in config
     - Call `start()` to retry

2. **Bun.serve() Initialization Failure**
   - **State**: STARTING → ERROR
   - **Recovery**:
     - Call `stop()` to cleanup
     - Check system resources
     - Call `start()` to retry

3. **Server Crash During Operation**
   - **State**: RUNNING → ERROR
   - **Recovery**:
     - Call `stop()` to cleanup
     - Investigate logs
     - Call `start()` to restart

4. **Graceful Stop Failure**
   - **State**: STOPPING → ERROR
   - **Recovery**:
     - Force cleanup with `stop()`
     - Check for leaked resources
     - Call `start()` if needed

5. **Request Handler Error**
   - **State**: Remain RUNNING
   - **Recovery**:
     - Return 500 error to client
     - Log error for debugging
     - Server continues operating

---

## State Invariants

### Global Invariants (all states)
- `port` is a valid port number (1-65535)
- `host` is a valid hostname or IP address
- If `isRunning === true`, then `server !== null`
- If `isRunning === false`, then `server === null || server.stop() was called`

### STOPPED Invariants
- `isRunning === false`
- `server === null`
- `startTime === null`
- Cannot handle requests

### STARTING Invariants
- Transient state (milliseconds)
- `isRunning === false`
- Server initialization in progress

### RUNNING Invariants
- `isRunning === true`
- `server !== null`
- `server.port` contains actual bound port
- `startTime` is valid ISO timestamp
- Can handle HTTP requests

### STOPPING Invariants
- Transient state (milliseconds)
- `isRunning === false`
- `server.stop()` was called
- No new requests accepted

### ERROR Invariants
- `isRunning === false`
- Error details available
- Requires manual recovery

---

## HTTP Request Handling

### Request Flow (RUNNING state only)

1. **Request Reception**: Bun calls `fetch` handler
2. **CORS Handling**: Add CORS headers, handle OPTIONS
3. **Routing**: Match method and path
4. **Handler Execution**: Call appropriate handler function
5. **Response**: Return HTTP response with CORS headers
6. **Error Handling**: Catch exceptions, return 500

### Supported Endpoints

| Method | Path | Handler | Description |
|--------|------|---------|-------------|
| POST | /events | handlePostEvent | Emit new event |
| GET | /events | handleGetEvents | Query events |
| GET | /functions | handleGetFunctions | List functions |
| GET | /patterns | handleGetPatterns | List patterns |
| GET | /health | handleGetHealth | Health check |
| OPTIONS | * | CORS preflight | CORS support |

---

## Lifecycle Examples

### Example 1: Normal Startup and Shutdown
```javascript
const server = new HTTPServerActor({ http: { port: 3000, host: 'localhost' } });

// Initial state: STOPPED
console.log(server.getStatus());
// { isRunning: false, port: 3000, url: null }

// Transition: STOPPED -> STARTING -> RUNNING
const result = await server.start();
// { success: true, url: 'http://localhost:3000', port: 3000 }

console.log(server.getStatus());
// { isRunning: true, port: 3000, url: 'http://localhost:3000', startTime: '...', uptime: 1234 }

// Transition: RUNNING -> STOPPING -> STOPPED
await server.stop();
// { success: true }

console.log(server.getStatus());
// { isRunning: false, port: 3000, url: null }
```

### Example 2: Error Handling - Port in Use
```javascript
const server1 = new HTTPServerActor({ http: { port: 3000 } });
await server1.start(); // { success: true, ... }

const server2 = new HTTPServerActor({ http: { port: 3000 } });
await server2.start();
// { success: false, error: "Port 3000 already in use" }
// State: ERROR

console.log(server2.getStatus());
// { isRunning: false, error: "Port 3000 already in use" }

// Recovery: cleanup and retry with different port
await server2.stop(); // Force cleanup
server2.port = 3001;
await server2.start(); // Retry with new port
```

### Example 3: Idempotent Operations
```javascript
await server.start(); // { success: true, ... }
await server.start(); // { success: false, error: "Already running" }

await server.stop(); // { success: true }
await server.stop(); // { success: true } (idempotent)
```

### Example 4: Request Handling
```javascript
// In RUNNING state
const response = await fetch('http://localhost:3000/health');
// HTTPServerActor.handleRequest() -> handleGetHealth() -> Response

const response2 = await fetch('http://localhost:3000/invalid');
// HTTPServerActor.handleRequest() -> 404 Response
```

---

## Testing Checklist

- [ ] Start from STOPPED transitions to RUNNING
- [ ] Start with port in use transitions to ERROR
- [ ] Start from RUNNING returns error
- [ ] Stop from RUNNING transitions to STOPPED
- [ ] Stop from STOPPED is idempotent
- [ ] Stop from ERROR cleans up resources
- [ ] getStatus returns correct state and uptime
- [ ] Server binds to correct port and host
- [ ] CORS headers are added to all responses
- [ ] OPTIONS requests return 204
- [ ] Valid routes return correct responses
- [ ] Invalid routes return 404
- [ ] Request handler errors return 500
- [ ] Actor references can be injected
- [ ] Port 0 (random port) works correctly
- [ ] Server restart works after stop

---

## Summary

HTTPServerActor implements a five-state machine (STOPPED, STARTING, RUNNING, STOPPING, ERROR) with primary responsibility for managing network resources. The state machine ensures:

1. **Resource Safety**: Network port is properly bound and released
2. **Graceful Startup**: Port conflicts are detected and reported
3. **Graceful Shutdown**: In-flight requests complete before stop
4. **Error Recovery**: Explicit error state with clear recovery path
5. **Operational Visibility**: Status includes URL, port, and uptime
6. **Request Isolation**: Handler errors don't crash the server

The specification provides complete transition coverage for all events, including transient states (STARTING, STOPPING) and error handling.
