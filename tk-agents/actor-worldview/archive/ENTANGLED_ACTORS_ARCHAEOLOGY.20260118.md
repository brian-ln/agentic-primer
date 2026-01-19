# Entangled Actors Pattern - Archaeological Findings

**Research Date:** January 17, 2026
**Source:** `/Users/bln/play/actors/system-meta-model/`
**Pattern Origin:** Actor System Meta-Model project (Aug 2025)

## Discovery Summary

The "entangled actors" pattern was found in the Actor System Meta-Model project, specifically in the cross-runtime transport implementations. The pattern describes **paired transport actors** (server and client) that work together to enable transparent communication across runtime boundaries.

## Source Files Analyzed

### Primary Documentation
- `/Users/bln/play/actors/system-meta-model/runtimes/README.md`
- `/Users/bln/play/actors/system-meta-model/runtimes/TRANSPORT_REQUIREMENTS.md`
- `/Users/bln/play/actors/system-meta-model/foundation/experiments/05-cross-runtime/README.md`

### Implementation Files
- `/Users/bln/play/actors/system-meta-model/runtimes/server/websocket-server-transport.ts` (490 lines)
- `/Users/bln/play/actors/system-meta-model/runtimes/server/websocket-client-transport.ts` (399 lines)
- `/Users/bln/play/actors/system-meta-model/runtimes/browser/websocket-client-transport.ts` (377 lines)

## Pattern Definition

### What Makes Actors "Entangled"?

From the TRANSPORT_REQUIREMENTS.md documentation:

> **Entangled Transport Pairs**
>
> Each transport must have matching server and client implementations:
>
> ### 1. WebSocket Transport Pair
> - **Server**: `websocket-server.ts` (Node/Bun/Deno built-in WebSocket)
> - **Client**: `websocket-client.ts` (Browser WebSocket API)
> - **Python**: `websocket_server.py` (http.server with manual upgrade)
>
> **Protocol**: JSON messages over WebSocket frames
> **Bidirectional**: ✅ Full duplex
> **Dependencies**: ❌ None (built-in APIs only)

### Key Characteristics

1. **Paired Implementation**: Every server transport has a matching client transport
2. **Identical Message Protocol**: Both sides use the same `TransportMessage` interface
3. **Bidirectional Communication**: Both can send and receive
4. **Transport Agnostic**: Actors don't know they're communicating across network boundaries
5. **Zero Dependencies**: Uses only built-in runtime APIs

## Code Evidence

### Shared Message Protocol

Both server and client transports use identical message structures:

```typescript
interface TransportMessage {
  type: 'actor-message' | 'connection-status' | 'error';
  target?: string;        // Actor name
  messageType?: string;   // Actor message type
  data?: any;            // Message payload
  requestId?: string;    // For request/response
  sender?: string;       // Message source
  timestamp?: string;    // ISO timestamp
}
```

### Server Transport (websocket-server-transport.ts)

**Responsibilities:**
- Accept WebSocket connections from clients
- Route incoming messages to local actors
- Broadcast messages to connected clients
- Manage client lifecycle (connect/disconnect)
- Support multiple runtime environments (Node.js, Bun, Deno)

**Key Methods:**
- `startServer(port)` - Start listening for connections
- `sendToClient(clientId, message)` - Send to specific client
- `broadcast(message)` - Send to all clients
- `setMessageRouter(router)` - Register message handler

**Lines of Code:** 490 lines (including multi-runtime support)

### Client Transport (websocket-client-transport.ts)

**Responsibilities:**
- Connect to WebSocket server
- Send messages to remote actors
- Handle incoming messages from server
- Automatic reconnection with exponential backoff
- Request/response pattern support

**Key Methods:**
- `connect(url)` - Connect to server
- `send(messageData)` - Fire-and-forget message
- `request(messageData)` - Request with response
- `onMessage(messageType, handler)` - Register handler
- `setAutoReconnect(enabled, maxAttempts)` - Configure reconnection

**Lines of Code:** 399 lines (server runtime), 377 lines (browser runtime)

## Entanglement Mechanics

### 1. Protocol Symmetry

Both sides implement the same message flow:

```
Client sends:
{
  type: 'actor-message',
  target: 'form-validator',
  messageType: 'validate-contact-form',
  data: { name: 'John', email: 'john@test.com' },
  requestId: 'req_123',
  sender: 'browser-client',
  timestamp: '2026-01-17T18:12:04Z'
}

Server responds:
{
  type: 'actor-message',
  data: { valid: true, saved: { id: 'contact-abc' } },
  requestId: 'req_123',
  sender: 'websocket-server',
  timestamp: '2026-01-17T18:12:05Z'
}
```

### 2. Request/Response Pairing

Client maintains pending request map:

```typescript
private pendingRequests = new Map<string, {
  resolve: Function,
  reject: Function,
  timeout: NodeJS.Timeout
}>();
```

When response arrives with matching `requestId`, promise resolves.

### 3. Bidirectional Communication

**Client → Server:**
- Client sends actor message
- Server routes to local actor
- Actor processes and returns result

**Server → Client:**
- Server can push messages to clients
- Uses `sendToClient(clientId, message)` or `broadcast(message)`
- Client handlers registered via `onMessage(type, handler)`

### 4. Transparent Routing

Actors don't know about network boundaries:

```javascript
// Browser actor code
const result = await runtime.ask('backend/form-validator', {
  type: 'validate-contact-form',
  data: formData
});
// Looks like local call, actually goes over WebSocket
```

## Cross-Runtime Support

The pattern works across multiple JavaScript/TypeScript runtimes:

| Runtime | Server Transport | Client Transport | Implementation |
|---------|------------------|------------------|----------------|
| **Node.js v21+** | ✅ Built-in WebSocket | ✅ Built-in WebSocket | Manual handshake |
| **Bun v1.0+** | ✅ Bun.serve() | ✅ Built-in WebSocket | Native API |
| **Deno v1.40+** | ✅ Deno.serve() | ✅ Built-in WebSocket | Native API |
| **Browser** | ❌ N/A | ✅ WebSocket API | Native API |
| **Python 3.8+** | 🔧 Manual impl | 🔧 Manual impl | http.server |

## Experimental Validation

The pattern was validated in Experiment 5 (Cross-Runtime Actor Communication):

**Test Results:**
- ✅ Valid form submission: Contact saved with ID
- ✅ Invalid data validation: 3 validation errors returned correctly
- ✅ Echo communication: Data round-trip preserved
- ✅ Error handling: "Actor not found" error handled
- ✅ Performance: Sub-100ms response times

**Message Flow Validated:**
```
Browser → WebSocket → Backend → Actor → Business Logic
  → Response → WebSocket → Browser
```

## Design Philosophy

From the runtimes/README.md:

1. **Zero Dependencies** - Uses only built-in web platform APIs
2. **Direct Imports** - No complex build/install processes
3. **Transport Agnostic** - Actors don't know about networking
4. **Cross-Runtime** - Same code works everywhere
5. **Modern First** - Designed for current/recent runtime versions

## Additional Transport Pairs

The pattern extends beyond WebSocket:

### 2. SSE (Server-Sent Events) Transport Pair
- **Server**: `sse-server.ts` (HTTP with `text/event-stream`)
- **Client**: `sse-client.ts` (Browser EventSource API)
- **Bidirectional**: ❌ Server-to-client only (client uses fetch for requests)

### 3. HTTP Streaming Transport Pair
- **Server**: `stream-server.ts` (HTTP with ReadableStream)
- **Client**: `stream-client.ts` (fetch with ReadableStream)
- **Bidirectional**: ✅ Via separate request/response streams

## Key Insight: Transport Selection Strategy

The system auto-detects best transport:

```javascript
const transport = createBestTransport({
  preferred: ['websocket', 'sse', 'stream'],
  fallback: true
});
```

**Priority Order:**
1. **WebSocket** - Best performance, full duplex
2. **SSE** - Simple, reliable, auto-reconnect
3. **HTTP Streaming** - Universal fallback

## Related Patterns

### Distributed Actors (Advanced)

```javascript
// Multiple servers talking to each other
const runtime = createDistributedRuntime({
  serverPort: 8080,           // Accept connections
  connectTo: ['ws://other:8081']  // Connect to other servers
});
```

This extends the entangled pattern to create server-to-server meshes.

## Pattern Benefits

1. **Transparent Distribution**: Actors communicate the same way locally or remotely
2. **No External Dependencies**: Uses only built-in runtime APIs
3. **Automatic Reconnection**: Client handles connection failures with exponential backoff
4. **Request/Response**: Built-in support for async request/response pattern
5. **Multi-Runtime**: Works across Node.js, Bun, Deno, browsers
6. **Error Propagation**: Network errors appear as normal actor errors

## Historical Context

**Project:** Actor System Meta-Model
**Timeline:** Developed around August 2025
**Purpose:** Enable zero-dependency actor systems that work identically across browser, server, and Python environments
**Status:** Experimentally validated, production-ready code

## Applicability to tk-agents

The entangled actors pattern is highly relevant for tk-agents workbench design:

1. **Browser ↔ Daemon Communication**: Workbench UI (browser) communicating with daemon (Node.js/Bun)
2. **Zero Dependencies**: Aligns with tk-agents philosophy of minimal dependencies
3. **Transport Agnostic**: Actors don't need to know they're distributed
4. **Built-in Reconnection**: Handles daemon restarts gracefully
5. **Request/Response**: Natural fit for workbench queries

## Files Extracted

The following files demonstrate the complete pattern:

- `websocket-server-transport.ts` (490 lines) - Server-side transport
- `websocket-client-transport.ts` (399/377 lines) - Client-side transport (server/browser)
- `backend-runtime.js` - Example server runtime implementation
- `browser-actor-runtime.js` - Example browser runtime implementation
- `test-page.html` - Working example of browser-server communication

---

**Pattern Status:** ✅ VALIDATED
**Implementation Quality:** Production-ready
**Documentation Quality:** Comprehensive
**Applicability:** High relevance to tk-agents workbench
