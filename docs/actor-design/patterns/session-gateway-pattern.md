# Session Gateway Pattern

## Overview

The Session Gateway Pattern uses a Durable Object actor as a WebSocket routing layer between clients and backend actor systems. This pattern provides session isolation, message routing, and protocol translation.

## When to Use

**Use SessionActor Pattern when:**
- You need **per-client WebSocket sessions** with isolated state
- Routing messages from **one client to multiple backend actors** (coordinator, workers)
- Building **dashboard/monitoring UIs** that observe actor state
- Implementing **client-specific routing logic** (e.g., routing to different client-side proxies)

**Use WebSocketBridge (agentic-primer) when:**
- All clients share **same ActorSystem** (no per-client isolation needed)
- Simple **broadcast** scenarios (e.g., real-time notifications)
- **Single-DO architecture** where client directly talks to one DO's ActorSystem
- No need for **complex routing** between multiple actor namespaces

## Architecture

```
┌─────────────┐                    ┌──────────────────┐
│   Browser   │◄──── WebSocket ───►│  SessionActor    │
│             │                    │  (Gateway DO)    │
└─────────────┘                    └────────┬─────────┘
                                            │
                    ┌───────────────────────┼───────────────────────┐
                    │                       │                       │
                    ▼                       ▼                       ▼
        ┌──────────────────┐   ┌──────────────────┐   ┌──────────────────┐
        │ CoordinatorActor │   │   WorkerActor    │   │  Other Actors    │
        │   (Task Queue)   │   │  (Processing)    │   │                  │
        └──────────────────┘   └──────────────────┘   └──────────────────┘
```

### Key Components

1. **SessionActor** - Gateway DO that:
   - Accepts WebSocket connections (one per client)
   - Parses wire protocol messages
   - Routes to appropriate backend actor namespaces
   - Implements client-side routing rules

2. **Wire Protocol** - Structured message format:
```typescript
interface WireMessage {
  to: string;        // "coordinator:main" or "worker:w1"
  from: string;      // "client:ui" or "client:proxy"
  type: string;      // "register_worker", "assign_task"
  payload: unknown;  // Message-specific data
  id: string;        // Request ID for correlation
  timestamp: number; // Message timestamp
  replyTo?: string;  // Optional correlation to original request
}
```

3. **Actor Addressing** - Format: `{type}:{id}`
   - Backend actors: `coordinator:main`, `worker:w1`
   - Client actors: `client:ui`, `client:coordinator-proxy`

## Implementation Example

### SessionActor Core

```typescript
export class SessionActor extends DurableObject {
  private sessionId: string;

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    this.sessionId = ctx.id.toString();
  }

  async fetch(request: Request): Promise<Response> {
    if (request.headers.get("Upgrade") !== "websocket") {
      return new Response("Expected WebSocket", { status: 400 });
    }

    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);

    // Use hibernation API - no manual tracking
    this.ctx.acceptWebSocket(server);

    // Send session established immediately
    this.sendToClient({
      type: "SessionEstablished",
      payload: { sessionId: this.sessionId },
    }, server);

    return new Response(null, {
      status: 101,
      webSocket: client,
    });
  }

  async webSocketMessage(ws: WebSocket, data: string | ArrayBuffer) {
    if (typeof data !== "string") {
      console.warn("SessionActor: Received non-text message, ignoring");
      return;
    }

    try {
      const wireMsg: WireMessage = JSON.parse(data);
      await this.receiveFromClient(wireMsg, ws);
    } catch (error) {
      console.error("SessionActor: Failed to parse message", error);
      this.sendToClient({
        type: "Error",
        payload: { error: "Invalid message format" },
      }, ws);
    }
  }
}
```

### Message Routing

```typescript
private async receiveFromClient(wireMsg: WireMessage, ws: WebSocket) {
  const [actorType, actorId] = wireMsg.to.split(":");

  if (!actorType || !actorId) {
    this.sendToClient({
      type: "Error",
      payload: { error: `Invalid 'to' field: ${wireMsg.to}` },
      replyTo: wireMsg.id,
    }, ws);
    return;
  }

  try {
    switch (actorType) {
      case "coordinator": {
        await this.routeToCoordinator(wireMsg, actorId, ws);
        break;
      }
      case "worker": {
        await this.routeToWorker(wireMsg, actorId, ws);
        break;
      }
      default: {
        this.sendToClient({
          type: "Error",
          payload: { error: `Unknown actor type: ${actorType}` },
          replyTo: wireMsg.id,
        }, ws);
      }
    }
  } catch (error) {
    console.error(`SessionActor: Error routing message to ${wireMsg.to}`, error);
    this.sendToClient({
      type: "Error",
      payload: {
        error: error instanceof Error ? error.message : "Unknown error"
      },
      replyTo: wireMsg.id,
    }, ws);
  }
}

private async routeToCoordinator(wireMsg: WireMessage, actorId: string, ws: WebSocket) {
  const stub = this.env.COORDINATOR_ACTOR.get(
    this.env.COORDINATOR_ACTOR.idFromName(actorId)
  );

  const result = await stub.handleMessage({
    type: wireMsg.type,
    payload: wireMsg.payload,
    from: `session:${this.sessionId}`,
    id: wireMsg.id,
    timestamp: wireMsg.timestamp,
  });

  this.sendToClient({
    type: `${wireMsg.type}Response`,
    payload: result,
    replyTo: wireMsg.id,
  }, ws);
}
```

### Client-Side Routing

SessionActor implements routing logic to direct responses to appropriate client-side actors:

```typescript
private sendToClient(msg: {
  type: string;
  payload: unknown;
  replyTo?: string;
}, targetWs?: WebSocket) {
  // Route responses to appropriate client-side actor
  let targetActor = "client:ui"; // Default

  const msgType = msg.type.toLowerCase();

  // Coordinator responses → coordinator-proxy
  if (msgType.includes("worker") || msgType.includes("task") || msgType.includes("status")) {
    if (msgType.includes("get_state") || msgType.includes("process_task")) {
      targetActor = "client:worker-proxy";
    } else {
      targetActor = "client:coordinator-proxy";
    }
  }

  const wireMsg: WireMessage = {
    to: targetActor,
    from: `session:${this.sessionId}`,
    type: msg.type,
    payload: msg.payload,
    id: crypto.randomUUID(),
    timestamp: Date.now(),
    ...(msg.replyTo && { replyTo: msg.replyTo }),
  };

  const messageStr = JSON.stringify(wireMsg);

  // If specific WebSocket provided, use it
  if (targetWs) {
    try {
      targetWs.send(messageStr);
    } catch (error) {
      console.error("SessionActor: Failed to send message to client", error);
    }
    return;
  }

  // Otherwise, broadcast to all active connections
  const sockets = this.ctx.getWebSockets();
  for (const ws of sockets) {
    try {
      ws.send(messageStr);
    } catch (error) {
      console.error("SessionActor: Failed to send message to client", error);
    }
  }
}
```

## Pattern Variations

### 1. Session Per User (Current Implementation)

Each WebSocket connection gets a unique SessionActor instance.

```typescript
// Router
if (path === "/ws") {
  const sessionId = url.searchParams.get("session") ?? crypto.randomUUID();
  const stub = env.SESSION_ACTOR.get(
    env.SESSION_ACTOR.idFromName(sessionId)
  );
  return stub.fetch(request);
}
```

### 2. Session Per Entity

Route different entities to different session instances:

```typescript
if (path === "/ws") {
  const userId = url.searchParams.get("userId");
  const sessionId = `user:${userId}`;
  const stub = env.SESSION_ACTOR.get(
    env.SESSION_ACTOR.idFromName(sessionId)
  );
  return stub.fetch(request);
}
```

## Comparison: SessionActor vs WebSocketBridge

| Feature | SessionActor Pattern | WebSocketBridge (agentic-primer) |
|---------|---------------------|----------------------------------|
| **Purpose** | Gateway routing layer | Direct ActorSystem bridge |
| **Isolation** | Per-client session state | Shared ActorSystem |
| **Routing** | Routes to multiple DO namespaces | Routes to local ActorSystem |
| **Use Case** | Multi-actor backend, dashboards | Single-DO actor systems |
| **Complexity** | Higher (manual routing logic) | Lower (delegates to ActorSystem) |
| **Client Protocol** | Custom wire protocol | Actor protocol (Message) |
| **State** | Session-specific routing | Stateless bridge |

## Testing Strategy

SessionActor requires comprehensive testing due to routing complexity. See `testing-actor-systems.md` for full methodology.

**Key test categories:**
1. WebSocket connection establishment
2. Message routing (coordinator, worker)
3. Wire protocol validation
4. Error handling (invalid format, unknown actor)
5. Complete workflows (register → assign → status)
6. Client-side routing rules

Example test structure:

```typescript
describe("SessionActor Message Routing to CoordinatorActor", () => {
  it("should route RegisterWorker message to coordinator and return response", async () => {
    const sessionStub = env.SESSION_ACTOR.get(
      env.SESSION_ACTOR.idFromName("test-session-coord-1")
    );

    const request = new Request("http://localhost/ws", {
      headers: { Upgrade: "websocket" },
    });

    const response = await sessionStub.fetch(request);
    const ws = response.webSocket!;
    ws.accept();

    const messages: string[] = [];
    ws.addEventListener("message", (event: MessageEvent) => {
      messages.push(event.data);
    });

    await new Promise((resolve) => setTimeout(resolve, 100));

    // Send RegisterWorker message
    const registerMsg = {
      to: "coordinator:main",
      from: "client:session-test",
      type: "register_worker",
      payload: {},
      id: crypto.randomUUID(),
      timestamp: Date.now(),
    };

    ws.send(JSON.stringify(registerMsg));
    await new Promise((resolve) => setTimeout(resolve, 200));

    const responseMessages = messages.slice(1); // Skip SessionEstablished
    expect(responseMessages.length).toBeGreaterThan(0);

    const responseMsg = JSON.parse(responseMessages[0]);
    expect(responseMsg.type).toBe("register_workerResponse");
    expect(responseMsg.payload.status).toBe("registered");
    expect(responseMsg.to).toBe("client:coordinator-proxy");
  });
});
```

## Best Practices

1. **Use Hibernation API**: Let Cloudflare manage WebSocket lifecycle
   ```typescript
   this.ctx.acceptWebSocket(server); // Don't manually track connections
   ```

2. **Error Handling**: Always send error responses back to client
   ```typescript
   try {
     await this.routeToCoordinator(wireMsg, actorId, ws);
   } catch (error) {
     this.sendToClient({
       type: "Error",
       payload: { error: error.message },
       replyTo: wireMsg.id,
     }, ws);
   }
   ```

3. **Protocol Validation**: Validate message format before routing
   ```typescript
   const [actorType, actorId] = wireMsg.to.split(":");
   if (!actorType || !actorId) {
     // Send error
     return;
   }
   ```

4. **Response Correlation**: Use `replyTo` field to correlate responses
   ```typescript
   this.sendToClient({
     type: `${wireMsg.type}Response`,
     payload: result,
     replyTo: wireMsg.id, // Correlate to original request
   }, ws);
   ```

## When Not to Use

Avoid SessionActor pattern if:
- You have a **single-DO architecture** (use WebSocketBridge instead)
- No need for **per-client isolation** (use shared WebSocketBridge)
- Simple **broadcast** scenarios (use WebSocketBridge.broadcast())
- **Low latency critical** (adds routing hop vs direct ActorSystem)

## References

- **Implementation**: `/Users/bln/play/projects/proj-20260211-140744/src/actor-system-session.ts` (lines 266-484)
- **Tests**: `/Users/bln/play/projects/proj-20260211-140744/src/session-actor.test.ts` (128 tests)
- **WebSocketBridge**: `/Users/bln/play/agentic-primer/packages/cloudflare/src/transports/websocket-bridge.ts`
- **DOActorSystem**: `/Users/bln/play/agentic-primer/packages/cloudflare/src/do-actor-system.ts`
