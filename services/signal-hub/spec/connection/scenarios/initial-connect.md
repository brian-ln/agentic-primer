# Scenario: Initial Connection

## Setup

- WebSocket connection established (HTTP 101 Upgrade complete)
- Client has valid JWT token
- Protocol version matches server (both 1.0)
- Server state: empty sessions map

## Flow

1. **WebSocket Accept**
   - Server creates session with `connectionState: 'connecting'`
   - Session stored in `sessions` map (key: WebSocket object)
   - `lastHeartbeat` initialized to current timestamp

2. **Client → Server: hub:connect**
   ```json
   {
     "type": "hub:connect",
     "from": "browser/client-ui",
     "to": "cloudflare/signal-hub",
     "payload": {
       "version": "1.0",
       "jwt": "eyJhbGc..."
     }
   }
   ```

3. **Server validates JWT**
   - Signature verification succeeds
   - Token not expired
   - Extract `actorId: "browser/client-ui"`
   - Extract `capabilities: ["send", "receive", "discover"]`

4. **Server updates session**
   - `session.actorIdentity = "browser/client-ui"`
   - `session.capabilities = ["send", "receive", "discover"]`
   - `session.authenticated = true`
   - `session.connectionState = 'connected'`

5. **Server → Client: hub:connected**
   ```json
   {
     "type": "hub:connected",
     "from": "cloudflare/signal-hub",
     "to": "browser/client-ui",
     "payload": {
       "sessionId": "sess_abc123",
       "actorIdentity": "browser/client-ui",
       "capabilities": ["send", "receive", "discover"],
       "serverTime": 1708272000000
     }
   }
   ```

6. **Client starts heartbeat timer**
   - Interval: 30 seconds
   - Sends `hub:heartbeat` periodically

## Expected Outcome

- Session state: `connected`
- Client can send all message types
- Rate limiting active (100 msg/min)
- Heartbeat mechanism active
- Session stored in both `sessions` and `connections` maps

## Verification

```typescript
// Server state after connection
sessions.has(ws) === true
sessions.get(ws).connectionState === 'connected'
sessions.get(ws).authenticated === true
sessions.get(ws).actorIdentity === 'browser/client-ui'
connections.has(sessionId) === true
```

## Error Paths

This scenario assumes success. See other scenarios for error handling:
- [Auth Failure](./auth-failure.md) - Invalid JWT
- [Version Mismatch](./version-mismatch.md) - Protocol version incompatible
