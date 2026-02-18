# Scenario: Hibernation and Wake

## Setup

- Client connected and idle for 30+ seconds
- No messages sent or received
- Cloudflare hibernates WebSocket (automatic, not preventable)
- In-memory state (session, registry) persists across hibernation

## Flow

1. **Hibernation (Automatic)**
   - WebSocket enters low-power state
   - No explicit server code runs
   - In-memory state preserved (sessions Map, registry Map)
   - Connection appears normal to client

2. **Client Sends Message (Triggers Wake)**
   ```json
   {
     "type": "hub:heartbeat",
     "from": "browser/client-ui",
     "to": "cloudflare/signal-hub",
     "payload": {
       "timestamp": 1708272030000
     }
   }
   ```

3. **Automatic Wake**
   - Cloudflare wakes Durable Object
   - Message delivered to `webSocketMessage` handler
   - Session retrieved from `sessions` Map (still exists!)
   - Handler processes normally

4. **Server → Client: hub:heartbeat_ack**
   ```json
   {
     "type": "hub:heartbeat_ack",
     "from": "cloudflare/signal-hub",
     "to": "browser/client-ui",
     "payload": {
       "serverTime": 1708272030100
     }
   }
   ```

## Expected Outcome

- WebSocket wakes transparently
- No state loss
- Normal operation resumes
- Client unaware of hibernation

## Key Insights

### What Persists

✅ In-memory state (sessions, registry, subscriptions)  
✅ WebSocket connection  
✅ Session authentication  
✅ Actor registrations  

### What Does NOT Persist (Eviction, Not Hibernation)

❌ Only lost on Durable Object eviction (different from hibernation)  
❌ Eviction closes all WebSockets (code 1001: "Going Away")  
❌ Clients must reconnect after eviction  

## Verification

```typescript
// After wake
sessions.has(ws) === true  // Session still exists
registry.has(actorAddress) === true  // Registration intact
session.lastHeartbeat // Updated to new timestamp
```

## Common Misconceptions

**Myth:** "Heartbeats prevent hibernation"  
**Reality:** Cannot prevent hibernation. Heartbeats only detect dead connections.

**Myth:** "Need to wake manually"  
**Reality:** Automatic wake on incoming message. No control needed.

**Myth:** "State lost during hibernation"  
**Reality:** In-memory state persists. Only lost on eviction (different event).

## Eviction vs. Hibernation

| Event | WebSocket | State | Recovery |
|-------|-----------|-------|----------|
| **Hibernation** | Stays open | Persists | Automatic wake on message |
| **Eviction** | Closes (1001) | Lost | Client reconnects, re-registers |

## See Also

- [Reconnect](./reconnect.md) - Recovery after eviction
- [Initial Connect](./initial-connect.md) - Normal connection
