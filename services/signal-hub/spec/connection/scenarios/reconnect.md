# Scenario: Reconnection After Disconnect

## Setup

- Client previously connected and registered
- Connection lost (network issue, server restart, etc.)
- Client implements auto-reconnect with exponential backoff
- Actor address: `browser/client-ui`

## Flow

1. **Detect Disconnect**
   ```javascript
   ws.onclose = (event) => {
     console.log(`Disconnected: code=${event.code}`);
     if (config.autoReconnect) {
       scheduleReconnect();
     }
   };
   ```

2. **Exponential Backoff**
   - Attempt 1: Wait 100ms
   - Attempt 2: Wait 200ms
   - Attempt 3: Wait 400ms
   - Max delay: 30 seconds
   - Add 25% jitter to prevent thundering herd

3. **New WebSocket Connection**
   - Client creates new WebSocket
   - Server accepts connection
   - New session created with fresh `sessionId`

4. **Client → Server: hub:connect**
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

5. **Server → Client: hub:connected**
   - New session established
   - **Important:** New `sessionId` (different from previous)

6. **Client Re-registers**
   ```json
   {
     "type": "hub:register",
     "from": "browser/client-ui",
     "to": "cloudflare/signal-hub",
     "payload": {
       "actorAddress": "browser/client-ui",
       "capabilities": ["send", "receive"],
       "metadata": {},
       "ttl": 300000
     }
   }
   ```

7. **Client Re-subscribes to Topics**
   ```javascript
   // Restore subscriptions from local state
   for (const topic of subscribedTopics) {
     await client.subscribe(topic);
   }
   ```

## Expected Outcome

- New session established with new sessionId
- Actor re-registered in registry
- All topic subscriptions restored
- Client ready to resume normal operation

## Timing Considerations

- **Old Registration TTL:** May still be active if disconnect was brief
- **Duplicate Detection:** Server may detect duplicate registration and close old session
- **Race Condition:** If old registration expires during reconnect, no conflict

## Verification

```typescript
// After reconnection
client.isConnected() === true
client.sessionId !== previousSessionId  // New session
registry.has(actorAddress) === true     // Re-registered
```

## Error Paths

- **Max Retries Exceeded:** Client gives up after 10 attempts
- **JWT Expired:** Client must re-authenticate before reconnecting
- **Registration Conflict:** Server rejects if old session still active

## See Also

- [Duplicate Connection](./duplicate-connection.md) - Handling registration conflicts
- [Initial Connect](./initial-connect.md) - Normal connection flow
