# Scenario: Authentication Failure

## Setup

- WebSocket connection established
- Client has invalid or missing JWT
- `AUTH_ENABLED=true` in server configuration
- Client attempts to connect

## Flow: Missing JWT

1. **Client → Server: hub:connect (No JWT)**
   ```json
   {
     "type": "hub:connect",
     "from": "browser/client-ui",
     "to": "cloudflare/signal-hub",
     "payload": {
       "version": "1.0"
     }
   }
   ```

2. **Server Validation**
   ```typescript
   if (env.AUTH_ENABLED === 'true') {
     if (!msg.payload.jwt) {
       throw new HubError('unauthorized', 'JWT required');
     }
   }
   ```

3. **Server → Client: hub:error**
   ```json
   {
     "type": "hub:error",
     "from": "cloudflare/signal-hub",
     "to": "browser/client-ui",
     "payload": {
       "code": "unauthorized",
       "message": "JWT required",
       "details": {}
     }
   }
   ```

4. **Server Closes Connection**
   ```typescript
   ws.close(1000, 'Unauthorized');
   ```

5. **Client Cleanup**
   - Connection state: `disconnected`
   - No reconnect attempt (invalid credentials)
   - Emit `error` event for application handling

## Flow: Invalid JWT Signature

1. **Client → Server: hub:connect (Malformed JWT)**
   ```json
   {
     "type": "hub:connect",
     "from": "browser/client-ui",
     "to": "cloudflare/signal-hub",
     "payload": {
       "version": "1.0",
       "jwt": "invalid.signature.here"
     }
   }
   ```

2. **Server Validation Fails**
   ```typescript
   try {
     const payload = await validateJWT(jwt, env.JWT_SECRET);
   } catch (err) {
     throw new HubError('unauthorized', `Invalid JWT: ${err.message}`);
   }
   ```

3. **Server → Client: hub:error**
   ```json
   {
     "type": "hub:error",
     "from": "cloudflare/signal-hub",
     "to": "browser/client-ui",
     "payload": {
       "code": "unauthorized",
       "message": "Invalid JWT: signature verification failed",
       "details": {}
     }
   }
   ```

## Flow: Expired JWT

1. **JWT Expiration Check**
   ```typescript
   if (decoded.exp < Date.now() / 1000) {
     throw new HubError('unauthorized', 'JWT expired');
   }
   ```

2. **Server → Client: hub:error**
   ```json
   {
     "type": "hub:error",
     "from": "cloudflare/signal-hub",
     "to": "browser/client-ui",
     "payload": {
       "code": "unauthorized",
       "message": "JWT expired",
       "details": {
         "expiredAt": 1708271000
       }
     }
   }
   ```

## Expected Outcome

- Connection rejected
- WebSocket closed with code 1000
- Session NOT created or immediately deleted
- Client receives clear error message

## Client Recovery

```javascript
client.on('error', async (error) => {
  if (error.code === 'unauthorized') {
    // Re-authenticate with auth provider
    const newJWT = await authService.authenticate();
    
    // Retry connection with new token
    await client.connect({ jwt: newJWT });
  }
});
```

## Verification

```typescript
// Server state after auth failure
sessions.has(ws) === false  // Session not created or deleted
connections.size === 0      // No active connections
registry.size === 0         // No actors registered
```

## Security Considerations

- Server MUST validate JWT before processing any other messages
- Failed authentication MUST close connection immediately
- Error messages SHOULD NOT leak sensitive information
- Rate limit connection attempts to prevent brute force

## See Also

- [Initial Connect](./initial-connect.md) - Successful authentication
- [Reconnect](./reconnect.md) - Re-authentication after token expiry
