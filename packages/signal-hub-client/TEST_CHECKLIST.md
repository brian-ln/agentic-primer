# Signal Hub Client - Test Checklist

**Package:** @agentic-primer/signal-hub-client
**Version:** 0.1.0
**Date:** 2026-02-16

---

## Build & Package Tests

- [x] TypeScript compilation (no errors)
- [x] Type declarations generated (.d.ts files)
- [x] Source maps generated (.js.map, .d.ts.map)
- [x] ESM output in dist/
- [ ] Package exports work correctly
- [ ] Peer dependencies resolved
- [ ] Package size < 100 KB

## Connection Tests

### Basic Connection
- [ ] Connect to local Signal Hub (ws://localhost:8787)
- [ ] Connect to remote Signal Hub (wss://...)
- [ ] JWT authentication succeeds
- [ ] hub:connected received
- [ ] Session ID assigned
- [ ] Server version received
- [ ] Actor identity populated

### Connection Failures
- [ ] Invalid URL rejected
- [ ] Invalid JWT rejected
- [ ] Connection timeout (10s)
- [ ] Network error handled
- [ ] Error event emitted

### Disconnection
- [ ] Manual disconnect works
- [ ] hub:disconnect sent
- [ ] WebSocket closed cleanly
- [ ] State changed to 'disconnected'
- [ ] disconnected event emitted

## Reconnection Tests

### Auto-Reconnect
- [ ] Reconnect on network failure
- [ ] Exponential backoff (1s, 2s, 4s, 8s, 16s, 30s)
- [ ] State changes to 'reconnecting'
- [ ] Successful reconnection
- [ ] Message queue flushed after reconnect

### Reconnect Limits
- [ ] Max attempts respected
- [ ] Error emitted when max reached
- [ ] Manual disconnect stops reconnect

## Actor Registration Tests

### Registration
- [ ] Register with explicit address
- [ ] Register with auto-generated address
- [ ] Register with capabilities
- [ ] Register with metadata
- [ ] hub:register sent
- [ ] hub:registered received
- [ ] Actor added to registry
- [ ] actors property updated

### Registration Failures
- [ ] Invalid address rejected
- [ ] Registration timeout (5s)
- [ ] hub:error handled
- [ ] Error event emitted

### Unregistration
- [ ] Unregister actor
- [ ] hub:unregister sent
- [ ] Actor removed from registry
- [ ] actors property updated

## Messaging Tests

### Fire-and-Forget (tell)
- [ ] Send message (tell pattern)
- [ ] hub:send created correctly
- [ ] Flat payload structure
- [ ] Message queued if disconnected
- [ ] Message sent when connected

### Acknowledged (ask)
- [ ] Send with ack (ask pattern)
- [ ] hub:send with pattern='ask'
- [ ] hub:delivery_ack received
- [ ] Promise resolves with message ID
- [ ] Timeout after TTL + 5s

### Message Options
- [ ] Custom TTL respected
- [ ] Trace ID included
- [ ] Priority set correctly
- [ ] Metadata preserved

### Message Failures
- [ ] Unknown actor error
- [ ] Timeout handling
- [ ] hub:error handling
- [ ] Promise rejection

## Heartbeat Tests

### Basic Heartbeat
- [ ] Heartbeat sent every 25s
- [ ] hub:heartbeat message format
- [ ] hub:heartbeat_ack received
- [ ] Last ack timestamp updated

### Heartbeat Failures
- [ ] No ack for 10s detected
- [ ] Connection considered dead
- [ ] Error event emitted
- [ ] Disconnect triggered

### Heartbeat Lifecycle
- [ ] Started on connect
- [ ] Stopped on disconnect
- [ ] Restarted on reconnect

## Event System Tests

### Message Events
- [ ] message event emitted
- [ ] Event includes SharedMessage
- [ ] originalFrom populated if forwarded
- [ ] forwarded flag set correctly

### Connection Events
- [ ] connected event emitted
- [ ] Event includes session info
- [ ] disconnected event emitted
- [ ] Event includes connection info

### State Change Events
- [ ] stateChange event emitted
- [ ] All 5 states tested:
  - [ ] disconnected
  - [ ] connecting
  - [ ] connected
  - [ ] reconnecting
  - [ ] disconnecting

### Error Events
- [ ] error event emitted
- [ ] Event includes message
- [ ] Event includes code (if available)
- [ ] Event includes error object

### Handler Management
- [ ] on() registers handler
- [ ] off() removes handler
- [ ] Multiple handlers work
- [ ] Handler errors caught

## State Management Tests

### Connection States
- [ ] Initial state: disconnected
- [ ] connecting state during connect
- [ ] connected state after hub:connected
- [ ] reconnecting state during auto-reconnect
- [ ] disconnecting state during disconnect
- [ ] State transitions are correct

### State Properties
- [ ] connected getter accurate
- [ ] connectionState getter accurate
- [ ] session getter accurate
- [ ] actors getter accurate

## Protocol Compliance Tests

### SharedMessage Format
- [ ] id is UUID
- [ ] from is CanonicalAddress
- [ ] to is CanonicalAddress
- [ ] type is string
- [ ] pattern is 'tell' or 'ask'
- [ ] timestamp is epoch ms
- [ ] payload is unknown
- [ ] metadata is object
- [ ] ttl is number or null
- [ ] signature is string or null

### Hub Messages
- [ ] hub:connect format correct
- [ ] hub:register format correct
- [ ] hub:unregister format correct
- [ ] hub:send flat payload format
- [ ] hub:heartbeat format correct
- [ ] hub:disconnect format correct

### Message Routing
- [ ] to='@(cloudflare/signal-hub)' for hub messages
- [ ] from address set correctly
- [ ] correlationId linked for ask/response
- [ ] metadata preserved

## Browser Compatibility Tests

### Chrome
- [ ] Connection works
- [ ] Messages work
- [ ] Events work
- [ ] No console errors

### Firefox
- [ ] Connection works
- [ ] Messages work
- [ ] Events work
- [ ] No console errors

### Safari
- [ ] Connection works
- [ ] Messages work
- [ ] Events work
- [ ] No console errors

### Edge
- [ ] Connection works
- [ ] Messages work
- [ ] Events work
- [ ] No console errors

## Node.js Tests

### With WebSocket Polyfill
- [ ] Import works
- [ ] Connection works
- [ ] Messages work
- [ ] Events work
- [ ] No errors

## Example Tests

### basic-usage.html
- [ ] Page loads
- [ ] Connect button works
- [ ] Register actor works
- [ ] Send message works
- [ ] Send with ack works
- [ ] UI updates correctly
- [ ] No console errors

### advanced-usage.html
- [ ] Page loads
- [ ] Multi-actor registration
- [ ] Message flow visualization
- [ ] Statistics tracking
- [ ] Broadcast works
- [ ] UI responsive
- [ ] No console errors

### node-usage.js
- [ ] Script runs
- [ ] Connection succeeds
- [ ] Registration succeeds
- [ ] Message sent
- [ ] Clean disconnect
- [ ] Exit code 0

## Performance Tests

### Memory
- [ ] No memory leaks on connect/disconnect
- [ ] Message queue cleared after flush
- [ ] Event handlers garbage collected
- [ ] Actor registry cleaned up

### Network
- [ ] Heartbeat overhead acceptable
- [ ] Message size reasonable
- [ ] No excessive reconnections
- [ ] Backoff delays correct

### Scalability
- [ ] 10 actors registered
- [ ] 100 actors registered
- [ ] 1000 messages sent
- [ ] Message queue handles 100+ messages

## Security Tests

### Authentication
- [ ] JWT sent in hub:connect
- [ ] Invalid JWT rejected
- [ ] Token not stored persistently
- [ ] Token not logged

### Address Validation
- [ ] Valid addresses accepted
- [ ] Invalid addresses rejected
- [ ] CanonicalAddress format enforced
- [ ] Empty address rejected

### Message Integrity
- [ ] UUIDs are unique
- [ ] Timestamps are accurate
- [ ] TTL enforced
- [ ] No message tampering

## Edge Cases

### Network Conditions
- [ ] Slow connection
- [ ] Intermittent connection
- [ ] Connection drops during send
- [ ] Connection drops during register
- [ ] Rapid connect/disconnect

### Invalid Data
- [ ] Invalid JSON from server
- [ ] Missing required fields
- [ ] Wrong message types
- [ ] Malformed hub messages

### Timing Issues
- [ ] Connect timeout
- [ ] Registration timeout
- [ ] Message timeout
- [ ] Heartbeat timeout
- [ ] Concurrent operations

### Boundary Conditions
- [ ] Empty message payload
- [ ] Large message (near 1MB)
- [ ] Very long address
- [ ] Empty capabilities array
- [ ] Null metadata

## Documentation Tests

### README.md
- [ ] API reference accurate
- [ ] Examples work
- [ ] Installation instructions correct
- [ ] Links valid

### IMPLEMENTATION.md
- [ ] Architecture accurate
- [ ] Protocol compliance documented
- [ ] Limitations documented
- [ ] Future enhancements listed

### CHANGELOG.md
- [ ] Version history accurate
- [ ] Features documented
- [ ] Format correct

### Type Definitions
- [ ] All types exported
- [ ] JSDoc comments present
- [ ] Examples in comments
- [ ] No type errors

## Integration Tests

### With Signal Hub
- [ ] Connect to local hub
- [ ] Register actors
- [ ] Send/receive messages
- [ ] Heartbeat works
- [ ] Hub errors handled
- [ ] Clean disconnect

### Multi-Client
- [ ] Two clients connect
- [ ] Clients send to each other
- [ ] Messages routed correctly
- [ ] No cross-talk

### Load Testing
- [ ] 10 concurrent clients
- [ ] 100 messages/second
- [ ] Hub stays responsive
- [ ] No dropped messages

---

## Test Results Summary

**Total Tests:** ~200
**Passing:** _TBD_
**Failing:** _TBD_
**Skipped:** _TBD_

**Last Tested:** _TBD_
**Test Environment:** _TBD_
**Tested By:** _TBD_

---

## Notes

- Use `examples/basic-usage.html` for manual testing
- Use `examples/advanced-usage.html` for multi-actor scenarios
- Use `examples/node-usage.js` for Node.js testing
- Requires Signal Hub running locally or remotely
- Default local URL: ws://localhost:8787
- Default JWT: 'dev-token' (for development)

## Running Tests

### Manual Browser Testing
```bash
# Start Signal Hub
cd packages/cloudflare/signal-hub
npm run dev

# Open examples (in separate terminal)
open packages/signal-hub-client/examples/basic-usage.html
open packages/signal-hub-client/examples/advanced-usage.html
```

### Node.js Testing
```bash
cd packages/signal-hub-client
npm install ws
node examples/node-usage.js
```

### Build Testing
```bash
cd packages/signal-hub-client
npm run build
ls -lh dist/
```
