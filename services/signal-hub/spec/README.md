# Signal Hub Specification (v1.0)

Hybrid domain-structured specification with machine-readable formats.

**Generated:** 2026-02-17
**Protocol Version:** 1.0
**Status:** Production

## Quick Navigation

### By Domain

| Domain | Description | Spec | Protocol | State Machine | Scenarios |
|--------|-------------|------|----------|---------------|-----------|
| **[Connection](./connection/)** | WebSocket lifecycle | [CONNECTION.spec.md](./connection/CONNECTION.spec.md) | [protocol.json](./connection/protocol.json) | [state-machine.json](./connection/state-machine.json) | [scenarios/](./connection/scenarios/) |
| **[Registration](./registration/)** | Actor registry | [REGISTRATION.spec.md](./registration/REGISTRATION.spec.md) | [protocol.json](./registration/protocol.json) | — | [scenarios/](./registration/scenarios/) |
| **[Messaging](./messaging/)** | Point-to-point | [MESSAGING.spec.md](./messaging/MESSAGING.spec.md) | [protocol.json](./messaging/protocol.json) | — | [scenarios/](./messaging/scenarios/) |
| **[Pub/Sub](./pubsub/)** | Topic-based | [PUBSUB.spec.md](./pubsub/PUBSUB.spec.md) | [protocol.json](./pubsub/protocol.json) | — | [scenarios/](./pubsub/scenarios/) |

### Shared Resources

| Resource | Description | Location |
|----------|-------------|----------|
| **Core Schemas** | SharedMessage, CanonicalAddress, ErrorResponse | [schemas/](./schemas/) |
| **Original Specs** | Archived flat specs (4059 lines) | [../docs/](../docs/) |

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      Signal Hub (DO)                        │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │ Connection   │  │ Registration │  │  Messaging   │     │
│  │  Lifecycle   │  │   Registry   │  │  (P2P/Bcast) │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Pub/Sub    │  │ Rate Limiting│  │   Auth/JWT   │     │
│  │   Topics     │  │ (100 msg/min)│  │  (Optional)  │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
└─────────────────────────────────────────────────────────────┘
         │                    │                    │
    WebSocket           WebSocket            WebSocket
         │                    │                    │
   ┌──────────┐        ┌──────────┐        ┌──────────┐
   │ Browser  │        │   SEAG   │        │ Browser  │
   │ Client   │        │  Agent   │        │ Client   │
   └──────────┘        └──────────┘        └──────────┘
```

## Specification Structure

### Hybrid Organization

This spec uses **Option D: Hybrid Domain Structure**:

- **Shared schemas** (JSON Schema format) in `schemas/`
- **Domain groupings** (connection, registration, messaging, pubsub)
- **Machine-readable** protocol definitions (`protocol.json`)
- **Executable** state machines (`state-machine.json`)
- **Scenario-driven** examples (markdown)

### File Types

| Format | Purpose | Examples |
|--------|---------|----------|
| `.spec.md` | Human-readable domain specs | CONNECTION.spec.md |
| `.schema.json` | JSON Schema definitions | shared-message.schema.json |
| `protocol.json` | AsyncAPI-style message specs | connection/protocol.json |
| `state-machine.json` | XState-compatible FSM | connection/state-machine.json |
| `scenarios/*.md` | Use case documentation | initial-connect.md |

## Quick Start

### For Implementers

1. **Read core concepts:**
   - [Connection lifecycle](./connection/CONNECTION.spec.md)
   - [Shared message format](./schemas/shared-message.schema.json)

2. **Understand state machines:**
   - [Connection FSM](./connection/state-machine.json)

3. **Review scenarios:**
   - [Initial connect](./connection/scenarios/initial-connect.md)
   - [Reconnection](./connection/scenarios/reconnect.md)

4. **Implement message handlers:**
   - [Registration protocol](./registration/protocol.json)
   - [Messaging protocol](./messaging/protocol.json)
   - [Pub/Sub protocol](./pubsub/protocol.json)

### For Clients

1. **Connection setup:**
   ```typescript
   const client = new SignalHubClient({
     url: 'wss://signal-hub.example.com',
     actorAddress: 'browser/my-app',
     jwt: '<auth-token>'
   });
   await client.connect();
   ```

2. **Actor registration:**
   ```typescript
   await client.register({
     actorAddress: 'browser/my-app',
     capabilities: ['send', 'receive'],
     metadata: { version: '1.0.0' },
     ttl: 300000  // 5 minutes
   });
   ```

3. **Messaging:**
   ```typescript
   // Point-to-point
   await client.send({
     to: 'seag/agent-1',
     payload: { type: 'inference_request', prompt: '...' }
   });

   // Pub/Sub
   await client.subscribe('system/events');
   await client.publish('system/events', { action: 'clicked' });
   ```

## Domain Details

### Connection Domain

**Scope:** WebSocket lifecycle, authentication, heartbeats

**Key Messages:**
- `hub:connect` - Establish connection with JWT
- `hub:heartbeat` - Keep-alive (30s interval)
- `hub:disconnect` - Graceful shutdown

**State Machine:** [connection/state-machine.json](./connection/state-machine.json)

**Scenarios:**
- [Initial connect](./connection/scenarios/initial-connect.md)
- [Reconnection](./connection/scenarios/reconnect.md)

### Registration Domain

**Scope:** Actor registry, discovery, TTL management

**Key Messages:**
- `hub:register` - Register actor in registry
- `hub:discover` - Query for actors by capabilities
- `hub:renew` - Extend registration TTL

**Schemas:** [registration/schemas/](./registration/schemas/)

**Protocol:** [registration/protocol.json](./registration/protocol.json)

### Messaging Domain

**Scope:** Point-to-point and broadcast messaging

**Key Messages:**
- `hub:send` - Send to specific actor (fire-and-forget)
- `hub:broadcast` - Send to all registered actors

**Delivery:** At-most-once (no guarantees)

**Protocol:** [messaging/protocol.json](./messaging/protocol.json)

### Pub/Sub Domain

**Scope:** Topic-based publish-subscribe

**Key Messages:**
- `hub:subscribe` - Subscribe to topic
- `hub:publish` - Publish to topic subscribers
- `hub:unsubscribe` - Remove subscription

**Delivery:** Best-effort fanout to all subscribers

**Schemas:** [pubsub/schemas/](./pubsub/schemas/)

## Validation

### JSON Schema Validation

```bash
# Install AJV CLI
npm install -g ajv-cli

# Validate a message
ajv validate \
  -s schemas/shared-message.schema.json \
  -d example-message.json
```

### State Machine Validation

```bash
# Using XState CLI (if available)
xstate validate connection/state-machine.json
```

### Protocol Testing

See original test suite:
```
/tests/integration/signal-hub/
├── connection.test.ts
├── registration.test.ts
├── messaging.test.ts
└── pubsub.test.ts
```

## Migration from Flat Specs

Original specs archived at `/services/signal-hub/docs/`:

| Old File | New Location | Notes |
|----------|--------------|-------|
| PROTOCOL.spec.md | Split across domains | Sections by message type |
| STATE_MACHINE.spec.md | connection/state-machine.json | Machine-readable FSM |
| SCHEMAS.spec.md | schemas/*.schema.json | JSON Schema format |
| EDGE_CASES.spec.md | */scenarios/*.md | Scenario documents |
| SERVER.spec.md | Preserved as-is | Implementation details |
| CLIENT.spec.md | Preserved as-is | Implementation details |

## Cross-References

### Internal

- **Implementation:** [../src/](../src/)
- **Tests:** [../../../tests/integration/signal-hub/](../../../tests/integration/signal-hub/)
- **Original docs:** [../docs/](../docs/)

### External

- **Shared protocols:** `@agentic-primer/protocols`
- **SEAG client:** `/ugs/src/messaging/signal-hub/`
- **Browser client:** `/packages/signal-hub-client/`

## Contributing

When updating specs:

1. Update machine-readable formats first (`.schema.json`, `protocol.json`)
2. Regenerate or update `.spec.md` files
3. Add scenario documents for new use cases
4. Update this README's navigation
5. Run validation:
   ```bash
   npm run validate-schemas
   ```

## Versioning

- **Spec version:** 1.0 (this document)
- **Protocol version:** 1.0 (wire protocol)
- **Schema version:** Draft-07 (JSON Schema)

### Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-02-17 | Initial hybrid domain structure |
| 0.x | 2026-02-01 | Flat specification (archived) |

## License

Same as Signal Hub codebase.
