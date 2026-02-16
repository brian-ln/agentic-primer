# Hub Message Validators - Quick Reference

## Import

```typescript
import { HubMessageValidators, validateHubMessage } from '@agentic-primer/protocols';
```

## Validate a Message

```typescript
const result = validateHubMessage(
  HubMessageValidators.hubConnectMetadata,
  data
);

if (result.success) {
  console.log('Valid:', result.data);
} else {
  console.error('Invalid:', result.error.issues);
}
```

## All Available Validators

### Connection
```typescript
HubMessageValidators.hubConnectMetadata
HubMessageValidators.hubConnectedPayload
HubMessageValidators.hubConnectedMetadata
HubMessageValidators.hubHeartbeatPayload
HubMessageValidators.hubHeartbeatAckPayload
HubMessageValidators.hubDisconnectPayload
```

### Discovery
```typescript
HubMessageValidators.hubRegisterPayload
HubMessageValidators.hubRegisterMetadata
HubMessageValidators.hubRegisteredPayload
HubMessageValidators.hubUnregisterPayload
HubMessageValidators.hubDiscoverPayload
HubMessageValidators.hubDiscoveredPayload
HubMessageValidators.hubListActorsPayload
HubMessageValidators.hubActorListPayload
HubMessageValidators.hubRenewPayload
HubMessageValidators.hubRenewedPayload
HubMessageValidators.actorRegistration
```

### Delivery
```typescript
HubMessageValidators.hubSendPayload
HubMessageValidators.hubSendMetadata
HubMessageValidators.hubDeliveryAckPayload
HubMessageValidators.hubBroadcastPayload
HubMessageValidators.hubBroadcastMetadata
HubMessageValidators.hubBroadcastAckPayload
HubMessageValidators.hubSubscribePayload
HubMessageValidators.hubSubscribedPayload
HubMessageValidators.hubPublishPayload
HubMessageValidators.hubPublishedPayload
HubMessageValidators.hubUnsubscribePayload
```

### Flow Control
```typescript
HubMessageValidators.hubPausePayload
HubMessageValidators.hubResumePayload
HubMessageValidators.hubQueueStatsPayload
HubMessageValidators.hubQueueStatsResponsePayload
```

### Errors
```typescript
HubMessageValidators.hubErrorPayload
HubMessageValidators.hubUnknownActorPayload
HubMessageValidators.hubUnauthorizedPayload
HubMessageValidators.hubRateLimitedPayload
HubMessageValidators.hubVersionMismatchPayload
HubMessageValidators.hubMessageTooLargePayload
HubMessageValidators.hubErrorCode
```

### Core Types
```typescript
HubMessageValidators.canonicalAddress
```

## Type Inference

```typescript
import { z } from 'zod';

type HubConnectMetadata = z.infer<typeof HubMessageValidators.hubConnectMetadata>;
type HubRegisterPayload = z.infer<typeof HubMessageValidators.hubRegisterPayload>;
type HubErrorPayload = z.infer<typeof HubMessageValidators.hubErrorPayload>;
```

## Common Patterns

### Validate Message Metadata

```typescript
function validateMetadata(msg: SharedMessage) {
  if (msg.type === 'hub:connect') {
    return validateHubMessage(
      HubMessageValidators.hubConnectMetadata,
      msg.metadata
    );
  }
}
```

### Validate Message Payload

```typescript
function validatePayload(msg: SharedMessage) {
  if (msg.type === 'hub:register') {
    return validateHubMessage(
      HubMessageValidators.hubRegisterPayload,
      msg.payload
    );
  }
}
```

### Type-Safe Handler

```typescript
import { z } from 'zod';

function handleConnect(
  metadata: z.infer<typeof HubMessageValidators.hubConnectMetadata>
) {
  // metadata.protocolVersion is type-safe
  // metadata.capabilities is string[]
  // TypeScript enforces correct structure
}
```

## Regenerate Validators

```bash
pnpm generate:hub-types
```

## See Also

- Full docs: `packages/protocols/HUB_MESSAGES_SCHEMA.md`
- Message spec: `docs/signal-hub/MESSAGE_TYPES.md`
- JSON Schema: `packages/protocols/schema/hub-messages.schema.json`
