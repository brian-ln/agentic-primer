# Signal Hub Message Schema & Validators

**Status:** ✅ Complete
**Priority:** P0 (MUST FIX before MVP)
**Pattern:** JSON Schema → Generated Zod Validators

## Overview

This directory contains JSON Schema definitions and generated Zod validators for all Signal Hub message types (`hub:*`). Following the agentic-primer pattern, JSON Schema is the **source of truth** and Zod validators are **generated automatically**.

## Architecture

```text
packages/protocols/
├── schema/
│   └── hub-messages.schema.json    # Source of truth (24 message types)
└── src/
    └── hub-messages.validators.ts  # Generated Zod schemas (DO NOT EDIT)

scripts/
└── generate-hub-types.ts           # Generation script
```

## Message Types Covered

**Total:** 32 definitions (24 payload types + 8 metadata/support types)

### Connection Lifecycle (5)
- `hub:connect` - Client → Server connection with auth
- `hub:connected` - Server confirmation with capabilities
- `hub:heartbeat` - Keep-alive ping (every 25s)
- `hub:heartbeat_ack` - Server alive confirmation
- `hub:disconnect` - Graceful shutdown

### Actor Discovery (9)
- `hub:register` - Register actor with capabilities
- `hub:registered` - Registration confirmation
- `hub:unregister` - Remove actor
- `hub:discover` - Query actors by glob pattern
- `hub:discovered` - Discovery results
- `hub:list_actors` - Get all actors (paginated)
- `hub:actor_list` - Actor list results
- `hub:renew` - Renew registration
- `hub:renewed` - Renewal confirmation

### Message Delivery (9)
- `hub:send` - Point-to-point message
- `hub:delivery_ack` - Delivery confirmation
- `hub:broadcast` - Fan-out to all actors
- `hub:broadcast_ack` - Broadcast stats
- `hub:subscribe` - Subscribe to topic
- `hub:subscribed` - Subscription confirmation
- `hub:publish` - Publish to topic
- `hub:published` - Publish stats
- `hub:unsubscribe` - Unsubscribe from topic

### Flow Control (4)
- `hub:pause` - Backpressure signal
- `hub:resume` - Resume sending
- `hub:queue_stats` - Query queue depth
- `hub:queue_stats_response` - Queue metrics

### Error Handling (6)
- `hub:error` - Generic error with code
- `hub:unknown_actor` - Actor not found
- `hub:unauthorized` - Permission denied
- `hub:rate_limited` - Too many requests
- `hub:version_mismatch` - Protocol incompatibility
- `hub:message_too_large` - Size limit exceeded

## Usage

### Runtime Validation

```typescript
import { HubMessageValidators, validateHubMessage } from '@agentic-primer/protocols/hub-messages.validators';

// Validate hub:connect metadata
const result = validateHubMessage(
  HubMessageValidators.hubConnectMetadata,
  {
    protocolVersion: '0.1.0',
    capabilities: ['send', 'broadcast'],
    authToken: 'bearer eyJhbGci...'
  }
);

if (result.success) {
  console.log('Valid:', result.data);
} else {
  console.error('Validation failed:', result.error.issues);
}
```

### Type-Safe Message Construction

```typescript
import { z } from 'zod';
import { HubMessageValidators } from '@agentic-primer/protocols/hub-messages.validators';

// Infer TypeScript types from Zod schemas
type HubConnectMetadata = z.infer<typeof HubMessageValidators.hubConnectMetadata>;
type HubRegisterPayload = z.infer<typeof HubMessageValidators.hubRegisterPayload>;

// TypeScript will enforce correct structure
const metadata: HubConnectMetadata = {
  protocolVersion: '0.1.0',
  capabilities: ['send', 'broadcast']
  // TypeScript error if missing required fields
};
```

### Server-Side Validation

```typescript
import { HubMessageValidators } from '@agentic-primer/protocols/hub-messages.validators';

async function handleHubMessage(msg: SharedMessage) {
  switch (msg.type) {
    case 'hub:connect': {
      const result = validateHubMessage(
        HubMessageValidators.hubConnectMetadata,
        msg.metadata
      );
      if (!result.success) {
        return sendError('invalid_metadata', result.error);
      }
      // result.data is type-safe
      await handleConnect(result.data);
      break;
    }

    case 'hub:register': {
      const result = validateHubMessage(
        HubMessageValidators.hubRegisterPayload,
        msg.payload
      );
      if (!result.success) {
        return sendError('invalid_payload', result.error);
      }
      await handleRegister(result.data);
      break;
    }
  }
}
```

## Regenerating Validators

**IMPORTANT:** Never edit `hub-messages.validators.ts` directly. Always edit the JSON Schema and regenerate.

```bash
# 1. Edit the source schema
vim packages/protocols/schema/hub-messages.schema.json

# 2. Regenerate validators
pnpm exec tsx scripts/generate-hub-types.ts

# 3. Verify generation
git diff packages/protocols/src/hub-messages.validators.ts
```

## Adding New Message Types

1. **Edit JSON Schema** (`packages/protocols/schema/hub-messages.schema.json`):

```json
{
  "definitions": {
    "hub-new-message-payload": {
      "type": "object",
      "properties": {
        "field1": { "type": "string" },
        "field2": { "type": "integer", "minimum": 0 }
      },
      "required": ["field1", "field2"],
      "additionalProperties": false
    }
  }
}
```

1. **Regenerate:**

```bash
pnpm exec tsx scripts/generate-hub-types.ts
```

1. **Use in code:**

```typescript
import { HubMessageValidators } from '@agentic-primer/protocols/hub-messages.validators';

const result = validateHubMessage(
  HubMessageValidators.hubNewMessagePayload,
  data
);
```

## Validation Features

### Strict Object Validation

All objects use `.strict()` to prevent extra properties:

```typescript
// ✅ Valid
{ protocolVersion: '0.1.0', capabilities: ['send'] }

// ❌ Invalid - extra property
{ protocolVersion: '0.1.0', capabilities: ['send'], extraField: 'bad' }
```

### Array Constraints

```typescript
// capabilities: minItems: 1
capabilities: ['send']        // ✅ Valid
capabilities: []               // ❌ Invalid - too short

// limit: min: 1, max: 100
limit: 50                      // ✅ Valid
limit: 0                       // ❌ Invalid - too low
limit: 150                     // ❌ Invalid - too high
```

### Integer Ranges

```typescript
// timestamp: minimum: 0
timestamp: Date.now()          // ✅ Valid
timestamp: -1                  // ❌ Invalid - negative

// priority: 0-2
priority: 1                    // ✅ Valid (0=high, 1=medium, 2=low)
priority: 3                    // ❌ Invalid - out of range
```

### Enums

```typescript
// status: enum
status: 'delivered'            // ✅ Valid
status: 'queued'               // ✅ Valid
status: 'pending'              // ❌ Invalid - not in enum
```

## Schema Reference

See [`MESSAGE_TYPES.md`](../../docs/signal-hub/MESSAGE_TYPES.md) for detailed protocol specifications.

## Pattern Consistency

This follows the same pattern as `domain.validators.ts`:

- **JSON Schema** as source of truth
- **Topological sort** for dependency ordering
- **Generated Zod schemas** with strict validation
- **Export object** for easy import
- **Helper function** for validation

## Testing

```typescript
import { describe, it, expect } from 'vitest';
import { HubMessageValidators } from './hub-messages.validators';

describe('Hub Message Validators', () => {
  it('validates hub:connect metadata', () => {
    const valid = {
      protocolVersion: '0.1.0',
      capabilities: ['send', 'broadcast']
    };

    const result = HubMessageValidators.hubConnectMetadata.safeParse(valid);
    expect(result.success).toBe(true);
  });

  it('rejects invalid hub:connect metadata', () => {
    const invalid = {
      protocolVersion: '0.1.0',
      capabilities: []  // Empty array not allowed
    };

    const result = HubMessageValidators.hubConnectMetadata.safeParse(invalid);
    expect(result.success).toBe(false);
  });
});
```

## Next Steps

1. ✅ JSON Schema created (32 definitions)
2. ✅ Generation script created
3. ✅ Zod validators generated
4. ✅ Documentation complete
5. ⏳ Integrate into Signal Hub server (validation middleware)
6. ⏳ Add to client SDK (type-safe message builders)
7. ⏳ Add comprehensive test suite

## Related Files

- **Specification:** `/docs/signal-hub/MESSAGE_TYPES.md`
- **JSON Schema:** `/packages/protocols/schema/hub-messages.schema.json`
- **Generated Validators:** `/packages/protocols/src/hub-messages.validators.ts`
- **Generation Script:** `/scripts/generate-hub-types.ts`
- **Domain Pattern:** `/packages/protocols/src/domain.validators.ts`
