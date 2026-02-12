# Protocol Integration Example

This document demonstrates how SEAG integrates with `@agentic-primer/protocols` for cross-system interoperability while maintaining backward compatibility.

## Setup

```typescript
import {
  // SEAG internal types (string-based addresses)
  address,
  createMessage,
  MessageRouter,
  // Protocol adapter (conversion layer)
  toProtocolAddress,
  fromProtocolAddress,
  addressToString,
  parseProtocolAddress,
  // Protocol types
  type ProtocolAddress,
} from './messaging';

import type { Address as ProtocolAddressType } from '@agentic-primer/protocols';
```

## Use Case 1: Internal SEAG Communication

Standard SEAG code uses string addresses for ergonomics and performance:

```typescript
// Create actors with hierarchical paths
const domainSupervisor = new Supervisor('domain');
const inferenceActor = new InferenceActor('inference');
domainSupervisor.addChild('inference', inferenceActor);

// Register with router
router.registerActor('domain', domainSupervisor);

// Send message using SEAG address
const msg = createMessage(
  address('domain/inference'),  // Hierarchical path
  'user-message',
  { text: 'Analyze this code' }
);

const response = await router.send(msg);
```

**Key Points:**
- SEAG addresses are strings: `@(domain/inference)`
- Fast, ergonomic, backward compatible
- No conversion needed for internal communication

## Use Case 2: Receiving External Protocol Messages

External systems send protocol-compliant messages:

```typescript
interface ExternalMessage {
  to: ProtocolAddressType;
  from: ProtocolAddressType;
  type: string;
  payload: any;
}

async function handleExternalMessage(extMsg: ExternalMessage) {
  // Convert protocol addresses to SEAG addresses
  const toAddr = fromProtocolAddress(extMsg.to);
  const fromAddr = fromProtocolAddress(extMsg.from);

  // Create SEAG message
  const seagMsg = createMessage(
    toAddr,
    extMsg.type,
    extMsg.payload,
    { from: fromAddr }
  );

  // Route through SEAG system
  const response = await router.send(seagMsg);

  // Convert response back to protocol format
  return {
    to: extMsg.from,  // Reply to sender
    from: extMsg.to,  // From original recipient
    success: response.success,
    payload: response.payload,
  };
}

// Example: External system sends to SEAG
const externalMsg = {
  to: { id: 'inference', namespace: 'domain', scope: 'node' as const },
  from: { id: 'external-client', scope: 'node' as const },
  type: 'analyze-code',
  payload: { code: 'function test() {}' },
};

const result = await handleExternalMessage(externalMsg);
```

**Key Points:**
- External systems use protocol Address objects
- Adapter converts to SEAG strings for routing
- SEAG routes message through supervision tree
- Response converted back to protocol format

## Use Case 3: Sending to External Protocol Systems

SEAG sends messages to protocol-compliant external systems:

```typescript
async function sendToExternal(
  seagAddr: string,
  externalSystem: ExternalProtocolSystem,
  type: string,
  payload: any
) {
  // Convert SEAG address to protocol format
  const protocolAddr = toProtocolAddress(address(seagAddr));

  // Send via external system's protocol API
  await externalSystem.send({
    to: protocolAddr,
    from: { id: 'seag-system', scope: 'node' as const },
    type,
    payload,
  });
}

// Example: SEAG sends to external service
await sendToExternal(
  'domain/inference',
  externalServiceHub,
  'job-request',
  { task: 'analyze', data: '...' }
);
```

**Key Points:**
- SEAG uses string addresses internally
- Convert to protocol Address for external APIs
- Protocol Address includes namespace for routing
- External systems receive structured addresses

## Use Case 4: Hierarchical Routing with Protocol Namespace

Protocol `namespace` field maps to SEAG supervision tree path:

```typescript
// SEAG hierarchical path
const seagPath = address('workflows/build-pipeline/tasks/compile');

// Convert to protocol
const protocol = toProtocolAddress(seagPath);
console.log(protocol);
// {
//   id: 'compile',
//   namespace: 'workflows/build-pipeline/tasks',
//   scope: 'node'
// }

// Router uses namespace for hierarchical delegation
// 1. Extract root: 'workflows'
// 2. Lookup root supervisor
// 3. Delegate to supervisor.receive()
// 4. Supervisor routes to 'build-pipeline/tasks/compile'
```

**Key Insight:**
- Protocol `namespace` = SEAG supervision tree path
- Enables cross-system hierarchical routing
- External systems can address actors by namespace+id
- SEAG reconstructs path for internal routing

## Use Case 5: Address Serialization for Storage

Store addresses in databases or logs:

```typescript
// Protocol Address objects for runtime
const addr = {
  id: 'inference',
  namespace: 'domain',
  scope: 'node' as const,
  version: 'v1',
};

// Serialize for storage
const serialized = addressToString(addr);
// => 'domain/inference@v1'

// Store in database
await db.saveAddress(actorId, serialized);

// Later: load and parse
const loaded = await db.loadAddress(actorId);
const parsed = parseProtocolAddress(loaded);
// => { id: 'inference', namespace: 'domain', scope: 'node', version: 'v1' }

// Convert to SEAG for routing
const seagAddr = fromProtocolAddress(parsed);
// => '@(domain/inference)'
```

**Key Points:**
- `addressToString()` creates canonical protocol strings
- Different from SEAG `@(...)` format
- Includes version information
- Round-trips through parse/serialize

## Use Case 6: Multi-System Address Resolution

Resolve addresses across multiple systems using protocol types:

```typescript
interface AddressResolver {
  resolve(addr: ProtocolAddressType): Promise<ActorLocation>;
}

class CrossSystemResolver implements AddressResolver {
  private seagRouter: MessageRouter;
  private externalRegistry: ExternalServiceRegistry;

  async resolve(addr: ProtocolAddressType): Promise<ActorLocation> {
    // Check if address is in SEAG system
    const seagAddr = fromProtocolAddress(addr);
    const seagNode = await this.seagRouter.lookup(seagAddr);

    if (seagNode) {
      return { system: 'seag', address: seagAddr };
    }

    // Check external systems
    const external = await this.externalRegistry.lookup(addr);
    if (external) {
      return { system: external.name, address: addressToString(addr) };
    }

    throw new Error(`Address not found: ${addressToString(addr)}`);
  }
}

// Usage: Find actor across systems
const resolver = new CrossSystemResolver(router, externalRegistry);
const location = await resolver.resolve({
  id: 'inference',
  namespace: 'services',
  scope: 'node',
});

// Route based on location
if (location.system === 'seag') {
  await router.send(createMessage(address(location.address), ...));
} else {
  await externalSystems[location.system].send(...);
}
```

## Use Case 7: Versioned Actors (Future)

Protocol addresses support versioning:

```typescript
// Protocol address with version
const v1Addr = {
  id: 'inference',
  namespace: 'domain',
  scope: 'node' as const,
  version: 'v1',
};

const v2Addr = {
  id: 'inference',
  namespace: 'domain',
  scope: 'node' as const,
  version: 'v2',
};

// Serialize with version
addressToString(v1Addr);  // => 'domain/inference@v1'
addressToString(v2Addr);  // => 'domain/inference@v2'

// Note: SEAG doesn't support versioning yet
// Version is lost when converting to SEAG address
fromProtocolAddress(v1Addr);  // => '@(domain/inference)'
fromProtocolAddress(v2Addr);  // => '@(domain/inference)' (same!)
```

**Future Work:**
- Add version support to SEAG addresses
- Version-aware routing in MessageRouter
- Alias resolution for version mapping

## Benefits of Protocol Integration

### 1. Cross-System Interoperability

Protocol addresses are universal:
- SEAG can communicate with other protocol-compliant systems
- External systems can address SEAG actors
- Namespace enables hierarchical routing across systems

### 2. Backward Compatibility

SEAG string addresses still work:
- No breaking changes to existing code
- Adapter provides seamless conversion
- Protocol support is opt-in

### 3. Future-Proof Design

Protocol types support features SEAG will add:
- Versioning (v1, v2, etc.)
- Edge-scoped addresses
- Computed addresses
- Cross-system routing

### 4. Type Safety

Protocol types validated via Zod:
- Runtime validation prevents invalid addresses
- TypeScript types provide compile-time safety
- Schema-driven ensures consistency

## Performance Considerations

### Conversion Overhead

```typescript
// Benchmark: SEAG address creation
const seagAddr = address('domain/inference');
// ~10-20ns (string template literal)

// Benchmark: Protocol address creation
const protocolAddr = toProtocolAddress(seagAddr);
// ~50-100ns (object creation + string parsing)

// Recommendation: Convert at boundaries, not in hot paths
```

### Routing Performance

```typescript
// Internal SEAG routing (no conversion)
await router.send(createMessage(address('domain/inference'), ...));
// Fast: uses string addresses directly

// External protocol routing (with conversion)
const protocolAddr = toProtocolAddress(address('domain/inference'));
await externalSystem.send(protocolAddr, ...);
// Conversion overhead: ~50-100ns (negligible for network calls)
```

**Guideline:**
- Use SEAG addresses for internal communication
- Convert to protocol addresses only at system boundaries
- Conversion overhead is negligible compared to network/IO

## Summary

| Aspect | SEAG Internal | Protocol External |
|--------|---------------|-------------------|
| **Address Type** | String (`@(id)`) | Object (`{ id, namespace, scope }`) |
| **Usage** | Internal routing | Cross-system communication |
| **Performance** | Fastest (string ops) | Fast (object ops) |
| **Namespace** | Encoded in path | Explicit field |
| **Versioning** | Not supported yet | Supported |
| **Validation** | Path validation | Zod schema validation |
| **Conversion** | N/A | `toProtocolAddress()`, `fromProtocolAddress()` |

**Best Practice:**
- Keep SEAG addresses for internal code
- Convert to protocol addresses at system boundaries
- Use protocol types for external APIs and storage
- Leverage namespace for hierarchical routing

## References

- **Protocol Adapter:** `src/protocol/index.ts`
- **Protocol README:** `src/protocol/README.md`
- **Protocol Tests:** `src/protocol/__tests__/adapter.test.ts`
- **Hierarchical Routing:** `src/messaging/router.ts`
- **Supervisor Base:** `src/messaging/supervisor-base.ts`
- **Integration Strategy:** `/Users/bln/play/agentic-primer/INTEGRATION_STRATEGY.md`
