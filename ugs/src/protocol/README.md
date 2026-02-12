# Protocol Adapter Layer

Bidirectional conversion between SEAG internal types and `@agentic-primer/protocols` types.

## Purpose

SEAG uses string-based addresses (`@(id)`) for backward compatibility and convenience.
The convergence domain protocols use structured Address objects for cross-system interoperability.

This adapter layer provides seamless conversion between the two representations while:
- Maintaining backward compatibility with existing SEAG code
- Supporting hierarchical path-based addressing
- Enabling protocol-compliant external integrations
- Preserving namespace information for routing

## Type Mapping

### SEAG → Protocol

| SEAG Address String | Protocol Address Object |
|---------------------|-------------------------|
| `@(actor-123)` | `{ id: 'actor-123', scope: 'node' }` |
| `@(domain/inference)` | `{ id: 'inference', namespace: 'domain', scope: 'node' }` |
| `@(workflows/build/tasks/compile)` | `{ id: 'compile', namespace: 'workflows/build/tasks', scope: 'node' }` |

**Key Points:**
- Flat IDs have no namespace
- Hierarchical paths split into namespace + id (last segment)
- All addresses default to `scope: 'node'`

### Protocol → SEAG

| Protocol Address Object | SEAG Address String |
|-------------------------|---------------------|
| `{ id: 'actor-123', scope: 'node' }` | `@(actor-123)` |
| `{ id: 'inference', namespace: 'domain', scope: 'node' }` | `@(domain/inference)` |
| `{ id: 'compile', namespace: 'workflows/build/tasks', scope: 'node' }` | `@(workflows/build/tasks/compile)` |

**Key Points:**
- Namespace + id reconstructs full path
- Version information is discarded in SEAG representation
- SEAG always uses `@(...)` format

## API

### Conversion Functions

#### `toProtocolAddress(seagAddr: SeagAddress): ProtocolAddress`

Convert SEAG `@(id)` string to Protocol Address object.

```typescript
import { toProtocolAddress } from './protocol';
import { address } from './messaging/message';

// Flat ID
toProtocolAddress(address('actor-123'))
// => { id: 'actor-123', scope: 'node' }

// Hierarchical path
toProtocolAddress(address('domain/inference'))
// => { id: 'inference', namespace: 'domain', scope: 'node' }

// Deep path
toProtocolAddress(address('workflows/build/tasks/compile'))
// => { id: 'compile', namespace: 'workflows/build/tasks', scope: 'node' }
```

#### `fromProtocolAddress(protocolAddr: ProtocolAddress): SeagAddress`

Convert Protocol Address object to SEAG `@(id)` string.

```typescript
import { fromProtocolAddress } from './protocol';

// Flat ID
fromProtocolAddress({ id: 'actor-123', scope: 'node' })
// => '@(actor-123)'

// With namespace
fromProtocolAddress({ id: 'inference', namespace: 'domain', scope: 'node' })
// => '@(domain/inference)'

// With version (version is ignored)
fromProtocolAddress({ id: 'inference', namespace: 'domain', scope: 'node', version: 'v1' })
// => '@(domain/inference)'
```

### String Serialization

#### `addressToString(addr: ProtocolAddress): string`

Serialize Protocol Address to canonical string representation.

**Different from `fromProtocolAddress`:** This creates protocol-standard strings (no `@(...)`), suitable for storage, display, or external systems.

```typescript
import { addressToString } from './protocol';

// Flat ID
addressToString({ id: 'actor-123', scope: 'node' })
// => 'actor-123'

// Namespaced
addressToString({ id: 'inference', namespace: 'domain', scope: 'node' })
// => 'domain/inference'

// Versioned
addressToString({ id: 'inference', namespace: 'domain', scope: 'node', version: 'v1' })
// => 'domain/inference@v1'
```

#### `parseProtocolAddress(str: string): ProtocolAddress`

Parse canonical protocol string to Address object.

Supports:
- Flat IDs: `actor-123`
- Namespaced: `domain/inference`
- Versioned: `domain/inference@v1`
- Deep paths: `workflows/build/tasks/compile`

```typescript
import { parseProtocolAddress } from './protocol';

parseProtocolAddress('domain/inference')
// => { id: 'inference', namespace: 'domain', scope: 'node' }

parseProtocolAddress('domain/inference@v1')
// => { id: 'inference', namespace: 'domain', scope: 'node', version: 'v1' }
```

### Validation & Construction

#### `validateProtocolAddress(addr: ProtocolAddress): boolean`

Validate Protocol Address using official `@agentic-primer/protocols` validator.

```typescript
import { validateProtocolAddress } from './protocol';

validateProtocolAddress({ id: 'test', scope: 'node' })
// => true

validateProtocolAddress({ id: 'test' } as any)
// => false (missing scope)
```

#### `createProtocolAddress(id: string, namespace?: string | null, version?: string | null): ProtocolAddress`

Create Protocol Address from parts with validation.

```typescript
import { createProtocolAddress } from './protocol';

// Flat ID
createProtocolAddress('actor-123')
// => { id: 'actor-123', namespace: null, scope: 'node' }

// Namespaced
createProtocolAddress('inference', 'domain')
// => { id: 'inference', namespace: 'domain', scope: 'node' }

// Versioned
createProtocolAddress('inference', 'domain', 'v1')
// => { id: 'inference', namespace: 'domain', scope: 'node', version: 'v1' }

// Throws on invalid input
createProtocolAddress('')
// => throws Error: Address ID cannot be empty
```

## Integration with Path-Based Addressing

The protocol adapter seamlessly integrates with SEAG's hierarchical path-based addressing:

```typescript
import { toProtocolAddress, fromProtocolAddress } from './protocol';
import { address } from './messaging/message';

// Hierarchical routing path
const seagPath = address('workflows/build-pipeline/tasks/compile');

// Convert to protocol (namespace captures supervision tree path)
const protocol = toProtocolAddress(seagPath);
console.log(protocol.namespace);
// => 'workflows/build-pipeline/tasks'
console.log(protocol.id);
// => 'compile'

// Convert back for routing
const back = fromProtocolAddress(protocol);
// => '@(workflows/build-pipeline/tasks/compile)'
```

**Key Insight:** The protocol `namespace` field maps directly to SEAG's supervision tree path, enabling:
- Hierarchical routing via `Address.namespace`
- Path-based queries
- Cross-system address resolution
- Versioned addressing (future)

## Round-Trip Guarantees

All conversions preserve essential information through round-trips:

### SEAG ↔ Protocol

```typescript
// Flat ID
const flat = address('actor-123');
const flatRoundTrip = fromProtocolAddress(toProtocolAddress(flat));
assert(flat === flatRoundTrip); // ✓

// Hierarchical path
const hier = address('domain/inference');
const hierRoundTrip = fromProtocolAddress(toProtocolAddress(hier));
assert(hier === hierRoundTrip); // ✓
```

### Protocol String ↔ Protocol Object

```typescript
// Namespaced
const str = 'domain/inference@v1';
const strRoundTrip = addressToString(parseProtocolAddress(str));
assert(str === strRoundTrip); // ✓
```

**Note:** Version information is lost when converting to SEAG addresses (SEAG doesn't support versioning yet).

## Usage in SEAG

### Internal Code (SEAG Addresses)

Most SEAG code continues using string addresses:

```typescript
import { address, createMessage } from './messaging/message';

const msg = createMessage(
  address('domain/inference'),
  'user-message',
  { text: 'Hello' }
);
```

### External Integrations (Protocol Addresses)

When interacting with protocol-compliant systems:

```typescript
import { toProtocolAddress, fromProtocolAddress } from './protocol';
import { address } from './messaging/message';

// Receive external protocol message
function handleExternalMessage(externalAddr: ProtocolAddress, payload: any) {
  // Convert to SEAG for routing
  const seagAddr = fromProtocolAddress(externalAddr);
  const msg = createMessage(seagAddr, 'external-event', payload);
  router.send(msg);
}

// Send to external protocol system
function sendToExternal(seagAddr: SeagAddress, payload: any) {
  // Convert to protocol for interop
  const protocolAddr = toProtocolAddress(seagAddr);
  externalSystem.send(protocolAddr, payload);
}
```

## Design Decisions

### 1. Keep SEAG Addresses as Strings

**Rationale:**
- String addresses are ergonomic and familiar
- Backward compatible with existing code
- Fast equality checks (`===`)
- Easy to log and debug

**Trade-off:** Requires conversion layer for protocol compliance.

### 2. Protocol as Canonical External Representation

**Rationale:**
- Structured addresses enable cross-system interoperability
- Protocol types are validated via Zod schemas
- Namespace field maps cleanly to hierarchical routing
- Future-proof for versioning and scoping

**Trade-off:** Internal/external type mismatch requires adapter layer.

### 3. Namespace = Supervision Tree Path

**Rationale:**
- Supervision tree naturally forms hierarchical namespaces
- `namespace` enables path-based routing in MessageRouter
- Aligns with protocol's intent for namespace qualifier
- Supports future multi-tenancy and scoping

**Trade-off:** Namespace must match supervision structure.

### 4. Discard Version in SEAG Conversion

**Rationale:**
- SEAG doesn't support versioning yet
- Version is optional in protocol
- Can add versioning support later without breaking changes

**Trade-off:** Version information lost in round-trip through SEAG.

## Future Work

### 1. Version Support in SEAG

Add version awareness to SEAG addresses:

```typescript
// Future: versioned SEAG addresses
address('domain/inference', { version: 'v1' })
// => '@(domain/inference@v1)'
```

### 2. Edge and Computed Scope Support

Currently only `scope: 'node'` is supported. Future:

```typescript
// Edge-scoped addresses
createProtocolAddress('rel-123', null, null, {
  scope: { edge: { source: 'a', target: 'b' } }
})

// Computed addresses
createProtocolAddress('result', 'query', null, {
  scope: { computed: 'SELECT * FROM nodes' }
})
```

### 3. Alias Resolution Integration

Integrate with alias resolver for protocol-aware alias handling:

```typescript
// Future: resolve aliases via protocol
resolveProtocolAddress({ id: 'llm', namespace: 'services' })
// => { id: 'inference', namespace: 'domain', ... }
```

## Testing

Comprehensive test coverage in `__tests__/adapter.test.ts`:

- ✓ Conversion functions (toProtocolAddress, fromProtocolAddress)
- ✓ Round-trip guarantees (SEAG ↔ Protocol)
- ✓ String serialization (addressToString, parseProtocolAddress)
- ✓ Validation (validateProtocolAddress)
- ✓ Construction (createProtocolAddress)
- ✓ Integration with path-based addressing

Run tests:

```bash
bun test src/protocol/__tests__/adapter.test.ts
```

## References

- **Protocol Package:** `@agentic-primer/protocols`
- **Protocol Schema:** `/Users/bln/play/agentic-primer/packages/protocols/schema/domain.schema.json`
- **SEAG Message Types:** `src/messaging/message.ts`
- **Hierarchical Routing:** `src/messaging/router.ts`, `src/messaging/supervisor-base.ts`
- **Integration Strategy:** `/Users/bln/play/agentic-primer/INTEGRATION_STRATEGY.md`
