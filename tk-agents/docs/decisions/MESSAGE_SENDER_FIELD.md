# ADR: Add Sender Field to Message Interface

**Status:** Accepted
**Date:** 2026-01-16
**Deciders:** User, Claude
**Related:** Actor system, Message protocol, Registry routing

## Context

The actor system uses a Message/Response protocol where actors communicate via the Registry. Messages originally contained only:
- `id` - unique identifier
- `type` - message type
- `payload` - content
- `correlationId` - optional request matching

**Problem**: Actors had no way to know who sent them a message. This limited:
- Task coordination (tasks can't identify which actor is working on them)
- Multi-party protocols (actors can't reply directly to senders)
- Audit trails (no sender tracking for debugging/logging)
- Supervision trees (parents can't track child actors)

## Decision

Add an **optional** `sender?: string` field to the Message interface.

```typescript
interface Message {
  id: string;
  type: string;
  payload: unknown;
  correlationId?: string;
  sender?: string;  // ← NEW
}
```

### Sender Population
- Registry automatically sets `sender: "registry"` when routing messages
- Actors can override by setting `sender` to their own ID when forwarding
- Optional field ensures backward compatibility

## Alternatives Considered

### Alternative 1: No Sender Field (Status Quo)
**Pros:**
- ✅ Simplest implementation
- ✅ Actors are completely stateless
- ✅ No sender lifecycle management needed

**Cons:**
- ❌ Can't build multi-party protocols
- ❌ No conversation threading
- ❌ Limited coordination capabilities
- ❌ Poor audit trails

**Rejected**: Too limiting for task coordination and bootstrap goals.

### Alternative 2: Required Sender Field
```typescript
sender: string;  // Required
```

**Pros:**
- ✅ Always know who sent a message
- ✅ Simpler logic (no undefined checks)
- ✅ Better type safety

**Cons:**
- ❌ Breaking change (all existing code breaks)
- ❌ Forces complexity on simple actors
- ❌ Requires migration of all messages

**Rejected**: Breaking change unacceptable, forces unnecessary complexity.

### Alternative 3: Optional Sender Field (Chosen)
```typescript
sender?: string;  // Optional
```

**Pros:**
- ✅ Backward compatible (existing code works)
- ✅ Simple actors can ignore sender
- ✅ Complex actors can use sender for coordination
- ✅ Registry can auto-populate
- ✅ Enables gradual adoption

**Cons:**
- ⚠️ Actors must handle `undefined` case
- ⚠️ Adds minimal complexity to Message interface
- ⚠️ Registry becomes slightly stateful (knows sender context)

**Accepted**: Best balance of compatibility and capability.

## Consequences

### Positive
1. **Task Coordination Enabled**: Tasks can track which actors are working on them
2. **Multi-Party Protocols**: Actors can reply directly to senders or forward messages
3. **Audit Trails**: Complete message provenance for debugging
4. **Backward Compatible**: All existing code continues to work
5. **Simple Migration**: New code can adopt incrementally

### Negative
1. **Optional Handling**: Actors must check `if (message.sender)` before using
2. **Registry Complexity**: Registry tracks sender context during routing
3. **Potential Confusion**: Multiple sources of sender (registry, forwarding actors)

### Neutral
1. **Design Pattern**: Establishes pattern for future optional fields
2. **Documentation Burden**: Must explain when/how sender is populated

## Implementation Notes

### Registry Behavior
```typescript
// In Registry.send()
const messageWithSender: Message = {
  ...message,
  sender: message.sender ?? "registry"  // Auto-populate if not set
};
```

### Actor Usage Patterns

**Pattern 1: Read Sender**
```typescript
async send(message: Message): Promise<Response> {
  console.log(`Received from: ${message.sender ?? "unknown"}`);
  // ... process message
}
```

**Pattern 2: Forward with Sender**
```typescript
async send(message: Message): Promise<Response> {
  // Forward to another actor, preserving original sender
  return this.registry.send(otherActorId, {
    ...message,
    sender: this.id  // Or keep message.sender for provenance
  });
}
```

**Pattern 3: Ignore Sender**
```typescript
async send(message: Message): Promise<Response> {
  // Simple actors can completely ignore sender
  return { success: true, data: processPayload(message.payload) };
}
```

## Trade-offs Accepted

1. **Optional = Runtime Checks**: Accepting that actors must handle `undefined` in exchange for backward compatibility
2. **Registry Statefulness**: Registry now knows about sender context, but keeps implementation simple
3. **Multiple Sender Semantics**: "registry" vs actor IDs vs custom values - accepting flexibility over strictness

## Future Considerations

### Possible Enhancements
1. **`replyTo` Field**: Add separate `replyTo?: string` for explicit reply routing
2. **Sender Chain**: Track full message path for complex forwarding scenarios
3. **Sender Validation**: Add authorization checks based on sender identity
4. **Conversation Threading**: Use sender + correlationId for multi-turn conversations

### Migration Path
If we ever need **required** sender:
1. Add deprecation warnings for messages without sender
2. Update all actor implementations to set sender
3. Make field required in next major version
4. All existing uses already handle optional case

## Validation

### Tests
- ✅ 100 tests passing (14 new tests added)
- ✅ Registry correctly populates sender
- ✅ Forwarding actors correctly propagate sender
- ✅ Backward compatibility verified (optional field works)

### Examples
- ✅ `examples/minimal-actors.ts` includes ForwardingActor demonstrating sender usage
- ✅ Documentation updated in ACTOR_INTERFACE.md

## References

- [ACTOR_INTERFACE.md](../../ACTOR_INTERFACE.md) - Interface documentation
- [base.ts](../../src/actors/base.ts) - Message interface definition
- [registry.ts](../../src/actors/registry.ts) - Registry sender population
- [minimal-actors.ts](../../examples/minimal-actors.ts) - Usage examples
