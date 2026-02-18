# Heartbeat Interval Analysis

**Date:** 2026-02-17
**Context:** Evaluate whether to increase heartbeat interval from current 25s client / 30s server configuration

## Current Configuration

### Client (SignalHubClient)
```typescript
// packages/signal-hub-client/src/SignalHubClient.ts:87
this.heartbeatInterval = options.heartbeatInterval ?? 25000; // 25s (< 30s Cloudflare hibernation)
```

### Server (SignalHub)
```toml
# services/signal-hub/wrangler.toml
HEARTBEAT_INTERVAL = "30000"  # 30s
```

### Protocol Spec
```
Client sends: Every 25 seconds
Server expects: Within 60 seconds (hardcoded in connection logic)
Timeout detection: 60 second window
```

**Sources:**
- `/Users/bln/play/agentic-primer/services/signal-hub/spec/connection/CONNECTION.spec.md`
- `/Users/bln/play/agentic-primer/services/signal-hub/ARCHITECTURE.md`

## Key Facts About Hibernation

### What Hibernation Does
- **Automatic:** Cloudflare DOs hibernate after ~30s of idle time
- **Transparent:** Application code is unaware of hibernation
- **State Preserved:** In-memory state (sessions, registry) persists across hibernation
- **Auto-Wake:** DO automatically wakes when new WebSocket message arrives
- **Cannot Be Prevented:** Heartbeats do NOT prevent hibernation

**Source:** `/Users/bln/play/agentic-primer/services/signal-hub/spec/connection/scenarios/hibernation-wake.md`

### Purpose of Heartbeat (Post-Hibernation Understanding)

**Original Misconception (commented in code):**
```typescript
// 25s (< 30s Cloudflare hibernation)  ❌ INCORRECT
```

**Actual Purpose:**
- Detect dead connections (client crashed, network partition, etc.)
- NOT to prevent hibernation (impossible)
- Provide liveness signal for connection health monitoring

## Analysis

### Trade-offs

#### Current (25s client / 60s timeout)
**Pros:**
- Fast detection of dead connections (within 60s)
- Frequent liveness signals
- Well under timeout threshold

**Cons:**
- More network traffic (heartbeat every 25s)
- More CPU cycles for heartbeat processing
- No actual benefit for hibernation prevention (was false assumption)

#### Proposed (60s client / 120s timeout)
**Pros:**
- Reduced network traffic (60s vs 25s = 2.4x reduction)
- Reduced CPU overhead
- Still provides dead connection detection
- Aligns timeout with actual heartbeat interval (cleaner design)

**Cons:**
- Slower dead connection detection (120s vs 60s)
- Larger window for "ghost" connections to exist

#### Alternative (45s client / 90s timeout)
**Pros:**
- Middle ground: better than 25s, faster than 60s
- 2x reduction in network traffic vs current
- 90s detection window (acceptable for most use cases)

**Cons:**
- Arbitrary values (not clean multiples)
- Still relatively frequent

## Recommendation

**Increase heartbeat interval to 60s client / 120s server timeout**

### Rationale

1. **Hibernation Myth Debunked:** The 25s interval was chosen to be "< 30s Cloudflare hibernation", but this was based on a misconception. Heartbeats cannot prevent hibernation, so optimizing for this is pointless.

2. **Network Efficiency:** Reducing from 25s to 60s cuts heartbeat traffic by 2.4x. For a system with thousands of connected actors, this is significant.

3. **Sufficient Dead Connection Detection:** A 120s window is acceptable for detecting crashed clients or network partitions. Most applications don't need sub-minute detection.

4. **Industry Standard:** Many real-time systems use 60s heartbeats (e.g., MQTT default, some WebSocket implementations).

5. **Clean Design:** Timeout = 2 × heartbeat interval is a clean ratio that provides one missed heartbeat tolerance.

### Implementation Changes

If recommendation accepted, update:

1. **Client default:**
```typescript
// packages/signal-hub-client/src/SignalHubClient.ts:87
this.heartbeatInterval = options.heartbeatInterval ?? 60000; // 60s for dead connection detection
```

2. **Server config:**
```toml
# services/signal-hub/wrangler.toml
HEARTBEAT_INTERVAL = "60000"  # 60s
```

3. **Connection handler:**
```typescript
// Adjust timeout check from 60s to 120s
const HEARTBEAT_TIMEOUT = 120000; // 2 × heartbeat interval
```

4. **Update documentation:**
- Remove references to "prevent hibernation"
- Update ARCHITECTURE.md with new intervals
- Update CONNECTION.spec.md protocol documentation

### Risk Assessment

**Low Risk** - This is a tuning parameter change that:
- Does not affect correctness
- Only impacts dead connection detection speed
- Can be easily rolled back if needed
- Can be made configurable per-client if needed

### Alternative: Make It Configurable

If uncertain, make the interval configurable per deployment:
```typescript
// Allow override via env var
HEARTBEAT_INTERVAL = env.HEARTBEAT_INTERVAL ?? "60000"
```

This allows experimentation in production without code changes.

## Conclusion

**Increase heartbeat interval from 25s to 60s (client) and timeout from 60s to 120s (server).**

The original 25s interval was optimized for a false assumption (preventing hibernation). With correct understanding of Cloudflare DO hibernation behavior, a 60s interval provides sufficient dead connection detection with significantly reduced overhead.

---

**Note:** All integration tests (71 tests) currently passing. Changes should be validated with same test suite after implementation.
