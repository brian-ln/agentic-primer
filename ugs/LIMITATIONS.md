# Known Limitations

**Last Updated**: 2026-02-03

This document tracks known limitations and constraints of the simplify-message-layer implementation.

---

## Performance Limitations

### Throughput Below Target [MEASURED]

**Limitation**: Sustained throughput of 690 msg/sec, below 10k msg/sec target

**Measured**: [MEASURED: mitata benchmarks on M4 Max, Bun 1.2.20]
- Sequential: 890 msg/sec
- Batched 10k: 690 msg/sec
- Message creation alone: 37,500/sec

**Root Cause**: [INFERRED from measurements]
- Bottleneck is program execution via `new Function()`, not messaging layer
- Promise/async overhead per operation
- No message batching optimizations

**Workarounds**:
1. Use `tell()` instead of `ask()` (2x faster: 612ns vs 1.13µs)
2. Batch operations in groups of 100
3. Use native actors instead of programs for high-throughput scenarios
4. Deploy multiple router instances for parallel actor pools

**Target Use Cases**:
- ✅ Low-latency actor communication (sub-millisecond)
- ✅ Moderate throughput applications (hundreds to low thousands msg/sec)
- ❌ High-throughput stream processing (>10k msg/sec sustained)

---

## Security Limitations

### CodeExecutionActor Sandboxing [DOCUMENTED]

**Limitation**: Sandboxing is defense-in-depth, not cryptographically secure

**Known Issues**:
1. **Constructor chain escapes**: [UNTESTED - requires validation]
   - Potential: `(function(){}).constructor.constructor('return process')()`
   - Status: Not yet tested, likely vulnerable

2. **Timeout doesn't work for sync loops**: [DOCUMENTED]
   - `while(true) {}` cannot be interrupted
   - JavaScript limitation - sync code cannot be preempted
   - Timeout only works for async operations and finite sync code

3. **`with` statement used**: [DOCUMENTED]
   - Deprecated feature, not secure in strict mode
   - Provides convenience but not isolation

**Threat Model**:
- ✅ Protects against accidental misuse (user typos, mistakes)
- ✅ Blocks common attack vectors (require, fs, process)
- ⚠️ Does **not** defend against determined attackers
- ❌ Should not be sole security layer for untrusted code

**Recommendations**:
1. Use Worker threads for production untrusted code execution
2. Implement static code analysis to detect dangerous patterns
3. Rate-limit execution requests
4. Log all execution attempts for audit
5. Consider vm2 or isolated-vm for stronger isolation

### FileSystemActor Symlink Bypass [HYPOTHESIS]

**Limitation**: Symlinks may bypass path validation

**Scenario**: [UNTESTED - requires validation]
```bash
# Create symlink outside basePath
ln -s /etc/passwd data/malicious-link

# Validation checks resolved path
resolve('./data', 'malicious-link') → './data/malicious-link' ✅

# But file read follows symlink
Bun.file('./data/malicious-link').text() → reads /etc/passwd ❌
```

**Status**: Hypothesis based on code inspection, not yet tested

**Mitigation** (if confirmed):
```typescript
// Add before validation
const resolved = realpathSync(resolve(this.basePath, path));
```

**Workaround Until Fixed**:
- Deploy FileSystemActor in environments where symlink creation is restricted
- Monitor for suspicious symlink creation
- Use read-only basePath where possible

---

## Streaming Limitations

### Real LLM Streaming Not Verified [HYPOTHESIS]

**Limitation**: Demo uses mock tokens, not real LLM API

**Status**:
- ✅ Callback infrastructure verified [VERIFIED: mock streaming demo]
- ✅ ModelManager supports streaming option [VERIFIED: code inspection]
- ⚠️ End-to-end LLM streaming untested [HYPOTHESIS - requires credentials]

**Blockers**:
- Missing CLOUDFLARE_API_TOKEN in environment
- No test credentials available

**What's Tested**:
- Router.streamAsk() invokes actor.stream()
- SessionActor.stream() calls onChunk callback
- Tokens delivered progressively (not batched)
- Error propagation works

**What's Untested**:
- Real LLM API integration
- Network I/O handling
- Token rate and timing with real models
- Error scenarios from LLM provider

### No Backpressure Handling [DOCUMENTED]

**Limitation**: Fast producers can overwhelm slow consumers

**Impact**:
- Memory growth if tokens arrive faster than consumer processes
- No flow control mechanism
- No queue size limits

**Planned**: Phase 5 enhancement

### No Stream Cancellation [DOCUMENTED]

**Limitation**: Cannot abort in-flight streaming requests

**Impact**:
- Wasted API calls if user navigates away
- Wasted bandwidth and latency
- No cleanup mechanism

**Planned**: Phase 5 enhancement (AbortController pattern)

---

## Platform Limitations

### Single Platform Tested [MEASURED]

**Limitation**: Benchmarks only on Apple M4 Max @ 3.79-3.83 GHz, Bun 1.2.20

**Generalization**: [HYPOTHESIS for other platforms]
- Performance on Intel/AMD CPUs: Unknown
- Performance on Node.js: Unknown (Bun-specific APIs used)
- Performance on Linux/Windows: Unknown

**Known Bun-Specific APIs**:
- `Bun.file()` for file operations
- `Bun.write()` for file writes
- Memory characteristics may differ on Node.js

---

## Integration Limitations

### Tool Actor Performance Not Benchmarked [UNMEASURED]

**Limitation**: FileSystemActor and CodeExecutionActor latency not measured

**Unknown**:
- P95 latency for file operations
- P95 latency for code execution
- Memory overhead per operation
- Throughput limits

**Status**: Benchmarking in progress (Phase 5)

### No Stress Testing [UNTESTED]

**Limitation**: System not tested under sustained high load

**Unknowns**:
- Performance degradation over time
- Memory leaks under load
- Concurrent operation limits
- Recovery after errors

**Status**: Stress testing in progress (Phase 5)

---

## Completed Features (No Longer Limited)

### ✅ Real-Time Streaming
- **Was**: No streaming support
- **Now**: Callback-based streaming implemented [VERIFIED]
- **Commit**: 8f71274

### ✅ Performance Benchmarks
- **Was**: Performance claims were [HYPOTHESIS]
- **Now**: Comprehensive benchmarks with [MEASURED] tags
- **Commit**: 68947a4

### ✅ Tool Actors
- **Was**: Only BashToolActor, ReadToolActor, WriteToolActor
- **Now**: Added FileSystemActor and CodeExecutionActor
- **Commit**: 804c9d1

---

## Future Work

**High Priority**:
1. Test and document security vulnerabilities (symlinks, constructor escape)
2. Benchmark tool actor performance
3. Verify real LLM streaming with credentials
4. Stress test under sustained load

**Medium Priority**:
5. Add backpressure handling to streaming
6. Implement stream cancellation
7. Improve CodeExecutionActor sandboxing (Worker threads)
8. Add file locking to FileSystemActor

**Low Priority**:
9. Test on multiple platforms (Intel/AMD, Node.js, Linux/Windows)
10. Optimize throughput (message batching, native actors)
11. Add multiple consumer support for streaming
12. Implement AsyncIterator wrapper for streaming

---

**Maintenance**: Update this document as limitations are addressed or new ones discovered.
