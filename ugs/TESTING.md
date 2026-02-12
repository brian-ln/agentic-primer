# Testing Documentation

## Overview

This project includes comprehensive testing across three dimensions:
1. **Integration Tests** - Validate all components work together
2. **Benchmarks** - Measure performance characteristics
3. **Stress Tests** - Verify stability under load

## Test Suite

### Integration Tests (`test-integration.ts`)

Tests the complete system with all actors working together:

**Coverage:**
- ✅ Stream mock inference to file
- ✅ Execute code, capture output, save to file
- ✅ Concurrent operations across all actors (30 operations)
- ✅ Cross-actor workflows (file read → code execution → file write)
- ✅ Error handling across actors
- ✅ List directory contents
- ✅ Delete file operation

**Run:**
```bash
bun run test:integration
# or
bun test ./test-integration.ts
```

**Results:**
```
 7 pass
 0 fail
 40 expect() calls
Ran 7 tests across 1 file. [71ms]
```

### Benchmarks

#### 1. Message Creation (`message-creation.bench.ts`)

Measures overhead of message utilities.

**Key Metrics:**
- createMessage: 71-259ns
- address(): 304ps (sub-nanosecond!)
- generateMessageId(): 45ns
- 100 message batch: 8.1µs (81ns/msg)

**Run:**
```bash
bun run bench:creation
```

#### 2. Message Routing (`routing.bench.ts`)

Measures Router.tell() and Router.ask() latency.

**Key Metrics:**
- Router.tell(): 612ns
- Router.ask(): 1.10µs
- ask round-trip: 1.13µs
- 10 concurrent asks: 14.4µs (1.44µs/msg)

**Run:**
```bash
bun run bench:routing
```

#### 3. Messaging Patterns (`patterns.bench.ts`)

Measures throughput, concurrency, and memory stability.

**Key Metrics:**
- Throughput: 690 msg/sec (10k batched)
- 100 concurrent operations: 139µs (1.39µs/msg)
- Memory: 34MB heap growth over 10k messages

**Run:**
```bash
bun run bench:patterns
```

#### 4. Tool Actors (`tool-actors.bench.ts`)

Measures FileSystemActor and CodeExecutionActor performance.

**FileSystemActor Metrics:**
- Write small file (100B): 20.69µs
- Write medium file (10KB): 24.96µs
- Write large file (100KB): 41.40µs
- Read file: 21.62µs
- List directory: 1.87ms
- Delete file: 103.65µs
- Write + read round-trip: 51.79µs

**CodeExecutionActor Metrics:**
- Simple arithmetic: 897ns
- Array operation: 806ns
- String manipulation: 994ns
- JSON processing: 869ns
- Loop computation: 833ns
- With console output: 980ns

**Combined Workflows:**
- Code → File write: 25.48µs
- File read → Code → File write: 61.41ms (avg, high variance due to caching)

**Run:**
```bash
bun run bench:tools
```

#### Run All Benchmarks

```bash
bun run bench
```

This runs all four benchmark suites and displays a summary.

### Stress Test (`test-stress.ts`)

Tests system stability under heavy load.

**Configuration:**
- 1000 messages across all actors
- Batch size: 50 messages
- Actors: FileSystem, CodeExecution, Session (mock)

**Key Metrics:**
- Overall throughput: 41,666 msg/sec
- Heap growth: 0MB (stable)
- FileSystem P95 latency: 1ms
- Code execution P95 latency: 1ms
- Streaming P95 latency: 2ms

**Run:**
```bash
bun run test:stress
```

**Results:**
```
🏁 Stress Test Complete: 4/4 checks passed

✨ All performance targets met under load!

Key Findings:
  • System stable under 1000 message load
  • Memory growth linear and acceptable
  • Latencies consistent with single-operation benchmarks
  • No performance degradation detected
```

## Performance Targets

| Target                     | Measured        | Status |
|----------------------------|-----------------|--------|
| Message creation < 1µs P95 | 92-315ns P95    | ✅ 3-10x better |
| Routing latency < 100µs P95| 1.3-16.7µs P95  | ✅ 6-75x better |
| FileSystem ops < 1ms P95   | 20-120µs P95    | ✅ 8-50x better |
| Code execution < 5ms P95   | 1-3µs P95       | ✅ 1600x better |
| Throughput > 10k msg/sec   | 690 msg/sec     | ❌ 14x slower (program execution) |
| Memory stable over 10k msg | 0-34MB growth   | ✅ Stable |
| Stress test stability      | 0MB heap growth | ✅ Excellent |

## Quick Test Commands

```bash
# Run all tests
bun test

# Run specific test suites
bun run test:integration    # Integration tests
bun run test:stress         # Stress test

# Run benchmarks
bun run bench              # All benchmarks
bun run bench:creation     # Message creation
bun run bench:routing      # Message routing
bun run bench:patterns     # Messaging patterns
bun run bench:tools        # Tool actors
```

## Test Environment

**Platform:** Apple M4 Max @ 3.76-3.83 GHz
**Runtime:** Bun 1.2.20 (arm64-darwin)
**OS:** macOS (Darwin 25.2.0)

## Test Data

Tests create temporary data in:
- `./data/test-integration/` - Integration test files
- `./data/bench-tool-actors/` - Benchmark temporary files
- `./data/test-stress/` - Stress test files

All test data directories are cleaned up automatically after tests complete.

## Interpreting Results

### Latency Units
- **ns (nanoseconds)**: 1 billionth of a second (10⁻⁹s)
- **µs (microseconds)**: 1 millionth of a second (10⁻⁶s)
- **ms (milliseconds)**: 1 thousandth of a second (10⁻³s)

### Percentiles
- **P50 (median)**: 50% of operations complete faster than this
- **P75**: 75% of operations complete faster than this
- **P95**: 95% of operations complete faster than this
- **P99**: 99% of operations complete faster than this

### What to Watch For
- **Consistent latencies**: P95 and P99 should be close to median
- **Memory stability**: Heap growth should be linear, not exponential
- **Throughput**: Operations per second under load
- **Concurrency**: Performance with multiple simultaneous operations

## CI/CD Integration

Add to your CI pipeline:

```yaml
# Example GitHub Actions
- name: Run Tests
  run: |
    bun run test:integration
    bun run test:stress

- name: Run Benchmarks
  run: bun run bench
```

## Troubleshooting

### Tests Timeout
If integration tests timeout, check:
1. Mock actors are being used (not real LLM calls)
2. File permissions in test data directories
3. Available system memory

### Benchmark Variance
Benchmarks may show variance due to:
1. System load (close other applications)
2. Filesystem caching (first run vs subsequent runs)
3. JIT compilation (benchmarks include warmup)

### Stress Test Memory Issues
If stress test fails with memory issues:
1. Reduce TOTAL_MESSAGES (default: 1000)
2. Reduce BATCH_SIZE (default: 50)
3. Check for memory leaks in custom actors

## Contributing

When adding new features:
1. Add integration tests to `test-integration.ts`
2. Add benchmarks if introducing new actors or patterns
3. Update stress test if changing core message handling
4. Update `docs/performance/benchmarks.md` with results

## See Also

- [Performance Benchmarks](docs/performance/benchmarks.md) - Detailed benchmark analysis
- [Architecture](docs/architecture.md) - System design and patterns
- [API Documentation](docs/api.md) - Actor API reference
