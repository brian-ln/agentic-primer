# Quick Test Reference - scenarios.test.ts

## File Stats
- **Lines:** 1,082
- **Test Suites:** 7
- **Test Cases:** 26
- **File Size:** 34KB

## Run Commands

### All Tests
```bash
bun test src/session-knowledge/__tests__/scenarios.test.ts
```

### By Suite
```bash
# Large sessions
bun test scenarios.test.ts -t "Large Sessions"

# Missing data
bun test scenarios.test.ts -t "Missing Data"

# Corrupted data
bun test scenarios.test.ts -t "Corrupted Data"

# Concurrent queries
bun test scenarios.test.ts -t "Concurrent"

# Empty results
bun test scenarios.test.ts -t "Empty Results"

# Performance
bun test scenarios.test.ts -t "Performance"

# Memory
bun test scenarios.test.ts -t "Memory"
```

### Single Test
```bash
bun test scenarios.test.ts -t "1000+ message"
bun test scenarios.test.ts -t "Search query performance"
```

## Test Suites Overview

### 1. Large Sessions (3 tests)
- Extract 1000+ messages in <30s
- Extract 100 messages benchmark
- Memory usage for large result sets

### 2. Missing Data (4 tests)
- No embeddings
- Partial embeddings
- Empty sessions
- Non-existent sessions

### 3. Corrupted Data (4 tests)
- Invalid timestamps
- NULL values
- Duplicate IDs
- Long content

### 4. Concurrent Queries (3 tests)
- Concurrent reads
- Concurrent writes
- Read during write

### 5. Empty Results (4 tests)
- No search matches
- No temporal results
- Empty session list
- Boundary timestamps

### 6. Performance (6 tests)
- Search: <100ms
- Temporal: <200ms
- Arc detection: <500ms
- Decay calculation: <1ms
- Batch insert: <1s
- Complex joins: <100ms

### 7. Memory (2 tests)
- Large result sets (1000+ records)
- Pagination performance

## Performance Targets

| Operation | Target | Test |
|-----------|--------|------|
| Extract 1000 msgs | <30s | 1.1 |
| Extract 100 msgs | <5s | 1.2 |
| Search query | <100ms | 6.1 |
| Temporal query | <200ms | 6.2 |
| Arc detection | <500ms | 6.3 |
| Decay calc | <1ms | 6.4 |
| Batch 100 | <1s | 6.5 |
| Complex join | <100ms | 6.6 |
| Large result | <500ms | 7.1 |
| Pagination | <100ms | 7.2 |

## Common Test Patterns

### Setup/Teardown
```typescript
beforeAll(async () => {
  // Initialize test database
  db = createClient({ url: `file:${TEST_DB_PATH}` });
  // Create schema
  // Initialize extractors
});

afterAll(async () => {
  // Cleanup
  await extractor.close();
  db.close();
  // Remove test database
});

beforeEach(async () => {
  // Clean tables
  await db.execute('DELETE FROM sessions');
});
```

### Performance Measurement
```typescript
const start = Date.now();
const result = await operation();
const time = Date.now() - start;

console.log(`Operation time: ${time}ms`);
expect(time).toBeLessThan(threshold);
```

### Data Generation
```typescript
const messages = TestDataGenerator.generateLargeSession(1000);
const corrupted = TestDataGenerator.generateCorruptedData();
```

## Test Database

**Location:** `~/.claude/index/test-scenarios/sessions-libsql.db`

**Tables:**
- sessions
- message_embeddings
- session_decisions
- session_learnings
- session_errors
- session_workflows
- thinking_arcs

**Cleanup:** Automatic (removed after tests)

## Debugging Tests

### Verbose Output
```bash
bun test scenarios.test.ts --verbose
```

### Single Test Debug
```bash
bun test scenarios.test.ts -t "specific test name" --verbose
```

### Check Test Database
```bash
sqlite3 ~/.claude/index/test-scenarios/sessions-libsql.db
sqlite> .tables
sqlite> SELECT COUNT(*) FROM sessions;
```

## Common Issues

### "Database locked"
```bash
# Close other connections
rm ~/.claude/index/test-scenarios/sessions-libsql.db
# Re-run tests
```

### "Table does not exist"
- Check beforeAll runs
- Verify schema creation

### "Timeout"
- Increase test timeout
- Check system resources
- Run fewer tests concurrently

### "Out of memory"
- Reduce test data size
- Run tests individually

## Test Data Scale

| Scale | Messages | Time |
|-------|----------|------|
| Small | 10-100 | <2s |
| Medium | 100-500 | <10s |
| Large | 1000+ | <30s |
| Stress | 10000+ | TBD |

## Expected Results

✅ All 26 tests pass
✅ Performance targets met
✅ No memory leaks
✅ Clean database state
✅ No errors/warnings

## Quick Verification

```bash
# Run all tests
bun test scenarios.test.ts

# Should see:
# ✓ Scenario 1.1: Extract from 1000+ message session within 30s
# ✓ Scenario 1.2: Extract from 100 message session
# ...
# ✓ Benchmark 6.6: Complex query with joins
# Total: 26 tests passed
```

## Test Coverage

- ✅ Extraction pipeline
- ✅ Embedding generation
- ✅ Temporal queries
- ✅ Arc detection
- ✅ Confidence decay
- ✅ Database operations
- ✅ Error handling
- ✅ Concurrent access
- ✅ Edge cases
- ✅ Performance
- ✅ Memory usage

## Files

- `scenarios.test.ts` - Main test file (1082 lines)
- `README.md` - Complete documentation
- `QUICK_TEST_REFERENCE.md` - This file

## Next Steps

1. Run tests: `bun test scenarios.test.ts`
2. Review results
3. Fix any failures
4. Add to CI/CD
5. Monitor performance
