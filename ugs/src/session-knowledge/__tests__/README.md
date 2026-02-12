# Session Knowledge System - Test Suite

## Overview

Comprehensive test suite for the Session Knowledge System (know tool) covering real-world scenarios, edge cases, and performance benchmarks.

## Test Files

### migrations.test.ts (NEW)

**Epic**: agentic-primer-t49.3
**Tests**: 33 tests, 73+ assertions
**Purpose**: Database migration safety testing

Comprehensive migration safety tests covering:

1. **Version Detection (4 tests)**
   - Prevents re-running migrations
   - Tracks migration versions
   - Stores migration timestamps

2. **Schema Changes (5 tests)**
   - Safe column addition
   - Preserves existing columns
   - Validates all knowledge tables

3. **Data Backfill (4 tests)**
   - Backfills temporal columns
   - Preserves existing data
   - Intelligent defaults

4. **Index Creation (3 tests)**
   - Creates performance indexes
   - Validates query performance
   - Prevents degradation

5. **New Tables (3 tests)**
   - Creates knowledge_relationships
   - Creates thinking_arcs
   - Enforces constraints

6. **Rollback Capability (2 tests)**
   - Backup/restore workflow
   - Data preservation

7. **Idempotency (3 tests)**
   - Safe re-runs
   - IF NOT EXISTS usage
   - Duplicate handling

8. **Concurrent Access (3 tests)**
   - Reads during migration
   - Writes after migration
   - ACID properties

9. **Data Integrity (3 tests)**
   - No data loss
   - Foreign keys preserved
   - Integrity checks

10. **Error Handling (3 tests)**
    - Empty database
    - No data to backfill
    - Database locking

**Quick Start**: See `MIGRATION-QUICK-START.md` for developer guide
**Detailed Docs**: See `MIGRATION-TEST-SUMMARY.md` for full analysis

### scenarios.test.ts

Real-world scenario and performance tests covering:

1. **Large Sessions (1000+ messages)**
   - Extraction performance validation
   - Memory usage monitoring
   - Scalability testing

2. **Missing Data & Graceful Degradation**
   - Sessions without embeddings
   - Partial data handling
   - Empty session handling
   - Non-existent session handling

3. **Corrupted Data & Error Handling**
   - Invalid timestamps
   - NULL values in required fields
   - Duplicate ID handling
   - Extremely long content

4. **Concurrent Queries & Database Locking**
   - Concurrent read operations
   - Concurrent write operations
   - Read during write scenarios

5. **Empty Results & Edge Cases**
   - Searches with no matches
   - Temporal queries with no results
   - Empty session lists
   - Boundary timestamp queries

6. **Performance Benchmarks**
   - Search query performance (<100ms)
   - Temporal query performance (<200ms)
   - Arc detection performance (<500ms)
   - Confidence decay calculations
   - Batch insert performance
   - Complex query with joins

7. **Memory & Resource Tests**
   - Large result set handling
   - Pagination performance
   - Memory usage monitoring

## Performance Assertions

### Critical Path Operations

- **Extraction**: <30s per 1000 messages
- **Search**: <100ms per query
- **Temporal queries**: <200ms
- **Arc detection**: <500ms per session
- **Confidence decay**: <1ms per calculation
- **Batch inserts**: <1s per 100 records
- **Complex joins**: <100ms

## Running Tests

### Run all tests
```bash
cd /Users/bln/play/agentic-primer/simplify
bun test src/session-knowledge/__tests__/scenarios.test.ts
```

### Run specific test suite
```bash
bun test src/session-knowledge/__tests__/scenarios.test.ts -t "Large Sessions"
bun test src/session-knowledge/__tests__/scenarios.test.ts -t "Performance Benchmarks"
```

### Run with verbose output
```bash
bun test src/session-knowledge/__tests__/scenarios.test.ts --verbose
```

## Test Data

Tests use a dedicated test database at:
```
~/.claude/index/test-scenarios/sessions-libsql.db
```

The test database is:
- Created automatically before tests
- Cleaned between test suites
- Removed after all tests complete

## Test Structure

### TestDataGenerator Utility

Helper class for generating test data:

- `generateLargeSession(count)` - Creates sessions with N messages
- `generateMessagesWithoutEmbeddings(count)` - Creates messages missing embeddings
- `generateCorruptedData()` - Creates various corrupted data scenarios

### Test Organization

Tests are organized by scenario:

1. **Setup/Teardown**
   - `beforeAll`: Initialize database schema
   - `afterAll`: Cleanup test data
   - `beforeEach`: Clean tables between tests

2. **Test Cases**
   - Each test is self-contained
   - Clear naming: `Scenario X.Y: Description`
   - Performance metrics logged to console

3. **Assertions**
   - Functional correctness
   - Performance thresholds
   - Data integrity

## Coverage

### Functional Coverage

- ✅ Knowledge extraction pipeline
- ✅ Embedding generation
- ✅ Temporal queries
- ✅ Arc detection
- ✅ Confidence decay
- ✅ Database operations
- ✅ Error handling
- ✅ Data validation

### Edge Case Coverage

- ✅ Empty/missing data
- ✅ Invalid data types
- ✅ Boundary values
- ✅ Concurrent access
- ✅ Large datasets
- ✅ Duplicate entries
- ✅ NULL values

### Performance Coverage

- ✅ Query response times
- ✅ Extraction throughput
- ✅ Memory usage
- ✅ Batch operations
- ✅ Concurrent operations
- ✅ Complex queries

## Test Metrics

### Expected Results

When all tests pass, you should see:

```
✓ Scenario 1.1: Extract from 1000+ message session within 30s
  Extract time: ~15-25s
  Decisions: 100+
  Learnings: 140+
  Errors: 70+

✓ Benchmark 6.1: Search query performance <100ms
  Text search time: 20-50ms
  Results found: 20+

✓ Benchmark 6.2: Temporal query performance <200ms
  Temporal query time: 50-150ms
  Results found: 30+

✓ Benchmark 6.3: Arc detection performance <500ms
  Arc detection time: 100-300ms
  Arcs detected: 3-8
```

## Troubleshooting

### Test Failures

**Timeout errors:**
- Increase test timeout in test configuration
- Check database connection
- Verify test data generation

**Performance failures:**
- Run tests on unloaded system
- Check disk I/O performance
- Verify no other processes using database

**Database errors:**
- Ensure test database directory exists
- Check file permissions
- Verify libSQL client installation

### Common Issues

1. **"Database locked"**
   - Close other connections to test database
   - Check for hanging connections

2. **"Table does not exist"**
   - Schema not initialized in beforeAll
   - Check SQL syntax

3. **"Out of memory"**
   - Reduce test data size
   - Run tests individually

## CI/CD Integration

### GitHub Actions

```yaml
name: Session Knowledge Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: oven-sh/setup-bun@v1
      - run: bun install
      - run: bun test src/session-knowledge/__tests__/scenarios.test.ts
```

## Future Enhancements

### Planned Test Additions

1. **Stress Testing**
   - 10,000+ message sessions
   - Sustained load testing
   - Resource exhaustion scenarios

2. **Integration Testing**
   - End-to-end CLI workflows
   - Multi-session scenarios
   - Cross-system interactions

3. **Regression Testing**
   - Version compatibility
   - Schema migration testing
   - Backward compatibility

4. **Security Testing**
   - SQL injection attempts
   - Path traversal tests
   - Input sanitization

## Contributing

When adding new tests:

1. Follow existing naming conventions
2. Add performance assertions where applicable
3. Document expected behavior
4. Include error scenarios
5. Update this README

## Resources

- [Bun Test Documentation](https://bun.sh/docs/cli/test)
- [libSQL Documentation](https://github.com/tursodatabase/libsql)
- [Session Knowledge System Spec](../../docs/specifications/SESSION_KNOWLEDGE_SYSTEM.md)
