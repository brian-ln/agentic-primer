# Session Knowledge System - Test Guide

**Epic:** `agentic-primer-t49.5`
**Status:** Complete Test Documentation
**Last Updated:** 2026-02-03

---

## Overview

The Session Knowledge System has comprehensive test coverage across multiple test suites:

- **Unit Tests**: Core functionality (graph, entities, context)
- **Integration Tests**: Cognitive features (temporal queries, confidence decay, arcs)
- **E2E Tests**: Full workflows with database operations
- **Manual Tests**: Hybrid approaches and system validation

**Total Test Coverage:**
- 37 cognitive integration tests (392 assertions)
- 14+ additional test files (entity, graph, context tests)
- ~50ms average execution time for integration tests

---

## Test Suites

### 1. Cognitive Integration Tests
**File:** `src/session-knowledge/__tests__/cognitive-integration.test.ts`

Tests all Phase 5 cognitive features:

**ConfidenceDecay (24 tests)**
- Tech domain: Exponential decay, 9-month half-life
- Science domain: Power law decay, 7.5-year half-life
- News domain: Fast exponential decay, 2-month half-life
- Core domain: Stepped decay, maintains stability
- Edge cases: Negative age, invalid confidence, unknown domains
- Time estimation: Calculate time to reach target confidence
- Decay rate: Calculate current rate of decay

**TemporalQueries (9 tests)**
- Query knowledge at specific points in time (bi-temporal semantics)
- Filter by valid_time correctly
- Detect changes between time periods
- Apply confidence decay to results
- Filter by domain
- Sort by confidence

**ArcDetector (4 tests)**
- Detect thinking arcs in real sessions
- Handle sessions with insufficient data
- Detect different arc types (breakthrough, pattern discovery, etc.)
- Validate confidence scores

**End-to-End Integration (1 test)**
- Complete temporal workflow combining all features
- Historical vs current knowledge queries
- Decay calculation verification

### 2. Entity Tests
**Files:** `src/entities/*.test.ts`

Tests graph entities and their behaviors:

- **provider.test.ts**: Provider entity CRUD operations
- **program.test.ts**: Program entity management
- **model.test.ts**: Model entity operations
- **embedding.test.ts**: Embedding entity tests
- **human.test.ts**: Human entity tests
- **agent.test.ts**: Agent entity operations
- **session.test.ts**: Session entity tests
- **task.test.ts**: Task entity management
- **information.test.ts**: Information entity operations

**E2E Tests:**
- **agent-session.e2e.test.ts**: Full agent-session workflows
- **model.e2e.test.ts**: Model integration tests

### 3. Core System Tests
**Files:** `src/*.test.ts`

Tests core graph and context systems:

- **graph.test.ts**: Address class, Node class, Edge class, GraphStore operations, Event sourcing, Query patterns, Database persistence
- **context.test.ts**: Context management and tracking

### 4. Manual/Ad-hoc Tests

**Hybrid Classification Test:**
- **File:** `src/session-knowledge/classification/test_hybrid.ts`
- **Purpose:** Validate in-memory vs database classification paths
- **Usage:** `bun run src/session-knowledge/classification/test_hybrid.ts`

---

## Running Tests

### Quick Start

```bash
# Run all tests
bun test

# Run specific test file
bun test src/session-knowledge/__tests__/cognitive-integration.test.ts

# Run tests matching pattern
bun test --test-name-pattern "ConfidenceDecay"

# Run with coverage
bun test --coverage

# Run only marked tests
bun test --only
```

### Using Test Scripts

```bash
# Run all test suites with reporting
./scripts/test-all.sh

# Watch mode for development
./scripts/test-watch.sh

# Run with verbose output
./scripts/test-all.sh --verbose

# Generate coverage report
./scripts/test-all.sh --coverage
```

### Test Organization

```
simplify/
├── src/
│   ├── session-knowledge/
│   │   ├── __tests__/
│   │   │   └── cognitive-integration.test.ts  # Integration tests
│   │   └── classification/
│   │       └── test_hybrid.ts                 # Manual tests
│   ├── entities/
│   │   ├── *.test.ts                          # Unit tests
│   │   └── *.e2e.test.ts                      # E2E tests
│   ├── graph.test.ts                          # Core tests
│   └── context.test.ts                        # Core tests
└── scripts/
    ├── test-all.sh                            # Test runner
    └── test-watch.sh                          # Watch mode
```

---

## Test Data Setup

### Database Requirements

Tests use the libSQL database at:
```
~/.claude/index/sessions-libsql.db
```

**Required Data:**
- Session metadata (indexed sessions)
- Prototype embeddings (for classification)
- Knowledge items (decisions, learnings, errors)
- Temporal data (valid_time, transaction_time)

### Initial Setup

```bash
# 1. Build index (creates database)
./build-index

# 2. Generate embeddings (for semantic tests)
bun run src/session-knowledge/embeddings/SessionEmbeddingIndexerLibSQL.ts sessions

# 3. Extract knowledge (for cognitive tests)
./know extract all

# 4. Run tests
bun test
```

### Test Fixtures

**Session IDs used in tests:**
- `4af7ce26-80a3-4ea4-b3b4-312c40e39e76` - Real session with knowledge items
- `fake-session-id` - Non-existent session for edge case testing

**Test Queries:**
- "libSQL" - Common search term with results
- "authentication" - Another common term
- "" (empty) - Match all queries

### Cleanup

Test data is persistent and shared across runs. Clean up periodically:

```bash
# Remove test data
rm ~/.claude/index/sessions-libsql.db

# Rebuild from scratch
./build-index
```

---

## Performance Benchmarks

### Expected Performance

**Cognitive Integration Tests:**
- Total execution: ~50ms
- Per test average: ~1.35ms
- Assertions: 392 expect() calls

**Target Metrics:**
- Query at time: <10ms per query
- Change detection: <20ms per range
- Decay calculation: <1ms per item
- Arc detection: <50ms per session

### Performance Assertions

Tests include performance checks:

```typescript
// Confidence decay should be fast
test('should calculate decay quickly', () => {
  const start = performance.now();
  decay.calculateDecay(1.0, 1000, 'tech');
  const elapsed = performance.now() - start;
  expect(elapsed).toBeLessThan(1); // <1ms
});
```

### Monitoring Performance

```bash
# Run tests with timing
bun test --timeout=10000

# Profile slow tests
bun test --test-name-pattern "slow" --timeout=30000
```

---

## CI/CD Integration

### GitHub Actions Workflow

See `.github/workflows/test-know.yml` for CI configuration.

**Workflow:**
1. Setup environment (Bun, dependencies)
2. Initialize test database
3. Run all test suites
4. Generate coverage report
5. Upload artifacts
6. Report results

**Triggers:**
- Push to `main` or `session-knowledge-*` branches
- Pull requests
- Manual dispatch

### Local CI Simulation

```bash
# Simulate CI environment
./scripts/test-all.sh --ci

# This runs:
# - All tests in order
# - Coverage generation
# - Exit code 1 on failure
# - JSON output for parsing
```

### Exit Codes

```
0 - All tests passed
1 - One or more tests failed
2 - Test setup failed
3 - Coverage threshold not met
```

---

## Test Flags and Options

### Bun Test Flags

```bash
# Timeout (default 5000ms)
bun test --timeout=10000

# Update snapshots
bun test --update-snapshots

# Re-run tests multiple times
bun test --rerun-each=5

# Only run marked tests
bun test --only

# Include TODO tests
bun test --todo

# Coverage
bun test --coverage --coverage-reporter=lcov

# Set coverage directory
bun test --coverage --coverage-dir=.coverage

# Bail after N failures
bun test --bail=3

# Filter by test name
bun test --test-name-pattern="ConfidenceDecay"

# Custom reporter
bun test --reporter=junit --reporter-outfile=test-results.xml
```

### Script Flags

```bash
# test-all.sh flags
./scripts/test-all.sh --verbose    # Show detailed output
./scripts/test-all.sh --coverage   # Generate coverage
./scripts/test-all.sh --ci         # CI mode (strict)
./scripts/test-all.sh --fast       # Skip slow tests

# test-watch.sh flags
./scripts/test-watch.sh            # Watch all files
./scripts/test-watch.sh --filter cognitive  # Watch specific tests
```

---

## Coverage Requirements

### Current Coverage

**Cognitive Integration:**
- ConfidenceDecay: 100% (all domains, edge cases)
- TemporalQueries: 90% (core queries, filtering)
- ArcDetector: 85% (detection, types, validation)

**Target Coverage:**
- Unit tests: 80% minimum
- Integration tests: 70% minimum
- E2E tests: 60% minimum

### Coverage Reports

```bash
# Generate coverage
bun test --coverage

# View HTML report
open coverage/index.html

# Check coverage thresholds
bun test --coverage --bail-on-coverage-fail
```

### Coverage Configuration

Add to `package.json`:
```json
{
  "bun": {
    "test": {
      "coverage": {
        "enabled": true,
        "threshold": {
          "line": 80,
          "function": 80,
          "branch": 75
        }
      }
    }
  }
}
```

---

## Troubleshooting

### Common Test Failures

#### 1. Database Connection Errors

**Error:** `Error: unable to open database file`

**Solution:**
```bash
# Ensure database exists
./build-index

# Check permissions
ls -la ~/.claude/index/sessions-libsql.db
chmod 644 ~/.claude/index/sessions-libsql.db
```

#### 2. Missing Test Data

**Error:** `expect(results.length).toBeGreaterThan(0)` fails

**Solution:**
```bash
# Rebuild index with force
./build-index --force

# Extract knowledge
./know extract all

# Re-run tests
bun test
```

#### 3. Timeout Errors

**Error:** `Test timeout after 5000ms`

**Solution:**
```bash
# Increase timeout
bun test --timeout=10000

# Or optimize slow queries
# Check database indexes
```

#### 4. Confidence Decay Precision

**Error:** `expect(0.5123).toBeCloseTo(0.5, 1)` fails

**Solution:**
```typescript
// Use appropriate precision
expect(confidence).toBeCloseTo(0.5, 1); // 1 decimal place
expect(confidence).toBeCloseTo(0.5, 5); // 5 decimal places
```

#### 5. Temporal Query Edge Cases

**Error:** Historical queries return 0 results

**Solution:**
```typescript
// Check date ranges
const date = new Date('2026-02-01'); // Use realistic dates
const results = await temporal.queryAtTime('libSQL', date);

// Verify data exists in that time range
const changes = await temporal.getChangesBetween(
  new Date('2026-01-01'),
  new Date('2026-02-03')
);
```

### Debug Mode

```bash
# Enable debug logging
DEBUG=1 bun test

# Verbose test output
bun test --verbose

# Run single test with console logs
bun test --test-name-pattern "specific test"
```

### Test Isolation

Tests should be isolated but share database state:

```typescript
describe('MyTests', () => {
  let temporal: TemporalQueries;

  beforeAll(() => {
    temporal = new TemporalQueries();
  });

  afterAll(() => {
    temporal.close(); // Clean up connections
  });

  test('isolated test', async () => {
    // Each test is independent
  });
});
```

---

## Writing New Tests

### Test Template

```typescript
import { describe, test, expect, beforeAll, afterAll } from 'bun:test';
import { YourModule } from '../path/to/module';

describe('YourModule', () => {
  let instance: YourModule;

  beforeAll(() => {
    // Setup: runs once before all tests
    instance = new YourModule();
  });

  afterAll(() => {
    // Cleanup: runs once after all tests
    instance.close();
  });

  describe('feature', () => {
    test('should do something', () => {
      // Arrange
      const input = 'test';

      // Act
      const result = instance.method(input);

      // Assert
      expect(result).toBe('expected');
    });

    test('should handle edge cases', () => {
      expect(() => instance.method(null)).toThrow();
    });
  });
});
```

### Async Tests

```typescript
test('should query database', async () => {
  const results = await temporal.queryAtTime('libSQL', new Date());
  expect(results.length).toBeGreaterThan(0);
});
```

### Performance Tests

```typescript
test('should be fast', () => {
  const start = performance.now();
  const result = decay.calculateDecay(1.0, 1000, 'tech');
  const elapsed = performance.now() - start;

  expect(result).toBeGreaterThan(0);
  expect(elapsed).toBeLessThan(5); // <5ms
});
```

### Integration Tests

```typescript
test('should support complete workflow', async () => {
  // 1. Setup
  const temporal = new TemporalQueries();

  try {
    // 2. Execute workflow
    const current = await temporal.queryAtTime('libSQL', new Date());
    const historical = await temporal.queryAtTime('libSQL', new Date('2026-01-15'));
    const withDecay = await temporal.getWithDecay('libSQL');

    // 3. Verify
    expect(current.length).toBeGreaterThan(0);
    expect(historical.length).toBeLessThanOrEqual(current.length);
    expect(withDecay.every(r => r.currentConfidence !== undefined)).toBe(true);
  } finally {
    // 4. Cleanup
    temporal.close();
  }
});
```

---

## Best Practices

### 1. Test Organization

- Group related tests with `describe()`
- Use clear, descriptive test names
- Follow Arrange-Act-Assert pattern
- One assertion per test (when possible)

### 2. Test Data

- Use realistic test data
- Don't hardcode dates (use Date constructors)
- Clean up resources in `afterAll()`
- Share expensive setup in `beforeAll()`

### 3. Assertions

```typescript
// Good
expect(results.length).toBeGreaterThan(0);
expect(results.every(r => r.type === 'decision')).toBe(true);

// Avoid
expect(results.length).toBe(42); // Brittle
expect(results).toMatchSnapshot(); // Fragile
```

### 4. Performance

- Test performance-critical code
- Use realistic data sizes
- Set reasonable thresholds
- Monitor test execution time

### 5. Error Handling

```typescript
// Test error cases
expect(() => decay.calculateDecay(1.5, 1000, 'tech')).toThrow();
expect(() => decay.calculateDecay(-0.1, 1000, 'tech')).toThrow();

// Test edge cases
const confidence = decay.calculateDecay(1.0, -1000, 'tech');
expect(confidence).toBe(1.0); // Handles negative gracefully
```

---

## Resources

### Documentation
- [Bun Test Runner](https://bun.sh/docs/cli/test)
- [Session Knowledge System Spec](../../docs/specifications/SESSION_KNOWLEDGE_SYSTEM.md)
- [Quick Reference](./QUICK-REFERENCE.md)

### Related Files
- Test scripts: `scripts/test-all.sh`, `scripts/test-watch.sh`
- CI workflow: `.github/workflows/test-know.yml`
- Package config: `package.json`

### Getting Help
- Check test output for error messages
- Review troubleshooting section above
- Examine existing tests for patterns
- Run with `--verbose` for detailed output

---

**Last Updated:** 2026-02-03
**Maintained By:** Session Knowledge System Team
**Epic:** `agentic-primer-t49.5`
