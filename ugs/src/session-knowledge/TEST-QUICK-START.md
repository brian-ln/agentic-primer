# Session Knowledge System - Test Quick Start

**Epic:** `agentic-primer-t49.5`
**For:** Developers running tests quickly

---

## TL;DR

```bash
# Run all tests
bun test

# Run with coverage
./scripts/test-all.sh --coverage

# Watch mode
./scripts/test-watch.sh

# Fast mode (skip slow tests)
./scripts/test-all.sh --fast

# Run specific test
bun test src/session-knowledge/__tests__/cognitive-integration.test.ts
```

---

## Common Commands

### Basic Testing

```bash
# All tests
bun test

# Specific file
bun test src/graph.test.ts

# Filter by name
bun test --test-name-pattern "ConfidenceDecay"

# With timeout
bun test --timeout=10000

# With coverage
bun test --coverage
```

### Using Scripts

```bash
# Run all suites with reporting
./scripts/test-all.sh

# Verbose output
./scripts/test-all.sh --verbose

# Fast mode (skip E2E)
./scripts/test-all.sh --fast

# CI mode
./scripts/test-all.sh --ci

# Watch mode
./scripts/test-watch.sh

# Watch specific tests
./scripts/test-watch.sh --filter cognitive
```

---

## Test Files

```
src/
├── session-knowledge/
│   └── __tests__/
│       └── cognitive-integration.test.ts  # 37 tests, 392 assertions
├── entities/
│   ├── *.test.ts                          # Unit tests
│   └── *.e2e.test.ts                      # E2E tests
├── graph.test.ts                          # Graph system tests
└── context.test.ts                        # Context tests
```

---

## Expected Results

**Cognitive Integration Tests:**
```
37 pass
0 fail
392 expect() calls
~50ms execution
```

**All Tests:**
```
Multiple suites
100+ tests total
<5 seconds execution
```

---

## Troubleshooting

### Database Missing

```bash
# Error: unable to open database file
./build-index
```

### Tests Fail

```bash
# Rebuild from scratch
rm ~/.claude/index/sessions-libsql.db
./build-index
./know extract all
bun test
```

### Timeout

```bash
# Increase timeout
bun test --timeout=20000
```

---

## CI Integration

The `.github/workflows/test-know.yml` workflow runs:

1. All test suites
2. Coverage generation
3. Type checking (lint)
4. Security scanning
5. Performance validation

**Triggers:**
- Push to `main` or `session-knowledge-*`
- Pull requests
- Manual dispatch

---

## More Info

See `TESTING.md` for comprehensive documentation.
