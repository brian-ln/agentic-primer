# Concept Graph Web Server - Test Infrastructure

## What is This?

This directory contains a **complete testing infrastructure** for the Concept Graph Web Server, following the **System Modeling Quad** pattern from spec-driven development.

## The Four Artifacts (System Modeling Quad)

```
1. server.spec.md       ← Human-readable specification
   - API endpoint contracts
   - Fixture tables for test scenarios
   - Data models and behavior

2. server.model.lisp    ← Formal specification in DSL
   - Type definitions
   - Interface contracts
   - Invariants and constraints
   - Machine-verifiable

3. tests/*.test.ts      ← Test suite
   - Generated from spec fixtures
   - API endpoint tests
   - Browser interaction tests
   - Edge case coverage

4. server.ts            ← Implementation
   - Guided by specs
   - @implements annotations
   - Verified by tests
```

## Quick Start

### 1. Run All Tests

```bash
./run-tests.sh
```

**What happens:**
1. Playwright browsers installed (if needed)
2. Server starts on port 3000
3. 43+ tests execute (API + Browser)
4. HTML report generated
5. Pass/fail summary displayed

### 2. View Test Report

```bash
bunx playwright show-report
```

Opens an interactive HTML report showing:
- Test execution timeline
- Screenshots of failures
- Network activity
- Console logs

### 3. Run Specific Tests

```bash
# API tests only (20+ tests)
bunx playwright test tests/api.test.ts

# Browser tests only (15+ tests)
bunx playwright test tests/browser.test.ts

# Single test by name
bunx playwright test -g "should load page"
```

## What Gets Tested?

### API Endpoints (20+ tests)

| Endpoint | Tests | Status |
|----------|-------|--------|
| GET /api/concepts | 2 tests | ✅ Automated |
| GET /api/relationships | 2 tests | ✅ Automated |
| GET /api/stats | 2 tests | ✅ Automated |
| GET /api/concept/:id | 4 tests | ✅ Automated |
| GET /api/search | 7 tests | ✅ Automated |
| GET / | 1 test | ✅ Automated |
| Edge Cases | 3 tests | ✅ Automated |

**Coverage:** 100% of API endpoints

### Browser Interactions (15+ tests)

| Feature | Tests | Status |
|---------|-------|--------|
| Page Load & Render | 3 tests | ✅ Automated |
| Node Selection | 3 tests | ✅ Automated |
| Detail Panel | 2 tests | ✅ Automated |
| Navigation | 1 test | ✅ Automated |
| Search & Filter | 2 tests | ✅ Automated |
| Graph Interactions | 2 tests | ✅ Automated |
| Responsive Behavior | 1 test | ✅ Automated |
| Error Detection | 1 test | ✅ Automated |

**Coverage:** 100% of core user flows

### Edge Cases (8+ tests)

- Concurrent requests
- Special characters in queries
- Long search strings
- Invalid API routes
- Case-insensitive search
- Empty search results

## Documentation

| File | Purpose |
|------|---------|
| **[TESTING.md](./TESTING.md)** | Complete testing guide (how to run, debug, extend) |
| **[TEST_SCENARIOS.md](./TEST_SCENARIOS.md)** | 36 detailed test scenarios with fixtures |
| **[server.spec.md](./server.spec.md)** | Formal specification with API contracts |
| **[server.model.lisp](./server.model.lisp)** | Formal model with types and invariants |
| **playwright.config.ts** | Playwright configuration |
| **run-tests.sh** | Test runner script |

## Test Structure

```
CONCEPT_GRAPH/
├── server.ts                  # Implementation
├── server.spec.md             # Specification
├── server.model.lisp          # Formal model
├── TEST_SCENARIOS.md          # Scenario documentation
├── TESTING.md                 # Testing guide
├── TEST_README.md             # This file
├── playwright.config.ts       # Test configuration
├── run-tests.sh              # Test runner
└── tests/
    ├── api.test.ts           # API endpoint tests
    └── browser.test.ts       # Browser interaction tests
```

## Example Test Output

```
Running 43 tests using 1 worker

  ✓ api.test.ts (20 tests)
    ✓ GET /api/concepts - should return all 50 concepts
    ✓ GET /api/concepts - should return concepts with valid structure
    ✓ GET /api/relationships - should return all 61 relationships
    ✓ GET /api/search - should search by keyword
    ✓ GET /api/search - should be case-insensitive
    ... (15 more API tests)

  ✓ browser.test.ts (15 tests)
    ✓ should load page and render graph
    ✓ should render SVG graph with nodes and edges
    ✓ should show detail panel when clicking a node
    ✓ should filter nodes when typing in search box
    ... (11 more browser tests)

  43 passed (12.3s)
```

## Common Commands

```bash
# Run all tests
./run-tests.sh

# Run with UI (interactive)
bunx playwright test --ui

# Run in debug mode
bunx playwright test --debug

# Run specific test
bunx playwright test -g "should load page"

# Run in headed mode (see browser)
bunx playwright test --headed

# Generate report
bunx playwright show-report
```

## Test Philosophy

This test infrastructure follows **spec-driven development** principles:

1. **Specification First:** Tests derive from `server.spec.md` fixture tables
2. **Formal Verification:** `server.model.lisp` defines contracts and invariants
3. **Comprehensive Coverage:** API + Browser + Edge Cases
4. **Automated Execution:** No manual intervention required
5. **Self-Documenting:** Test names read like specifications

## Coverage Summary

| Category | Scenarios | Automated | Coverage |
|----------|-----------|-----------|----------|
| API Endpoints | 13 | 20+ | 100% |
| Browser Interactions | 11 | 15+ | 100% |
| Edge Cases | 12 | 8+ | 67% |
| **TOTAL** | **36** | **43+** | **89%** |

**Note:** Some edge cases (memory leaks, malformed JSON files) require manual testing or additional tooling.

## Performance Metrics

Tests verify these performance targets:

- ✅ API Response Time: < 50ms (in-memory data)
- ✅ Page Load Time: < 2s on localhost
- ✅ Graph Render Time: < 1s for 50 nodes
- ✅ Search Filter Update: < 200ms

## Technology Stack

- **Test Framework:** [Playwright](https://playwright.dev) v1.57+
- **Runtime:** Bun v1.3+
- **Browser:** Chromium (can extend to Firefox, WebKit)
- **Reporter:** HTML (with screenshots and traces)
- **Server:** Bun.serve() HTTP server

## Troubleshooting

### Tests won't start

```bash
# Check server starts manually
PORT=3000 bun run server.ts

# Install Playwright browsers
bunx playwright install chromium
```

### Port conflict

```bash
# Kill process on port 3000
lsof -ti:3000 | xargs kill -9
```

### Tests timeout

```bash
# Increase timeout in playwright.config.ts
# Or check that concepts.json and relationships.json exist
```

See **[TESTING.md](./TESTING.md)** for complete troubleshooting guide.

## Next Steps

### For Developers

1. Read **[server.spec.md](./server.spec.md)** to understand API contracts
2. Review **[TEST_SCENARIOS.md](./TEST_SCENARIOS.md)** for detailed test cases
3. Run `./run-tests.sh` to verify everything works
4. Add new tests when adding features (follow patterns in `tests/`)

### For QA/Testers

1. Review **[TEST_SCENARIOS.md](./TEST_SCENARIOS.md)** for manual test cases
2. Run automated tests: `./run-tests.sh`
3. Report failures with test name and screenshot (in `test-results/`)
4. Use `bunx playwright test --ui` for interactive debugging

### For DevOps/CI

1. Add to CI pipeline:
   ```bash
   bun install
   bunx playwright install --with-deps chromium
   cd CONCEPT_GRAPH && ./run-tests.sh
   ```
2. Upload `playwright-report/` artifact on failure
3. Set timeout to 5 minutes (tests complete in ~15s normally)

## Contributing

When adding features:

1. **Update specification:** Add to `server.spec.md`
2. **Define test scenarios:** Add to `TEST_SCENARIOS.md`
3. **Write tests:** Add to `tests/api.test.ts` or `tests/browser.test.ts`
4. **Verify:** Run `./run-tests.sh` and ensure all pass
5. **Document:** Update relevant documentation

## FAQ

**Q: Do I need to start the server manually?**
A: No, Playwright automatically starts and stops the server.

**Q: Can I use Firefox or Safari?**
A: Yes, uncomment Firefox/WebKit in `playwright.config.ts`.

**Q: How long do tests take?**
A: ~15 seconds for full suite (43+ tests).

**Q: What if a test fails?**
A: Check `test-results/` for screenshots, or run with `--debug` flag.

**Q: Are these tests running against production data?**
A: No, tests use local `concepts.json` and `relationships.json` files.

**Q: Can I run tests in parallel?**
A: Currently sequential (workers: 1) to avoid port conflicts. Can parallelize with different ports.

## Support

- **Documentation Issues:** Check [TESTING.md](./TESTING.md)
- **Test Failures:** Run with `--debug` flag
- **Playwright Issues:** See [Playwright Docs](https://playwright.dev)
- **Bun Issues:** See [Bun Docs](https://bun.sh/docs)

---

**Status:** ✅ Production Ready
**Version:** 1.0
**Test Count:** 43+ automated tests
**Coverage:** 89% of defined scenarios
**Last Updated:** 2026-01-16
