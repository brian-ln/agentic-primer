# Concept Graph Web Server - Testing Guide

## Overview

This document describes the testing infrastructure for the Concept Graph Web Server, including how to run tests, understand test organization, and extend the test suite.

## Quick Start

### Run All Playwright Tests (Concept Graph)

```bash
cd CONCEPT_GRAPH
./run-tests.sh
```

This will:
1. Install Playwright browsers (if needed)
2. Start the server on port 3000
3. Run all API and browser tests
4. Generate an HTML report

**Important:** These Playwright tests are separate from the actor system tests.

### Run Actor System Tests (src/)

```bash
# From project root
bun test src/

# This runs all Bun tests for the actor system (82 tests)
```

**Note:** Do NOT run `bun test` without arguments, as it will try to load Playwright test files which use incompatible test framework (@playwright/test vs bun:test).

### Run Specific Test Files

```bash
# API tests only
bunx playwright test tests/api.test.ts

# Browser tests only
bunx playwright test tests/browser.test.ts
```

### Run in UI Mode (Interactive)

```bash
bunx playwright test --ui
```

This opens an interactive test runner where you can:
- See tests execute in real-time
- Inspect DOM at each step
- Debug failing tests
- Re-run specific tests

### View Test Report

```bash
bunx playwright show-report
```

## Test Architecture

### Test Categories

The test suite is organized into three main categories as defined in `TEST_SCENARIOS.md`:

#### 1. API Endpoint Tests (`tests/api.test.ts`)

Tests all REST API endpoints without requiring a browser. These tests use Playwright's `request` fixture to make HTTP calls directly.

**Coverage:**
- ✅ GET /api/concepts - All concepts retrieval
- ✅ GET /api/relationships - All relationships retrieval
- ✅ GET /api/stats - Graph statistics
- ✅ GET /api/concept/:id - Concept details (success and error cases)
- ✅ GET /api/search - Search with keyword, domain, tag filters
- ✅ GET / - HTML application serving
- ✅ Edge cases (concurrent requests, special characters, etc.)

**Total: 20+ API test scenarios**

#### 2. Browser Interaction Tests (`tests/browser.test.ts`)

Tests the frontend graph visualization and user interactions using full browser automation.

**Coverage:**
- ✅ Page load and initial render
- ✅ SVG graph rendering (50 nodes, 61 edges)
- ✅ Node selection and detail panel
- ✅ Visual highlighting (selected nodes, connected edges)
- ✅ Navigation via relationships
- ✅ Search and filtering
- ✅ Detail panel controls (open, close, navigate)
- ✅ Graph interactions (drag nodes, zoom)
- ✅ Responsive behavior (window resize)
- ✅ Console error detection

**Total: 15+ browser test scenarios**

#### 3. Edge Cases and Error Handling

Distributed across both test files:
- Invalid routes and malformed inputs
- Concurrent request handling
- Special characters and long queries
- XSS prevention (HTML escaping)
- Graceful error handling

### Test Organization

```
CONCEPT_GRAPH/
├── tests/
│   ├── api.test.ts          # API endpoint tests
│   └── browser.test.ts      # Browser interaction tests
├── playwright.config.ts     # Playwright configuration
├── run-tests.sh            # Test runner script
├── TEST_SCENARIOS.md       # Comprehensive test scenario documentation
├── server.spec.md          # Formal specification
└── server.model.lisp       # Formal model
```

## Test Infrastructure

### Playwright Configuration

**File:** `playwright.config.ts`

Key settings:
- **Test Directory:** `./tests`
- **Timeout:** 30 seconds per test
- **Workers:** 1 (sequential execution to avoid port conflicts)
- **Browsers:** Chromium (can enable Firefox, WebKit)
- **Base URL:** `http://localhost:3000`
- **Web Server:** Auto-starts server on port 3000 before tests
- **Reports:** HTML report generated in `playwright-report/`
- **Screenshots:** Captured on test failure
- **Traces:** Captured on retry

### Server Lifecycle Management

The test infrastructure automatically manages the server lifecycle:

1. **Startup:** Playwright starts the server using `PORT=3000 bun run CONCEPT_GRAPH/server.ts`
2. **Health Check:** Waits for port 3000 to be available (10s timeout)
3. **Test Execution:** All tests run against the server
4. **Shutdown:** Server automatically stops after tests complete

You can reuse an existing server instance during development by setting:
```bash
REUSE_SERVER=1 ./run-tests.sh
```

## Writing New Tests

### API Test Pattern

```typescript
test('should do something with API', async ({ request }) => {
  const response = await request.get('http://localhost:3000/api/endpoint');

  expect(response.status()).toBe(200);
  const data = await response.json();
  expect(data).toHaveProperty('field');
});
```

### Browser Test Pattern

```typescript
test('should interact with browser', async ({ page }) => {
  await page.goto('/');

  // Wait for element
  await page.waitForSelector('.some-element');

  // Interact
  await page.click('.button');

  // Assert
  const text = await page.locator('.result').textContent();
  expect(text).toBe('Expected Value');
});
```

### Best Practices

1. **Wait for D3 simulation stability:** D3 force simulation animates nodes, which can make them unstable for clicking
   ```typescript
   // Helper function to wait for simulation to stabilize
   async function waitForSimulationStable(page: any, timeout = 10000) {
     await page.waitForFunction(
       () => document.body.getAttribute('data-simulation-stable') === 'true',
       { timeout }
     );
   }

   // Use in tests that interact with nodes
   await page.waitForSelector('.node', { timeout: 5000 });
   await waitForSimulationStable(page);
   await page.locator('.node').first().click({ force: true });
   ```

2. **Use force clicks for animated elements:** When elements are continuously animated by D3, use `{ force: true }`
   ```typescript
   // Bypass actionability checks for D3-animated nodes
   await page.locator('.node').first().click({ force: true });
   ```

3. **Use waitFor selectors:** Always wait for elements before interacting
   ```typescript
   await page.waitForSelector('.node', { timeout: 5000 });
   ```

2. **Avoid hardcoded waits:** Use `waitForFunction` instead of `waitForTimeout`
   ```typescript
   // Bad
   await page.waitForTimeout(1000);

   // Good
   await page.waitForFunction(() => document.querySelector('.loaded'));
   ```

3. **Use data-testid attributes:** Add test-specific selectors to HTML
   ```html
   <button data-testid="clear-search">Clear</button>
   ```
   ```typescript
   await page.click('[data-testid="clear-search"]');
   ```

4. **Check for console errors:** Wrap tests to detect JavaScript errors
   ```typescript
   const errors: string[] = [];
   page.on('console', (msg) => {
     if (msg.type() === 'error') errors.push(msg.text());
   });
   // ... test code ...
   expect(errors).toEqual([]);
   ```

5. **Use descriptive test names:** Tests should read like specifications
   ```typescript
   test('should display concept details when node is clicked', async ({ page }) => {
     // ...
   });
   ```

## Debugging Failed Tests

### 1. Run in Debug Mode

```bash
bunx playwright test --debug
```

This opens Playwright Inspector where you can:
- Step through test execution
- Inspect the page at any step
- See network requests
- View console logs

### 2. View Screenshots

On test failure, screenshots are automatically saved to `test-results/`:
```bash
ls test-results/
# browser-Browser-Interaction-Tests-should-load-page-chromium/test-failed-1.png
```

### 3. View Traces

```bash
bunx playwright show-trace test-results/trace.zip
```

This shows a timeline of test execution with screenshots, network, console.

### 4. Run Single Test

```bash
bunx playwright test -g "should load page and render graph"
```

### 5. Headed Mode (See Browser)

```bash
bunx playwright test --headed
```

Watch the browser execute tests in real-time.

## Continuous Integration

### GitHub Actions Example

```yaml
name: Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: oven-sh/setup-bun@v1
      - run: bun install
      - run: bunx playwright install --with-deps chromium
      - run: cd CONCEPT_GRAPH && ./run-tests.sh
      - uses: actions/upload-artifact@v3
        if: failure()
        with:
          name: playwright-report
          path: playwright-report/
```

## Test Coverage Goals

### Current Coverage (v1.0)

| Category | Scenarios Defined | Automated | Coverage |
|----------|------------------|-----------|----------|
| API Endpoints | 13 | 20+ | 100% |
| Browser Interactions | 11 | 15+ | 100% |
| Edge Cases | 12 | 8+ | 67% |
| **TOTAL** | **36** | **43+** | **89%** |

### Remaining Manual Tests

Some scenarios in `TEST_SCENARIOS.md` require manual testing:

1. **Memory Leak Detection** (Scenario 3.10): Requires memory profiling tools
2. **Malformed JSON Files** (Scenario 3.6): Requires file system manipulation outside test environment
3. **Large Dataset Scalability** (Scenario 3.11): Requires test fixture generation
4. **XSS Attack Prevention** (Scenario 3.9): Partially covered, needs security audit

## Performance Benchmarks

### Target Metrics (from server.spec.md)

- API Response Time: < 50ms
- Page Load Time: < 2s
- Graph Render Time: < 1s (50 nodes)
- Search Filter Update: < 200ms

### Running Performance Tests

```bash
# API performance
bunx playwright test tests/api.test.ts --grep "concurrent"

# Browser performance
bunx playwright test tests/browser.test.ts --grep "load"
```

## Test Data

### Current Test Data

- **concepts.json:** 50 concepts across 15 domains
- **relationships.json:** 61 relationships between concepts
  - **Note:** The frontend filters to 59 valid relationships (2 relationships point to non-existent concepts: `referential-transparency` and `replication`)
  - API endpoints return all 61 relationships from JSON
  - Browser tests verify 59 relationships are rendered in the graph

### Test Fixtures (Future)

For isolation and edge case testing, consider creating fixture files:

```
CONCEPT_GRAPH/
└── tests/
    └── fixtures/
        ├── concepts-minimal.json      # 5 concepts for fast tests
        ├── concepts-large.json        # 500 concepts for performance tests
        ├── concepts-malformed.json    # Invalid JSON for error tests
        └── relationships-invalid.json # Relationships with bad IDs
```

To use fixtures, modify server startup in `playwright.config.ts`:
```typescript
webServer: {
  command: 'CONCEPTS_FILE=tests/fixtures/concepts-minimal.json bun run server.ts',
  // ...
}
```

## Troubleshooting

### Tests Fail with "Port Already in Use"

**Problem:** Server on port 3000 already running

**Solution:**
```bash
# Find and kill process on port 3000
lsof -ti:3000 | xargs kill -9

# Or use different port
PORT=3001 ./run-tests.sh
```

### Tests Timeout Waiting for Server

**Problem:** Server not starting or port check failing

**Solution:**
1. Check server starts manually: `PORT=3000 bun run server.ts`
2. Verify `concepts.json` and `relationships.json` exist
3. Increase timeout in `playwright.config.ts`:
   ```typescript
   webServer: {
     timeout: 20 * 1000, // Increase to 20s
   }
   ```

### Browser Tests Fail but API Tests Pass

**Problem:** Frontend JavaScript errors

**Solution:**
1. Check browser console errors in test output
2. Run server manually and open browser DevTools
3. Verify D3.js CDN is accessible
4. Check for CORS issues

### Node Click Tests Timeout or Fail

**Problem:** D3 force simulation makes nodes unstable or places them outside viewport

**Solution:**
1. **Simulation stability:** The app exposes `data-simulation-stable` attribute when the force simulation settles. Tests should wait for this:
   ```typescript
   await waitForSimulationStable(page);
   ```

2. **Viewport constraints:** The app constrains nodes within viewport bounds using:
   - Force positioning: `forceX()` and `forceY()` pull nodes toward center
   - Bounding box: Tick handler clamps node positions within margins

3. **Force clicks:** Use `{ force: true }` to bypass Playwright's actionability checks:
   ```typescript
   await page.locator('.node').first().click({ force: true });
   ```

### Tests Pass Locally but Fail in CI

**Problem:** Environment differences

**Solution:**
1. Ensure Playwright browsers installed: `bunx playwright install --with-deps`
2. Check CI has sufficient memory (D3.js rendering is heavy)
3. Use `--headed` mode locally to see visual differences
4. Verify same Bun version in CI

## Related Documentation

- **[TEST_SCENARIOS.md](./TEST_SCENARIOS.md)** - Comprehensive test scenario documentation with 36 defined test cases
- **[server.spec.md](./server.spec.md)** - Formal specification with API contracts and fixture tables
- **[server.model.lisp](./server.model.lisp)** - Formal model with type definitions and invariants
- **[README_WEBAPP.md](./README_WEBAPP.md)** - Application user guide
- **[Playwright Docs](https://playwright.dev)** - Official Playwright documentation

## Contributing

When adding new features:

1. **Write specification first:** Update `server.spec.md` with new API contracts
2. **Define test scenarios:** Add to `TEST_SCENARIOS.md`
3. **Implement tests:** Create tests in `tests/` directory
4. **Run tests:** Ensure all tests pass with `./run-tests.sh`
5. **Update this guide:** Document new test patterns or edge cases

## Test Execution Checklist

Before committing code:

- [ ] All API tests pass
- [ ] All browser tests pass
- [ ] No console errors in browser tests
- [ ] New features have corresponding tests
- [ ] Test coverage remains above 85%
- [ ] Performance benchmarks met
- [ ] Documentation updated

---

**Version:** 1.0
**Last Updated:** 2026-01-16
**Author:** Background Agent (Automated Test Infrastructure)
