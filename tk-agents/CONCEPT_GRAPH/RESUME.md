# CONCEPT_GRAPH - Session Resume Context

## Project Overview

**CONCEPT_GRAPH** is a graph visualization web application with:
- Backend: `server.ts` (Bun.serve)
- Frontend: Browser-based graph visualization
- Tests: Playwright tests in `tests/` directory

## Current Status

### Issue: Playwright Tests Not Running
- **Problem**: `bun test` picks up Playwright tests but can't run them properly
- **Error**: "Playwright Test did not expect test.describe() to be called here"
- **Root Cause**:
  - No `playwright.config.ts` configuration
  - Wrong test runner (need `playwright test` not `bun test`)
  - Server needs to run on random port during tests

### Test Files
- `tests/api.test.ts` - API endpoint tests (expects server at localhost:3000)
- `tests/browser.test.ts` - Browser interaction tests

### What Needs to Be Done

**Phase 1: Create Playwright Configuration**
1. Create `playwright.config.ts` with:
   - webServer config to auto-start server
   - Random port allocation (use PORT env var)
   - Test directory: `tests/`
   - Exclude from `bun test` (only run with `playwright test`)

**Phase 2: Update Server**
2. Modify `server.ts` to:
   - Read PORT from environment variable
   - Default to 3000 if not set
   - Support random port allocation

**Phase 3: Update Tests**
3. Update test files to:
   - Get server URL from Playwright's webServer config
   - Use dynamic port instead of hardcoded localhost:3000
   - Fix any test code issues

**Phase 4: Verify**
4. Run tests and confirm:
   - `bunx playwright test` works
   - All tests pass
   - `bun test` doesn't pick up Playwright tests (only runs actor tests in ../src)

**Phase 5: Document**
5. Create `TESTING.md` explaining:
   - How to run Playwright tests
   - How server startup works
   - Test structure and patterns

## Commands

```bash
# Run Playwright tests (once configured)
bunx playwright test

# Run actor tests (in parent directory)
cd .. && bun test src/actors/*.test.ts

# Start server manually
bun server.ts
# Or with custom port:
PORT=4000 bun server.ts
```

## Related Files

**Project Root Files:**
- `../src/actors/` - Actor system (separate from CONCEPT_GRAPH)
- `../ACTOR_INTERFACE.md` - Actor interface documentation
- `../examples/` - Actor examples

**CONCEPT_GRAPH Files:**
- `server.ts` - Web server
- `tests/api.test.ts` - API tests
- `tests/browser.test.ts` - Browser tests
- `playwright.config.ts` - **NEEDS TO BE CREATED**
- `TESTING.md` - **NEEDS TO BE CREATED**

## Background Agent

An agent (a35bcf5) is currently working on this in the parent session. If you want to work on this independently:
1. Follow the phases above
2. Check agent's progress: `/private/tmp/claude/-Users-bln-play-projects-proj-20260113-150839-agentic-primer-tk-agents/tasks/a35bcf5.output`
3. If agent completes first, you can review/test the implementation

## Key Decision: Random Port Handling

The challenge is ensuring tests can communicate with the server on whatever port it starts on:

**Solution**: Playwright's `webServer` config:
```typescript
// playwright.config.ts
export default defineConfig({
  webServer: {
    command: 'PORT=0 bun server.ts',  // Random port
    url: 'http://localhost:3000',     // Wait for server ready
    reuseExistingServer: !process.env.CI,
  },
});
```

Playwright will:
1. Start server with PORT=0 (random port)
2. Capture the actual port used
3. Make it available to tests via `baseURL`

## Next Steps

1. Create `playwright.config.ts`
2. Update `server.ts` for PORT env var
3. Update tests to use `baseURL` from config
4. Test with `bunx playwright test`
5. Document in TESTING.md
