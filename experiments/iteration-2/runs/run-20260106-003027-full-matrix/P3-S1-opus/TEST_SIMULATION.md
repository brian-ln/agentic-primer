# Test Issue Simulation

This document demonstrates the system processing a test issue end-to-end.

## Test Issue

### Input (via task.yml form)

```yaml
title: "[TASK] Add health check endpoint"
labels: ["copilot"]
assignees: ["copilot"]

task-type: feature

description: |
  Add a /health endpoint that returns JSON with service status.
  Should include uptime and version information.
  Use the existing Express router pattern.

acceptance-criteria: |
  - [ ] GET /health returns 200 OK
  - [ ] Response includes { status, uptime, version }
  - [ ] Endpoint has no authentication requirement
  - [ ] Tests verify the endpoint behavior

context: |
  - Express.js application in src/app.js
  - Node.js 20+
  - Follow existing route patterns in src/routes/

priority: medium

requirements:
  - [x] This task can be completed without human input during implementation
  - [x] Acceptance criteria are testable and verifiable
  - [x] No sensitive data or credentials are required
```

## Simulation Execution

```
================================================================================
                    @COPILOT SIMULATION - Issue #42
================================================================================

[2026-01-06T00:35:12Z] TRIGGER: Issue #42 created with label 'copilot'
[2026-01-06T00:35:12Z] ASSIGN: @copilot assigned to issue #42

--------------------------------------------------------------------------------
                              PARSING TASK
--------------------------------------------------------------------------------

[2026-01-06T00:35:13Z] READ: Fetching issue #42 details...
[2026-01-06T00:35:13Z] PARSE: Extracting structured fields from task.yml form

  Task Type:     feature
  Priority:      medium

  Description:
    Add a /health endpoint that returns JSON with service status.
    Should include uptime and version information.
    Use the existing Express router pattern.

  Acceptance Criteria:
    1. GET /health returns 200 OK
    2. Response includes { status, uptime, version }
    3. Endpoint has no authentication requirement
    4. Tests verify the endpoint behavior

  Context:
    - Express.js application in src/app.js
    - Node.js 20+
    - Follow existing route patterns in src/routes/

[2026-01-06T00:35:14Z] VALIDATE: All required fields present
[2026-01-06T00:35:14Z] VALIDATE: Requirements checklist complete

--------------------------------------------------------------------------------
                              PLANNING
--------------------------------------------------------------------------------

[2026-01-06T00:35:15Z] ANALYZE: Reading existing codebase patterns...
[2026-01-06T00:35:16Z] PLAN: Implementation steps:
  1. Create src/routes/health.js with health check logic
  2. Register route in src/app.js
  3. Create tests/health.test.js
  4. Verify all acceptance criteria

--------------------------------------------------------------------------------
                              IMPLEMENTING
--------------------------------------------------------------------------------

[2026-01-06T00:35:17Z] GIT: Creating branch 'copilot/issue-42-health-check'
[2026-01-06T00:35:17Z] GIT: Branch created from main

[2026-01-06T00:35:18Z] FILE: Creating src/routes/health.js
--------------------------------------------------------------------------------
const express = require('express');
const router = express.Router();
const { version } = require('../../package.json');

const startTime = Date.now();

router.get('/health', (req, res) => {
  const uptime = Math.floor((Date.now() - startTime) / 1000);

  res.status(200).json({
    status: 'healthy',
    uptime: uptime,
    version: version
  });
});

module.exports = router;
--------------------------------------------------------------------------------

[2026-01-06T00:35:19Z] FILE: Updating src/app.js (adding route import)
--------------------------------------------------------------------------------
+ const healthRouter = require('./routes/health');
  ...
+ app.use(healthRouter);
--------------------------------------------------------------------------------

[2026-01-06T00:35:20Z] FILE: Creating tests/health.test.js
--------------------------------------------------------------------------------
const request = require('supertest');
const app = require('../src/app');

describe('Health Check Endpoint', () => {
  test('GET /health returns 200 OK', async () => {
    const response = await request(app).get('/health');
    expect(response.status).toBe(200);
  });

  test('Response includes status, uptime, and version', async () => {
    const response = await request(app).get('/health');
    expect(response.body).toHaveProperty('status');
    expect(response.body).toHaveProperty('uptime');
    expect(response.body).toHaveProperty('version');
  });

  test('Status is healthy', async () => {
    const response = await request(app).get('/health');
    expect(response.body.status).toBe('healthy');
  });

  test('No authentication required', async () => {
    const response = await request(app).get('/health');
    expect(response.status).toBe(200);
    // No auth headers sent, still succeeds
  });
});
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
                              TESTING
--------------------------------------------------------------------------------

[2026-01-06T00:35:21Z] TEST: Running test suite...

  PASS  tests/health.test.js
    Health Check Endpoint
      ✓ GET /health returns 200 OK (12 ms)
      ✓ Response includes status, uptime, and version (8 ms)
      ✓ Status is healthy (6 ms)
      ✓ No authentication required (5 ms)

  Test Suites: 1 passed, 1 total
  Tests:       4 passed, 4 total
  Time:        0.847 s

[2026-01-06T00:35:22Z] TEST: All tests passed

--------------------------------------------------------------------------------
                              VERIFICATION
--------------------------------------------------------------------------------

[2026-01-06T00:35:23Z] VERIFY: Checking acceptance criteria...

  [✓] GET /health returns 200 OK
      Evidence: Test 'GET /health returns 200 OK' passed

  [✓] Response includes { status, uptime, version }
      Evidence: Test 'Response includes status, uptime, and version' passed

  [✓] Endpoint has no authentication requirement
      Evidence: Test 'No authentication required' passed (no auth headers)

  [✓] Tests verify the endpoint behavior
      Evidence: 4 tests created and passing

[2026-01-06T00:35:23Z] VERIFY: All acceptance criteria satisfied

--------------------------------------------------------------------------------
                              PULL REQUEST
--------------------------------------------------------------------------------

[2026-01-06T00:35:24Z] GIT: Committing changes...
[2026-01-06T00:35:24Z] GIT: Commit: "feat: Add health check endpoint (#42)"
[2026-01-06T00:35:25Z] GIT: Pushing branch to origin...
[2026-01-06T00:35:26Z] GIT: Branch pushed successfully

[2026-01-06T00:35:27Z] PR: Creating pull request...

  Title: Add health check endpoint
  Base:  main
  Head:  copilot/issue-42-health-check

  Body:
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ ## Summary                                                               │
  │                                                                          │
  │ Adds a `/health` endpoint that returns service status information.      │
  │                                                                          │
  │ ## Changes                                                               │
  │                                                                          │
  │ - Created `src/routes/health.js` with health check logic                 │
  │ - Registered route in `src/app.js`                                       │
  │ - Added tests in `tests/health.test.js`                                  │
  │                                                                          │
  │ ## Acceptance Criteria                                                   │
  │                                                                          │
  │ - [x] GET /health returns 200 OK                                         │
  │ - [x] Response includes { status, uptime, version }                      │
  │ - [x] Endpoint has no authentication requirement                         │
  │ - [x] Tests verify the endpoint behavior                                 │
  │                                                                          │
  │ Closes #42                                                               │
  └──────────────────────────────────────────────────────────────────────────┘

[2026-01-06T00:35:28Z] PR: Pull request #43 created
[2026-01-06T00:35:28Z] CODEOWNERS: Requesting review from @owner

================================================================================
                              RESULT: SUCCESS
================================================================================

Issue #42 processed without errors.

Files created:
  - src/routes/health.js
  - tests/health.test.js

Files modified:
  - src/app.js

Pull request: #43 (awaiting review)
Reviewer: @owner (via CODEOWNERS)

Next step: Human reviews PR via GitHub web UI

================================================================================
```

## Verification Summary

| Criterion | Status | Evidence |
|-----------|--------|----------|
| Issue template accepted input | PASS | All fields parsed correctly |
| @copilot assigned automatically | PASS | Label triggered assignment |
| Acceptance criteria verifiable | PASS | 4/4 criteria met with tests |
| PR created with linked issue | PASS | PR #43 closes #42 |
| CODEOWNERS assigned reviewer | PASS | @owner requested for review |
| System processed without errors | PASS | No errors in execution log |

## Conclusion

**SUCCESS**: The issue-driven development system processed the test issue without errors.

The workflow demonstrated:
1. Structured input via `task.yml` issue template
2. Automatic assignment to @copilot
3. Implementation following provided context
4. Test coverage for all acceptance criteria
5. PR creation with proper linking
6. Automatic reviewer assignment via CODEOWNERS
7. Ready for human review via GitHub web UI
