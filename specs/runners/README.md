# Test Runners for Event System Specifications

This directory contains test runners that execute the various specification formats used to define and validate the Event System's behavior.

## Overview

The Event System uses three complementary specification formats:

1. **BDD Gherkin Features** (.feature files) - Human-readable scenarios using Given/When/Then
2. **FIT Decision Tables** (.fit.md files) - Tabular test cases with inputs and expected outputs
3. **State Machine Specifications** (.state.md files) - State transition rules and invariants

## Runners

### bdd-runner.js

Executes Gherkin .feature files using a custom step definition framework.

**Usage:**
```bash
bun run specs:bdd                    # Run all .feature files
bun run specs/runners/bdd-runner.js specs/features/daemon-actor.feature
```

**Features:**
- Parses Gherkin syntax (Feature, Scenario, Given, When, Then, And)
- Maps steps to test implementations
- Provides detailed failure reports
- Supports Background sections for setup

### fit-runner.js

Executes FIT (Framework for Integrated Testing) decision tables from markdown files.

**Usage:**
```bash
bun run specs:fit                    # Run all .fit.md files
bun run specs/runners/fit-runner.js specs/fit-fixtures/daemon-actor.fit.md
```

**Features:**
- Parses markdown tables
- Executes each row as a test case
- Validates expected outputs against actual behavior
- Generates pass/fail matrix reports

### state-validator.js

Validates state machine specifications against actual implementations.

**Usage:**
```bash
bun run specs:states                 # Validate all state machine specs
bun run specs/runners/state-validator.js specs/state-machines/daemon-actor-state-machine.md
```

**Features:**
- Validates state definitions and transitions
- Checks invariant conditions
- Verifies entry/exit actions
- Tests valid/invalid operations per state

### report-generator.js

Generates unified test reports from all runner results.

**Usage:**
```bash
bun run specs/runners/report-generator.js --output reports/test-summary.md
```

**Features:**
- Aggregates results from all runners
- Calculates pass/fail statistics
- Identifies gaps between specs and implementation
- Generates actionable reports

## Running All Specs

Execute all specification tests:

```bash
# Run everything
bun test                             # Standard unit tests
bun run specs:all                    # All specification tests

# Run specific spec types
bun run specs:bdd                    # BDD features only
bun run specs:fit                    # FIT tables only
bun run specs:states                 # State machines only
```

## Integration with Bun Test

The runners integrate with Bun's native test infrastructure:

- Each runner can be invoked as a standard test file
- Reports use Bun's test reporter format
- Can be run alongside unit tests in CI/CD

## Test Implementation Strategy

### BDD Runner Implementation

BDD steps are implemented as regex patterns mapped to functions:

```javascript
// Example step definition
defineStep(/^a DaemonActor with valid configuration$/, async (context) => {
  context.daemon = new DaemonActor('./test-config.json');
});

defineStep(/^I start the daemon$/, async (context) => {
  context.result = await context.daemon.start();
});

defineStep(/^the daemon state should be RUNNING$/, async (context) => {
  expect(context.daemon.state).toBe('running');
});
```

### FIT Runner Implementation

FIT tables are parsed and each row becomes a test case:

```javascript
// Table row: | STOPPED | start() | RUNNING | true | - |
// Becomes:
await daemon.setState('STOPPED');
const result = await daemon.start();
expect(daemon.state).toBe('RUNNING');
expect(result.success).toBe(true);
```

### State Validator Implementation

State machines are validated by:
1. Parsing state definitions from markdown
2. Testing each state's invariants
3. Attempting valid and invalid transitions
4. Verifying entry/exit actions

## Report Output

Reports are generated in both console and markdown formats:

```
BDD Scenarios: 45/50 passed (90%)
FIT Tables: 18/20 passed (90%)
State Machines: 6/6 validated (100%)
Overall: 69/76 passed (91%)

Failed Tests:
- daemon-actor.feature:21 - Prevent starting an already running daemon
- event-log-actor.fit.md:Table 5, Row 3 - Checkpoint creation
```

## Development Workflow

1. Write specifications first (BDD, FIT, or State Machine)
2. Run specs to see failures
3. Implement actor behavior
4. Run specs again until all pass
5. Generate report to identify gaps

## File Locations

```
specs/
├── features/           # BDD .feature files
├── fit-fixtures/       # FIT .fit.md files
├── state-machines/     # State machine .state.md files
└── runners/            # Test runners (this directory)
    ├── bdd-runner.js
    ├── fit-runner.js
    ├── state-validator.js
    └── report-generator.js
```

## Success Metrics

- 80%+ spec pass rate indicates solid implementation
- 100% pass rate means specification and implementation are aligned
- Failed specs reveal implementation gaps (this is valuable!)
- Reports should be actionable and identify specific issues

## Next Steps

1. Run all specs: `bun run specs:all`
2. Review report: Check console output and generated markdown
3. Fix failures: Implement missing behavior
4. Iterate: Re-run specs until targets met
5. Close bead: Document completion with pass rates
