# Session Knowledge System - Test Architecture

**Epic:** `agentic-primer-t49.5`

---

## Test Pyramid

```
                    ┌─────────────────────────┐
                    │   E2E Tests (2 files)   │
                    │   Full workflows        │
                    │   ~5% of tests          │
                    └─────────────────────────┘
                            ▲
                   ┌────────┴────────┐
                   │  Integration     │
                   │  (37 tests)      │
                   │  Component combo │
                   │  ~15% of tests   │
                   └──────────────────┘
                          ▲
              ┌───────────┴───────────┐
              │   Unit Tests          │
              │   (100+ tests)        │
              │   Single components   │
              │   ~80% of tests       │
              └───────────────────────┘
```

---

## Test Flow

```
┌──────────────┐
│  Developer   │
│   Changes    │
└──────┬───────┘
       │
       ▼
┌──────────────────────────────────────────┐
│  Local Development                        │
│  ┌────────────────────────────────────┐ │
│  │  test-watch.sh                     │ │
│  │  - Monitors file changes           │ │
│  │  - Auto-runs relevant tests        │ │
│  │  - Fast feedback (<1s)             │ │
│  └────────────────────────────────────┘ │
└──────┬───────────────────────────────────┘
       │
       ▼
┌──────────────────────────────────────────┐
│  Pre-Commit                               │
│  ┌────────────────────────────────────┐ │
│  │  test-all.sh --fast                │ │
│  │  - Run core tests                  │ │
│  │  - Skip slow E2E                   │ │
│  │  - Quick validation (<5s)          │ │
│  └────────────────────────────────────┘ │
└──────┬───────────────────────────────────┘
       │
       ▼
┌──────────────────────────────────────────┐
│  CI Pipeline                              │
│  ┌────────────────────────────────────┐ │
│  │  test-all.sh --ci                  │ │
│  │  - All test suites                 │ │
│  │  - Coverage generation             │ │
│  │  - Type checking                   │ │
│  │  - Security scanning               │ │
│  │  - Performance validation          │ │
│  └────────────────────────────────────┘ │
└──────┬───────────────────────────────────┘
       │
       ▼
┌──────────────┐
│  Deployment  │
│  (if green)  │
└──────────────┘
```

---

## Component Tests

### 1. Cognitive Integration Tests

```
cognitive-integration.test.ts
├── ConfidenceDecay (24 tests)
│   ├── Tech domain
│   ├── Science domain
│   ├── News domain
│   ├── Core domain
│   ├── Edge cases
│   ├── Time estimation
│   └── Configuration
├── TemporalQueries (9 tests)
│   ├── queryAtTime
│   ├── getChangesBetween
│   └── getWithDecay
├── ArcDetector (4 tests)
│   ├── detectArcs
│   └── getSessionArcs
└── E2E Integration (1 test)
    └── Complete workflow
```

### 2. Entity Tests

```
entities/
├── provider.test.ts      - Provider CRUD
├── model.test.ts         - Model operations
├── agent.test.ts         - Agent management
├── session.test.ts       - Session lifecycle
├── task.test.ts          - Task handling
├── information.test.ts   - Information storage
├── embedding.test.ts     - Embedding vectors
├── human.test.ts         - Human entities
├── program.test.ts       - Program entities
├── agent-session.e2e.ts  - E2E workflows
└── model.e2e.ts          - E2E integration
```

### 3. Core System Tests

```
core/
├── graph.test.ts         - Graph operations
│   ├── Address class
│   ├── Node class
│   ├── Edge class
│   ├── GraphStore
│   └── Event sourcing
└── context.test.ts       - Context management
```

---

## Test Data Flow

```
┌─────────────────────────────────────┐
│  Test Database                       │
│  ~/.claude/index/sessions-libsql.db │
└─────────────┬───────────────────────┘
              │
    ┌─────────┼─────────┐
    │         │         │
    ▼         ▼         ▼
┌────────┐ ┌───────┐ ┌────────┐
│Sessions│ │ Know  │ │Temporal│
│Metadata│ │ledge  │ │ Data   │
└────────┘ └───────┘ └────────┘
    │         │         │
    └─────────┼─────────┘
              │
              ▼
        ┌──────────┐
        │  Tests   │
        │  Read    │
        └──────────┘
```

**Characteristics:**
- Shared database across tests
- Read-only operations (no mutations)
- Real data for integration tests
- Cleanup not required (idempotent)

---

## Test Runner Architecture

```
┌────────────────────────────────────┐
│  test-all.sh                       │
│  ┌──────────────────────────────┐ │
│  │  Environment Check           │ │
│  │  - Bun version               │ │
│  │  - Database existence        │ │
│  │  - Dependencies              │ │
│  └──────────────────────────────┘ │
│  ┌──────────────────────────────┐ │
│  │  Suite Execution             │ │
│  │  - Core System Tests         │ │
│  │  - Entity Tests (optional)   │ │
│  │  - Cognitive Tests           │ │
│  │  - Additional Tests          │ │
│  └──────────────────────────────┘ │
│  ┌──────────────────────────────┐ │
│  │  Reporting                   │ │
│  │  - Pass/Fail summary         │ │
│  │  - Coverage report           │ │
│  │  - Exit code                 │ │
│  │  - JSON output (CI)          │ │
│  └──────────────────────────────┘ │
└────────────────────────────────────┘
```

---

## CI/CD Pipeline

```
┌──────────────────────────────────────────────────┐
│  GitHub Actions Workflow                         │
│  ┌────────────────────────────────────────────┐ │
│  │  test (Main Job)                           │ │
│  │  - Setup Bun                               │ │
│  │  - Install dependencies                    │ │
│  │  - Run test-all.sh --ci                    │ │
│  │  - Generate coverage                       │ │
│  │  - Upload artifacts                        │ │
│  └────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────┐ │
│  │  lint                                      │ │
│  │  - Type checking                           │ │
│  │  - Code quality                            │ │
│  └────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────┐ │
│  │  integration (main only)                   │ │
│  │  - Full database tests                     │ │
│  │  - Performance validation                  │ │
│  └────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────┐ │
│  │  security                                  │ │
│  │  - Dependency audit                        │ │
│  │  - Secret scanning                         │ │
│  └────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────┘
```

---

## Coverage Architecture

```
┌─────────────────────────────────────┐
│  Source Code                         │
│  src/session-knowledge/              │
└─────────────┬───────────────────────┘
              │
              ▼
┌─────────────────────────────────────┐
│  Test Execution                      │
│  bun test --coverage                 │
└─────────────┬───────────────────────┘
              │
              ▼
┌─────────────────────────────────────┐
│  Coverage Data                       │
│  .coverage/                          │
│  ├── lcov.info                       │
│  ├── coverage.txt                    │
│  └── html/                           │
└─────────────┬───────────────────────┘
              │
    ┌─────────┼─────────┐
    │         │         │
    ▼         ▼         ▼
┌────────┐ ┌──────┐ ┌────────┐
│Artifact│ │Report│ │Threshold│
│Upload  │ │PR    │ │Check    │
└────────┘ └──────┘ └────────┘
```

---

## Performance Monitoring

```
Test Execution Timeline:

0ms     ┌─────────────────────────────────┐
        │  Setup (beforeAll)              │
        │  - Create instances             │
        │  - Open connections             │
10ms    └─────────────────────────────────┘
        │
        ┌─────────────────────────────────┐
        │  Unit Tests (fast)              │
        │  - ConfidenceDecay: ~1ms each   │
        │  - Validation: ~0.5ms each      │
30ms    └─────────────────────────────────┘
        │
        ┌─────────────────────────────────┐
        │  Integration Tests (slower)     │
        │  - Temporal queries: ~5ms each  │
        │  - Arc detection: ~30ms each    │
60ms    └─────────────────────────────────┘
        │
        ┌─────────────────────────────────┐
        │  Cleanup (afterAll)             │
        │  - Close connections            │
70ms    └─────────────────────────────────┘
```

**Benchmarks:**
- Unit test: <1ms
- Integration test: <10ms
- E2E test: <100ms
- Full suite: <5s

---

## Error Handling Flow

```
Test Execution
      │
      ▼
  ┌───────┐
  │ PASS? │
  └───┬───┘
      │
  ┌───┴───┐
  │       │
  ▼       ▼
PASS    FAIL
  │       │
  │       ▼
  │   ┌────────────────┐
  │   │ Capture Output │
  │   └────────┬───────┘
  │            │
  │            ▼
  │   ┌────────────────┐
  │   │ Show Error     │
  │   │ - Stack trace  │
  │   │ - Test name    │
  │   │ - Expected     │
  │   └────────┬───────┘
  │            │
  │            ▼
  │   ┌────────────────┐
  │   │ Mark Failed    │
  │   └────────┬───────┘
  │            │
  └────────────┴───────
               │
               ▼
         ┌──────────┐
         │  Report  │
         │  Summary │
         └──────────┘
               │
               ▼
         ┌──────────┐
         │Exit Code │
         │ 0 or 1   │
         └──────────┘
```

---

## Watch Mode Architecture

```
┌────────────────────────────────────┐
│  test-watch.sh                     │
│                                    │
│  ┌──────────────────────────────┐ │
│  │  File Monitor                │ │
│  │  - fswatch (macOS)           │ │
│  │  - inotifywait (Linux)       │ │
│  │  - Watches src/              │ │
│  └───────────┬──────────────────┘ │
│              │                    │
│              ▼                    │
│  ┌──────────────────────────────┐ │
│  │  Change Detection            │ │
│  │  - Filter relevant files     │ │
│  │  - Debounce changes          │ │
│  └───────────┬──────────────────┘ │
│              │                    │
│              ▼                    │
│  ┌──────────────────────────────┐ │
│  │  Test Execution              │ │
│  │  - Run filtered tests        │ │
│  │  - Show results              │ │
│  └───────────┬──────────────────┘ │
│              │                    │
│              ▼                    │
│  ┌──────────────────────────────┐ │
│  │  Wait for Next Change        │ │
│  └──────────────────────────────┘ │
└────────────────────────────────────┘
```

---

## Documentation Hierarchy

```
Test Documentation
├── TESTING.md                 (Comprehensive, 667 lines)
│   ├── Overview
│   ├── Test Suites
│   ├── Running Tests
│   ├── Test Data Setup
│   ├── Performance
│   ├── CI/CD
│   ├── Troubleshooting
│   └── Best Practices
├── TEST-QUICK-START.md        (Quick reference, 100 lines)
│   ├── TL;DR
│   ├── Common Commands
│   └── Troubleshooting
├── TEST-ARCHITECTURE.md       (This file, architecture)
│   ├── Test Pyramid
│   ├── Test Flow
│   ├── Component Tests
│   └── Diagrams
└── README.md                  (Updated with test info)
    └── Test Coverage section
```

---

## Integration Points

```
┌─────────────────────────────────────────────────┐
│  Know Tool CLI                                   │
│  ./know [command]                                │
└───────────┬─────────────────────────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────┐
│  Session Knowledge System                        │
│  ├── Extraction                                  │
│  ├── Classification                              │
│  ├── Temporal                                    │
│  └── Search                                      │
└───────────┬─────────────────────────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────┐
│  Test Suite                                      │
│  ├── Unit Tests (components)                    │
│  ├── Integration Tests (workflows)              │
│  └── E2E Tests (full system)                    │
└───────────┬─────────────────────────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────┐
│  CI/CD Pipeline                                  │
│  ├── GitHub Actions                              │
│  ├── Coverage Reports                            │
│  └── Quality Gates                               │
└─────────────────────────────────────────────────┘
```

---

## Summary

**Test Organization:**
- Pyramid: 80% unit, 15% integration, 5% E2E
- Coverage: 37 cognitive tests, 100+ total tests
- Execution: <5s full suite, <1s fast mode

**Infrastructure:**
- Scripts: test-all.sh, test-watch.sh
- CI: GitHub Actions workflow
- Coverage: lcov reports with thresholds

**Quality:**
- Comprehensive documentation (667 lines)
- Clear architecture and flows
- Fast feedback loops
- Automated validation

---

**Epic:** `agentic-primer-t49.5`
**Status:** ✅ Complete
