# Immediate Path-Based Addressing Migration Plan

**Date:** 2026-02-06
**Branch:** feature/path-addressing
**Status:** READY FOR EXECUTION
**Estimated Duration:** 2 hours

---

## Executive Summary

This plan migrates the SEAG codebase from dual routing (flat IDs + hierarchical paths) to **paths-only mode**. The migration removes all flat ID support, simplifying the codebase and completing Phase 6 of the path-addressing initiative.

**Current State:**
- ✅ Dual routing system implemented (Phase 6 complete)
- ✅ Migration tooling built and tested
- ✅ Path caching and performance optimization complete
- ✅ All 2416 tests passing (6 known failures unrelated to addressing)

**Target State:**
- ❌ Flat ID support removed entirely
- ❌ All actors using hierarchical paths
- ❌ Alias resolver deleted
- ❌ Address parser simplified to paths-only
- ❌ 100% path-based addressing throughout codebase

**Migration Scope:**
- **Total flat ID usages:** 251 occurrences
- **Files affected:** 36 files
- **Production files:** 17 in src/
- **Demo/test files:** 19 files
- **Complex migrations:** 30 actor registrations
- **Simple migrations:** 221 address calls

---

## 1. Pre-Migration Audit

### 1.1 Current Usage Statistics

```
Total Flat ID Usages:     251
Total Actors:             41
Hierarchical Actors:      0
Flat ID Actors:           41
Migration Progress:       0.00%

BY COMPLEXITY
  Simple:     221 (quick fixes)
  Moderate:   0 (requires review)
  Complex:    30 (structural changes)

BY TYPE
  address-call:         216 occurrences (34 files)
  actor-registration:   30 occurrences (12 files)
  alias-registration:   5 occurrences (4 files)
```

### 1.2 Top 20 Files by Flat ID Usage

| Count | File                               | Category   |
|-------|------------------------------------|------------|
| 47    | test-integration.ts                | Test       |
| 22    | demo-knowledge-graph.ts            | Demo       |
| 17    | demo-task-knowledge.ts             | Demo       |
| 15    | build-pipeline.ts (examples)       | Example    |
| 14    | demo-knowledge-actors.ts           | Demo       |
| 13    | compiler.ts (query)                | Production |
| 11    | demo-actors-only.ts                | Demo       |
| 10    | demo-full-system.ts                | Demo       |
| 9     | test-stress.ts                     | Test       |
| 8     | streaming-benchmark.ts             | Test       |
| 6     | integration-demo.ts (query)        | Demo       |
| 6     | examples-phase3.ts (query)         | Demo       |
| 6     | app.ts (widget-actors)             | Example    |
| 6     | program-executor-example.ts        | Example    |
| 6     | inference-actor-usage.ts           | Example    |
| 5     | analyzer.ts (migration)            | Production |
| 5     | demo-message-layer.ts              | Demo       |
| 4     | test-streaming-live.ts             | Test       |
| 4     | test-streaming-integration.ts      | Test       |
| 4     | refactor.ts (migration)            | Production |

### 1.3 Production Files Requiring Migration

**Critical Production Files (src/):**
1. `src/query/compiler.ts` - 13 occurrences
2. `src/migration/analyzer.ts` - 5 occurrences
3. `src/migration/refactor.ts` - 4 occurrences
4. `src/migration/planner.ts` - 3 occurrences
5. `src/messaging/router.ts` - Dual routing logic (to be simplified)
6. `src/messaging/alias-resolver.ts` - **DELETE THIS FILE**
7. `src/messaging/address-parser.ts` - Simplify to paths-only
8. `src/messaging/actors/task.ts` - 4 occurrences
9. `src/messaging/browser/actor-registry.ts` - 4 occurrences
10. `src/messaging/browser/widget-actor.ts` - 1 occurrence
11. `src/messaging/channels/stream.ts` - 2 occurrences
12. `src/messaging/hierarchical-routing-poc.ts` - 3 occurrences
13. `src/query/examples-phase3.ts` - 6 occurrences (demo code)
14. `src/query/integration-demo.ts` - 6 occurrences (demo code)
15. `src/query/explain-demo.ts` - 2 occurrences (demo code)
16. `src/query/explain/examples.ts` - 3 occurrences (demo code)
17. `src/messaging/__tests__/streaming-benchmark.ts` - 8 occurrences (test)

### 1.4 Dependency Analysis

**Files importing alias-resolver.ts:**
- `src/migration/refactor.ts` (needs update)
- `docs/DUAL_ROUTING_MIGRATION.md` (documentation reference)

**Files importing address-parser.ts:**
- None directly use the format detection (all use parseAddress from message.ts)

### 1.5 Pre-Migration Checklist

```bash
# Command 1: Ensure clean working directory
git status

# Command 2: Confirm we're on the right branch
git branch --show-current

# Command 3: Verify all tests pass before migration
npm test 2>&1 | tail -10

# Command 4: Create safety snapshot
git add -A && git commit -m "Pre-migration snapshot: All systems operational"

# Command 5: Verify analysis is current
npm run migrate:analyze
```

**Expected Results:**
- Branch: `feature/path-addressing`
- Tests: 2416 pass, 6 fail (known failures)
- Analysis: 251 flat IDs across 36 files

---

## 2. Transformation Strategy

### 2.1 Overview

The migration follows a **bottom-up approach**:
1. **Phase A:** Automated refactoring (address calls, registrations)
2. **Phase B:** Manual cleanup (complex patterns, edge cases)
3. **Phase C:** Code removal (alias-resolver, dual routing logic)
4. **Phase D:** Documentation updates

### 2.2 Path Mapping Strategy

Since we're moving to paths-only, we need to define canonical paths for all flat IDs. Here's the proposed hierarchy:

```
# Core Services
@(inference) → @(services/inference)
@(executor) → @(services/executor)
@(llm) → @(services/llm)
@(storage) → @(services/storage)
@(knowledge) → @(services/knowledge)

# Domain Actors
@(tasks) → @(domain/tasks)
@(relationships) → @(domain/relationships)
@(orchestrator) → @(domain/orchestrator)

# Tools
@(tool-bash) → @(tools/bash)
@(tool-read) → @(tools/read)
@(tool-write) → @(tools/write)

# Test Actors
@(test-session) → @(test/session)
@(test-session-mock) → @(test/session-mock)
@(mock-session) → @(test/mock-session)
@(filesystem) → @(test/filesystem)
@(code-execution) → @(test/code-execution)
@(streaming-test) → @(test/streaming)
@(test-actor) → @(test/actor)
@(non-streaming) → @(test/non-streaming)

# Demo Actors
@(calculator) → @(demo/calculator)
@(echo) → @(demo/echo)
@(echo-caller) → @(demo/echo-caller)
@(worker) → @(demo/worker)
@(logger) → @(demo/logger)
@(file-checker) → @(demo/file-checker)
@(file-processor) → @(demo/file-processor)
@(program-a|b|c) → @(demo/program-{a,b,c})
@(api-client) → @(demo/api-client)
@(stress-session) → @(demo/stress-session)
@(stream-session) → @(demo/stream-session)

# Workflow Actors
@(file-workflow) → @(workflows/file)
@(data-pipeline) → @(workflows/data-pipeline)
@(parallel-executor) → @(workflows/parallel-executor)
@(report-generator) → @(workflows/report-generator)
@(error-handler) → @(workflows/error-handler)
@(failing-program) → @(workflows/failing-program)

# Test Config
@(test-config) → @(test/config)
@(test-workflow-config) → @(test/workflow-config)
@(quick-mock) → @(test/quick-mock)

# Benchmark Actors
@(bench-1k) → @(bench/1k)
@(bench-10k) → @(bench/10k)
@(bench-100k) → @(bench/100k)
@(bench-bp) → @(bench/backpressure)
```

---

## 3. Phase A: Automated Refactoring (30 minutes)

### 3.1 Step 1: Generate Mapping Configuration (5 minutes)

Create a mapping file for the refactoring tool:

```bash
# Command 1: Create mapping file
cat > .migration-output/path-mappings.json << 'EOF'
{
  "inference": "services/inference",
  "executor": "services/executor",
  "llm": "services/llm",
  "storage": "services/storage",
  "knowledge": "services/knowledge",
  "tasks": "domain/tasks",
  "relationships": "domain/relationships",
  "orchestrator": "domain/orchestrator",
  "tool-bash": "tools/bash",
  "tool-read": "tools/read",
  "tool-write": "tools/write",
  "test-session": "test/session",
  "test-session-mock": "test/session-mock",
  "mock-session": "test/mock-session",
  "filesystem": "test/filesystem",
  "code-execution": "test/code-execution",
  "streaming-test": "test/streaming",
  "test-actor": "test/actor",
  "non-streaming": "test/non-streaming",
  "calculator": "demo/calculator",
  "echo": "demo/echo",
  "echo-caller": "demo/echo-caller",
  "worker": "demo/worker",
  "logger": "demo/logger",
  "file-checker": "demo/file-checker",
  "file-processor": "demo/file-processor",
  "program-a": "demo/program-a",
  "program-b": "demo/program-b",
  "program-c": "demo/program-c",
  "api-client": "demo/api-client",
  "stress-session": "demo/stress-session",
  "stream-session": "demo/stream-session",
  "file-workflow": "workflows/file",
  "data-pipeline": "workflows/data-pipeline",
  "parallel-executor": "workflows/parallel-executor",
  "report-generator": "workflows/report-generator",
  "error-handler": "workflows/error-handler",
  "failing-program": "workflows/failing-program",
  "test-config": "test/config",
  "test-workflow-config": "test/workflow-config",
  "quick-mock": "test/quick-mock",
  "bench-1k": "bench/1k",
  "bench-10k": "bench/10k",
  "bench-100k": "bench/100k",
  "bench-bp": "bench/backpressure",
  "flat-id": "test/flat-id",
  "id": "test/id"
}
EOF

# Command 2: Verify mapping file
cat .migration-output/path-mappings.json | jq 'keys | length'
```

**Expected Result:** 42 mappings defined

### 3.2 Step 2: Update Migration Refactor Tool (10 minutes)

The existing refactor tool needs to load this mapping and apply transformations.

**Manual Edit Required:**
Edit `src/migration/refactor.ts` to:
1. Load path-mappings.json
2. Use mappings for address() transformations
3. Use mappings for spawn() actor registrations

**Changes needed in refactor.ts:**
```typescript
// Add at top of file
import { readFileSync } from 'fs';
import { join } from 'path';

// Add method to load mappings
private loadPathMappings(): Record<string, string> {
  const mappingFile = join(process.cwd(), '.migration-output', 'path-mappings.json');
  try {
    return JSON.parse(readFileSync(mappingFile, 'utf-8'));
  } catch {
    console.warn('Path mappings not found, using empty mappings');
    return {};
  }
}

// Update transformAddressCall to use mappings
private transformAddressCall(node: any, mappings: Record<string, string>): any {
  // ... existing code ...
  const flatId = node.arguments[0].value;
  const canonicalPath = mappings[flatId] || `unmapped/${flatId}`;
  // Replace with canonical path
}
```

### 3.3 Step 3: Dry Run Transformation (5 minutes)

```bash
# Command 1: Generate migration plan with mappings
npm run migrate:plan

# Command 2: Dry run to preview changes
npm run migrate:execute -- --dry-run --verbose > .migration-output/dry-run.log 2>&1

# Command 3: Check dry run results
cat .migration-output/dry-run.log | grep -E "^(✓|✗)" | head -20

# Command 4: Count expected changes
cat .migration-output/dry-run.log | grep "✓" | wc -l
```

**Expected Result:** ~36 files processed, ~251 changes applied (in dry run)

### 3.4 Step 4: Execute Automated Transformation (10 minutes)

**CRITICAL: Create commit point before executing**

```bash
# Command 1: Commit pre-execution state
git add -A && git commit -m "Pre-refactor snapshot: Analysis and mappings complete"

# Command 2: Execute migration (NO DRY RUN)
npm run migrate:execute -- --verbose 2>&1 | tee .migration-output/execution.log

# Command 3: Check execution results
cat .migration-output/execution.log | grep -E "^(✓|✗)" | tail -20

# Command 4: Verify file changes
git status

# Command 5: Review changes in key files
git diff src/query/compiler.ts | head -50
git diff demo-task-knowledge.ts | head -50
```

**Expected Result:**
- ~36 files modified
- ~251 flat IDs transformed to paths
- Git shows modifications to all affected files

---

## 4. Phase B: Manual Cleanup (20 minutes)

### 4.1 Edge Cases That Need Manual Review

#### 4.1.1 Migration Tool Self-References (5 minutes)

The migration tool itself contains flat ID examples for testing. These need special handling.

**Files:**
- `src/migration/analyzer.ts` (lines 288, 337)
- `src/migration/planner.ts` (line references)
- `src/migration/refactor.ts` (example code)

**Action:**
These are **test fixtures** demonstrating flat ID detection. They should remain as-is but be clearly marked as intentional examples.

```bash
# Command: Add comments to preserve these as examples
# Manual edit required - add comments like:
# // EXAMPLE: Flat ID for testing (intentionally preserved)
```

#### 4.1.2 Documentation Examples (5 minutes)

**Files:**
- `docs/DUAL_ROUTING_MIGRATION.md` - Contains migration examples with flat IDs
- `docs/PATH_ADDRESSING_DESIGN.md` - May reference flat IDs

**Action:**
Update documentation to show "before/after" migration examples, not live flat ID usage.

```bash
# Command: Review documentation files
grep -n "@(" docs/*.md | grep -v "domain/" | grep -v "services/"

# Manual edit: Update examples to show hierarchical paths
```

#### 4.1.3 Complex Registration Patterns (10 minutes)

Some actor registrations may have dynamic IDs or computed paths that the AST tool can't handle.

**Action:**
```bash
# Command 1: Search for dynamic registrations
grep -n "spawn.*\${" src/**/*.ts examples/**/*.ts

# Command 2: Search for computed IDs
grep -n "spawn.*\+" src/**/*.ts examples/**/*.ts

# Manual review: Update each case individually
```

---

## 5. Phase C: Code Removal (15 minutes)

### 5.1 Step 1: Remove Alias Resolver (5 minutes)

```bash
# Command 1: Remove the alias resolver file
git rm src/messaging/alias-resolver.ts

# Command 2: Remove tests for alias resolver
git rm src/messaging/__tests__/alias-resolver.test.ts

# Command 3: Check for remaining imports
grep -r "alias-resolver" src/ examples/ --include="*.ts"

# Command 4: Remove import from refactor.ts (manual edit)
# Edit src/migration/refactor.ts - remove:
# import { ... } from './alias-resolver';
```

**Verification:**
```bash
# Should return no results
grep -r "alias-resolver" src/ --include="*.ts"
```

### 5.2 Step 2: Simplify Address Parser (5 minutes)

The address-parser.ts currently detects flat vs hierarchical. In paths-only mode, we can simplify it.

**Manual Edit Required:**

Edit `src/messaging/address-parser.ts`:

```typescript
// BEFORE (dual routing):
export enum AddressFormat {
  FLAT_ID = 'FLAT_ID',
  HIERARCHICAL_PATH = 'HIERARCHICAL_PATH',
}

export function parseAddressInfo(addr: Address): ParsedAddressInfo {
  const raw = parseAddress(addr);
  const isHierarchical = raw.includes('/');

  if (isHierarchical) {
    // ... hierarchical handling
  } else {
    // ... flat ID handling
  }
}

// AFTER (paths-only):
export interface ParsedAddressInfo {
  address: Address;
  raw: string;
  segments: string[];
}

export function parseAddressInfo(addr: Address): ParsedAddressInfo {
  const raw = parseAddress(addr);
  const segments = raw.split('/').filter(s => s.length > 0);

  return { address: addr, raw, segments };
}

// Remove: AddressFormat enum
// Remove: isFlatId(), isHierarchicalPath(), isLegacy field
```

**Commands:**
```bash
# Command 1: Edit the file manually (use Read + Edit tools)

# Command 2: Verify compilation
npm run build 2>&1 | grep -E "(error|Error)" | head -10

# Command 3: Run tests to catch breakage
npm test src/messaging/__tests__/address-parser.test.ts
```

### 5.3 Step 3: Remove Dual Routing from Router (5 minutes)

Edit `src/messaging/router.ts` to remove flat ID resolution logic.

**Manual Edit Required:**

```typescript
// BEFORE: Router checks format and attempts alias resolution
import { resolveAliasAddress } from './alias-resolver';
import { parseAddressInfo, AddressFormat } from './address-parser';

async route(message: Message): Promise<boolean> {
  const info = parseAddressInfo(message.to);

  if (info.format === AddressFormat.FLAT_ID) {
    // Attempt alias resolution
    const resolved = await resolveAliasAddress(message.to);
    // ... fallback to graph store
  } else {
    // Hierarchical routing
  }
}

// AFTER: Router assumes all addresses are hierarchical
import { parseAddressInfo } from './address-parser';

async route(message: Message): Promise<boolean> {
  const info = parseAddressInfo(message.to);
  // Route hierarchically using segments
  return this.routeHierarchical(info.segments);
}
```

**Commands:**
```bash
# Command 1: Edit router.ts (manual)

# Command 2: Remove imports
# - Remove: import { resolveAliasAddress } from './alias-resolver';
# - Remove: import { AddressFormat } from './address-parser';

# Command 3: Simplify routing logic

# Command 4: Verify compilation
npm run build 2>&1 | grep -E "(error|Error)"

# Command 5: Run routing tests
npm test src/messaging/__tests__/router.test.ts
```

---

## 6. Phase D: Test Execution and Fixes (30 minutes)

### 6.1 Step 1: Run Full Test Suite (10 minutes)

```bash
# Command 1: Run all tests
npm test 2>&1 | tee .migration-output/post-migration-tests.log

# Command 2: Extract test summary
tail -30 .migration-output/post-migration-tests.log

# Command 3: Identify new failures
grep -E "^\\s+✗" .migration-output/post-migration-tests.log > .migration-output/failures.txt

# Command 4: Count failures by file
cat .migration-output/failures.txt | awk -F'›' '{print $1}' | sort | uniq -c
```

**Expected Issues:**
- Tests that expect flat IDs to work (dual routing tests)
- Tests for removed functionality (alias resolver tests)
- Hard-coded flat ID assertions

### 6.2 Step 2: Fix Test Failures (15 minutes)

#### 6.2.1 Remove Dual Routing Tests

```bash
# Command 1: Identify dual routing test files
find src -name "*dual-routing*" -o -name "*alias-resolver*"

# Command 2: Remove or skip these tests
git rm src/messaging/__tests__/dual-routing.test.ts
git rm src/messaging/__tests__/alias-resolver.test.ts

# Alternative: Mark as skipped
# Edit files and add: test.skip("...", () => { ... })
```

#### 6.2.2 Update Tests with Hard-Coded Flat IDs

```bash
# Command 1: Find tests using flat IDs in assertions
grep -r "expect.*@(" src/**/*.test.ts | grep -v "/" | head -20

# Command 2: Update each test to use hierarchical paths
# Manual edit: Replace flat IDs with mapped paths
```

**Example Test Fix:**
```typescript
// BEFORE
expect(actor.address).toBe('@(tasks)');

// AFTER
expect(actor.address).toBe('@(domain/tasks)');
```

### 6.3 Step 3: Verify No Regressions (5 minutes)

```bash
# Command 1: Run tests again after fixes
npm test 2>&1 | tail -30

# Command 2: Compare test counts
echo "Before migration: 2416 pass, 6 fail"
echo "After migration: [TO BE DETERMINED]"

# Command 3: Check for performance regressions
npm test src/messaging/__tests__/path-performance.test.ts

# Command 4: Verify no new errors
git diff --stat
```

**Acceptance Criteria:**
- All tests pass (except known 6 failures)
- No new failures introduced
- Performance tests show improvement (no alias resolution overhead)

---

## 7. Phase E: Documentation Updates (10 minutes)

### 7.1 Update Migration Documentation

```bash
# Command 1: Update DUAL_ROUTING_MIGRATION.md
# Mark as DEPRECATED / HISTORICAL
cat > docs/DUAL_ROUTING_MIGRATION.md << 'EOF'
# Dual Routing Migration Guide

**STATUS: DEPRECATED**
**Date Retired:** 2026-02-06

This document describes the dual routing system that was used during migration from flat IDs to hierarchical paths. As of 2026-02-06, the system uses **paths-only mode** and this guide is preserved for historical reference only.

[... rest of content marked as historical ...]
EOF

# Command 2: Create new PATHS_ONLY.md guide
cat > docs/PATHS_ONLY.md << 'EOF'
# Path-Based Addressing Guide

**Status:** ACTIVE
**Date:** 2026-02-06

This system uses **hierarchical path addressing** exclusively. All actors are addressed using canonical paths like `@(domain/tasks)` or `@(services/inference)`.

## Address Format

**Format:** `@(segment/segment/...)`

Examples:
- `@(services/inference)` - Service actors
- `@(domain/tasks)` - Domain actors
- `@(tools/bash)` - Tool actors
- `@(workflows/build/compile)` - Nested workflows

## Routing

All routing is hierarchical through supervision trees. See docs/PATH_ADDRESSING_DESIGN.md for details.
EOF
```

### 7.2 Update README and Architecture Docs

```bash
# Command 1: Update README.md references
# Replace "dual routing" mentions with "path-based addressing"

# Command 2: Update ARCHITECTURE.md
# Remove sections about flat IDs and alias resolution

# Command 3: Update ROADMAP.md
# Mark Phase 6 as COMPLETE
```

---

## 8. Timeline Breakdown

### Minute-by-Minute Execution Plan

```
00:00-00:10  Pre-Migration Audit
             ├─ Run git status, verify branch
             ├─ Run npm test, verify baseline
             ├─ Run npm run migrate:analyze
             └─ Create pre-migration snapshot commit

00:10-00:20  Path Mapping Configuration
             ├─ Create path-mappings.json
             ├─ Verify 42 mappings defined
             └─ Review mapping strategy

00:20-00:35  Update Migration Tooling
             ├─ Edit src/migration/refactor.ts
             ├─ Add loadPathMappings() method
             ├─ Update transformAddressCall()
             └─ Test compilation

00:35-00:45  Dry Run and Validation
             ├─ npm run migrate:plan
             ├─ npm run migrate:execute --dry-run
             ├─ Review dry-run.log
             └─ Verify 251 changes detected

00:45-00:50  Pre-Execution Safety
             ├─ Create pre-refactor snapshot commit
             └─ Final review of plan

00:50-01:05  Execute Automated Migration
             ├─ npm run migrate:execute
             ├─ Review execution.log
             ├─ git status review
             └─ git diff spot checks

01:05-01:15  Manual Edge Case Cleanup
             ├─ Review migration tool self-references
             ├─ Update documentation examples
             └─ Handle dynamic registrations

01:15-01:25  Remove Alias Resolver
             ├─ git rm alias-resolver.ts
             ├─ git rm __tests__/alias-resolver.test.ts
             └─ Remove imports from other files

01:25-01:35  Simplify Address Parser
             ├─ Edit address-parser.ts
             ├─ Remove AddressFormat enum
             ├─ Remove isFlatId() / isHierarchicalPath()
             └─ Test compilation

01:35-01:45  Simplify Router
             ├─ Edit router.ts
             ├─ Remove alias resolution calls
             ├─ Remove dual routing logic
             └─ Test compilation

01:45-02:00  Test Suite Execution
             ├─ npm test (full suite)
             ├─ Review failures
             └─ Identify fix targets

02:00-02:15  Fix Test Failures
             ├─ Remove dual routing tests
             ├─ Update hard-coded flat ID assertions
             └─ Re-run tests

02:15-02:25  Verify No Regressions
             ├─ Compare test counts
             ├─ Check performance tests
             └─ Review git diff

02:25-02:35  Documentation Updates
             ├─ Mark DUAL_ROUTING_MIGRATION.md as deprecated
             ├─ Create PATHS_ONLY.md guide
             └─ Update README.md

02:35-02:40  Final Verification
             ├─ Run full test suite
             ├─ Check all 2416+ tests pass
             └─ Verify no compiler errors

02:40-02:45  Commit and Push
             ├─ git add -A
             ├─ git commit -m "feat: Complete migration to paths-only addressing"
             └─ git push origin feature/path-addressing

Total: 2 hours 45 minutes (with buffer)
```

---

## 9. Success Metrics

### 9.1 Code Quality Metrics

- [ ] **Zero flat ID usage** in production code (src/)
- [ ] **Zero flat ID usage** in test code (except examples/demos)
- [ ] **Zero imports** of alias-resolver.ts
- [ ] **Simplified address-parser.ts** (no format detection)
- [ ] **Simplified router.ts** (no dual routing logic)

### 9.2 Test Metrics

- [ ] **All existing tests pass** (2416+ pass, 6 known fails)
- [ ] **No new test failures** introduced by migration
- [ ] **Performance tests pass** (no regression)
- [ ] **Path cache tests pass** (99%+ hit rate maintained)

### 9.3 Migration Metrics

```bash
# Command: Verify zero flat IDs
npm run migrate:analyze

# Expected output:
# Total Flat ID Usages:     0
# Migration Progress:       100.00%
```

### 9.4 Documentation Metrics

- [ ] DUAL_ROUTING_MIGRATION.md marked as deprecated
- [ ] PATHS_ONLY.md guide created
- [ ] README.md updated
- [ ] ARCHITECTURE.md updated
- [ ] ROADMAP.md Phase 6 marked complete

---

## 10. Rollback Plan

### 10.1 Git Safety Strategy

We'll create commit points at each major phase:

```
feature/path-addressing
├─ [commit 1] Pre-migration snapshot
├─ [commit 2] Pre-refactor snapshot (mappings ready)
├─ [commit 3] Post-refactor (automated transformation)
├─ [commit 4] Manual cleanup complete
├─ [commit 5] Code removal complete
└─ [commit 6] Final: Migration complete
```

### 10.2 Rollback Commands

**If migration fails partway through:**

```bash
# Scenario 1: Automated refactor failed
git reset --hard HEAD~1  # Revert to pre-refactor snapshot
git status

# Scenario 2: Tests break catastrophically
git reset --hard HEAD~2  # Revert to pre-migration snapshot
git status

# Scenario 3: Need to restart from analysis
git stash  # Save any manual fixes
git reset --hard HEAD~3  # Revert to before mappings
git stash pop  # Restore manual fixes if needed
```

### 10.3 Recovery Procedure

If we need to restart:

1. **Analyze what went wrong:**
   ```bash
   git diff HEAD~1 HEAD > .migration-output/failed-attempt.patch
   cat .migration-output/execution.log | grep "✗"
   ```

2. **Identify problematic files:**
   ```bash
   cat .migration-output/failed-attempt.patch | grep "^diff --git"
   ```

3. **Fix tooling or mappings:**
   - Update path-mappings.json with correct paths
   - Fix refactor.ts if transformation logic is broken
   - Update plan based on errors

4. **Restart from clean state:**
   ```bash
   git reset --hard [last-good-commit]
   npm run migrate:analyze
   # Fix mappings / tooling
   npm run migrate:execute --dry-run
   # Verify, then proceed
   ```

---

## 11. Known Risks and Mitigations

### 11.1 Risk: Breaking Test Suite

**Probability:** Medium
**Impact:** High

**Mitigation:**
- Create commit points before each major phase
- Run tests after each phase
- Use --dry-run before actual execution
- Keep pre-migration snapshot for fast rollback

**Contingency:**
- If >20 tests fail: rollback and analyze
- If <20 tests fail: fix individually
- If tests hang: identify problematic test files and skip/fix

### 11.2 Risk: Missing Flat ID Mappings

**Probability:** Low
**Impact:** Medium

**Mitigation:**
- Comprehensive path-mappings.json with all 42 known IDs
- Refactor tool uses `unmapped/${flatId}` for unknown IDs
- Manual review of unmapped/ paths after execution

**Contingency:**
- Search for `unmapped/` in code: `grep -r "unmapped/" src/`
- Add missing mappings to path-mappings.json
- Re-run refactor tool on affected files

### 11.3 Risk: Dynamic Actor IDs

**Probability:** Low
**Impact:** Low

**Mitigation:**
- Manual review phase catches dynamic patterns
- Search for `spawn.*\${` and `spawn.*\+` patterns
- Document any dynamic ID patterns that need custom handling

**Contingency:**
- Convert dynamic flat IDs to dynamic paths:
  ```typescript
  // BEFORE
  spawn(`task-${id}`, ...)

  // AFTER
  spawn(`domain/tasks/${id}`, ...)
  ```

### 11.4 Risk: Performance Regression

**Probability:** Very Low
**Impact:** Low

**Mitigation:**
- Paths-only mode removes alias resolution overhead
- Expected performance **improvement** (no async lookups)
- Path cache already at 99%+ hit rate

**Contingency:**
- Run performance benchmarks before/after:
  ```bash
  npm test src/messaging/__tests__/path-performance.test.ts
  ```
- If regression detected: investigate router changes

---

## 12. Immediate Next Actions

### First 3 Commands to Start Migration

```bash
# Command 1: Verify clean state and create safety snapshot
git status && git add -A && git commit -m "Pre-migration snapshot: All systems operational"

# Command 2: Create path mappings file
cat > .migration-output/path-mappings.json << 'EOF'
{
  "inference": "services/inference",
  "executor": "services/executor",
  "llm": "services/llm",
  "storage": "services/storage",
  "knowledge": "services/knowledge",
  "tasks": "domain/tasks",
  "relationships": "domain/relationships",
  "orchestrator": "domain/orchestrator",
  "tool-bash": "tools/bash",
  "tool-read": "tools/read",
  "tool-write": "tools/write",
  "test-session": "test/session",
  "test-session-mock": "test/session-mock",
  "mock-session": "test/mock-session",
  "filesystem": "test/filesystem",
  "code-execution": "test/code-execution",
  "streaming-test": "test/streaming",
  "test-actor": "test/actor",
  "non-streaming": "test/non-streaming",
  "calculator": "demo/calculator",
  "echo": "demo/echo",
  "echo-caller": "demo/echo-caller",
  "worker": "demo/worker",
  "logger": "demo/logger",
  "file-checker": "demo/file-checker",
  "file-processor": "demo/file-processor",
  "program-a": "demo/program-a",
  "program-b": "demo/program-b",
  "program-c": "demo/program-c",
  "api-client": "demo/api-client",
  "stress-session": "demo/stress-session",
  "stream-session": "demo/stream-session",
  "file-workflow": "workflows/file",
  "data-pipeline": "workflows/data-pipeline",
  "parallel-executor": "workflows/parallel-executor",
  "report-generator": "workflows/report-generator",
  "error-handler": "workflows/error-handler",
  "failing-program": "workflows/failing-program",
  "test-config": "test/config",
  "test-workflow-config": "test/workflow-config",
  "quick-mock": "test/quick-mock",
  "bench-1k": "bench/1k",
  "bench-10k": "bench/10k",
  "bench-100k": "bench/100k",
  "bench-bp": "bench/backpressure",
  "flat-id": "test/flat-id",
  "id": "test/id"
}
EOF

# Command 3: Verify mappings and run initial analysis
cat .migration-output/path-mappings.json | jq 'keys | length' && npm run migrate:analyze
```

### Expected Output After First 3 Commands

```
# Command 1 output:
On branch feature/path-addressing
nothing to commit, working tree clean
[feature/path-addressing abc1234] Pre-migration snapshot: All systems operational

# Command 2 output:
(mapping file created silently)

# Command 3 output:
42
Analyzing codebase for flat ID usage...
Total Flat ID Usages:     251
Migration Progress:       0.00%
```

---

## 13. Execution Checklist

Use this checklist during execution:

### Pre-Flight Checks
- [ ] On feature/path-addressing branch
- [ ] Clean working directory
- [ ] All tests passing (2416 pass, 6 fail)
- [ ] Migration analysis run and reviewed
- [ ] Pre-migration snapshot committed

### Phase A: Automated Refactoring
- [ ] Path mappings file created (42 mappings)
- [ ] Migration refactor tool updated
- [ ] Dry run completed successfully
- [ ] Pre-refactor snapshot committed
- [ ] Automated migration executed
- [ ] Git diff reviewed for correctness

### Phase B: Manual Cleanup
- [ ] Migration tool self-references preserved/commented
- [ ] Documentation examples updated
- [ ] Dynamic registration patterns handled
- [ ] Manual cleanup committed

### Phase C: Code Removal
- [ ] alias-resolver.ts deleted
- [ ] alias-resolver.test.ts deleted
- [ ] address-parser.ts simplified
- [ ] router.ts dual routing removed
- [ ] Code removal committed

### Phase D: Test Execution
- [ ] Full test suite run
- [ ] Dual routing tests removed/skipped
- [ ] Hard-coded flat ID assertions updated
- [ ] All tests passing (2416+)
- [ ] Performance tests passing
- [ ] Test fixes committed

### Phase E: Documentation
- [ ] DUAL_ROUTING_MIGRATION.md marked deprecated
- [ ] PATHS_ONLY.md created
- [ ] README.md updated
- [ ] ARCHITECTURE.md updated
- [ ] Documentation committed

### Final Verification
- [ ] Migration analyzer shows 0 flat IDs
- [ ] All tests pass (2416+)
- [ ] No compiler errors
- [ ] No import errors for removed files
- [ ] Performance metrics unchanged or improved

### Completion
- [ ] Final commit: "feat: Complete migration to paths-only addressing"
- [ ] Push to origin
- [ ] Create PR for review
- [ ] Update project board

---

## 14. Post-Migration Verification

After completing all phases, run these verification commands:

```bash
# Verification 1: Zero flat IDs
npm run migrate:analyze | grep "Total Flat ID Usages"
# Expected: Total Flat ID Usages: 0

# Verification 2: All tests pass
npm test 2>&1 | tail -10
# Expected: 2416+ pass, 6 fail (known)

# Verification 3: No alias resolver references
grep -r "alias-resolver" src/ --include="*.ts"
# Expected: (no output)

# Verification 4: No flat ID format checks
grep -r "AddressFormat.FLAT_ID" src/ --include="*.ts"
# Expected: (no output)

# Verification 5: No unmapped paths
grep -r "unmapped/" src/ --include="*.ts"
# Expected: (no output)

# Verification 6: Compilation succeeds
npm run build
# Expected: (no errors)

# Verification 7: Performance unchanged
npm test src/messaging/__tests__/path-performance.test.ts
# Expected: All benchmarks pass

# Verification 8: Git status clean
git status
# Expected: On branch feature/path-addressing, nothing to commit
```

---

## 15. Appendix: File-by-File Migration Plan

### Production Files (src/)

| File | Flat IDs | Complexity | Strategy |
|------|----------|------------|----------|
| src/query/compiler.ts | 13 | Simple | Automated refactor |
| src/migration/analyzer.ts | 5 | Complex | Preserve as examples |
| src/migration/refactor.ts | 4 | Complex | Update + preserve examples |
| src/migration/planner.ts | 3 | Simple | Automated refactor |
| src/messaging/actors/task.ts | 4 | Simple | Automated refactor |
| src/messaging/browser/actor-registry.ts | 4 | Simple | Automated refactor |
| src/messaging/browser/widget-actor.ts | 1 | Simple | Automated refactor |
| src/messaging/channels/stream.ts | 2 | Simple | Automated refactor |
| src/messaging/hierarchical-routing-poc.ts | 3 | Simple | Automated refactor |
| src/messaging/router.ts | - | Complex | Manual simplification |
| src/messaging/alias-resolver.ts | - | Complex | **DELETE FILE** |
| src/messaging/address-parser.ts | - | Complex | Manual simplification |

### Demo/Test Files (Automated)

All demo and test files will be processed automatically with the refactor tool. Total: ~19 files, ~220 occurrences.

Key files:
- test-integration.ts (47)
- demo-knowledge-graph.ts (22)
- demo-task-knowledge.ts (17)
- build-pipeline.ts (15)
- demo-actors-only.ts (11)

---

## 16. Questions and Answers

### Q: What if the automated refactor breaks something?

**A:** We have commit points at every phase. Use `git reset --hard HEAD~1` to revert to the last good state. Review the execution log to identify what went wrong, fix the issue, and re-run.

### Q: What about actors spawned at runtime with dynamic IDs?

**A:** The refactor tool handles static string literals. For dynamic IDs like `spawn(\`task-\${id}\`, ...)`, you'll need to manually update them to `spawn(\`domain/tasks/\${id}\`, ...)` in Phase B.

### Q: How do we handle test fixtures with intentional flat IDs?

**A:** Add clear comments like `// EXAMPLE: Flat ID for testing (intentionally preserved)` so future developers understand these are intentional examples, not legacy code.

### Q: What if we discover more flat IDs after migration?

**A:** Run `npm run migrate:analyze` at any time to re-scan. If flat IDs are found, create a path mapping, update the file(s), and re-run analysis to verify.

### Q: How long will this actually take?

**A:** The timeline estimates 2 hours 45 minutes with buffer. With automation, experienced execution could complete in 1.5-2 hours. Budget 3 hours to be safe.

---

## 17. Success Criteria Summary

**Migration is COMPLETE when:**

✅ Migration analyzer reports **0 flat ID usages**
✅ Migration progress shows **100.00%**
✅ All **2416+ tests passing** (6 known failures unchanged)
✅ File `src/messaging/alias-resolver.ts` **deleted**
✅ File `src/messaging/address-parser.ts` **simplified** (no format enum)
✅ File `src/messaging/router.ts` **simplified** (no dual routing)
✅ No imports of **alias-resolver** anywhere in src/
✅ No references to **AddressFormat.FLAT_ID** in src/
✅ No **unmapped/** paths in code
✅ Documentation updated (PATHS_ONLY.md created)
✅ Compilation succeeds with **zero errors**

**Go/No-Go Decision Point:**

Before pushing to origin, ALL criteria above must be met. If any criterion fails, rollback to last good commit and investigate.

---

## 18. Final Notes

This migration represents the completion of **Phase 6: Migration & Integration** of the path-based addressing initiative. Upon successful execution:

1. **Codebase Simplification:** Removes ~400 lines of dual routing code
2. **Performance Improvement:** Eliminates async alias resolution overhead
3. **Developer Experience:** Single canonical addressing format
4. **Architectural Clarity:** Pure hierarchical supervision tree model

The system will be **paths-only** with no backward compatibility layer. This is the intended final state for SEAG's addressing system.

**Ready to execute?** Start with the 3 commands in Section 12.

---

**Document Version:** 1.0
**Last Updated:** 2026-02-06
**Author:** Claude Code (Sonnet 4.5)
**Status:** READY FOR EXECUTION
