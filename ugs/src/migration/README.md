# Migration Tooling

Automated migration tooling for transitioning from flat ID addressing to hierarchical path-based addressing.

## Quick Start

```bash
# 1. Analyze codebase
npm run migrate:analyze

# 2. Generate migration plan
npm run migrate:plan

# 3. Execute migration (dry-run first!)
npm run migrate:execute:dry

# 4. Execute for real
npm run migrate:execute

# 5. Verify progress
npm run migrate:verify
```

## Components

### Analyzer
Scans codebase for flat ID usage patterns.

**Files:** `analyzer.ts`

**Detects:**
- `address('flat-id')` calls
- Actor registrations (`router.registerActor`, `supervisor.addChild`)
- Alias registrations
- Categorizes by complexity

### Planner
Generates step-by-step migration plans.

**Files:** `planner.ts`

**Features:**
- Path proposals based on heuristics
- Dependency analysis
- Migration phases
- Effort estimation
- Risk assessment

### Refactor
Safe code transformations.

**Files:** `refactor.ts`

**Features:**
- AST-based replacements
- Dry-run mode
- Automatic backups
- Rollback support
- Session tracking

### Reporter
Progress tracking and visualization.

**Files:** `reporter.ts`

**Features:**
- Progress metrics
- Phase tracking
- Complexity heat maps
- Formatted reports

### CLI
Command-line interface.

**Files:** `cli.ts`

**Commands:**
- `analyze` - Scan codebase
- `plan` - Generate plan
- `execute` - Apply changes
- `report` - Show progress
- `verify` - Check results

## Example Output

### Analysis Report

```text
═══════════════════════════════════════════════════════
  Migration Analysis Report
═══════════════════════════════════════════════════════

Files Scanned: 42
Total Flat ID Usages: 127
Migration Progress: 21.74%

BY COMPLEXITY
───────────────────────────────────────────────────────
  Simple:     89 (quick fixes)
  Moderate:   26 (requires review)
  Complex:    12 (structural changes)
```

### Migration Plan

```text
═══════════════════════════════════════════════════════
  Migration Plan
═══════════════════════════════════════════════════════

Total Steps: 72
Estimated Effort: 6.3 hours

PATH PROPOSALS
───────────────────────────────────────────────────────
  ✓ inference → domain/inference
  ✓ llm-service → services/llm-service
  ? my-actor → domain/my-actor (review needed)
```

### Progress Report

```text
═══════════════════════════════════════════════════════
  Migration Progress Report
═══════════════════════════════════════════════════════

Overall Progress: [███████████████░░░░░░░░░░░░░░░] 52.2%

PHASE PROGRESS
───────────────────────────────────────────────────────
  ✓ Phase 1: Create Aliases [100%]
  ✓ Phase 2: Update Registrations [100%]
  ○ Phase 3: Update Usages [52%]
```

## Testing

```bash
bun test src/migration/__tests__/migration.test.ts
```

**Coverage:**
- 20 tests
- Analyzer, planner, refactor, reporter
- Integration workflow

## Documentation

Full documentation: [`docs/MIGRATION_TOOLING.md`](../../docs/MIGRATION_TOOLING.md)

## Architecture

```text
analyzer.ts    - Codebase analysis (detects flat IDs)
planner.ts     - Migration planning (generates steps)
refactor.ts    - Code transformations (applies changes)
reporter.ts    - Progress tracking (metrics & reports)
cli.ts         - CLI interface (commands & workflow)
```

## Success Metrics

✅ Analyzer detects all flat ID usages in codebase
✅ Planner creates actionable migration steps
✅ Refactor safely transforms flat IDs → paths
✅ Reporter shows progress metrics
✅ CLI integrates with existing workflow
✅ 20 tests for migration utilities
✅ Documentation with usage examples

## Related

- **Dual Routing:** [src/messaging/router.ts](../messaging/router.ts)
- **Address Parser:** [src/messaging/address-parser.ts](../messaging/address-parser.ts)
- **Alias Resolver:** [src/messaging/alias-resolver.ts](../messaging/alias-resolver.ts)
