# Migration Output Archive

This directory contains the output from the flat ID → hierarchical path migration
completed on 2026-02-06.

## Status: COMPLETE (100%)

All actors migrated from flat IDs to hierarchical paths. Remaining 6 flat IDs
are intentional examples in migration tooling.

## Files

- `analysis-report.txt` - Final analysis showing 100% completion
- `analysis.json` - Detailed analysis data (6 flat IDs in tool examples)
- `path-mappings.json` - 47 flat ID → path mappings used
- `session-*.json` - Complete migration session logs (2.7MB each)
- `baseline-tests.log` - Pre-migration test results
- `final-test-results.log` - Post-migration test results
- `dry-run.log` - Migration dry-run output
- `plan-report.txt` - Migration plan summary
- `plan.json` - Detailed migration plan

## Migration Summary

**Started:** 2026-02-06
**Completed:** 2026-02-06
**Duration:** ~2 hours

**Results:**
- 251 flat ID usages → 6 remaining (examples only)
- 41 actors migrated to hierarchical paths
- 100% migration completion
- Dual routing removed
- Alias resolver deleted
- Paths-only mode active

## Historical Reference

See `/MIGRATION_IMPLEMENTATION_SUMMARY.md` for complete migration report.
See `/MIGRATION_CLEANUP_REPORT.md` for cleanup actions and decisions.

## Preservation

This archive is preserved to document:
- What was changed during migration
- The mapping strategy used
- Before/after test results
- Complete audit trail of changes
