# Migration Tests - Quick Start Guide

## Running Tests

```bash
# Run all migration tests
bun test src/session-knowledge/__tests__/migrations.test.ts

# Run specific test suite
bun test src/session-knowledge/__tests__/migrations.test.ts -t "Version Detection"

# Run with verbose output
bun test src/session-knowledge/__tests__/migrations.test.ts --verbose
```

## What Gets Tested

| Test Suite | Tests | Purpose |
|------------|-------|---------|
| Version Detection | 4 | Prevents re-running migrations |
| Schema Changes | 5 | Ensures safe column addition |
| Data Backfill | 4 | Validates existing records updated correctly |
| Index Creation | 3 | Checks performance not degraded |
| New Tables | 3 | Verifies new tables created properly |
| Rollback | 2 | Tests backup/restore capability |
| Idempotency | 3 | Ensures migrations are rerunnable |
| Concurrent Access | 3 | Validates DB accessible during migration |
| Data Integrity | 3 | Confirms no data corruption |
| Error Handling | 3 | Tests edge cases and failures |

**Total: 33 tests, 73+ assertions**

## Test Database

- **Location**: `~/.claude/index/test/migrations-test.db`
- **Isolation**: Completely separate from production DB
- **Cleanup**: Automatic after tests complete
- **Safety**: Production data never touched

## Before Writing a Migration

1. Study existing migration: `src/session-knowledge/migrations/add-cognitive-features.ts`
2. Plan your schema changes (document them!)
3. Write tests first (TDD approach)
4. Implement migration
5. Run tests: `bun test migrations.test.ts`

## Migration Checklist

- [ ] Version check (prevent re-runs)
- [ ] IF NOT EXISTS on CREATE TABLE
- [ ] IF NOT EXISTS on CREATE INDEX
- [ ] ALTER TABLE for column additions (not DROP/MODIFY)
- [ ] Backfill existing data
- [ ] Update version metadata
- [ ] Test on empty database
- [ ] Test on populated database
- [ ] Test idempotency (run twice)
- [ ] Measure performance impact

## Common Test Patterns

### Testing Schema Changes
```typescript
test('should add new column safely', async () => {
  await applyMigration(client);

  const pragma = await client.execute('PRAGMA table_info(table_name)');
  const columns = pragma.rows.map(r => r.name);

  expect(columns).toContain('new_column');
  expect(columns).toContain('existing_column'); // Still there!
});
```

### Testing Data Backfill
```typescript
test('should backfill with defaults', async () => {
  await insertSampleData(client);
  await applyMigration(client);

  const result = await client.execute('SELECT * FROM table WHERE id = ?', ['test-id']);
  const row = result.rows[0];

  expect(row.new_column).toBe(expectedDefault);
  expect(row.old_data).toBe(originalValue); // Unchanged!
});
```

### Testing Idempotency
```typescript
test('should handle re-run safely', async () => {
  await applyMigration(client);
  await expect(applyMigration(client)).rejects.toThrow('already applied');
});
```

## Key Safety Rules

1. **Never DROP columns** (SQLite doesn't support it easily)
2. **Never MODIFY columns** (requires recreate in SQLite)
3. **Always ADD columns** (safe and reversible via backup)
4. **Always check version** before applying migration
5. **Always backfill** new columns on existing data
6. **Always create backup** before production migration

## Debugging Failed Tests

```bash
# Check test database
sqlite3 ~/.claude/index/test/migrations-test.db

# View schema
.schema session_decisions

# Check data
SELECT * FROM session_decisions LIMIT 5;

# Check metadata
SELECT * FROM index_metadata WHERE key LIKE '%version%';
```

## Real-World Usage

```bash
# Backup production DB first!
cp ~/.claude/index/sessions-libsql.db ~/.claude/index/sessions-libsql.db.backup

# Run migration
bun run src/session-knowledge/migrations/add-cognitive-features.ts

# Verify
sqlite3 ~/.claude/index/sessions-libsql.db
> SELECT value FROM index_metadata WHERE key = 'cognitive_schema_version';
> .quit

# If something went wrong, restore:
# cp ~/.claude/index/sessions-libsql.db.backup ~/.claude/index/sessions-libsql.db
```

## Test Output Guide

```
✓ 33 pass      ← All tests passed
  0 fail       ← No failures
  73 expect()  ← 73 assertions verified
  [416ms]      ← Fast execution (<1 second)
```

## When Tests Fail

1. **Read error message carefully** - usually tells you what's wrong
2. **Check version metadata** - might be version conflict
3. **Verify test database cleanup** - stale data can cause issues
4. **Run single test** to isolate problem:
   ```bash
   bun test migrations.test.ts -t "specific test name"
   ```

## Adding New Tests

1. Add to appropriate `describe()` block
2. Use `beforeEach()` to setup clean state
3. Always cleanup with `client.close()` in tests that create clients
4. Follow existing test patterns for consistency

## Performance Expectations

- Individual test: < 20ms
- Full suite: < 500ms
- If slower: investigate database locking or inefficient queries

## Questions?

- Check test file: `migrations.test.ts` (heavily commented)
- Check summary doc: `MIGRATION-TEST-SUMMARY.md` (detailed explanations)
- Check actual migration: `../migrations/add-cognitive-features.ts` (reference implementation)
