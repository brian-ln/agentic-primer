# Session Knowledge Schema Migration Notes

## Current State (2026-02-03)

### Overview
The database schema has evolved through manual alterations. The schema file (`schema-libsql.sql`) now documents the "canonical" MVP schema, while the actual database contains additional legacy fields for backwards compatibility.

### Schema Reconciliation Decision

**Approach Taken: Document canonical schema, keep legacy fields in database**

- Schema file defines the MVP-level fields that are actively used
- Actual database contains additional unused fields from earlier iterations
- CLI tools updated to use only the active MVP fields
- Future migrations can clean up legacy fields if needed

### Table Schemas

#### session_learnings

**Schema File (Canonical MVP):**
```sql
CREATE TABLE session_learnings (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  learning TEXT NOT NULL,
  context TEXT,        -- When/where this was learned
  actionable TEXT,     -- How to apply this learning
  message_id TEXT,     -- Optional: link to specific message
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);
```

**Actual Database (with legacy fields):**
```sql
CREATE TABLE session_learnings (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  learning TEXT NOT NULL,
  context TEXT,
  actionable TEXT,
  message_id TEXT,
  -- Legacy fields (unused by CLI):
  category TEXT,
  evidence TEXT,
  application TEXT,
  confidence REAL DEFAULT 0.0,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);
```

**Active Fields:** id, session_id, timestamp, learning, context, actionable, message_id
**Legacy Fields:** category, evidence, application, confidence

#### session_errors

**Schema File (Canonical MVP):**
```sql
CREATE TABLE session_errors (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  tool_name TEXT,
  error_type TEXT,
  error_message TEXT,
  root_cause TEXT,     -- Why the error occurred
  suggested_fix TEXT,  -- How to fix/prevent it
  message_id TEXT,     -- Optional: link to specific message
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);
```

**Actual Database (with legacy fields):**
```sql
CREATE TABLE session_errors (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  tool_name TEXT,
  error_type TEXT,
  error_message TEXT,
  root_cause TEXT,
  suggested_fix TEXT,
  message_id TEXT,
  -- Legacy fields (unused by CLI):
  resolution TEXT,
  prevention TEXT,
  confidence REAL DEFAULT 0.0,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);
```

**Active Fields:** id, session_id, timestamp, tool_name, error_type, error_message, root_cause, suggested_fix, message_id
**Legacy Fields:** resolution, prevention, confidence

#### session_decisions

**Schema File (Canonical MVP):**
```sql
CREATE TABLE session_decisions (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  decision TEXT NOT NULL,
  reasoning TEXT,
  alternatives TEXT,
  context TEXT,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);
```

**Actual Database (with legacy fields):**
```sql
CREATE TABLE session_decisions (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  decision TEXT NOT NULL,
  reasoning TEXT,
  alternatives TEXT,
  context TEXT,
  -- Legacy fields (unused by CLI):
  message_id TEXT,
  confidence REAL DEFAULT 0.0,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);
```

**Active Fields:** id, session_id, timestamp, decision, reasoning, alternatives, context
**Legacy Fields:** message_id, confidence

### Database Path

The libSQL database is located at: `~/.claude/index/sessions-libsql.db`

All CLI tools now correctly point to this path (updated from `sessions.db`).

### CLI Tools Updated

The following CLI tools have been updated to match the actual database schema:

1. **learnings.ts**
   - Uses: context, actionable, message_id
   - Removed: category, evidence, application, confidence
   - Changed command: `category` → `search` (text-based search)

2. **errors.ts**
   - Uses: root_cause, suggested_fix, message_id
   - Removed: resolution, prevention, confidence

3. **decisions.ts**
   - Uses: context
   - Removed: confidence (message_id exists in DB but not exposed in CLI yet)

### Test Results

All CLI tools verified working:

```bash
# Learnings
bun run src/session-knowledge/cli/learnings.ts recent 2
✅ Shows 2 recent learnings with context/actionable fields

# Errors
bun run src/session-knowledge/cli/errors.ts recent 1
✅ Shows 1 recent error with root_cause/suggested_fix fields

# Decisions
bun run src/session-knowledge/cli/decisions.ts recent 2
✅ Shows 2 recent decisions with context field
```

### Future Considerations

#### Option 1: Keep Legacy Fields (Current Approach)
- **Pros:** No data migration needed, backwards compatible
- **Cons:** Schema drift between docs and reality
- **Best for:** MVP and rapid iteration

#### Option 2: Clean Migration
Create a migration script to:
1. Drop unused columns (requires SQLite ALTER TABLE or recreate)
2. Migrate any data if needed
3. Update schema to match exactly

**Migration SQL (if needed in future):**
```sql
-- Example: Remove legacy fields from session_learnings
-- Note: SQLite doesn't support DROP COLUMN, requires recreate

BEGIN TRANSACTION;

-- Create new table with clean schema
CREATE TABLE session_learnings_new (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  learning TEXT NOT NULL,
  context TEXT,
  actionable TEXT,
  message_id TEXT,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

-- Copy data
INSERT INTO session_learnings_new
SELECT id, session_id, timestamp, learning, context, actionable, message_id
FROM session_learnings;

-- Swap tables
DROP TABLE session_learnings;
ALTER TABLE session_learnings_new RENAME TO session_learnings;

-- Recreate indexes
CREATE INDEX idx_learnings_session ON session_learnings(session_id);
CREATE INDEX idx_learnings_timestamp ON session_learnings(timestamp);

COMMIT;
```

### Recommendations

For MVP phase:
1. ✅ Keep current approach (legacy fields remain but unused)
2. ✅ Document canonical schema in schema file
3. ✅ Ensure CLI tools use only canonical fields
4. ⏭️ Consider clean migration before v1.0 release

For Production:
- Run clean migration script
- Ensure schema file exactly matches database
- Add schema versioning and migration framework
