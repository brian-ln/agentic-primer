# Session Knowledge System - Quick Reference

## Database Location
```
~/.claude/index/sessions-libsql.db
```

## CLI Tools

### Learnings
```bash
bun run src/session-knowledge/cli/learnings.ts recent 10
bun run src/session-knowledge/cli/learnings.ts today
bun run src/session-knowledge/cli/learnings.ts search "vector"
```

### Errors
```bash
bun run src/session-knowledge/cli/errors.ts recent 10
bun run src/session-knowledge/cli/errors.ts today
bun run src/session-knowledge/cli/errors.ts types
```

### Decisions
```bash
bun run src/session-knowledge/cli/decisions.ts recent 10
bun run src/session-knowledge/cli/decisions.ts today
```

## Current Schema (MVP)

### session_learnings
| Field | Type | Description |
|-------|------|-------------|
| id | TEXT | Primary key |
| session_id | TEXT | Foreign key to sessions |
| timestamp | INTEGER | Unix timestamp |
| learning | TEXT | The learning content |
| context | TEXT | When/where learned |
| actionable | TEXT | How to apply it |
| message_id | TEXT | Optional link to message |

### session_errors
| Field | Type | Description |
|-------|------|-------------|
| id | TEXT | Primary key |
| session_id | TEXT | Foreign key to sessions |
| timestamp | INTEGER | Unix timestamp |
| tool_name | TEXT | Tool that caused error |
| error_type | TEXT | Error category |
| error_message | TEXT | Error description |
| root_cause | TEXT | Why it occurred |
| suggested_fix | TEXT | How to fix/prevent |
| message_id | TEXT | Optional link to message |

### session_decisions
| Field | Type | Description |
|-------|------|-------------|
| id | TEXT | Primary key |
| session_id | TEXT | Foreign key to sessions |
| timestamp | INTEGER | Unix timestamp |
| decision | TEXT | The decision made |
| reasoning | TEXT | Why this was chosen |
| alternatives | TEXT | Other options considered |
| context | TEXT | Situation context |

## Manual Data Entry

### Add Learning
```sql
INSERT INTO session_learnings (id, session_id, timestamp, learning, context, actionable)
VALUES (
  'lea_' || strftime('%s', 'now') || '_' || substr(hex(randomblob(4)), 1, 7),
  'manual',
  strftime('%s', 'now') * 1000,
  'Your learning here',
  'Context about when/where',
  'How to apply this'
);
```

### Add Error
```sql
INSERT INTO session_errors (id, session_id, timestamp, error_type, error_message, root_cause, suggested_fix)
VALUES (
  'err_' || strftime('%s', 'now') || '_' || substr(hex(randomblob(4)), 1, 7),
  'manual',
  strftime('%s', 'now') * 1000,
  'ErrorType',
  'Error description',
  'Why it happened',
  'How to fix it'
);
```

### Add Decision
```sql
INSERT INTO session_decisions (id, session_id, timestamp, decision, reasoning, alternatives, context)
VALUES (
  'dec_' || strftime('%s', 'now') || '_' || substr(hex(randomblob(4)), 1, 7),
  'manual',
  strftime('%s', 'now') * 1000,
  'Decision made',
  'Why this was chosen',
  'Other options',
  'Situation context'
);
```

## Schema Documentation

- **Main Schema:** `src/session-knowledge/index/schema-libsql.sql`
- **Migration Notes:** `src/session-knowledge/SCHEMA-MIGRATION.md`
- **Fix Summary:** `src/session-knowledge/SCHEMA-FIX-SUMMARY.md`

## Status
✅ Schema file matches database (active fields)
✅ CLI tools working correctly
✅ Manual data entry working
✅ Ready for MVP use
