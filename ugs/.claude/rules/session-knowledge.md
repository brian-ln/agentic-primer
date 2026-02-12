---
name: session-knowledge
description: Instructions for working with the session knowledge system
---

# Session Knowledge System - Agent Instructions

## Overview
This project has a semantic knowledge extraction system that learns from Claude Code sessions. It extracts decisions, learnings, and errors automatically.

## When to Use

### Extract Knowledge (After Significant Work)
When you complete substantial work in a session (bug fixes, feature implementation, architecture decisions):
```bash
/extract-knowledge <session-id>
```

### Query Before Starting Similar Work
Before making architectural decisions or implementing features, check if similar work was done:
```bash
/decisions recent 20        # Recent architecture/tech decisions
/learnings category technical  # Technical insights
/errors type <ErrorType>    # Known error patterns
```

### Learn from Errors
When encountering an error, check if it's been solved before:
```bash
/errors recent 10
/errors type NetworkError
/errors tools Bash
```

## Common Patterns

### Starting a Feature
1. `/decisions recent` - Check recent architectural choices
2. `/learnings category architectural` - Review design patterns used
3. Implement with consistency to existing decisions

### Debugging an Error
1. `/errors type <error-category>` - Check for known solutions
2. If found: Apply the documented resolution
3. After fixing: Extract knowledge so future sessions benefit

### End of Session
If you made decisions, discovered insights, or solved errors:
```bash
/extract-knowledge <current-session-id>
```

## CLI Commands (via ./know)

All commands support `--json` for machine-readable output:

```bash
# Prototype Management
./know prototypes              # List all category prototypes
./know prototypes list         # Same as above
./know prototypes delete decision  # Delete specific prototype
./know prototypes clear        # Delete all prototypes

# Knowledge Extraction
./know extract <session-id>    # Extract from specific session
./know extract all             # Process all unprocessed sessions
./know extract today           # Process today's sessions
./know extract yesterday       # Process yesterday's sessions

# Query Decisions
./know decisions today
./know decisions yesterday
./know decisions recent [N]    # Default: 10
./know decisions session <id>
./know decisions range <start-date> <end-date>

# Query Learnings
./know learnings today
./know learnings yesterday
./know learnings recent [N]
./know learnings session <id>
./know learnings category <name>  # technical, architectural, tooling, process
./know learnings categories       # List all categories with counts
./know learnings range <start-date> <end-date>

# Query Errors
./know errors today
./know errors yesterday
./know errors recent [N]
./know errors session <id>
./know errors type <error-type>  # TypeError, NetworkError, etc.
./know errors types              # List all error types with counts
./know errors tools              # Group errors by tool that generated them
./know errors range <start-date> <end-date>

# Session & Stats
./know sessions [today|yesterday|recent [N]|range <start> <end>]
./know stats                     # System statistics
```

## Decision Tree for Agents

```
Is this a new implementation task?
├─ YES: Query /decisions and /learnings first
│       └─ Implement consistently with prior art
└─ NO: Continue

Did I encounter an error?
├─ YES: Query /errors for known solutions
│       ├─ Found: Apply documented resolution
│       └─ Not found: Solve and document
└─ NO: Continue

Did I make architectural decisions?
├─ YES: Note for extraction at session end
└─ NO: Continue

Did I discover non-obvious insights?
├─ YES: Note for extraction at session end
└─ NO: Continue

End of session AND made decisions/learnings/fixed errors?
├─ YES: /extract-knowledge <session-id>
└─ NO: Done
```

## Cost & Performance

**Per Session (200 messages, 26% candidates):**
- Stage 1 (embedding similarity): Fast, filters 90% of messages
- Stage 2 (LLM classification): ~$0.004 with Cloudflare Workers AI
- **Total: ~$0.024/session**

**Cloudflare Free Tier:** 10,000 neurons/day ≈ 400 sessions

## Technical Details

**Two-Stage Pipeline:**
1. **Candidate Detection**: Embedding cosine similarity (threshold: 0.65)
2. **LLM Classification**: @cf/meta/llama-3.2-3b-instruct extracts structured metadata

**Prerequisites:**
- Message embeddings must exist (run SessionEmbeddingIndexerLibSQL.ts first)
- Prototype embeddings must be generated (/generate-prototypes)
- Cloudflare credentials in .env (CLOUDFLARE_ACCOUNT_ID, CLOUDFLARE_GATEWAY_ID, CLOUDFLARE_API_TOKEN)

## Agent-Friendly Features

**JSON Output:**
All commands support `--json` flag or auto-detect piped output:
```bash
./know decisions recent 10 --json | jq '.results[] | .decision'
```

**Machine-Readable Structure:**
```json
{
  "command": "decisions",
  "filter": "recent",
  "count": 3,
  "results": [{
    "id": "dec_abc123",
    "session_id": "f03b3b54...",
    "timestamp": "2026-01-30T12:34:56",
    "decision": "Chose libSQL over sqlite-vec",
    "reasoning": "Cleaner implementation, no extension loading",
    "alternatives": "sqlite-vec (30KB), pgvector (separate DB)",
    "confidence": 0.92
  }]
}
```

## Files & Architecture

**Core Components:**
- `src/session-knowledge/classification/` - Two-stage classification pipeline
- `src/session-knowledge/extraction/` - Batch processing orchestration
- `src/session-knowledge/cli.ts` - Unified CLI entry point
- `src/session-knowledge/index/schema-libsql.sql` - Database schema

**Skills (/ commands):**
- `/generate-prototypes` - Setup prototype embeddings
- `/extract-knowledge` - Extract from sessions
- `/decisions` - Query decisions
- `/learnings` - Query learnings
- `/errors` - Query errors

## Examples

**Before implementing authentication:**
```bash
./know decisions recent 20
./know learnings category architectural
# Discover: Previous session chose JWT over sessions
# Implement: Follow established pattern
```

**After fixing network timeout:**
```bash
# At end of session
/extract-knowledge <current-session-id>
# Future sessions can query:
# ./know errors type NetworkError
```

**Exploring codebase patterns:**
```bash
./know learnings category technical
./know decisions session <id>  # Deep dive into specific session
```
