# Session Knowledge System - Agent Cheat Sheet

**Quick reference for Claude Code agents using the session knowledge system**

## When to Query Knowledge (Decision Tree)

```
START
  │
  ├─► Implementing a feature?
  │     ├─► Query /decisions recent 20
  │     ├─► Query /learnings category architectural
  │     └─► Implement consistently with past decisions
  │
  ├─► Encountering an error?
  │     ├─► Query /errors type <ErrorType>
  │     ├─► Found solution? → Apply documented resolution
  │     └─► No solution? → Solve, then extract knowledge
  │
  ├─► Making architectural decision?
  │     ├─► Query /decisions recent
  │     └─► Note decision for extraction at session end
  │
  ├─► Discovered non-obvious insight?
  │     └─► Note learning for extraction at session end
  │
  └─► End of session with new knowledge?
        └─► /extract-knowledge <session-id>
```

## Quick Command Reference

### Most Common Commands

```bash
# Before implementing: Check past decisions
/decisions recent 20
/learnings category technical

# During debugging: Check for known errors
/errors type NetworkError
/errors recent 10

# After major work: Extract knowledge
/extract-knowledge <session-id>

# Browse knowledge
/learnings categories         # See what we've learned
/errors types                 # See error patterns
/decisions today              # Recent architecture choices
```

### All Commands (via ./know or / skills)

| Command | Quick Usage | Purpose |
|---------|-------------|---------|
| `/generate-prototypes` | One-time setup | Create category embeddings |
| `/extract-knowledge <id>` | After major work | Extract decisions/learnings/errors |
| `/decisions recent 20` | Before implementing | Check architectural consistency |
| `/learnings category <type>` | Explore codebase patterns | Find proven approaches |
| `/errors type <ErrorType>` | When debugging | Find known solutions |

### Query Filters

**Time-based:**
```bash
/decisions today          # Just today's decisions
/decisions yesterday      # Yesterday only
/decisions recent 20      # Most recent N items
```

**Session-based:**
```bash
/decisions session f03b3b54-ca47-46d3-be1f-20ccfc82f9de
/learnings session <id>
/errors session <id>
```

**Category/Type filtering:**
```bash
/learnings category technical    # technical, architectural, tooling, process
/errors type NetworkError        # TypeError, NetworkError, DatabaseError, etc.
```

**List aggregates:**
```bash
/learnings categories    # Show all categories with counts
/errors types           # Show all error types with counts
/errors tools           # Group by tool that generated error
```

## Output Modes

### Human-Friendly (Default in Terminal)
```bash
./know decisions recent 5

📋 Decisions (3 found)

[12:34:56] Chose libSQL over sqlite-vec
  Reasoning: Cleaner implementation, no extension loading
  Alternatives: sqlite-vec (30KB), pgvector (separate DB)
  Confidence: 92%
  Session: f03b3b54...
```

### Machine-Readable (Auto-detect piped or --json flag)
```bash
./know decisions recent 5 --json
# or
./know decisions recent 5 | jq '.results[] | .decision'

{
  "command": "decisions",
  "filter": "recent",
  "count": 3,
  "results": [
    {
      "id": "dec_abc123",
      "session_id": "f03b3b54...",
      "timestamp": "2026-01-30T12:34:56",
      "decision": "Chose libSQL over sqlite-vec",
      "reasoning": "Cleaner implementation...",
      "alternatives": "sqlite-vec (30KB)...",
      "confidence": 0.92
    }
  ]
}
```

## Typical Agent Workflows

### 1. Feature Implementation

```bash
# BEFORE coding
./know decisions recent 20             # Check architectural patterns
./know learnings category architectural # Review design approaches
./know learnings category technical     # Check implementation patterns

# Code implementation...

# AFTER coding (if made decisions/discoveries)
/extract-knowledge <session-id>
```

### 2. Bug Fixing

```bash
# WHEN error occurs
./know errors type <ErrorType>         # Check for known solutions
./know errors tools Bash               # If tool-specific error

# If solution found: Apply it
# If not found: Debug and solve

# AFTER fix
/extract-knowledge <session-id>        # Document for future
```

### 3. Exploring Codebase

```bash
./know learnings categories            # What has been learned?
./know learnings category technical    # Technical insights
./know decisions recent 30             # Recent architecture choices
./know sessions recent 10              # What sessions exist?
```

### 4. End of Session Checklist

```
Did I:
  □ Make architectural/technical decisions?
  □ Discover non-obvious insights?
  □ Fix errors that should be documented?

If YES to any:
  /extract-knowledge <current-session-id>
```

## Progressive Context Enrichment

The system provides progressive detail:

1. **List view** (default): High-level summary
   ```bash
   ./know decisions recent
   → Shows: decision, reasoning, confidence
   ```

2. **JSON view** (--json): Full structured data
   ```bash
   ./know decisions recent --json
   → Shows: All fields including IDs, timestamps, embeddings
   ```

3. **Session deep-dive**: All knowledge from one session
   ```bash
   ./know decisions session <id>
   ./know learnings session <id>
   ./know errors session <id>
   ```

## Field Reference

### Decisions
- **decision**: What was chosen
- **reasoning**: Why it was chosen
- **alternatives**: What else was considered
- **context**: Additional metadata
- **confidence**: 0.0-1.0 classification confidence

### Learnings
- **insight**: Key discovery
- **category**: technical | architectural | tooling | process | performance
- **evidence**: Supporting context
- **application**: How to apply this
- **confidence**: 0.0-1.0 classification confidence

### Errors
- **type**: TypeError | NetworkError | ImportError | DatabaseError | ValidationError | AuthError
- **message**: Error description
- **tool**: Tool that generated it (Bash, Read, etc.)
- **resolution**: How it was fixed
- **prevention**: How to avoid in future
- **confidence**: 0.0-1.0 classification confidence

## Cost & Performance

**Per Command:**
- Query operations: Free, instant (indexed database)
- Extraction: ~$0.024 per session (200 messages)
- Prototype generation: One-time, ~$0.02

**Cloudflare Free Tier:**
- 10,000 neurons/day
- ≈ 400 sessions extractable per day

## Troubleshooting

| Issue | Solution |
|-------|----------|
| "No embeddings found" | Run message embeddings first |
| "No prototypes found" | Run `/generate-prototypes` |
| 0 extractions | Normal if session has no decisions/learnings/errors |
| Confidence < 0.5 | Low-quality extraction, may be false positive |
| Missing Cloudflare creds | Set CLOUDFLARE_ACCOUNT_ID, CLOUDFLARE_GATEWAY_ID, CLOUDFLARE_API_TOKEN |

## Architecture Quick Facts

**Two-Stage Pipeline:**
1. **Stage 1**: Embedding similarity (0.65 threshold) → Filters 90% of messages
2. **Stage 2**: LLM classification (@cf/meta/llama-3.2-3b-instruct) → Extracts structured data

**Model:** Cloudflare Workers AI (llama-3.2-3b-instruct)
- Input: $0.08/M tokens
- Output: $0.27/M tokens
- Accuracy: ~85% precision/recall

**Storage:** libSQL with native F32_BLOB(768) vectors
- DiskANN indexing for fast similarity search
- Full-text search on all fields

## Integration with Claude Code

**Skills (.claude/skills/):**
- `/generate-prototypes` → Setup
- `/extract-knowledge` → Extract from sessions
- `/decisions` → Query decisions
- `/learnings` → Query learnings
- `/errors` → Query errors

**Custom Rules (.clinerules):**
- Auto-loaded by Claude Code
- Provides usage patterns
- Decision tree logic

**CLI (./know):**
- Unified entry point
- Self-documenting (--help)
- Agent-friendly (--json, auto-detect)

## Best Practices for Agents

1. **Query before implementing**: Check `/decisions` and `/learnings` first
2. **Document errors**: Always extract after fixing novel errors
3. **Be consistent**: Follow established architectural patterns
4. **Filter by confidence**: Ignore results with confidence < 0.5
5. **Use JSON mode**: For programmatic processing
6. **Check categories**: Use `/learnings categories` to discover what's been learned
7. **Session context**: Use `session <id>` filter for deep-dive into specific work

## Example Agent Reasoning

```
User: "Add user authentication"

Agent thought process:
1. Check past decisions about auth:
   ./know decisions recent 20 | grep -i auth

2. Check architectural patterns:
   ./know learnings category architectural

3. Found: Previous session chose JWT over sessions
   Reasoning: Stateless, works with mobile

4. Implement JWT auth following established pattern

5. After implementation:
   /extract-knowledge <current-session-id>
   (Extracts: "Implemented JWT auth with refresh tokens")
```

## Help System

```bash
# Main help
./know --help

# Command-specific help
./know decisions --help
./know extract --help
./know learnings --help
./know errors --help
./know sessions --help
./know stats --help
./know prototypes --help
```

## Related Documentation

- **Full Guide**: `docs/planning/session-knowledge-implementation.md`
- **Schema**: `src/session-knowledge/index/schema-libsql.sql`
- **Skills**: `.claude/skills/*/SKILL.md`
- **Agent Rules**: `.clinerules`

---

**Last Updated**: 2026-01-30
**System Version**: Phase 4 - Semantic Classification
**Model**: @cf/meta/llama-3.2-3b-instruct
