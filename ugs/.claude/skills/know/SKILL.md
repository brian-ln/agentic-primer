---
name: know
description: Capture and query organizational knowledge. Use to extract decisions/learnings/errors after completing work ("extract knowledge from this session"), or query accumulated insights before starting work ("what did we decide about X?", "what have we learned about Y?", "any known solutions for Z?"). Builds searchable memory of architectural choices, technical discoveries, and error resolutions.
disable-model-invocation: false
user-invocable: true
argument-hint: "<command> [args]"
allowed-tools:
  - Bash
---

# Organizational Knowledge System

Build and query a searchable knowledge base of architectural decisions, technical learnings, and error solutions accumulated across all your Claude Code work. Extracts semantic insights from sessions and stores them for future reference.

## When to Use This Skill

**Use `/know` when the user asks about:**
- Past decisions or architecture choices
- What we've learned before
- Known errors or solutions
- Extracting knowledge after completing work
- Exploring what knowledge exists in the system

**Natural language examples that should trigger this skill:**
- "What have we decided about authentication?"
- "Show me recent architectural decisions"
- "Any learnings about error handling?"
- "What errors have we seen with NetworkError?"
- "Extract knowledge from this session"
- "What do we know about API design?"

## Commands

The skill interprets natural language and maps to knowledge commands:

### Manual Entry Commands
```bash
/know add decision "<text>"  # Manually add a decision
/know add learning "<text>"  # Manually add a learning
/know add error "<text>"     # Manually add an error
```

### Query Commands
```bash
/know decisions [filter]     # Query architectural/technical decisions
/know learnings [filter]     # Query insights and discoveries
/know errors [filter]        # Query errors and their resolutions
/know workflows [filter]     # Query work organization patterns
/know discover              # See what knowledge exists
/know stats                 # System statistics
```

### Extraction Commands
```bash
/know extract <session-id>   # Extract from specific session
/know extract all           # Process all sessions
/know extract today         # Process today's sessions
```

### Setup Commands
```bash
/know prototypes            # Manage category embeddings
```

## Filters

All query commands support:
- `today`, `yesterday` - Time-based
- `recent [N]` - Most recent N items (default: 10)
- `session <id>` - From specific session
- `range <start> <end>` - Date range (YYYY-MM-DD)

**Learnings-specific:**
- `category <name>` - Filter by category (technical, architectural, tooling, process)
- `categories` - List all categories

**Errors-specific:**
- `type <ErrorType>` - Filter by error type (NetworkError, TypeError, etc.)
- `types` - List all error types
- `tools` - Group by tool that generated error

**Workflows-specific:**
- `type <type>` - Filter by workflow type (delegation, organization, planning, collaboration, tooling)
- `types` - List all workflow types
- `effective` - Show most effective workflows

## Task

Parse the user's request and execute the appropriate knowledge command.

If the request is literal (matches command syntax):
```bash
./know $ARGUMENTS
```

If the request is natural language, interpret intent and map to command:

**User says:** "$ARGUMENTS"

**Examples:**

Manual entry (when user wants to declaratively add knowledge):
- "I only ever program in bun.js" → `./know add decision "Always use bun.js for JavaScript" --reasoning "User preference"`
- "remember that we use JWT for auth" → `./know add decision "Use JWT authentication" --context "Team convention"`
- "note: prefer functional components" → `./know add learning "Prefer functional components over class components"`

Query (when user asks about past knowledge):
- "recent decisions" → `./know decisions recent 10`
- "what did we learn about APIs?" → `./know learnings recent 20` (then explain results)
- "show me today's errors" → `./know errors today`
- "any NetworkError solutions?" → `./know errors type NetworkError`
- "what workflows worked well?" → `./know workflows effective`
- "show me delegation patterns" → `./know workflows type delegation`
- "what knowledge do we have?" → `./know discover`

Extraction (when user wants to capture from sessions):
- "extract knowledge from session abc123" → `./know extract abc123`
- "extract from this session" → `./know extract <current-session-id>`
- "process today's sessions" → `./know extract today`

## Output

The CLI supports both human-friendly and JSON output:
- Human-friendly by default in terminal
- JSON when piped or `--json` flag used

Results include:
- **Decisions**: Choice, reasoning, alternatives, confidence
- **Learnings**: Insight, category, evidence, application, confidence
- **Errors**: Type, message, resolution, prevention, confidence
- **Workflows**: Description, type, effectiveness, tools, outcome, lessons, confidence
- **All**: Session ID, timestamp, message ID

## Prerequisites

Before querying, knowledge must be extracted:
```bash
/know extract <session-id>
```

Initial setup (one-time):
```bash
/know prototypes
```

## Examples

**Manually add knowledge (preferences, conventions, external decisions):**
```bash
# Add a preference or convention
/know add decision "Always use bun.js for JavaScript runtime" \\
  --reasoning "Faster than Node, TypeScript native" \\
  --alternatives "Node.js (slower), Deno (less ecosystem)"

# Add a discovered insight
/know add learning "libSQL has native vector support" \\
  --context "Evaluated vector databases" \\
  --actionable "Use F32_BLOB(768) for embeddings"

# Add a known error solution
/know add error "Import path resolution fails in Bun" \\
  --type "ImportError" \\
  --root-cause "Bun requires explicit extensions" \\
  --suggested-fix "Add .ts/.js to imports"
```

**Before implementing a feature:**
```bash
/know decisions recent 20
/know learnings category architectural
```

**When debugging an error:**
```bash
/know errors type NetworkError
/know errors recent 10
```

**After completing significant work:**
```bash
/know extract <current-session-id>
```

**Exploring what's available:**
```bash
/know discover
/know stats
```

## Related

- Full CLI docs: `./know --help`
- Per-command help: `./know decisions --help`
- Agent cheat sheet: `docs/AGENT-CHEATSHEET.md`
- Rules file: `.claude/rules/session-knowledge.md`
