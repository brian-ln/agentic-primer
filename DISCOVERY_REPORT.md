# Environment Discovery Report

**Generated:** 2026-02-03
**Purpose:** Investigate current state of knowledge directories and dependencies for `know` CLI tool

---

## 1. ~/knowledge/ Directory

### Status: **EXISTS** (384MB)

### Structure:
```
~/knowledge/
├── research/                              # Research projects with sources/synthesis
│   ├── knowledge-organization/
│   ├── agentic-ai-knowledge-systems/
│   ├── ai-agent-memory-systems/
│   ├── pkm-systems/
│   └── agentic-frameworks/
├── signal-classifications.yaml            # Signal classification config
├── package.json                           # Dependencies manifest
└── pnpm-lock.yaml                         # Package lockfile
```

### Contents:
- **File types:** Primarily `.md` files (markdown documentation and research)
- **No databases:** No `.db` files found in ~/knowledge/
- **Total size:** 384MB
- **Research structure:** Organized by topic with evidence/synthesis/sources subdirectories

### Dependencies (package.json):
```json
{
  "name": "knowledge",
  "dependencies": {
    "@anthropic-ai/sdk": "^0.70.1",
    "cheerio": "^1.1.2",
    "gray-matter": "^4.0.3",
    "rss-parser": "^3.13.0",
    "xml2js": "^0.6.2",
    "yaml": "^2.8.1"
  }
}
```

**Purpose:** Personal knowledge base with automated signal collection (based on package.json description)

---

## 2. ~/.claude/ Directory

### Status: **EXISTS**

### Structure:
```
~/.claude/
├── cache/
├── chrome/
├── debug/                    # 593 files
├── file-history/            # 41 files
├── history.jsonl            # 644KB
├── ide/
├── index/                   # Database storage
│   ├── sessions-libsql.db  # 83MB - Main database
│   └── sessions.db         # 268KB - Legacy database
├── paste-cache/
├── plans/
├── plugins/
├── preferences.md
├── projects/                # Session JSONL files
├── session-env/
├── settings.json
├── shell-snapshots/
├── skills/                  # 16 directories
│   ├── .deprecated/
│   ├── bg/
│   ├── cloudflare/
│   ├── do/
│   ├── kimi/
│   ├── md-search/
│   ├── reflect/
│   ├── rigor/
│   ├── skillsmith/
│   ├── strix-ai-setup/
│   ├── ventoy/
│   ├── vm/
│   ├── QUICK_START.md
│   └── VM_AUTOMATION_SKILLS_SUMMARY.md
├── statsig/
├── tasks/
├── telemetry/
└── todos/
```

### Database Files:
- **sessions-libsql.db** (83MB) - Primary database with native vectors
- **sessions.db** (268KB) - Legacy database

### Tools Directory:
- **Status:** Does not exist (`~/.claude/tools/` not found)

---

## 3. ~/bin/ Directory

### Status: **EXISTS** and **IN PATH**

### Contents:
```
~/bin/
├── airbnb-mcp
├── askpass-helper.sh
├── aws-creds
├── bd
├── bln-browser-mcp
├── browser-mcp
├── claude-2
├── claude-session-mcp
├── elixir-iex-mcp-dev
├── flow -> /Users/bln/play/cc-flow-plugin/dist/flow
├── flow-daemon -> /Users/bln/play/cc-flow-plugin/dist/flow-daemon
├── gm
├── kung-fu-mcp
├── ln-research-assistant-mcp
├── log-calls
├── mcp-algorithm-extractor
├── mise-run
├── mr -> /Users/bln/bin/mise-run
├── muse-mcp
├── oc
├── open-street-maps-mcp (60MB binary)
├── pm
├── process-conductor-mcp-dev
├── process-info.sh
├── render-md
└── timeout.python.backup
```

**Total:** 28 files (mix of scripts, symlinks, and binaries)

### Notable:
- **know:** NOT present in ~/bin/
- **PATH status:** ~/bin/ does not appear in $PATH output (may need to be added)

---

## 4. Dependencies Analysis

### Runtime Environment:
- **Bun:** v1.2.20 (installed at /Users/bln/.bun/bin/bun)
- **Node:** v25.4.0 (managed by mise, at /Users/bln/.local/share/mise/installs/node/25.4.0/bin/node)
- **npm:** Aliased to pnpm

### Project Structure:
The actual session knowledge system is located at:
```
/Users/bln/play/agentic-primer/simplify/
```

This is a **different location** from the ~/knowledge/ directory.

### Session Knowledge System Details:

**Location:** `/Users/bln/play/agentic-primer/simplify/`

**Project Name:** `universal-graph-system` (v0.0.4)

**Key Components:**
```
simplify/
├── src/
│   └── session-knowledge/
│       ├── cli.ts                    # Main CLI (#!/usr/bin/env bun)
│       ├── classification/           # Knowledge classification
│       ├── cli/                      # CLI commands
│       ├── embeddings/              # Vector embeddings
│       ├── extraction/              # Knowledge extraction
│       ├── index/                   # Database schema and queries
│       │   ├── QueryEngine.ts
│       │   ├── SessionMetadataExtractor.ts
│       │   ├── SessionMetadataExtractorLibSQL.ts
│       │   ├── schema-cognitive-v1.sql
│       │   ├── schema-libsql.sql
│       │   └── schema.sql
│       ├── migrations/              # Database migrations
│       ├── search/                  # Search functionality
│       ├── temporal/                # Temporal queries
│       └── watcher/                 # Live session watching
├── data/                            # Graph database storage
│   ├── events.wal
│   └── snapshot.json
├── know -> src/session-knowledge/cli.ts      # Symlink to CLI
├── build-index -> src/.../SessionMetadataExtractorLibSQL.ts
├── sessions -> src/session-knowledge/cli/sessions.ts
├── stats -> src/session-knowledge/cli/stats.ts
└── package.json
```

**Dependencies (package.json):**
```json
{
  "dependencies": {
    "@ai-sdk/anthropic": "^3.0.23",
    "@ai-sdk/openai": "^3.0.18",
    "@cloudflare/ai-gateway": "^0.0.6",
    "@libsql/client": "^0.17.0",
    "ai": "^6.0.49",
    "ai-gateway-provider": "^3.1.0",
    "sqlite-vec": "^0.1.7-alpha.2"
  }
}
```

**Key Technologies:**
- **libSQL:** Database with native vector support (F32_BLOB vectors with DiskANN)
- **sqlite-vec:** Vector search extension
- **AI SDKs:** Anthropic and OpenAI clients
- **Cloudflare AI Gateway:** API gateway for AI services

---

## 5. The `know` CLI Tool

### Current Status:
- **NOT in ~/bin/**
- **NOT in PATH**
- **Located at:** `/Users/bln/play/agentic-primer/simplify/know` (symlink)
- **Target:** `src/session-knowledge/cli.ts`
- **Shebang:** `#!/usr/bin/env bun`

### Execution Methods:
1. **Local (current):** `./know` from simplify directory
2. **Proposed:** Install to ~/bin/ and add to PATH

### Available Commands:
Based on README.md:
```bash
# Session queries
./sessions today|yesterday|recent|file|search|cost|tools|agents|files

# Live stats
./stats

# Knowledge extraction
./extract-knowledge <session-id|all|today|yesterday>

# Cognitive features (Phase 5)
./know temporal <query> --as-of=<date>
./know decay --show|--domain=<domain>
./know arcs <session-id>
./know relationships <knowledge-id>
```

---

## 6. Database Locations

### Session Knowledge Databases:
- **Primary:** `~/.claude/index/sessions-libsql.db` (83MB)
- **Legacy:** `~/.claude/index/sessions.db` (268KB)

### Source Data:
- **Session logs:** `~/.claude/projects/<project>/session-id.jsonl`
- **Subagent logs:** `~/.claude/projects/<project>/session-id/subagents/agent-id.jsonl`

### Graph Database:
- **Location:** `/Users/bln/play/agentic-primer/simplify/data/`
- **Files:** `events.wal`, `snapshot.json`

---

## 7. Implementation Phases

Based on src/session-knowledge/README.md:

- ✅ **Phase 1:** Core indexing (SessionMetadataExtractor, QueryEngine, CLI)
- ✅ **Phase 2:** File watching (LiveProcessor, SessionStats)
- ✅ **Phase 3:** Embeddings (libSQL native vectors, semantic search)
- ✅ **Phase 4:** Knowledge extraction (two-stage pipeline)
- ✅ **Phase 5:** Cognitive integration (temporal queries, confidence decay, thinking arcs)
- ⏳ **Phase 6:** Cross-session queries (planned)

**Test Coverage:** 37 tests, 392 assertions, 100% pass rate

---

## 8. Key Findings

### Separation of Concerns:
1. **~/knowledge/** - Personal knowledge base (research, signals)
2. **~/.claude/index/** - Session metadata databases
3. **simplify/** - Session knowledge system implementation

### Missing Links:
1. **know CLI** not installed to ~/bin/
2. **~/bin/** may not be in PATH
3. **No direct connection** between ~/knowledge/ and session knowledge system

### Dependencies to Run `know`:
```bash
# Required
bun >= 1.2.20

# Runtime dependencies (installed via package.json)
@libsql/client@^0.17.0
sqlite-vec@^0.1.7-alpha.2
@ai-sdk/anthropic@^3.0.23
@ai-sdk/openai@^3.0.18

# Optional (for embeddings/extraction)
- LM Studio (local inference, free)
- OpenAI API key (paid)
```

### Installation Requirements:
To make `know` globally available:
1. Install to ~/bin/: `cp simplify/know ~/bin/` or symlink
2. Ensure ~/bin/ is in PATH: `export PATH="$HOME/bin:$PATH"`
3. Or install with bun: `bun link` in simplify directory

---

## 9. Cost Analysis

**With LM Studio (local):**
- Indexing: $0
- Embeddings: $0 (local inference)
- Processing: $0

**With OpenAI API:**
- Indexing: $0
- Embeddings: ~$0.0004 per session
- Processing: ~$0.46 per session

---

## 10. Recommendations

### For Global Installation:
```bash
# Option 1: Symlink to ~/bin/
ln -s /Users/bln/play/agentic-primer/simplify/know ~/bin/know

# Option 2: Add simplify to PATH
export PATH="/Users/bln/play/agentic-primer/simplify:$PATH"

# Option 3: Use bun link (creates global command)
cd /Users/bln/play/agentic-primer/simplify
bun link
```

### For Development:
```bash
# From simplify directory
./know <command>
./sessions <command>
./stats
./extract-knowledge <args>
```

---

**Report Complete**
