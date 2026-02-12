# Session Knowledge System

**Epic:** `agentic-primer-9ad`
**Status:** Phase 1 Implementation
**Spec:** `docs/specifications/SESSION_KNOWLEDGE_SYSTEM.md`

## Quick Start

### 1. Extract Session Metadata
```bash
# Extract metadata from all sessions (first time)
bun run src/session-knowledge/index/SessionMetadataExtractorLibSQL.ts

# Or use symlink
./build-index

# Force rebuild
./build-index --force
```

### 2. Query Sessions
```bash
# Today's sessions
./sessions today

# Yesterday
./sessions yesterday

# Recent 20
./sessions recent 20

# Find by file
./sessions file src/auth.ts

# Search
./sessions search "authentication bug"

# Cost summary (last 7 days)
./sessions cost 7

# Tool stats
./sessions tools

# Agent stats
./sessions agents

# Most modified files
./sessions files
```

### 3. Live Stats
```bash
# Show current session stats
./stats

# Start live processor (watches all sessions)
bun run src/session-knowledge/watcher/LiveProcessor.ts
```

### 4. Multi-Provider Support

The Session Knowledge System supports multiple LLM providers through OpenAI-compatible APIs:

**Supported Providers:**
- **LM Studio** (localhost:1234) - Free, local inference
- **Ollama** (localhost:11434) - Free, local inference
- **OpenAI** - Paid API service
- Any OpenAI-compatible endpoint

**Quick Setup:**
```bash
# Copy example configuration
cp .env.example .env

# Edit .env to configure your provider
# (Defaults to LM Studio - no config needed if running)
```

**Test Provider Availability:**
```bash
# Test all providers
./scripts/smoke-test-all.sh

# Test individual providers
./scripts/smoke-test-lmstudio.ts
./scripts/smoke-test-ollama.ts
./scripts/smoke-test-openai.ts

# See scripts/SMOKE-TESTS.md for detailed documentation
```

**Configuration:**

1. **LM Studio (default):**
```bash
# Start LM Studio with models loaded
# Chat: llama-3.2-3b-instruct
# Embeddings: text-embedding-nomic-embed-text-v1.5

# No configuration needed - uses localhost:1234 by default
```

2. **Ollama:**
```bash
# Install and start Ollama
# https://ollama.ai

# Pull models
ollama pull llama3.2:3b
ollama pull nomic-embed-text

# Configure environment
export LLM_BASE_URL="http://localhost:11434/v1"
export LLM_MODEL="llama3.2:3b"
export EMBEDDING_BASE_URL="http://localhost:11434/v1"
export EMBEDDING_MODEL="nomic-embed-text"
```

3. **OpenAI:**
```bash
# Set API key
export OPENAI_API_KEY="sk-..."

# Configure endpoints
export LLM_BASE_URL="https://api.openai.com/v1"
export LLM_MODEL="gpt-4o-mini"
export EMBEDDING_BASE_URL="https://api.openai.com/v1"
export EMBEDDING_MODEL="text-embedding-3-small"
```

**Model Recommendations:**

*Chat/Classification:*
- LM Studio: `llama-3.2-3b-instruct` (fast, accurate)
- Ollama: `llama3.2:3b` (fast, accurate)
- OpenAI: `gpt-4o-mini` (~$0.15 per 1M input tokens)

*Embeddings:*
- LM Studio: `text-embedding-nomic-embed-text-v1.5` (768 dims)
- Ollama: `nomic-embed-text` (768 dims)
- OpenAI: `text-embedding-3-small` (1536 dims, ~$0.02 per 1M tokens)

### 5. Semantic Search (Phase 3)

**Generate embeddings:**
```bash
# Make sure your provider is running (LM Studio, Ollama, or OpenAI configured)

# Generate embeddings (first time)
bun run src/session-knowledge/embeddings/SessionEmbeddingIndexerLibSQL.ts sessions

# Semantic search
bun run src/session-knowledge/cli/search-semantic-libsql.ts "how did we handle authentication?"

# Embedding stats
bun run src/session-knowledge/embeddings/SessionEmbeddingIndexerLibSQL.ts stats
```

### 6. Knowledge Extraction (Phase 4)

**Extract knowledge from sessions:**
```bash
# Make sure your provider is running (LM Studio, Ollama, or OpenAI configured)

# Extract from specific session
./extract-knowledge <session-id>

# Extract from all unprocessed sessions
./extract-knowledge all

# Extract from today's sessions
./extract-knowledge today

# Extract from yesterday's sessions
./extract-knowledge yesterday
```

**Two-Stage Pipeline:**
1. Stage 1: Fast candidate detection using heuristic patterns
2. Stage 2: LLM classification of candidates only (90% cost reduction)

**Extracted Knowledge:**
- Decisions (stored in session_decisions table)
- Learnings (stored in session_learnings table)
- Errors (stored in session_errors table)

### 7. Cognitive Features (Phase 5)

**Temporal Queries - Query Knowledge at Specific Points in Time:**
```bash
# What did we know about auth on Jan 15?
./know temporal "authentication" --as-of="2026-01-15"

# What decisions existed on Feb 1?
./know temporal "decisions" --as-of="2026-02-01"

# JSON output for programmatic use
./know temporal "libSQL" --as-of="2026-01-30" --json
```

**Confidence Decay - See How Knowledge Ages:**
```bash
# Show decay curves for all domains
./know decay --show

# Show tech knowledge decay specifically
./know decay --domain=tech --show

# Query knowledge with decay applied
./know decay --domain=tech

# See what knowledge is aging fastest
./know decay --json | jq '.results | sort_by(.currentConfidence)[:5]'
```

**Thinking Arcs - Detect Conceptual Evolution:**
```bash
# Detect thinking patterns in a session
./know arcs f03b3b54-ca47-46d3-be1f-20ccfc82f9de

# JSON output
./know arcs abc123 --json
```

**Knowledge Relationships - How Knowledge Connects:**
```bash
# Show relationships for a knowledge item
./know relationships decision-abc123

# JSON output
./know relationships learning-xyz789 --json
```

**Bi-Temporal Semantics:**
- `valid_time`: When the fact was true in the real world
- `transaction_time`: When we learned about it
- Query knowledge "as it was known" at any point in time
- Track when facts were valid vs when we discovered them

**Domain-Specific Decay:**
- **Tech** (9 month half-life): Exponential decay - frameworks change fast
- **Science** (7.5 year half-life): Power law decay - fundamentals are stable
- **News** (2 month half-life): Exponential decay - events become history
- **Core** (5 year half-life): Stepped decay - stable until paradigm shifts

**Arc Types:**
- **Breakthrough**: Sudden insights ("realized", "aha moment")
- **Pattern Discovery**: Recognizing recurring structures ("the pattern is", "always")
- **Abstraction**: Moving from concrete to general ("fundamentally", "in general")
- **Refinement**: Iterative improvement ("actually", "more precisely")
- **Synthesis**: Combining concepts ("integrating", "bringing together")

## Components

### Phase 1: Core Index ✅
- **SessionMetadataExtractor** - Scans JSONL logs and extracts metadata
- **QueryEngine** - Fast queries by date, file, cost
- **Schema** - libSQL database with FTS and native vectors
- **CLI** - `sessions` command

### Phase 2: File Watching ✅
- **FileWatcher** - Monitors active sessions
- **SessionStats** - Real-time stats (Tier 1)
- **LiveProcessor** - Integrates watching + stats
- **CLI** - `./stats` command

### Phase 3: Embeddings ✅
- **EmbeddingGenerator** - LM Studio / OpenAI API client
- **VectorStoreLibSQL** - libSQL native F32_BLOB vectors with DiskANN
- **SessionEmbeddingIndexer** - Batch processes sessions for search
- **CLI** - Semantic search command

### Phase 4: Knowledge Extraction ✅
- **KnowledgeExtractor** - Two-stage pipeline for extracting decisions, learnings, and errors
- **MessageCandidateDetector** - Stage 1: Fast heuristic-based candidate detection
- **SemanticMessageClassifier** - Stage 2: LLM-based classification of candidates
- **CLI** - `./extract-knowledge` command

### Phase 5: Cognitive Integration ✅
- **TemporalQueries** - Bi-temporal query engine (query knowledge at any point in time)
- **ConfidenceDecay** - Domain-specific knowledge aging (tech/science/news/core)
- **ArcDetector** - Thinking pattern recognition (breakthrough, pattern discovery, abstraction)
- **Knowledge Relationships** - Track how knowledge connects (supports, contradicts, supersedes)
- **CLI** - `./know temporal`, `./know decay`, `./know arcs`, `./know relationships`

### Phase 6: Cross-Session (Planned)
- **MultiSessionWatcher** - Track all sessions
- **CrossSessionSearch** - Query across sessions
- **SessionSimilarity** - Find related work

## Architecture

```
~/.claude/index/sessions-libsql.db  # libSQL database with native vectors
~/.claude/projects/<project>/        # Session JSONL files
  ├─ session-id.jsonl
  └─ session-id/subagents/
      └─ agent-id.jsonl
```

## Performance

- Index build: ~1-2 min for 1000 sessions
- Query by date: <5ms
- Search: <20ms
- On-demand session load: ~50ms

## Cost

**With LM Studio (local):**
- Indexing: $0 (programmatic)
- Embeddings: **$0 (local inference)**
- Processing: TBD (Tier 2-4)

**With OpenAI API:**
- Indexing: $0 (programmatic)
- Embeddings: ~$0.0004 per session (text-embedding-3-small)
- Processing: ~$0.46 per session (with all tiers)

Total: **$0** (LM Studio) or <$0.50 (OpenAI)

## Status

- [x] Phase 1: Core indexing (SessionMetadataExtractor, QueryEngine, CLI)
- [x] Phase 2: File watching (LiveProcessor, SessionStats)
- [x] Phase 3: Embeddings (libSQL native vectors, semantic search)
- [x] Phase 4: Knowledge extraction (KnowledgeExtractor, two-stage pipeline)
- [x] Phase 5: Cognitive integration (Temporal queries, confidence decay, thinking arcs)
- [ ] Phase 6: Cross-session queries

## Test Coverage

**Cognitive Integration Tests:**
- 37 tests covering all cognitive features
- 392 expect() assertions
- 100% pass rate (52ms execution)

**Test Categories:**
- ConfidenceDecay: All domains (tech/science/news/core), edge cases, time estimation
- TemporalQueries: Bi-temporal semantics, change detection, decay application
- ArcDetector: Pattern recognition, arc types, real session data
- Integration: End-to-end workflows combining all features

**Running Tests:**
```bash
# Quick start
bun test

# Run all test suites
./scripts/test-all.sh

# Watch mode for development
./scripts/test-watch.sh

# See TEST-QUICK-START.md or TESTING.md for full documentation
```

## Related Beads

- `agentic-primer-9ad` - Main epic
- `agentic-primer-9ad.1` - Phase 1 (current)
- `agentic-primer-9ad.2` - Phase 3 (embeddings)
- `agentic-primer-9ad.3` - Phase 4 (extraction)
- `agentic-primer-9ad.4` - Phase 5 (cross-session)
