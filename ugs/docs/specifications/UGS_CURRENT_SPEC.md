# UGS (Universal Graph System) - Current Specification
**Version**: 0.2.0
**Date**: 2026-01-26
**Status**: Working prototype with program types, inference, streaming, embeddings, and tags

## WHAT WE HAVE ✅

### Core Graph Engine (src/graph.ts)
- ✅ **Graph Storage**: Nodes and edges with typed properties
- ✅ **Event Sourcing**: All mutations emit events to WAL
- ✅ **File Persistence**: WAL + snapshot for durability
- ✅ **Indexing**: O(1) ID lookup, type-based indexing
- ✅ **Traversal**: BFS/DFS, Dijkstra pathfinding
- ✅ **Node Tags**: Tag any node, query by tag (`addTags`, `getByTag`, `getByTags`)

### Program Types (src/entities/*.ts)
- ✅ **Provider**: Routes to LLM APIs (Cloudflare AI Gateway)
- ✅ **Model**: Inference configuration with situational params
- ✅ **Session**: JSONL conversation logs, multi-turn history
- ✅ **Agent**: Autonomous execution with harness (maxTurns, checkpoints)
- ✅ **Task**: Work specifications with state machine
- ✅ **Human**: Notifications and approvals
- ✅ **Information**: Structured knowledge nodes

### Inference Pipeline
- ✅ **Vercel AI SDK**: `ai`, `@ai-sdk/openai`, `@ai-sdk/anthropic`
- ✅ **Cloudflare AI Gateway**: Unified routing with `cf-aig-authorization`
- ✅ **ExecutionContext**: AsyncLocalStorage-based credential passing
- ✅ **Session Integration**: Agent creates Session, preserves history across turns
- ✅ **Streaming**: Both `generateText` (batch) and `streamText` (streaming) supported
- ✅ **Token Callbacks**: `onToken` callback + `MODEL_TOKEN_RECEIVED`, `AGENT_TOKEN` events

### Embeddings (src/entities/embedding.ts)
- ✅ **Cloudflare Workers AI**: `@cf/baai/bge-base-en-v1.5` (768 dimensions)
- ✅ **Node Embedding**: `embedNode(nodeId)` stores vector in properties
- ✅ **Similarity Search**: `findSimilar`, `findSimilarToNode`, `findSimilarToText`
- ✅ **Cosine Similarity**: Brute-force O(n) search (fine for <10k nodes)

### CLI (./ugs executable)
- ✅ **Program Commands**: provider, model, session, agent, task, human, info
- ✅ **Lifecycle**: create, configure, publish, deprecate
- ✅ **Inference**: `./ugs model invoke <id> --message "..."` (batch or `--stream`)
- ✅ **Agent Execution**: `./ugs agent assign`, `./ugs agent step`
- ✅ **Tags**: `./ugs tag add|remove|list`, `./ugs nodes --tag`
- ✅ **Embeddings**: `./ugs embed`, `./ugs similar`, `./ugs embed-stats`

### Test Coverage
- ✅ **981 unit tests** passing across all entities (+32 security tests)
- ✅ **94 security tests** covering SQL injection, rate limiting, error sanitization, symlink detection, ReDoS, prototype pollution
- ✅ **E2E tests** for inference, embeddings, and agent+session integration (require credentials)

## WHAT WE DON'T HAVE YET ❌

### Production Features
- ❌ **Tool Calling**: Agent tool execution beyond detection
- ❌ **Multi-Tenancy**: Single data directory, no isolation
- ❌ **Browser Context**: AsyncLocalStorage is Node.js/Bun only
- ❌ **Vector Index**: Brute-force similarity only, no ANN

### Security
- ❌ **Capability System**: Credentials from env vars, no graph-based secrets
- ❌ **Authorization**: No permission checking on operations
- ❌ **Audit**: Events logged but no query/analysis tools

### Scale
- ❌ **Distributed**: Single process only
- ❌ **Performance Testing**: No benchmarks
- ❌ **Large Graphs**: Untested beyond ~100 nodes

## ARCHITECTURE

See `docs/PROGRAM_TYPES_ARCHITECTURE.md` for:
- Program type definitions and state machines
- ExecutionContext and credential flow
- Inference chain diagram
- CLI command reference

## TEST STATUS

```bash
bun test                    # 981 pass (978 unit + 3 skipped e2e)
bun test security.test.ts   # 94 security tests (100% pass rate)
bun test model.e2e.test.ts  # Requires ANTHROPIC_API_KEY, CLOUDFLARE_*
bun test agent-session.e2e.test.ts  # Full integration test
bun test embedding.test.ts  # Embedding tests (e2e need CLOUDFLARE_*)
```

## NEXT PRIORITIES

### P1 - Security & Identity
- Capability-based secrets management (graph nodes, not env vars)
- Authorization checks on operations
- Principal tracking through delegation chains

### P2 - Features
- Agent tool execution (beyond detection)
- Program versioning

### P3 - Scale
- Multi-tenancy / isolation
- Performance testing
- Browser-compatible context (TC39 AsyncContext)
- Vector index for large-scale similarity search

## SOURCE FILES

| Component | Path | Tests |
|-----------|------|-------|
| Context | `src/context.ts` | `src/context.test.ts` |
| Graph | `src/graph.ts` | `src/graph.test.ts` |
| Provider | `src/entities/provider.ts` | `src/entities/provider.test.ts` |
| Model | `src/entities/model.ts` | `src/entities/model.test.ts` |
| Session | `src/entities/session.ts` | `src/entities/session.test.ts` |
| Agent | `src/entities/agent.ts` | `src/entities/agent.test.ts` |
| Task | `src/entities/task.ts` | `src/entities/task.test.ts` |
| Human | `src/entities/human.ts` | `src/entities/human.test.ts` |
| Information | `src/entities/information.ts` | `src/entities/information.test.ts` |
| Embedding | `src/entities/embedding.ts` | `src/entities/embedding.test.ts` |
