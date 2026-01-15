# Project Context & Working Assumptions

**Last Updated**: 2026-01-15
**Status**: Exploratory Research / Conceptual Development

---

## What This Project Is

**tk-agents is a conceptual exploration and evolution of connected ideas**, NOT a production product.

### Purpose
- Explore actor-based task/knowledge graph protocols
- Test ideas iteratively through prototyping
- Connect concepts across disciplines (CS, neuroscience, mathematics, system design)
- Learn through experimentation and reflection

### NOT Goals
- Production-ready system
- Optimized for performance
- Complete implementation of all features
- Industrial-strength error handling

---

## Working Model

### Human Role
**"I am not writing the code"** - The vision is to have AI agents with skills and knowledge do the implementation work.

### AI Agents As Builders
- Agents have skills and knowledge
- Agents implement based on specifications
- Human provides direction, evaluation criteria, and feedback
- Human role: architect, evaluator, researcher - not coder

### Success Criteria
Quality critique focuses on:
- ✅ **Specification precision**: Can this be explained clearly enough for agents to implement?
- ✅ **Evaluation criteria**: Can we define fitness functions to evaluate agent work?
- ✅ **Conceptual clarity**: Does this advance understanding of the idea space?

NOT:
- ❌ "This is too hard to code" (with human coding assumption)
- ❌ Production optimization concerns (premature)
- ❌ Complete feature implementation (iterative exploration)

---

## Optimization Targets

### Current Phase: Exploration
- **Optimize for**: Idea clarity, conceptual connections, learning
- **Accept**: Incomplete implementations, naive algorithms, missing features
- **Value**: Rapid iteration, experimental freedom, intellectual honesty

### Future Phases (When Relevant)
- Production hardening
- Performance optimization
- Complete feature sets
- Operational concerns

**Premature optimization is the root of all evil** - focus on understanding first.

---

## Key Architectural Concepts

### Core Principle: Everything is a Node, All Interactions are Messages
```
send(nodeId, messageType, payload) -> result
```

This is the **primitive protocol** - everything builds on this.

### Actor Model Philosophy
- Actors send messages to other actors
- Addresses discovered via incoming messages (may not be direct responses)
- Message passing semantics, not implementation details
- "Don't care about the how" - care about the abstraction

### Graph as Coordination Structure
- Nodes: Tasks, Knowledge, Actors, Executions, WorkerPools
- Edges: Relationships, dependencies, assignments
- Query: Graph traversal, pattern matching, semantic search

---

## Current State (2026-01-15)

### What Exists
- **Core Protocol**: Graph.send() message routing (~100 lines)
- **Domain Nodes**: TaskNode, KnowledgeNode (~550 lines)
- **Actors**: ClaudeActor, BashActor (~310 lines)
- **Registry**: Actor management (~100 lines)
- **Tests**: Basic protocol and actor tests (19 passing)
- **Analysis**:
  - ERLANG_ARCHITECTURE_ANALYSIS.md (exploration of Erlang patterns)
  - CRITICAL_ANALYSIS.md (architectural critique)
  - SOCRATIC_QUESTIONS.md (probing questions)
  - CONCEPT_GRAPH/ (50 concepts, 61 relationships, interactive explorer)

### What's Explored (Not Implemented)
- Supervision trees and fault tolerance
- OTP-style behaviors (gen_server, gen_statem)
- Process groups and discovery
- Mailboxes and async messaging
- Event sourcing and time travel
- Memory/access patterns and tiering
- Distributed coordination

---

## Design Decisions (Rationale)

### Message Passing Over Direct Calls
- **Why**: Uniform interface, serialization-ready, distributed-ready
- **Cost**: Debugging complexity, runtime type checking
- **Tradeoff**: Accepted for conceptual purity and future flexibility

### Prototype Code Quality
- **Current**: Simple, readable, minimal
- **Future**: Agent-implemented with proper specifications
- **Philosophy**: Prototype to learn, then specify for agents to build properly

### Erlang Terminology & Patterns
- **Purpose**: Connect to proven distributed systems concepts
- **Caveat**: JavaScript/TypeScript doesn't have BEAM VM guarantees
- **Approach**: Learn from Erlang thinking, adapt to TypeScript reality

---

## Future Problem Spaces (Tracked, Not Solved)

### 1. Latency & Locality Tiers
Addressable actors with different latency/availability profiles:
- CPU registers → memory → cache → disk → network
- Fallacies of distributed computing
- Tiering strategies for actor placement

### 2. Compression & Reconstruction
- Deterministic reconstruction from inputs + process
- Compress into interconnected facts (actors with state)
- Recreate on demand (like SQL views)
- Universe storage constraints

### 3. Concurrency & Coordination
- Graph represents static relationships
- Need: Conflict resolution, concurrency control, deadlock detection
- Race conditions when multiple actors modify same node

### 4. Error Model
- Currently: throw exceptions
- Need: Recoverable vs fatal errors, timeouts, circuit breakers, retry logic

### 5. Scaling & Persistence
- Currently: In-memory Map<string, NodeActor>
- Need: Persistence layer, indexing, tiering, distribution

---

## Meta: This Conversation Uses The Theory

**You (Claude) are an actor** interacting with other actors:
- API endpoints (Claude API server)
- LLM inference
- Local tools (file system, shell, git)
- Background agents/tasks spawned
- User (human actor receiving messages)

This conversation IS an instance of actor-based coordination in action.

---

## How To Critique This Project

### Good Critiques
- "Can this be specified precisely enough for agents to implement?"
- "Are success criteria objective and measurable?"
- "Does this clarify thinking about [concept]?"
- "What experiments would test this idea?"
- "What conceptual gaps exist?"

### Not Helpful Critiques
- "Too complex to code" (assumes human coding)
- "Missing production features" (not the goal)
- "Performance not optimized" (premature optimization)
- "Incomplete implementation" (iterative exploration)

---

## Next Steps

See [EXPLORATION_ROADMAP.md](EXPLORATION_ROADMAP.md) for prioritized next explorations.

See [WORK_SESSION_LOG.md](WORK_SESSION_LOG.md) for session-by-session progress.
