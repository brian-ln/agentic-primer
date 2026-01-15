# Concept Graph - Index

Welcome to the Concept Graph system for exploring interdisciplinary concepts from the tk-agents conversation.

## Start Here

### New Users
1. **[QUICK_START.md](QUICK_START.md)** - 5-minute tour with common commands
2. Run: `bun explore.ts stats` - See what's in the graph
3. Run: `bun examples.ts` - See 6 example explorations

### Detailed Documentation
- **[README.md](README.md)** - Comprehensive guide with all features
- **[DELIVERABLE_SUMMARY.md](DELIVERABLE_SUMMARY.md)** - Project overview and success metrics

## Files

### Data Files
- **[concepts.json](concepts.json)** - 50 concepts with domains, tags, descriptions
- **[relationships.json](relationships.json)** - 61 typed relationships between concepts

### Code Files
- **[search.ts](search.ts)** - Core search and traversal utilities (importable)
- **[explore.ts](explore.ts)** - Interactive CLI explorer (executable)
- **[examples.ts](examples.ts)** - Demonstration script (executable)

## Quick Commands

```bash
# Overview
bun explore.ts stats
bun explore.ts domains

# Search
bun explore.ts search actor
bun explore.ts search --domain neuroscience
bun explore.ts search --tag erlang

# Details
bun explore.ts show actor-model
bun explore.ts related erlang-otp

# Navigation
bun explore.ts explore message-passing 2
bun explore.ts path graph-protocol memory-reconsolidation

# Examples
bun examples.ts
```

## What's Inside

### Domains (9)
- computer-science (core CS concepts)
- neuroscience (brain mechanisms)
- cognitive-science (cognition and memory)
- mathematics (formal foundations)
- system-design (distributed systems, fault tolerance)
- programming-languages (language design)
- software-engineering (patterns and practices)
- design-patterns (reusable solutions)
- concurrency (parallel computation)

### Key Concepts (50)
Computer Science: actor-model, message-passing, erlang-otp, supervision-tree, state-machine, graph-protocol, event-sourcing, distributed-systems, mailbox, fault-tolerance

Neuroscience: memory-reconsolidation, synaptic-plasticity, working-memory, access-pattern

Mathematics: category-theory, lambda-calculus, set-theory, graph-theory, dag

Programming: functional-programming, immutability, pure-functions, pattern-matching, type-system

System Design: let-it-crash, circuit-breaker, workflow-engine, reactive-systems, cqrs

### Relationship Types (28)
is-a, uses, implements, provides, enables, requires, embodies, achieves, combines-with, based-on, relates-to, mechanism, interacts-with, influences, often-used-with, formalizes, foundation-of, emphasizes, feature-of, runs-on, property-of, inspired-by, models, may-model, built-on, can-represent, creates, exhibits, records

## Common Use Cases

### "I want to understand X"
```bash
bun explore.ts show X           # See concept details
bun explore.ts related X        # See related concepts
bun explore.ts explore X 2      # Explore neighborhood
```

### "How does X relate to Y?"
```bash
bun explore.ts path X Y         # Find shortest path
```

### "Show me all X concepts"
```bash
bun explore.ts search --domain X     # By domain
bun explore.ts search --tag X        # By tag
bun explore.ts search X              # By keyword
```

### "Explore a topic in depth"
```bash
# Example: Erlang/OTP patterns
bun explore.ts explore erlang-otp 2
bun explore.ts show supervision-tree
bun explore.ts show gen-server
bun explore.ts search --tag erlang
```

## Programmatic Usage

```typescript
import {
  search,
  getConcept,
  getRelatedConcepts,
  traverse,
  findPath,
} from "./search";

// Search by criteria
const results = search({
  domain: "computer-science",
  tag: "concurrency",
  keyword: "actor",
});

// Get concept
const concept = getConcept("actor-model");

// Get related
const related = getRelatedConcepts("erlang-otp");

// Traverse graph
const neighborhood = traverse("message-passing", { maxDepth: 2 });

// Find path
const path = findPath("actor-model", "memory-reconsolidation");
```

## Extending the Graph

### Add a Concept
Edit `concepts.json`:
```json
{
  "id": "my-concept",
  "label": "My Concept",
  "domains": ["computer-science"],
  "tags": ["tag1", "tag2"],
  "description": "What this concept means",
  "references": ["related-concept-1"]
}
```

### Add a Relationship
Edit `relationships.json`:
```json
{
  "from": "my-concept",
  "to": "existing-concept",
  "type": "uses",
  "description": "How they relate"
}
```

### Verify
```bash
bun explore.ts stats            # Check counts
bun explore.ts show my-concept  # Verify it appears
```

## Architecture

### Design Principles
1. **Simplicity** - JSON data, TypeScript utilities, no database
2. **Multiple projections** - Domain, tag, graph, path views
3. **Extensibility** - Easy to add concepts and relationships
4. **Performance** - Pre-built indexes, efficient algorithms
5. **Usability** - CLI for exploration, importable for code

### Structure
```
Concepts (nodes)
  ↓
  id, label, domains[], tags[], description, references[]

Relationships (edges)
  ↓
  from, to, type, description

Indexes
  ↓
  by-id (Map), by-domain (Map), by-tag (Map)
  outgoing-edges (Map), incoming-edges (Map)

Search/Traversal
  ↓
  domain, tag, keyword filters
  BFS traversal, shortest path (BFS)
```

## Key Insights

1. **Actor model** connects concurrency, message-passing, and fault tolerance
2. **Access patterns** bridge CS and neuroscience via memory reconsolidation
3. **Mathematics** provides theoretical foundations (category theory, lambda calculus)
4. **Fault tolerance** has multiple approaches (supervision, circuit-breaker, replication)
5. **Message passing** is universal (actors, Smalltalk, distributed systems)

## Getting Help

- Read [README.md](README.md) for comprehensive documentation
- Read [QUICK_START.md](QUICK_START.md) for quick tour
- Run `bun explore.ts help` for command reference
- Run `bun examples.ts` to see usage patterns
- Look at `concepts.json` and `relationships.json` to understand structure

## Next Steps

1. **Explore**: Try the quick commands above
2. **Learn**: Read through the examples
3. **Extend**: Add your own concepts and relationships
4. **Integrate**: Import `search.ts` in your code
5. **Visualize**: Consider adding graph visualization (future)

---

**Quick Start:** `bun explore.ts stats && bun examples.ts`

**Documentation:** [README.md](README.md) | [QUICK_START.md](QUICK_START.md)

**Project:** Part of tk-agents - Task/Knowledge Graph Actor Protocol
