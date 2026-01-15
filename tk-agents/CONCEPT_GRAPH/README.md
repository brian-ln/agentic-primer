# Concept Graph: Interdisciplinary Knowledge Navigation

A knowledge graph system for exploring concepts from the tk-agents conversation, spanning computer science, neuroscience, mathematics, cognitive theory, and system design.

## Overview

This knowledge graph captures 50+ key concepts and their relationships, organized by domains and tags. The system enables multiple exploration strategies:

- **Categorical search** - Find concepts by domain (computer-science, neuroscience, mathematics, etc.)
- **Tag-based search** - Filter by topics (concurrency, memory, fault-tolerance, etc.)
- **Keyword search** - Search labels and descriptions
- **Graph traversal** - Explore concept neighborhoods and connections
- **Path finding** - Discover how concepts relate across disciplines

## Structure

```
CONCEPT_GRAPH/
├── concepts.json       # Node definitions with tags, domains, descriptions
├── relationships.json  # Edges showing how concepts connect
├── search.ts          # Core search and traversal utilities
├── explore.ts         # Interactive CLI explorer
└── README.md          # This file
```

## Quick Start

### Interactive Exploration

```bash
# Make explorer executable
chmod +x explore.ts

# Show graph statistics
bun explore.ts stats

# List all domains
bun explore.ts domains

# List all tags
bun explore.ts tags

# Search for concepts
bun explore.ts search actor
bun explore.ts search memory

# Search by domain
bun explore.ts search --domain neuroscience
bun explore.ts search --domain mathematics

# Search by tag
bun explore.ts search --tag erlang
bun explore.ts search --tag concurrency

# Show concept details
bun explore.ts show actor-model
bun explore.ts show memory-reconsolidation

# Show related concepts
bun explore.ts related erlang-otp
bun explore.ts related graph-protocol

# Explore neighborhood (depth 1-3)
bun explore.ts explore message-passing 1
bun explore.ts explore state-machine 2

# Find path between concepts
bun explore.ts path actor-model memory-reconsolidation
bun explore.ts path erlang-otp category-theory
```

### Programmatic Usage

```typescript
import {
  search,
  getConcept,
  getRelatedConcepts,
  traverse,
  findPath,
} from "./search";

// Search by domain
const csconcepts = search({ domain: "computer-science" });

// Search by tag
const concurrencyConcepts = search({ tag: "concurrency" });

// Search by keyword
const actorConcepts = search({ keyword: "actor" });

// Combined search
const results = search({
  domain: "system-design",
  tag: "fault-tolerance",
  maxResults: 10,
});

// Get concept details
const actorModel = getConcept("actor-model");

// Get related concepts
const related = getRelatedConcepts("erlang-otp");
console.log(related.outgoing); // Concepts it points to
console.log(related.incoming); // Concepts that point to it

// Traverse graph (explore neighborhood)
const neighborhood = traverse("message-passing", {
  maxDepth: 2,
  direction: "both",
});

// Find shortest path
const path = findPath("actor-model", "memory-reconsolidation");
```

## Domains

The knowledge graph spans multiple disciplines:

- **computer-science** - Core CS concepts (actors, graphs, algorithms)
- **neuroscience** - Brain mechanisms (memory, plasticity, learning)
- **cognitive-science** - Cognition and memory systems
- **mathematics** - Formal foundations (category theory, set theory, lambda calculus)
- **system-design** - Distributed systems, fault tolerance, workflows
- **programming-languages** - Language design and paradigms
- **software-engineering** - Design patterns and practices
- **design-patterns** - Reusable solution patterns
- **concurrency** - Parallel and concurrent computation

## Key Concepts

### Computer Science
- **actor-model** - Computational model with message-passing actors
- **message-passing** - Communication via messages vs shared memory
- **erlang-otp** - Battle-tested fault-tolerant platform
- **supervision-tree** - Hierarchical fault recovery
- **state-machine** - Formal model of computation with states and transitions
- **graph-protocol** - Message-based graph interaction protocol
- **event-sourcing** - Storing state as immutable events
- **distributed-systems** - Networked computation with coordination challenges

### Neuroscience & Cognitive Science
- **memory-reconsolidation** - Memory updating mechanism
- **synaptic-plasticity** - Synaptic strength changes underlying learning
- **working-memory** - Limited-capacity temporary information store
- **cognitive-architecture** - Framework for modeling cognition
- **access-pattern** - How data/memories are accessed over time

### Mathematics & Theory
- **category-theory** - Abstract mathematics of composition
- **lambda-calculus** - Formal system for computation
- **set-theory** - Foundation of mathematics
- **graph-theory** - Study of nodes and edges
- **dag** - Directed acyclic graphs for dependencies

### System Design
- **fault-tolerance** - Continuing operation despite failures
- **let-it-crash** - Design philosophy: restart instead of defensive handling
- **supervision-tree** - Hierarchical fault recovery
- **circuit-breaker** - Preventing cascading failures
- **workflow-engine** - Managing multi-step workflows

### Programming
- **functional-programming** - Computation via function evaluation
- **immutability** - Data cannot change after creation
- **pure-functions** - No side effects, deterministic
- **pattern-matching** - Matching structures against patterns
- **type-system** - Classifying values and expressions

## Relationship Types

Concepts are connected via typed relationships:

- **is-a** - Type/subtype relationship (gen_statem is-a behavior-pattern)
- **uses** - One concept uses another (actor-model uses message-passing)
- **implements** - Concrete implementation (erlang-otp implements actor-model)
- **provides** - Supplies functionality (erlang-otp provides supervision-tree)
- **enables** - Makes something possible (event-sourcing enables time-travel)
- **requires** - Dependency (distributed-systems requires consensus)
- **embodies** - Philosophical alignment (supervision-tree embodies let-it-crash)
- **relates-to** - General relationship
- **inspired-by** - Historical influence
- **foundation-of** - Theoretical basis

## Example Explorations

### 1. Actor Model Ecosystem

```bash
bun explore.ts explore actor-model 2
```

Shows the actor model and its immediate neighborhood:
- **Uses:** message-passing, process-isolation
- **Implemented by:** erlang-otp
- **Related to:** smalltalk, distributed-systems
- **Applied in:** graph-protocol, task-graph

### 2. Memory Systems (Cross-Disciplinary)

```bash
bun explore.ts search memory
```

Reveals concepts across neuroscience and computer science:
- **Neuroscience:** memory-reconsolidation, working-memory, synaptic-plasticity
- **Computer Science:** access-pattern, event-sourcing, immutability
- **Connections:** How memory patterns inform system design

### 3. Fault Tolerance Strategies

```bash
bun explore.ts search --tag fault-tolerance
```

Shows fault tolerance approaches:
- **Erlang approach:** supervision-tree, let-it-crash
- **Pattern-based:** circuit-breaker
- **Distributed:** replication, consensus
- **System design:** reactive-systems

### 4. Bridging Disciplines

```bash
bun explore.ts path actor-model memory-reconsolidation
```

Discovers connections between actor systems and neuroscience:
- actor-model → message-passing → access-pattern → memory-reconsolidation
- Shows how computational models and cognitive science intersect

### 5. Theoretical Foundations

```bash
bun explore.ts search --domain mathematics
```

Mathematical concepts underlying systems:
- **category-theory** - Composition and abstraction
- **lambda-calculus** - Computational foundation
- **set-theory** - Mathematical foundations
- **graph-theory** - Structure and algorithms

## Adding New Concepts

### 1. Add to concepts.json

```json
{
  "id": "new-concept",
  "label": "New Concept",
  "domains": ["computer-science"],
  "tags": ["tag1", "tag2"],
  "description": "What this concept means and why it matters",
  "references": ["related-concept-1", "related-concept-2"]
}
```

### 2. Add relationships to relationships.json

```json
{
  "from": "new-concept",
  "to": "existing-concept",
  "type": "uses",
  "description": "How they relate"
}
```

### 3. Common Patterns

**When adding a concept:**
- Use kebab-case IDs (actor-model, not ActorModel)
- Include 2-4 domains that best categorize it
- Add 3-6 specific tags for discovery
- Write 1-2 sentence descriptions (clear, concise)
- Reference 3-5 related concepts

**When adding relationships:**
- Choose appropriate type (is-a, uses, implements, etc.)
- Write description explaining the relationship
- Consider bidirectional relationships if applicable
- Think about transitive relationships (if A→B and B→C, is A→C meaningful?)

## Design Philosophy

### Simplicity First
- JSON for easy editing and inspection
- Flat file structure (no database required)
- Pure TypeScript with minimal dependencies
- CLI for immediate exploration

### Multiple Projections
The same graph supports different views:
- **Domain view** - Group by discipline
- **Tag view** - Organize by topics
- **Graph view** - Navigate relationships
- **Path view** - Discover connections

### Extensible
- Add new concepts without breaking existing structure
- New relationship types can be defined
- Search and traversal can be extended
- Easy to export to other formats (GraphML, Cypher, RDF)

## Future Enhancements

Potential extensions (not implemented yet):

1. **Visual Graph Rendering** - D3.js or Cytoscape.js visualization
2. **Semantic Search** - Embedding-based similarity search
3. **Pattern Detection** - Discover common motifs in relationships
4. **Export Formats** - GraphML, RDF, Neo4j Cypher
5. **Concept Clustering** - Automatic community detection
6. **Interactive Web UI** - Browser-based exploration
7. **Citation Tracking** - Link to papers, books, documentation
8. **Temporal Dimension** - Track concept evolution over time

## Related Work

This knowledge graph draws inspiration from:

- **Zettelkasten** - Personal knowledge management via linked notes
- **Roam Research / Obsidian** - Bidirectional linking and graph views
- **Semantic Web** - RDF, ontologies, linked data
- **Category Theory** - Mathematical study of relationships
- **Cognitive Maps** - Mental representation of knowledge
- **Concept Maps** - Visual knowledge representation

## Usage Tips

### Effective Search Strategies

1. **Start broad, narrow down**
   ```bash
   bun explore.ts domains              # See available domains
   bun explore.ts search --domain X    # Explore one domain
   bun explore.ts show concept-id      # Deep dive on one concept
   ```

2. **Use tags for cross-cutting themes**
   ```bash
   bun explore.ts search --tag concurrency
   bun explore.ts search --tag memory
   bun explore.ts search --tag fault-tolerance
   ```

3. **Explore neighborhoods**
   ```bash
   bun explore.ts explore concept-id 1  # Immediate neighbors
   bun explore.ts explore concept-id 2  # Two hops
   bun explore.ts explore concept-id 3  # Wider context
   ```

4. **Find conceptual bridges**
   ```bash
   bun explore.ts path cs-concept neuroscience-concept
   ```

### Common Queries

```bash
# Erlang/OTP ecosystem
bun explore.ts explore erlang-otp 2

# Functional programming concepts
bun explore.ts search --tag functional

# Graph-related concepts
bun explore.ts search graph

# System design patterns
bun explore.ts search --domain system-design

# Mathematical foundations
bun explore.ts search --domain mathematics
```

## License

Part of the tk-agents project. See main repository for license information.

## Contributing

To add concepts or improve the graph:

1. Edit `concepts.json` to add new concepts
2. Edit `relationships.json` to add connections
3. Test with `bun explore.ts stats` to verify
4. Submit changes with clear descriptions

Keep concepts focused, descriptions concise, and relationships meaningful.
