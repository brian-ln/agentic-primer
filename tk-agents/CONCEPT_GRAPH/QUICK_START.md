# Concept Graph - Quick Start Guide

## 5-Minute Tour

### 1. See what's in the graph

```bash
bun explore.ts stats
```

Output: 50 concepts, 61 relationships across 9 domains

### 2. Explore a domain

```bash
# See all domains
bun explore.ts domains

# Explore one domain
bun explore.ts search --domain computer-science
bun explore.ts search --domain neuroscience
```

### 3. Search by topic

```bash
# Find all concepts related to actors
bun explore.ts search actor

# Find fault-tolerance concepts
bun explore.ts search --tag fault-tolerance

# Search by keyword
bun explore.ts search memory
```

### 4. Deep dive on a concept

```bash
# Show full details
bun explore.ts show actor-model

# Show related concepts
bun explore.ts related erlang-otp
```

### 5. Explore connections

```bash
# Explore neighborhood (1-3 hops)
bun explore.ts explore message-passing 2

# Find path between concepts
bun explore.ts path graph-protocol memory-reconsolidation
```

## Common Use Cases

### "I want to understand Erlang/OTP patterns"

```bash
bun explore.ts explore erlang-otp 2
bun explore.ts show supervision-tree
bun explore.ts show gen-server
bun explore.ts search --tag erlang
```

### "How do memory concepts relate to system design?"

```bash
bun explore.ts search memory
bun explore.ts related access-pattern
bun explore.ts path event-sourcing memory-reconsolidation
```

### "What are the mathematical foundations?"

```bash
bun explore.ts search --domain mathematics
bun explore.ts show category-theory
bun explore.ts show lambda-calculus
```

### "Show me functional programming concepts"

```bash
bun explore.ts search --tag functional
bun explore.ts explore functional-programming 2
```

### "Find connections between disciplines"

```bash
# CS to neuroscience
bun explore.ts path graph-protocol memory-reconsolidation

# Theory to practice
bun explore.ts path category-theory erlang-otp

# Architecture patterns
bun explore.ts search --tag architecture
```

## Key Concepts by Domain

### Computer Science Core
- `actor-model` - Fundamental concurrency model
- `message-passing` - Communication primitive
- `state-machine` - Formal behavior modeling
- `graph-theory` - Structure and algorithms

### Erlang/OTP Ecosystem
- `erlang-otp` - Production actor platform
- `supervision-tree` - Fault recovery hierarchy
- `gen-server` - Server behavior pattern
- `gen-statem` - State machine behavior
- `let-it-crash` - Design philosophy

### System Design
- `fault-tolerance` - Resilience strategies
- `distributed-systems` - Networked computation
- `event-sourcing` - Immutable event logs
- `workflow-engine` - Task orchestration

### Neuroscience & Cognition
- `memory-reconsolidation` - Memory updating
- `synaptic-plasticity` - Learning mechanism
- `working-memory` - Temporary information store
- `cognitive-architecture` - Cognition models

### Mathematics & Theory
- `category-theory` - Composition and abstraction
- `lambda-calculus` - Computation foundation
- `set-theory` - Mathematical basis
- `graph-theory` - Structure mathematics

### Programming Paradigms
- `functional-programming` - Function-based computation
- `immutability` - Unchanging data
- `pure-functions` - Side-effect-free functions
- `pattern-matching` - Structural matching

## Exploration Strategies

### Start Broad
1. List domains: `bun explore.ts domains`
2. Pick one: `bun explore.ts search --domain X`
3. Dive deeper: `bun explore.ts show concept-id`

### Follow Connections
1. Start with a concept: `bun explore.ts show actor-model`
2. See related: `bun explore.ts related actor-model`
3. Explore neighborhood: `bun explore.ts explore actor-model 2`

### Find Bridges
1. Identify two distant concepts
2. Find path: `bun explore.ts path concept-A concept-B`
3. Explore intermediate concepts

### Topic Research
1. Search by tag: `bun explore.ts search --tag concurrency`
2. Look at each result: `bun explore.ts show concept-id`
3. Follow references to related concepts

## All Commands

```
stats                         # Graph statistics
domains                       # List all domains
tags                          # List all tags

search <keyword>              # Search by keyword
search --domain <domain>      # Search by domain
search --tag <tag>            # Search by tag

show <id>                     # Show concept details
related <id>                  # Show related concepts
explore <id> [depth]          # Explore neighborhood
path <from> <to>              # Find shortest path

help                          # Show help
```

## Tips

- **Tab completion**: Most shells support tab completion for commands
- **Pipe to less**: For long output, use `bun explore.ts ... | less`
- **Save output**: Redirect to file: `bun explore.ts ... > output.txt`
- **Chain searches**: Use xargs or scripting to process multiple concepts

## Next Steps

1. **Read the full README** - `/CONCEPT_GRAPH/README.md`
2. **Explore the data** - Look at `concepts.json` and `relationships.json`
3. **Use programmatically** - Import `search.ts` in TypeScript code
4. **Extend the graph** - Add new concepts and relationships
5. **Visualize** - Create graph visualizations (future enhancement)

## Useful Queries

```bash
# Overview
bun explore.ts stats
bun explore.ts domains
bun explore.ts tags

# Erlang deep dive
bun explore.ts explore erlang-otp 2
bun explore.ts search --tag erlang

# Memory systems
bun explore.ts search memory
bun explore.ts related access-pattern

# Theoretical foundations
bun explore.ts search --domain mathematics
bun explore.ts path category-theory functional-programming

# System patterns
bun explore.ts search --tag fault-tolerance
bun explore.ts explore supervision-tree 2

# Cross-disciplinary
bun explore.ts path actor-model memory-reconsolidation
bun explore.ts path lambda-calculus erlang-otp
```
