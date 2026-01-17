# Knowledge System Specification

## Overview

The Knowledge System provides a graph-based knowledge management capability for storing, querying, and synthesizing information. KnowledgeNode actors integrate with the Graph system to enable linking knowledge nodes together, creating rich knowledge graphs with traceable sources and versioning.

## Core Concepts

### KnowledgeNode

A KnowledgeNode is a NodeActor that stores information with:
- **Title**: Human-readable identifier for the knowledge unit
- **Content**: The actual knowledge content (text)
- **Sources**: Append-only list tracking the provenance of information
- **Version**: Monotonically increasing version number for change tracking

### Knowledge Properties

```typescript
interface KnowledgeProperties extends NodeProperties {
  type: "knowledge";
  title: string;
  content: string;
  sources: string[];
  version: number;
}
```

## Core Features

### 1. Knowledge Creation

Create knowledge nodes with initial content and sources:

```typescript
const knowledge = createKnowledge({
  title: "Actor Model Concepts",
  content: "The actor model is a mathematical model of concurrent computation...",
  sources: ["Hewitt 1973", "Agha 1986"]
}, graph);
```

**Properties:**
- Unique ID generated automatically (knowledge_N pattern)
- Initial version is always 1
- Content and sources are optional at creation
- CreatedAt timestamp recorded

### 2. Append Operation

Add content to existing knowledge with source tracking:

```typescript
knowledge.handleMessage({
  type: "append",
  payload: {
    data: "Additional information about message passing...",
    source: "Armstrong 2003"
  }
}, graph);
```

**Behavior:**
- Content concatenated with double-newline separator (if existing content present)
- Version incremented by 1
- Source added to sources list (if provided)
- Returns `{ success: true, version: N }`

**Invariant:** Version is monotonically increasing - never decreases.

### 3. Query Operation

Search knowledge content for relevant information:

```typescript
const result = knowledge.handleMessage({
  type: "query",
  payload: {
    question: "What is message passing?",
    context: { /* optional context */ }
  }
}, graph);
// Returns: { answer: string, confidence: number, sources: string[] }
```

**Behavior:**
- Extracts keywords from question (words > 3 characters)
- Performs keyword matching against content
- Calculates confidence score (matched keywords / total keywords)
- Extracts relevant snippet from first matching sentence
- Falls back to truncated content (200 chars) if no match
- Returns all sources associated with the knowledge

**Confidence Calculation:**
```
confidence = matchedKeywords / totalKeywords
```
Where:
- `matchedKeywords` = count of keywords found in content (case-insensitive)
- `totalKeywords` = count of words in question with length > 3

### 4. Synthesize Operation

Combine content from multiple knowledge nodes:

```typescript
const result = knowledge.handleMessage({
  type: "synthesize",
  payload: {
    fromNodes: ["knowledge_2", "knowledge_3"]
  }
}, graph);
// Returns: { synthesis: string, sources: string[] }
```

**Behavior:**
- Collects content from this node and specified nodes
- Filters out empty content
- Formats with source headers: `[Source N]\n{content}`
- Separates sources with dividers: `\n\n---\n\n`
- Deduplicates sources using Set
- Returns combined synthesis and unique sources

### 5. Standard NodeActor Messages

KnowledgeNode supports all standard NodeActor messages:

| Message | Description | Response |
|---------|-------------|----------|
| `get` | Retrieve full node state | `{ id, type, properties, edges }` |
| `observe` | Get human-readable summary | `{ state, observations, metadata }` |
| `update` | Update properties | `{ success, updatedProperties }` |
| `link` | Create edge to another node | `{ edgeId, success }` |
| `unlink` | Remove an edge | `{ success }` |
| `delete` | Remove node from graph | `{ success }` |

## Usage Examples

### Creating and Building Knowledge

```typescript
import { Graph } from "./graph";
import { createKnowledge } from "./knowledge";

const graph = new Graph();

// Create initial knowledge
const actorKnowledge = createKnowledge({
  title: "Actor Model",
  content: "Actors are the fundamental units of computation.",
  sources: ["Hewitt 1973"]
}, graph);

// Append more information
actorKnowledge.handleMessage({
  type: "append",
  payload: {
    data: "Each actor has a mailbox for receiving messages.",
    source: "Agha 1986"
  }
}, graph);

// Version is now 2, sources include both papers
```

### Querying Knowledge

```typescript
const result = actorKnowledge.handleMessage({
  type: "query",
  payload: {
    question: "What is an actor?"
  }
}, graph);

// result.answer contains relevant snippet
// result.confidence indicates match quality (0.0 - 1.0)
// result.sources lists provenance
```

### Linking Knowledge Nodes

```typescript
const erlangKnowledge = createKnowledge({
  title: "Erlang Processes",
  content: "Erlang processes are lightweight and communicate via message passing.",
  sources: ["Armstrong 2003"]
}, graph);

// Link knowledge nodes
actorKnowledge.handleMessage({
  type: "link",
  payload: {
    toId: erlangKnowledge.properties.id,
    edgeType: "references"
  }
}, graph);
```

### Synthesizing Multiple Sources

```typescript
const synthesis = actorKnowledge.handleMessage({
  type: "synthesize",
  payload: {
    fromNodes: [erlangKnowledge.properties.id]
  }
}, graph);

// synthesis.synthesis contains formatted combined content
// synthesis.sources contains deduplicated sources
```

## Integration Points

### Graph System

KnowledgeNodes integrate with the Graph system:
- Registered via `graph.registerNode()`
- Edges managed via Graph's `addEdge()` and `removeEdge()`
- Retrieved via `graph.getNode()` and `graph.getAllEdges()`

### Edge Types

Recommended edge types for knowledge graphs:
- `references` - Knowledge A references Knowledge B
- `requires_knowledge` - Task requires this knowledge
- `produces` - Task produces this knowledge as artifact

### Task Integration

Tasks can reference knowledge nodes:
```typescript
task.handleMessage({
  type: "link",
  payload: {
    toId: knowledge.properties.id,
    edgeType: "requires_knowledge"
  }
}, graph);
```

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| create | O(1) | Counter increment + registration |
| append | O(n) | String concatenation where n = content length |
| query | O(k * n) | k = keywords, n = content length |
| synthesize | O(m * n) | m = nodes, n = avg content length |
| get | O(e) | e = number of edges |
| link/unlink | O(1) | Delegated to Graph |

### Space Complexity

- Content stored as single string (no compression)
- Sources stored as array (append-only)
- Version is single integer

## Error Handling

### Unknown Message Type
Throws `Error` with message: `Unknown message type: {type}`

### Missing Node in Synthesize
If a referenced node doesn't exist or isn't a knowledge node:
- Node is silently skipped
- No error thrown
- Only valid knowledge content included in synthesis

### Update Protection
Protected fields (cannot be updated):
- `id`
- `type`
- `createdAt`

## State Management

### KnowledgeNode Lifecycle

```
Created -> Active -> (optionally) Deleted
```

**States:**
- **Created**: Node instantiated with initial properties
- **Active**: Node registered in graph, accepting messages
- **Deleted**: Node removed from graph via delete message

### Version Tracking

```
v1 (initial) -> v2 (after append) -> v3 (after append) -> ...
```

**Invariants:**
- Version starts at 1
- Version increments exactly by 1 on each append
- Version never decreases
- Version is the only mechanism for tracking content changes

### Source Management

```
[initial sources] -> [+ source from append] -> [+ source from append] -> ...
```

**Invariants:**
- Sources list is append-only
- Sources are never removed or modified
- Duplicate sources may exist (no deduplication on append)
- Synthesize deduplicates across nodes

## Message Interface

### Append Message

```typescript
{
  type: "append",
  payload: {
    data: string;      // Required: content to append
    source?: string;   // Optional: source attribution
  }
}
```

**Response:**
```typescript
{
  success: boolean;
  version: number;     // New version after append
}
```

### Query Message

```typescript
{
  type: "query",
  payload: {
    question: string;                    // Required: question to answer
    context?: Record<string, unknown>;   // Optional: additional context
  }
}
```

**Response:**
```typescript
{
  answer: string;      // Relevant snippet or fallback
  confidence: number;  // 0.0 - 1.0 match quality
  sources: string[];   // All sources for this knowledge
}
```

### Synthesize Message

```typescript
{
  type: "synthesize",
  payload: {
    fromNodes: string[];  // Required: node IDs to include
  }
}
```

**Response:**
```typescript
{
  synthesis: string;   // Combined, formatted content
  sources: string[];   // Deduplicated sources from all nodes
}
```

## Invariants

### 1. Version Monotonicity Invariant
```
forall append operations A1, A2 where A1 happens-before A2:
  version_after(A1) < version_after(A2)
```

### 2. Source Append-Only Invariant
```
forall time t1 < t2:
  sources_at(t1) is-subset-of sources_at(t2)
```

### 3. Content Accumulation Invariant
```
forall append operations with data D:
  content_after contains D
```

### 4. ID Immutability Invariant
```
forall knowledge nodes K:
  K.id at creation == K.id at any later time
```

### 5. Type Immutability Invariant
```
forall knowledge nodes K:
  K.type == "knowledge" at all times
```

## Non-Requirements

What this spec does NOT require:
- Full-text search indexing
- Semantic/embedding-based search
- Content compression
- Source deduplication on append
- Version conflict resolution
- Distributed knowledge graphs
- Knowledge persistence/serialization

## Verification

### Manual Verification Checklist

- [ ] KnowledgeNode implements NodeActor interface
- [ ] ID format is `knowledge_N` with incrementing counter
- [ ] Version starts at 1 and increments on append
- [ ] Sources are append-only (never removed)
- [ ] Query returns confidence between 0 and 1
- [ ] Synthesize deduplicates sources
- [ ] Protected fields cannot be updated
- [ ] Unknown message types throw errors

### Datalog Verification

Load `KNOWLEDGE_SYSTEM.spec.datalog` and run:
```prolog
% Verify all invariants hold
?- all_invariants_hold.

% Check version monotonicity
?- version_monotonicity_invariant.

% Verify source append-only property
?- source_append_only_invariant.
```

## Summary

**Key insights:**

1. **Content Accumulation** - Append-only content model with version tracking
2. **Source Provenance** - Every piece of information has traceable sources
3. **Simple Query** - Keyword-based search with confidence scoring
4. **Synthesis** - Combine multiple knowledge nodes with source deduplication
5. **Graph Integration** - Full NodeActor compliance for graph-based knowledge management
