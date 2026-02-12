# Knowledge Integration Layer

**Status:** ✅ Complete
**Bead:** simplify-blq
**Date:** 2026-02-05

## Overview

Integrates **KnowledgeActor** and **RelationshipActor** with the query/DSL system, enabling semantic search, graph traversal, and epistemic reasoning over the knowledge base.

## Architecture

### Components

1. **KnowledgePatternBuilder** (`src/query/knowledge-integration.ts`)
   - Type-safe pattern builders for knowledge entities
   - Shortcuts: `decisions()`, `learnings()`, `errors()`
   - Filters: epistemic level, confidence, session, category
   - Relationship constraints with strength filtering

2. **Semantic Search** (`searchKnowledge()`)
   - Vector embeddings via `EmbeddingGenerator`
   - Similarity-based retrieval
   - Multi-dimensional filtering (category, confidence, session)

3. **Graph Traversal** (`traverseKnowledge()`)
   - BFS traversal over relationship edges
   - Relationship type filtering
   - Direction control (inbound, outbound, both)
   - Depth limiting with strength thresholds

4. **Knowledge Actors Integration**
   - `KnowledgeActor`: Manages epistemic knowledge items
   - `RelationshipActor`: Manages knowledge graph edges
   - `LibSQLKnowledgeStore`: Persistent storage layer

## API

### Pattern Builders

```typescript
// Generic knowledge pattern
knowledge('k')
  .category('decision')
  .epistemicLevel('know')
  .minConfidence(0.95)
  .sessionId('session-123')

// Category-specific shortcuts
decisions('dec').epistemicLevel('believe')
learnings('learn').sessionId('session-456')
errors('err').minConfidence(0.8)

// Relationship constraints
knowledge('k1').relatedTo('k2', {
  type: 'supports',
  direction: 'inbound',
  minStrength: 0.8
})
```

### Semantic Search

```typescript
const results = await searchKnowledge({
  query: 'authentication patterns',
  category: 'decision',
  minConfidence: 0.9,
  limit: 10,
  threshold: 0.7
});
```

### Graph Traversal

```typescript
const network = await traverseKnowledge({
  from: '@(knowledge/decisions/dec-123)',
  relationshipType: 'supports',
  direction: 'inbound',
  maxDepth: 3,
  minStrength: 0.8
});
```

### Query Integration

```typescript
query()
  .match(
    decisions('dec')
      .epistemicLevel('know')
      .minConfidence(0.95)
  )
  .traverse(
    knowledgeTraversal('dec', {
      relationshipType: 'supports',
      direction: 'inbound',
      maxDepth: 3,
      as: 'evidence'
    })
  )
  .return(['dec', 'evidence'])
```

## Query Examples

### 1. High-Confidence Decisions

```typescript
query()
  .match(
    decisions('dec')
      .epistemicLevel('know')
      .minConfidence(0.95)
  )
  .return(['dec'])
```

### 2. Session Learnings

```typescript
query()
  .match(learnings('learn').sessionId('session-123'))
  .return(['learn'])
```

### 3. Decision Evidence Network

```typescript
query()
  .match(decisions('dec').id('dec-123'))
  .traverse(
    knowledgeTraversal('dec', {
      relationshipType: 'supports',
      direction: 'inbound',
      maxDepth: 3,
      as: 'evidence'
    })
  )
  .return(['dec', 'evidence'])
```

### 4. Find Contradictions

```typescript
query()
  .match(
    knowledge('k1'),
    knowledge('k2').relatedTo('k1', {
      type: 'contradicts',
      direction: 'outbound'
    })
  )
  .return(['k1', 'k2'])
```

### 5. Validate Uncertain Knowledge

```typescript
query()
  .match(
    knowledge('k')
      .epistemicLevel('suspect')
      .minConfidence(0.6)
  )
  .forEach(send('@(knowledge)').ask('validate', { variable: 'k' }))
```

### 6. Knowledge Gaps

```typescript
query()
  .match(
    decisions('dec')
      .epistemicLevel('believe')
      .notExists(
        knowledge('evidence')
          .relatedTo('dec', {
            type: 'supports',
            direction: 'inbound'
          })
      )
  )
  .return(['dec'])
```

### 7. Epistemic Analytics

```typescript
query()
  .match(knowledge('k'))
  .aggregate({
    operation: 'group',
    variable: 'k',
    by: 'epistemic_level',
    as: 'levelCounts'
  })
  .return(['levelCounts'])
```

## Epistemic Levels

Knowledge items are classified by epistemic confidence:

| Level | Range | Description |
|-------|-------|-------------|
| `reject` | 0.00-0.20 | Actively disbelieved |
| `doubt` | 0.20-0.40 | Strong uncertainty |
| `wonder` | 0.40-0.60 | Neutral / exploring |
| `suspect` | 0.60-0.80 | Tentative belief |
| `believe` | 0.80-0.95 | Strong belief |
| `know` | 0.95-1.00 | Near certainty |

## Relationship Types

Knowledge graph relationships:

- `supports`: Evidence/reasoning supports conclusion
- `contradicts`: Evidence contradicts claim
- `requires`: Depends on other knowledge
- `extends`: Builds upon other knowledge
- `questions`: Raises doubt about claim
- `related-to`: General relationship

## Testing

**62 tests** covering:
- Pattern builder API (22 tests)
- Query integration (8 tests)
- Semantic search (7 tests)
- Graph traversal (4 tests)
- Address utilities (5 tests)
- Complex scenarios (10 tests)
- Edge cases (6 tests)

Run tests:
```bash
bun test src/query/knowledge-integration.test.ts
```

## Files Created

1. **`src/query/knowledge-integration.ts`** - Integration layer (350 lines)
2. **`src/query/knowledge-integration.test.ts`** - Comprehensive tests (500+ lines, 62 tests)
3. **`src/query/knowledge-demo.ts`** - Live demonstration
4. **`src/query/examples.ts`** - 15 new knowledge examples (56-70)
5. **`src/query/index.ts`** - Updated exports

## Integration Points

### Existing Actors
- **KnowledgeActor** (`src/messaging/actors/knowledge.ts`)
  - Handles: create, get, query, update, add-evidence, update-confidence
  - Storage: LibSQLKnowledgeStore
  - Epistemic promotion logic

- **RelationshipActor** (`src/messaging/actors/relationship.ts`)
  - Handles: create, upsert, get, query, traverse, delete
  - BFS graph traversal
  - Relationship strength filtering

### Storage Layer
- **LibSQLKnowledgeStore** (`src/storage/LibSQLKnowledgeStore.ts`)
  - Adapts to session-knowledge schema
  - Tables: `session_decisions`, `session_learnings`, `session_errors`
  - Relationships: `knowledge_relationships`

### Embeddings
- **EmbeddingGenerator** (`src/session-knowledge/embeddings/`)
  - Generates vector embeddings
  - Used for semantic search

- **VectorStoreLibSQL** (`src/session-knowledge/embeddings/`)
  - Vector similarity search
  - Cosine distance calculation

## Future Enhancements

1. **Semantic Search**
   - Dedicated knowledge embeddings (currently uses message embeddings)
   - Batch indexing for knowledge items
   - Hybrid search (keyword + semantic)

2. **Graph Analytics**
   - PageRank for knowledge importance
   - Community detection in knowledge clusters
   - Path analysis for evidence chains

3. **Epistemic Reasoning**
   - Auto-promotion based on evidence accumulation
   - Contradiction detection and resolution
   - Confidence propagation through relationships

4. **Query Optimization**
   - Knowledge-specific index hints
   - Relationship cardinality estimation
   - Adaptive traversal depth

## Success Metrics

- ✅ **62 tests passing** (100% success rate)
- ✅ **645 total query tests** passing
- ✅ **Type-safe API** with full TypeScript support
- ✅ **Knowledge patterns** integrated with existing query DSL
- ✅ **Graph traversal** working via relationship edges
- ✅ **Semantic search** foundation in place
- ✅ **Examples** demonstrating all features
- ✅ **Zero breaking changes** to existing query API

## Related Work

- **Bead simplify-jo5**: Ask messaging (M1) - Required for knowledge queries
- **Session Knowledge System**: Provides data source and storage
- **Epistemic Gradients**: Theory underlying confidence levels
- **Actor Fabric**: Messaging infrastructure for knowledge actors

## Usage Example

```typescript
import {
  query,
  decisions,
  learnings,
  knowledgeTraversal,
  searchKnowledge
} from '@simplify/query';

// 1. Find high-confidence decisions
const highConf = query()
  .match(decisions('dec').epistemicLevel('know').minConfidence(0.95))
  .return(['dec']);

// 2. Semantic search
const similar = await searchKnowledge({
  query: 'authentication patterns',
  category: 'decision',
  limit: 5
});

// 3. Traverse evidence network
const evidence = query()
  .match(decisions('dec').id('dec-123'))
  .traverse(knowledgeTraversal('dec', {
    relationshipType: 'supports',
    direction: 'inbound',
    maxDepth: 3
  }))
  .return(['dec', 'evidence']);

// 4. Link knowledge
const linked = query()
  .match(
    decisions('dec').id('dec-123'),
    learnings('learn').sessionId('session-456')
  )
  .createRelationship('learn', 'dec', {
    type: 'supports',
    properties: { strength: 0.9 }
  })
  .return(['dec', 'learn']);
```

## Conclusion

KnowledgeActor integration complete. The query/DSL layer now supports:
- Declarative knowledge queries with epistemic reasoning
- Semantic search over knowledge base
- Graph traversal via typed relationships
- Knowledge gap detection and validation workflows
- Full integration with existing actor fabric

Ready for Phase 3 Wave 3 (event triggers and stream messaging).
