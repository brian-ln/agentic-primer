# Content Storage - Quick Reference

**Purpose:** Quick lookup for content storage architecture and usage

---

## Terminology Cheat Sheet

| Old Term | New Term | Meaning |
|----------|----------|---------|
| DeliverableActor | **ContentActor** | Agent-created document (metadata in graph) |
| deliverable_type | **content_type** | Type: completion, design, research, etc. |
| created_deliverable | **created_content** | Edge: agent → content |
| has_deliverable | **has_content** | Edge: task → content |
| Blob (document) | **Content/Document** | What agents create |
| Blob (storage) | **BlobActor** | Storage abstraction layer |

---

## Architecture Layers (Quick View)

```
Application Layer
  ↓ query
Content Layer (ContentActor, SectionActor)
  ↓ send(blob_id, "read/slice", {})
Blob Storage Layer (BlobActor)
  ↓ implementation choice
Implementation Layer (filesystem, S3, database, memory)
```

**Key Rule:** Content actors NEVER know WHERE or HOW content is stored.

---

## ContentActor Properties

**Stored in graph:**
```typescript
{
  content_id: string,
  title: string,
  content_type: "completion" | "design" | "research" | ...,
  blob_id: string,  // Reference to BlobActor
  size: number,
  hash: string,
  tags: string[],           // ["actor-model", "design"]
  embeddings: number[],     // [0.1, 0.2, ...] for semantic search
  section_index: [          // Byte offsets for on-demand reading
    {
      section_id: string,
      heading: string,
      depth: number,
      byte_offset: number,  // Start position
      byte_length: number,  // Section size
      tags?: string[],
      embeddings?: number[]
    }
  ]
}
```

---

## BlobActor Interface

**Messages content actors can send:**

```typescript
// Read full content
await graph.send(blob_id, "read", {});
// → { content: string }

// Read byte range (for sections)
await graph.send(blob_id, "slice", { offset: 500, length: 1200 });
// → { content: string }

// Validate integrity
await graph.send(blob_id, "validate", {});
// → { valid: boolean, checks: {...}, issues: string[] }
```

**Properties (hidden from content layer):**
```typescript
{
  blob_id: string,
  storage_ref: string,    // Generic reference (path, S3 key, DB ID)
  storage_type: string,   // "filesystem" | "s3" | "database" | "memory"
  size: number,
  hash: string,
  cache_policy?: string,  // "hot" | "warm" | "cold" (system-managed)
  last_accessed?: Date
}
```

---

## Common Operations

### 1. Create Content (Agent)

```typescript
// Agent creates content
const contentId = await createContent(graph, {
  agentId: "agent_bg_123",
  taskId: "task_28",
  content: markdownString,
  metadata: {
    title: "Design Document",
    type: "design",
    tags: ["architecture", "actors"]
  }
});
```

### 2. Search by Tags (No Content Read)

```typescript
// Query graph metadata only
const designDocs = contentNodes
  .filter(c => c.tags.includes("design"));
```

### 3. Semantic Search by Embeddings

```typescript
// 1. Search index (no content reads)
const matches = contentNodes
  .flatMap(c => c.section_index.map(s => ({
    ...s,
    similarity: cosineSimilarity(queryEmbedding, s.embeddings)
  })))
  .sort((a, b) => b.similarity - a.similarity)
  .slice(0, 10);

// 2. Read only top matches
for (const match of matches) {
  const content = await graph.send(
    match.blob_id,
    "slice",
    { offset: match.byte_offset, length: match.byte_length }
  );
}
```

### 4. Read Section On-Demand

```typescript
// Get section from index
const section = content.section_index.find(s => s.heading === "Overview");

// Read only that section
const { content: sectionContent } = await graph.send(
  blob_id,
  "slice",
  { offset: section.byte_offset, length: section.byte_length }
);
```

### 5. Export Content

```typescript
// Works regardless of backend
const { content } = await graph.send(blob_id, "read", {});
await Bun.write(outputPath, content);
```

---

## Graph Edges

```typescript
// Agent created content
graph.addEdge(agentId, contentId, "created_content", {
  createdAt: Date,
  purpose: string
});

// Task has content
graph.addEdge(taskId, contentId, "has_content", {
  linkedAt: Date,
  phase: string
});

// Content stored in blob
graph.addEdge(contentId, blobId, "stored_in_blob", {
  format: "markdown"
});

// Content contains section
graph.addEdge(contentId, sectionId, "contains_section", {
  order: number
});

// Section has parent (hierarchy)
graph.addEdge(childSectionId, parentSectionId, "parent_section", {
  depth: number
});
```

---

## Datalog Queries

```datalog
# Get all content for agent
?[content_id, title, type] :=
  *created_content{ from_id: $agent_id, to_id: content_id },
  *content{ id: content_id, title, content_type: type }

# Get all content for task
?[content_id, title, agent_id] :=
  *has_content{ from_id: $task_id, to_id: content_id },
  *content{ id: content_id, title },
  *created_content{ from_id: agent_id, to_id: content_id }

# Find content by type
?[content_id, title, tags] :=
  *content{ id: content_id, title, content_type: $type, tags }

# Get section hierarchy
?[section_id, heading, depth, parent_id] :=
  *contains_section{ from_id: $content_id, to_id: section_id },
  *section{ id: section_id, heading, depth },
  *parent_section{ from_id: section_id, to_id: parent_id }
```

---

## Migration Phases

### Phase 1: Hybrid (Filesystem + Graph)

```typescript
// Write file (existing)
await Bun.write(filename, content);

// Create ContentActor (new)
// BlobActor references existing file
```

### Phase 2: Graph Primary

```typescript
// BlobActor stores in docs/content/
// No write to project root
```

### Phase 3: Pure Graph

```typescript
// BlobActor chooses backend (system decision)
// Export to filesystem on-demand only
```

---

## Design Principles

1. **Content actors focus on metadata** (what, who, how to find)
2. **BlobActor abstracts storage** (hides where, how)
3. **System manages performance** (caching, placement)
4. **Index-based model** (metadata in graph, content on-demand)
5. **Message-based interaction** (ContentActor → BlobActor via messages)

---

## Files Reference

| File | Purpose |
|------|---------|
| CONTENT_STORAGE_ARCHITECTURE.md | High-level architecture overview |
| GRAPH_CONTENT_STORAGE_DESIGN.md | Detailed design specification |
| CONTENT_STORAGE_SCHEMA.datalog | Datalog schema |
| content-storage-examples.ts | Usage examples |
| CONTENT_STORAGE_QUICK_REF.md | This file |

---

## Common Mistakes to Avoid

❌ **Don't:** Have ContentActor manage storage location
✅ **Do:** Delegate to BlobActor via messages

❌ **Don't:** Have ContentActor decide caching strategy
✅ **Do:** Let system manage performance

❌ **Don't:** Read full content for tag searches
✅ **Do:** Query section_index in graph metadata

❌ **Don't:** Store full markdown in graph nodes
✅ **Do:** Store index (byte offsets) in graph, content in BlobActor

❌ **Don't:** Hardcode storage backend
✅ **Do:** Use BlobActor abstraction

---

**Quick Start:** See `examples/content-storage-examples.ts` for working code
