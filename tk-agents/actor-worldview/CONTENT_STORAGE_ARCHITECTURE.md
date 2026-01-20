# Content Storage Architecture - Refined Design

**Date:** 2026-01-18
**Status:** Architecture Specification
**Purpose:** Document three-layer content storage architecture with clear separation of concerns

---

## Executive Summary

This document clarifies the architecture for storing agent deliverables (completion reports, designs, research) in the graph system. Based on user feedback, the design emphasizes:

1. **Correct terminology:** "Content" or "Document", not "Deliverable" or "Blob"
2. **Layer separation:** Content Layer → Blob Storage Layer → Implementation
3. **Index-based model:** Metadata in graph, content via BlobActor abstraction
4. **System-managed performance:** Content actors don't manage caching or placement

---

## Three-Layer Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      APPLICATION LAYER                          │
│  CLI, Dashboard, Queries                                        │
│  - List content by agent                                        │
│  - Search by tags/embeddings                                    │
│  - Export to filesystem                                         │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                       CONTENT LAYER                             │
│  High-level concerns: metadata, relationships, discovery        │
│                                                                 │
│  ContentActor (graph node)                                      │
│  ├─ Metadata: content_id, title, type, size, hash              │
│  ├─ Tags: ["design", "actor-model"]                            │
│  ├─ Embeddings: [0.1, 0.2, ...] (semantic search)              │
│  ├─ Section Index: [{ heading, byte_offset, byte_length }]     │
│  └─ Methods: search(), read(), export()                         │
│                                                                 │
│  SectionActor (graph node)                                      │
│  ├─ Metadata: section_id, heading, depth                       │
│  ├─ Byte offsets: byte_offset, byte_length                     │
│  ├─ Tags/embeddings: section-specific metadata                 │
│  └─ Methods: getContent() → asks BlobActor                      │
│                                                                 │
│  Graph Edges:                                                   │
│  ├─ agent → content (created_content)                           │
│  ├─ task → content (has_content)                                │
│  └─ content → section (contains_section)                        │
│                                                                 │
│  Does NOT know: WHERE or HOW content is stored                  │
│  Delegates to: BlobActor via messages                           │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              │ send(blob_id, "read", {})
                              │ send(blob_id, "slice", {offset, length})
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    BLOB STORAGE LAYER                           │
│  Storage abstraction: hides implementation from content layer   │
│                                                                 │
│  BlobActor Interface:                                           │
│  ├─ store(content_id, bytes) → blob_id                          │
│  ├─ retrieve(blob_id) → bytes                                   │
│  ├─ slice(blob_id, offset, length) → bytes                      │
│  └─ delete(blob_id)                                             │
│                                                                 │
│  Properties (hidden from content layer):                        │
│  ├─ storage_ref: generic reference (path, S3 key, DB ID)        │
│  ├─ storage_type: "filesystem" | "s3" | "database" | "memory"   │
│  ├─ cache_policy: "hot" | "warm" | "cold"                       │
│  └─ last_accessed: Date (for cache eviction)                    │
│                                                                 │
│  Does NOT expose: WHERE stored, WHICH backend                   │
│  Managed by: System (performance, caching, placement)           │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              │ Implementation choice
                              │ (transparent to upper layers)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   IMPLEMENTATION LAYER                          │
│  BlobActor chooses backend, content actors don't care           │
│                                                                 │
│  Option 1: Filesystem                                           │
│  └─ docs/content/{content_id}.md                                │
│                                                                 │
│  Option 2: S3 / Blob Store                                      │
│  └─ s3://bucket/content/{content_id}.md                         │
│                                                                 │
│  Option 3: Database (CozoDB, SQLite)                            │
│  └─ SELECT content FROM blobs WHERE id = ?                      │
│                                                                 │
│  Option 4: In-memory (testing, ephemeral)                       │
│  └─ Map<blob_id, bytes>                                         │
│                                                                 │
│  System manages:                                                │
│  ├─ Placement: which backend for which content                  │
│  ├─ Caching: frequently accessed content in memory              │
│  ├─ Performance: monitoring, optimization                       │
│  └─ Cost: S3 tiers, compression, deduplication                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Key Principles

### 1. Terminology

**ContentActor / DocumentActor** (not DeliverableActor)
- Represents agent-created documents
- Stores metadata in graph
- Delegates storage to BlobActor

**BlobActor** (storage abstraction, not "blob" as in "document")
- Abstracts WHERE and HOW content is stored
- Provides uniform interface (read, slice, write)
- Hides implementation details

### 2. Layer Separation

**Content Layer concerns:**
- What is this document? (title, type, tags)
- Who created it? (agent, task relationships)
- How do I find it? (semantic search, tags)
- What sections does it have? (index metadata)

**Content Layer does NOT care:**
- WHERE is content stored? (filesystem, S3, database)
- HOW is content cached? (system decision)
- WHICH backend is used? (BlobActor decision)
- Performance tuning (system responsibility)

**Blob Storage Layer concerns:**
- Store bytes reliably
- Retrieve bytes efficiently
- Support byte-range requests (slicing)
- Choose appropriate backend

**Implementation Layer concerns:**
- Physical storage (disk, cloud, memory)
- Backend-specific optimizations
- Cost management (S3 tiers, compression)

### 3. Index-Based Model

**Metadata lives in graph:**
```typescript
// ContentActor properties (in graph)
{
  content_id: "content_123",
  title: "Actor Model Design",
  content_type: "design",
  tags: ["architecture", "actors"],
  embeddings: [0.1, 0.2, ...],  // For semantic search

  // Section index (byte offsets)
  section_index: [
    {
      section_id: "sec_1",
      heading: "Overview",
      depth: 1,
      byte_offset: 0,
      byte_length: 500,
      tags: ["intro"],
      embeddings: [0.15, 0.22, ...]
    },
    // ...
  ]
}
```

**Content lives in BlobActor:**
```typescript
// Read full content
const { content } = await graph.send(blob_id, "read", {});

// Read section on-demand
const { content } = await graph.send(
  blob_id,
  "slice",
  { offset: 500, length: 1200 }
);
```

**Benefits:**
- Search without reading content (tags, embeddings in graph)
- On-demand content loading (only when needed)
- Flexible metadata evolution (add tags without re-storing content)
- Scalable (large documents don't bloat graph memory)

### 4. System-Managed Performance

**Content actors send messages:**
```typescript
// ContentActor doesn't manage performance
const content = await graph.send(blob_id, "read", {});
```

**System decides:**
- Caching strategy (hot/warm/cold)
- Placement (which backend for which content)
- Performance monitoring (latency, throughput)
- Cost optimization (S3 tiers, compression)

**Example: System detects frequently accessed content**
```typescript
// System observes access patterns
BlobActor notices: content_123 accessed 50 times/hour

// System decides: promote to hot cache
system.updateCachePolicy(blob_id, "hot");

// Future reads served from memory (transparent to ContentActor)
```

---

## Usage Examples

### Example 1: Agent Creates Content

```typescript
// Agent completes work
const agentId = "agent_bg_a7e4d2";
const taskId = "task_28";
const reportContent = `# Completion Report\n\n...`;

// Create content in graph
const contentId = await createContent(graph, {
  agentId,
  taskId,
  content: reportContent,
  metadata: {
    title: "Task 28 Completion Report",
    type: "completion",
    tags: ["actor-model", "completion"]
  }
});

// Under the hood:
// 1. ContentActor created (metadata in graph)
// 2. BlobActor stores content (filesystem, S3, etc.)
// 3. Section index computed (byte offsets)
// 4. Edges created (agent → content, task → content)
```

### Example 2: Search by Tags (No Content Read)

```typescript
// Find all design documents
const designDocs = await graph.query(`
  ?[content_id, title, tags] :=
    *content{ id: content_id, title, tags, content_type: "design" }
`);

// No blob reads needed - metadata in graph
```

### Example 3: Semantic Search by Embeddings

```typescript
// Find sections similar to query
const queryEmbedding = await embedText("actor model patterns");

// Search section index (no content reads yet)
const matches = contentNodes
  .flatMap(c => c.section_index.map(s => ({
    content_id: c.content_id,
    section_id: s.section_id,
    heading: s.heading,
    similarity: cosineSimilarity(queryEmbedding, s.embeddings)
  })))
  .sort((a, b) => b.similarity - a.similarity)
  .slice(0, 5);

// NOW read only top 5 matching sections
for (const match of matches) {
  const content = await graph.send(
    match.blob_id,
    "slice",
    { offset: match.byte_offset, length: match.byte_length }
  );
  console.log(content);
}
```

### Example 4: Read Section On-Demand

```typescript
// Navigate to specific section
async function readSection(contentId: string, sectionId: string) {
  // Get content metadata
  const content = graph.getNodeProperties(contentId);

  // Find section in index
  const section = content.section_index.find(s => s.section_id === sectionId);

  // Get blob reference
  const blobEdge = graph.getEdgesFrom(contentId)
    .find(e => e.type === "stored_in_blob");

  // Ask BlobActor for section content (on-demand)
  const { content: sectionContent } = await graph.send(
    blobEdge.toId,
    "slice",
    {
      offset: section.byte_offset,
      length: section.byte_length
    }
  );

  return sectionContent;
}
```

### Example 5: Export to Filesystem

```typescript
// Export content to file (regardless of where stored)
async function exportContent(contentId: string, outputPath: string) {
  // Get blob reference
  const blobEdge = graph.getEdgesFrom(contentId)
    .find(e => e.type === "stored_in_blob");

  // Read full content (BlobActor handles WHERE it's stored)
  const { content } = await graph.send(blobEdge.toId, "read", {});

  // Write to filesystem
  await Bun.write(outputPath, content);
}

// Works regardless of BlobActor backend:
// - If filesystem: copy file
// - If S3: download then write
// - If database: query then write
// - If memory: serialize then write
```

---

## Migration Path

### Phase 1: Hybrid (Filesystem + Graph)

**Goal:** Validate design without breaking existing workflows

**Implementation:**
- Agents write files to project root (existing behavior)
- ALSO create ContentActor in graph
- BlobActor references existing files
- Validate consistency

**Code:**
```typescript
// Hybrid approach
async function createContent(agentId, taskId, content, metadata) {
  // Write to filesystem (existing)
  const filename = `${metadata.type.toUpperCase()}_${taskId}.md`;
  await Bun.write(filename, content);

  // Create ContentActor (new)
  const contentId = `content_${Date.now()}`;

  // BlobActor references existing file
  const blobId = await BlobActor.store(contentId, content, {
    backend: "filesystem",
    path: filename
  });

  // Create graph nodes
  graph.registerNode(contentId, ContentActor(...), {
    content_id: contentId,
    title: metadata.title,
    blob_id: blobId,
    tags: metadata.tags,
    section_index: await computeSectionIndex(content)
  });

  // Create edges
  graph.addEdge(agentId, contentId, "created_content", {...});
  graph.addEdge(taskId, contentId, "has_content", {...});
}
```

### Phase 2: Graph Primary

**Goal:** Move content into dedicated storage, deprecate scattered files

**Implementation:**
- BlobActor stores in `docs/content/` directory
- Agents use graph API exclusively
- Old files remain for backward compatibility
- CLI queries graph, not filesystem

**Code:**
```typescript
// Graph primary
async function createContent(agentId, taskId, content, metadata) {
  const contentId = `content_${Date.now()}`;

  // BlobActor stores in dedicated directory
  const blobId = await BlobActor.store(contentId, content, {
    backend: "filesystem",
    path: `docs/content/${contentId}.md`
  });

  // Create ContentActor (graph)
  graph.registerNode(contentId, ContentActor(...), {
    content_id: contentId,
    title: metadata.title,
    blob_id: blobId,
    tags: metadata.tags,
    embeddings: await embedContent(content),
    section_index: await computeSectionIndex(content)
  });

  // Create edges
  graph.addEdge(agentId, contentId, "created_content", {...});
  graph.addEdge(taskId, contentId, "has_content", {...});

  // NO write to project root
}
```

### Phase 3: Pure Graph Storage

**Goal:** Remove filesystem dependency, use optimal backend

**Implementation:**
- BlobActor chooses backend (S3, database, memory)
- System manages placement (hot → memory, cold → S3)
- Export to filesystem on-demand only
- Full actor model compliance

**Code:**
```typescript
// Pure graph storage
async function createContent(agentId, taskId, content, metadata) {
  const contentId = `content_${Date.now()}`;

  // BlobActor chooses backend (system decision)
  const blobId = await BlobActor.store(contentId, content, {
    // System decides: memory, S3, database, etc.
    let_system_choose: true
  });

  // Rest same as Phase 2
}

// Export on-demand
async function exportContent(contentId, outputPath) {
  const blobId = getBlobId(contentId);
  const { content } = await graph.send(blobId, "read", {});
  await Bun.write(outputPath, content);
}
```

---

## Decision Log

### Why "Content" not "Deliverable"?

**User feedback:**
> "I think a better name would be 'content' or 'document' or ... not blob."

**Decision:** Use **ContentActor** (or DocumentActor)
- More general purpose (not just agent deliverables)
- Clearer separation from BlobActor (storage abstraction)
- Aligns with common terminology (CMS, documentation systems)

### Why Layer Separation?

**User feedback:**
> "They may ride on top of the blob actor to do the actual storage and access. Assume that it could be filesystem or an actual blob store or other datastore that our document/content actors can not have to worry about."

**Decision:** Three layers (Content → Blob Storage → Implementation)
- Content actors focus on metadata, relationships, discovery
- BlobActor abstracts storage implementation
- System manages performance, placement, caching
- Decouples concerns, enables flexibility

### Why Index-Based Model?

**User feedback:**
> "The Markdown as a tree might be in memory or might be created on the fly or just an index into the file with metadata attached (like tags, embedding vectors, links, ...)."

**Decision:** Index-based approach
- Metadata (tags, embeddings, byte offsets) in graph
- Content data in BlobActor
- Parse sections on-demand using byte offsets
- Best balance: queryable, efficient, flexible

### Why System-Managed Performance?

**User feedback:**
> "Even the storage and retrieval speed are not their concerns and need to be managed by the system, success criteria of the engagements, etc."

**Decision:** System manages performance, not content actors
- Content actors send messages (simple interface)
- System observes access patterns
- System optimizes caching, placement
- System monitors against success criteria

---

## Success Criteria

### Design Phase (Now)

**MUST:**
- Terminology corrected (Content, not Deliverable)
- Layer separation documented
- Index-based model specified
- System management boundary clear

**SHOULD:**
- Examples demonstrate all three layers
- Migration path defined
- Performance strategy outlined

**MAY:**
- Backend comparison (filesystem, S3, database)
- Cost analysis (storage, bandwidth)

### Implementation Phase (Future)

**MUST:**
- ContentActor implemented
- BlobActor abstraction layer implemented
- Section index computation working
- On-demand section reading working

**SHOULD:**
- Multiple backends supported (filesystem, memory)
- Tag/embedding search working
- Migration script from filesystem
- CLI integration complete

**MAY:**
- S3 backend
- CozoDB backend
- System performance monitoring
- Automatic cache management

---

## References

**Related Designs:**
- BLOB_STORAGE_ACTOR_REDESIGN.md (BlobActor implementation)
- GRAPH_CONTENT_STORAGE_DESIGN.md (this architecture, detailed)
- actor-worldview/README.md (actor model principles)

**User Feedback:**
- Terminology: "content" or "document", not "blob"
- Layer separation: Content → Blob → Implementation
- Index-based: metadata + byte offsets
- System-managed: performance, caching, placement

**Implementation Files:**
- src/content.ts (ContentActor factory)
- src/section.ts (SectionActor factory)
- src/blob.ts (BlobActor abstraction)
- actor-worldview/CONTENT_STORAGE_SCHEMA.datalog (Datalog schema)
- examples/content-storage-examples.ts (usage examples)

---

**Status:** Architecture Complete - Ready for Implementation
**Next Step:** Create tasks for Phase 1 (Hybrid) implementation
**Estimated Effort:** 2-3 days (Phase 1), 1-2 days (Phase 2), 1 day (Phase 3)
