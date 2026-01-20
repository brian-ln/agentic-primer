# Graph Content Storage Design

**Date:** 2026-01-18
**Status:** Design Specification (Revised)
**Priority:** P2 - Architecture
**Purpose:** Replace filesystem-based agent contents with graph storage using layered content/blob architecture and index-based markdown model

---

## Executive Summary

**Problem:** Agent contents currently scatter markdown files across project root directory (100+ files). This creates:
- Filesystem pollution
- No queryable relationships (which agent created what?)
- No hierarchical navigation (sections, headings)
- Manual cleanup required
- Lost context after conversation compaction

**Solution:** Store contents in graph using layered architecture:
1. **Content Layer** - ContentActor manages metadata, tags, embeddings (graph nodes)
2. **Blob Storage Layer** - BlobActor abstracts storage implementation (filesystem, S3, database)
3. **Index-based Model** - Metadata + byte offsets for on-demand section reading
4. **System Management** - Performance, caching, placement managed by system (not content actors)

**Benefits:**
- Queryable: "Show all contents for agent X"
- Navigable: Index-based section access with on-demand parsing
- Persistent: Metadata in graph, content via BlobActor abstraction
- Clean: No filesystem pollution
- Auditable: Full lineage tracking
- Flexible: BlobActor can use any storage backend
- Performant: System manages caching and performance

---

## Current State

### Filesystem-Based Deliverables

**Current workflow:**
```bash
# Agent completes work
Agent → Creates file in project root
      → ACTOR_POC_COMPLETION_REPORT.md
      → BLOB_STORAGE_DESIGN.md
      → GRAPH_QUERY_RESEARCH.md
      → ... (100+ files)

# Problems:
- No link to agent that created it
- No link to task that spawned agent
- Hard to find contents for specific work
- Manual cleanup required
- Lost after compaction
```

**Current contents directory:**
```
/Users/.../tk-agents/
├── ACTOR_POC_COMPLETION_REPORT.md
├── BLOB_STORAGE_DESIGN.md
├── GRAPH_QUERY_RESEARCH.md
├── TASK_AUTOMATION_DESIGN.md
├── ... (100+ markdown files)
```

**Query limitations:**
```bash
# Current: Manual grep
grep -r "agent_a113108" *.md  # Unreliable
ls -lt *.md | head -20         # Recency only

# No way to:
- "Show all contents for task_28"
- "Which agent created this design?"
- "Navigate sections of large document"
- "Find all research contents"
```

---

## Proposed Design

### Architecture Overview

**Three-Layer Separation:**

```
┌─────────────────────────────────────────────────────┐
│                  Application Layer                  │
│  (CLI, Dashboard, Queries)                          │
└─────────────────┬───────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────┐
│                   Content Layer                     │
│  (High-level concerns - metadata, relationships)    │
│                                                      │
│  Nodes:                                             │
│  - AgentActor (existing)                            │
│  - ContentActor (new - was DeliverableActor)        │
│  - SectionActor (new - index metadata only)         │
│                                                      │
│  Properties:                                        │
│  - content_id, title, type, size, hash              │
│  - tags[], embeddings[] (semantic search)           │
│  - section_index[] (byte offsets + metadata)        │
│                                                      │
│  Edges:                                             │
│  - agent → content (created_content)                │
│  - task → content (has_content)                     │
│  - content → section (contains_section)             │
│                                                      │
│  Does NOT know: WHERE or HOW content is stored      │
│  Delegates to: BlobActor via messages               │
└─────────────────┬───────────────────────────────────┘
                  │ send(blob_id, "read", {})
                  │ send(blob_id, "slice", {offset, length})
                  ▼
┌─────────────────────────────────────────────────────┐
│              Blob Storage Layer                     │
│  (Storage abstraction - hides implementation)       │
│                                                      │
│  BlobActor Interface:                               │
│  - store(content_id, bytes) → blob_id               │
│  - retrieve(blob_id) → bytes                        │
│  - slice(blob_id, offset, length) → bytes           │
│  - delete(blob_id)                                  │
│                                                      │
│  Does NOT expose: WHERE stored, WHICH backend       │
│  Managed by: System (performance, caching)          │
└─────────────────┬───────────────────────────────────┘
                  │ Could be...
                  ▼
┌─────────────────────────────────────────────────────┐
│              Implementation Layer                   │
│  (BlobActor chooses backend, content actors don't)  │
│                                                      │
│  Options:                                           │
│  - Filesystem (docs/content/)                       │
│  - S3 / Blob Store (cloud storage)                  │
│  - Database (CozoDB, SQLite)                        │
│  - In-memory (ephemeral, testing)                   │
│                                                      │
│  System manages: Placement, caching, performance    │
└─────────────────────────────────────────────────────┘
```

**Key Insight:** Content actors send messages to BlobActor. The SYSTEM decides performance strategy (caching, placement, backend). Content actors are decoupled from storage implementation.

### Node Types

#### 1. ContentActor (formerly DeliverableActor)

Represents an agent content document (design, research, completion report). **Metadata only** - actual content stored via BlobActor.

```typescript
export interface ContentProperties extends NodeProperties {
  id: string;
  type: "content";

  // Content metadata (in graph)
  contentType: ContentType;  // completion, design, research, etc.
  title: string;
  filename: string;

  // Storage reference (delegates to BlobActor)
  blobId: string;  // BlobActor handles WHERE and HOW

  // Integrity
  size: number;
  hash: string;  // sha256:... (for validation)

  // Timestamps
  createdAt: Date;
  updatedAt?: Date;

  // Semantic metadata (for search, discovery)
  description?: string;
  tags?: string[];           // Manual tags: ["actor-model", "design"]
  embeddings?: number[];     // Vector embeddings for semantic search

  // Section index (byte offsets for on-demand reading)
  sectionIndex?: SectionIndexEntry[];

  // Cached statistics (optional, derived)
  sectionCount?: number;
  headingCount?: number;
  wordCount?: number;
}

export type ContentType =
  | "completion"      // Task completion reports
  | "design"          // Architecture/design docs
  | "research"        // Research findings
  | "planning"        // Planning documents
  | "workflow"        // Workflow specifications
  | "analysis"        // Analysis reports
  | "specification"   // Formal specs
  | "other";

// Index-based section metadata (stored in content node)
export interface SectionIndexEntry {
  section_id: string;
  heading: string;
  depth: number;             // 1 = H1, 2 = H2, etc.
  byte_offset: number;       // Start position in file
  byte_length: number;       // Section length
  tags?: string[];           // Section-specific tags
  embeddings?: number[];     // Section-specific embeddings
}
```

**Key Design Decision:** Index-based approach.
- **Metadata lives in graph** (content_id, title, tags, embeddings, section index)
- **Content DATA lives in BlobActor** (actual markdown bytes)
- **Sections parsed on-demand** using byte offsets from index
- **ContentActor doesn't care** WHERE blob is stored (filesystem, S3, database)
- **System manages** caching, performance, placement

#### 2. SectionActor

Represents a section/heading within content. **Stores metadata + byte offsets** for on-demand content retrieval.

```typescript
export interface SectionProperties extends NodeProperties {
  id: string;
  type: "section";

  // Section metadata
  heading: string;
  depth: number;      // 1 = H1, 2 = H2, etc.
  anchor: string;     // Slugified heading for linking

  // Position in document (byte offsets for on-demand reading)
  byte_offset: number;   // Start position in content blob
  byte_length: number;   // Section length in bytes
  line_start?: number;   // Optional: line number (for display)
  line_end?: number;     // Optional: line number (for display)

  // Semantic metadata (optional)
  content_summary?: string;   // First 200 chars (cached)
  tags?: string[];            // Section-specific tags
  embeddings?: number[];      // Section-specific embeddings

  // Cached metrics (optional, derived)
  word_count?: number;
  code_block_count?: number;

  // Tree structure (via edges, not embedded)
  // Use parent_section edges for hierarchy
}
```

**Reading section content:**
```typescript
// SectionActor asks BlobActor for bytes at offset
async function getSectionContent(sectionId: string): Promise<string> {
  const section = graph.getNodeProperties(sectionId) as SectionProperties;

  // Find parent content's blob reference
  const contentEdge = graph.getEdgesTo(sectionId)
    .find(e => e.type === "contains_section");
  const blobEdge = graph.getEdgesFrom(contentEdge.fromId)
    .find(e => e.type === "stored_in_blob");

  // Ask BlobActor for slice (on-demand)
  const { content } = await graph.send(
    blobEdge.toId,
    "slice",
    {
      offset: section.byte_offset,
      length: section.byte_length
    }
  );

  return content;
}
```

#### 3. BlobActor (Storage Abstraction)

**Storage abstraction layer** - hides implementation from content actors. BlobActor decides WHERE and HOW to store content.

```typescript
// BlobActor interface (abstract storage)
export interface BlobProperties extends NodeProperties {
  id: string;
  type: "blob";

  // Implementation details (hidden from content layer)
  // Could be: filesystem path, S3 key, database ID, etc.
  storage_ref: string;   // Generic reference to stored content
  storage_type: string;  // "filesystem" | "s3" | "database" | "memory"

  // Metadata
  size: number;
  hash: string;
  mimeType: string;      // "text/markdown"

  // Timestamps
  createdAt: Date;
  updatedAt?: Date;

  // Optional: caching hints (managed by system)
  cache_policy?: "hot" | "warm" | "cold";
  last_accessed?: Date;
}

// BlobActor message interface
export type BlobMessage =
  // Read operations
  | { type: "read"; payload: {} }                                    // Read full content
  | { type: "slice"; payload: { offset: number; length: number } }   // Read byte range
  | { type: "metadata"; payload: {} }                                // Get metadata only

  // Write operations
  | { type: "write"; payload: { content: string | Uint8Array } }
  | { type: "append"; payload: { content: string | Uint8Array } }

  // Validation
  | { type: "validate"; payload: {} }                                // Check integrity
  | { type: "compute_hash"; payload: {} }

  // Lifecycle
  | { type: "delete"; payload: {} };
```

**Key Responsibilities:**
- **Abstracts storage implementation** (content actors don't know WHERE stored)
- **Provides uniform interface** (read, slice, write, validate)
- **Supports multiple backends** (filesystem, S3, database, memory)
- **System-managed performance** (caching, placement decisions)

**What ContentActor sees:**
```typescript
// ContentActor only knows blob_id, not WHERE or HOW stored
const { content } = await graph.send(blobId, "read", {});

// Or read specific byte range
const { content } = await graph.send(blobId, "slice", { offset: 1000, length: 500 });
```

**What BlobActor manages internally:**
- Choosing storage backend (filesystem vs S3 vs database)
- Caching frequently accessed content
- Compression, encryption
- Storage location (which disk, which S3 bucket, etc.)
- Performance optimization

### Edge Types

**1. created_content**

Links agent to content it created.

```typescript
graph.addEdge(agentId, contentId, "created_content", {
  createdAt: new Date().toISOString(),
  purpose: "completion report"  // Why agent created it
});
```

**2. has_content**

Links task to its contents.

```typescript
graph.addEdge(taskId, contentId, "has_content", {
  linkedAt: new Date().toISOString(),
  phase: "implementation"  // When in task lifecycle
});
```

**3. stored_in_blob**

Links content to blob storage.

```typescript
graph.addEdge(contentId, blobId, "stored_in_blob", {
  format: "markdown"
});
```

**4. contains_section**

Links content to top-level sections.

```typescript
graph.addEdge(contentId, sectionId, "contains_section", {
  order: 0  // Section order in document
});
```

**5. parent_section**

Links section to parent section (hierarchical structure).

```typescript
graph.addEdge(childSectionId, parentSectionId, "parent_section", {
  depth: 2  // Child depth
});
```

---

## Index-Based Content Model

### Design Choice: Index vs In-Memory vs On-the-Fly

The user feedback clarified three options for markdown tree representation:

**Option A: In-memory tree**
- Parse markdown into full tree structure in memory
- Fast navigation, high memory overhead
- Tree structure lives in graph nodes

**Option B: On-the-fly parsing**
- Parse sections on demand when accessed
- Low memory, slower repeated access
- No persistent structure

**Option C: Index-based (CHOSEN)**
- Store metadata (tags, embeddings, byte offsets) in graph
- Parse on-demand using index
- Best balance: queryable metadata, efficient storage, flexible access

### Index-Based Model Details

**Metadata in Graph (ContentActor properties):**
```typescript
{
  content_id: "content_123",
  title: "Design Document",
  type: "markdown",
  size_bytes: 45000,
  hash: "sha256:...",
  tags: ["design", "architecture"],
  embeddings: [0.1, 0.2, ...],  // For semantic search

  // Section index (byte offsets for on-demand reading)
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
    {
      section_id: "sec_2",
      heading: "Architecture",
      depth: 1,
      byte_offset: 500,
      byte_length: 1200,
      tags: ["design", "system"],
      embeddings: [0.18, 0.31, ...]
    },
    // ... more sections
  ]
}
```

**Content Data via BlobActor:**
```typescript
// Read full content
const { content } = await graph.send(blobId, "read", {});

// Read specific section (on-demand)
const { content: sectionContent } = await graph.send(
  blobId,
  "slice",
  {
    offset: section.byte_offset,
    length: section.byte_length
  }
);
```

**Benefits:**
- **Queryable:** Search by tags, embeddings without reading full content
- **Efficient:** Only load content when needed
- **Flexible:** Can add more metadata without changing blob storage
- **Scalable:** Large documents don't bloat graph memory

### Tag and Embedding-Based Search

**Search by tags (no content read needed):**
```typescript
// Find all design sections across all content
const designSections = contentNodes
  .flatMap(c => c.section_index)
  .filter(s => s.tags.includes("design"));

// Execute without reading blobs
```

**Semantic search by embeddings:**
```typescript
// Find sections similar to query embedding
const queryEmbedding = await embedText("actor model patterns");

const similarSections = contentNodes
  .flatMap(c => c.section_index.map(s => ({
    ...s,
    content_id: c.content_id,
    similarity: cosineSimilarity(queryEmbedding, s.embeddings)
  })))
  .sort((a, b) => b.similarity - a.similarity)
  .slice(0, 10);

// THEN read only the top matching sections from BlobActor
for (const section of similarSections) {
  const content = await graph.send(
    section.blob_id,
    "slice",
    { offset: section.byte_offset, length: section.byte_length }
  );
}
```

---

## Blob Storage Actor Interface

### Message Types

```typescript
export type BlobMessage =
  // Read operations
  | { type: "read_content"; payload: {} }
  | { type: "read_metadata"; payload: {} }

  // Write operations
  | { type: "write_content"; payload: { content: string } }
  | { type: "append_content"; payload: { content: string } }

  // Validation
  | { type: "validate"; payload: {} }
  | { type: "compute_hash"; payload: {} }
  | { type: "sync_metadata"; payload: {} }

  // Lifecycle
  | { type: "delete"; payload: {} };
```

### Example Usage

```typescript
// Create blob for content content
const blobAddr = BlobActor({
  content: markdownContent,
  mimeType: "text/markdown",
  graph
});

const blobId = "blob_" + Date.now();
graph.registerNode(blobId, blobAddr, {
  id: blobId,
  type: "blob",
  path: `docs/contents/${blobId}.md`,
  size: markdownContent.length,
  hash: computeHash(markdownContent),
  mimeType: "text/markdown",
  createdAt: new Date()
});

// Read content
const { content } = await graph.send(blobId, "read_content", {});

// Validate integrity
const result = await graph.send(blobId, "validate", {});
if (!result.valid) {
  console.error("Integrity check failed:", result.issues);
}
```

---

## Hierarchical Markdown Model

### MarkdownGraph Integration

Reuse existing `MarkdownGraph` subsystem (from commit d412675).

```typescript
import { MarkdownGraph } from "../markdown-graph/MarkdownGraph";

// Parse content into graph structure
const mdGraph = MarkdownGraph.fromMarkdown(content);

// Extract sections
const sections = mdGraph.findNodes({ type: "heading" });

// Create SectionActor for each heading
for (const section of sections) {
  const sectionId = `section_${contentId}_${section.id}`;

  const sectionAddr = SectionActor({
    heading: section.metadata.text,
    depth: section.properties.depth,
    anchor: slugify(section.metadata.text),
    lineStart: section.position.start.line,
    lineEnd: section.position.end.line,
    graph
  });

  graph.registerNode(sectionId, sectionAddr, {
    id: sectionId,
    type: "section",
    heading: section.metadata.text,
    depth: section.properties.depth,
    // ...
  });

  // Link content → section
  graph.addEdge(contentId, sectionId, "contains_section", {
    order: sections.indexOf(section)
  });
}

// Build hierarchical structure
for (const section of sections) {
  const parent = mdGraph.getParent(section.id);
  if (parent && parent.type === "heading") {
    const childSectionId = `section_${contentId}_${section.id}`;
    const parentSectionId = `section_${contentId}_${parent.id}`;

    graph.addEdge(childSectionId, parentSectionId, "parent_section", {
      depth: section.properties.depth
    });
  }
}
```

### Section Queries

```datalog
# Get all top-level sections of a content
?[section_id, heading, depth] :=
  *contains_section{ from_id: $content_id, to_id: section_id },
  *section{ id: section_id, heading, depth }

# Get section hierarchy (recursive)
?[section_id, heading, depth, parent_id] :=
  *section{ id: section_id, heading, depth },
  *parent_section{ from_id: section_id, to_id: parent_id }
```

### Navigation Example

```typescript
// Navigate to specific section
async function navigateToSection(
  contentId: string,
  sectionHeading: string
): Promise<string> {
  // Find section by heading
  const sectionEdges = graph.getEdgesFrom(contentId)
    .filter(e => e.type === "contains_section");

  for (const edge of sectionEdges) {
    const section = graph.getNodeProperties(edge.toId) as SectionProperties;
    if (section.heading === sectionHeading) {
      // Read content for this section
      const { lineStart, lineEnd } = section;
      const blobEdge = graph.getEdgesFrom(contentId)
        .find(e => e.type === "stored_in_blob");
      const blobId = blobEdge.toId;

      const { content } = await graph.send(blobId, "read_content", {});
      const lines = content.split("\n");
      const sectionContent = lines.slice(lineStart - 1, lineEnd).join("\n");

      return sectionContent;
    }
  }

  throw new Error(`Section not found: ${sectionHeading}`);
}
```

---

## Graph Relationships

### Example Graph Structure

```
task_28 (TaskActor)
  │
  ├─[spawned_by]─> agent_bg_a7e4d2 (AgentActor)
  │                      │
  │                      ├─[created_content]─> content_1 (DeliverableActor)
  │                      │                              │
  │                      │                              ├─[stored_in_blob]─> blob_123 (BlobActor)
  │                      │                              │
  │                      │                              ├─[contains_section]─> section_1_1 (SectionActor: "Overview")
  │                      │                              │                          │
  │                      │                              │                          └─[parent_section]─> section_1_2 (SectionActor: "Goals")
  │                      │                              │
  │                      │                              └─[contains_section]─> section_1_3 (SectionActor: "Design")
  │                      │
  │                      └─[created_content]─> content_2 (DeliverableActor)
  │
  └─[has_content]─> content_1
  └─[has_content]─> content_2
```

### Datalog Schema

```datalog
# Deliverables
:create content {
  id: String,
  content_type: String,
  title: String,
  filename: String,
  blob_id: String,
  size: Int,
  hash: String,
  created_at: String,
  updated_at: String?,
  description: String?,
  tags: [String]?,
  section_count: Int?,
  heading_count: Int?,
  word_count: Int?
}

# Sections
:create section {
  id: String,
  heading: String,
  depth: Int,
  anchor: String,
  line_start: Int,
  line_end: Int,
  content_summary: String?,
  word_count: Int?,
  code_block_count: Int?,
  child_section_ids: [String]
}

# Blobs (existing)
:create blob {
  id: String,
  path: String,
  size: Int,
  hash: String,
  mime_type: String,
  created_at: String,
  updated_at: String?
}

# Edges
:create created_content {
  from_id: String,  # agent_id
  to_id: String,    # content_id
  created_at: String,
  purpose: String?
}

:create has_content {
  from_id: String,  # task_id
  to_id: String,    # content_id
  linked_at: String,
  phase: String?
}

:create stored_in_blob {
  from_id: String,  # content_id
  to_id: String,    # blob_id
  format: String
}

:create contains_section {
  from_id: String,  # content_id
  to_id: String,    # section_id
  order: Int
}

:create parent_section {
  from_id: String,  # child_section_id
  to_id: String,    # parent_section_id
  depth: Int
}
```

---

## Query Examples

### Query 1: Show all contents for agent X

```datalog
# Find all contents created by agent_bg_a7e4d2
?[content_id, title, type, created_at] :=
  *created_content{ from_id: "agent_bg_a7e4d2", to_id: content_id, created_at },
  *content{ id: content_id, title, content_type: type }

:order created_at desc
```

**TypeScript usage:**
```typescript
async function getAgentDeliverables(agentId: string) {
  const edges = graph.getEdgesFrom(agentId)
    .filter(e => e.type === "created_content");

  return edges.map(e => {
    const content = graph.getNodeProperties(e.toId);
    return {
      id: e.toId,
      title: content.title,
      type: content.contentType,
      createdAt: e.properties.createdAt
    };
  });
}
```

### Query 2: What contents did task_5 produce?

```datalog
# Find all contents linked to task_5
?[content_id, title, agent_id] :=
  *has_content{ from_id: "task_5", to_id: content_id },
  *content{ id: content_id, title },
  *created_content{ from_id: agent_id, to_id: content_id }
```

**TypeScript usage:**
```typescript
async function getTaskDeliverables(taskId: string) {
  const edges = graph.getEdgesFrom(taskId)
    .filter(e => e.type === "has_content");

  return edges.map(e => {
    const content = graph.getNodeProperties(e.toId);

    // Find which agent created it
    const agentEdge = graph.getEdgesTo(e.toId)
      .find(edge => edge.type === "created_content");

    return {
      id: e.toId,
      title: content.title,
      agentId: agentEdge?.fromId,
      content: async () => {
        const blobEdge = graph.getEdgesFrom(e.toId)
          .find(edge => edge.type === "stored_in_blob");
        return await graph.send(blobEdge.toId, "read_content", {});
      }
    };
  });
}
```

### Query 3: Navigate hierarchical sections

```datalog
# Get section hierarchy for content_1
?[section_id, heading, depth, parent_id] :=
  *contains_section{ from_id: "content_1", to_id: section_id },
  *section{ id: section_id, heading, depth },
  *parent_section{ from_id: section_id, to_id: parent_id }

:order depth, heading
```

**TypeScript usage:**
```typescript
async function getSectionHierarchy(contentId: string) {
  // Get all sections
  const sectionEdges = graph.getEdgesFrom(contentId)
    .filter(e => e.type === "contains_section")
    .sort((a, b) => a.properties.order - b.properties.order);

  // Build tree structure
  const sections = sectionEdges.map(e => {
    const section = graph.getNodeProperties(e.toId) as SectionProperties;

    // Find parent
    const parentEdge = graph.getEdgesFrom(e.toId)
      .find(edge => edge.type === "parent_section");

    return {
      id: e.toId,
      heading: section.heading,
      depth: section.depth,
      parentId: parentEdge?.toId,
      children: []
    };
  });

  // Build hierarchy
  const tree = buildTree(sections);
  return tree;
}

function buildTree(sections: any[]) {
  const map = new Map(sections.map(s => [s.id, { ...s, children: [] }]));
  const roots = [];

  for (const section of sections) {
    if (section.parentId) {
      const parent = map.get(section.parentId);
      parent?.children.push(map.get(section.id));
    } else {
      roots.push(map.get(section.id));
    }
  }

  return roots;
}
```

### Query 4: Find all design contents

```datalog
# Find all design documents
?[content_id, title, agent_id, task_id] :=
  *content{ id: content_id, title, content_type: "design" },
  *created_content{ from_id: agent_id, to_id: content_id },
  *has_content{ from_id: task_id, to_id: content_id }

:order title
```

### Query 5: Search section content

```datalog
# Find sections containing "actor model"
?[content_id, section_id, heading] :=
  *section{ id: section_id, heading, content_summary },
  *contains_section{ from_id: content_id, to_id: section_id },
  content_summary ~ "actor model"
```

---

## Migration Path

### Phase 1: Hybrid (Filesystem + Graph)

**Goal:** Validate graph storage without breaking existing workflows

**Implementation:**
1. Agent creates content file as usual (filesystem)
2. Agent also creates DeliverableActor in graph
3. BlobActor stores reference to existing file
4. Validate consistency between filesystem and graph

**Code:**
```typescript
// Agent completion
async function createDeliverable(
  agentId: string,
  taskId: string,
  content: string,
  metadata: { title: string; type: DeliverableType }
) {
  // Phase 1: Write to filesystem (existing behavior)
  const filename = `${metadata.type.toUpperCase()}_${taskId}.md`;
  const filepath = path.join(process.cwd(), filename);
  await Bun.write(filepath, content);

  // NEW: Also store in graph
  const contentId = `content_${Date.now()}`;

  // Create blob actor
  const blobId = `blob_${Date.now()}`;
  const blobAddr = BlobActor({
    path: filepath,  // Reference existing file
    content,
    mimeType: "text/markdown",
    graph
  });
  graph.registerNode(blobId, blobAddr, {
    id: blobId,
    type: "blob",
    path: filepath,
    size: content.length,
    hash: computeHash(content),
    mimeType: "text/markdown",
    createdAt: new Date()
  });

  // Create content actor
  const contentAddr = DeliverableActor({
    title: metadata.title,
    contentType: metadata.type,
    filename,
    blobId,
    graph
  });
  graph.registerNode(contentId, contentAddr, {
    id: contentId,
    type: "content",
    contentType: metadata.type,
    title: metadata.title,
    filename,
    blobId,
    size: content.length,
    hash: computeHash(content),
    createdAt: new Date()
  });

  // Create relationships
  graph.addEdge(agentId, contentId, "created_content", {
    createdAt: new Date().toISOString(),
    purpose: metadata.type
  });
  graph.addEdge(taskId, contentId, "has_content", {
    linkedAt: new Date().toISOString()
  });
  graph.addEdge(contentId, blobId, "stored_in_blob", {
    format: "markdown"
  });

  // Parse sections
  await parseSectionsIntoGraph(contentId, content);

  return contentId;
}
```

**Validation:**
```typescript
// Verify consistency
async function validateHybridStorage() {
  const contents = graph.getNodeIds()
    .filter(id => id.startsWith("content_"));

  for (const contentId of contents) {
    const props = graph.getNodeProperties(contentId) as DeliverableProperties;

    // Check filesystem file exists
    const fileExists = await Bun.file(props.filename).exists();
    if (!fileExists) {
      console.error(`File missing for ${contentId}: ${props.filename}`);
    }

    // Check blob reference
    const blobEdge = graph.getEdgesFrom(contentId)
      .find(e => e.type === "stored_in_blob");
    if (!blobEdge) {
      console.error(`Blob missing for ${contentId}`);
    }

    // Validate hash
    const result = await graph.send(blobEdge.toId, "validate", {});
    if (!result.valid) {
      console.error(`Hash mismatch for ${contentId}`);
    }
  }
}
```

### Phase 2: Graph Primary

**Goal:** Move content into blob storage, deprecate scattered files

**Changes:**
1. BlobActor stores content in `docs/contents/` directory
2. Agents use graph API exclusively
3. Old files remain for backward compatibility
4. CLI queries graph, not filesystem

**Implementation:**
```typescript
// Phase 2: Graph primary storage
async function createDeliverable(
  agentId: string,
  taskId: string,
  content: string,
  metadata: { title: string; type: DeliverableType }
) {
  const contentId = `content_${Date.now()}`;

  // Store in blob storage directory (not project root)
  const blobPath = `docs/contents/${contentId}.md`;
  await Bun.write(blobPath, content);

  // Create blob actor
  const blobId = `blob_${Date.now()}`;
  const blobAddr = BlobActor({
    path: blobPath,
    content,
    mimeType: "text/markdown",
    graph
  });
  graph.registerNode(blobId, blobAddr, { /* ... */ });

  // Create content actor
  const contentAddr = DeliverableActor({
    title: metadata.title,
    contentType: metadata.type,
    filename: `${contentId}.md`,
    blobId,
    graph
  });
  graph.registerNode(contentId, contentAddr, { /* ... */ });

  // Create relationships
  graph.addEdge(agentId, contentId, "created_content", { /* ... */ });
  graph.addEdge(taskId, contentId, "has_content", { /* ... */ });
  graph.addEdge(contentId, blobId, "stored_in_blob", { /* ... */ });

  // Parse sections
  await parseSectionsIntoGraph(contentId, content);

  // NO filesystem write to project root
  return contentId;
}
```

**Migration script:**
```typescript
// Migrate old contents to graph storage
async function migrateOldDeliverables() {
  const oldFiles = await Array.fromAsync(
    new Bun.Glob("*.md").scan({ cwd: process.cwd() })
  );

  for (const filename of oldFiles) {
    // Skip known non-content files
    if (filename === "README.md" || filename === "CLAUDE.md") continue;

    const content = await Bun.file(filename).text();

    // Extract metadata from content
    const metadata = extractMetadata(content);

    // Create content in graph
    const contentId = await createDeliverable(
      metadata.agentId || "unknown",
      metadata.taskId || "unknown",
      content,
      {
        title: metadata.title || filename,
        type: metadata.type || "other"
      }
    );

    console.log(`Migrated ${filename} → ${contentId}`);

    // Optionally delete old file
    // await Bun.$`rm ${filename}`;
  }
}
```

### Phase 3: Pure Graph Storage

**Goal:** Remove filesystem dependency entirely

**Changes:**
1. All contents stored in graph nodes only
2. Blob storage uses in-memory or database backend
3. Export to filesystem on-demand for viewing
4. Complete actor model compliance

**Implementation:**
```typescript
// Phase 3: Pure graph storage (no filesystem)
class InMemoryBlobStore {
  private blobs = new Map<string, string>();

  write(blobId: string, content: string) {
    this.blobs.set(blobId, content);
  }

  read(blobId: string): string {
    return this.blobs.get(blobId) || "";
  }

  delete(blobId: string) {
    this.blobs.delete(blobId);
  }
}

// Or use CozoDB for blob storage
async function storeBlobInCozoDB(blobId: string, content: string) {
  await cozo.run(`
    ?[id, content, created_at] <- [["${blobId}", "${content}", "${new Date().toISOString()}"]]
    :put blob { id, content, created_at }
  `);
}

// Export content to filesystem on-demand
async function exportDeliverable(contentId: string, outputPath: string) {
  const blobEdge = graph.getEdgesFrom(contentId)
    .find(e => e.type === "stored_in_blob");

  const { content } = await graph.send(blobEdge.toId, "read_content", {});
  await Bun.write(outputPath, content);

  console.log(`Exported ${contentId} to ${outputPath}`);
}
```

---

## Integration Points

### 1. Agent Completion Protocol

Update agent completion to create contents in graph:

```typescript
// src/protocols/agent-completion.ts

export async function agentComplete(
  agentId: string,
  taskId: string,
  contents: Array<{
    title: string;
    type: DeliverableType;
    content: string;
  }>
) {
  const contentIds = [];

  for (const content of contents) {
    const contentId = await createDeliverable(
      agentId,
      taskId,
      content.content,
      { title: content.title, type: content.type }
    );
    contentIds.push(contentId);
  }

  // Update agent status
  await graph.send(agentId, "update", {
    properties: {
      status: "completed",
      contentIds
    }
  });

  return contentIds;
}
```

### 2. CLI Integration

Add CLI commands for querying contents:

```bash
# List contents for agent
bun src/cli/task.ts contents --agent agent_bg_a7e4d2

# List contents for task
bun src/cli/task.ts contents --task task_28

# Show content details
bun src/cli/task.ts show-content content_123

# Navigate sections
bun src/cli/task.ts sections content_123

# Export content to file
bun src/cli/task.ts export-content content_123 --output report.md
```

**Implementation:**
```typescript
// src/cli/task.ts

async function cmdDeliverables(args: {
  agent?: string;
  task?: string;
  type?: DeliverableType;
}) {
  const graph = await loadGraph(TASKS_FILE);

  let contentIds: string[];

  if (args.agent) {
    contentIds = graph.getEdgesFrom(args.agent)
      .filter(e => e.type === "created_content")
      .map(e => e.toId);
  } else if (args.task) {
    contentIds = graph.getEdgesFrom(args.task)
      .filter(e => e.type === "has_content")
      .map(e => e.toId);
  } else {
    contentIds = graph.getNodeIds()
      .filter(id => id.startsWith("content_"));
  }

  // Filter by type
  if (args.type) {
    contentIds = contentIds.filter(id => {
      const props = graph.getNodeProperties(id) as DeliverableProperties;
      return props.contentType === args.type;
    });
  }

  // Display
  console.log(`Found ${contentIds.length} contents:\n`);
  for (const id of contentIds) {
    const props = graph.getNodeProperties(id) as DeliverableProperties;
    console.log(`${id} (${props.contentType})`);
    console.log(`  Title: ${props.title}`);
    console.log(`  Size: ${formatBytes(props.size)}`);
    console.log(`  Created: ${props.createdAt.toISOString()}`);
    console.log();
  }
}
```

### 3. Dashboard Integration

Display contents in browser dashboard:

```javascript
// browser/widgets/contents-widget.js

async function fetchDeliverables(taskId) {
  const response = await fetch(`/api/tasks/${taskId}/contents`);
  return await response.json();
}

function renderDeliverables(contents) {
  return `
    <div class="contents">
      <h3>Deliverables</h3>
      ${contents.map(d => `
        <div class="content" data-id="${d.id}">
          <span class="type">${d.type}</span>
          <span class="title">${d.title}</span>
          <button onclick="viewDeliverable('${d.id}')">View</button>
          <button onclick="showSections('${d.id}')">Sections</button>
        </div>
      `).join("")}
    </div>
  `;
}

async function showSections(contentId) {
  const sections = await fetchSections(contentId);
  renderSectionTree(sections);
}
```

### 4. Graph Query Integration

Expose content queries via graph API:

```typescript
// src/graph/queries.ts

export const contentQueries = {
  // Get all contents for agent
  byAgent: (agentId: string) => `
    ?[content_id, title, type, created_at] :=
      *created_content{ from_id: "${agentId}", to_id: content_id, created_at },
      *content{ id: content_id, title, content_type: type }
    :order created_at desc
  `,

  // Get all contents for task
  byTask: (taskId: string) => `
    ?[content_id, title, agent_id] :=
      *has_content{ from_id: "${taskId}", to_id: content_id },
      *content{ id: content_id, title },
      *created_content{ from_id: agent_id, to_id: content_id }
  `,

  // Find contents by type
  byType: (type: DeliverableType) => `
    ?[content_id, title, agent_id, task_id] :=
      *content{ id: content_id, title, content_type: "${type}" },
      *created_content{ from_id: agent_id, to_id: content_id },
      *has_content{ from_id: task_id, to_id: content_id }
  `,

  // Get section hierarchy
  sections: (contentId: string) => `
    ?[section_id, heading, depth, parent_id] :=
      *contains_section{ from_id: "${contentId}", to_id: section_id, order },
      *section{ id: section_id, heading, depth },
      *parent_section{ from_id: section_id, to_id: parent_id }
    :order depth, order
  `
};
```

---

## Implementation Tasks

### Phase 1: Foundation (P0)

1. **Create DeliverableActor factory**
   - File: `src/content.ts`
   - Message handlers: create, read, update, delete
   - Tests: 10+ unit tests

2. **Create SectionActor factory**
   - File: `src/section.ts`
   - Hierarchical navigation
   - Tests: 8+ unit tests

3. **Integrate MarkdownGraph**
   - Parse contents into sections
   - Build hierarchical structure
   - Tests: Integration with existing MarkdownGraph

4. **Update agent completion protocol**
   - Call `createDeliverable()` on completion
   - Store in graph + filesystem (hybrid)
   - Tests: End-to-end agent workflow

### Phase 2: Queries (P1)

5. **Implement Datalog schema**
   - Add content, section relations
   - Migration from existing tasks.json
   - Tests: Query validation

6. **Add CLI commands**
   - `contents`, `show-content`, `sections`, `export-content`
   - JSON output mode
   - Tests: CLI integration tests

7. **Dashboard widgets**
   - Deliverables list widget
   - Section navigation widget
   - Tests: Browser tests

### Phase 3: Migration (P2)

8. **Hybrid storage implementation**
   - Write to both filesystem + graph
   - Validation checks
   - Tests: Consistency validation

9. **Migration script**
   - Migrate old contents to graph
   - Preserve lineage (best-effort)
   - Tests: Migration validation

10. **Graph-primary transition**
    - Move blobs to `docs/contents/`
    - Deprecate project root files
    - Tests: Backward compatibility

### Phase 4: Pure Graph (P3 - Future)

11. **In-memory blob storage**
    - Remove filesystem dependency
    - Export on-demand
    - Tests: Performance benchmarks

12. **CozoDB blob storage**
    - Store blobs in database
    - Query content directly
    - Tests: Database integration

---

## Open Questions

### 1. Section Granularity

**Question:** How deep should section hierarchy go?

**Options:**
- A: Only top-level headings (H1, H2)
- B: All headings (H1-H6)
- C: Configurable depth per content

**Recommendation:** Start with B (all headings), allow filtering by depth in queries.

### 2. Content Search

**Question:** Should we index section content for full-text search?

**Options:**
- A: Store content summary only (200 chars)
- B: Store full content in section nodes
- C: Use external search index (Elasticsearch, etc.)

**Recommendation:** Start with A (summaries), add full-text search in Phase 3 if needed.

### 3. Blob Storage Backend

**Question:** What storage backend for blobs in Phase 3?

**Options:**
- A: In-memory (ephemeral)
- B: CozoDB (integrated with graph)
- C: Filesystem (existing)
- D: S3/object storage (cloud)

**Recommendation:** Start with C (filesystem), migrate to B (CozoDB) in Phase 3.

### 4. Versioning

**Question:** Should contents support versioning?

**Options:**
- A: No versioning (current state only)
- B: Git-based versioning (store hash per version)
- C: Full version history in graph

**Recommendation:** Defer to Phase 4. Use git for now.

### 5. Access Control

**Question:** Should contents have access control?

**Options:**
- A: Public (all agents can read all contents)
- B: Task-scoped (only agents on same task)
- C: Fine-grained ACLs

**Recommendation:** Start with A (public), add task-scoping in Phase 2 if needed.

---

## Success Metrics

### Phase 1 (Hybrid)

**MUST (Objective):**
- All agent completions create content nodes in graph
- 100% of contents have blob references
- All tests pass (content, section, integration)

**SHOULD (Measured):**
- Query performance <100ms for typical queries
- Migration success rate >95%
- Zero hash mismatches in validation

**MAY (Subjective):**
- API ergonomics (developer feedback)
- CLI usability (user feedback)

### Phase 2 (Graph Primary)

**MUST:**
- Zero new files in project root
- All contents queryable via CLI
- Dashboard displays contents correctly

**SHOULD:**
- Query performance <50ms
- Section navigation <100ms
- Migration completes in <5 minutes

### Phase 3 (Pure Graph)

**MUST:**
- Zero filesystem dependencies for contents
- Export function works for all contents
- All tests pass

**SHOULD:**
- In-memory storage <100MB overhead
- Export performance <1s per content
- CozoDB integration <200ms query latency

---

## References

### Existing Designs

- **BlobActor:** BLOB_STORAGE_ACTOR_REDESIGN.md
- **MarkdownGraph:** src/markdown-graph/README.md
- **Agent Workflow:** actor-worldview/bg-workflow/README.md
- **Graph System:** src/graph.ts

### Related Commits

- d412675: Add markdown-graph subsystem
- (See git log for blob storage, agent workflow)

### External

- [MarkdownGraph API](../src/markdown-graph/README.md)
- [Actor Worldview](./README.md)
- [Background Workflow](./bg-workflow/README.md)

---

**Status:** Design Complete - Ready for Implementation
**Next Step:** Create task for Phase 1 implementation
**Estimated Effort:** 3-4 days (Phase 1), 2 days (Phase 2), 1 day (Phase 3)
