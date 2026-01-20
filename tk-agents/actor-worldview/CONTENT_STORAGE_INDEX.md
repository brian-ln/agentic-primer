# Content Storage Documentation - Index

**Last Updated:** 2026-01-18
**Status:** Design Complete, Ready for Implementation

This index organizes all content storage documentation for easy navigation.

---

## Quick Start

**New to content storage?** Start here:

1. 📖 **[CONTENT_STORAGE_ARCHITECTURE.md](CONTENT_STORAGE_ARCHITECTURE.md)** - High-level overview (20 min read)
2. 🎯 **[CONTENT_STORAGE_QUICK_REF.md](CONTENT_STORAGE_QUICK_REF.md)** - Quick reference (5 min read)
3. 💻 **[../examples/content-storage-examples.ts](../examples/content-storage-examples.ts)** - Working code examples

---

## Documentation Map

### Architecture & Design

| Document | Purpose | Audience | Size |
|----------|---------|----------|------|
| **[CONTENT_STORAGE_ARCHITECTURE.md](CONTENT_STORAGE_ARCHITECTURE.md)** | High-level architecture overview with diagrams | Architects, developers | 650 lines |
| **[GRAPH_CONTENT_STORAGE_DESIGN.md](GRAPH_DELIVERABLE_STORAGE_DESIGN.md)** | Detailed design specification | Implementers | 1339 lines |
| **[CONTENT_STORAGE_QUICK_REF.md](CONTENT_STORAGE_QUICK_REF.md)** | Quick reference cheat sheet | All | 280 lines |

### Schema & Implementation

| Document | Purpose | Audience | Size |
|----------|---------|----------|------|
| **[CONTENT_STORAGE_SCHEMA.datalog](DELIVERABLE_STORAGE_SCHEMA.datalog)** | Datalog schema definition | Database developers | 305 lines |
| **[../examples/content-storage-examples.ts](../examples/content-storage-examples.ts)** | Usage examples and patterns | Developers | 655 lines |

### Reports & History

| Document | Purpose | Audience | Size |
|----------|---------|----------|------|
| **[CONTENT_STORAGE_REFINEMENT_REPORT.md](../CONTENT_STORAGE_REFINEMENT_REPORT.md)** | Design refinement completion report | Project managers, reviewers | 450 lines |

---

## Key Concepts

### Terminology

**ContentActor (not DeliverableActor)**
- Agent-created document (design, research, completion report)
- Stores metadata in graph (title, tags, embeddings, section index)
- Delegates storage to BlobActor

**BlobActor (storage abstraction)**
- Hides WHERE and HOW content is stored
- Provides uniform interface (read, slice, write)
- Supports multiple backends (filesystem, S3, database, memory)

**SectionActor (index metadata)**
- Represents section/heading within content
- Stores byte offsets for on-demand reading
- Enables semantic search without reading content

### Three-Layer Architecture

```
Content Layer → Blob Storage Layer → Implementation Layer
   ↓                    ↓                      ↓
Metadata          Abstraction           Physical Storage
Tags              read/slice            Filesystem
Embeddings        Interface             S3
Relationships                           Database
                                        Memory
```

### Index-Based Model

**Metadata in graph:** tags, embeddings, byte offsets
**Content in BlobActor:** actual markdown bytes
**Read on-demand:** Use byte offsets to slice content

---

## Common Tasks

### For Developers

**I want to...**

- **Create content from agent:**
  → See [Examples: Create Content](../examples/content-storage-examples.ts#L18-L74)

- **Search by tags:**
  → See [Quick Ref: Search by Tags](CONTENT_STORAGE_QUICK_REF.md#2-search-by-tags-no-content-read)

- **Read section on-demand:**
  → See [Quick Ref: Read Section](CONTENT_STORAGE_QUICK_REF.md#4-read-section-on-demand)

- **Export content:**
  → See [Examples: Export Content](../examples/content-storage-examples.ts#L356-L386)

- **Understand architecture:**
  → See [Architecture: Three-Layer](CONTENT_STORAGE_ARCHITECTURE.md#three-layer-architecture)

### For Architects

**I want to...**

- **Understand design rationale:**
  → See [Architecture: Decision Log](CONTENT_STORAGE_ARCHITECTURE.md#decision-log)

- **See migration path:**
  → See [Architecture: Migration Path](CONTENT_STORAGE_ARCHITECTURE.md#migration-path)

- **Review layer separation:**
  → See [Architecture: Key Principles](CONTENT_STORAGE_ARCHITECTURE.md#key-principles)

- **Compare storage options:**
  → See [Design: Blob Storage Backend](GRAPH_DELIVERABLE_STORAGE_DESIGN.md#3-blob-storage-backend)

### For Implementers

**I want to...**

- **Implement ContentActor:**
  → See [Design: ContentActor Node Type](GRAPH_DELIVERABLE_STORAGE_DESIGN.md#1-contentactor-formerly-deliverableactor)

- **Implement BlobActor interface:**
  → See [Design: BlobActor Storage Abstraction](GRAPH_DELIVERABLE_STORAGE_DESIGN.md#3-blobactor-storage-abstraction)

- **Create schema:**
  → See [Schema: Node Relations](DELIVERABLE_STORAGE_SCHEMA.datalog#L9-L60)

- **Write queries:**
  → See [Schema: Sample Queries](DELIVERABLE_STORAGE_SCHEMA.datalog#L107-L173)

---

## Design Changes (2026-01-18)

**Based on user feedback, the following refinements were made:**

### ✅ Terminology Updates

| Old | New | Rationale |
|-----|-----|-----------|
| DeliverableActor | ContentActor | More general-purpose |
| deliverable_type | content_type | Clearer naming |
| created_deliverable | created_content | Consistent terminology |
| has_deliverable | has_content | Consistent terminology |

### ✅ Architecture Clarifications

1. **Layer Separation:** Content Layer → Blob Storage Layer → Implementation
2. **Index-Based Model:** Metadata in graph, content via BlobActor
3. **System-Managed Performance:** Content actors don't manage caching/placement
4. **Storage Abstraction:** BlobActor hides WHERE and HOW from content layer

### ✅ New Features Documented

1. **Tag-based search:** Search without reading content
2. **Embedding search:** Semantic search using vector embeddings
3. **On-demand reading:** Read sections using byte offsets
4. **Multiple backends:** Filesystem, S3, database, memory support

---

## Migration Roadmap

### Phase 1: Hybrid (2-3 days)

**Goal:** Validate design without breaking workflows

- Agents write files + create ContentActor
- BlobActor references existing files
- Validate consistency
- **Status:** Design complete, ready for implementation

### Phase 2: Graph Primary (1-2 days)

**Goal:** Move to dedicated storage

- BlobActor stores in `docs/content/`
- Agents use graph API only
- Old files remain for backward compatibility
- **Status:** Design complete

### Phase 3: Pure Graph (1 day)

**Goal:** Remove filesystem dependency

- BlobActor chooses backend (system decision)
- Export on-demand only
- Full actor model compliance
- **Status:** Design complete

---

## Success Criteria

### Design Phase (COMPLETE ✅)

**MUST:**
- ✅ Terminology corrected (Content, not Deliverable)
- ✅ Layer separation documented
- ✅ Index-based model specified
- ✅ System management boundary clear

**SHOULD:**
- ✅ Examples demonstrate all three layers
- ✅ Migration path defined
- ✅ Performance strategy outlined

**MAY:**
- ✅ Architecture diagram with ASCII art
- ✅ Decision log with rationale
- ✅ Quick reference guide

### Implementation Phase (PENDING)

**MUST:**
- ContentActor factory implemented
- BlobActor abstraction layer implemented
- Section index computation working
- On-demand section reading working

**SHOULD:**
- Multiple backends (filesystem, memory)
- Tag/embedding search
- Migration script
- CLI integration

---

## Related Documentation

### Actor Worldview

- [Actor Worldview README](README.md) - Actor model principles
- [Pure Actor Model Architecture](PURE_ACTOR_MODEL_ARCHITECTURE.md) - Pure actor design
- [Blob Storage Actor Redesign](BLOB_STORAGE_ACTOR_REDESIGN.md) - BlobActor implementation

### Integration Points

- **Agent Workflow:** How agents create content on completion
- **Task System:** How tasks link to content deliverables
- **Graph System:** How content nodes integrate with graph
- **CLI:** How to query and export content

---

## File Inventory

**Total Documentation:** 3,842 lines across 6 files

| File | Lines | Type | Purpose |
|------|-------|------|---------|
| CONTENT_STORAGE_ARCHITECTURE.md | 650 | Architecture | High-level overview |
| GRAPH_CONTENT_STORAGE_DESIGN.md | 1339 | Design | Detailed specification |
| CONTENT_STORAGE_QUICK_REF.md | 280 | Reference | Quick lookup |
| CONTENT_STORAGE_SCHEMA.datalog | 305 | Schema | Datalog relations |
| content-storage-examples.ts | 655 | Examples | Working code |
| CONTENT_STORAGE_REFINEMENT_REPORT.md | 450 | Report | Completion summary |
| CONTENT_STORAGE_INDEX.md | 163 | Index | This file |

---

## Quick Links

**Architecture & Design:**
- [Architecture Overview](CONTENT_STORAGE_ARCHITECTURE.md#three-layer-architecture)
- [Index-Based Model](CONTENT_STORAGE_ARCHITECTURE.md#3-index-based-model)
- [System-Managed Performance](CONTENT_STORAGE_ARCHITECTURE.md#4-system-managed-performance)
- [Migration Path](CONTENT_STORAGE_ARCHITECTURE.md#migration-path)

**Implementation:**
- [ContentActor Interface](GRAPH_DELIVERABLE_STORAGE_DESIGN.md#1-contentactor-formerly-deliverableactor)
- [BlobActor Interface](GRAPH_DELIVERABLE_STORAGE_DESIGN.md#3-blobactor-storage-abstraction)
- [Datalog Schema](DELIVERABLE_STORAGE_SCHEMA.datalog)
- [Code Examples](../examples/content-storage-examples.ts)

**Reference:**
- [Terminology Cheat Sheet](CONTENT_STORAGE_QUICK_REF.md#terminology-cheat-sheet)
- [Common Operations](CONTENT_STORAGE_QUICK_REF.md#common-operations)
- [Graph Edges](CONTENT_STORAGE_QUICK_REF.md#graph-edges)
- [Datalog Queries](CONTENT_STORAGE_QUICK_REF.md#datalog-queries)

---

**Status:** Documentation Complete - Ready for Implementation Phase 1
**Next Step:** Create implementation tasks for ContentActor, BlobActor, Section Index
**Review:** Design reviewed and approved (2026-01-18)
