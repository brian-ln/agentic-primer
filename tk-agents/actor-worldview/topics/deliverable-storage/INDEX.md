# Graph Deliverable Storage - Index

**Version:** 1.0
**Date:** 2026-01-18
**Status:** Design Complete

---

## Overview

This folder contains the complete design for graph-based deliverable storage, replacing filesystem-based agent artifacts with queryable graph nodes.

**Problem:** Agent deliverables scatter markdown files across project root (100+ files), with no queryable relationships or hierarchical structure.

**Solution:** Store deliverables as graph nodes with blob storage actors and markdown-as-tree model.

---

## Documents

### 1. GRAPH_DELIVERABLE_STORAGE_DESIGN.md
**Primary design document** (34 KB, 1338 lines)

**Contents:**
- Architecture overview
- Node types (DeliverableActor, SectionActor, BlobActor)
- Edge types (5 relationship types)
- Blob storage actor interface
- Hierarchical markdown model
- Query examples (Datalog + TypeScript)
- Migration path (3 phases)
- Integration points (agent completion, CLI, dashboard)
- Implementation tasks (12 tasks)
- Open questions (5 design decisions)
- Success metrics (MUST/SHOULD/MAY)

**Read this first** for complete understanding.

---

### 2. DELIVERABLE_STORAGE_SCHEMA.datalog
**Datalog schema specification** (8 KB, 304 lines)

**Contents:**
- Node relations (deliverable, section, blob)
- Edge relations (5 types)
- Indexes (performance)
- Sample queries (10 queries)
- Migration queries
- Validation rules
- Materialized views
- Event sourcing (optional)

**Use this** for CozoDB schema implementation.

---

### 3. ../examples/deliverable-storage-examples.ts
**TypeScript usage examples** (18 KB, 654 lines)

**Contents:**
- 10 complete working examples
- Helper functions
- Ready-to-run code

**Examples:**
1. Agent creates deliverable
2. Query agent deliverables
3. Query task deliverables
4. Navigate sections
5. Read section content
6. Filter by type
7. Validate integrity
8. Export to file
9. Search by tag
10. Aggregate by type

**Use this** for implementation reference.

---

### 4. ../GRAPH_DELIVERABLE_STORAGE_COMPLETION.md
**Completion report** (root directory)

**Contents:**
- Executive summary
- Research findings
- Deliverables overview
- Architecture summary
- Migration strategy
- Success criteria compliance
- Next steps

**Use this** for quick summary and status.

---

## Quick Start

### For Understanding the Design

1. Read **GRAPH_DELIVERABLE_STORAGE_DESIGN.md** (start here)
2. Review **DELIVERABLE_STORAGE_SCHEMA.datalog** (schema reference)
3. Explore **deliverable-storage-examples.ts** (concrete examples)

### For Implementation

1. Study **Node Types** section (DeliverableActor, SectionActor)
2. Review **Edge Types** section (5 relationship types)
3. Reference **deliverable-storage-examples.ts** for patterns
4. Follow **Implementation Tasks** section (12 tasks, 4 phases)

### For Integration

1. Check **Integration Points** section (agent completion, CLI, dashboard)
2. Review **Query Examples** (Datalog + TypeScript)
3. Study **Migration Path** (3 phases: hybrid → graph-primary → pure graph)

---

## Key Concepts

### Actors

**DeliverableActor:**
- Stores metadata (title, type, size, hash)
- References BlobActor for content
- Linked to agent (creator) and task (owner)

**SectionActor:**
- Represents markdown heading
- Hierarchical structure via parent_section edges
- Position tracking (lineStart, lineEnd)

**BlobActor (existing):**
- Stores raw content
- Validates integrity (hash, size)
- Message-based interface

### Edges

1. **created_deliverable:** agent → deliverable
2. **has_deliverable:** task → deliverable
3. **stored_in_blob:** deliverable → blob
4. **contains_section:** deliverable → section
5. **parent_section:** child section → parent section

### Queries

```datalog
# Get all deliverables for agent
?[deliverable_id, title, type] :=
  *created_deliverable{ from_id: $agent_id, to_id: deliverable_id },
  *deliverable{ id: deliverable_id, title, deliverable_type: type }
```

```typescript
// Navigate section hierarchy
const sections = await getSectionHierarchy(deliverableId);
printSectionTree(sections);
```

---

## Migration Phases

### Phase 1: Hybrid (Filesystem + Graph)
- Write to both filesystem and graph
- Validate consistency
- No breaking changes

**Code:** See design doc section "Phase 1: Hybrid"

### Phase 2: Graph Primary
- Store in `docs/deliverables/`
- CLI queries graph
- Old files remain

**Code:** See design doc section "Phase 2: Graph Primary"

### Phase 3: Pure Graph
- In-memory or CozoDB blobs
- Export on-demand
- Full actor model

**Code:** See design doc section "Phase 3: Pure Graph Storage"

---

## Integration Points

### Agent Completion
```typescript
// Agent creates deliverable on completion
const deliverableId = await createDeliverable(
  agentId,
  taskId,
  content,
  { title, type, description, tags }
);
```

### CLI Commands
```bash
bun src/cli/task.ts deliverables --agent agent_bg_a7e4d2
bun src/cli/task.ts sections deliverable_123
bun src/cli/task.ts export-deliverable deliverable_123
```

### Dashboard
- Deliverables list widget
- Section navigation widget
- Hierarchical tree view

### Graph Queries
- 10 Datalog queries in schema
- TypeScript API wrappers
- Real-time graph traversal

---

## File Organization

```
actor-worldview/
├── GRAPH_DELIVERABLE_STORAGE_DESIGN.md  # Main design (read first)
├── DELIVERABLE_STORAGE_SCHEMA.datalog   # Schema spec
└── DELIVERABLE_STORAGE_INDEX.md         # This file

examples/
└── deliverable-storage-examples.ts      # 10 working examples

/ (root)
└── GRAPH_DELIVERABLE_STORAGE_COMPLETION.md  # Completion report
```

---

## Implementation Roadmap

### Phase 1: Foundation (P0) - 3-4 days
- DeliverableActor factory
- SectionActor factory
- MarkdownGraph integration
- Agent completion update

### Phase 2: Queries (P1) - 2 days
- Datalog schema
- CLI commands
- Dashboard widgets

### Phase 3: Migration (P2) - 1 day
- Hybrid storage
- Migration script
- Graph-primary transition

### Phase 4: Pure Graph (P3) - Future
- In-memory blobs
- CozoDB integration

---

## Success Metrics

### MUST (Objective)
- All agent completions create deliverable nodes
- 100% deliverables have blob references
- All tests pass

### SHOULD (Measured)
- Query performance <100ms
- Migration success rate >95%
- Zero hash mismatches

### MAY (Subjective)
- API ergonomics
- CLI usability
- Design elegance

---

## Related Documents

### Existing Systems
- **BlobActor:** ../BLOB_STORAGE_ACTOR_REDESIGN.md
- **MarkdownGraph:** ../src/markdown-graph/README.md
- **Agent Workflow:** bg-workflow/README.md
- **Graph System:** ../src/graph.ts

### Actor Worldview
- **README:** README.md (framework overview)
- **Analysis:** ACTOR_WORLDVIEW_ANALYSIS_V2.md
- **Compilation:** ACTOR_COMPILATION_RESEARCH.md

---

## Status

**Design:** ✅ Complete (2026-01-18)
**Implementation:** Pending (awaiting review)
**Review:** Needed

---

## Reading Order

**For quick overview:**
1. This file (DELIVERABLE_STORAGE_INDEX.md)
2. Completion report (../GRAPH_DELIVERABLE_STORAGE_COMPLETION.md)

**For full understanding:**
1. Main design (GRAPH_DELIVERABLE_STORAGE_DESIGN.md)
2. Schema spec (DELIVERABLE_STORAGE_SCHEMA.datalog)
3. Examples (../examples/deliverable-storage-examples.ts)

**For implementation:**
1. Main design → "Implementation Tasks" section
2. Examples → Helper functions
3. Schema → Datalog queries

---

**Document Status:** Index and Navigation
**Created:** 2026-01-18
**Purpose:** Quick reference and navigation for deliverable storage design
