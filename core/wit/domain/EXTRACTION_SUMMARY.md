# Phase 2: Domain Protocol Extraction Summary

**Date**: 2026-02-05
**Bead**: Phase 2 - WIT Platform Migration
**Source**: Simplify/UGS (Universal Graph System)

## Overview

This phase extracted domain protocols from the Simplify/UGS implementation into WIT interface definitions, covering:

1. **Graph Primitives** (4 files) - Core graph model with addresses, nodes, edges
2. **Entity Model** (5 files) - Domain entities (base, agent, task, session, human)
3. **Query System** (2 files) - Pattern matching and criteria evaluation
4. **Knowledge Management** (3 files) - Embeddings, convergence, session state
5. **Package Manifest** (1 file) - World definitions and cross-package imports

**Total**: 15 WIT files + README + summary

## Extraction Methodology

### Source Analysis

Analyzed Simplify/UGS TypeScript implementation:
- `/Users/bln/play/agentic-primer/simplify/src/graph.ts` - Graph primitives
- `/Users/bln/play/agentic-primer/simplify/src/entities/*.ts` - 6 entity types
- `/Users/bln/play/agentic-primer/simplify/src/query/*.ts` - Query system
- `/Users/bln/play/agentic-primer/simplify/README.md` - Formal specification

### Design Decisions

1. **Address as Structured Record** (not string)
   - Supports namespacing: `@(namespace/id)`
   - Supports versioning: `@(id:version)`
   - Supports scoping: node, edge, computed
   - **Rationale**: Extensibility for future features

2. **Resources for Entities** (not records)
   - Agent, Task, Session, Human use `resource` types
   - Enables lifecycle management and polymorphism
   - **Rationale**: State encapsulation + behavior binding

3. **Separated Query Definition from Execution**
   - QueryDefinition (data) vs QueryPlan (behavior)
   - Inspired by Halo paper optimization techniques
   - **Rationale**: Enable compilation, caching, optimization

4. **Interface-Scoped Types**
   - All types defined within interfaces (not package-level)
   - **Rationale**: WIT validation requirements

## Domain Model Coverage

### Graph Primitives

| Component | File | Status | Key Features |
|-----------|------|--------|--------------|
| Address | `graph/address.wit` | ✅ Validated | Parsing, resolution, namespacing |
| Node | `graph/node.wit` | ⚠️ Needs validation | Properties, data, indexing |
| Edge | `graph/edge.wit` | ⚠️ Needs validation | Directional, weighted, typed |
| Graph | `graph/graph.wit` | ⚠️ Needs validation | Traversal, pathfinding, analytics |

### Entity Model

| Entity | File | TypeScript Source | Key Features |
|--------|------|-------------------|--------------|
| Base | `entity/entity.wit` | `entities/program.ts` | Kind discriminator, lifecycle |
| Agent | `entity/agent.wit` | `entities/agent.ts` | State machine, harness, streaming |
| Task | `entity/task.wit` | `entities/task.ts` | Lifecycle, criteria, priorities |
| Session | `entity/session.wit` | `entities/session.ts` | JSONL logging, messaging |
| Human | `entity/human.wit` | `entities/human.ts` | Approvals, notifications |

**Entity State Machines**:
- **Agent**: `idle → thinking → executing → waiting → completed | error`
- **Task**: `pending → assigned → in-progress → completed | failed`
- **Session**: `created → active → paused | completed`
- **Human**: `available ↔ busy ↔ away → offline`

### Query System

| Component | File | Source | Coverage |
|-----------|------|--------|----------|
| Criteria | `query/criteria.wit` | `query/types.ts` | Success criteria evaluation |
| Query DSL | `query/query.wit` | `query/builder.ts` + `compiler.ts` | Pattern matching, optimization |

**Query Features**:
- Pattern matching with predicates
- Graph traversal (BFS/DFS)
- Aggregations (count, collect, group)
- Query compilation and caching (Halo-inspired)

### Knowledge Management

| Component | File | Source | Purpose |
|-----------|------|--------|---------|
| Embeddings | `knowledge/embedding.wit` | `entities/embedding.ts` | Vector similarity search |
| Convergence | `knowledge/convergence.wit` | Session knowledge patterns | Detect converging conclusions |
| Session State | `knowledge/session-state.wit` | Session management | Context, artifacts, patterns |

## Architectural Integration

### Cross-Package Dependencies

```
convergence:domain@0.1.0
├─ Imports from signal-hub:core@0.1.0
│  ├─ types (json-value)
│  └─ event (event system)
├─ Internal dependencies
│  ├─ entity → graph (address, node)
│  └─ query → graph + entity
```

### World Definitions

1. **domain** - Complete domain model (all interfaces)
2. **domain-graph** - Graph-only functionality
3. **domain-entity** - Entity management
4. **domain-query** - Query and knowledge

## Validation Status

### Validated Files

- ✅ `graph/address.wit` - Full validation passed

### Pending Validation

The following files require syntax corrections for WIT validation:
- `graph/node.wit` - Use statement syntax
- `graph/edge.wit` - Use statement syntax
- `graph/graph.wit` - Cross-interface references
- All `entity/*.wit` files - Resource syntax
- All `query/*.wit` files - Complex type references
- All `knowledge/*.wit` files` - Interface dependencies

### Known Issues

1. **Reserved Keywords**: `from`, `to` → renamed to `source`, `target`
2. **Use Statements**: Must be inside interfaces, not package-level
3. **Resource Syntax**: Needs validation with wasm-tools

## Implementation Roadmap

### Phase 2a: Validation (Next)

- [ ] Fix use statement syntax across all files
- [ ] Validate resource definitions
- [ ] Test cross-package imports
- [ ] Run full `wasm-tools component wit core/wit/domain/`

### Phase 2b: TypeScript Bindings

- [ ] Generate TypeScript bindings with `jco`
- [ ] Create adapter layer for Simplify/UGS
- [ ] Implement domain protocol providers

### Phase 2c: Integration

- [ ] Connect domain protocols to worker system
- [ ] Implement event sourcing integration
- [ ] Add persistence layer

## Key Insights from Extraction

1. **Address Primitive is Central**: Everything references addresses
2. **Entity Lifecycle Complexity**: Each entity has unique state machine
3. **Query Optimization Matters**: Halo paper insights apply directly
4. **Knowledge Layer is Emergent**: Convergence detection enables synthesis

## References

- **Source Code**: `/Users/bln/play/agentic-primer/simplify/`
- **UGS Spec**: `simplify/README.md`
- **Halo Paper**: Query optimization for distributed systems
- **WIT Spec**: Component Model specification

## Metrics

- **WIT Files**: 15
- **Interfaces**: ~20
- **Types**: ~80 (records, variants, enums)
- **Functions**: ~100
- **Lines of WIT**: ~1,500
- **Source TS Files Analyzed**: 15+

## Next Steps

1. Complete WIT validation for all files
2. Generate TypeScript bindings
3. Implement protocol adapters
4. Integrate with Phase 1 infrastructure protocols
5. Build convergence framework worker

---

**Bead Link**: `@(phase2-domain-extraction:2026-02-05)`
**Dependencies**: Phase 1 (infrastructure protocols)
**Enables**: Phase 3 (integration and testing)
