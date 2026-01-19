# Actor Worldview - Complete File Index

**Date:** 2026-01-18
**Version:** 2.0 Consolidated
**Purpose:** Complete navigation guide to all actor worldview documentation

---

## Quick Navigation

| Need | Start Here |
|------|------------|
| **Understanding the philosophy** | [WORLDVIEW.md](./WORLDVIEW.md) |
| **Getting started** | [README.md](./README.md) |
| **Implementation guidelines** | [guides/IMPLEMENTATION_GUIDE.md](./guides/IMPLEMENTATION_GUIDE.md) |
| **Primer-specific application** | [WORLDVIEW.md#application-to-primer](./WORLDVIEW.md#application-to-primer) |
| **Design patterns** | [patterns/](./patterns/) |
| **Migration guides** | [migration/](./migration/) |
| **Historical context** | [archive/](./archive/) |

---

## Core Documents

### WORLDVIEW.md (NEW - Authoritative)
**Size:** ~20KB | **Status:** Current

**Consolidated actor worldview philosophy** integrating all research and user feedback.

**Sections:**
1. Seven Key Principles
   - Graph-based addressing
   - System-managed placement
   - Format-agnostic serialization
   - Pragmatic self-description
   - Virtual actors optional
   - External system boundaries
   - Design vs implementation
2. Design Framework (Design → Fitness → Optimize → Validate)
3. Application to Primer
4. Design Patterns
5. Optimization Techniques
6. References

**Read this:** For complete understanding of actor model as design tool

### README.md
**Size:** 8KB | **Status:** Navigation guide (updated)

**Purpose:** Entry point and reading order guide

**Contents:**
- Overview of actor worldview framework
- Key documents and purposes
- Reading order recommendations
- Design vs implementation mindset
- Status and next steps

**Read this:** First, to understand structure and navigation

---

## Organized by Category

### 📚 Guides (How-To)

#### guides/ADDRESSING_CONVENTIONS.md
**From:** ACTOR_ADDRESSING_CONVENTIONS.md
**Size:** 360 lines
**Purpose:** Actor hierarchy, graph integration, naming conventions

**Topics:**
- Hierarchical addressing (`primer.tasks.task_28`)
- Graph-based traversal
- Router actors as indexes
- Naming conventions and standards

**Use when:** Creating new actors or designing actor topology

#### guides/IMPLEMENTATION_GUIDE.md
**From:** ACTOR_MODEL_GUIDE.md
**Size:** 591 lines
**Purpose:** Practical implementation patterns and guidelines

**Topics:**
- Actor factory pattern
- Message handling
- Virtual actor lifecycle
- Testing strategies
- Common pitfalls

**Use when:** Implementing actors in code

---

### 🏗️ Architectures (System Designs)

#### architectures/PURE_ACTOR_MODEL.md
**From:** PURE_ACTOR_MODEL_ARCHITECTURE.md
**Size:** 1,226 lines
**Purpose:** Complete pure actor architecture with location transparency

**Topics:**
- Hierarchical addressing
- Location transparency mechanism
- System topology configurations
- Message routing
- Migration strategy

**Use when:** Understanding full pure actor vision

#### architectures/CLI_ACTOR_ARCHITECTURE.md
**From:** CLI_ACTOR_ARCHITECTURE_V2.md
**Size:** 998 lines
**Purpose:** CLI transformation to actor-based thin shell

**Topics:**
- V2 pure actor architecture
- Comparison with V1 (hybrid HTTP/actor)
- CLI transformation patterns
- Configuration-driven deployment
- Testing strategies

**Use when:** Transforming CLIs to actor-based design

#### architectures/HIERARCHY_DESIGN.md
**From:** ACTOR_HIERARCHY_DESIGN.md
**Size:** 537 lines
**Purpose:** Actor hierarchy patterns and organization

**Topics:**
- Hierarchical supervision trees
- Collection vs instance actors
- Router actor patterns
- Parent-child relationships

**Use when:** Designing actor hierarchy for new subsystems

---

### 🎨 Patterns (Design Patterns)

#### patterns/ENTANGLED_ACTORS.md
**From:** ENTANGLED_ACTORS_PATTERN.md
**Size:** 683 lines
**Purpose:** Entangled actors pattern for coordinated state

**Topics:**
- Entangled actor pattern
- Coordination mechanisms
- Use cases and examples
- Implementation guidelines

**Use when:** Actors need coordinated state changes

#### patterns/PATTERN_CATALOG.md
**From:** ACTOR_PATTERN_AUDIT.md
**Size:** 947 lines
**Purpose:** Complete catalog of actor patterns in tk-agents

**Topics:**
- Virtual actor pattern
- Supervision tree pattern
- Effect actor pattern
- Router actor pattern
- Adapter actor pattern
- Pattern audit results

**Use when:** Choosing pattern for new feature

---

### 🔄 Migration (Implementation Guides)

#### migration/MIGRATION_GUIDE.md
**From:** ACTOR_MIGRATION_GUIDE.md
**Size:** 872 lines
**Purpose:** Migrating existing code to actor model

**Topics:**
- Migration strategy (phased approach)
- V1 → V1.5 → V2 roadmap
- Rollback plans
- Testing during migration
- Integration patterns

**Use when:** Planning migration from current system to actors

#### migration/CLI_TRANSFORMATION.md
**From:** CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md
**Size:** 542 lines
**Purpose:** CLI transformation completion report

**Topics:**
- Transformation pattern (parse → send → format)
- 88% LOC reduction results
- Performance improvements (12x faster)
- Before/after comparisons

**Use when:** Understanding CLI transformation results

---

### 🔬 Topics (Specialized)

#### topics/COMPILATION_RESEARCH.md
**From:** actor-worldview/ACTOR_COMPILATION_RESEARCH.md
**Size:** 596 lines
**Purpose:** Actor compilation and optimization research

**Topics:**
- Actor fusion techniques
- Specialization and inlining
- Dataflow compilation
- Hardware-aware optimization
- Bidirectional conversion (von Neumann ↔ actors)

**Use when:** Optimizing actor systems for performance

#### topics/deliverable-storage/
**Purpose:** Specialized subsystem for deliverable storage

**Files:**
- `INDEX.md` (325 lines) - Overview and navigation
- `DESIGN.md` (1,338 lines) - Complete design specification
- `SCHEMA.datalog` (1,338 lines) - Datalog schema definition

**Use when:** Working with deliverable storage subsystem

---

### 🗂️ Subsystems

#### subsystems/bg-workflow/
**Purpose:** Background workflow actor design (already consolidated)

**Files:**
- `README.md` - Consolidated bg-workflow worldview
- `INDEX.md` - Navigation guide
- `CONSOLIDATION_REPORT.md` - What was consolidated
- `archive/` - Historical bg-workflow documents

**Use when:** Understanding /bg workflow implementation

---

### 📦 Archive (Historical)

**Location:** `archive/`
**Purpose:** Historical documents preserved with timestamps

**Contents:**
- `ACTOR_WORLDVIEW_ANALYSIS_V1.20260118.md` - V1 analysis (superseded by V2)
- `WORLDVIEW_COMPLETION_REPORT.20260118.md` - Phase 1 completion report
- `CLI_ACTOR_ARCHITECTURE.20260118.md` - V1 hybrid HTTP/actor design
- `CLI_ACTOR_ARCHITECTURE_V1_BACKUP.20260118.md` - Exact duplicate of V1
- `PURE_ACTOR_COMPLETION_REPORT.20260118.md` - Pure actor project report
- `PURE_ACTOR_DELIVERABLES_INDEX.20260118.md` - Pure actor deliverables index
- `ACTOR_MODEL_DELIVERABLES_INDEX.20260118.md` - Actor model deliverables index
- `ACTOR_WORLDVIEW_DELIVERABLES_INDEX.20260118.md` - Worldview deliverables index
- `CLI_ACTOR_DELIVERABLES_INDEX.20260118.md` - CLI actor deliverables index
- `ACTOR_WORLDVIEW_STATUS_CHECKPOINT.20260118.md` - Status checkpoint
- `ACTOR_MODEL_AUDIT_COMPLETION_REPORT.20260118.md` - Audit completion report
- `ACTOR_POC_INDEX.20260118.md` - POC index
- `ACTOR_POC_RESULTS.20260118.md` - POC results
- `ACTOR_POC_COMPLETION_REPORT.20260118.md` - POC completion report
- `ENTANGLED_ACTORS_ARCHAEOLOGY.20260118.md` - Entangled actors research
- `ENTANGLED_ACTORS_COMPLETION_REPORT.20260118.md` - Entangled actors completion
- `ENTANGLED_ACTORS_RECOMMENDATIONS.20260118.md` - Entangled actors recommendations
- `ACTOR_MODEL_COMPARISON.20260118.md` - Model comparison analysis
- `ACTOR_ARCHITECTURE_CLARIFICATION.20260118.md` - Architecture clarifications

**Note:** All content preserved for historical reference and archaeology

---

## Reading Paths

### Path 1: Quick Start (30 minutes)
1. [README.md](./README.md) - Understand structure (5 min)
2. [WORLDVIEW.md](./WORLDVIEW.md) - Read Executive Summary + Seven Principles (15 min)
3. [WORLDVIEW.md#application-to-primer](./WORLDVIEW.md#application-to-primer) - See Primer application (10 min)

**Outcome:** Understand actor worldview philosophy and Primer hierarchy

### Path 2: Implementation (2 hours)
1. [guides/IMPLEMENTATION_GUIDE.md](./guides/IMPLEMENTATION_GUIDE.md) - Learn patterns (30 min)
2. [guides/ADDRESSING_CONVENTIONS.md](./guides/ADDRESSING_CONVENTIONS.md) - Naming conventions (15 min)
3. [patterns/](./patterns/) - Study specific patterns (45 min)
4. [architectures/CLI_ACTOR_ARCHITECTURE.md](./architectures/CLI_ACTOR_ARCHITECTURE.md) - CLI example (30 min)

**Outcome:** Ready to implement actors in code

### Path 3: Deep Dive (4-6 hours)
1. Complete Path 1 and Path 2
2. [WORLDVIEW.md](./WORLDVIEW.md) - Read complete document (60 min)
3. [architectures/PURE_ACTOR_MODEL.md](./architectures/PURE_ACTOR_MODEL.md) - Full architecture (90 min)
4. [topics/COMPILATION_RESEARCH.md](./topics/COMPILATION_RESEARCH.md) - Optimization techniques (60 min)
5. [migration/MIGRATION_GUIDE.md](./migration/MIGRATION_GUIDE.md) - Migration strategy (45 min)

**Outcome:** Complete mastery of actor worldview and implementation strategies

### Path 4: Historical Context (2 hours)
1. Read current documents (Path 1)
2. [archive/ACTOR_WORLDVIEW_ANALYSIS_V1.20260118.md](./archive/ACTOR_WORLDVIEW_ANALYSIS_V1.20260118.md) - V1 analysis (45 min)
3. Compare V1 vs V2 (see what changed) (30 min)
4. [archive/ACTOR_MODEL_COMPARISON.20260118.md](./archive/ACTOR_MODEL_COMPARISON.20260118.md) - Model comparison (45 min)

**Outcome:** Understand evolution of actor worldview

---

## Document Statistics

**Active Documents:** 15 files
- Core: 2 (WORLDVIEW.md, README.md)
- Guides: 2
- Architectures: 3
- Patterns: 2
- Migration: 2
- Topics: 4 (including deliverable-storage/)

**Archive:** 18 historical documents (timestamped)

**Total:** 33 documents preserving all content

**Size:** ~45KB active content, ~60KB archived content

---

## Maintenance

### Adding New Documents

**For new guides:**
```bash
# Add to guides/ directory
cp NEW_GUIDE.md actor-worldview/guides/
# Update this INDEX.md
```

**For new architectures:**
```bash
# Add to architectures/ directory
cp NEW_ARCHITECTURE.md actor-worldview/architectures/
# Update this INDEX.md
```

**For new patterns:**
```bash
# Add to patterns/ directory
cp NEW_PATTERN.md actor-worldview/patterns/
# Update this INDEX.md
```

### Archiving Documents

When superseding a document:
```bash
# Archive with timestamp
cp DOCUMENT.md archive/DOCUMENT.YYYYMMDD.md
# Remove from active directory
# Update this INDEX.md
```

### Updating This Index

When structure changes:
1. Update category sections
2. Update file counts in statistics
3. Update reading paths if needed
4. Add changelog entry below

---

## Changelog

### 2026-01-18: Consolidation
- Created WORLDVIEW.md (consolidated all philosophy)
- Organized into guides/, architectures/, patterns/, migration/, topics/
- Archived 18 historical documents with timestamps
- Created comprehensive INDEX.md
- Updated README.md with new structure

### Before 2026-01-18
- Multiple scattered documents in root directory
- No clear organization
- Version conflicts (V1, V2, backups)
- Duplicate content

---

## Support

**Questions about:**
- **Philosophy:** See [WORLDVIEW.md](./WORLDVIEW.md)
- **Implementation:** See [guides/](./guides/)
- **Specific subsystems:** See [architectures/](./architectures/) or [topics/](./topics/)
- **Migration:** See [migration/](./migration/)
- **Historical context:** See [archive/](./archive/)

**Can't find something?**
- Check this INDEX.md
- Search archive/ for historical documents
- Check README.md for reading order

---

**Last Updated:** 2026-01-18
**Version:** 2.0 Consolidated
**Maintained by:** Actor worldview consolidation effort

