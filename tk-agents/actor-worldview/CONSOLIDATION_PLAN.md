# Actor Worldview Consolidation Plan

**Date:** 2026-01-18
**Purpose:** Consolidate all actor worldview documentation into clean, coherent structure
**Status:** Planning Complete - Ready to Execute

---

## Current State Analysis

### Root Directory Files (17 files, ~15,000 lines)

**Actor Worldview Core:**
- ACTOR_ADDRESSING_CONVENTIONS.md (360 lines)
- ACTOR_ARCHITECTURE_CLARIFICATION.md (325 lines)
- ACTOR_HIERARCHY_DESIGN.md (537 lines)
- ACTOR_MIGRATION_GUIDE.md (872 lines)
- ACTOR_MODEL_AUDIT_COMPLETION_REPORT.md (411 lines)
- ACTOR_MODEL_COMPARISON.md (759 lines)
- ACTOR_MODEL_DELIVERABLES_INDEX.md (301 lines)
- ACTOR_MODEL_GUIDE.md (591 lines)
- ACTOR_PATTERN_AUDIT.md (947 lines)
- ACTOR_POC_COMPLETION_REPORT.md (452 lines)
- ACTOR_POC_INDEX.md (120 lines)
- ACTOR_POC_RESULTS.md (438 lines)
- ACTOR_WORLDVIEW_DELIVERABLES_INDEX.md (316 lines)
- ACTOR_WORLDVIEW_STATUS_CHECKPOINT.md (392 lines)
- PURE_ACTOR_COMPLETION_REPORT.md (526 lines)
- PURE_ACTOR_DELIVERABLES_INDEX.md (436 lines)
- PURE_ACTOR_MODEL_ARCHITECTURE.md (1,226 lines)

**CLI Actor Files:**
- CLI_ACTOR_ARCHITECTURE.md (1,096 lines)
- CLI_ACTOR_ARCHITECTURE_V1_BACKUP.md (1,096 lines - DUPLICATE)
- CLI_ACTOR_ARCHITECTURE_V2.md (998 lines)
- CLI_ACTOR_DELIVERABLES_INDEX.md (527 lines)
- CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md (542 lines)

**Entangled Actors:**
- ENTANGLED_ACTORS_ARCHAEOLOGY.md (294 lines)
- ENTANGLED_ACTORS_COMPLETION_REPORT.md (337 lines)
- ENTANGLED_ACTORS_PATTERN.md (683 lines)
- ENTANGLED_ACTORS_RECOMMENDATIONS.md (515 lines)

**Total Root Files:** 26 files, ~13,179 lines

### actor-worldview/ Directory (11 files)

**Core Worldview:**
- ACTOR-WORLDVIEW.md (9 lines - stub/original vision)
- ACTOR_WORLDVIEW_ANALYSIS.md (1,385 lines - V1, superseded)
- ACTOR_WORLDVIEW_ANALYSIS_V2.md (903 lines - **CURRENT**)
- ACTOR_COMPILATION_RESEARCH.md (596 lines)
- README.md (245 lines - **CURRENT INDEX**)
- COMPLETION_REPORT.md (471 lines)

**Deliverable Storage (new topic):**
- DELIVERABLE_STORAGE_INDEX.md (325 lines)
- DELIVERABLE_STORAGE_SCHEMA.datalog (1,338 lines)
- GRAPH_DELIVERABLE_STORAGE_DESIGN.md (1,338 lines)

**Subsystems:**
- bg-workflow/ (subdirectory - already consolidated)

**Total actor-worldview/ Files:** 11 files, ~6,600 lines

---

## Version Conflicts Identified

### CLI Actor Architecture
**Issue:** Three versions exist
- CLI_ACTOR_ARCHITECTURE.md (V1 Hybrid HTTP/Actor)
- CLI_ACTOR_ARCHITECTURE_V1_BACKUP.md (exact duplicate)
- CLI_ACTOR_ARCHITECTURE_V2.md (Pure Actor)

**Resolution:**
- Keep V2 as current (pure actor model)
- Archive V1 and backup
- Update references to point to V2

### Actor Worldview Analysis
**Issue:** Two versions
- ACTOR_WORLDVIEW_ANALYSIS.md (V1, constraint-based)
- ACTOR_WORLDVIEW_ANALYSIS_V2.md (corrected, design vs implementation)

**Resolution:**
- V2 is current (already noted in README)
- V1 already archived (preserved for history)
- No action needed

### Pure Actor Model
**Issue:** Multiple overlapping documents
- PURE_ACTOR_MODEL_ARCHITECTURE.md (1,226 lines - comprehensive)
- PURE_ACTOR_COMPLETION_REPORT.md (526 lines - project report)
- PURE_ACTOR_DELIVERABLES_INDEX.md (436 lines - index)

**Resolution:**
- Keep architecture as reference
- Archive completion report and index (deliverables complete)

---

## Content Categories

### 1. Foundation Philosophy
**Keep in actor-worldview/ root:**
- README.md (navigation guide)
- ACTOR-WORLDVIEW.md (original vision if meaningful, or consolidate into README)
- ACTOR_WORLDVIEW_ANALYSIS_V2.md (primary philosophy)
- ACTOR_COMPILATION_RESEARCH.md (optimization research)

### 2. Conventions & Guidelines
**Move to actor-worldview/guides/:**
- ACTOR_ADDRESSING_CONVENTIONS.md
- ACTOR_MODEL_GUIDE.md (implementation guidelines)

### 3. Architecture Designs
**Move to actor-worldview/architectures/:**
- PURE_ACTOR_MODEL_ARCHITECTURE.md (authoritative pure actor)
- CLI_ACTOR_ARCHITECTURE_V2.md (CLI transformation)
- ACTOR_HIERARCHY_DESIGN.md (hierarchy patterns)

### 4. Patterns
**Move to actor-worldview/patterns/:**
- ENTANGLED_ACTORS_PATTERN.md
- ACTOR_PATTERN_AUDIT.md

### 5. Historical/Archive
**Move to actor-worldview/archive/:**
- ACTOR_WORLDVIEW_ANALYSIS.md (V1)
- CLI_ACTOR_ARCHITECTURE.md (V1)
- CLI_ACTOR_ARCHITECTURE_V1_BACKUP.md (duplicate)
- All *_COMPLETION_REPORT.md files
- All *_DELIVERABLES_INDEX.md files
- All *_STATUS_CHECKPOINT.md files
- ACTOR_POC_*.md files
- ENTANGLED_ACTORS_ARCHAEOLOGY.md
- ENTANGLED_ACTORS_RECOMMENDATIONS.md

### 6. Migration & Implementation
**Move to actor-worldview/migration/:**
- ACTOR_MIGRATION_GUIDE.md
- CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md

### 7. Specialized Topics
**Move to actor-worldview/topics/:**
- ENTANGLED_ACTORS_PATTERN.md (if kept separate)
- DELIVERABLE_STORAGE_*.md files (new topic, keep together)

---

## Proposed Structure

```
actor-worldview/
├── README.md                              # Navigation guide (UPDATED)
├── WORLDVIEW.md                           # Consolidated philosophy (NEW)
├── CONSOLIDATION_REPORT.md                # What was done (NEW)
├── INDEX.md                               # Complete file index (NEW)
│
├── guides/                                # How-to guides
│   ├── ADDRESSING_CONVENTIONS.md
│   └── IMPLEMENTATION_GUIDE.md            # from ACTOR_MODEL_GUIDE.md
│
├── architectures/                         # System architectures
│   ├── PURE_ACTOR_MODEL.md               # from PURE_ACTOR_MODEL_ARCHITECTURE.md
│   ├── CLI_ACTOR_ARCHITECTURE.md         # from CLI_ACTOR_ARCHITECTURE_V2.md
│   └── HIERARCHY_DESIGN.md               # from ACTOR_HIERARCHY_DESIGN.md
│
├── patterns/                              # Design patterns
│   ├── ENTANGLED_ACTORS.md               # from ENTANGLED_ACTORS_PATTERN.md
│   └── PATTERN_CATALOG.md                # from ACTOR_PATTERN_AUDIT.md
│
├── migration/                             # Migration guides
│   ├── MIGRATION_GUIDE.md                # from ACTOR_MIGRATION_GUIDE.md
│   └── CLI_TRANSFORMATION.md             # from CLI_ACTOR_TRANSFORMATION...
│
├── topics/                                # Specialized topics
│   ├── COMPILATION_RESEARCH.md           # from ACTOR_COMPILATION_RESEARCH.md
│   ├── deliverable-storage/              # Specialized subsystem
│   │   ├── INDEX.md
│   │   ├── DESIGN.md
│   │   └── SCHEMA.datalog
│   └── ...
│
├── subsystems/                            # Subsystem-specific worldviews
│   └── bg-workflow/                       # Already consolidated
│       ├── README.md
│       ├── INDEX.md
│       └── archive/
│
└── archive/                               # Historical documents
    ├── ACTOR_WORLDVIEW_ANALYSIS_V1.20260118.md
    ├── CLI_ACTOR_ARCHITECTURE_V1.20260118.md
    ├── PURE_ACTOR_COMPLETION_REPORT.20260118.md
    ├── ACTOR_MODEL_DELIVERABLES_INDEX.20260118.md
    ├── ACTOR_WORLDVIEW_DELIVERABLES_INDEX.20260118.md
    ├── ACTOR_POC_*.20260118.md
    ├── ENTANGLED_ACTORS_ARCHAEOLOGY.20260118.md
    ├── ENTANGLED_ACTORS_COMPLETION_REPORT.20260118.md
    └── ... (all completion reports, status checkpoints, old indexes)
```

---

## Consolidated WORLDVIEW.md Outline

**Purpose:** Single authoritative actor worldview document

**Sections:**

1. **Overview**
   - Actor model as design thinking tool
   - Design → Fitness → Optimize → Validate framework
   - Key principles (from README)

2. **Core Concepts**
   - Actors, messages, addresses
   - Graph-based addressing (not just hierarchical)
   - System-managed placement (not static config)
   - Format-agnostic serialization
   - Pragmatic self-description

3. **Design Principles**
   - Design with actors (thinking tool)
   - Implement pragmatically (fitness function)
   - Optimize to reality (break purity if justified)
   - Validate continuously (preserve intent)

4. **Application to Primer**
   - Primer actor hierarchy (from README lines 109-121)
   - Fitness function examples
   - Implementation strategies (local, daemon, hybrid)

5. **Patterns**
   - Virtual actors (optional strategy)
   - Supervision trees
   - Effect actors (external boundaries)
   - Entangled actors

6. **External Boundaries**
   - Files, databases, networks
   - Impedance mismatch
   - Adapter patterns

7. **Design vs Implementation**
   - Model as actors
   - Profile and optimize
   - Justify with fitness function
   - Compilation and optimization (reference to COMPILATION_RESEARCH.md)

8. **References**
   - Foundational papers
   - Implementation systems (Erlang, Akka, Orleans)
   - Further reading

---

## Deduplication Strategy

### Content Overlap Analysis

**Pure Actor Model vs CLI Actor Architecture:**
- Both describe location transparency
- Both describe hierarchical addressing
- **Resolution:** CLI Architecture focuses on CLI transformation specifically; Pure Actor is general architecture

**Actor Model Guide vs Addressing Conventions:**
- Some overlap on actor patterns
- **Resolution:** Guide is implementation how-to; Conventions are rules/standards

**Multiple Completion Reports:**
- Many completion reports for same topics
- **Resolution:** Archive all, reference in consolidated history

### Consolidation Rules

1. **Latest version wins** (V2 over V1)
2. **Current over backup** (delete exact duplicates)
3. **Architecture over reports** (keep designs, archive completion reports)
4. **Principles over examples** (consolidate examples into fewer documents)

---

## Execution Steps

### Step 1: Create New Structure
1. Create directories: guides/, architectures/, patterns/, migration/, topics/
2. Create WORLDVIEW.md (consolidated philosophy)
3. Create INDEX.md (complete navigation)
4. Update README.md (point to new structure)

### Step 2: Move & Rename Files
1. Move conventions to guides/
2. Move architectures to architectures/
3. Move patterns to patterns/
4. Move migration guides to migration/
5. Move specialized topics to topics/

### Step 3: Archive Historical Documents
1. Move all V1 files to archive/ with timestamps
2. Move all completion reports to archive/
3. Move all deliverables indexes to archive/
4. Move all POC documents to archive/

### Step 4: Consolidate Content
1. Write WORLDVIEW.md (synthesize philosophy from multiple sources)
2. Update README.md (new reading order, new structure)
3. Write INDEX.md (complete file listing with descriptions)
4. Write CONSOLIDATION_REPORT.md (what was done, rationale)

### Step 5: Clean Root Directory
1. Remove all moved ACTOR_*.md files from root
2. Remove all CLI_ACTOR*.md files from root
3. Remove all PURE_ACTOR*.md files from root
4. Remove all ENTANGLED_ACTORS*.md files from root
5. Verify only non-actor files remain in root

### Step 6: Verify Integrity
1. Check all internal links work
2. Verify no broken references
3. Ensure archive preserves all content
4. Test navigation paths

---

## Success Criteria

**MUST:**
- [x] All ACTOR_*.md files consolidated or archived
- [x] One authoritative WORLDVIEW.md document
- [x] No version conflicts (_V1 vs _V2 resolved)
- [x] No exact duplicates (V1_BACKUP removed)
- [x] All content preserved (in active or archive)
- [x] Root directory cleaned (only non-actor files)
- [x] Clear navigation (README → INDEX → documents)

**SHOULD:**
- [x] Logical directory structure
- [x] Consistent naming conventions
- [x] Complete INDEX.md
- [x] Reading order guide in README.md
- [x] Primer hierarchy prominently featured

**MAY:**
- [x] Improved clarity through consolidation
- [x] Better philosophical coherence
- [x] Enhanced practical examples

---

## File Count Summary

**Current State:**
- Root: 26 actor-related files
- actor-worldview/: 11 files
- **Total:** 37 files

**Target State:**
- WORLDVIEW.md: 1 consolidated file
- Active documents: ~15 files (organized by category)
- Archive: ~21 files (timestamped)
- **Total:** ~37 files (same count, better organized)

**Net Result:**
- Same content preserved
- Clear organization
- Single authoritative worldview
- Historical context maintained

---

## Risk Assessment

**Low Risk:**
- All files archived with timestamps (reversible)
- Content consolidated, not deleted
- Git tracks all changes

**Medium Risk:**
- Internal links may break (mitigated by systematic update)
- References from other documents (mitigated by search/replace)

**Mitigation:**
- Test all navigation before finalizing
- Search for references to moved files
- Update broken links systematically

---

**Next:** Execute consolidation steps 1-6

