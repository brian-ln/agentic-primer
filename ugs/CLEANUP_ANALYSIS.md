# Documentation Cleanup & Organization Analysis

**Date**: 2026-02-06
**Scope**: agentic-primer (parent) + simplify (subdirectory)
**Safety**: All operations git-recoverable

---

## Executive Summary

**Current State:**
- **agentic-primer**: 99 root .md files, 45 archived, 390+ in experiments/
- **simplify**: 76 root .md files, 180 total (excluding node_modules)
- **Combined**: 275+ documentation files requiring organization

**Key Findings:**
1. **Heavy fragmentation**: Documentation scattered across root, docs/, archive/, experiments/
2. **Temporal accumulation**: 23 COMPLETION_REPORT files, 21 PHASE/STREAM files in simplify
3. **Knowledge extraction gap**: Valuable insights buried in reports, not in permanent knowledge base
4. **Duplicate security docs**: 18 SECURITY_* files in simplify root with overlapping content
5. **Archive underutilization**: Only 45 files archived in agentic-primer vs 99 active root files

**Cleanup Impact:**
- **Archive/Delete**: ~120 files (intermediate reports, redundant docs)
- **Extract to Knowledge**: ~40 documents with permanent value
- **Reorganize**: ~115 files into structured docs/
- **Keep in place**: ~50 permanent project docs

**Implementation Status:**
- ✅ **Beads Created**: 5 beads (1 epic + 4 phase tasks)
- ✅ **Total Effort**: 19 hours (1140 minutes)
- ✅ **Dependencies**: Sequential execution (Phase 1→2→3→4)
- ✅ **See**: CLEANUP_BEADS_SUMMARY.md for execution details

**Quick Start:**
```bash
bd ready --label phase-1     # Show Phase 1 bead
bd start simplify-06a        # Begin knowledge extraction (safe, no git changes)
```

---

## Section 1: Current State Analysis

### 1.1 agentic-primer (Parent Repo)

**Root Directory (99 .md files):**

**Permanent Documentation (Keep & Organize):**
- Core: README.md, ARCHITECTURE.md, PROJECT_OVERVIEW.md, SUMMARY.md (4)
- Bootstrap: BOOTSTRAP.md, SUCCESS_CRITERIA.md, AFTER_CLEAR.md (3)
- Activity System: ACTIVITY_QUICK_REFERENCE.md, ACTIVITY_WORKTREE_SYSTEM.md (2)
- Protocols: SUBAGENT_PROTOCOLS.md, SUBAGENT_PROTOCOLS_QUICK_REF.md, CLAUDE_ACTIVITY_PROTOCOL.md (3)

**Knowledge Extraction Candidates (40+ files):**
- Research: ACTOR_MODEL_RESEARCH_SUMMARY.md, COGNITIVE-SYSTEMS-SYNTHESIS.md (1933 lines)
- Analysis: BMAD_ANALYSIS_SUMMARY.md, DEV_TEAM_FORMULA_ANALYSIS.md (1031 lines)
- Architecture: ALTERNATIVE_ARCHITECTURES.md (814 lines), FORMULA_APPROACH_COMPARISON.md (862 lines)
- Discovery: DISCOVERY_REPORT.md, HARNESS_REUSABILITY_ANALYSIS.md (829 lines)
- Decisions: WIT_PLATFORM_MIGRATION_PLAN.md (1864 lines), SESSION_INTEGRATION.md (920 lines)

**Intermediate/Session Work (Archive or Delete):**
- Reports: BOOTSTRAP_DEPLOYMENT_TEST_RESULTS.md, BG_COMMAND_ENHANCEMENT_SUMMARY.md
- Logs: EXECUTION_LOG_V1.md
- Snapshots: STATUS.md (729 lines - point-in-time status)
- Index files: BMAD_ANALYSIS_INDEX.md, FORMULA_ARTIFACT_INDEX.md, SPEC_KIT_ANALYSIS_INDEX.md

**Subdirectories:**
- archive/ (45 .md files) - Good practice, but incomplete
- experiments/ (390+ .md files) - Massive accumulation, needs pruning
- core/wit/domain/ (8 .md files) - Protocol definitions (PERMANENT)
- docs/knowledge/ (1 dir) - Underutilized knowledge base location

### 1.2 simplify (Subdirectory)

**Root Directory (76 .md files):**

**Permanent Documentation (Keep):**
- Core: README.md (739 lines), ARCHITECTURE.md (1769 lines), ROADMAP.md
- Guides: AI_AGENT_GUIDE.md, TESTING.md
- Reference: AGENTS.md, ACTOR_MODEL.md

**Completion Reports (23 files - Archive/Delete):**
```
COMPLETION_REPORT.md
COMPLETION_REPORT_UPDATE.md
COMPLETION_REPORT_PREDICATE_PUSHDOWN.md
INTEGRATION_TESTS_SUMMARY.md (2 variants)
PHASE_5_COMPLETION_REPORT.md
STREAM_A4_COMPLETION_REPORT.md
STREAM_A5_COMPLETION_REPORT.md
STREAM_C1_COMPLETION_REPORT.md
STREAM_C3_COMPLETION_REPORT.md
STREAM_C4_COMPLETION_REPORT.md
TEST_DOCUMENTATION_SUMMARY.md
TEST_MIGRATION_REPORT.md
WORKFLOW_ORCHESTRATION_SUMMARY.md
... (10 more)
```

**Security Documentation Sprawl (18 files - Consolidate):**
```
SECURITY_AUDIT.md
SECURITY_BEST_PRACTICES.md
SECURITY_CHANGES.md
SECURITY_CHEATSHEET.md
SECURITY_DEFERRED.md
SECURITY_FINDINGS.md (952 lines)
SECURITY_FIXES.md (829 lines)
SECURITY_IMPLEMENTATION_REPORT.md (899 lines)
SECURITY_IMPLEMENTATION_SUMMARY.md
SECURITY_PATTERNS.md (639 lines)
SECURITY_PREVENTION.md (648 lines)
SECURITY_QUICK_REFERENCE.md
SECURITY_REVIEW.md (653 lines)
SECURITY_SETUP.md
SQL_INJECTION_PREVENTION_COMPLETE.md
SQL_PREVENTION_MANIFEST.md
... (2 more)
```

**Research Documents (Extract to Knowledge):**
- AGENTIC_WORKFLOW_RESEARCH.md
- BROWSER_LIFECYCLE_RESEARCH.md (2116 lines)
- GRAPH_QUERY_RESEARCH.md (1671 lines)
- OPENCLAW_ARCHITECTURE_MODEL.md (1296 lines)
- SSR_HYBRID_PATTERNS.md (1979 lines)

**Phase/Stream Plans (Archive):**
```
PHASE_2_PLAN.md (682 lines)
PHASE_3_PLAN.md
PHASE_5_7_IMPLEMENTATION_PLAN.md (810 lines)
BATCH_CLASSIFICATION_PLAN.md (1181 lines)
```

**Status Snapshots (Archive/Delete):**
- STATUS_RETROSPECTIVE.md (1084 lines)
- SESSION_SUMMARY.md
- NEXT_STEPS.md

**docs/ Subdirectory (68 .md files, well-organized):**
- specifications/ (7 files) ✅ Good structure
- analysis/ (3 files) ✅ Good structure
- workflows/ (4 files) ✅ Good structure
- archive/ (8 files) ✅ Good practice
- actors/, beads/, automation/, performance/, future/ ✅ Domain-organized

### 1.3 Git Status

**Uncommitted Changes:**
- agentic-primer: Clean working directory
- simplify: M package.json, ?? PHASE_5_7_IMPLEMENTATION_PLAN.md

**Recent Activity (Since 2026-01-01):**
- agentic-primer: 142 commits (WIT protocols, npm package, SEAG integration)
- simplify: 30 commits (path addressing, workflow orchestration, Phase 3)

### 1.4 File Type Distribution

**agentic-primer:**
```
Root .md:           99 files
archive/ .md:       45 files
experiments/ .md:  390+ files
core/wit/ .md:       8 files
TOTAL:            540+ .md files
```

**simplify:**
```
Root .md:           76 files
docs/ .md:          68 files (60 active, 8 archived)
src/ .md:           36 files (embedded docs)
TOTAL:             180 .md files
```

---

## Section 2: Knowledge Capture Strategy

### 2.1 What Belongs in ~/knowledge?

**Criteria for Knowledge Extraction:**
1. **Research findings** - Novel insights, technical discoveries
2. **Architecture decisions** - Why choices were made, alternatives considered
3. **Design patterns** - Reusable approaches, lessons learned
4. **Problem solutions** - Non-obvious fixes, debugging insights
5. **Comparative analysis** - Tradeoff evaluations, technology comparisons
6. **Domain expertise** - Deep dives into specific topics

**NOT Knowledge (Stays in Project):**
- API documentation
- User guides
- Setup instructions
- Project status
- Temporary reports

### 2.2 Knowledge Extraction Candidates

**From agentic-primer (40 files → ~25 knowledge docs):**

**Category: Research**
- COGNITIVE-SYSTEMS-SYNTHESIS.md (1933 lines) → ~/knowledge/ai/cognitive-systems-synthesis.md
- ACTOR_MODEL_RESEARCH_SUMMARY.md (808 lines) → ~/knowledge/architecture/actor-model-research.md
- HARNESS_REUSABILITY_ANALYSIS.md (829 lines) → ~/knowledge/testing/harness-reusability.md

**Category: Architecture Decisions**
- WIT_PLATFORM_MIGRATION_PLAN.md (1864 lines) → ~/knowledge/decisions/wit-platform-migration.md
- ALTERNATIVE_ARCHITECTURES.md (814 lines) → ~/knowledge/architecture/alternative-approaches.md
- FORMULA_APPROACH_COMPARISON.md (862 lines) → ~/knowledge/architecture/formula-approaches.md
- SESSION_INTEGRATION.md (920 lines) → ~/knowledge/decisions/session-integration.md

**Category: Design Patterns**
- ACTIVITY_WORKTREE_SYSTEM.md (746 lines) → ~/knowledge/patterns/activity-worktree-system.md
- SUBAGENT_PROTOCOLS.md (850 lines) → ~/knowledge/patterns/subagent-protocols.md
- BMAD_FORMULA_EXPRESSIONS.md (781 lines) → ~/knowledge/patterns/bmad-formulas.md

**Category: Analysis**
- BMAD_ANALYSIS_SUMMARY.md → ~/knowledge/analysis/bmad-approach.md
- DEV_TEAM_FORMULA_ANALYSIS.md (1031 lines) → ~/knowledge/analysis/dev-team-formula.md
- SPEC_KIT_ANALYSIS_SUMMARY.md (706 lines) → ~/knowledge/analysis/spec-kit.md
- DISCOVERY_REPORT.md → ~/knowledge/analysis/early-discoveries.md

**From simplify (15 files → ~10 knowledge docs):**

**Category: Research**
- BROWSER_LIFECYCLE_RESEARCH.md (2116 lines) → ~/knowledge/web/browser-lifecycle.md
- GRAPH_QUERY_RESEARCH.md (1671 lines) → ~/knowledge/databases/graph-query-patterns.md
- AGENTIC_WORKFLOW_RESEARCH.md → ~/knowledge/ai/agentic-workflows.md
- SSR_HYBRID_PATTERNS.md (1979 lines) → ~/knowledge/web/ssr-hybrid-patterns.md

**Category: Architecture**
- OPENCLAW_ARCHITECTURE_MODEL.md (1296 lines) → ~/knowledge/architecture/openclaw-model.md
- ACTOR_MODEL.md → ~/knowledge/patterns/actor-model-ugs.md
- GRAPH_ACTOR_IMPLEMENTATION.md (641 lines) → ~/knowledge/implementation/graph-actors.md

**Category: Design Review**
- DESIGN_REVIEW.md (1007 lines) → ~/knowledge/reviews/ugs-design-review.md
- STATUS_RETROSPECTIVE.md (1084 lines) → ~/knowledge/retrospectives/ugs-phase-3-retro.md

**Category: Security**
- SECURITY_FINDINGS.md (952 lines) → ~/knowledge/security/ugs-findings.md
- SECURITY_PATTERNS.md (639 lines) → ~/knowledge/security/patterns.md

### 2.3 Permanent Project Documentation

**agentic-primer - Keep in Place:**
- README.md, ARCHITECTURE.md, PROJECT_OVERVIEW.md, SUMMARY.md
- BOOTSTRAP.md, SUCCESS_CRITERIA.md, AFTER_CLEAR.md
- SIMULATION_HARNESS.md, RUN_SIMULATION.md
- core/wit/domain/ (protocol definitions)

**simplify - Keep in Place:**
- README.md, ARCHITECTURE.md, ROADMAP.md
- AI_AGENT_GUIDE.md, TESTING.md
- docs/ (entire structure remains)

### 2.4 Transient/Session-Specific (Archive or Delete)

**Archive (Historical Value):**
- All COMPLETION_REPORT_*.md files
- All PHASE_*_PLAN.md files
- All STREAM_*_COMPLETION_REPORT.md files
- STATUS_RETROSPECTIVE.md, SESSION_SUMMARY.md

**Delete (Low Value, Git-Recoverable):**
- *_INDEX.md files (generated, no unique content)
- NEXT_STEPS.md (superseded)
- Test completion summaries (TEST_*, INTEGRATION_TEST_*, SMOKE-TESTS-*)
- Duplicate security summaries (keep 3-4 best, archive rest)

---

## Section 3: Reorganization Plan

### 3.1 Target Directory Structure

**agentic-primer (Parent):**
```
agentic-primer/
├── README.md                    # ✅ Project overview
├── ARCHITECTURE.md              # ✅ System architecture
├── PROJECT_OVERVIEW.md          # ✅ Detailed overview
├── SUMMARY.md                   # ✅ Quick summary
│
├── docs/                        # ✨ NEW: Consolidated documentation
│   ├── bootstrap/               # ✨ Bootstrap system
│   │   ├── BOOTSTRAP.md
│   │   ├── SUCCESS_CRITERIA.md
│   │   └── AFTER_CLEAR.md
│   ├── activity/                # ✨ Activity management
│   │   ├── SYSTEM.md
│   │   └── QUICK_REFERENCE.md
│   ├── protocols/               # ✨ Agent protocols
│   │   ├── SUBAGENT.md
│   │   ├── QUICK_REF.md
│   │   └── CLAUDE_ACTIVITY.md
│   ├── simulation/              # ✨ Testing framework
│   │   ├── HARNESS.md
│   │   ├── RUN.md
│   │   └── ALGORITHM.md
│   ├── formula/                 # ✨ Formula system (consolidate 32 files)
│   │   ├── README.md
│   │   ├── IMPLEMENTATION.md
│   │   ├── EXAMPLES.md
│   │   └── COMPARISON.md
│   └── spec-kit/                # ✨ Spec-kit docs (consolidate 20 files)
│       ├── README.md
│       ├── VISUAL_GUIDE.md
│       └── WORKFLOW.md
│
├── archive/                     # ✅ Existing, expand to 120+ files
│   ├── 2025-12/                 # ✨ Date-organize archives
│   ├── 2026-01/
│   └── reports/                 # ✨ Completion reports
│
├── experiments/                 # ✅ Reduce from 390+ to ~100 files
│   ├── README.md
│   ├── iteration-2/
│   └── archive/                 # ✨ OLD: Move old runs here
│
├── core/
│   └── wit/                     # ✅ Keep as-is (protocol source)
│
└── packages/
    └── protocols/               # ✅ NEW: npm package
```

**simplify (Subdirectory):**
```
simplify/
├── README.md                    # ✅ UGS overview
├── ARCHITECTURE.md              # ✅ System architecture
├── ROADMAP.md                   # ✅ Development roadmap
├── AI_AGENT_GUIDE.md            # ✅ Agent instructions
├── TESTING.md                   # ✅ Test documentation
│
├── docs/                        # ✅ Already well-structured
│   ├── specifications/          # ✅ Keep as-is
│   ├── analysis/                # ✅ Keep as-is
│   ├── workflows/               # ✅ Keep as-is
│   ├── actors/                  # ✅ Keep as-is
│   ├── beads/                   # ✅ Keep as-is
│   ├── automation/              # ✅ Keep as-is
│   ├── performance/             # ✅ Keep as-is
│   ├── future/                  # ✅ Keep as-is
│   └── archive/                 # ✨ Expand with reports
│       ├── reports/             # ✨ NEW: All completion reports
│       ├── plans/               # ✨ NEW: All phase plans
│       └── security/            # ✨ NEW: Consolidated security docs
│
├── docs-archive/                # ✨ NEW: Root-level docs moved here
│   ├── research/                # ✨ Research that's been extracted
│   ├── security/                # ✨ Consolidated 18→3 files
│   └── sessions/                # ✨ Session reports
│
├── src/                         # ✅ Keep as-is
├── examples/                    # ✅ Keep as-is
├── beads/                       # ✅ Keep as-is
└── scripts/                     # ✅ Keep as-is
```

### 3.2 File Moves/Renames

**agentic-primer - Create Organized Structure:**

1. **Bootstrap System (3 files → docs/bootstrap/):**
   - BOOTSTRAP.md → docs/bootstrap/BOOTSTRAP.md
   - SUCCESS_CRITERIA.md → docs/bootstrap/SUCCESS_CRITERIA.md
   - AFTER_CLEAR.md → docs/bootstrap/AFTER_CLEAR.md

2. **Activity System (4 files → docs/activity/):**
   - ACTIVITY_WORKTREE_SYSTEM.md → docs/activity/SYSTEM.md
   - ACTIVITY_QUICK_REFERENCE.md → docs/activity/QUICK_REFERENCE.md
   - CLAUDE_ACTIVITY_PROTOCOL.md → docs/protocols/CLAUDE_ACTIVITY.md
   - ACTIVITY_SYSTEM_DIAGRAM.md → docs/activity/DIAGRAM.md

3. **Protocols (8 files → docs/protocols/):**
   - SUBAGENT_PROTOCOLS.md → docs/protocols/SUBAGENT.md
   - SUBAGENT_PROTOCOLS_QUICK_REF.md → docs/protocols/QUICK_REF.md
   - SUBAGENT_IMPLEMENTATION_GUIDE.md → docs/protocols/IMPLEMENTATION.md
   - CLARIFICATION_PROTOCOL_EXAMPLES.md → docs/protocols/CLARIFICATION_EXAMPLES.md

4. **Formula System (32 files → docs/formula/):**
   - Consolidate: BMAD_*, FORMULA_*, SPEC_KIT_FORMULA_* (32 files)
   - Create: docs/formula/README.md (index all content)
   - Keep: Top 5-6 most important files, link to rest

5. **Simulation/Testing (5 files → docs/simulation/):**
   - SIMULATION_HARNESS.md → docs/simulation/HARNESS.md
   - RUN_SIMULATION.md → docs/simulation/RUN.md
   - SIMULATION_ALGORITHM.md → docs/simulation/ALGORITHM.md
   - HARNESS_GUIDE.md → docs/simulation/GUIDE.md
   - HARNESS_TEMPLATE.md → docs/simulation/TEMPLATE.md

**simplify - Reorganize Root:**

1. **Completion Reports (23 files → docs/archive/reports/):**
   - All COMPLETION_REPORT_*.md
   - All STREAM_*_COMPLETION_REPORT.md
   - All *_SUMMARY.md (completion-related)

2. **Phase Plans (4 files → docs/archive/plans/):**
   - PHASE_2_PLAN.md
   - PHASE_3_PLAN.md
   - PHASE_5_7_IMPLEMENTATION_PLAN.md
   - BATCH_CLASSIFICATION_PLAN.md

3. **Security Consolidation (18 files → docs-archive/security/):**
   - Keep in root: SECURITY_BEST_PRACTICES.md (master doc)
   - Move to archive: All other SECURITY_*.md files
   - Create: docs-archive/security/README.md (index)

4. **Research (5 files → Extract to ~/knowledge, then archive):**
   - AGENTIC_WORKFLOW_RESEARCH.md → docs-archive/research/
   - BROWSER_LIFECYCLE_RESEARCH.md → docs-archive/research/
   - GRAPH_QUERY_RESEARCH.md → docs-archive/research/
   - OPENCLAW_ARCHITECTURE_MODEL.md → docs-archive/research/
   - SSR_HYBRID_PATTERNS.md → docs-archive/research/

5. **Session Reports (5 files → docs-archive/sessions/):**
   - STATUS_RETROSPECTIVE.md
   - SESSION_SUMMARY.md
   - IMPLEMENTATION_SUMMARY_SUBPROCESS.md
   - WORKER_CODE_EXECUTION_IMPLEMENTATION.md

### 3.3 Archive Candidates (120 files)

**agentic-primer (75 files):**
- 18 completion/status reports
- 32 formula system files (after consolidation)
- 10 analysis/index files
- 15 old bootstrap/harness variants

**simplify (45 files):**
- 23 completion reports
- 4 phase plans
- 15 security docs (consolidate to 3)
- 3 session reports

### 3.4 Deletion Candidates (Git-Recoverable, 30 files)

**agentic-primer:**
- *_INDEX.md files (10) - Generated content
- EXECUTION_LOG_V1.md - Superseded
- Duplicate summaries (5)

**simplify:**
- TEST_ISSUES.md - Resolved
- NEXT_STEPS.md - Superseded
- QUALITY_REVIEW.md - Completed, findings captured elsewhere
- Duplicate test summaries (8)
- PROTOCOL_INTEGRATION_PROMPT.md - Uncommitted work file

---

## Section 4: Implementation Phases

### Phase 1: Knowledge Extraction (High Value First)
**Duration**: 2-3 sessions
**Risk**: Low (no deletions)
**Dependencies**: Create ~/knowledge structure

**Steps:**
1. **Setup knowledge base structure:**
   ```bash
   mkdir -p ~/knowledge/{ai,architecture,patterns,decisions,analysis,implementation,security,reviews,retrospectives,web,databases,testing}
   ```

2. **Extract agentic-primer research (Top 10):**
   - COGNITIVE-SYSTEMS-SYNTHESIS.md (1933 lines)
   - WIT_PLATFORM_MIGRATION_PLAN.md (1864 lines)
   - DEV_TEAM_FORMULA_ANALYSIS.md (1031 lines)
   - SESSION_INTEGRATION.md (920 lines)
   - FORMULA_APPROACH_COMPARISON.md (862 lines)
   - ALTERNATIVE_ARCHITECTURES.md (814 lines)
   - ACTOR_MODEL_RESEARCH_SUMMARY.md (808 lines)
   - HARNESS_REUSABILITY_ANALYSIS.md (829 lines)
   - BMAD_FORMULA_EXPRESSIONS.md (781 lines)
   - DISCOVERY_REPORT.md

3. **Extract simplify research (Top 5):**
   - BROWSER_LIFECYCLE_RESEARCH.md (2116 lines)
   - SSR_HYBRID_PATTERNS.md (1979 lines)
   - GRAPH_QUERY_RESEARCH.md (1671 lines)
   - OPENCLAW_ARCHITECTURE_MODEL.md (1296 lines)
   - STATUS_RETROSPECTIVE.md (1084 lines)

4. **Extract security/design insights (5):**
   - SECURITY_FINDINGS.md → ~/knowledge/security/
   - SECURITY_PATTERNS.md → ~/knowledge/security/
   - DESIGN_REVIEW.md → ~/knowledge/reviews/
   - GRAPH_ACTOR_IMPLEMENTATION.md → ~/knowledge/implementation/

**Output**: 20 knowledge documents created, ready for permanent storage

### Phase 2: Reorganize Permanent Docs (Structure)
**Duration**: 1-2 sessions
**Risk**: Low (git-tracked moves)
**Dependencies**: Phase 1 complete

**Steps:**

**agentic-primer:**
1. Create docs/ structure:
   ```bash
   mkdir -p docs/{bootstrap,activity,protocols,simulation,formula,spec-kit}
   ```

2. Move bootstrap system (3 files)
3. Move activity system (4 files)
4. Move protocols (8 files)
5. Move simulation/testing (5 files)
6. Create docs/formula/README.md index
7. Create docs/spec-kit/README.md index

**simplify:**
1. Create archive structure:
   ```bash
   mkdir -p docs/archive/{reports,plans}
   mkdir -p docs-archive/{research,security,sessions}
   ```

2. No moves yet (archive phase handles this)

**Output**: Clean doc structure, easy navigation

### Phase 3: Archive Intermediate Work (Cleanup)
**Duration**: 2 sessions
**Risk**: Low (archiving, not deleting)
**Dependencies**: Phase 1 & 2 complete

**Steps:**

**agentic-primer:**
1. Move completion/status reports to archive/reports/ (18 files)
2. Move analysis files to archive/analysis/ (10 files)
3. Date-organize archive/ by month (archive/2025-12/, archive/2026-01/)
4. Consolidate formula files:
   - Keep docs/formula/README.md + top 5 files
   - Move remaining 27 to archive/formula/

**simplify:**
1. Move completion reports to docs/archive/reports/ (23 files)
2. Move phase plans to docs/archive/plans/ (4 files)
3. Move session reports to docs-archive/sessions/ (5 files)
4. Consolidate security docs:
   - Keep SECURITY_BEST_PRACTICES.md in root
   - Move 17 others to docs-archive/security/
   - Create docs-archive/security/README.md index
5. Move research (after extraction) to docs-archive/research/ (5 files)

**Output**: Clean root directories, organized archives

### Phase 4: Clean Redundant Content (Final Cleanup)
**Duration**: 1 session
**Risk**: Medium (deletions, but git-recoverable)
**Dependencies**: Phases 1-3 complete, verify no dependencies

**Steps:**

**agentic-primer:**
1. Delete index files (git rm):
   - BMAD_ANALYSIS_INDEX.md
   - FORMULA_ARTIFACT_INDEX.md
   - SPEC_KIT_ANALYSIS_INDEX.md
   - (7 more)

2. Delete superseded files:
   - EXECUTION_LOG_V1.md (duplicate in archive/)
   - Duplicate summaries (5 files)

3. Prune experiments/:
   - Archive runs older than 60 days
   - Move to experiments/archive/2025-*

**simplify:**
1. Delete resolved/completed files (git rm):
   - TEST_ISSUES.md
   - NEXT_STEPS.md
   - QUALITY_REVIEW.md
   - Duplicate test summaries (8 files)

2. Delete uncommitted work file:
   - PROTOCOL_INTEGRATION_PROMPT.md

3. Consolidate test docs:
   - Keep TESTING.md (master)
   - Keep docs/__tests__/README.md
   - Archive completion reports (already in Phase 3)

**Output**: Minimal root directories, no redundancy

---

## Section 5: Implementation Beads

### Bead Structure

```
cleanup-phase1-knowledge-extraction
├── depends: []
├── estimate: 6-8 hours
└── tasks:
    ├── Create ~/knowledge structure
    ├── Extract 20 high-value documents
    └── Verify no git changes in repos

cleanup-phase2-reorganize-docs
├── depends: [cleanup-phase1-knowledge-extraction]
├── estimate: 4-6 hours
└── tasks:
    ├── Create docs/ structure in agentic-primer
    ├── Move 20 files to new locations
    ├── Create index/README files
    └── Commit: "docs: reorganize permanent documentation"

cleanup-phase3-archive-intermediate
├── depends: [cleanup-phase2-reorganize-docs]
├── estimate: 4-5 hours
└── tasks:
    ├── Move 45 files to archives in simplify
    ├── Move 75 files to archives in agentic-primer
    ├── Create archive index files
    └── Commit: "docs: archive intermediate work products"

cleanup-phase4-delete-redundant
├── depends: [cleanup-phase3-archive-intermediate]
├── estimate: 2-3 hours
└── tasks:
    ├── Delete 30 redundant files
    ├── Prune experiments/ directory
    ├── Final verification
    └── Commit: "docs: remove redundant and obsolete files"
```

### Effort Summary

| Phase | Duration | Files Moved | Files Deleted | Risk |
|-------|----------|-------------|---------------|------|
| 1. Knowledge Extraction | 6-8h | 0 (copies) | 0 | Low |
| 2. Reorganize Docs | 4-6h | 20 | 0 | Low |
| 3. Archive Intermediate | 4-5h | 120 | 0 | Low |
| 4. Delete Redundant | 2-3h | 0 | 30 | Medium |
| **TOTAL** | **16-22h** | **140** | **30** | **Low** |

---

## Section 6: Git Safety & Recovery

### Safety Principles

1. **All operations are recoverable:**
   ```bash
   # Recover deleted file
   git checkout HEAD~1 -- path/to/file.md

   # Recover moved file history
   git log --follow -- path/to/new-location.md

   # Undo last commit
   git reset --soft HEAD~1
   ```

2. **Commit after each phase:**
   - Phase 1: No repo changes (external knowledge base)
   - Phase 2: "docs: reorganize permanent documentation"
   - Phase 3: "docs: archive intermediate work products"
   - Phase 4: "docs: remove redundant and obsolete files"

3. **Create recovery branch before Phase 4:**
   ```bash
   # Before deletions
   git checkout -b pre-cleanup-snapshot
   git checkout main
   # Proceed with Phase 4

   # If needed, recover:
   git checkout pre-cleanup-snapshot -- path/to/file.md
   ```

### Recovery Procedures

**Scenario 1: Accidentally deleted important file**
```bash
git log --diff-filter=D --summary | grep filename.md
git checkout <commit-hash>~1 -- path/to/filename.md
```

**Scenario 2: Wrong file moved**
```bash
git log --follow -- new/path/filename.md
git mv new/path/filename.md correct/path/filename.md
git commit --amend
```

**Scenario 3: Want to undo entire phase**
```bash
git reflog  # Find commit before phase
git reset --hard <commit-hash>
```

---

## Section 7: Decision Points (User Input Needed)

### Question 1: Knowledge vs Documentation Boundary

**Current proposal:**
- ~/knowledge: Research, decisions, patterns, analysis
- Project docs: API reference, guides, setup, architecture

**Question**: Do you agree with this boundary, or should some "decision" docs stay in-project?

**Example**: WIT_PLATFORM_MIGRATION_PLAN.md (1864 lines)
- Option A: Extract to ~/knowledge/decisions/ (proposed)
- Option B: Keep in docs/architecture/ (alternative)

### Question 2: Aggressive Deletion Policy

**Current proposal:**
- Delete 30 files (mostly redundant/superseded)
- Archive 120 files (historical value, but not frequently accessed)

**Question**: Are you comfortable with this level of cleanup? Alternative: Archive everything, delete nothing.

### Question 3: Security Docs Consolidation

**Current state**: 18 SECURITY_* files in simplify root

**Proposed consolidation:**
- Keep: SECURITY_BEST_PRACTICES.md (master doc in root)
- Archive: 17 other files to docs-archive/security/
- Create: docs-archive/security/README.md (index)

**Question**: Should security docs remain highly visible in root, or is consolidated archive acceptable?

### Question 4: Experiments Directory

**Current state**: 390+ .md files in agentic-primer/experiments/

**Proposal**: Move runs older than 60 days to experiments/archive/

**Question**: What retention policy for experiment runs? 60 days? 90 days? Keep all?

---

## Appendix A: File Inventory

### A.1 agentic-primer Root Files (99 total)

**By Category:**
- Permanent (15): README, ARCHITECTURE, protocols, bootstrap
- Knowledge (40): Research, analysis, decisions
- Reports (18): Completion, status, logs
- Formula/BMAD (32): System documentation
- Index/Generated (10): Redundant
- Superseded (4): Old versions

**Largest Files:**
1. COGNITIVE-SYSTEMS-SYNTHESIS.md (1933 lines)
2. WIT_PLATFORM_MIGRATION_PLAN.md (1864 lines)
3. HARNESS_GUIDE.md (1555 lines)
4. FORMULA_IMPLEMENTATION_ROADMAP.md (1210 lines)
5. DEV_TEAM_FORMULA_ANALYSIS.md (1031 lines)

### A.2 simplify Root Files (76 total)

**By Category:**
- Permanent (10): README, ARCHITECTURE, guides
- Reports (23): Completion, streams, phases
- Security (18): Sprawled documentation
- Research (5): Large research docs
- Plans (4): Phase plans
- Tests (8): Completion summaries
- Other (8): Mixed

**Largest Files:**
1. BROWSER_LIFECYCLE_RESEARCH.md (2116 lines)
2. SSR_HYBRID_PATTERNS.md (1979 lines)
3. ARCHITECTURE.md (1769 lines)
4. GRAPH_QUERY_RESEARCH.md (1671 lines)
5. OPENCLAW_ARCHITECTURE_MODEL.md (1296 lines)

### A.3 docs/ Subdirectories

**simplify/docs/ (68 files, well-organized):**
- specifications/ (7): ✅ Production specs
- analysis/ (3): ✅ Technical analysis
- workflows/ (4): ✅ Workflow docs
- actors/ (2): ✅ Actor implementations
- beads/ (5): ✅ Bead documentation
- automation/ (1): ✅ Automation docs
- performance/ (1): ✅ Benchmarks
- future/ (5): ✅ Future plans
- archive/ (8): ✅ Archived docs
- Other (32): Mixed documentation

**agentic-primer/docs/ (1 subdirectory):**
- knowledge/ (empty): Underutilized

---

## Appendix B: Knowledge Categories

### Category Taxonomy

**ai/** (4 docs)
- cognitive-systems-synthesis.md
- agentic-workflows.md
- agent-protocols.md

**architecture/** (6 docs)
- actor-model-research.md
- alternative-approaches.md
- formula-approaches.md
- openclaw-model.md

**patterns/** (5 docs)
- activity-worktree-system.md
- subagent-protocols.md
- bmad-formulas.md
- actor-model-ugs.md
- ssr-hybrid-patterns.md

**decisions/** (4 docs)
- wit-platform-migration.md
- session-integration.md

**analysis/** (5 docs)
- bmad-approach.md
- dev-team-formula.md
- spec-kit.md
- early-discoveries.md

**implementation/** (3 docs)
- graph-actors.md

**security/** (2 docs)
- ugs-findings.md
- patterns.md

**reviews/** (2 docs)
- ugs-design-review.md

**retrospectives/** (1 doc)
- ugs-phase-3-retro.md

**web/** (2 docs)
- browser-lifecycle.md
- ssr-hybrid-patterns.md

**databases/** (1 doc)
- graph-query-patterns.md

**testing/** (1 doc)
- harness-reusability.md

---

## Appendix C: Beads Definition

*(Beads will be created separately using `bd create`)*

**Phase 1: cleanup-phase1-knowledge-extraction**
- Priority: P0
- Estimate: 6-8 hours
- Dependencies: None
- Deliverable: 20 documents in ~/knowledge

**Phase 2: cleanup-phase2-reorganize-docs**
- Priority: P0
- Estimate: 4-6 hours
- Dependencies: Phase 1
- Deliverable: Organized docs/ structure

**Phase 3: cleanup-phase3-archive-intermediate**
- Priority: P1
- Estimate: 4-5 hours
- Dependencies: Phase 2
- Deliverable: 120 files archived

**Phase 4: cleanup-phase4-delete-redundant**
- Priority: P2
- Estimate: 2-3 hours
- Dependencies: Phase 3
- Deliverable: Clean repository

---

## Next Steps

1. **Review this analysis** - Verify categorizations and proposals
2. **Answer decision points** - Clarify knowledge boundary, deletion policy
3. **Approve bead creation** - Create implementation beads
4. **Execute Phase 1** - Begin knowledge extraction (safe, reversible)

**Ready to proceed?** This plan provides comprehensive cleanup while preserving all valuable content through git history and knowledge extraction.
