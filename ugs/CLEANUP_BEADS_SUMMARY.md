# Documentation Cleanup - Implementation Beads

**Created**: 2026-02-06
**Analysis**: See CLEANUP_ANALYSIS.md
**Total Effort**: 19 hours (1140 minutes)

---

## Overview

Created 5 beads to implement comprehensive documentation cleanup and organization across agentic-primer and simplify repositories.

## Beads Created

### Epic: simplify-ft0
**Documentation Cleanup & Organization (Epic)**
- **Priority**: P0
- **Estimate**: 1140 minutes (19 hours)
- **Type**: Epic
- **Labels**: cleanup, documentation, organization
- **Dependencies**: All phase beads

**Description**: Complete cleanup and organization of documentation across agentic-primer and simplify repositories. Extract valuable knowledge to ~/knowledge, reorganize permanent docs into logical structure, archive intermediate work products, remove redundancy.

**Impact**: 175 files affected (20 extracted, 20 moved, 120 archived, 30 deleted, ~15 consolidated)

---

### Phase 1: simplify-06a
**Extract knowledge to ~/knowledge**
- **Priority**: P0
- **Estimate**: 420 minutes (7 hours)
- **Type**: Task
- **Labels**: cleanup, knowledge-extraction, phase-1
- **Dependencies**: None

**Description**: Extract 20 high-value documents (research, decisions, patterns) from both agentic-primer and simplify repos to permanent knowledge base at ~/knowledge.

**Categories**: ai, architecture, patterns, decisions, analysis, security, reviews

**Safety**: No git changes in repos (external copy operation only)

**Deliverables**:
- ~/knowledge/{ai,architecture,patterns,decisions,analysis,implementation,security,reviews,retrospectives,web,databases,testing}/
- 20 knowledge documents extracted and organized

**Key Files**:
- From agentic-primer (15):
  - COGNITIVE-SYSTEMS-SYNTHESIS.md (1933 lines) → ~/knowledge/ai/
  - WIT_PLATFORM_MIGRATION_PLAN.md (1864 lines) → ~/knowledge/decisions/
  - DEV_TEAM_FORMULA_ANALYSIS.md (1031 lines) → ~/knowledge/analysis/
  - ACTOR_MODEL_RESEARCH_SUMMARY.md (808 lines) → ~/knowledge/architecture/
  - SESSION_INTEGRATION.md (920 lines) → ~/knowledge/decisions/
  - FORMULA_APPROACH_COMPARISON.md (862 lines) → ~/knowledge/architecture/
  - ALTERNATIVE_ARCHITECTURES.md (814 lines) → ~/knowledge/architecture/
  - HARNESS_REUSABILITY_ANALYSIS.md (829 lines) → ~/knowledge/testing/
  - BMAD_FORMULA_EXPRESSIONS.md (781 lines) → ~/knowledge/patterns/
  - DISCOVERY_REPORT.md → ~/knowledge/analysis/
  - (5 more)

- From simplify (5):
  - BROWSER_LIFECYCLE_RESEARCH.md (2116 lines) → ~/knowledge/web/
  - SSR_HYBRID_PATTERNS.md (1979 lines) → ~/knowledge/web/
  - GRAPH_QUERY_RESEARCH.md (1671 lines) → ~/knowledge/databases/
  - OPENCLAW_ARCHITECTURE_MODEL.md (1296 lines) → ~/knowledge/architecture/
  - STATUS_RETROSPECTIVE.md (1084 lines) → ~/knowledge/retrospectives/

---

### Phase 2: simplify-b9x
**Reorganize permanent documentation**
- **Priority**: P0
- **Estimate**: 300 minutes (5 hours)
- **Type**: Task
- **Labels**: cleanup, reorganization, phase-2
- **Dependencies**: simplify-06a

**Description**: Create organized docs/ structure in agentic-primer (bootstrap/, activity/, protocols/, simulation/, formula/, spec-kit/). Move 20 permanent documentation files from root to new structure. Create index/README files.

**Git Commit**: "docs: reorganize permanent documentation"

**Deliverables**:
- agentic-primer/docs/{bootstrap,activity,protocols,simulation,formula,spec-kit}/
- 20 files moved to logical locations
- 6 index/README files created

**Structure Created**:
```
agentic-primer/docs/
├── bootstrap/              (3 files)
│   ├── BOOTSTRAP.md
│   ├── SUCCESS_CRITERIA.md
│   └── AFTER_CLEAR.md
├── activity/               (4 files)
│   ├── SYSTEM.md
│   ├── QUICK_REFERENCE.md
│   └── DIAGRAM.md
├── protocols/              (8 files)
│   ├── SUBAGENT.md
│   ├── QUICK_REF.md
│   ├── IMPLEMENTATION.md
│   └── CLAUDE_ACTIVITY.md
├── simulation/             (5 files)
│   ├── HARNESS.md
│   ├── RUN.md
│   └── ALGORITHM.md
├── formula/                (consolidate 32 → keep 6 + README)
│   └── README.md (index)
└── spec-kit/               (consolidate 20 → keep 4 + README)
    └── README.md (index)
```

---

### Phase 3: simplify-zkg
**Archive intermediate work products**
- **Priority**: P1
- **Estimate**: 270 minutes (4.5 hours)
- **Type**: Task
- **Labels**: cleanup, archiving, phase-3
- **Dependencies**: simplify-b9x

**Description**: Archive 120 intermediate files from both repos.

**Git Commit**: "docs: archive intermediate work products"

**Deliverables**:
- 120 files moved to archives
- Archive organization by date and category
- Index files created for navigability

**agentic-primer Actions (75 files)**:
- Move completion/status reports to archive/reports/ (18 files)
- Move analysis files to archive/analysis/ (10 files)
- Date-organize existing archive/ (archive/2025-12/, archive/2026-01/)
- Consolidate formula files (keep 6 in docs/, move 27 to archive/)

**simplify Actions (45 files)**:
- Move completion reports to docs/archive/reports/ (23 files)
  - All COMPLETION_REPORT_*.md
  - All STREAM_*_COMPLETION_REPORT.md
  - Completion summaries
- Move phase plans to docs/archive/plans/ (4 files)
  - PHASE_2_PLAN.md
  - PHASE_3_PLAN.md
  - PHASE_5_7_IMPLEMENTATION_PLAN.md
  - BATCH_CLASSIFICATION_PLAN.md
- Move session reports to docs-archive/sessions/ (5 files)
  - STATUS_RETROSPECTIVE.md (after knowledge extraction)
  - SESSION_SUMMARY.md
  - Implementation summaries
- Consolidate security docs (18 → keep 1 + archive 17)
  - Keep: SECURITY_BEST_PRACTICES.md (root)
  - Archive: docs-archive/security/ (17 files)
  - Create: docs-archive/security/README.md
- Move research docs to docs-archive/research/ (5 files after extraction)

---

### Phase 4: simplify-ggt
**Delete redundant and obsolete files**
- **Priority**: P2
- **Estimate**: 150 minutes (2.5 hours)
- **Type**: Task
- **Labels**: cleanup, deletion, phase-4
- **Dependencies**: simplify-zkg

**Description**: Delete 30 git-recoverable redundant files. Create pre-cleanup-snapshot branch first for safety.

**Git Commit**: "docs: remove redundant and obsolete files"

**Safety**:
- Create pre-cleanup-snapshot branch before deletions
- All operations git-recoverable
- Document recovery procedures

**Deliverables**:
- 30 files deleted
- Experiments/ pruned (390+ → ~100 files)
- Recovery documentation

**agentic-primer Deletions (15 files)**:
- Index files (10): *_INDEX.md (redundant, generated content)
  - BMAD_ANALYSIS_INDEX.md
  - FORMULA_ARTIFACT_INDEX.md
  - SPEC_KIT_ANALYSIS_INDEX.md
  - (7 more)
- Superseded files (5):
  - EXECUTION_LOG_V1.md (duplicate in archive/)
  - Duplicate summaries

**simplify Deletions (15 files)**:
- Resolved/completed (3):
  - TEST_ISSUES.md
  - NEXT_STEPS.md
  - QUALITY_REVIEW.md
- Duplicate test summaries (8)
- Uncommitted work file (1):
  - PROTOCOL_INTEGRATION_PROMPT.md

**Experiments Pruning**:
- Archive runs older than 60 days
- Move to experiments/archive/2025-*
- Reduce from 390+ to ~100 files

**Recovery Procedures**:
```bash
# Recover deleted file
git checkout pre-cleanup-snapshot -- path/to/file.md

# Undo phase
git checkout pre-cleanup-snapshot
```

---

## Execution Order

```
simplify-ft0 (Epic)
    ├─> simplify-06a (Phase 1: Extract knowledge)
    │       └─> simplify-b9x (Phase 2: Reorganize docs)
    │               └─> simplify-zkg (Phase 3: Archive intermediate)
    │                       └─> simplify-ggt (Phase 4: Delete redundant)
```

## Timeline

**Total**: 19 hours (1140 minutes)

| Phase | Duration | Cumulative | Risk | Git Changes |
|-------|----------|------------|------|-------------|
| 1. Knowledge Extraction | 7h | 7h | Low | None |
| 2. Reorganize Docs | 5h | 12h | Low | Moves only |
| 3. Archive Intermediate | 4.5h | 16.5h | Low | Moves only |
| 4. Delete Redundant | 2.5h | 19h | Medium | Deletions |

## Success Criteria

### After Phase 1:
- ✅ 20 documents in ~/knowledge with proper categorization
- ✅ No changes to git repositories
- ✅ Knowledge extractable and searchable

### After Phase 2:
- ✅ docs/ structure created in agentic-primer
- ✅ 20 files moved to logical locations
- ✅ Index/README files provide navigation
- ✅ Root directory cleaner (99 → 79 files)

### After Phase 3:
- ✅ 120 files archived
- ✅ Archive organized by date/category
- ✅ Root directories significantly cleaner
  - agentic-primer: 79 → 30 files
  - simplify: 76 → 20 files

### After Phase 4:
- ✅ 30 redundant files removed
- ✅ Experiments/ pruned to recent runs
- ✅ Recovery branch created
- ✅ Recovery procedures documented
- ✅ Final state:
  - agentic-primer root: ~30 permanent docs
  - simplify root: ~20 permanent docs
  - Organized archives with navigation

## Benefits

**Immediate**:
- Faster file discovery (less clutter)
- Clear separation: permanent vs transient
- Better navigation (organized structure)
- Reduced cognitive load

**Long-term**:
- Knowledge preservation in ~/knowledge
- Historical context in archives
- Consistent documentation patterns
- Easier onboarding for new contributors

**Metrics**:
- Root files reduced: 175 → 50 (71% reduction)
- Knowledge captured: 20 permanent documents
- Archives organized: 120 files with navigation
- Redundancy eliminated: 30 files

## Recovery & Safety

**Git Protection**:
- All phases use git commits
- Phase 4 creates pre-cleanup-snapshot branch
- All deletions are git-recoverable

**Recovery Commands**:
```bash
# Recover specific file
git checkout <commit-hash>~1 -- path/to/file.md

# Undo entire phase
git log  # Find commit before phase
git reset --hard <commit-hash>

# Recover from snapshot branch
git checkout pre-cleanup-snapshot -- path/to/file.md
```

**Validation**:
- After each phase: verify expected state
- Before Phase 4: review deletion list
- After Phase 4: confirm no unintended deletions

## Next Steps

1. **Review CLEANUP_ANALYSIS.md** - Full detailed plan
2. **Answer decision points** (in CLEANUP_ANALYSIS.md Section 7):
   - Knowledge vs documentation boundary
   - Deletion policy comfort level
   - Security docs consolidation
   - Experiments retention policy
3. **Execute Phase 1** - Safe, no git changes
   ```bash
   bd ready --label phase-1  # Show Phase 1 bead
   bd start simplify-06a     # Begin execution
   ```
4. **Validate** - Check ~/knowledge structure
5. **Continue** - Move to Phase 2 after validation

## References

- **Full Analysis**: CLEANUP_ANALYSIS.md
- **Epic Bead**: simplify-ft0
- **Phase Beads**: simplify-06a, simplify-b9x, simplify-zkg, simplify-ggt
- **Bead Query**: `bd list --label cleanup --pretty`
