# Documentation Cleanup - Quick Start

**Status**: Ready to execute
**Created**: 2026-02-06
**Effort**: 19 hours across 4 phases

---

## What This Does

Comprehensive cleanup and organization of 275+ documentation files across agentic-primer and simplify repositories:

1. **Extract knowledge** → 20 high-value docs to ~/knowledge
2. **Reorganize** → Create logical docs/ structure
3. **Archive** → 120 intermediate work products
4. **Delete** → 30 redundant/obsolete files

**Result**: Clean, navigable documentation with preserved knowledge and history.

---

## Three Documents

### 1. CLEANUP_ANALYSIS.md (Comprehensive)
**Full detailed analysis**:
- File inventory (every .md file categorized)
- Knowledge extraction strategy
- Reorganization plan with before/after
- Implementation phases with steps
- Decision points requiring user input

**Use for**: Understanding full scope, reviewing decisions, implementation details

### 2. CLEANUP_BEADS_SUMMARY.md (Implementation)
**Bead execution guide**:
- 5 beads created (1 epic + 4 phases)
- Detailed deliverables per phase
- Success criteria
- Recovery procedures
- Timeline and dependencies

**Use for**: Executing cleanup, tracking progress, verifying completion

### 3. This Document (Quick Start)
**Immediate action guide**:
- What to do now
- Command reference
- Decision checklist

---

## What to Do Now

### Step 1: Review the Plan (5 minutes)

**Open and skim**:
```bash
# Read executive summary
head -100 CLEANUP_ANALYSIS.md

# Check bead status
bd list --label cleanup --pretty
```

**Key sections to review in CLEANUP_ANALYSIS.md**:
- Executive Summary (page 1)
- Section 2: Knowledge Capture Strategy
- Section 3: Reorganization Plan
- Section 7: Decision Points (4 questions)

### Step 2: Answer Decision Points (Optional, 10 minutes)

**From CLEANUP_ANALYSIS.md Section 7**:

**Question 1: Knowledge vs Documentation Boundary**
- Proposal: ~/knowledge = research, decisions, patterns
- Alternative: Keep some decisions in-project
- Decision: _______________

**Question 2: Deletion Policy**
- Proposal: Delete 30 redundant files (git-recoverable)
- Alternative: Archive everything, delete nothing
- Decision: _______________

**Question 3: Security Docs**
- Proposal: Keep 1 in root, archive 17 to docs-archive/security/
- Alternative: Keep all visible in root
- Decision: _______________

**Question 4: Experiments Retention**
- Proposal: Archive runs older than 60 days
- Alternative: 90 days? Keep all?
- Decision: _______________

**Default**: If you don't answer, we'll proceed with proposals (all reasonable defaults).

### Step 3: Execute Phase 1 (7 hours)

**Phase 1 is completely safe** (no git changes, only copies to ~/knowledge):

```bash
# Show Phase 1 bead
bd view simplify-06a

# Start execution
bd start simplify-06a

# Create knowledge structure
mkdir -p ~/knowledge/{ai,architecture,patterns,decisions,analysis,implementation,security,reviews,retrospectives,web,databases,testing}

# Extract documents (see CLEANUP_BEADS_SUMMARY.md for full list)
# Example:
cp /Users/bln/play/agentic-primer/COGNITIVE-SYSTEMS-SYNTHESIS.md ~/knowledge/ai/cognitive-systems-synthesis.md

# ... (extract remaining 19 documents)

# Mark complete
bd close simplify-06a --notes "Extracted 20 documents to ~/knowledge"
```

**Validation**:
```bash
# Verify structure
ls ~/knowledge/

# Count extracted docs
find ~/knowledge -name "*.md" | wc -l
# Should show: 20 (or more if you had existing docs)

# No changes to repos
cd /Users/bln/play/agentic-primer && git status  # Clean
cd /Users/bln/play/agentic-primer/simplify && git status  # Clean
```

### Step 4: Continue to Phase 2 (After Phase 1 validation)

```bash
bd view simplify-b9x
bd start simplify-b9x
# See CLEANUP_BEADS_SUMMARY.md for detailed steps
```

---

## Command Reference

### Beads

```bash
# List all cleanup beads
bd list --label cleanup --pretty

# View specific phase
bd view simplify-06a   # Phase 1
bd view simplify-b9x   # Phase 2
bd view simplify-zkg   # Phase 3
bd view simplify-ggt   # Phase 4
bd view simplify-ft0   # Epic (overview)

# Start/close phase
bd start <bead-id>
bd close <bead-id> --notes "Completion notes"

# Check dependencies
bd view simplify-ft0   # Shows all dependencies
```

### Git Safety

```bash
# Before Phase 4 (deletions)
git checkout -b pre-cleanup-snapshot
git checkout main

# Recover deleted file
git checkout pre-cleanup-snapshot -- path/to/file.md

# Undo entire phase
git log
git reset --hard <commit-before-phase>

# Check history of moved file
git log --follow -- new/path/file.md
```

### Navigation

```bash
# View analysis
cat CLEANUP_ANALYSIS.md | less

# View implementation guide
cat CLEANUP_BEADS_SUMMARY.md | less

# Check current state
find . -maxdepth 1 -name "*.md" | wc -l  # Count root files
```

---

## Phase Summary

### Phase 1: Knowledge Extraction (7h)
- **Safe**: No git changes
- **Action**: Copy 20 docs to ~/knowledge
- **Validate**: Check ~/knowledge structure, count files

### Phase 2: Reorganize Docs (5h)
- **Risk**: Low (git-tracked moves)
- **Action**: Create docs/ structure, move 20 files
- **Commit**: "docs: reorganize permanent documentation"
- **Validate**: Check docs/ structure, verify moves

### Phase 3: Archive Intermediate (4.5h)
- **Risk**: Low (archiving, not deleting)
- **Action**: Move 120 files to archives
- **Commit**: "docs: archive intermediate work products"
- **Validate**: Check archive organization, count files

### Phase 4: Delete Redundant (2.5h)
- **Risk**: Medium (deletions, but git-recoverable)
- **Action**: Delete 30 files, prune experiments/
- **Commit**: "docs: remove redundant and obsolete files"
- **Safety**: Create pre-cleanup-snapshot branch first
- **Validate**: Verify deletions were intentional

---

## Expected Outcomes

### Immediate (After Completion)

**File Count**:
- agentic-primer root: 99 → ~30 files (70% reduction)
- simplify root: 76 → ~20 files (74% reduction)
- Total reduction: 175 → 50 root files

**Organization**:
- ✅ Permanent docs in clear locations
- ✅ Knowledge preserved in ~/knowledge
- ✅ Historical context in organized archives
- ✅ No redundancy or clutter

**Navigation**:
- ✅ README/index files in each docs/ subdirectory
- ✅ Clear separation: permanent vs transient vs archived
- ✅ Faster file discovery

### Long-term

**Maintainability**:
- Easier to find relevant documentation
- Clear patterns for where new docs go
- Reduced cognitive load for contributors

**Knowledge Preservation**:
- Valuable research/decisions captured in ~/knowledge
- Searchable permanent knowledge base
- Historical context retained in archives

**Onboarding**:
- New contributors see clean structure
- README files guide navigation
- Less overwhelming file count

---

## Troubleshooting

### "I can't find a file that was deleted"

```bash
# Search git history
git log --all --full-history -- "**/filename.md"

# Recover it
git checkout <commit-hash> -- path/to/filename.md
```

### "I want to undo a phase"

```bash
# Find commit before phase
git log --oneline

# Reset to that commit
git reset --hard <commit-hash>

# Or recover from snapshot (Phase 4 only)
git checkout pre-cleanup-snapshot
```

### "I need to modify the plan"

Edit CLEANUP_ANALYSIS.md and update relevant phase bead:
```bash
bd update <bead-id> --description "Updated description"
```

### "Phase 1 taking too long"

Phase 1 is extracting 20 large documents. You can:
- Parallelize by extracting multiple docs at once
- Skip lower-priority extractions (see CLEANUP_BEADS_SUMMARY.md for rankings)
- Spread across multiple sessions

---

## Success Indicators

**After Phase 1**:
- ✅ `ls ~/knowledge` shows 12+ category directories
- ✅ `find ~/knowledge -name "*.md" | wc -l` shows 20+ docs
- ✅ `git status` is clean in both repos

**After Phase 2**:
- ✅ `ls agentic-primer/docs` shows 6 subdirectories
- ✅ Root .md count reduced by ~20
- ✅ Git commit created with moved files

**After Phase 3**:
- ✅ Archive directories well-organized
- ✅ Root .md count significantly reduced
- ✅ Archive has navigation (README files)

**After Phase 4**:
- ✅ `pre-cleanup-snapshot` branch exists
- ✅ Root directories clean and minimal
- ✅ All beads marked complete
- ✅ No regrets about deletions

---

## Next Action

**If ready to start**:
```bash
bd start simplify-06a
mkdir -p ~/knowledge/{ai,architecture,patterns,decisions,analysis,implementation,security,reviews,retrospectives,web,databases,testing}
# Begin extraction (see CLEANUP_BEADS_SUMMARY.md for file list)
```

**If need to review first**:
```bash
cat CLEANUP_ANALYSIS.md | less
# Read Section 7: Decision Points
# Decide on any modifications
```

**If have questions**:
- Check CLEANUP_ANALYSIS.md Appendices for detailed inventories
- Review CLEANUP_BEADS_SUMMARY.md for phase-by-phase details
- All decisions have reasonable defaults if you want to proceed immediately

---

## Files Reference

| File | Purpose | Length |
|------|---------|--------|
| CLEANUP_ANALYSIS.md | Comprehensive analysis & plan | ~1200 lines |
| CLEANUP_BEADS_SUMMARY.md | Implementation guide | ~500 lines |
| CLEANUP_QUICK_START.md | This file (quick reference) | ~400 lines |

**Read in order**:
1. This file (5 min) → Understand scope
2. CLEANUP_ANALYSIS.md (20 min) → Review plan
3. CLEANUP_BEADS_SUMMARY.md (10 min) → Check implementation
4. Execute Phase 1 → Begin cleanup

**Good luck!** This cleanup will make navigation significantly easier while preserving all valuable content.
