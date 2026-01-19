# Background Agent Workflow - Navigation Index

**Location:** `actor-worldview/bg-workflow/`
**Last Updated:** 2026-01-18

---

## Quick Navigation

| Document | Purpose | Audience |
|----------|---------|----------|
| **[README.md](./README.md)** | **Complete consolidated documentation** | All users - START HERE |
| **[Archive](./archive/)** | Historical versions with timestamps | Reference only |

---

## What's in README.md?

The consolidated README contains ALL unique content from 8 original documents:

### 1. Core Design
- Problem statement and user requirements
- Before/after workflow comparison
- Session log integration strategy
- CLARIFICATION_NEEDED protocol
- Technical implementation options

**Source:** BG_WORKFLOW_REDESIGN.md (70 KB)

### 2. Implementation Status
- Completed deliverables
- Test results (17/17 passing)
- File inventory
- Integration guide

**Source:** BG_WORKFLOW_IMPLEMENTATION_REPORT.md (14.7 KB)

### 3. Checkpoint-Resume Pattern
- Token efficiency design
- Brief poll + checkpoint + exit
- Resume logic using Task.resume
- Token savings comparison

**Source:** BG_WORKFLOW_CHECKPOINT_RESUME.md (17 KB)

### 4. Success Criteria Framework
- Three-tier model (MUST/SHOULD/MAY)
- Objective measurement with formulas
- Subjective evaluation protocol
- Decision tree (ask vs infer)

**Source:** BG_WORKFLOW_SUCCESS_CRITERIA.md (21 KB)

### 5. Graph Integration
- Agent as graph node design
- Graph relationships and queries
- Migration path (file → graph → actor)
- Actor model alignment

**Source:** BG_WORKFLOW_GRAPH_INTEGRATION.md (11 KB)

### 6. Usage Guide
- User workflow (how to use /bg)
- Developer examples (session context, clarification)
- Plugin maintainer instructions

**Sources:** BG_WORKFLOW_LOCAL.md (7.9 KB), BG_WORKFLOW_DELIVERABLES_INDEX.md (11 KB)

### 7. Q&A and Clarifications
- Time-based vs line-based context extraction
- Agent wait behavior
- Timestamp handling
- Integration with logs-as-actors

**Source:** BG_WORKFLOW_CLARIFICATIONS.md (13 KB)

---

## Archive Contents

All original documents preserved with timestamps for historical reference:

```
archive/
├── BG_WORKFLOW_REDESIGN.20260118.md (45 KB)
├── BG_WORKFLOW_IMPLEMENTATION_REPORT.20260118.md (17 KB)
├── BG_WORKFLOW_DELIVERABLES_INDEX.20260118.md (11 KB)
├── BG_WORKFLOW_LOCAL.20260118.md (7.9 KB)
├── BG_WORKFLOW_CLARIFICATIONS.20260118.md (13 KB)
├── BG_WORKFLOW_CHECKPOINT_RESUME.20260118.md (17 KB)
├── BG_WORKFLOW_SUCCESS_CRITERIA.20260118.md (21 KB)
└── BG_WORKFLOW_GRAPH_INTEGRATION.20260118.md (11 KB)
```

**When to use archive:**
- Need to reference specific historical version
- Compare evolution of design
- Audit trail for decisions

**When to use README:**
- **All other times** - it's the authoritative source

---

## Related Documentation

### Implementation Files
- `src/utils/session-log.ts` - Session context extraction
- `src/utils/session-log.test.ts` - Tests (8/8 passing)
- `src/protocols/clarification-protocol.ts` - Clarification protocol
- `src/protocols/clarification-protocol.test.ts` - Tests (9/9 passing)

### Configuration & Examples
- `docs/bg-skill-update.md` - Skill update guide
- `examples/bg-workflow-demo.ts` - Runnable demo

---

## Consolidation Report

**Date:** 2026-01-18
**Action:** Merged 8 BG_WORKFLOW_*.md files into single authoritative README
**Result:**
- ✅ One clean copy in actor-worldview/bg-workflow/
- ✅ No duplicates in root folder
- ✅ All unique content preserved
- ✅ Archive contains timestamped originals
- ✅ Clear navigation structure

**Rationale:**
User requested "one clean copy in the actor-mindset folder" with "no duplicate or multiple versions". This consolidation provides single source of truth while preserving historical versions in archive.

---

## Quick Start

**If you're new to /bg workflow:**
1. Read [README.md](./README.md) Overview section
2. Review Core Design section for workflow comparison
3. Try examples in Usage Guide section

**If you're implementing:**
1. Read Implementation Status section for current state
2. Review code in `src/utils/` and `src/protocols/`
3. Run tests: `bun test src/**/*.test.ts`

**If you're updating the skill:**
1. Read Usage Guide → For Plugin Maintainers
2. Reference `docs/bg-skill-update.md`
3. Test with simple background task

---

**Next:** See [README.md](./README.md) for complete documentation
