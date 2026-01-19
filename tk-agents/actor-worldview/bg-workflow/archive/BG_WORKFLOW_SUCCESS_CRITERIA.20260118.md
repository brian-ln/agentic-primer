# Background Workflow Success Criteria - Objective Measurement

## Problem Statement

Current BG_WORKFLOW_REDESIGN.md says agents should use CLARIFICATION_NEEDED when "success criteria undefined" - but this is too vague. We need:

1. **Objective criteria**: Measurable, verifiable, unambiguous
2. **Subjective evaluation protocol**: When objective criteria insufficient
3. **Clear decision tree**: When to ask vs when to infer

**User feedback:**
> "Success criteria need to be objectively defined and measurable at a sufficient level of detail to meet the goal and deliver the deliverables. Objectively defined and we can have subjective things too. The evaluation of objective compliance is needed, but the next best action needs some subjectivity at times using a well defined protocol."

---

## Objective Success Criteria Framework

### What Makes Criteria Objective?

**Objective criteria are:**
- ✅ **Measurable**: Can be quantified or verified
- ✅ **Unambiguous**: No interpretation needed
- ✅ **Observable**: Can be checked by inspection
- ✅ **Binary, scaled, or unknown**: Pass/fail, numeric score with defined formula, or "I don't know"

**CRITICAL: Numeric scores MUST have defined formulas**
- ❌ NO "thumb in the wind" numbers
- ✅ YES explicit calculation method
- ✅ "I don't know" is a valid answer (tertiary: pass/fail/unknown)
- ✅ Uncertainty must be quantified if scoring (e.g., "85% ± 10%")

**Examples:**

| Task | ❌ Subjective (Vague) | ✅ Objective (Measurable with Formula) |
|------|----------------------|--------------------------------------|
| Code quality | "Write good code" | **Formula**: `score = (1 - errors/LOC) × (1 - warnings/LOC) × coverage` <br> **Threshold**: score ≥ 0.80 <br> **Unknown**: "I don't know - no tests exist yet" |
| Documentation | "Document the API" | **Formula**: `coverage = documented_funcs / total_public_funcs` <br> **Threshold**: coverage = 1.0 (100%) <br> **Unknown**: "I don't know - public API not yet defined" |
| Performance | "Make it fast" | **Formula**: Measure p50, p95, p99 via benchmarks <br> **Threshold**: p95 < 100ms AND p50 < 50ms <br> **Uncertainty**: "85ms ± 15ms (10 runs)" |
| Research | "Find relevant info" | **Formula**: `score = (academic_sources/3) × (recent_sources/5) × 100` <br> **Threshold**: score ≥ 100% (meets minimums) <br> **Unknown**: "I don't know - topic has no recent research" |
| Design | "Create a design" | **Checklist**: [arch diagram, sequence diagram, data model, decisions] <br> **Formula**: `completeness = items_present / items_required` <br> **Threshold**: completeness = 1.0 |

---

## Three-Tier Success Criteria Model

### Tier 1: MUST (Objective, Hard Requirements)

**These MUST be met for success:**
- Deliverables exist and are complete
- Tests pass (if code)
- Compilation succeeds (if applicable)
- Format matches specification
- All required sections present

**Measurement:**
```typescript
interface MustCriteria {
  deliverables: {
    required: string[];          // ["README.md", "src/impl.ts"]
    actual: string[];            // What was created
    allPresent: boolean;         // required ⊆ actual
  };

  tests: {
    totalTests: number;
    passing: number;
    failing: number;
    coverage: number;            // 0-100%
    passingRate: number;         // passing/total
    meetsThreshold: boolean;     // passingRate >= 1.0
  };

  compilation: {
    errors: number;
    warnings: number;
    compilable: boolean;         // errors === 0
  };

  format: {
    requiredSections: string[];  // ["Overview", "Usage", "Examples"]
    actualSections: string[];
    allPresent: boolean;
  };
}

// Success = all meetsThreshold/allPresent/compilable = true
```

**Agent can determine objectively:**
- Do files exist? ✓
- Do tests pass? ✓
- Does code compile? ✓
- Are sections present? ✓

**No clarification needed** - these are mechanically verifiable.

---

### Tier 2: SHOULD (Objective, Soft Requirements)

**These SHOULD be met, but may require trade-offs:**
- Performance targets (with acceptable ranges)
- Code coverage (with minimum threshold)
- Documentation completeness (with checklist)
- Design quality (with rubric)

**Measurement:**
```typescript
interface ShouldCriteria {
  performance: {
    target: number;              // Target value
    actual: number;              // Measured value
    acceptable: [number, number]; // [min, max] acceptable range
    withinRange: boolean;
  };

  coverage: {
    target: number;              // 80%
    actual: number;              // Measured
    minimum: number;             // 70% (acceptable)
    meetsMinimum: boolean;
  };

  documentation: {
    checklist: ChecklistItem[];
    completed: number;
    total: number;
    completionRate: number;      // completed/total
    meetsTarget: boolean;        // completionRate >= 0.8
  };

  designQuality: {
    rubric: RubricItem[];        // Each scored 0-5
    scores: number[];
    averageScore: number;
    meetsTarget: boolean;        // averageScore >= 4.0
  };
}
```

**Agent can measure objectively:**
- Run benchmarks for performance ✓
- Calculate code coverage ✓
- Check documentation checklist ✓
- Score design against rubric ✓

**Clarification protocol:**
If `meetsMinimum = false` but close, agent can:

```yaml
[CLARIFICATION_NEEDED]
reason: "Performance target missed but within acceptable range"

questions:
  - question_id: Q1
    text: "Performance is 110ms (target: 100ms, acceptable: 50-150ms). Accept?"
    context: "Within acceptable range but missed target. Trade-off: optimization would delay delivery by 2 days"
    type: optional
    current_assumption: "Accept 110ms as sufficient (within range)"
```

---

### Tier 3: MAY (Subjective, Judgment Calls)

**These require human judgment:**
- Design elegance
- User experience
- Naming conventions
- Code style (beyond linting)
- Architecture philosophy (actor model vs other)

**Evaluation Protocol:**

```typescript
interface SubjectiveEvaluation {
  criterion: string;
  context: string;              // What was done
  options: EvaluationOption[];  // Possible judgments
  agentRecommendation: string;  // What agent thinks
  rationale: string;            // Why agent thinks this
  needsHumanJudgment: boolean;  // If truly subjective
}

interface EvaluationOption {
  label: string;
  description: string;
  tradeoffs: string[];          // What you gain/lose
  objectiveSupport: any[];      // Objective data supporting this
}
```

**When to clarify:**

**✅ CLARIFY when:**
- Fundamental design philosophy choice (actor model vs traditional)
- User experience trade-offs with no clear winner
- Multiple valid approaches with different trade-offs
- Architectural decision affecting future work

**❌ DON'T CLARIFY when:**
- Minor naming choices (use project conventions)
- Code style within project norms (follow existing patterns)
- Implementation details (agent picks reasonable approach)
- Optimization choices (agent uses best practices)

**Subjective evaluation template:**

```yaml
[SUBJECTIVE_EVALUATION]
agent_id: task_bg_a7e4d2
timestamp: 2026-01-18T10:15:23Z
criterion: "Actor model vs traditional HTTP client"

context: |
  Implementing CozoDB client. Two approaches:

  Option A: Actor-based (primer.graph.cozodb actor)
  - Fits actor worldview
  - More abstraction
  - 300 LOC

  Option B: Traditional HTTP client
  - Direct, familiar pattern
  - Less abstraction
  - 150 LOC

options:
  - label: "Actor-based (Recommended)"
    description: "primer.graph.cozodb actor with message passing"
    tradeoffs:
      - "Pro: Aligns with actor worldview"
      - "Pro: Location transparency"
      - "Pro: Supervision/restart capabilities"
      - "Con: More abstraction layers"
      - "Con: 2x LOC vs traditional"
    objectiveSupport:
      - "Existing actor infrastructure in place"
      - "Follows ACTOR_WORLDVIEW_ANALYSIS_V2.md design principles"
      - "Consistent with primer.tasks, primer.knowledge patterns"

  - label: "Traditional HTTP client"
    description: "Direct HTTP client with connection pooling"
    tradeoffs:
      - "Pro: Simple, direct, familiar"
      - "Pro: 50% less code"
      - "Pro: Easier debugging"
      - "Con: Doesn't fit actor model"
      - "Con: Hard-coded to HTTP (no location transparency)"
    objectiveSupport:
      - "Existing HTTP patterns in daemon code"
      - "Node.js fetch API well-documented"

agentRecommendation: "Actor-based (Option A)"
rationale: |
  Project is building actor worldview. Even though traditional approach is simpler,
  actor-based aligns with architectural direction. LOC increase (150 → 300) is
  acceptable trade-off for architectural consistency.

needsHumanJudgment: true
reason: "Fundamental architectural philosophy choice affecting future work"

continueWithAssumption: true
parallelWork: |
  Can implement data model and query logic (independent of client choice).
  Will need supervision pattern answer before finalizing actor implementation.
[/SUBJECTIVE_EVALUATION]
```

**Parent response:**

```yaml
[SUBJECTIVE_EVALUATION_RESPONSE]
agent_id: task_bg_a7e4d2
timestamp: 2026-01-18T10:18:15Z

decision: "Actor-based (Option A)"
reasoning: "Correct - we're committed to actor worldview. LOC increase acceptable."
additional_guidance: "Use lightweight supervision (restart on failure only)"

resume_signal: true
[/SUBJECTIVE_EVALUATION_RESPONSE]
```

---

## Decision Tree: Ask vs Infer

### When Agent Should ASK (Clarification Required)

```
                    [Agent encounters decision point]
                                |
                                |
            [Is this objectively measurable?] ───YES──> [Measure it]
                                |                           |
                               NO                           |
                                |                      [Meets MUST?] ──YES──> Continue
                                |                           |
                                |                          NO
                                |                           |
                    [Is there a clear project pattern?] ────> FAIL (fix and retry)
                                |
                        NO ─────┴───── YES
                        |               |
                        |          [Follow pattern]
                        |               |
            [Affects fundamental design?] ───NO──> [Agent chooses reasonable default]
                        |
                       YES
                        |
              [Use SUBJECTIVE_EVALUATION]
                        |
              [Present options with tradeoffs]
                        |
              [Continue parallel work if possible]
```

### When Agent Should INFER (No Clarification Needed)

**Agent infers when:**

1. **Project conventions exist:**
   - File naming: `<topic>_RESEARCH.md` pattern found
   - Code style: Existing files use specific patterns
   - Test structure: Existing tests show approach
   - Documentation: Existing docs show format

2. **Industry best practices apply:**
   - TypeScript: Use strict mode
   - Testing: Arrange-Act-Assert pattern
   - Error handling: Try-catch with specific errors
   - Async: Prefer async/await over callbacks

3. **Technical correctness is clear:**
   - Type safety: Use TypeScript types
   - Security: Sanitize inputs, no SQL injection
   - Performance: O(n) better than O(n²)
   - Reliability: Handle errors, retry on transient failures

4. **Minor implementation details:**
   - Variable names (follow conventions)
   - Internal function structure
   - Optimization choices (use reasonable defaults)
   - Temporary file paths

**Example (Agent infers correctly):**

```
Task: "Research Datalog optimization patterns"

Agent observes:
- Existing research docs use format: <TOPIC>_RESEARCH.md
- Include sections: Overview, Patterns, Examples, References
- Code examples in examples/ directory

Agent infers:
✓ Create DATALOG_OPTIMIZATION_RESEARCH.md
✓ Use standard sections (Overview, Patterns, Examples, References)
✓ Put code examples in examples/datalog-patterns/
✓ Include 5+ patterns (typical count from other research docs)

Agent does NOT clarify:
- Exact pattern count (uses 5+ as reasonable)
- Section ordering (uses standard template)
- Example format (follows existing examples)
```

---

## Objective Compliance Evaluation

### Automated Checks

```typescript
interface ComplianceCheck {
  category: "MUST" | "SHOULD" | "MAY";
  criterion: string;
  expected: any;
  actual: any;
  passed: boolean;
  evidence: string[];           // What was measured
  autoVerifiable: boolean;      // Can machine verify?
}

// Example compliance report
const compliance: ComplianceCheck[] = [
  {
    category: "MUST",
    criterion: "All deliverables present",
    expected: ["README.md", "src/impl.ts", "tests/impl.test.ts"],
    actual: ["README.md", "src/impl.ts", "tests/impl.test.ts"],
    passed: true,
    evidence: ["ls check passed"],
    autoVerifiable: true
  },
  {
    category: "MUST",
    criterion: "Tests pass",
    expected: { passing: "100%", failing: 0 },
    actual: { passing: "100%", failing: 0 },
    passed: true,
    evidence: ["bun test output: 23 pass, 0 fail"],
    autoVerifiable: true
  },
  {
    category: "SHOULD",
    criterion: "Code coverage >80%",
    expected: { minimum: 80, target: 90 },
    actual: { coverage: 85 },
    passed: true,
    evidence: ["bun test --coverage: 85% line coverage"],
    autoVerifiable: true
  },
  {
    category: "MAY",
    criterion: "Actor model design philosophy",
    expected: "Follow actor worldview where appropriate",
    actual: "Implemented using actor model",
    passed: true,
    evidence: ["primer.graph.cozodb actor created", "Message passing interface", "Supervision pattern applied"],
    autoVerifiable: false  // Requires human judgment
  }
];

// Objective compliance score
const mustPassed = compliance.filter(c => c.category === "MUST" && c.passed).length;
const mustTotal = compliance.filter(c => c.category === "MUST").length;
const mustCompliance = mustPassed / mustTotal; // Must be 1.0

const shouldPassed = compliance.filter(c => c.category === "SHOULD" && c.passed).length;
const shouldTotal = compliance.filter(c => c.category === "SHOULD").length;
const shouldCompliance = shouldPassed / shouldTotal; // Target: 0.8+

// Success = mustCompliance === 1.0 && shouldCompliance >= 0.8
```

### COMPLETION_REPORT Enhancement

**Updated format with objective measurements:**

```yaml
[COMPLETION_REPORT]
agent_id: task_bg_a7e4d2
timestamp: 2026-01-18T10:45:32Z
status: success

deliverables:
  - path: DATALOG_OPTIMIZATION_RESEARCH.md
    size: 45KB
    sections: ["Overview", "Patterns (12)", "Examples (8)", "References (15)"]
  - path: examples/datalog-patterns/
    files: 8
    total_size: 12KB

# OBJECTIVE COMPLIANCE (Tier 1: MUST)
objective_compliance:
  must_criteria:
    - criterion: "Deliverables present"
      expected: ["DATALOG_OPTIMIZATION_RESEARCH.md", "examples/"]
      actual: ["DATALOG_OPTIMIZATION_RESEARCH.md", "examples/"]
      passed: true

    - criterion: "Required sections present"
      expected: ["Overview", "Patterns", "Examples", "References"]
      actual: ["Overview", "Patterns (12)", "Examples (8)", "References (15)"]
      passed: true

    - criterion: "Minimum pattern count"
      expected: 5
      actual: 12
      passed: true
      evidence: "Project research docs average 8-12 patterns"

  must_compliance: 1.0  # 3/3 passed

  should_criteria:
    - criterion: "Academic sources"
      expected: 3
      actual: 8
      passed: true

    - criterion: "Recent sources (within 2 years)"
      expected: 5
      actual: 10
      passed: true

    - criterion: "Code examples executable"
      expected: true
      actual: true
      passed: true
      evidence: "All 8 examples run successfully"

  should_compliance: 1.0  # 3/3 passed

# SUBJECTIVE EVALUATIONS (Tier 3: MAY)
subjective_evaluations:
  - criterion: "Pattern explanation clarity"
    agentAssessment: "Clear and well-structured"
    evidence: ["Each pattern has: description, use case, code example, trade-offs"]
    needsHumanReview: false

  - criterion: "Example complexity"
    agentAssessment: "Appropriate - ranges from basic to advanced"
    evidence: ["Examples 1-3: basic", "Examples 4-6: intermediate", "Examples 7-8: advanced"]
    needsHumanReview: false

# INFERENCES MADE (What agent assumed)
inferences:
  - assumption: "Use standard research doc format"
    basis: "PROJECT_PATTERN"
    evidence: "5 existing research docs use this format"

  - assumption: "12 patterns sufficient (target: 5+)"
    basis: "PROJECT_PATTERN"
    evidence: "Average research doc has 8-12 patterns"

  - assumption: "Examples in examples/ directory"
    basis: "PROJECT_CONVENTION"
    evidence: "All example code in examples/ subdirectories"

# CLARIFICATIONS (What was asked)
clarifications: 0
clarification_history: []

# SUMMARY
summary: |
  Researched Datalog query optimization patterns for graph traversal.
  Found 12 optimization patterns with 8 executable examples.
  All MUST criteria met (100%). All SHOULD criteria met (100%).
  Used project conventions for format and structure.

metrics_achieved:
  - "Deliverables: 100% complete"
  - "Objective compliance: 100% (MUST + SHOULD)"
  - "Clarifications: 0 (full autonomy)"
  - "Pattern count: 12 (target: 5+, 240% of minimum)"
  - "Example coverage: 100% (all patterns have examples)"

total_duration: 42m 18s
[/COMPLETION_REPORT]
```

---

## Integration with BG_WORKFLOW_REDESIGN.md

### Update Section: "When to Use CLARIFICATION_NEEDED"

**REPLACE lines 186-209 with:**

```markdown
#### When to Use

**Use CLARIFICATION_NEEDED when:**

1. **Objective criteria cannot be determined**
   - Success metrics undefined in task description
   - No project pattern to infer from
   - Multiple valid interpretations with different outcomes
   - Example: "Optimize the workflow" without specifying what to optimize (time? cost? UX?)

2. **Subjective judgment required**
   - Fundamental design philosophy choice (affects architecture)
   - Trade-offs with no clear winner (need human priorities)
   - Example: Actor model vs traditional approach for new component

3. **Missing critical information**
   - Technical constraints unclear (which API version? which database?)
   - Dependencies unknown (what libraries can be used?)
   - Integration points undefined (how does this connect to X?)
   - Example: "Implement authentication" without specifying OAuth vs JWT vs other

**Do NOT use CLARIFICATION_NEEDED when:**

1. **Objective criteria are inferrable**
   - Project conventions exist (follow them)
   - Industry best practices apply (use them)
   - Technical correctness is clear (type safety, security, performance)
   - Example: Use `.spec.md` format because 10 other specs use it

2. **Minor implementation details**
   - Variable naming (follow conventions)
   - Internal structure (use best practices)
   - Optimization choices (use reasonable defaults)
   - Example: Don't ask about internal function names

3. **Information discoverable in session logs**
   - Recent decisions about similar work
   - Existing patterns in codebase
   - User preferences from past tasks
   - Example: Don't ask if user prefers TypeScript (check recent work)

4. **Options can be presented in COMPLETION_REPORT**
   - Multiple valid approaches, any is acceptable
   - Cosmetic differences only
   - Example: Don't ask about diagram layout - pick one and deliver
```

### Add New Section After "When to Use"

```markdown
#### Success Criteria Tiers

**Tier 1: MUST (Objective, Hard Requirements)**
- Deliverables exist and complete
- Tests pass (if applicable)
- Code compiles (if applicable)
- Required sections present
- **Measurement**: Mechanically verifiable
- **If not met**: Task fails, must fix

**Tier 2: SHOULD (Objective, Soft Requirements)**
- Performance targets (with acceptable ranges)
- Code coverage (with minimum threshold)
- Documentation completeness
- **Measurement**: Quantifiable with rubric
- **If not met**: Clarify if acceptable trade-off

**Tier 3: MAY (Subjective, Judgment Calls)**
- Design elegance
- Architecture philosophy (actor model vs traditional)
- UX trade-offs
- **Measurement**: Human judgment required
- **If not met**: Use SUBJECTIVE_EVALUATION protocol

See BG_WORKFLOW_SUCCESS_CRITERIA.md for complete framework.
```

---

## Summary

**Key improvements:**

1. **Objective measurement framework**: Three-tier model (MUST/SHOULD/MAY)
2. **Clear decision tree**: When to ask vs when to infer
3. **Subjective evaluation protocol**: How to handle judgment calls
4. **Enhanced COMPLETION_REPORT**: Include objective compliance scores
5. **Inference documentation**: What agent assumed and why

**Result:**
- Agents know when clarification is truly needed
- Success is objectively measurable where possible
- Subjective judgments use well-defined protocol
- Users can verify compliance automatically
- Next best action uses objective + subjective appropriately

**Alignment with actor worldview:**
- Design → Fitness → Optimize
- Fitness function must be objective and measurable
- Optimization may require subjective judgment with protocol
- Validation checks objective compliance
