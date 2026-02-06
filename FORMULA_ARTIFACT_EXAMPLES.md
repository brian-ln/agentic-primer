# Artifact-Aware Formula Examples

**Date:** 2026-01-11
**Purpose:** Demonstrate the value of first-class artifact tracking through concrete examples

---

## Example 1: Debugging Workflow Failures

### Scenario: Validation Step Fails

**Without Artifact Tracking (Task-Centric):**

```
Error: Step 'validation' failed
Agent: @agent-dev-validator
Message: Validation could not complete
Exit code: 1
```

**Developer investigation:**
```bash
# Developer has to manually investigate
cd src/components
ls -la
# Find: MessageRouter.spec.md ✓
# Find: MessageRouter.model.lisp ✓
# Find: MessageRouter.test.js ✓
# Find: MessageRouter.js ✗ (missing!)

# Ah, implementation never got created. Why?
# Check dev-coder logs...
# Dig through agent output...
# Eventually find: dev-coder read wrong spec file
```

**Time to diagnose:** 15-20 minutes

---

**With Artifact Tracking (Artifact-Aware):**

```
Error: Step 'validation' failed - Missing required input artifact

Step: validation
Agent: @agent-dev-validator
Missing Artifacts:
  ✗ MessageRouter.js (implementation)
    Expected path: src/components/MessageRouter.js
    Required: true
    Status: not_found
    Produced by: step 'implementation'

Artifact Status Summary:
  ✓ MessageRouter.spec.md (documentation) - validated
  ✓ MessageRouter.model.lisp (formal-spec) - validated
  ✓ MessageRouter.test.js (test-suite) - validated
  ✗ MessageRouter.js (implementation) - not_found

Root Cause: Step 'implementation' completed but did not produce required artifact.
Recommendation: Check @agent-dev-coder logs for step 'implementation'
```

**Developer investigation:**
```bash
# Immediately go to implementation step logs
# Find: dev-coder read wrong spec file
```

**Time to diagnose:** 2-3 minutes

**Benefit:** 80% reduction in debugging time through precise error messages

---

## Example 2: Quality Gate Enforcement

### Scenario: Test Suite Missing Coverage

**Without Artifact Tracking:**

Developer submits component for review:
```bash
bd mol wisp component-dev --var component_name="CacheManager"
# All steps complete
# Workflow marked as done
```

Reviewer manually checks:
```javascript
// Reads CacheManager.test.js
// Manually compares to CacheManager.spec.md
// Finds: Only 3 of 7 requirements have tests
// Rejects in PR review
```

**Review cycle:** Multiple days, manual process

---

**With Artifact Tracking:**

```toml
[[steps.outputs]]
artifact = "CacheManager.test.js"
validation = [
  "File exists",
  "Every requirement from spec.md has a test",  # ← Automated check
  "NO test skipping"
]
```

Formula executor automatically validates:
```
Step: test-suite
Artifact: CacheManager.test.js
Validation Results:
  ✓ File exists
  ✗ Every requirement from spec.md has a test
    Failed: 4 requirements missing tests
      - Requirement 3.2: Cache eviction policy
      - Requirement 3.4: TTL expiration
      - Requirement 4.1: Concurrent access handling
      - Requirement 5.2: Memory pressure handling
  ✓ NO test skipping

Artifact Status: INVALID
Step Status: FAILED

Cannot proceed to validation - test suite incomplete.
```

**Review cycle:** Immediate feedback, automated enforcement

**Benefit:** Catch quality issues before human review, save days of cycle time

---

## Example 3: Artifact Lineage and Root Cause Analysis

### Scenario: Tests Fail After Spec Update

**Without Artifact Tracking:**

```bash
# Update specification
vim MessageRouter.spec.md
# Add new requirement: "Support message prioritization"

# Re-run workflow
bd mol wisp component-dev --var component_name="MessageRouter"
# Tests fail

bun test MessageRouter.test.js
# FAIL: Unexpected behavior in routing

# Developer investigates:
# - Is it the spec update?
# - Is it the implementation?
# - Is it the test?
# - Which change caused the failure?
```

Manual git archaeology to find root cause.

---

**With Artifact Tracking (Lineage):**

```
Test Execution Failed:
  Artifact: MessageRouter.test.js
  Status: tests_failing (12 of 45 tests fail)

Artifact Lineage:
  MessageRouter.test.js
    ← derived from: MessageRouter.spec.md (modified 10 minutes ago)
    ← derived from: MessageRouter.model.lisp (last modified: 2 days ago)
    ← parallel with: MessageRouter.js (created 5 minutes ago)

Change Analysis:
  MessageRouter.spec.md: MODIFIED
    Added requirement 3.5: "Support message prioritization"
    Added requirement 3.6: "Priority levels: HIGH, NORMAL, LOW"

  MessageRouter.test.js: RECREATED
    Now includes tests for requirements 3.5, 3.6

  MessageRouter.js: RECREATED
    Missing implementation of priority handling

Root Cause: Implementation does not handle priority (new requirement 3.5, 3.6)
Recommendation: Re-run @agent-dev-coder with updated specifications
```

**Benefit:** Automatic root cause analysis through artifact lineage tracking

---

## Example 4: Workflow Composition and Artifact Reuse

### Scenario: Building System with Multiple Components

**Without Artifact Tracking:**

Developer creates 3 components separately:
```bash
# Component 1
bd mol wisp component-dev --var component_name="MessageRouter"
# Creates: MessageRouter.spec.md, .model.lisp, .test.js, .js

# Component 2
bd mol wisp component-dev --var component_name="CacheManager"
# Creates: CacheManager.spec.md, .model.lisp, .test.js, .js

# Component 3
bd mol wisp component-dev --var component_name="Logger"
# Creates: Logger.spec.md, .model.lisp, .test.js, .js

# Now need integration tests
# Manually create integration-tests.formula.toml
# Manually specify which files to use
# Hope paths are correct
# Hope artifacts are complete
```

Manual coordination, error-prone.

---

**With Artifact Tracking (Composition):**

```toml
# system-integration.formula.toml
formula = "system-integration"
description = "Integration tests for messaging system"

[[steps]]
id = "integration-tests"
title = "Test MessageRouter + CacheManager + Logger integration"

# Automatically reference artifacts from other workflows
[[steps.inputs]]
artifact = "MessageRouter.js"
source_formula = "component-dev"
source_vars = { component_name = "MessageRouter" }
type = "implementation"
required = true

[[steps.inputs]]
artifact = "CacheManager.js"
source_formula = "component-dev"
source_vars = { component_name = "CacheManager" }
type = "implementation"
required = true

[[steps.inputs]]
artifact = "Logger.js"
source_formula = "component-dev"
source_vars = { component_name = "Logger" }
type = "implementation"
required = true

[[steps.outputs]]
artifact = "messaging-system-integration.test.js"
type = "test-suite"
description = "Integration tests for complete messaging system"
validation = [
  "All components integrate successfully",
  "Message flow works end-to-end",
  "Cache is used by router",
  "Logger records all events"
]
```

Formula executor automatically:
- Locates artifacts from previous workflows
- Validates artifacts exist and are complete
- Checks artifact compatibility (types match)
- Provides clear errors if artifacts are missing

```bash
bd mol wisp system-integration

# Executor checks:
Resolving input artifacts...
  ✓ MessageRouter.js found at src/components/MessageRouter.js (validated)
  ✓ CacheManager.js found at src/components/CacheManager.js (validated)
  ✓ Logger.js found at src/components/Logger.js (validated)

All inputs available. Proceeding with integration tests...
```

**Benefit:** Composable workflows with automatic artifact resolution and validation

---

## Example 5: Parallel Development Isolation Enforcement

### Scenario: Preventing Cross-Contamination

**Without Artifact Tracking:**

```bash
# Launch parallel development
# @agent-dev-tester creates tests
# @agent-dev-coder creates implementation

# Problem: dev-coder accidentally reads test file
# Implementation is now biased by test structure
# Defeats purpose of isolated parallel development
```

No enforcement mechanism - relies on agent discipline.

---

**With Artifact Tracking (Isolation):**

```toml
[[steps]]
id = "test-suite"
agent = "@agent-dev-tester"
parallel_with = ["implementation"]
isolation = true  # ← Enforces access restrictions

# Permitted inputs
[[steps.inputs]]
artifact = "MessageRouter.spec.md"
required = true

[[steps.inputs]]
artifact = "MessageRouter.model.lisp"
required = true

# Produces output
[[steps.outputs]]
artifact = "MessageRouter.test.js"
required = true
```

Formula executor enforces isolation:
```javascript
class FormulaExecutor {
  async executeStep(step) {
    if (step.isolation) {
      // Create isolated execution environment
      const allowedPaths = step.inputs.map(i => i.path);
      const deniedPaths = this.getParallelStepOutputs(step.parallel_with);

      await this.runAgent(step.agent, step.description, {
        allowedPaths,   // Can only read spec.md and model.lisp
        deniedPaths     // Cannot read implementation.js
      });
    }
  }
}
```

If dev-coder tries to read test file:
```
Access Denied: MessageRouter.test.js

Step: implementation
Agent: @agent-dev-coder
Isolation: true
Denied artifact: MessageRouter.test.js (produced by parallel step 'test-suite')

This step runs in parallel with 'test-suite' and cannot access its outputs.
Permitted inputs:
  ✓ MessageRouter.spec.md
  ✓ MessageRouter.model.lisp
```

**Benefit:** Automatic enforcement of parallel development isolation

---

## Example 6: Incremental Workflow Recovery

### Scenario: Workflow Interrupted, Resume from Checkpoint

**Without Artifact Tracking:**

```bash
# Workflow running
# Phase 1: Complete ✓
# Phase 2: Complete ✓
# Phase 3: In progress... (system crash)

# Restart workflow
# What completed?
# What needs to re-run?
# Manual inspection required
```

Developer manually checks file timestamps, git commits, agent logs.

---

**With Artifact Tracking (Checkpointing):**

```
Workflow State Recovery:
  Formula: component-dev
  Component: MessageRouter
  Last checkpoint: Phase 2 complete

Artifact Status:
  ✓ MessageRouter.spec.md (validated at 10:23:45)
  ✓ MessageRouter.model.lisp (validated at 10:23:45)
  ✓ MessageRouter.test.js (validated at 10:31:12)
  ✓ MessageRouter.js (validated at 10:31:18)
  ✗ MessageRouter-validation-report.md (not found)

Recovery Plan:
  Phase 1 (specifications): SKIP - artifacts validated
  Phase 2 (parallel dev): SKIP - artifacts validated
  Phase 3 (validation): RESUME - artifact missing

Resume from step 'validation'? [y/N]
```

Formula executor automatically:
- Checks which artifacts exist and are validated
- Determines which steps can be skipped
- Resumes from first incomplete step

**Benefit:** Automatic workflow recovery without re-running completed work

---

## Example 7: Multi-Variant Workflow Support

### Scenario: Development vs Production Artifacts

**Without Artifact Tracking:**

```bash
# Create production implementation
bd mol wisp component-dev --var component_name="APIClient"
# Creates: APIClient.js (production version)

# Need test mock version too
# Manually create APIClient.mock.js
# Manually ensure same interface
# Manually keep in sync
```

Manual management of artifact variants.

---

**With Artifact Tracking (Variants):**

```toml
# Production variant
[[steps]]
id = "implementation"
agent = "@agent-dev-coder"

[[steps.outputs]]
artifact = "{{component_name}}.js"
path = "src/{{component_name}}.js"
type = "implementation"
variant = "production"
validation = [
  "Complete implementation",
  "All error handling",
  "Production logging"
]

# Test mock variant (parallel)
[[steps]]
id = "test-mock"
agent = "@agent-dev-coder"
parallel_with = ["implementation"]

[[steps.outputs]]
artifact = "{{component_name}}.js"
path = "test/mocks/{{component_name}}.js"
type = "implementation"
variant = "test-mock"
validation = [
  "Same interface as production variant",
  "Configurable responses",
  "No external dependencies"
]
```

Formula executor manages variants:
```
Artifact Registry:
  APIClient.js (variant: production)
    path: src/APIClient.js
    status: validated

  APIClient.js (variant: test-mock)
    path: test/mocks/APIClient.js
    status: validated

Cross-validation:
  ✓ Both variants implement same interface
  ✓ Test mock has no external dependencies
  ✓ Production variant includes error handling
```

**Benefit:** Automatic management and validation of artifact variants

---

## Example 8: Observable Metrics from Artifacts

### Scenario: Measuring Workflow Quality Over Time

**Without Artifact Tracking:**

```bash
# Manual metrics collection
# Developer tracks in spreadsheet:
# - How many components created
# - How many passed validation first try
# - How many iterations required
# - Test coverage per component
```

Manual, error-prone, inconsistent.

---

**With Artifact Tracking (Metrics):**

```toml
[metrics.artifacts]
spec_completeness = "Percentage of required specification sections complete"
test_coverage = "Percentage of specification requirements covered by tests"
implementation_completeness = "Percentage of specification features implemented"
validation_pass_rate = "Percentage of validations passing first try"
```

Formula executor automatically tracks:
```javascript
const metrics = {
  workflow_runs: [
    {
      component: "MessageRouter",
      timestamp: "2026-01-11T10:23:45Z",
      artifacts: {
        spec_md: { completeness: 100, sections: 5, validation_pass: true },
        model_lisp: { completeness: 100, state_fields: 8, validation_pass: true },
        test_js: { coverage: 98, tests: 45, validation_pass: true },
        impl_js: { completeness: 100, features: 12, validation_pass: false }
      },
      quality_gates: {
        test_pass_rate: 96,
        spec_compliance: 100,
        iteration_count: 2
      }
    },
    {
      component: "CacheManager",
      // ...
    }
  ]
};
```

Automatic reporting:
```
Workflow Quality Report (Last 10 Components)

Artifact Quality:
  Specifications (spec.md):
    Avg completeness: 98% (range: 95-100%)
    First-pass validation: 90%

  Formal Models (model.lisp):
    Avg completeness: 97% (range: 92-100%)
    First-pass validation: 80%

  Test Suites (test.js):
    Avg coverage: 94% (range: 85-100%)
    First-pass validation: 70%

  Implementations (impl.js):
    Avg completeness: 96% (range: 88-100%)
    First-pass validation: 60%

Bottleneck Analysis:
  Implementation validation has lowest first-pass rate (60%)
  Recommendation: Improve specification clarity to reduce implementation issues
```

**Benefit:** Automatic quality metrics and bottleneck identification

---

## Example 9: Brownfield Reverse-Engineering

### Scenario: Creating Specs from Existing Code

**Without Artifact Tracking:**

```bash
# Manually reverse-engineer existing code
# Create spec.md by reading impl.js
# Hope spec matches actual behavior
# No validation that spec is accurate
```

---

**With Artifact Tracking (Reverse-Engineering):**

```toml
# brownfield-spec.formula.toml
formula = "brownfield-spec"
description = "Reverse-engineer specifications from existing code"

[[steps]]
id = "existing-implementation"
title = "Identify existing implementation"

[[steps.outputs]]
artifact = "{{component_name}}.js"
path = "{{existing_code_path}}"
type = "implementation"
source = "existing"  # Not produced by this workflow
required = true

[[steps]]
id = "reverse-engineer"
title = "Extract specifications from implementation"
agent = "@agent-dev-spec-modeler"
needs = ["existing-implementation"]

[[steps.inputs]]
artifact = "{{component_name}}.js"
type = "implementation"
source = "existing"

[[steps.outputs]]
artifact = "{{component_name}}.spec.md"
type = "documentation"
validation = [
  "All public methods documented",
  "All state fields identified",
  "All message types extracted"
]

[[steps.outputs]]
artifact = "{{component_name}}.model.lisp"
type = "formal-spec"
validation = [
  "State schema matches implementation",
  "Message interface matches implementation",
  "All behaviors captured"
]

[[steps]]
id = "validation"
title = "Validate extracted specs against implementation"
agent = "@agent-dev-validator"

# Cross-validation: specs should describe implementation accurately
[[steps.quality_gates]]
gate = "spec_accuracy"
description = "Extracted specs must accurately describe implementation"
thresholds = {
  state_field_match: 100,
  method_coverage: 100,
  behavior_accuracy: 95
}
```

Formula executor validates:
```
Reverse-Engineering Validation:

Artifact: MessageRouter.spec.md (extracted)
Artifact: MessageRouter.model.lisp (extracted)
Artifact: MessageRouter.js (existing)

Cross-Validation Results:
  State Fields:
    ✓ Implementation has 8 state fields
    ✓ Model defines 8 state fields
    ✓ 100% match

  Message Interface:
    ✓ Implementation handles 6 message types
    ✓ Model defines 6 message types
    ✓ 100% match

  Behavioral Coverage:
    ✓ Spec documents 12 of 12 public methods (100%)
    ⚠ Spec may miss 2 edge cases in error handling (95% confidence)

Validation: PASS (with warnings)
Recommendation: Review edge case handling documentation
```

**Benefit:** Validated reverse-engineering with accuracy guarantees

---

## Example 10: Security and Compliance Validation

### Scenario: Ensure Artifacts Meet Security Standards

**Without Artifact Tracking:**

```bash
# Manual security review
# Developer reads code
# Manually checks for common issues
# Inconsistent coverage
```

---

**With Artifact Tracking (Security Validation):**

```toml
[quality_gates.strict]
test_pass_rate_min = 100
coverage_min = 100
spec_compliance_min = 100
security_scan = true  # ← Additional validation

[[steps.outputs]]
artifact = "{{component_name}}.js"
type = "implementation"
validation = [
  "No hardcoded credentials",
  "Input validation on all public methods",
  "Error messages don't leak sensitive data",
  "No SQL injection vulnerabilities",
  "No command injection vulnerabilities",
  "Dependencies have no known CVEs"
]
```

Formula executor runs security scans:
```
Security Validation: MessageRouter.js

Static Analysis Results:
  ✓ No hardcoded credentials found
  ✓ Input validation present on all 6 public methods
  ✓ Error messages sanitized (no data leakage)
  ✓ No SQL injection vulnerabilities detected
  ✗ Potential command injection in executeShellCommand()
    Line 142: execSync(userInput) without sanitization
    Severity: HIGH
    Recommendation: Use parameterized execution or whitelist allowed commands

Dependency Scan Results:
  ✓ No known CVEs in direct dependencies
  ⚠ Transitive dependency 'old-parser@1.2.3' has CVE-2024-12345 (MEDIUM)
    Recommendation: Update to version 1.2.4+

Security Validation: FAILED
Cannot proceed to approval - security issues must be resolved.
```

**Benefit:** Automatic security validation with clear remediation steps

---

## Summary: Value Proposition

### Debugging (Example 1)
- **Before:** 15-20 minutes manual investigation
- **After:** 2-3 minutes with precise error messages
- **Benefit:** 80% reduction in debugging time

### Quality Gates (Example 2)
- **Before:** Multiple days, manual PR review
- **After:** Immediate automated feedback
- **Benefit:** Days of cycle time saved

### Root Cause Analysis (Example 3)
- **Before:** Manual git archaeology
- **After:** Automatic lineage-based analysis
- **Benefit:** Instant root cause identification

### Workflow Composition (Example 4)
- **Before:** Manual coordination, error-prone
- **After:** Automatic artifact resolution
- **Benefit:** Composable workflows, reduced errors

### Isolation Enforcement (Example 5)
- **Before:** Relies on agent discipline
- **After:** Automatic access control
- **Benefit:** Guaranteed parallel development integrity

### Recovery (Example 6)
- **Before:** Manual checkpoint inspection
- **After:** Automatic recovery from last validated artifact
- **Benefit:** No wasted re-execution of completed work

### Variant Management (Example 7)
- **Before:** Manual sync between variants
- **After:** Automatic variant validation
- **Benefit:** Consistent multi-variant artifacts

### Metrics (Example 8)
- **Before:** Manual tracking in spreadsheets
- **After:** Automatic metrics collection
- **Benefit:** Data-driven workflow optimization

### Reverse-Engineering (Example 9)
- **Before:** No validation of spec accuracy
- **After:** Cross-validation against implementation
- **Benefit:** Guaranteed spec accuracy

### Security (Example 10)
- **Before:** Inconsistent manual review
- **After:** Automatic security scanning
- **Benefit:** Consistent security standards enforcement

---

## Conclusion

Artifact-aware formulas transform workflows from "hope it works" to "know it works":

- **Precise errors** instead of vague failures
- **Automatic validation** instead of manual review
- **Data-driven decisions** instead of guesswork
- **Composable workflows** instead of one-offs
- **Traceable lineage** instead of mystery failures

**The examples demonstrate clear, measurable value** that justifies the additional complexity of artifact tracking.
