# Formula Implementation Roadmap: Phase 2/3 Recommendations

**Date:** 2026-01-11
**Purpose:** Actionable roadmap for implementing artifact-aware formulas in beads

---

## Executive Summary

**What we've created:**
- `component-dev.formula.toml` - Production-ready formula with artifact tracking
- Design documentation explaining the artifact-aware approach
- 10 concrete examples demonstrating value
- Cost-benefit analysis showing 2.3x ROI

**What to do next:**
- Implement in 4 phases over 10 weeks
- Start with Week 1-2 (schema + registry) for immediate value
- Measure success with concrete metrics
- Iterate based on findings

**Expected impact:**
- 80% reduction in debugging time (15 min → 2-3 min)
- Days saved in PR review cycles (immediate feedback)
- Automatic quality enforcement (95%+ compliance)
- Composable workflows (reuse across projects)

---

## Phase 1: Schema + Basic Registry (Weeks 1-2)

### Goal
Enable precise error messages through artifact tracking

### Tasks

#### Week 1: Schema Extensions

**1.1 Extend TOML Parser**
```javascript
// Add support for nested artifact arrays
class FormulaParser {
  parseStep(stepData) {
    return {
      id: stepData.id,
      title: stepData.title,
      agent: stepData.agent,
      // NEW: Parse artifact arrays
      outputs: stepData.outputs?.map(o => this.parseArtifact(o)) || [],
      inputs: stepData.inputs?.map(i => this.parseArtifact(i)) || [],
      quality_gates: stepData.quality_gates || []
    };
  }

  parseArtifact(artifactData) {
    return {
      artifact: artifactData.artifact,
      path: this.resolvePath(artifactData.path),
      type: artifactData.type,
      required: artifactData.required ?? true,
      description: artifactData.description,
      validation: artifactData.validation || []
    };
  }
}
```

**Files to modify:**
- `src/formula-parser.js` - Add artifact parsing
- `src/formula-schema.js` - Define artifact schema

**Test:**
```javascript
// Test parsing component-dev.formula.toml
const formula = parser.parse('component-dev.formula.toml');
assert(formula.steps[1].outputs.length === 2); // spec.md + model.lisp
assert(formula.steps[1].outputs[0].artifact === '{{component_name}}.spec.md');
```

---

**1.2 Variable Interpolation**
```javascript
class FormulaParser {
  resolvePath(pathTemplate, vars) {
    let resolved = pathTemplate;
    for (const [key, value] of Object.entries(vars)) {
      resolved = resolved.replace(new RegExp(`{{${key}}}`, 'g'), value);
    }
    return resolved;
  }
}
```

**Test:**
```javascript
const vars = { component_name: 'MessageRouter', output_dir: 'src' };
const path = parser.resolvePath('{{output_dir}}/{{component_name}}.js', vars);
assert(path === 'src/MessageRouter.js');
```

---

#### Week 2: Artifact Registry

**2.1 Implement Registry**
```javascript
class ArtifactRegistry {
  constructor() {
    this.artifacts = new Map(); // artifact_id -> ArtifactMetadata
  }

  register(stepId, artifactDef, vars) {
    const id = this.resolveTemplate(artifactDef.artifact, vars);
    const artifact = {
      id,
      path: this.resolveTemplate(artifactDef.path, vars),
      type: artifactDef.type,
      required: artifactDef.required,
      description: artifactDef.description,
      validation: artifactDef.validation,
      producedBy: stepId,
      status: 'pending'  // pending, available, validated, invalid
    };
    this.artifacts.set(id, artifact);
    return artifact;
  }

  getStatus(artifactId) {
    const artifact = this.artifacts.get(artifactId);
    if (!artifact) return 'not_registered';
    return artifact.status;
  }

  checkInputsAvailable(step) {
    const missing = [];
    for (const inputDef of step.inputs) {
      const id = this.resolveTemplate(inputDef.artifact, this.vars);
      const status = this.getStatus(id);
      if (status !== 'validated' && status !== 'available') {
        missing.push({ id, status, inputDef });
      }
    }
    return { available: missing.length === 0, missing };
  }
}
```

**Files to create:**
- `src/artifact-registry.js` - Registry implementation

**Test:**
```javascript
const registry = new ArtifactRegistry();
const artifact = registry.register('specifications', {
  artifact: 'spec.md',
  path: 'src/spec.md',
  type: 'documentation',
  required: true
}, {});

assert(registry.getStatus('spec.md') === 'pending');
```

---

**2.2 Integrate with Executor**
```javascript
class FormulaExecutor {
  constructor() {
    this.registry = new ArtifactRegistry();
  }

  async executeStep(step) {
    // Check inputs available
    const inputCheck = this.registry.checkInputsAvailable(step);
    if (!inputCheck.available) {
      throw new ArtifactError(
        `Step ${step.id} missing required inputs`,
        inputCheck.missing
      );
    }

    // Execute step
    await this.runAgent(step.agent, step.description);

    // Register outputs
    for (const outputDef of step.outputs) {
      const artifact = this.registry.register(step.id, outputDef, this.vars);

      // Basic availability check
      if (fs.existsSync(artifact.path)) {
        artifact.status = 'available';
      } else if (artifact.required) {
        throw new ArtifactError(
          `Step ${step.id} did not produce required artifact ${artifact.id}`,
          [{ id: artifact.id, path: artifact.path, producedBy: step.id }]
        );
      }
    }
  }
}
```

**Files to modify:**
- `src/formula-executor.js` - Add registry integration

---

**2.3 Enhanced Error Messages**
```javascript
class ArtifactError extends Error {
  constructor(message, artifacts) {
    super(message);
    this.name = 'ArtifactError';
    this.artifacts = artifacts;
  }

  format() {
    let msg = `❌ ${this.message}\n\n`;

    if (this.artifacts.length > 0) {
      msg += 'Missing Artifacts:\n';
      for (const artifact of this.artifacts) {
        msg += `  ✗ ${artifact.id} (${artifact.type || 'unknown'})\n`;
        msg += `    Expected path: ${artifact.path}\n`;
        msg += `    Status: ${artifact.status}\n`;
        if (artifact.producedBy) {
          msg += `    Produced by: step '${artifact.producedBy}'\n`;
        }
      }

      msg += '\nRecommendation: Check agent logs for step that should produce missing artifacts\n';
    }

    return msg;
  }
}
```

**Files to create:**
- `src/artifact-error.js` - Error formatting

---

### Success Criteria (Week 2)

**Functional:**
- ✓ Formula parser can read artifact arrays
- ✓ Variable interpolation works for artifact paths
- ✓ Registry tracks artifacts and their status
- ✓ Executor checks inputs before step execution
- ✓ Executor registers outputs after step completion
- ✓ Error messages include artifact details

**Metrics:**
- Baseline: Average 15 minutes to diagnose workflow failures
- Target: Reduce to 5 minutes (67% reduction)
- Measure: Time from error to identifying problematic artifact

**Test Plan:**
```bash
# Run component-dev formula with missing implementation
bd mol wisp component-dev --var component_name="TestRouter"

# Artificially skip implementation step
# Expected: Clear error message identifying missing impl.js artifact
# Measure: Time to diagnose < 5 minutes
```

---

## Phase 2: Structural Validation (Weeks 3-4)

### Goal
Automatic quality gates with file-based validation

### Tasks

#### Week 3: Validation Engine

**3.1 Implement Criterion Evaluators**
```javascript
class ValidationEngine {
  constructor(registry) {
    this.registry = registry;
    this.evaluators = {
      'File exists': this.checkFileExists.bind(this),
      'Valid Lisp syntax': this.checkLispSyntax.bind(this),
      'Valid JavaScript syntax': this.checkJSSyntax.bind(this),
      'Contains required sections: {sections}': this.checkRequiredSections.bind(this),
      'NO test skipping': this.checkNoTestSkipping.bind(this)
    };
  }

  async validate(artifact) {
    const results = [];

    for (const criterion of artifact.validation) {
      const evaluator = this.findEvaluator(criterion);
      const result = await evaluator(artifact, criterion);
      results.push({
        criterion,
        passed: result.passed,
        details: result.details
      });
    }

    artifact.status = results.every(r => r.passed) ? 'validated' : 'invalid';
    return results;
  }

  checkFileExists(artifact) {
    const exists = fs.existsSync(artifact.path);
    return {
      passed: exists,
      details: exists ? `File found at ${artifact.path}` : `File not found at ${artifact.path}`
    };
  }

  checkLispSyntax(artifact) {
    try {
      const content = fs.readFileSync(artifact.path, 'utf8');
      // Basic Lisp syntax check (balanced parens)
      let depth = 0;
      for (const char of content) {
        if (char === '(') depth++;
        if (char === ')') depth--;
        if (depth < 0) throw new Error('Unmatched closing paren');
      }
      if (depth !== 0) throw new Error('Unmatched opening paren');
      return { passed: true, details: 'Valid Lisp syntax' };
    } catch (e) {
      return { passed: false, details: `Syntax error: ${e.message}` };
    }
  }

  checkNoTestSkipping(artifact) {
    const content = fs.readFileSync(artifact.path, 'utf8');
    const skipPatterns = [
      /test\.skip/,
      /test\.todo/,
      /if\s*\([^)]*\)\s*return/,  // Conditional returns
      /console\.warn.*skip/i
    ];

    const violations = [];
    for (const pattern of skipPatterns) {
      const matches = content.match(new RegExp(pattern, 'g'));
      if (matches) {
        violations.push(...matches);
      }
    }

    return {
      passed: violations.length === 0,
      details: violations.length > 0
        ? `Found ${violations.length} test skipping patterns: ${violations.join(', ')}`
        : 'No test skipping detected'
    };
  }
}
```

**Files to create:**
- `src/validation-engine.js` - Core validation logic
- `src/evaluators/` - Directory for criterion evaluators

---

**3.2 Integrate Validation with Executor**
```javascript
class FormulaExecutor {
  async executeStep(step) {
    // ... check inputs, run agent ...

    // Register and validate outputs
    for (const outputDef of step.outputs) {
      const artifact = this.registry.register(step.id, outputDef, this.vars);

      // Run validation
      const results = await this.validationEngine.validate(artifact);

      // Report results
      console.log(`\nArtifact: ${artifact.id}`);
      for (const result of results) {
        const icon = result.passed ? '✓' : '✗';
        console.log(`  ${icon} ${result.criterion}`);
        if (result.details) {
          console.log(`    ${result.details}`);
        }
      }

      // Fail if validation fails
      if (artifact.status === 'invalid' && artifact.required) {
        throw new ValidationError(
          `Artifact ${artifact.id} failed validation`,
          artifact,
          results
        );
      }
    }
  }
}
```

---

#### Week 4: Quality Gates

**4.1 Implement Quality Gate Checks**
```javascript
class FormulaExecutor {
  async checkQualityGates(step) {
    if (!step.quality_gates) return;

    for (const gate of step.quality_gates) {
      if (gate.gate === 'validation_thresholds') {
        await this.checkValidationThresholds(gate, step);
      } else if (gate.gate === 'user_approval') {
        await this.requestUserApproval(gate, step);
      }
    }
  }

  async checkValidationThresholds(gate, step) {
    const thresholds = this.resolveThresholds(gate.thresholds);
    const metrics = this.computeStepMetrics(step);

    const failures = [];
    for (const [key, minValue] of Object.entries(thresholds)) {
      const actualValue = metrics[key];
      if (actualValue < minValue) {
        failures.push({
          metric: key,
          required: minValue,
          actual: actualValue
        });
      }
    }

    if (failures.length > 0 && gate.blocking) {
      throw new QualityGateError(
        `Quality gate '${gate.description}' failed`,
        failures
      );
    }
  }

  async requestUserApproval(gate, step) {
    if (!gate.blocking) return;

    console.log(`\n🚪 Quality Gate: ${gate.description}`);
    console.log('\nStep artifacts:');

    // Show all step outputs
    for (const outputDef of step.outputs) {
      const artifact = this.registry.artifacts.get(
        this.resolveTemplate(outputDef.artifact, this.vars)
      );
      console.log(`  ${artifact.status === 'validated' ? '✓' : '✗'} ${artifact.id}`);
    }

    const approved = await this.promptUser('\nProceed to next phase? [y/N] ');
    if (!approved) {
      throw new Error('User rejected quality gate');
    }
  }
}
```

**Files to modify:**
- `src/formula-executor.js` - Add quality gate checks

---

### Success Criteria (Week 4)

**Functional:**
- ✓ Validation engine can evaluate structural criteria
- ✓ File existence, syntax checks work
- ✓ Test skipping detection works
- ✓ Quality gates block workflow on failure
- ✓ User approval gates work

**Metrics:**
- Target: 70% of quality issues caught automatically (before PR review)
- Measure: Track validation failures vs issues found in manual review

**Test Plan:**
```bash
# Create test with skipped tests
echo 'test.skip("skipped test", () => {})' > test-invalid.test.js

# Run formula with this artifact
bd mol wisp component-dev --var component_name="Invalid"

# Expected: Validation fails with clear message about test skipping
# Metric: Issue caught before PR review
```

---

## Phase 3: Semantic Validation (Weeks 5-6)

### Goal
Deep quality checks with content analysis

### Tasks

#### Week 5: Specification Parsing

**5.1 Extract Requirements from Spec**
```javascript
class SpecificationParser {
  parseSpecMd(path) {
    const content = fs.readFileSync(path, 'utf8');
    const requirements = [];

    // Parse markdown to extract requirements
    // Look for numbered lists, "MUST", "SHOULD", etc.
    const lines = content.split('\n');
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];

      // Match requirement patterns
      if (/^\d+\.\d+\s+/.test(line)) {
        requirements.push({
          id: line.match(/^(\d+\.\d+)/)[1],
          text: line.replace(/^\d+\.\d+\s+/, ''),
          line: i + 1
        });
      }
    }

    return { requirements };
  }

  parseModelLisp(path) {
    const content = fs.readFileSync(path, 'utf8');

    // Extract state schema fields
    const stateFields = this.extractStateSchema(content);

    // Extract message types
    const messageTypes = this.extractMessageTypes(content);

    return { stateFields, messageTypes };
  }
}
```

**Files to create:**
- `src/parsers/spec-parser.js` - Spec.md parsing
- `src/parsers/model-parser.js` - Model.lisp parsing

---

**5.2 Cross-Artifact Validation**
```javascript
class ValidationEngine {
  async validateCrossArtifact(criterion, artifact, relatedArtifacts) {
    if (criterion === 'Every requirement from spec.md has a test') {
      return await this.checkTestCoverage(artifact, relatedArtifacts['spec.md']);
    }

    if (criterion === 'All state fields from state-schema initialized') {
      return await this.checkStateInitialization(artifact, relatedArtifacts['model.lisp']);
    }
  }

  async checkTestCoverage(testArtifact, specArtifact) {
    // Parse spec to extract requirements
    const spec = this.specParser.parseSpecMd(specArtifact.path);

    // Parse test file to extract test names
    const testContent = fs.readFileSync(testArtifact.path, 'utf8');
    const testNames = this.extractTestNames(testContent);

    // Match requirements to tests
    const missing = [];
    for (const req of spec.requirements) {
      const hasTest = testNames.some(name =>
        name.includes(req.id) ||
        name.toLowerCase().includes(req.text.toLowerCase().slice(0, 20))
      );

      if (!hasTest) {
        missing.push(req);
      }
    }

    return {
      passed: missing.length === 0,
      details: missing.length > 0
        ? `Missing tests for ${missing.length} requirements: ${missing.map(r => r.id).join(', ')}`
        : `All ${spec.requirements.length} requirements have tests`
    };
  }
}
```

---

#### Week 6: Executable Validation

**6.1 Run Tests and Analyze Results**
```javascript
class ValidationEngine {
  async validateExecutable(criterion, artifact) {
    if (criterion === 'Tests pass when executed') {
      return await this.runTests(artifact);
    }
  }

  async runTests(testArtifact) {
    // Run test suite
    const result = await this.executor.exec(`bun test ${testArtifact.path}`);

    // Parse test output
    const summary = this.parseTestOutput(result.stdout);

    return {
      passed: summary.failures === 0,
      details: `${summary.passed} passed, ${summary.failures} failed, ${summary.skipped} skipped`,
      metrics: {
        test_pass_rate: (summary.passed / summary.total) * 100,
        test_count: summary.total
      }
    };
  }

  parseTestOutput(output) {
    // Parse test framework output
    // Returns: { passed, failures, skipped, total }
    const match = output.match(/(\d+) passed.*?(\d+) failed/);
    return {
      passed: parseInt(match[1]),
      failures: parseInt(match[2]),
      skipped: 0,
      total: parseInt(match[1]) + parseInt(match[2])
    };
  }
}
```

---

### Success Criteria (Week 6)

**Functional:**
- ✓ Can parse spec.md to extract requirements
- ✓ Can parse model.lisp to extract state fields and messages
- ✓ Cross-artifact validation works (test coverage, state init)
- ✓ Can execute tests and analyze results
- ✓ Metrics captured from validation

**Metrics:**
- Target: 90% of spec-test-impl inconsistencies detected
- Measure: Validation findings vs issues found in integration testing

**Test Plan:**
```bash
# Create component with missing test coverage
# spec.md has 5 requirements
# test.js only tests 3 requirements

bd mol wisp component-dev --var component_name="Incomplete"

# Expected: Validation identifies 2 missing test requirements
# Metric: Caught before integration testing
```

---

## Phase 4: Composition + Recovery (Weeks 7-8)

### Goal
Composable workflows and automatic recovery

### Tasks

#### Week 7: Workflow Composition

**7.1 Cross-Formula Artifact References**
```javascript
class ArtifactRegistry {
  async resolveExternalArtifact(inputDef) {
    if (!inputDef.source_formula) return null;

    // Find artifact from previous workflow execution
    const sourceExecutionId = this.findLatestExecution(
      inputDef.source_formula,
      inputDef.source_vars
    );

    if (!sourceExecutionId) {
      throw new Error(
        `Cannot find execution of formula '${inputDef.source_formula}' ` +
        `with vars ${JSON.stringify(inputDef.source_vars)}`
      );
    }

    // Load artifact metadata from previous execution
    const artifact = await this.loadArtifactMetadata(
      sourceExecutionId,
      inputDef.artifact
    );

    // Verify artifact exists and is validated
    if (artifact.status !== 'validated') {
      throw new Error(
        `Artifact '${artifact.id}' from formula '${inputDef.source_formula}' ` +
        `is not validated (status: ${artifact.status})`
      );
    }

    return artifact;
  }
}
```

**Files to modify:**
- `src/artifact-registry.js` - Add cross-formula resolution

---

**7.2 Execution History**
```javascript
class ExecutionHistory {
  async saveExecution(formulaName, vars, artifacts) {
    const executionId = this.generateExecutionId(formulaName, vars);
    const record = {
      id: executionId,
      formula: formulaName,
      vars,
      artifacts: artifacts.map(a => ({
        id: a.id,
        path: a.path,
        type: a.type,
        status: a.status,
        validation_results: a.validation_results
      })),
      timestamp: Date.now()
    };

    await this.storage.save(`executions/${executionId}.json`, record);
    return executionId;
  }

  async findLatestExecution(formulaName, vars) {
    const executions = await this.storage.list('executions/');

    // Filter by formula and vars
    const matches = executions.filter(e =>
      e.formula === formulaName &&
      this.varsMatch(e.vars, vars)
    );

    // Return most recent
    return matches.sort((a, b) => b.timestamp - a.timestamp)[0]?.id;
  }
}
```

**Files to create:**
- `src/execution-history.js` - Execution tracking

---

#### Week 8: Checkpoint and Recovery

**8.1 Workflow Checkpointing**
```javascript
class FormulaExecutor {
  async executeWorkflow(formula, vars) {
    // Load checkpoint if exists
    const checkpoint = await this.loadCheckpoint(formula.name, vars);

    if (checkpoint) {
      console.log('Checkpoint found. Workflow state:');
      for (const [stepId, status] of Object.entries(checkpoint.steps)) {
        console.log(`  ${status === 'complete' ? '✓' : '○'} ${stepId}`);
      }

      const resume = await this.promptUser('\nResume from checkpoint? [y/N] ');
      if (resume) {
        return await this.resumeFromCheckpoint(formula, vars, checkpoint);
      }
    }

    // Execute workflow with checkpointing
    for (const step of formula.steps) {
      await this.executeStep(step);

      // Save checkpoint after each step
      await this.saveCheckpoint(formula.name, vars, {
        steps: this.getStepStatuses(),
        artifacts: this.registry.artifacts
      });
    }
  }

  async resumeFromCheckpoint(formula, vars, checkpoint) {
    // Restore artifact registry
    this.registry.restore(checkpoint.artifacts);

    // Find first incomplete step
    for (const step of formula.steps) {
      if (checkpoint.steps[step.id] === 'complete') {
        console.log(`⏭️  Skipping ${step.id} (already complete)`);
        continue;
      }

      console.log(`▶️  Resuming from ${step.id}`);
      await this.executeStep(step);
      await this.saveCheckpoint(formula.name, vars, {
        steps: this.getStepStatuses(),
        artifacts: this.registry.artifacts
      });
    }
  }
}
```

**Files to modify:**
- `src/formula-executor.js` - Add checkpointing

---

### Success Criteria (Week 8)

**Functional:**
- ✓ Can reference artifacts from other formula executions
- ✓ Execution history persisted and queryable
- ✓ Checkpoints saved after each step
- ✓ Can resume from checkpoint on failure
- ✓ Only re-runs incomplete steps

**Metrics:**
- Target: 80% reduction in recovery time
- Measure: Time to resume vs time to re-run from scratch

**Test Plan:**
```bash
# Run workflow to partial completion
bd mol wisp component-dev --var component_name="Test"
# Kill process after Phase 2

# Resume workflow
bd mol wisp component-dev --var component_name="Test"
# Expected: Resumes from Phase 3, skips Phases 1-2
# Metric: Recovery time < 20% of full execution time
```

---

## Phase 5: Production Polish (Weeks 9-10)

### Goal
Production-ready system with documentation

### Tasks

#### Week 9: Documentation

**9.1 Formula Author Guide**
```markdown
# Writing Artifact-Aware Formulas

## Step 1: Define Outputs
Every step that creates files should declare outputs:

```toml
[[steps.outputs]]
artifact = "{{component_name}}.js"
path = "{{output_dir}}/{{component_name}}.js"
type = "implementation"
required = true
validation = [
  "File exists",
  "Valid JavaScript syntax"
]
```

## Step 2: Define Inputs
Steps that depend on artifacts should declare inputs:

```toml
[[steps.inputs]]
artifact = "{{component_name}}.spec.md"
type = "documentation"
required = true
```

## Step 3: Add Quality Gates
Define thresholds for automatic quality enforcement:

```toml
[[steps.quality_gates]]
gate = "validation_thresholds"
blocking = true
thresholds = "quality_gates.standard"
```
```

**Files to create:**
- `docs/formula-author-guide.md`
- `docs/validation-criteria-catalog.md`
- `docs/artifact-types.md`

---

**9.2 Example Formulas**
```bash
examples/
├── component-dev.formula.toml (System Modeling Quad)
├── brownfield-spec.formula.toml (Reverse-engineering)
├── system-integration.formula.toml (Composition)
├── spec-refinement.formula.toml (Iterative improvement)
└── multi-component.formula.toml (Parallel components)
```

---

#### Week 10: Team Training

**10.1 Training Materials**
- Workshop: Introduction to Artifact-Aware Formulas
- Hands-on: Create your first formula
- Best practices: Common patterns and anti-patterns
- Troubleshooting guide

**10.2 Migration Guide**
```markdown
# Migrating to Artifact-Aware Formulas

## Option 1: Keep Existing Formulas
Your current formulas continue to work. No changes needed.

## Option 2: Add Artifact Tracking
Enhance existing formulas with artifact arrays:

```toml
# Before
[[steps]]
description = "Create spec.md and model.lisp"

# After
[[steps]]
description = "Create specifications"

[[steps.outputs]]
artifact = "spec.md"
type = "documentation"
validation = ["File exists"]
```

## Option 3: Full Artifact-Aware
Use complete artifact tracking with all features.
```

---

### Success Criteria (Week 10)

**Functional:**
- ✓ Comprehensive documentation complete
- ✓ Example formulas for common patterns
- ✓ Training materials ready
- ✓ Migration guide for existing formulas

**Adoption:**
- Target: 80% of new formulas use artifact tracking
- Measure: Track formula creation over next 3 months

---

## Success Metrics: How to Measure ROI

### Week 2: Basic Registry

**Metric:** Time to diagnose workflow failures
**Baseline:** 15 minutes average
**Target:** 5 minutes (67% reduction)
**How to measure:**
1. Track failure diagnosis time before artifact tracking (2 weeks)
2. Track after Week 2 implementation (2 weeks)
3. Compare averages

---

### Week 4: Structural Validation

**Metric:** Quality issues caught before PR review
**Baseline:** 30% caught automatically
**Target:** 70% caught automatically
**How to measure:**
1. Track validation failures (automatic)
2. Track issues found in PR review (manual)
3. Calculate: automatic / (automatic + manual)

---

### Week 6: Semantic Validation

**Metric:** Spec-test-impl consistency issues detected
**Baseline:** 50% detected before integration testing
**Target:** 90% detected before integration testing
**How to measure:**
1. Track validation findings (cross-artifact checks)
2. Track issues found in integration testing
3. Calculate: validation / (validation + integration)

---

### Week 8: Recovery

**Metric:** Workflow recovery time
**Baseline:** 100% of execution time (re-run from scratch)
**Target:** 20% of execution time (resume from checkpoint)
**How to measure:**
1. Simulate failure at various points
2. Measure time to resume vs time to re-run
3. Calculate: resume_time / full_execution_time

---

### 6 Months: Overall ROI

**Metric:** Total time saved
**Target:** 500+ hours (on track for 925/year)
**How to measure:**
1. Debugging time saved: Track per-failure diagnosis time × failure count
2. PR review time saved: Track issues caught automatically × review cycle time
3. Recovery time saved: Track recovery time before vs after
4. Sum: Total hours saved

**ROI Calculation:**
- Investment: 10 weeks = 400 hours
- Return: 500+ hours saved
- ROI: 500/400 = 1.25x (on track for 2.3x annual)

---

## Risk Mitigation

### Risk 1: Adoption Resistance

**Risk:** Team finds artifact tracking too complex
**Mitigation:**
- Maintain backward compatibility (current formulas still work)
- Provide simple examples (start with basic artifact tracking)
- Offer migration support (pair programming sessions)
- Show value early (Week 2 precise errors)

---

### Risk 2: Performance Overhead

**Risk:** Validation slows down workflow execution
**Mitigation:**
- Lazy validation (only validate when needed)
- Parallel validation (validate multiple artifacts simultaneously)
- Caching (don't re-validate unchanged artifacts)
- Configurable validation levels (standard vs strict)

---

### Risk 3: Schema Complexity

**Risk:** Artifact schema becomes too complex
**Mitigation:**
- Start simple (file existence, syntax only)
- Add complexity incrementally (semantic validation opt-in)
- Provide templates (common validation patterns)
- Clear documentation (validation criteria catalog)

---

### Risk 4: Cross-Formula Dependencies

**Risk:** Composition creates brittle dependencies
**Mitigation:**
- Version artifact contracts (semantic types)
- Validate compatibility (type checking)
- Clear error messages (missing or incompatible artifacts)
- Fallback strategies (manual artifact specification)

---

## Decision Points

### Week 2: Continue or Pivot?

**Evaluate:**
- Is debugging time reduced by 50%+?
- Are error messages significantly clearer?
- Is team feedback positive?

**If yes:** Continue to Week 3-4 (structural validation)
**If no:** Investigate issues, adjust approach

---

### Week 4: Continue or Pivot?

**Evaluate:**
- Are 70%+ of quality issues caught automatically?
- Are quality gates working effectively?
- Is adoption trajectory positive?

**If yes:** Continue to Week 5-6 (semantic validation)
**If no:** Focus on improving structural validation, polish UX

---

### Week 6: Continue or Pivot?

**Evaluate:**
- Are 90%+ of inconsistencies detected?
- Is semantic validation providing value?
- Is performance acceptable?

**If yes:** Continue to Week 7-8 (composition + recovery)
**If no:** Optimize semantic validation, consider reducing scope

---

### Week 8: Production or Polish?

**Evaluate:**
- Is recovery working reliably?
- Is composition solving real problems?
- Are all success metrics on track?

**If yes:** Proceed to Week 9-10 (production polish)
**If no:** Extend implementation phase, delay documentation

---

## Recommended Next Steps

### Immediate (This Week):

1. **Review deliverables:**
   - `component-dev.formula.toml` - Formula with artifact tracking
   - `FORMULA_ARTIFACT_DESIGN.md` - Design rationale
   - `FORMULA_ARTIFACT_EXAMPLES.md` - 10 concrete examples
   - `FORMULA_APPROACH_COMPARISON.md` - Cost-benefit analysis

2. **Make decision:**
   - Proceed with implementation? (Recommended: Yes)
   - Which phases to implement? (Recommended: All 4)
   - What timeline? (Recommended: 10 weeks)

3. **Assign resources:**
   - Who will implement? (1-2 engineers)
   - What priority? (Recommend: High - 2.3x ROI)
   - What support needed? (Code review, testing)

---

### Week 1-2 (Start Immediately):

1. **Implement schema + registry**
   - Extend TOML parser for artifact arrays
   - Build artifact registry
   - Integrate with executor
   - Create enhanced error messages

2. **Test with component-dev formula**
   - Run real component development workflow
   - Measure debugging time improvement
   - Collect team feedback

3. **Evaluate and decide:**
   - Are error messages clearer?
   - Is debugging faster?
   - Continue to Week 3-4?

---

### Week 3-10 (Contingent on Week 2 Success):

1. **Follow implementation roadmap**
2. **Measure success metrics at each phase**
3. **Iterate based on findings**
4. **Document learnings**
5. **Train team**

---

## Conclusion

**We have a clear path forward:**

1. **Proven value:** 10 examples demonstrate concrete benefits
2. **Justified investment:** 2.3x ROI in first year
3. **Low risk:** Incremental rollout with decision points
4. **Clear metrics:** Track success at each phase
5. **Production-ready design:** component-dev.formula.toml is complete

**Recommendation:** Start Week 1-2 implementation immediately to validate approach with real component development.

**Expected outcome:** By Week 2, we'll have precise error messages and data to decide whether to continue. By Week 10, we'll have a production-ready artifact-aware formula system with documented ROI.

---

**End of Roadmap**
