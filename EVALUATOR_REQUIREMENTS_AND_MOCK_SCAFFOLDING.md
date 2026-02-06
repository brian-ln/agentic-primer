# Evaluator Agent Requirements and Mock Scaffolding Analysis

**Created:** 2026-01-08 (as of Thu Jan 8 05:47:02 EST 2026)

## Executive Summary

The 27 simulation solutions in `/experiments/iteration-2/runs/run-20260106-003027-full-matrix` are **GitHub Actions workflows** that simulate @copilot agent behavior. An evaluator agent needs to assess **functional correctness** (not just efficiency or elegance). To run these in `act` (local GitHub Actions runner), we need **mock scaffolding** for external dependencies that `act` doesn't provide.

---

## 1. What These Solutions Are

### All Solutions Use GitHub Actions

**Finding:** 26 out of 27 test directories contain `.github/workflows/*.yml` files.

**Common Pattern:**
```yaml
on:
  issues:
    types: [opened, labeled]
  pull_request:
    types: [opened, synchronize]
```

### Three Distinct Approaches

| Approach | Count | Description | Example |
|----------|-------|-------------|---------|
| **Workflow simulation** | ~15 | Workflows with "Copilot agent simulation" steps that echo messages | P2-S2-sonnet |
| **Direct automation** | ~8 | Workflows that directly call GitHub APIs for issues/PRs | P1-S2-opus |
| **Hybrid + scripts** | ~4 | Workflows + shell scripts for validation/testing | P1-S3-sonnet |

**Key Insight:** Most solutions **acknowledge they are simulating** @copilot behavior since the actual GitHub Copilot agent API doesn't exist yet (as of Jan 2026).

---

## 2. External Dependencies in These Solutions

### 2.1 GitHub API Operations (24 out of 26 workflows)

**What they do:**
- `github.rest.issues.createComment()` - Post comments to issues
- `github.rest.issues.addLabels()` - Add labels like "copilot", "processing"
- `github.rest.issues.addAssignees()` - Assign issues/PRs to users
- `github.rest.pulls.create()` - Create pull requests
- `github.rest.pulls.createReview()` - Post PR reviews
- `github.rest.issues.listEventsForTimeline()` - Get issue timeline

**Example from P1-S2-opus:**
```yaml
- name: Acknowledge issue
  uses: actions/github-script@v7
  with:
    script: |
      await github.rest.issues.createComment({
        owner: context.repo.owner,
        repo: context.repo.repo,
        issue_number: issueNumber,
        body: `## @copilot Processing...`
      });
```

**act Compatibility:** `act` does NOT provide GitHub API endpoints. These calls will fail.

### 2.2 Git Operations (11 out of 26 workflows)

**What they do:**
- `git config user.name / user.email`
- `git checkout -b <branch>`
- `git add .`
- `git commit -m "..."`
- `git push origin <branch>`

**Example from P2-S2-sonnet:**
```bash
git config user.name "github-actions[bot]"
git checkout -b copilot/issue-${{ github.event.issue.number }}
git add .
git commit -m "feat: implement task from issue #123"
git push origin copilot/issue-123
```

**act Compatibility:** Git operations work locally, but `git push` fails (no remote configured in test environment).

### 2.3 File Creation and Modification

**What they do:**
- Create markdown files: `docs/knowledge/insights/logs/issues.jsonl`
- Create implementation artifacts: `src/features/issue-123-implementation.md`
- Update knowledge base: `docs/knowledge/patterns/*.md`

**act Compatibility:** Works fine - local filesystem operations succeed.

### 2.4 CODEOWNERS Auto-Assignment (18 solutions)

**What they do:**
```
# .github/CODEOWNERS
* @owner
docs/ @docs-team
```

**Purpose:** GitHub automatically requests review from specified users when PR touches those paths.

**act Compatibility:** CODEOWNERS is a GitHub feature, not a workflow action. `act` ignores it.

### 2.5 Simulated @copilot Processing

**Example from P2-S2-sonnet (lines 92-143):**
```yaml
- name: Copilot agent simulation
  run: |
    echo "Simulating Copilot agent work..."
    echo "  1. Analyzing issue requirements"
    echo "  2. Consulting knowledge base"
    echo "  3. Generating implementation plan"

    # Create a sample implementation file as proof of processing
    mkdir -p src/features
    cat > src/features/issue-${ISSUE_NUMBER}-implementation.md << 'IMPL'
    # Implementation for Issue #123
    This file represents the work completed by the Copilot agent.
    In a real scenario, this would contain actual code changes.
    IMPL
```

**Key Observation:** Many solutions **explicitly simulate** @copilot because they acknowledge the actual @copilot agent doesn't exist. This is **honest** but means the workflows don't actually do real work.

---

## 3. What Evaluator Agent Needs to Know

### 3.1 Correctness Criteria (Not Efficiency)

**From SUCCESS_CRITERIA.md:**
1. **Functional Test:** System processes a test issue end-to-end without errors
2. **Syntax Valid:** All generated files pass automated validation (yamllint, shellcheck)
3. **Observable Behavior:** GitHub workflow actually triggers on issue creation
4. **Reliability:** 90%+ success rate across 20+ test runs
5. **Multi-Agent:** Works with 3+ different AI agents
6. **Single-Command:** Bootstrap completes from bare repo
7. **Self-Improvement:** System creates 3+ successful improvement PRs

**Evaluator Focus:** Not "is this the most efficient solution?" but **"does this actually work?"**

### 3.2 Functional vs Syntactic Correctness

**Critical Distinction:**

| Dimension | Syntactic Validity | Functional Correctness |
|-----------|-------------------|----------------------|
| YAML workflows | yamllint passes | Workflow triggers and completes successfully |
| Shell scripts | shellcheck passes | Scripts execute without errors |
| Issue templates | Valid YAML schema | GitHub renders form correctly |
| CODEOWNERS | File exists | Auto-review actually happens |
| Git operations | Commands syntactically correct | Push succeeds to remote |
| @copilot invocation | Mentioned in comments | Actual agent receives task |

**From SIMULATION_ALGORITHM.md (lines 328-343):**
> **Finding:** Simulated tests hide implementation gaps (false positives)
>
> "Would this meet success criteria?"
> - In simulation: Yes (100% pass)
> - In reality: No (~25% functional)
>
> Critical gaps:
> - No @copilot trigger mechanism
> - Missing copilot-setup-steps.yml
> - Placeholder values (@owner)
> - No error handling

**Implication:** Evaluator must test "would this actually work in production?" not just "does the YAML parse?"

### 3.3 Key Questions Evaluator Must Answer

For each solution:

1. **Trigger Mechanism:**
   - Does the workflow trigger on the correct event (`issues.opened`)?
   - Are conditions properly scoped (`if: contains(labels, 'copilot')`)?

2. **@copilot Integration:**
   - How does the workflow "invoke" @copilot?
   - Is it simulated (echo statements) or real (API calls)?
   - Would a real @copilot agent receive the task?

3. **State Transitions:**
   - Does the workflow update issue labels correctly?
   - Does it create branches, commits, PRs in the right order?
   - Are there race conditions or missing error handlers?

4. **End-to-End Flow:**
   - User creates issue → Workflow triggers → @copilot processes → PR created → Review requested
   - Where would this break in production?

5. **Knowledge Base Integration:**
   - Does the solution consult existing knowledge?
   - Does it log learnings for future use?
   - Are knowledge paths hardcoded or configurable?

6. **Placeholder Detection:**
   - Count instances of `@owner`, `TODO`, `FIXME`, `<replace-me>`
   - Are these placeholders documented with instructions?
   - Would a user know what to replace?

---

## 4. Running in `act` - What's Possible

### 4.1 What `act` Provides

`act` is a local GitHub Actions runner that:
- Parses `.github/workflows/*.yml` files
- Runs jobs in Docker containers
- Provides environment variables (`GITHUB_*`)
- Supports `actions/checkout`, `actions/setup-*`
- Runs shell commands locally

**Official docs:** https://github.com/nektos/act

### 4.2 What `act` Does NOT Provide

| Feature | GitHub.com | act | Workaround |
|---------|-----------|-----|------------|
| GitHub API | Real endpoints | None | Mock server |
| Issue creation | Real issues | Event payloads | JSON fixtures |
| PR creation | Real PRs | None | Mock responses |
| CODEOWNERS | Auto-review | Ignored | Manual verification |
| Secrets | Encrypted | `.secrets` file | Test values |
| Webhooks | Real | Simulated | Event JSON |
| Remote git push | Real remote | Fails | Local bare repo |

### 4.3 Triggering Workflows in `act`

**Simulating an issue creation event:**

```bash
# Create event payload
cat > issue_event.json <<'EOF'
{
  "action": "opened",
  "issue": {
    "number": 1,
    "title": "Add unit tests for utils.js",
    "body": "## Task Description\nAdd comprehensive unit tests...",
    "user": {
      "login": "testuser"
    },
    "labels": []
  },
  "repository": {
    "name": "test-repo",
    "owner": {
      "login": "testorg"
    }
  }
}
EOF

# Run workflow with act
act issues \
  -e issue_event.json \
  -W .github/workflows/copilot-automation.yml
```

**Result:** Workflow triggers, but GitHub API calls fail unless mocked.

---

## 5. Mock Scaffolding Needed

### 5.1 GitHub API Mock Server

**Purpose:** Provide endpoints for `github.rest.*` calls.

**Options:**

| Tool | Description | Complexity |
|------|-------------|-----------|
| **Prism** | OpenAPI mock server | Medium - needs GitHub OpenAPI spec |
| **WireMock** | HTTP mock server | High - manual endpoint mapping |
| **gh-api-mock** | Custom Express server | Low - purpose-built |

**Recommendation:** Custom Express.js server that:
- Listens on `http://localhost:3000`
- Implements minimal endpoints:
  - `POST /repos/:owner/:repo/issues/:number/comments`
  - `POST /repos/:owner/:repo/issues/:number/labels`
  - `POST /repos/:owner/:repo/issues/:number/assignees`
  - `POST /repos/:owner/:repo/pulls`
- Returns success responses with mock data
- Logs all calls for verification

**Example stub:**
```javascript
// mock-github-api.js
const express = require('express');
const app = express();
app.use(express.json());

const state = {
  issues: {},
  prs: {}
};

app.post('/repos/:owner/:repo/issues/:number/comments', (req, res) => {
  const { owner, repo, number } = req.params;
  const { body } = req.body;

  if (!state.issues[number]) {
    state.issues[number] = { comments: [] };
  }
  state.issues[number].comments.push(body);

  console.log(`[API] Comment added to issue #${number}: ${body.substring(0, 50)}...`);
  res.json({ id: Date.now(), body });
});

app.post('/repos/:owner/:repo/issues/:number/labels', (req, res) => {
  const { number } = req.params;
  const { labels } = req.body;

  if (!state.issues[number]) {
    state.issues[number] = { labels: [] };
  }
  state.issues[number].labels.push(...labels);

  console.log(`[API] Labels added to issue #${number}: ${labels.join(', ')}`);
  res.json({ labels });
});

app.post('/repos/:owner/:repo/pulls', (req, res) => {
  const { title, head, base, body } = req.body;
  const prNumber = Object.keys(state.prs).length + 1;

  state.prs[prNumber] = { title, head, base, body };

  console.log(`[API] PR created: #${prNumber} - ${title}`);
  res.json({ number: prNumber, html_url: `http://github.com/test/repo/pull/${prNumber}` });
});

app.listen(3000, () => console.log('GitHub API mock running on :3000'));
```

**Integration with act:**
```bash
# Start mock server
node mock-github-api.js &

# Configure act to use mock
export GITHUB_API_URL=http://localhost:3000

# Run workflow
act issues -e issue_event.json
```

### 5.2 Mock @copilot Agent

**Problem:** These workflows expect a @copilot agent to:
1. Receive task assignments
2. Generate code/documentation
3. Create pull requests

**Solution Options:**

| Approach | Complexity | Fidelity |
|----------|-----------|----------|
| **No-op stub** | Low | Just log assignments, don't act |
| **Template responder** | Medium | Return canned responses |
| **LLM agent** | High | Actually invoke Claude/GPT |

**Recommendation for evaluation:** **Template responder**

**Why:** Evaluator is assessing whether the workflow correctly:
- Triggers on the right events
- Assigns tasks properly
- Creates PRs with correct metadata
- Updates labels/status

The evaluator doesn't need to test whether @copilot writes good code (that's a separate concern).

**Implementation:**
```bash
#!/bin/bash
# mock-copilot-agent.sh

# Watch for files in a "copilot-queue" directory
# When a task file appears, simulate copilot work

QUEUE_DIR="./copilot-queue"
WORK_DIR="./copilot-workspace"

mkdir -p "$QUEUE_DIR" "$WORK_DIR"

while true; do
  for task_file in "$QUEUE_DIR"/*.json; do
    if [ -f "$task_file" ]; then
      echo "[COPILOT] Processing task: $task_file"

      # Parse task (issue number, title, body)
      ISSUE_NUM=$(jq -r '.issue.number' "$task_file")
      ISSUE_TITLE=$(jq -r '.issue.title' "$task_file")

      # Simulate work (create a file)
      mkdir -p "$WORK_DIR/issue-$ISSUE_NUM"
      cat > "$WORK_DIR/issue-$ISSUE_NUM/implementation.md" <<EOF
# Implementation for Issue #$ISSUE_NUM

## Task
$ISSUE_TITLE

## Changes
- Added feature X
- Updated documentation
EOF

      # Create a branch and PR (simulated)
      git checkout -b "copilot/issue-$ISSUE_NUM" 2>/dev/null || true
      git add "$WORK_DIR/issue-$ISSUE_NUM"
      git commit -m "feat: implement issue #$ISSUE_NUM" || true

      # Signal completion
      rm "$task_file"
      echo "[COPILOT] Completed issue #$ISSUE_NUM"
    fi
  done
  sleep 2
done
```

**Workflow integration:**
- Workflows write task files to `copilot-queue/`
- Mock agent picks them up and creates artifacts
- Workflows verify artifacts exist

### 5.3 Local Git Remote

**Problem:** `git push` fails in `act` because there's no remote repository.

**Solution:** Create a bare Git repository as a mock remote.

```bash
# Create bare repo
mkdir -p /tmp/mock-git-remote
cd /tmp/mock-git-remote
git init --bare test-repo.git

# Configure local repo to use it
cd /path/to/test-workspace
git remote add origin /tmp/mock-git-remote/test-repo.git
git push origin main

# Now workflows can push branches
git push origin copilot/issue-123  # Works!
```

**Verification:**
```bash
# Check what branches were pushed
git -C /tmp/mock-git-remote/test-repo.git branch -a
```

### 5.4 Issue/PR State Tracking

**Problem:** Multiple workflows modify issue state (labels, assignees, comments). Need to track state changes.

**Solution:** Shared JSON state file.

```bash
# state.json (initialized)
{
  "issues": {},
  "prs": {}
}
```

**Update via jq:**
```bash
# Add label to issue #1
jq '.issues["1"].labels += ["copilot"]' state.json > state.tmp && mv state.tmp state.json

# Add comment to issue #1
jq '.issues["1"].comments += ["Processing..."]' state.json > state.tmp && mv state.tmp state.json
```

**Verification script:**
```bash
#!/bin/bash
# verify-state.sh

# Check if issue #1 has the correct labels
LABELS=$(jq -r '.issues["1"].labels | join(",")' state.json)
if [[ "$LABELS" == *"copilot"* ]] && [[ "$LABELS" == *"processing"* ]]; then
  echo "✓ Issue #1 has correct labels"
else
  echo "✗ Issue #1 missing expected labels: $LABELS"
  exit 1
fi

# Check if PR was created
PR_COUNT=$(jq '.prs | length' state.json)
if [ "$PR_COUNT" -gt 0 ]; then
  echo "✓ PR was created"
else
  echo "✗ No PR was created"
  exit 1
fi
```

### 5.5 Complete Test Harness

**Full scaffolding architecture:**

```
┌─────────────────────────────────────────────────────────────┐
│ Test Harness (bash script)                                  │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  1. Setup phase:                                            │
│     - Start mock GitHub API server (Express.js)            │
│     - Start mock @copilot agent (bash loop)                │
│     - Create bare git remote                                │
│     - Initialize state.json                                 │
│     - Configure environment variables                       │
│                                                              │
│  2. Execution phase:                                        │
│     - Generate issue event JSON                             │
│     - Run workflow with `act`                               │
│     - Wait for completion                                   │
│                                                              │
│  3. Verification phase:                                     │
│     - Check state.json for expected changes                 │
│     - Verify files were created                             │
│     - Validate git history (branch, commit, push)           │
│     - Check mock API logs                                   │
│                                                              │
│  4. Teardown phase:                                         │
│     - Stop mock servers                                     │
│     - Clean up temporary files                              │
│     - Generate test report                                  │
│                                                              │
└─────────────────────────────────────────────────────────────┘

         ▲                              ▲
         │                              │
         │                              │
    GitHub Actions                  @copilot
    Mock API Server                Mock Agent
    (Express.js)                   (bash loop)
         │                              │
         └──────────────┬───────────────┘
                        │
                   state.json
```

**Example harness script:**
```bash
#!/bin/bash
# test-harness.sh

set -euo pipefail

SOLUTION_DIR="$1"  # e.g., P1-S2-opus
TEST_ISSUE_TITLE="Add unit tests for utils.js"

echo "=== Test Harness for $SOLUTION_DIR ==="

# 1. Setup
echo "[1/4] Setup phase..."
node mock-github-api.js > api.log 2>&1 &
API_PID=$!
./mock-copilot-agent.sh > copilot.log 2>&1 &
COPILOT_PID=$!
git init --bare /tmp/mock-remote.git
git remote add origin /tmp/mock-remote.git || true
echo '{"issues":{}, "prs":{}}' > state.json

# 2. Execution
echo "[2/4] Running workflow..."
cat > issue_event.json <<EOF
{
  "action": "opened",
  "issue": {
    "number": 1,
    "title": "$TEST_ISSUE_TITLE",
    "body": "## Task Description\nAdd comprehensive tests...",
    "user": {"login": "testuser"},
    "labels": []
  }
}
EOF

cd "$SOLUTION_DIR"
act issues -e ../issue_event.json -W .github/workflows/*.yml > ../act.log 2>&1 || true
cd ..

# 3. Verification
echo "[3/4] Verifying results..."
EXIT_CODE=0

# Check if workflow triggered
if grep -q "Job succeeded" act.log; then
  echo "✓ Workflow completed successfully"
else
  echo "✗ Workflow failed or did not complete"
  EXIT_CODE=1
fi

# Check if labels were added
if jq -e '.issues["1"].labels | contains(["copilot"])' state.json >/dev/null; then
  echo "✓ Issue labeled with 'copilot'"
else
  echo "✗ Issue was not labeled"
  EXIT_CODE=1
fi

# Check if PR was created
if [ "$(jq '.prs | length' state.json)" -gt 0 ]; then
  echo "✓ Pull request was created"
else
  echo "✗ No pull request was created"
  EXIT_CODE=1
fi

# 4. Teardown
echo "[4/4] Cleanup..."
kill $API_PID $COPILOT_PID 2>/dev/null || true
rm -rf /tmp/mock-remote.git

echo "=== Test complete: $([ $EXIT_CODE -eq 0 ] && echo "PASS" || echo "FAIL") ==="
exit $EXIT_CODE
```

---

## 6. Assumptions We Can Make

### 6.1 Common Patterns Across Solutions

Based on analysis of all 26 workflows:

1. **All solutions use GitHub Actions** - No other automation mechanism (no Jenkins, GitLab CI, etc.)
2. **Most trigger on `issues.opened` or `issues.labeled`** - Standard pattern
3. **All use `actions/github-script@v7` for API calls** - Consistent approach
4. **Many create branches named `copilot/issue-N`** - Naming convention
5. **Most use `github-actions[bot]` as commit author** - Standard identity
6. **Knowledge base stored in `docs/knowledge/`** - Common structure
7. **None actually invoke a real @copilot API** - All simulate in some way

### 6.2 What We DON'T Need to Mock

1. **Syntax validation tools** (yamllint, shellcheck, markdownlint)
   - These are real tools that work in `act`
   - Solutions that validate syntax can do so without mocking

2. **File system operations**
   - Creating directories, writing files works natively

3. **Shell script execution**
   - bash, python, node scripts execute normally

4. **Git operations (except push)**
   - commit, checkout, branch work locally

### 6.3 Simplifying Assumptions for Evaluation

**Focus on workflow structure, not runtime behavior:**

The evaluator can assess many qualities **statically** without running in `act`:

| Criterion | Static Analysis | Requires Execution |
|-----------|----------------|-------------------|
| YAML syntax valid | yamllint | No |
| Workflow triggers on correct event | Parse `on:` block | No |
| Uses correct GitHub API endpoints | Parse script blocks | No |
| Creates expected files | Check file paths in code | Partially (verify structure) |
| Handles errors | Check for `|| true`, try/catch | Partially (detect presence) |
| Has placeholders | Grep for TODO, @owner | No |
| Follows naming conventions | Check branch/label names | No |

**For full functional testing, run in `act` with mocks.**

**For evaluation scoring, static analysis covers 70-80% of criteria.**

---

## 7. Evaluation Rubric Mapping

**From SIMULATION_ALGORITHM.md (lines 119-146):**

| Rubric Dimension | How to Assess | Requires Mocking? |
|-----------------|---------------|-------------------|
| **COMPLETENESS (30 pts)** | | |
| File coverage | Count expected vs actual files | No - static analysis |
| Content depth | Check for placeholder text | No - grep/parse |
| **CORRECTNESS (25 pts)** | | |
| Syntax validity | yamllint, shellcheck | No - run validators |
| Semantic correctness | Would it work? | **Yes** - needs execution |
| **ACTIONABILITY (20 pts)** | | |
| Ready to use | Test end-to-end | **Yes** - needs execution |
| **SPECIFICITY (15 pts)** | | |
| Placeholder density | Count TODO, FIXME | No - static analysis |
| Contextual appropriateness | Manual review | No - human judgment |
| **INSIGHT QUALITY (10 pts)** | | |
| Novel approaches | Identify unique patterns | No - code review |
| Assumptions stated | Check for comments | No - static analysis |

**Conclusion:** ~50 points (Completeness + Specificity + Insight) can be assessed **without running workflows**. The remaining 50 points (Correctness + Actionability) require execution with mocks.

---

## 8. Recommendations for Evaluator Agent

### 8.1 Two-Phase Evaluation Strategy

**Phase 1: Static Analysis (Fast)**
- Parse all YAML files with yamllint
- Check shell scripts with shellcheck
- Count files created vs expected
- Detect placeholders (TODO, @owner, FIXME)
- Verify workflow triggers and event types
- Score: COMPLETENESS, SPECIFICITY, INSIGHT (55 points)

**Phase 2: Execution Testing (Slower)**
- Set up mock scaffolding (API server, @copilot agent, git remote)
- Run workflows with `act` using test event payloads
- Verify state transitions (labels, PRs, commits)
- Score: CORRECTNESS, ACTIONABILITY (45 points)

**Total: 100 points**

### 8.2 Minimal Mock Scaffolding

**What evaluator MUST have to test execution:**

1. **GitHub API mock** (Express.js, ~100 LOC)
   - Endpoints: comments, labels, assignees, pulls
   - Returns success responses
   - Logs calls for verification

2. **@copilot agent stub** (bash script, ~50 LOC)
   - Watches queue directory
   - Creates placeholder artifacts
   - Signals completion

3. **Git remote** (bare repo, 1 command)
   - `git init --bare /tmp/mock-remote.git`

4. **State tracker** (JSON file + jq)
   - Records API calls
   - Enables verification

**Total: ~150 lines of scaffolding code**

### 8.3 Test Cases for Each Solution

**Standard test scenario:**
```yaml
Given:
  - Empty repository
  - User creates issue #1: "Add unit tests for utils.js"
  - Issue body follows template structure

When:
  - Issue is created (webhook fires)
  - Workflow runs

Then:
  - Issue is labeled with "copilot"
  - Issue is labeled with "processing" or "ready"
  - Comment is posted acknowledging receipt
  - Branch is created (copilot/issue-1)
  - Commit is made with implementation
  - PR is created linking to issue #1
  - PR is assigned to issue creator
  - Knowledge base is updated (if applicable)
```

**Verification points:**
- [ ] Workflow triggered on correct event
- [ ] Labels added to issue
- [ ] Comment posted to issue
- [ ] Branch created with expected name
- [ ] Commit made with expected message
- [ ] PR created with correct title/body
- [ ] PR links to issue ("Closes #1")
- [ ] Files created in expected locations

### 8.4 Failure Modes to Detect

**Common issues found in solutions:**

1. **Missing @copilot invocation mechanism**
   - Workflow mentions @copilot in comments but doesn't trigger agent
   - **Detection:** Check if solution explains how @copilot receives tasks

2. **Placeholder values**
   - `@owner` in CODEOWNERS without instructions
   - **Detection:** Grep for `@owner`, `TODO`, `<replace>`

3. **Hardcoded assumptions**
   - Assumes repository is named "myproject"
   - **Detection:** Check for hardcoded strings

4. **Missing error handling**
   - `git push` without `|| true` or error check
   - **Detection:** Parse scripts for unsafe commands

5. **Simulated validation**
   - Echoes "Validation: PASS" instead of running validators
   - **Detection:** Check if yamllint/shellcheck are actually invoked

6. **Race conditions**
   - Multiple jobs modifying same issue without coordination
   - **Detection:** Check for job dependencies (`needs:`)

---

## 9. Example Evaluation Flow

**For solution: P2-S2-sonnet**

```bash
# Phase 1: Static Analysis
yamllint P2-S2-sonnet/.github/workflows/*.yml
# → PASS (syntax valid)

grep -r "TODO\|FIXME\|@owner" P2-S2-sonnet/
# → FOUND: @owner in CODEOWNERS (placeholder detected)

count_files P2-S2-sonnet/
# → Expected: 8 files (workflow, template, knowledge base)
# → Actual: 7 files (missing scripts/)
# → Coverage: 87.5% (26.25/30 points for COMPLETENESS)

# Phase 2: Execution Testing
./test-harness.sh P2-S2-sonnet

# Test output:
# ✓ Workflow triggered on issues.opened
# ✓ Issue labeled with "copilot-task"
# ✓ Issue labeled with "copilot-processing"
# ✓ Comment posted to issue
# ✓ Copilot agent simulation ran
# ✓ Branch created: copilot/issue-1
# ✓ Commit made: "feat: implement task from issue #1"
# ✓ PR created: #1
# ✗ PR not assigned (API mock doesn't track assignees)
# ✓ Knowledge base updated (issues.jsonl appended)

# CORRECTNESS: 20/25 (workflow executes, but edge case with assignees)
# ACTIONABILITY: 17/20 (mostly ready, but needs @owner replacement)

# Total Score: 26.25 + 12 + 20 + 17 + 8 = 83.25/100 (B)
```

---

## 10. Key Takeaways

### For the Evaluator Agent

1. **These are GitHub Actions workflows**, not standalone scripts
2. **Most solutions simulate @copilot** because real API doesn't exist
3. **Correctness = "would this work in production?"** not just "does YAML parse?"
4. **~50 points can be assessed statically**, ~50 points need execution
5. **Mock scaffolding is simple** (~150 LOC for minimal harness)
6. **Focus on workflow logic**, not @copilot's code quality

### For Running in `act`

1. **Mock GitHub API** (Express.js server, 4-5 endpoints)
2. **Mock @copilot agent** (bash script that creates placeholder files)
3. **Mock git remote** (bare repository)
4. **State tracker** (JSON file to verify API calls)

### Critical Gap to Watch For

**From self-reflections in simulation runs:**

> "Would this meet success criteria in reality?"
> - Simulation: Yes (100% pass)
> - Production: No (~25% functional)
>
> Gaps:
> - No actual @copilot trigger
> - Placeholder values
> - No error handling

**Evaluator MUST distinguish:**
- ✓ "This workflow would trigger correctly"
- vs
- ✗ "This workflow mentions @copilot but has no invocation mechanism"

---

## 11. Appendix: File Inventory

**Solutions with .github/workflows/:**
```
P1-S1-haiku, P1-S1-opus, P1-S1-sonnet
P1-S2-haiku, P1-S2-opus, P1-S2-sonnet
P1-S3-haiku, P1-S3-opus, P1-S3-sonnet
P2-S1-haiku, P2-S1-opus, P2-S1-sonnet
P2-S2-haiku, P2-S2-opus, P2-S2-sonnet, P2-S2-sonnet-CONTROL, P2-S2-sonnet-TEST
P2-S3-haiku, P2-S3-opus, P2-S3-sonnet
P3-S1-haiku, P3-S1-opus, P3-S1-sonnet
P3-S2-haiku, P3-S2-opus, P3-S2-sonnet
P3-S3-haiku, P3-S3-opus, P3-S3-sonnet
```

**Total workflow files:** 26

**Solutions with scripts/:** 4
- P1-S3-sonnet (8 shell scripts)
- P3-S2-sonnet (1 shell script)
- P1-S1-haiku (1 shell script)

**Solutions with CODEOWNERS:** 18

**Solutions with knowledge base (docs/knowledge/):** 22

---

## References

- **SUCCESS_CRITERIA.md** - Observable outcomes for bootstrap system
- **SIMULATION_ALGORITHM.md** - Evaluation rubric and methodology
- **SIMULATION_HARNESS.md** - Three-phase testing process
- **act documentation** - https://github.com/nektos/act
- **GitHub Actions API** - https://docs.github.com/en/rest
- **Example workflow:** `/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-sonnet/.github/workflows/copilot-issue-agent.yml`
