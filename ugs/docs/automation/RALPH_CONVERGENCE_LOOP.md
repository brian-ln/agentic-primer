# Ralph Convergence Loop

A hybrid of Ralph Wiggum's persistent iteration with structured reflection and adaptation.

---

## Core Concept

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│   OUTER LOOP (Convergence)                                      │
│   ┌───────────────────────────────────────────────────────────┐ │
│   │                                                           │ │
│   │   INNER LOOP (Ralph)                                      │ │
│   │   ┌─────────────────────────────────────────────────────┐ │ │
│   │   │  while iterations < MAX_INNER:                      │ │ │
│   │   │      feed PROMPT.md → agent works → check done?     │ │ │
│   │   │      if COMPLETE: exit both loops                   │ │ │
│   │   └─────────────────────────────────────────────────────┘ │ │
│   │                         │                                 │ │
│   │                    not complete                           │ │
│   │                         ▼                                 │ │
│   │   ┌─────────────────────────────────────────────────────┐ │ │
│   │   │  REFLECT: Why stuck? What patterns?                 │ │ │
│   │   │  ADAPT: Update PROMPT.md based on lessons           │ │ │
│   │   │  INCREMENT: outer_iteration++                       │ │ │
│   │   └─────────────────────────────────────────────────────┘ │ │
│   │                         │                                 │ │
│   │                         └──────────── loop ───────────────┘ │
│   │                                                           │ │
│   └───────────────────────────────────────────────────────────┘ │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Key insight**: Ralph retries the same prompt. We add a reflection checkpoint that **adapts the prompt** when stuck.

---

## File Structure

```
task/
├── PROMPT.md           # Current task spec (updated by reflection)
├── PROMPT.history/     # Previous versions for learning
│   ├── v0.md
│   ├── v1.md
│   └── ...
├── REFLECTION.md       # Cumulative reflection notes
├── VALIDATION.sh       # Success criteria as executable script
└── loop.sh             # The hybrid loop driver
```

---

## PROMPT.md Template

```markdown
# Task: [Title]
Bead: [bead-id]
Iteration: [N]

## What
- **Inputs**: [list]
- **Actions**: [concrete steps]
- **Outputs**: [artifacts]

## Success Criteria
Run `./VALIDATION.sh` - all checks must pass:
- [ ] criterion 1
- [ ] criterion 2

## Completion
When ALL criteria pass, output:
<promise>COMPLETE</promise>

## Context from Previous Iterations
[Added by reflection - what was tried, what failed, what to do differently]

---
## Work Log
[Agent appends progress here each iteration]
```

---

## VALIDATION.sh Template

```bash
#!/bin/bash
# Validation script for task
# Exit 0 = all pass, Exit 1 = some failed

TASK_ID="${BEAD_ID:-unknown}"
FAIL=0

echo "=== Validating $TASK_ID ==="

# Criterion 1: Tests pass
if bun test src/entities/program.test.ts 2>/dev/null; then
  echo "✓ Tests pass"
else
  echo "✗ Tests fail"
  FAIL=1
fi

# Criterion 2: CLI works
if ./ugs program create test-$$ --impl 'true' >/dev/null 2>&1; then
  echo "✓ CLI creates program"
else
  echo "✗ CLI fails"
  FAIL=1
fi

# Criterion 3: Events logged
if ./ugs events 2>/dev/null | grep -q PROGRAM_CREATED; then
  echo "✓ Events logged"
else
  echo "✗ No events"
  FAIL=1
fi

echo "=== Result: $([ $FAIL -eq 0 ] && echo PASS || echo FAIL) ==="
exit $FAIL
```

---

## loop.sh - The Hybrid Driver

```bash
#!/bin/bash
# Ralph Convergence Loop
# Combines Ralph's persistent iteration with reflection checkpoints

set -e

TASK_DIR="${1:-.}"
MAX_INNER="${MAX_INNER:-5}"      # Ralph iterations before reflection
MAX_OUTER="${MAX_OUTER:-10}"     # Max reflection cycles
BEAD_ID="${BEAD_ID:-}"

cd "$TASK_DIR"

outer=0
total_iterations=0

# Update beads state
update_state() {
  [ -n "$BEAD_ID" ] && bd set-state "$BEAD_ID" "$1" --reason "$2" 2>/dev/null || true
}

# Log to beads
log_comment() {
  [ -n "$BEAD_ID" ] && bd comments add "$BEAD_ID" "$1" 2>/dev/null || true
}

echo "=== Ralph Convergence Loop ==="
echo "Task: $TASK_DIR"
echo "Bead: ${BEAD_ID:-none}"
echo "Max inner: $MAX_INNER, Max outer: $MAX_OUTER"
echo ""

update_state "phase=executing" "Starting loop"

while [ $outer -lt $MAX_OUTER ]; do
  echo ">>> Outer iteration $outer (reflection cycle)"

  # Update prompt with current iteration
  sed -i.bak "s/^Iteration: .*/Iteration: $outer/" PROMPT.md

  inner=0
  completed=false

  # === INNER LOOP (Ralph) ===
  while [ $inner -lt $MAX_INNER ]; do
    total_iterations=$((total_iterations + 1))
    echo "  > Inner iteration $inner (total: $total_iterations)"

    # Feed prompt to Claude
    cat PROMPT.md | claude -p --max-turns 50 2>&1 | tee -a .iteration.log

    # Check completion
    if grep -q "<promise>COMPLETE</promise>" .iteration.log; then
      echo "  ✓ Completion promise found!"
      completed=true
      break
    fi

    # Run validation
    if ./VALIDATION.sh; then
      echo "  ✓ Validation passed!"
      completed=true
      break
    fi

    inner=$((inner + 1))
  done

  # === EXIT IF COMPLETE ===
  if $completed; then
    echo ""
    echo "=== TASK COMPLETE ==="
    echo "Total iterations: $total_iterations"
    echo "Outer cycles: $outer"

    update_state "phase=complete" "Completed after $total_iterations iterations"
    log_comment "Task completed in $total_iterations iterations ($outer reflection cycles)"

    [ -n "$BEAD_ID" ] && bd close "$BEAD_ID" 2>/dev/null || true
    exit 0
  fi

  # === REFLECT ===
  echo ""
  echo ">>> REFLECTION CHECKPOINT"
  update_state "phase=reflecting" "Stuck after $MAX_INNER iterations"

  # Archive current prompt
  mkdir -p PROMPT.history
  cp PROMPT.md "PROMPT.history/v$outer.md"

  # Generate reflection
  cat > .reflect_prompt.md << 'REFLECT_EOF'
# Reflection Task

Review the work done and generate insights for the next iteration.

## Analyze
1. Read PROMPT.md (the task spec)
2. Read .iteration.log (recent work)
3. Run ./VALIDATION.sh to see what's failing
4. Check git diff to see what was changed

## Output
Append to REFLECTION.md:

```markdown
## Iteration [N] Reflection

### What Was Attempted
- [list actions taken]

### What Failed
- [list specific failures from validation]

### Root Cause
- [why did it fail - be specific]

### Adaptation for Next Iteration
- [concrete changes to try]
```

Then update PROMPT.md:
1. Add a "## Context from Previous Iterations" section if missing
2. Add key insights about what NOT to do
3. Add hints about what TO try
4. Keep the core task spec unchanged

Output <promise>REFLECTED</promise> when done.
REFLECT_EOF

  cat .reflect_prompt.md | claude -p --max-turns 10 2>&1 | tee -a .reflection.log

  # Log reflection to beads
  if [ -f REFLECTION.md ]; then
    log_comment "$(tail -50 REFLECTION.md)"
  fi

  # === ADAPT ===
  echo ">>> Adapting prompt for next cycle"
  update_state "phase=specifying" "Adapting based on reflection"

  # Update iteration label in beads
  if [ -n "$BEAD_ID" ]; then
    bd update "$BEAD_ID" --remove-label "iteration:$outer" --add-label "iteration:$((outer+1))" 2>/dev/null || true
  fi

  outer=$((outer + 1))
  rm -f .iteration.log

  echo ""
done

# === MAX ITERATIONS REACHED ===
echo ""
echo "=== MAX OUTER ITERATIONS REACHED ==="
echo "Total iterations: $total_iterations"
update_state "phase=blocked" "Max iterations reached without completion"
log_comment "BLOCKED: Max iterations ($total_iterations) reached. Manual intervention needed."

exit 1
```

---

## Usage

### 1. Create task in beads

```bash
bd create "Implement Program entity" \
  --type task \
  --label convergence,iteration:0 \
  --description "..."
# Returns: agentic-primer-xyz
```

### 2. Set up task directory

```bash
mkdir -p tasks/agentic-primer-xyz
cd tasks/agentic-primer-xyz

# Create PROMPT.md with task spec
# Create VALIDATION.sh with success criteria
# Copy loop.sh
```

### 3. Run the loop

```bash
BEAD_ID=agentic-primer-xyz ./loop.sh
```

### 4. Monitor

```bash
# Watch beads state
bd show agentic-primer-xyz

# Watch iterations
tail -f tasks/agentic-primer-xyz/.iteration.log

# Check reflections
cat tasks/agentic-primer-xyz/REFLECTION.md
```

---

## Key Differences from Pure Ralph

| Aspect | Pure Ralph | Ralph Convergence |
|--------|-----------|-------------------|
| Prompt | Same every iteration | Adapts based on reflection |
| Failure handling | Just retry | Analyze why, then adapt |
| State tracking | None | Beads integration |
| History | Only in files | PROMPT.history + REFLECTION.md |
| Checkpoints | None | Every N iterations |
| Learning | Implicit (file state) | Explicit (reflection notes) |

---

## Integration with Beads

The loop automatically:
- Updates `phase` state dimension
- Tracks `iteration:N` labels
- Adds comments with progress/reflections
- Closes the bead on completion
- Marks blocked if max iterations reached

Query progress:
```bash
bd list --label convergence
bd show <bead-id>
bd comments <bead-id>
```

---

## Philosophy

> "Ralph is deterministically bad. We add deterministic learning."

- **Ralph's insight**: Persistence beats perfection. Keep trying.
- **Convergence insight**: Adapt based on failure patterns. Learn.
- **Combined**: Persistent iteration WITH intelligent adaptation.

The loop doesn't just retry—it reflects, adapts, and improves the approach each cycle.
