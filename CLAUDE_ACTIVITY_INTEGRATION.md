# Claude Code Activity Integration Guide

## How Claude Code Uses Activities

This document describes **how Claude Code (the AI assistant) reliably tracks and uses the activity worktree system** to ensure context-aware file operations and background agent execution.

## Core Principles

1. **Always verify activity context before file operations**
2. **Use absolute paths from activity worktree**
3. **Track current activity state in conversation**
4. **Switch activities explicitly when user requests**
5. **Launch background agents in correct worktree**

## Session Initialization

### On Session Start

```python
# Pseudo-code for Claude's session initialization

def initialize_session():
    # Check if activity system exists
    result = bash("./scripts/activity-context init-check")

    if result.strip() == "yes":
        # Get current activity
        current = bash("./scripts/activity current").strip()
        activity_path = bash("./scripts/activity path").strip()

        # Set internal state
        self.current_activity = current
        self.current_activity_path = activity_path

        # Inform user
        log(f"Working in activity: {current}")
        if current != "main":
            log(f"Worktree path: {activity_path}")
    else:
        # Not initialized
        self.current_activity = None
        self.current_activity_path = REPO_ROOT

        # Don't spam user, but be ready to initialize if requested
```

## Context Tracking

### Internal State Variables

Claude maintains these variables during conversation:

```python
class ActivityContext:
    current_activity: str | None = None      # e.g., "exploring-idea-x" or "main"
    current_activity_path: str = REPO_ROOT   # Absolute path to worktree
    activities_initialized: bool = False     # Is system initialized?
```

### When to Update Context

**Update context when:**
1. Session starts
2. User creates new activity
3. User switches activity
4. User mentions activity by name
5. User says "work on <task> in <activity>"

**Don't update context when:**
- User just asks about activities (read-only query)
- User shows activity info (read-only)
- Working in main repo and no activity mentioned

## File Operations Strategy

### Reading Files

**Before reading any file:**

```python
def read_file_in_activity(file_path: str) -> str:
    # If activity is active, use activity path as base
    if self.current_activity and self.current_activity != "main":
        base_path = self.current_activity_path
    else:
        base_path = REPO_ROOT

    # If file_path is relative, make it absolute
    if not file_path.startswith("/"):
        absolute_path = os.path.join(base_path, file_path)
    else:
        absolute_path = file_path

    return Read(absolute_path)
```

**Example:**

```python
# User: "Read README.md in architecture-exploration activity"

# Step 1: Switch activity
bash("./scripts/activity switch architecture-exploration")
self.current_activity = "architecture-exploration"
self.current_activity_path = bash("./scripts/activity path").strip()

# Step 2: Read file with absolute path
content = Read(f"{self.current_activity_path}/README.md")
```

### Writing Files

**Always use activity-aware absolute paths:**

```python
def write_file_in_activity(file_path: str, content: str) -> None:
    # Resolve to activity worktree
    if self.current_activity and self.current_activity != "main":
        base_path = self.current_activity_path
    else:
        base_path = REPO_ROOT

    # Make absolute
    if not file_path.startswith("/"):
        absolute_path = os.path.join(base_path, file_path)
    else:
        absolute_path = file_path

    Write(absolute_path, content)
```

### Editing Files

**Same pattern as writing:**

```python
def edit_file_in_activity(file_path: str, old_string: str, new_string: str) -> None:
    # Get absolute path in activity
    base_path = self.current_activity_path or REPO_ROOT

    if not file_path.startswith("/"):
        absolute_path = os.path.join(base_path, file_path)
    else:
        absolute_path = file_path

    Edit(absolute_path, old_string, new_string)
```

## User Request Patterns

### Pattern 1: "Create worktree for X"

**User says:**
- "Create a worktree for this idea"
- "I want to explore X in a separate worktree"
- "Start new activity for Y"

**Claude does:**

```python
def handle_create_activity(user_request):
    # 1. Extract/suggest activity name
    suggested_name = extract_or_suggest_name(user_request)
    # e.g., "exploring-new-architecture"

    # 2. Ask for confirmation (if needed)
    # "I'll create activity 'exploring-new-architecture'. Proceed?"

    # 3. Create activity
    result = bash(f"./scripts/activity create {suggested_name} \"{description}\"")

    # 4. Update internal state
    self.current_activity = suggested_name
    self.current_activity_path = bash("./scripts/activity path").strip()

    # 5. Confirm to user
    respond(f"Created activity '{suggested_name}'")
    respond(f"Working in: {self.current_activity_path}")
    respond("All file operations will now use this worktree.")
```

### Pattern 2: "Switch to activity X"

**User says:**
- "Switch to main"
- "Work in architecture-exploration activity"
- "Go back to my experiment worktree"

**Claude does:**

```python
def handle_switch_activity(activity_name):
    # 1. Validate activity exists
    exists = bash(f"./scripts/activity show {activity_name}")
    if not exists:
        respond(f"Activity '{activity_name}' doesn't exist")
        list_activities()
        return

    # 2. Switch
    bash(f"./scripts/activity switch {activity_name}")

    # 3. Update internal state
    self.current_activity = activity_name
    self.current_activity_path = bash("./scripts/activity path").strip()

    # 4. Confirm to user
    respond(f"Switched to activity: {activity_name}")
    if activity_name != "main":
        respond(f"Working in: {self.current_activity_path}")
```

### Pattern 3: "What activities do I have?"

**User says:**
- "List activities"
- "What worktrees are active?"
- "Show my activities"

**Claude does:**

```python
def handle_list_activities():
    # Simply run and show output
    output = bash("./scripts/activity list")
    respond(output)

    # No state change needed (read-only)
```

### Pattern 4: "Do X in activity Y"

**User says:**
- "Create README.md in architecture-exploration"
- "Run analysis in experiment-123"
- "Show files in my exploration worktree"

**Claude does:**

```python
def handle_task_in_activity(task, activity_name):
    # 1. Switch to activity
    bash(f"./scripts/activity switch {activity_name}")
    self.current_activity = activity_name
    self.current_activity_path = bash("./scripts/activity path").strip()

    # 2. Do task using activity path
    perform_task_with_base_path(task, self.current_activity_path)

    # 3. Confirm context
    respond(f"[Working in activity: {activity_name}]")
```

### Pattern 5: "Run agent in activity X"

**User says:**
- "Run cleanup agent in experiment-123"
- "Launch analysis in architecture-exploration"

**Claude does:**

```python
def handle_background_agent_in_activity(agent_task, activity_name):
    # 1. Switch to activity
    bash(f"./scripts/activity switch {activity_name}")
    activity_path = bash("./scripts/activity path").strip()

    # 2. Update state
    self.current_activity = activity_name
    self.current_activity_path = activity_path

    # 3. Launch agent with correct CWD
    bash(f"cd {activity_path}")

    # 4. In agent prompt, specify:
    agent_prompt = f"""
    You are working in activity: {activity_name}
    Working directory: {activity_path}

    All file operations should use {activity_path} as the base directory.

    Task: {agent_task}
    """

    # 5. Launch Task agent
    Task(agent_prompt, cwd=activity_path)
```

## Background Agent Integration

### Problem

Background agents (via Task tool) need to:
1. Know which activity they're working in
2. Use correct base path for file operations
3. Not accidentally work in wrong worktree

### Solution

**When launching background agent:**

```python
def launch_background_agent(task_description):
    # Get current activity context
    activity_name = self.current_activity or "main"
    activity_path = self.current_activity_path or REPO_ROOT

    # Build activity-aware prompt
    agent_prompt = f"""
    [ACTIVITY CONTEXT]
    Activity: {activity_name}
    Working Directory: {activity_path}

    IMPORTANT: All file paths should be relative to {activity_path}
    or use absolute paths starting with {activity_path}

    [TASK]
    {task_description}
    """

    # Launch with correct CWD
    # Note: Task tool inherits CWD from parent process
    bash(f"cd {activity_path}")
    Task(agent_prompt)
```

### Agent Self-Verification

**Agents should verify context on start:**

```bash
# In agent initialization
echo "Verifying activity context..."
pwd  # Should be activity worktree path

EXPECTED_PATH=$(./scripts/activity path)
CURRENT_PATH=$(pwd)

if [ "$CURRENT_PATH" != "$EXPECTED_PATH" ]; then
    echo "ERROR: Wrong working directory!"
    echo "Expected: $EXPECTED_PATH"
    echo "Current: $CURRENT_PATH"
    exit 1
fi

echo "Working in activity: $(./scripts/activity current)"
```

## Edge Cases & Error Handling

### Edge Case 1: Activity System Not Initialized

**Scenario:** User asks about activities but system not initialized

**Handler:**

```python
if not bash("./scripts/activity-context init-check") == "yes":
    respond("Activity system not initialized yet.")
    respond("Would you like me to initialize it?")
    respond("I'll run: ./scripts/activity init")

    if user_confirms():
        bash("./scripts/activity init")
        respond("Activity system initialized!")
```

### Edge Case 2: Activity Doesn't Exist

**Scenario:** User tries to switch to non-existent activity

**Handler:**

```python
try:
    bash(f"./scripts/activity switch {name}")
except:
    respond(f"Activity '{name}' doesn't exist.")
    respond("Here are your current activities:")
    bash("./scripts/activity list")
    respond("Create new activity: ./scripts/activity create <name>")
```

### Edge Case 3: User Works Across Multiple Activities

**Scenario:** User switches activities frequently

**Handler:**

```python
# Always re-verify context before file operations
def ensure_correct_context():
    # Check if our internal state matches reality
    actual_current = bash("./scripts/activity current").strip()

    if actual_current != self.current_activity:
        # State mismatch, sync up
        self.current_activity = actual_current
        self.current_activity_path = bash("./scripts/activity path").strip()

        log(f"Synced activity context: {actual_current}")
```

### Edge Case 4: File Path Ambiguity

**Scenario:** User says "read src/main.py" but multiple activities have this file

**Handler:**

```python
def handle_ambiguous_file_path(file_path):
    if not file_path.startswith("/"):
        # Relative path - use current activity
        current = self.current_activity or "main"

        if current != "main":
            confirm(f"Reading {file_path} from activity '{current}'. Correct?")

        base_path = self.current_activity_path or REPO_ROOT
        absolute_path = os.path.join(base_path, file_path)
    else:
        # Absolute path - use as-is
        absolute_path = file_path

    return absolute_path
```

## Best Practices for Claude

### DO

✅ **Always verify current activity before file operations**
```python
current = bash("./scripts/activity current")
```

✅ **Use absolute paths from activity worktree**
```python
path = bash("./scripts/activity path")
absolute = f"{path}/src/file.py"
```

✅ **Confirm activity context to user**
```python
respond(f"[Working in activity: {current}]")
```

✅ **Switch activity explicitly when user requests**
```python
bash(f"./scripts/activity switch {name}")
```

✅ **Include activity context in agent prompts**
```python
prompt = f"Working in activity: {name}\nBase path: {path}\n..."
```

### DON'T

❌ **Don't assume relative paths without context**
```python
# BAD: Which activity's src/?
Read("src/file.py")

# GOOD: Explicit activity path
Read(f"{activity_path}/src/file.py")
```

❌ **Don't work in wrong worktree**
```python
# BAD: Forgetting to switch
# Still in main, but user asked for work in activity-x

# GOOD: Switch first
bash("./scripts/activity switch activity-x")
```

❌ **Don't lose track of current activity**
```python
# BAD: State gets out of sync

# GOOD: Update state after every switch
self.current_activity = new_activity
```

❌ **Don't launch agents without activity context**
```python
# BAD: Agent doesn't know where it is
Task("do something")

# GOOD: Agent has full context
Task(f"Working in {path}\nTask: do something")
```

## Testing Activity Integration

### Self-Test Checklist

Before deploying activity-aware behavior, Claude should verify:

- [ ] Can detect if system is initialized
- [ ] Can get current activity name
- [ ] Can get activity worktree path
- [ ] Can switch between activities
- [ ] Can create new activities
- [ ] Can list activities
- [ ] Can read files from activity worktree
- [ ] Can write files to activity worktree
- [ ] Can launch background agents in activity
- [ ] Can handle "not initialized" error gracefully
- [ ] Can handle "activity doesn't exist" error gracefully

### Manual Test Script

```bash
#!/usr/bin/env bash
# test-activity-integration.sh - Test Claude's activity integration

set -euo pipefail

echo "=== Activity Integration Test ==="

# Test 1: Check initialized
echo "Test 1: Check if initialized"
./scripts/activity-context init-check

# Test 2: Get current activity
echo "Test 2: Get current activity"
./scripts/activity current

# Test 3: Get activity path
echo "Test 3: Get activity path"
./scripts/activity path

# Test 4: Create test activity
echo "Test 4: Create test activity"
./scripts/activity create integration-test "Testing integration"

# Test 5: Switch activities
echo "Test 5: Switch to main"
./scripts/activity switch main

echo "Test 5b: Switch back to test"
./scripts/activity switch integration-test

# Test 6: Verify path changes
echo "Test 6: Verify path"
EXPECTED_PATH="/Users/bln/play/agentic-primer/.wt/integration-test"
ACTUAL_PATH=$(./scripts/activity path)

if [ "$EXPECTED_PATH" = "$ACTUAL_PATH" ]; then
    echo "✓ Path correct: $ACTUAL_PATH"
else
    echo "✗ Path mismatch!"
    echo "  Expected: $EXPECTED_PATH"
    echo "  Actual: $ACTUAL_PATH"
    exit 1
fi

# Test 7: Create file in activity
echo "Test 7: Create file in activity"
ACTIVITY_PATH=$(./scripts/activity path)
echo "Test content" > "$ACTIVITY_PATH/TEST_FILE.md"

if [ -f "$ACTIVITY_PATH/TEST_FILE.md" ]; then
    echo "✓ File created in activity worktree"
else
    echo "✗ File creation failed"
    exit 1
fi

# Test 8: Clean up
echo "Test 8: Clean up"
./scripts/activity switch main
./scripts/activity remove integration-test --force

echo ""
echo "=== All Tests Passed ==="
```

## Summary

**For Claude Code to reliably use activities:**

1. **Initialize session:** Check if system exists, get current activity
2. **Track state:** Maintain `current_activity` and `current_activity_path`
3. **Switch explicitly:** When user requests or mentions activity
4. **Use absolute paths:** Always resolve to `{activity_path}/{file}`
5. **Context in prompts:** Include activity info in background agents
6. **Verify often:** Re-check context if operations fail

**Golden Rule:** When in doubt, verify current activity with:
```bash
./scripts/activity current
./scripts/activity path
```

This ensures Claude always knows where it's working and prevents cross-activity contamination.
