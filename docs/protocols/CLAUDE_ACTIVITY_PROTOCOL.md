# Claude Code Activity Protocol

## How I (Claude Code) Track and Use Activities

This is my internal protocol for reliably managing activity context throughout conversations.

## Core State Variables

I maintain these variables during every conversation:

```python
# Activity state (persisted mentally across messages)
current_activity: str | None = None           # "main", "exploring-idea-x", etc.
current_activity_path: str = REPO_ROOT        # Absolute path to worktree
activities_initialized: bool = False          # Has system been initialized?
```

## Session Initialization (First Message)

**On conversation start, I MUST:**

```python
# 1. Check if activity system exists
result = bash("./scripts/activity-context init-check")

if result.strip() == "yes":
    # 2. System exists - get current activity
    self.activities_initialized = True
    self.current_activity = bash("./scripts/activity current").strip()
    self.current_activity_path = bash("./scripts/activity path").strip()

    # 3. Log internally (don't spam user)
    internal_log(f"✓ Activity context loaded: {self.current_activity}")
else:
    # 4. System not initialized
    self.activities_initialized = False
    self.current_activity = None
    self.current_activity_path = REPO_ROOT

    internal_log("✓ Activities not initialized (will use main repo)")
```

## State Update Triggers

**I MUST update my activity state when:**

1. **User creates new activity**
   ```python
   # After: ./scripts/activity create <name>
   self.current_activity = <name>
   self.current_activity_path = bash("./scripts/activity path").strip()
   ```

2. **User switches activity**
   ```python
   # After: ./scripts/activity switch <name>
   self.current_activity = <name>
   self.current_activity_path = bash("./scripts/activity path").strip()
   ```

3. **User mentions specific activity**
   ```python
   # User: "Work on X in activity Y"
   # After: ./scripts/activity switch Y
   self.current_activity = "Y"
   self.current_activity_path = bash("./scripts/activity path").strip()
   ```

4. **Error or state mismatch detected**
   ```python
   # If file operation fails, re-sync state
   actual_current = bash("./scripts/activity current").strip()
   if actual_current != self.current_activity:
       self.current_activity = actual_current
       self.current_activity_path = bash("./scripts/activity path").strip()
       internal_log(f"⚠ State synced: {actual_current}")
   ```

## File Operation Protocol

### Reading Files

**Before EVERY Read() call:**

```python
def read_file(file_path: str) -> str:
    # 1. Resolve to absolute path in current activity
    absolute_path = self._resolve_path(file_path)

    # 2. Read
    return Read(absolute_path)

def _resolve_path(self, file_path: str) -> str:
    """Convert relative path to absolute path in current activity."""

    # If already absolute, use as-is
    if file_path.startswith("/"):
        return file_path

    # Get base path (current activity or main repo)
    if self.current_activity and self.current_activity != "main":
        base_path = self.current_activity_path
    else:
        base_path = REPO_ROOT

    # Join and return
    return os.path.join(base_path, file_path)
```

### Writing Files

**Same pattern as reading:**

```python
def write_file(file_path: str, content: str) -> None:
    absolute_path = self._resolve_path(file_path)
    Write(absolute_path, content)
```

### Editing Files

**Same pattern:**

```python
def edit_file(file_path: str, old_string: str, new_string: str) -> None:
    absolute_path = self._resolve_path(file_path)
    Edit(absolute_path, old_string, new_string)
```

### Bash Commands

**Set working directory for activity-specific commands:**

```python
def run_bash_in_activity(command: str) -> str:
    # Get current activity path
    cwd = self.current_activity_path or REPO_ROOT

    # Run with correct CWD
    # Note: Bash tool resets CWD between calls, so pass absolute paths
    # or use cd && command if needed
    return Bash(f"cd {cwd} && {command}")
```

## User Request Pattern Matching

### Pattern 1: Create Activity

**User says (examples):**
- "Create a worktree for this"
- "I want to explore X in a separate branch"
- "Start new activity for Y"

**My response:**

```python
def handle_create_activity(user_request):
    # 1. Extract or suggest name
    name = self._suggest_activity_name(user_request)
    # e.g., "exploring-new-architecture"

    # 2. Extract description
    description = self._extract_description(user_request) or f"Activity: {name}"

    # 3. Create activity
    result = bash(f'./scripts/activity create {name} "{description}"')

    if "Created activity" in result:
        # 4. Update state
        self.current_activity = name
        self.current_activity_path = bash("./scripts/activity path").strip()

        # 5. Confirm to user
        respond(f"Created and switched to activity: {name}")
        respond(f"Working in: {self.current_activity_path}")
        respond("All file operations will use this worktree.")
    else:
        respond(f"Failed to create activity: {result}")
```

### Pattern 2: Switch Activity

**User says (examples):**
- "Switch to main"
- "Go to activity X"
- "Work in my experiment branch"

**My response:**

```python
def handle_switch_activity(activity_name):
    # 1. Validate exists
    result = bash(f"./scripts/activity show {activity_name}")

    if "Error" in result:
        respond(f"Activity '{activity_name}' doesn't exist.")
        bash("./scripts/activity list")
        return

    # 2. Switch
    bash(f"./scripts/activity switch {activity_name}")

    # 3. Update state
    self.current_activity = activity_name
    self.current_activity_path = bash("./scripts/activity path").strip()

    # 4. Confirm
    respond(f"Switched to activity: {activity_name}")
```

### Pattern 3: List Activities

**User says (examples):**
- "What activities do I have?"
- "List my worktrees"
- "Show activities"

**My response:**

```python
def handle_list_activities():
    # Simply show output (no state change)
    result = bash("./scripts/activity list")
    respond(result)
```

### Pattern 4: Work in Specific Activity

**User says (examples):**
- "Read README in activity X"
- "Create file in worktree Y"
- "Show files in my experiment"

**My response:**

```python
def handle_work_in_activity(task, activity_name):
    # 1. Switch to activity
    bash(f"./scripts/activity switch {activity_name}")

    # 2. Update state
    self.current_activity = activity_name
    self.current_activity_path = bash("./scripts/activity path").strip()

    # 3. Do task (will use updated state)
    perform_task(task)

    # 4. Confirm context
    respond(f"[Working in activity: {activity_name}]")
```

### Pattern 5: Background Agent in Activity

**User says (examples):**
- "Run cleanup agent in activity X"
- "Launch analysis in my experiment"

**My response:**

```python
def handle_background_agent(agent_task, activity_name):
    # 1. Switch to activity
    bash(f"./scripts/activity switch {activity_name}")

    # 2. Update state
    self.current_activity = activity_name
    self.current_activity_path = bash("./scripts/activity path").strip()

    # 3. Build agent prompt with context
    agent_prompt = f"""
    [ACTIVITY CONTEXT]
    Activity: {activity_name}
    Working Directory: {self.current_activity_path}

    IMPORTANT: All file paths should be relative to {self.current_activity_path}
    or use absolute paths starting with {self.current_activity_path}.

    [TASK]
    {agent_task}
    """

    # 4. Launch agent
    # Note: Set CWD before launching
    bash(f"cd {self.current_activity_path}")
    Task(agent_prompt)

    # 5. Confirm
    respond(f"Launched agent in activity: {activity_name}")
```

## Context Awareness in Responses

**Always include activity context when relevant:**

```python
def respond_with_context(message: str):
    # If working in non-main activity, prefix with context
    if self.current_activity and self.current_activity != "main":
        context_note = f"[Activity: {self.current_activity}] "
        respond(context_note + message)
    else:
        respond(message)
```

## Error Handling

### Activity Doesn't Exist

```python
try:
    bash(f"./scripts/activity switch {name}")
except:
    respond(f"Activity '{name}' doesn't exist.")
    respond("Available activities:")
    bash("./scripts/activity list")
```

### Activity System Not Initialized

```python
if not self.activities_initialized:
    respond("Activity system not initialized.")
    respond("Initialize with: ./scripts/activity init")
    respond("Or I can initialize it now if you'd like.")
```

### File Not Found

```python
try:
    Read(absolute_path)
except FileNotFoundError:
    respond(f"File not found: {absolute_path}")
    respond(f"Current activity: {self.current_activity}")
    respond(f"Working directory: {self.current_activity_path}")
    respond("Did you mean a different file or activity?")
```

### State Mismatch

```python
# If internal state doesn't match reality
def verify_state():
    if not self.activities_initialized:
        return

    actual_current = bash("./scripts/activity current").strip()
    if actual_current != self.current_activity:
        # State mismatch - resync
        self.current_activity = actual_current
        self.current_activity_path = bash("./scripts/activity path").strip()

        internal_log(f"⚠ State synced to: {actual_current}")
```

## Best Practices (My Rules)

### DO

✅ **Verify activity state at session start**
```python
# First thing in every conversation
initialize_activity_state()
```

✅ **Always resolve to absolute paths**
```python
# Never use relative paths without context
absolute_path = self._resolve_path(file_path)
```

✅ **Update state immediately after switches**
```python
# After any switch command
self.current_activity = new_activity
self.current_activity_path = get_activity_path()
```

✅ **Include context in agent prompts**
```python
# Agents need to know where they are
prompt = f"Working in activity: {name}\nBase: {path}\n{task}"
```

✅ **Confirm activity switches to user**
```python
# User needs to know context changed
respond(f"Switched to activity: {name}")
```

### DON'T

❌ **Don't assume relative paths**
```python
# BAD: Which activity's src/?
Read("src/file.py")

# GOOD: Explicit absolute path
Read(f"{self.current_activity_path}/src/file.py")
```

❌ **Don't forget to update state**
```python
# BAD: State out of sync
bash("./scripts/activity switch new-activity")
# ... forget to update self.current_activity ...

# GOOD: Always update
bash("./scripts/activity switch new-activity")
self.current_activity = "new-activity"
self.current_activity_path = bash("./scripts/activity path").strip()
```

❌ **Don't work in wrong worktree**
```python
# BAD: Forgot to switch
# User asked for activity X, but I'm still in main

# GOOD: Switch first
bash(f"./scripts/activity switch {requested_activity}")
self.current_activity = requested_activity
```

❌ **Don't launch agents without context**
```python
# BAD: Agent doesn't know where it is
Task("Do something")

# GOOD: Full context
Task(f"Activity: {name}\nPath: {path}\nTask: Do something")
```

## Self-Verification Checklist

**Before responding to user, I verify:**

- [ ] Is activity system initialized? (check once per session)
- [ ] Do I know current activity? (self.current_activity)
- [ ] Do I know current path? (self.current_activity_path)
- [ ] If user mentioned activity, did I switch to it?
- [ ] Are my file paths absolute or properly resolved?
- [ ] If launching agent, does it have activity context?
- [ ] Did I confirm context changes to user?

## Example Full Workflow

**User:** "I want to explore a new architecture idea. Create a worktree for it and add a design doc."

**My internal process:**

```python
# 1. Parse request
task = "explore new architecture idea"
needs_worktree = True
needs_file = "design doc"

# 2. Suggest activity name
activity_name = "architecture-exploration"  # slug format

# 3. Create activity
result = bash(f'./scripts/activity create {activity_name} "Exploring new architecture patterns"')

# 4. Update state
self.current_activity = activity_name
self.current_activity_path = bash("./scripts/activity path").strip()

# 5. Confirm to user
respond(f"Created activity: {activity_name}")
respond(f"Working in: {self.current_activity_path}")

# 6. Create file (will use updated state)
file_path = self._resolve_path("ARCHITECTURE_DESIGN.md")
# = "/Users/bln/play/agentic-primer/.wt/architecture-exploration/ARCHITECTURE_DESIGN.md"

content = """
# Architecture Exploration

## Context
...
"""

Write(file_path, content)

# 7. Confirm to user
respond(f"Created ARCHITECTURE_DESIGN.md in activity: {activity_name}")
respond("You can now work on this idea in isolation from main.")
```

## Summary: My Commitment

**I will ALWAYS:**

1. **Initialize** activity state at session start
2. **Track** current activity in my mental state
3. **Resolve** file paths to absolute paths in current activity
4. **Update** state immediately after switches
5. **Include** context in background agent prompts
6. **Verify** state if operations fail
7. **Confirm** context changes to user

**This ensures:**
- No cross-activity contamination
- No wrong-directory errors
- No state confusion
- No lost work
- Clean, predictable behavior

**Golden Rule:** When in doubt, verify:
```bash
./scripts/activity current
./scripts/activity path
```

This is my protocol. I will follow it reliably.
