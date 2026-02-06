# Activity Worktree Management System

## Executive Summary

A lightweight, git-native system for managing parallel development activities using git worktrees. Each activity gets its own isolated workspace (`.wt/<activity-name>/`) with its own branch, tracked via simple JSON metadata that persists across sessions.

**Key Design Principles:**
- **Git-native**: Leverage git worktrees for isolation
- **Simple metadata**: JSON file for tracking (no complex database)
- **Session persistence**: Survives Claude Code restarts
- **Context-aware**: Tools and agents know which activity is active
- **No commits pollution**: `.wt/` is gitignored

## Architecture Overview

### Directory Structure

```
/Users/bln/play/agentic-primer/
├── .wt/                          # Git-ignored worktrees directory
│   ├── exploring-idea-x/         # Activity worktree
│   │   ├── .git                  # Git worktree metadata (file, not dir)
│   │   ├── src/                  # Full repo copy
│   │   └── ...
│   ├── feature-experiment/       # Another activity
│   └── .activities.json          # Activity metadata (THE SOURCE OF TRUTH)
├── scripts/
│   ├── activity                  # Main CLI tool
│   ├── activity-lib.sh           # Shared functions
│   └── ...
└── README.md
```

### Metadata Schema

**File:** `.wt/.activities.json`

```json
{
  "schema_version": "1.0",
  "current_activity": "main",
  "activities": {
    "main": {
      "name": "main",
      "description": "Main repository (not a worktree)",
      "worktree_path": "/Users/bln/play/agentic-primer",
      "branch": "genesis",
      "is_main": true,
      "created": "2026-01-10T12:00:00Z",
      "last_accessed": "2026-01-10T12:30:00Z",
      "status": "active"
    },
    "exploring-idea-x": {
      "name": "exploring-idea-x",
      "description": "Exploring idea X for feature Y",
      "worktree_path": "/Users/bln/play/agentic-primer/.wt/exploring-idea-x",
      "branch": "feature/exploring-idea-x",
      "is_main": false,
      "created": "2026-01-09T17:00:00Z",
      "last_accessed": "2026-01-09T18:30:00Z",
      "status": "active",
      "parent_branch": "main",
      "beads_issues": []
    }
  }
}
```

**Field Definitions:**
- `schema_version`: Metadata format version (for future migrations)
- `current_activity`: Name of the currently active activity
- `activities`: Map of activity name → activity object
- `name`: Unique activity identifier (slug format: lowercase-with-dashes)
- `description`: Human-readable description
- `worktree_path`: Absolute path to worktree directory
- `branch`: Git branch name for this activity
- `is_main`: Boolean indicating if this is the main repo (not a worktree)
- `created`: ISO 8601 timestamp
- `last_accessed`: ISO 8601 timestamp (updated on switch)
- `status`: `active` | `archived` | `completed`
- `parent_branch`: Branch this activity branched from (optional)
- `beads_issues`: Array of beads issue IDs associated with this activity (optional)

### Why This Design?

**Chosen: JSON Metadata File (`.wt/.activities.json`)**

Pros:
- ✅ Simple, human-readable, editable
- ✅ No dependencies (just `jq` for parsing)
- ✅ Easy to version control (if desired) or gitignore
- ✅ Survives session restarts
- ✅ Easy to backup/restore
- ✅ Works offline

Rejected alternatives:
- ❌ **Git notes**: Tied to commits, complex to query, not portable
- ❌ **Beads integration**: Too heavy, creates issues just for activities
- ❌ **Simple text file**: Hard to parse, no structure, error-prone
- ❌ **Environment variables**: Don't persist across sessions

### Context Tracking: The "Current Activity" Problem

**Challenge:** How does Claude Code know which activity is active?

**Solution:** Multi-layer approach:

1. **Session State File** (`.wt/.current-activity`)
   - Simple text file containing current activity name
   - Read at session start
   - Updated on activity switch

2. **Claude Code Environment**
   - Scripts export `ACTIVITY_CWD` environment variable
   - Points to current worktree absolute path
   - Used by background agents

3. **Metadata Sync**
   - `.activities.json` has `current_activity` field
   - Kept in sync with `.current-activity` file
   - Source of truth for queries

**Activity Switch Flow:**
```bash
# User: "Switch to activity exploring-idea-x"
./scripts/activity switch exploring-idea-x

# Script does:
1. Validate activity exists (check .activities.json)
2. Update .wt/.current-activity → "exploring-idea-x"
3. Update .activities.json → current_activity = "exploring-idea-x"
4. Update last_accessed timestamp
5. Export ACTIVITY_CWD=/Users/bln/play/agentic-primer/.wt/exploring-idea-x
6. Report: "Switched to activity: exploring-idea-x"
```

## CLI Interface Design

### Command Structure

```bash
./scripts/activity <command> [options] [args]
```

### Commands

#### 1. Create Activity
```bash
./scripts/activity create <name> [description]

# Examples:
./scripts/activity create exploring-idea-x "Exploring idea X"
./scripts/activity create feature-y
```

**Actions:**
1. Validate name (slug format, no spaces)
2. Create worktree: `git worktree add .wt/<name> -b feature/<name>`
3. Add entry to `.activities.json`
4. Set as current activity (optional: `--no-switch` flag)
5. Report success with worktree path

#### 2. List Activities
```bash
./scripts/activity list [--status=active|archived|all]

# Output:
# ACTIVITY               BRANCH                      STATUS    LAST ACCESSED
# * exploring-idea-x     feature/exploring-idea-x    active    2h ago
#   feature-y            feature/feature-y           active    1d ago
#   main                 genesis                     active    30m ago
# (* = current activity)
```

#### 3. Switch Activity
```bash
./scripts/activity switch <name>

# Examples:
./scripts/activity switch main
./scripts/activity switch exploring-idea-x
```

**Actions:**
1. Validate activity exists
2. Update current activity pointers
3. Update last_accessed timestamp
4. Report new working directory

#### 4. Show Activity Info
```bash
./scripts/activity show [name]

# If no name, shows current activity
# Output:
# Activity: exploring-idea-x
# Description: Exploring idea X for feature Y
# Worktree: /Users/bln/play/agentic-primer/.wt/exploring-idea-x
# Branch: feature/exploring-idea-x
# Created: 2026-01-09 17:00:00
# Last Accessed: 2 hours ago
# Status: active
```

#### 5. Current Activity
```bash
./scripts/activity current

# Output:
# exploring-idea-x
```

**Use case:** Scripts can use this to get current worktree path:
```bash
ACTIVITY_PATH=$(./scripts/activity path)
cd "$ACTIVITY_PATH"
```

#### 6. Get Activity Path
```bash
./scripts/activity path [name]

# Output:
# /Users/bln/play/agentic-primer/.wt/exploring-idea-x
```

#### 7. Archive Activity
```bash
./scripts/activity archive <name>

# Sets status to "archived", doesn't delete worktree
```

#### 8. Remove Activity
```bash
./scripts/activity remove <name> [--force]

# Actions:
1. Confirm with user (unless --force)
2. Remove git worktree: git worktree remove .wt/<name>
3. Delete branch (optional): git branch -d feature/<name>
4. Remove from .activities.json
5. If was current activity, switch to "main"
```

#### 9. Initialize System
```bash
./scripts/activity init

# First-time setup:
1. Create .wt/ directory
2. Add .wt/ to .gitignore
3. Create .activities.json with "main" entry
4. Create .current-activity file
```

## Background Agent Integration

### Problem Statement

When launching background agents via Task tool, they need to:
1. Know which worktree to work in
2. Have correct working directory
3. Operate on correct files

### Solution: `ACTIVITY_CWD` Environment Variable

**In agent launch script:**
```bash
# Get current activity path
ACTIVITY_PATH=$(./scripts/activity path)

# Launch agent with correct CWD
cd "$ACTIVITY_PATH"
task-agent run --cwd "$ACTIVITY_PATH" "agent instructions"
```

**In Claude Code prompt to agent:**
```
You are working in activity: exploring-idea-x
Working directory: /Users/bln/play/agentic-primer/.wt/exploring-idea-x
All file operations should use this as the base path.
```

### Example: Background Agent in Worktree

```bash
# User says: "Run cleanup agent in activity exploring-idea-x"

# Step 1: Switch to activity (if not already there)
./scripts/activity switch exploring-idea-x

# Step 2: Launch agent with correct context
ACTIVITY_PATH=$(./scripts/activity path)
cd "$ACTIVITY_PATH"

# Step 3: Use Task tool with CWD set
# (Claude Code task agent inherits CWD from parent process)
task cleanup-agent --context "Working in activity: exploring-idea-x"
```

## Claude Code Integration Strategy

### How Claude Stays Context-Aware

**Option 1: Prompt Augmentation (Recommended)**

When user mentions activity work, Claude should:

```python
# Pseudo-code for Claude's internal logic
if user_mentions_activity():
    current_activity = shell("./scripts/activity current")
    activity_path = shell("./scripts/activity path")

    # Update internal context
    set_working_directory(activity_path)

    # All subsequent Read/Write/Edit/Bash calls use activity_path
    # as base directory
```

**Option 2: Wrapper Functions**

Create wrapper scripts that Claude uses:

```bash
# scripts/activity-read <file>
# Reads file from current activity worktree
ACTIVITY_PATH=$(./scripts/activity path)
cat "$ACTIVITY_PATH/$1"

# scripts/activity-write <file> <content>
# Writes file to current activity worktree
ACTIVITY_PATH=$(./scripts/activity path)
echo "$2" > "$ACTIVITY_PATH/$1"
```

**Recommended:** Option 1 (Prompt Augmentation)

Simpler, no special tools needed, uses existing Read/Write/Edit tools.

### Session Start Behavior

```bash
# When Claude Code session starts:
1. Check if .wt/.activities.json exists
2. If yes:
   - Read current_activity from .activities.json
   - Set internal context to that activity's worktree path
   - Inform user: "Working in activity: <name>"
3. If no:
   - Default to main repo
   - Offer to run: ./scripts/activity init
```

### User Workflow Examples

#### Example 1: Create New Activity
```
User: "I want to explore a new idea about agent bootstrapping.
       Create a worktree for this."

Claude:
1. Suggests name: "agent-bootstrapping-exploration"
2. Runs: ./scripts/activity create agent-bootstrapping-exploration "Exploring agent bootstrapping"
3. Reports: "Created activity 'agent-bootstrapping-exploration' at .wt/agent-bootstrapping-exploration"
4. Switches to that activity
5. Confirms: "Now working in activity: agent-bootstrapping-exploration"
```

#### Example 2: Switch Activities
```
User: "Switch back to main work"

Claude:
1. Runs: ./scripts/activity switch main
2. Reports: "Switched to activity: main (main repository)"
3. Updates internal context to use main repo path
```

#### Example 3: Background Agent in Activity
```
User: "Run analysis agent in the bootstrapping activity"

Claude:
1. Runs: ./scripts/activity switch agent-bootstrapping-exploration
2. Gets path: /Users/bln/play/agentic-primer/.wt/agent-bootstrapping-exploration
3. Launches agent with CWD set to that path
4. Agent operates on files in that worktree
```

#### Example 4: List Activities
```
User: "What activities do I have going?"

Claude:
1. Runs: ./scripts/activity list
2. Shows table of activities with status
3. Highlights current activity
```

## Implementation Plan

### Phase 1: Core Infrastructure (30 min)

1. **Create `.wt/` directory structure**
   - `mkdir -p .wt`
   - Add `.wt/` to `.gitignore` (or create if doesn't exist)

2. **Create `scripts/activity-lib.sh`**
   - Shared functions for metadata manipulation
   - JSON parsing/writing (using `jq`)
   - Validation functions
   - Path resolution

3. **Create `scripts/activity` main CLI**
   - Dispatch to subcommands
   - Help text
   - Error handling

4. **Implement `init` command**
   - Creates `.wt/` directory
   - Initializes `.activities.json` with "main" entry
   - Creates `.current-activity` file
   - Adds `.wt/` to `.gitignore`

### Phase 2: Core Commands (45 min)

5. **Implement `create` command**
   - Validate name
   - Create git worktree
   - Add to metadata
   - Switch to new activity

6. **Implement `list` command**
   - Read metadata
   - Format table output
   - Show current activity indicator

7. **Implement `switch` command**
   - Validate activity exists
   - Update current activity pointers
   - Update timestamps

8. **Implement `current` and `path` commands**
   - Simple metadata reads
   - Used by other scripts

### Phase 3: Management Commands (30 min)

9. **Implement `show` command**
   - Display activity details
   - Human-friendly timestamps

10. **Implement `archive` command**
    - Update status field
    - Keep worktree intact

11. **Implement `remove` command**
    - Remove git worktree
    - Delete branch (optional)
    - Remove from metadata
    - Handle "current activity" edge case

### Phase 4: Testing & Documentation (15 min)

12. **Create test activity**
    - Demonstrate system works
    - Validate metadata persistence

13. **Update main README.md**
    - Add section on activity management
    - Link to this document

14. **Create usage examples**
    - Common workflows
    - Integration patterns

## File Structure

```
/Users/bln/play/agentic-primer/
├── .wt/                              # Git-ignored worktrees
│   ├── .activities.json              # Metadata (THE SOURCE OF TRUTH)
│   ├── .current-activity             # Current activity name
│   └── <activity-name>/              # Activity worktrees
├── .gitignore                        # Contains: .wt/
├── scripts/
│   ├── activity                      # Main CLI (executable)
│   ├── activity-lib.sh               # Shared library functions
│   └── ...
├── ACTIVITY_WORKTREE_SYSTEM.md       # This document
└── README.md                         # Updated with activity mgmt info
```

## Technical Details

### JSON Schema Validation

```bash
# Validate .activities.json structure
function validate_activities_json() {
    local json_file="$1"

    # Check schema_version exists
    if ! jq -e '.schema_version' "$json_file" >/dev/null; then
        echo "Error: Missing schema_version"
        return 1
    fi

    # Check current_activity exists
    if ! jq -e '.current_activity' "$json_file" >/dev/null; then
        echo "Error: Missing current_activity"
        return 1
    fi

    # Check activities is an object
    if ! jq -e '.activities | type == "object"' "$json_file" >/dev/null; then
        echo "Error: activities must be an object"
        return 1
    fi

    return 0
}
```

### Activity Name Validation

```bash
# Valid activity names: lowercase, numbers, dashes only
function validate_activity_name() {
    local name="$1"

    # Check format
    if ! [[ "$name" =~ ^[a-z0-9-]+$ ]]; then
        echo "Error: Activity name must be lowercase letters, numbers, and dashes only"
        return 1
    fi

    # Check reserved names
    if [[ "$name" == "main" || "$name" == "current" || "$name" == "list" ]]; then
        echo "Error: '$name' is a reserved name"
        return 1
    fi

    return 0
}
```

### Atomic Metadata Updates

```bash
# Atomic write to prevent corruption
function update_activities_json() {
    local json_content="$1"
    local json_file=".wt/.activities.json"
    local temp_file=".wt/.activities.json.tmp"

    # Write to temp file
    echo "$json_content" > "$temp_file"

    # Validate JSON
    if ! jq . "$temp_file" >/dev/null 2>&1; then
        echo "Error: Invalid JSON"
        rm "$temp_file"
        return 1
    fi

    # Atomic move
    mv "$temp_file" "$json_file"
}
```

### Timestamp Utilities

```bash
# ISO 8601 timestamp
function now_iso8601() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# Human-readable time ago
function time_ago() {
    local timestamp="$1"
    local now_epoch=$(date +%s)
    local then_epoch=$(date -j -f "%Y-%m-%dT%H:%M:%SZ" "$timestamp" +%s 2>/dev/null || echo "0")
    local diff=$((now_epoch - then_epoch))

    if [ "$diff" -lt 60 ]; then
        echo "${diff}s ago"
    elif [ "$diff" -lt 3600 ]; then
        echo "$((diff / 60))m ago"
    elif [ "$diff" -lt 86400 ]; then
        echo "$((diff / 3600))h ago"
    else
        echo "$((diff / 86400))d ago"
    fi
}
```

## Success Criteria Checklist

- ✅ **Can create worktrees in `.wt/` directory**
  - `./scripts/activity create <name>` creates worktree

- ✅ **`.wt/` is gitignored**
  - Added to `.gitignore` on init

- ✅ **Activities are tracked reliably (survives session restart)**
  - `.activities.json` persists metadata
  - `.current-activity` tracks active activity

- ✅ **Can switch between activities**
  - `./scripts/activity switch <name>` changes context

- ✅ **Background agents can work in specific worktrees**
  - `ACTIVITY_CWD` environment variable
  - Claude uses activity path as base directory

- ✅ **No confusion about which worktree is active**
  - `./scripts/activity current` shows active activity
  - `.current-activity` file is source of truth

- ✅ **Clean, simple interface for user**
  - Single `activity` command with subcommands
  - Clear error messages
  - Human-readable output

## Edge Cases & Error Handling

### Edge Case 1: Activity Already Exists
```bash
./scripts/activity create existing-activity
# Error: Activity 'existing-activity' already exists
```

### Edge Case 2: Switch to Non-Existent Activity
```bash
./scripts/activity switch nonexistent
# Error: Activity 'nonexistent' does not exist
# Run './scripts/activity list' to see available activities
```

### Edge Case 3: Remove Current Activity
```bash
./scripts/activity remove current-activity
# Warning: You are removing the currently active activity.
# You will be switched to 'main' after removal.
# Continue? [y/N]
```

### Edge Case 4: Worktree Directory Deleted Manually
```bash
# User deleted .wt/activity-name/ manually
./scripts/activity list
# Shows activity but marks as "MISSING WORKTREE"
# Offer to clean up: ./scripts/activity clean
```

### Edge Case 5: Metadata Corruption
```bash
# .activities.json is corrupted
./scripts/activity list
# Error: Metadata file is corrupted
# Backup found at: .wt/.activities.json.backup
# Restore? [y/N]
```

### Edge Case 6: Concurrent Access
```bash
# Multiple Claude sessions modifying metadata
# Solution: Use file locking or atomic writes
# If conflict detected, show warning and latest state
```

## Future Enhancements

### Phase 2 Features (Future)

1. **Beads Integration**
   - Link activities to beads issues
   - `./scripts/activity link-bead <activity> <bead-id>`
   - Auto-create beads for activities

2. **Stash/Unstash Between Activities**
   - Auto-stash when switching activities
   - `./scripts/activity switch --stash`

3. **Activity Templates**
   - Pre-configured activity setups
   - `./scripts/activity create-from-template <template-name> <activity-name>`

4. **Activity Dependencies**
   - Track which activities depend on others
   - Block removal of activities with dependencies

5. **Activity Metrics**
   - Time spent in each activity
   - Commits per activity
   - Files changed per activity

6. **Activity Sync**
   - Sync activities across machines
   - Export/import activity metadata

7. **Interactive Mode**
   - TUI for managing activities
   - Visual activity switcher

## Appendix: Alternative Designs Considered

### Alt 1: Environment Variable Only
**Rejected:** Doesn't persist across sessions

### Alt 2: Git Config
```bash
git config activity.current "exploring-idea-x"
```
**Rejected:** Pollutes git config, harder to query

### Alt 3: Symlink `.wt/current` → `.wt/activity-name/`
**Rejected:** Symlinks can be fragile, harder to track metadata

### Alt 4: Database (SQLite)
**Rejected:** Overkill for simple key-value storage

### Alt 5: Each Activity Has Own Metadata File
```
.wt/activity-name/.activity-metadata.json
```
**Rejected:** Harder to query all activities, fragmented data

## Conclusion

This system provides a **simple, git-native, session-persistent way to manage parallel development activities** using worktrees. It solves the core problems:

1. ✅ Isolated workspaces (git worktrees)
2. ✅ Reliable tracking (JSON metadata)
3. ✅ Context awareness (current activity pointers)
4. ✅ Background agent support (ACTIVITY_CWD)
5. ✅ Clean UX (single CLI command)

The implementation is **pragmatic**: uses standard tools (git, jq, bash), stores data in simple formats (JSON), and provides clear interfaces for both users and automation.

**Next Steps:** Implement the core scripts and test with a proof-of-concept activity.
