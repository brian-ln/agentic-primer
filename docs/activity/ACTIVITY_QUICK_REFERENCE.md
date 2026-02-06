# Activity Worktree Quick Reference

## For Users

### Getting Started
```bash
# Initialize system (first time only)
./scripts/activity init

# Create new activity
./scripts/activity create my-activity "Description of what I'm exploring"

# List all activities
./scripts/activity list

# Switch between activities
./scripts/activity switch my-activity
./scripts/activity switch main

# Show current activity
./scripts/activity current
./scripts/activity show

# Remove activity when done
./scripts/activity remove my-activity
```

### Common Workflows

#### Start New Exploration
```bash
./scripts/activity create exploration-x "Exploring new idea X"
# Creates: .wt/exploration-x/ with branch feature/exploration-x
# Automatically switches to new activity
```

#### Work in Multiple Activities
```bash
# Create activities
./scripts/activity create feature-a "Feature A development"
./scripts/activity create feature-b "Feature B development"

# Switch between them
./scripts/activity switch feature-a
# ... do work ...

./scripts/activity switch feature-b
# ... do work ...

# Back to main
./scripts/activity switch main
```

#### Clean Up Completed Work
```bash
# Archive (keeps worktree)
./scripts/activity archive old-activity

# Remove completely (deletes worktree)
./scripts/activity remove old-activity
```

## For Claude Code

### Check Activity Context

**At session start or when user mentions activity work:**

```bash
# Check if system is initialized
./scripts/activity-context init-check

# Get current activity context
eval "$(./scripts/activity-context)"
# Now you have:
# $ACTIVITY_NAME, $ACTIVITY_PATH, $ACTIVITY_BRANCH, $ACTIVITY_DESCRIPTION
```

### Working in Activity Worktree

**When reading/writing files:**

```bash
# Get activity path
ACTIVITY_PATH=$(./scripts/activity path)

# Read files from activity
cat "$ACTIVITY_PATH/README.md"

# Write files to activity
echo "content" > "$ACTIVITY_PATH/newfile.txt"
```

**Using Read/Write/Edit tools:**

```python
# Get activity path first
activity_path = bash("./scripts/activity path")

# Then use absolute paths with Read/Write/Edit
read_file(f"{activity_path}/README.md")
write_file(f"{activity_path}/newfile.txt", content)
edit_file(f"{activity_path}/existing.txt", old, new)
```

### Background Agents in Activities

**Launch agent in specific activity:**

```bash
# Switch to activity first
./scripts/activity switch my-activity

# Get activity path
ACTIVITY_PATH=$(./scripts/activity path)

# Launch agent with correct working directory
cd "$ACTIVITY_PATH"
# Use Task tool or background agent here

# In agent prompt, specify:
# "You are working in activity: my-activity"
# "Working directory: $ACTIVITY_PATH"
# "Use this as base path for all file operations"
```

### User Communication Patterns

**When user says:** "Create a worktree for this"

**Claude should:**
1. Suggest activity name (slug format)
2. Ask for confirmation/description
3. Run: `./scripts/activity create <name> "<description>"`
4. Confirm: "Created and switched to activity: <name>"

**When user says:** "Switch to <activity>"

**Claude should:**
1. Run: `./scripts/activity switch <name>`
2. Confirm: "Switched to activity: <name>"
3. Update internal context to use activity path

**When user says:** "What activities do I have?"

**Claude should:**
1. Run: `./scripts/activity list`
2. Show formatted output
3. Highlight current activity

**When user says:** "Work on <task> in <activity>"

**Claude should:**
1. Run: `./scripts/activity switch <activity>`
2. Get path: `./scripts/activity path`
3. Do work using activity path as base
4. Confirm activity context in responses

## Integration Examples

### Example 1: Session Start

```bash
# Claude checks activity system status
if [ -f /Users/bln/play/agentic-primer/.wt/.activities.json ]; then
    current=$(./scripts/activity current)
    path=$(./scripts/activity path)
    echo "Working in activity: $current ($path)"
else
    echo "Activity system not initialized"
    echo "Suggest: ./scripts/activity init"
fi
```

### Example 2: Create and Use Activity

```bash
# User: "I want to explore a new architecture idea"

# Claude:
./scripts/activity create architecture-exploration "Exploring new architecture patterns"

# Get context
ACTIVITY_PATH=$(./scripts/activity path)

# Create files in activity
cat > "$ACTIVITY_PATH/ARCHITECTURE_IDEAS.md" <<EOF
# Architecture Exploration
...
EOF

# Report to user
echo "Created activity 'architecture-exploration'"
echo "Working in: $ACTIVITY_PATH"
```

### Example 3: Background Agent in Activity

```bash
# User: "Run analysis agent in architecture-exploration activity"

# Claude:
./scripts/activity switch architecture-exploration
ACTIVITY_PATH=$(./scripts/activity path)

cd "$ACTIVITY_PATH"

# Launch agent (using Task tool or similar)
# Agent prompt includes:
# "Working in activity: architecture-exploration"
# "Base path: $ACTIVITY_PATH"
# "All file operations relative to this path"
```

### Example 4: List and Switch

```bash
# User: "What activities do I have?"

# Claude:
./scripts/activity list

# Output:
# ACTIVITY                  BRANCH                         STATUS     LAST ACCESSED
# --------                  ------                         ------     -------------
# * main                     genesis                        active     30m ago
#   architecture-exploration feature/architecture-exploration active  1h ago
#   test-exploration         feature/test-exploration       active     2h ago

# User: "Switch to architecture-exploration"

# Claude:
./scripts/activity switch architecture-exploration

# Output:
# ✓ Switched to activity: architecture-exploration
# ℹ Working directory: /Users/bln/play/agentic-primer/.wt/architecture-exploration
```

## File Path Resolution

**Golden Rule:** Always use absolute paths from activity worktree

```bash
# CORRECT
ACTIVITY_PATH=$(./scripts/activity path)
read "$ACTIVITY_PATH/src/main.py"

# WRONG (relative paths can be ambiguous)
read "src/main.py"  # Which activity's src/?
```

## Activity Naming Conventions

**Good names:**
- `exploring-idea-x`
- `feature-authentication`
- `experiment-performance`
- `refactor-database`
- `bugfix-123`

**Bad names:**
- `Exploring Idea X` (no spaces)
- `FEATURE_AUTH` (no uppercase)
- `temp` (too vague)
- `stuff` (too vague)

## Environment Variables

When working in activities, these are available:

```bash
ACTIVITY_NAME       # Current activity name
ACTIVITY_PATH       # Absolute path to worktree
ACTIVITY_CWD        # Same as ACTIVITY_PATH (for compatibility)
ACTIVITY_BRANCH     # Git branch for this activity
ACTIVITY_DESCRIPTION # Human description
```

## Troubleshooting

### Activity system not initialized
```bash
Error: Activities not initialized
# Fix:
./scripts/activity init
```

### Activity doesn't exist
```bash
Error: Activity 'foo' does not exist
# Fix:
./scripts/activity list  # See available activities
```

### Worktree has uncommitted changes
```bash
# When trying to remove activity with uncommitted changes
git -C /path/to/worktree status
git -C /path/to/worktree stash  # Save changes
# Or commit changes before removing
```

### Wrong activity context
```bash
# Always verify current activity
./scripts/activity current

# Switch if needed
./scripts/activity switch <correct-activity>
```

## Advanced Usage

### Get Activity Metadata Programmatically

```bash
# Using jq to query metadata
jq -r '.activities["my-activity"]' .wt/.activities.json

# Get specific field
jq -r '.activities["my-activity"].branch' .wt/.activities.json

# List all active activities
jq -r '.activities | to_entries[] | select(.value.status == "active") | .key' .wt/.activities.json
```

### Scripting with Activities

```bash
#!/usr/bin/env bash
# Example: Run command in all active activities

for activity in $(./scripts/activity list active | tail -n +3 | awk '{print $2}'); do
    echo "Processing activity: $activity"
    path=$(./scripts/activity path "$activity")
    cd "$path"
    # Run your command here
done
```

## See Also

- **Full Documentation:** `ACTIVITY_WORKTREE_SYSTEM.md`
- **Git Worktree Docs:** `man git-worktree`
- **Source Code:** `scripts/activity`, `scripts/activity-lib.sh`
