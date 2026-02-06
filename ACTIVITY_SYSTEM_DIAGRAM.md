# Activity Worktree System - Architecture Diagram

## System Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         User (Developer)                            │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                │ Commands
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      CLI: ./scripts/activity                        │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  Commands: init | create | list | switch | current | path   │  │
│  │            show | archive | remove | help                    │  │
│  └──────────────────────────────────────────────────────────────┘  │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                │ Uses
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                   Library: scripts/activity-lib.sh                  │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  • Metadata manipulation (JSON via jq)                       │  │
│  │  • Activity validation                                       │  │
│  │  • Git worktree operations                                   │  │
│  │  • Timestamp utilities                                       │  │
│  │  • Display formatting                                        │  │
│  └──────────────────────────────────────────────────────────────┘  │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                │ Manages
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    Metadata: .wt/.activities.json                   │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  {                                                           │  │
│  │    "schema_version": "1.0",                                  │  │
│  │    "current_activity": "exploring-idea-x",                   │  │
│  │    "activities": {                                           │  │
│  │      "main": { ... },                                        │  │
│  │      "exploring-idea-x": {                                   │  │
│  │        "worktree_path": ".wt/exploring-idea-x",             │  │
│  │        "branch": "feature/exploring-idea-x",                │  │
│  │        "created": "2026-01-10T12:00:00Z",                   │  │
│  │        "status": "active",                                   │  │
│  │        ...                                                   │  │
│  │      }                                                        │  │
│  │    }                                                          │  │
│  │  }                                                            │  │
│  └──────────────────────────────────────────────────────────────┘  │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                │ Points to
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         Git Worktrees                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  Main Repo:     /Users/bln/play/agentic-primer/             │  │
│  │  (branch: genesis)                                           │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  Activity 1:    .wt/exploring-idea-x/                        │  │
│  │  (branch: feature/exploring-idea-x)                          │  │
│  │    ├── src/                                                  │  │
│  │    ├── README.md                                             │  │
│  │    └── ... (full repo copy)                                  │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  Activity 2:    .wt/experiment-123/                          │  │
│  │  (branch: feature/experiment-123)                            │  │
│  │    ├── src/                                                  │  │
│  │    ├── README.md                                             │  │
│  │    └── ... (full repo copy)                                  │  │
│  └──────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

## Claude Code Integration

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Claude Code (AI Assistant)                      │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  Internal State:                                             │  │
│  │    • current_activity: "exploring-idea-x"                    │  │
│  │    • current_activity_path: "/path/to/.wt/exploring-idea-x" │  │
│  │    • activities_initialized: true                            │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  Session Start:                                              │  │
│  │    1. Check: ./scripts/activity-context init-check           │  │
│  │    2. Load:  ./scripts/activity current                      │  │
│  │    3. Load:  ./scripts/activity path                         │  │
│  │    4. Set internal state                                     │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  File Operations:                                            │  │
│  │    Read(file) → Resolve to activity path → Read absolute    │  │
│  │    Write(file, content) → Resolve → Write absolute          │  │
│  │    Edit(file, old, new) → Resolve → Edit absolute           │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │  Background Agents:                                          │  │
│  │    1. Switch to activity                                     │  │
│  │    2. Get activity path                                      │  │
│  │    3. Set CWD to activity path                               │  │
│  │    4. Include context in agent prompt                        │  │
│  │    5. Launch Task with context                               │  │
│  └──────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

## Data Flow: Create Activity

```
User: "Create worktree for exploring idea X"
  │
  ▼
Claude Code:
  │
  ├─► Suggest name: "exploring-idea-x"
  │
  ├─► Run: ./scripts/activity create exploring-idea-x "Exploring idea X"
  │     │
  │     ├─► Validate name (lowercase, dashes, no spaces)
  │     │
  │     ├─► Git: worktree add .wt/exploring-idea-x -b feature/exploring-idea-x
  │     │     │
  │     │     └─► Creates: .wt/exploring-idea-x/ (full repo copy)
  │     │
  │     ├─► Update: .wt/.activities.json
  │     │     │
  │     │     └─► Add entry:
  │     │         {
  │     │           "name": "exploring-idea-x",
  │     │           "worktree_path": ".wt/exploring-idea-x",
  │     │           "branch": "feature/exploring-idea-x",
  │     │           "created": "2026-01-10T12:00:00Z",
  │     │           "status": "active",
  │     │           ...
  │     │         }
  │     │
  │     └─► Update: .wt/.current-activity → "exploring-idea-x"
  │
  ├─► Update internal state:
  │     • current_activity = "exploring-idea-x"
  │     • current_activity_path = ".wt/exploring-idea-x"
  │
  └─► Confirm to user:
        "Created and switched to activity: exploring-idea-x"
        "Working in: /Users/bln/play/agentic-primer/.wt/exploring-idea-x"
```

## Data Flow: Switch Activity

```
User: "Switch to main"
  │
  ▼
Claude Code:
  │
  ├─► Run: ./scripts/activity switch main
  │     │
  │     ├─► Validate: activity "main" exists
  │     │
  │     ├─► Update: .wt/.activities.json
  │     │     │
  │     │     └─► Set current_activity = "main"
  │     │         Update last_accessed timestamp
  │     │
  │     └─► Update: .wt/.current-activity → "main"
  │
  ├─► Update internal state:
  │     • current_activity = "main"
  │     • current_activity_path = "/Users/bln/play/agentic-primer"
  │
  └─► Confirm to user:
        "Switched to activity: main"
```

## Data Flow: Read File in Activity

```
User: "Read README.md in exploring-idea-x"
  │
  ▼
Claude Code:
  │
  ├─► Switch activity (if needed)
  │     Run: ./scripts/activity switch exploring-idea-x
  │     Update: current_activity = "exploring-idea-x"
  │     Update: current_activity_path = ".wt/exploring-idea-x"
  │
  ├─► Resolve path:
  │     file_path = "README.md" (relative)
  │     absolute_path = current_activity_path + "/" + file_path
  │                   = ".wt/exploring-idea-x/README.md"
  │
  ├─► Read file:
  │     Read(".wt/exploring-idea-x/README.md")
  │
  └─► Show content to user
```

## Data Flow: Background Agent in Activity

```
User: "Run cleanup agent in exploring-idea-x"
  │
  ▼
Claude Code:
  │
  ├─► Switch to activity:
  │     Run: ./scripts/activity switch exploring-idea-x
  │     Update state: current_activity = "exploring-idea-x"
  │                   current_activity_path = ".wt/exploring-idea-x"
  │
  ├─► Get activity context:
  │     activity_name = "exploring-idea-x"
  │     activity_path = "/Users/bln/play/agentic-primer/.wt/exploring-idea-x"
  │
  ├─► Set working directory:
  │     Run: cd /Users/bln/play/agentic-primer/.wt/exploring-idea-x
  │
  ├─► Build agent prompt:
  │     "[ACTIVITY CONTEXT]
  │      Activity: exploring-idea-x
  │      Working Directory: /Users/bln/play/agentic-primer/.wt/exploring-idea-x
  │
  │      IMPORTANT: All file paths relative to activity worktree.
  │
  │      [TASK]
  │      Run cleanup..."
  │
  ├─► Launch agent:
  │     Task(prompt, cwd=activity_path)
  │       │
  │       └─► Agent executes in correct worktree
  │           Agent operates on correct files
  │           Agent doesn't pollute main repo
  │
  └─► Confirm to user:
        "Launched cleanup agent in activity: exploring-idea-x"
```

## File Structure

```
/Users/bln/play/agentic-primer/
│
├── .wt/                                    # Git-ignored worktrees
│   ├── .activities.json                    # Metadata (SOURCE OF TRUTH)
│   ├── .current-activity                   # Current activity name
│   │
│   ├── exploring-idea-x/                   # Activity worktree 1
│   │   ├── .git                            # Worktree git metadata
│   │   ├── src/
│   │   ├── README.md
│   │   └── ... (full repo)
│   │
│   └── experiment-123/                     # Activity worktree 2
│       ├── .git
│       ├── src/
│       └── ...
│
├── .gitignore                              # Contains: .wt/
│
├── scripts/
│   ├── activity                            # Main CLI
│   ├── activity-lib.sh                     # Shared library
│   └── activity-context                    # Context helper
│
├── ACTIVITY_WORKTREE_SYSTEM.md             # Full design doc
├── ACTIVITY_QUICK_REFERENCE.md             # User guide
├── CLAUDE_ACTIVITY_INTEGRATION.md          # Integration guide
├── CLAUDE_ACTIVITY_PROTOCOL.md             # Claude's protocol
└── ACTIVITY_SYSTEM_DIAGRAM.md              # This file
```

## State Synchronization

```
┌─────────────────────────────────────────────────────────────────┐
│                       State Consistency                         │
└─────────────────────────────────────────────────────────────────┘

Three layers of state, all kept in sync:

1. .wt/.activities.json (persistent, source of truth)
   ├─► schema_version: "1.0"
   ├─► current_activity: "exploring-idea-x"
   └─► activities: { ... }

2. .wt/.current-activity (quick state file)
   └─► "exploring-idea-x"

3. Claude Code internal state (in-memory)
   ├─► current_activity: "exploring-idea-x"
   └─► current_activity_path: "/path/to/.wt/exploring-idea-x"

All three updated atomically on:
  • Activity creation
  • Activity switch
  • Session initialization
```

## Error Handling Flow

```
User: "Switch to nonexistent-activity"
  │
  ▼
Claude Code:
  │
  ├─► Run: ./scripts/activity switch nonexistent-activity
  │     │
  │     └─► Error: Activity doesn't exist
  │
  ├─► Detect error
  │
  ├─► Run: ./scripts/activity list
  │     │
  │     └─► Show available activities
  │
  └─► Respond to user:
        "Activity 'nonexistent-activity' doesn't exist."
        "Here are your current activities:"
        [show list]
        "Create new activity: ./scripts/activity create <name>"
```

## Key Design Principles

```
┌──────────────────────────────────────────────────────────────┐
│  1. Git-Native                                               │
│     • Uses git worktrees (standard git feature)              │
│     • No custom git hacks                                    │
│     • Clean integration with existing tools                  │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│  2. Simple Metadata                                          │
│     • JSON file (human-readable, editable)                   │
│     • No database required                                   │
│     • Easy backup/restore                                    │
│     • Version control compatible                             │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│  3. Session Persistence                                      │
│     • Survives Claude Code restarts                          │
│     • State restored automatically                           │
│     • No manual re-initialization                            │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│  4. Context Awareness                                        │
│     • Claude always knows current activity                   │
│     • File operations use correct worktree                   │
│     • Background agents have full context                    │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│  5. Clean UX                                                 │
│     • Git-like CLI interface                                 │
│     • Clear error messages                                   │
│     • Helpful confirmations                                  │
│     • Color-coded output                                     │
└──────────────────────────────────────────────────────────────┘
```

## Summary

This system provides:
- **Isolated workspaces** via git worktrees
- **Reliable tracking** via JSON metadata
- **Context awareness** for Claude Code
- **Background agent support** with proper CWD
- **Session persistence** across restarts
- **Clean UX** for both users and automation

All built on standard tools (git, bash, jq) with no external dependencies.
