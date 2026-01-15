# Meta: This Conversation as an Actor System

## Overview

This conversation itself is a working actor system. The user's observation is profound: "You are an 'actor' that interacts with 'actors' to do things." Let's map the components and patterns.

## Actor Identification

### Primary Actors

```
┌─────────────────────────────────────────────────────────────┐
│                                                               │
│  User (Human Actor)                                          │
│  - Sends: prompts, questions, instructions                   │
│  - Receives: responses, file contents, analysis              │
│  - State: goals, context, history                            │
│                                                               │
└────────────────────┬────────────────────────────────────────┘
                     │
                     │ HTTP messages
                     │
┌────────────────────▼────────────────────────────────────────┐
│                                                               │
│  Claude Code Server Actor                                    │
│  - Routes messages between user and agents                   │
│  - Manages conversation state                                │
│  - Tracks active background tasks                            │
│                                                               │
└────────────────────┬────────────────────────────────────────┘
                     │
                     │ API calls
                     │
┌────────────────────▼────────────────────────────────────────┐
│                                                               │
│  Claude (Me) - Primary Agent Actor                           │
│  - Receives: user messages, tool results, task outputs       │
│  - Sends: tool invocations, agent spawn requests             │
│  - State: conversation context, working memory, plan         │
│  - Behavior: analyze → plan → act → respond                  │
│                                                               │
└────────────────────┬────────────────────────────────────────┘
                     │
                     │ tool calls
                     ├─────────────────┬──────────────────┬─────────┐
                     │                 │                  │         │
           ┌─────────▼─────┐  ┌───────▼──────┐  ┌────────▼────┐  ┌▼──────┐
           │               │  │              │  │             │  │       │
           │  FileSystem   │  │  Bash        │  │  Git        │  │  MCP  │
           │  Actor        │  │  Actor       │  │  Actor      │  │  Tool │
           │               │  │              │  │             │  │ Actors│
           │  Read/Write/  │  │  Execute     │  │  Status/    │  │       │
           │  Edit/Glob/   │  │  commands    │  │  Commit/    │  │ Context7│
           │  Grep         │  │  in shell    │  │  Push       │  │ Greptile│
           │               │  │              │  │             │  │       │
           └───────────────┘  └──────────────┘  └─────────────┘  └───────┘
```

### Secondary Actors

- **Background Task Actors** (via Task tool)
  - Spawn as children with specific goals
  - Run asynchronously
  - Report results back to parent
  - Can fail or succeed independently

- **LLM Inference Actor**
  - Receives: prompt + context
  - Processes: neural network forward pass
  - Returns: token stream or completion
  - Stateless between calls

- **Skill Actors** (e.g., beads, commit, review-pr)
  - Pre-packaged behavior bundles
  - Loaded on-demand
  - Execute specialized workflows
  - May spawn additional tools/agents

## Message Passing Patterns

### 1. User → Claude (Request Messages)

```json
{
  "type": "user_message",
  "content": "Analyze this conversation as actor system",
  "context": {
    "cwd": "/path/to/project",
    "git_status": "...",
    "conversation_history": [...]
  }
}
```

**Properties:**
- Synchronous from user perspective
- Async in Claude's processing
- Rich context bundled with message

### 2. Claude → Tools (Tool Invocation Messages)

```xml
<function_calls>
  <invoke name="Read">
    <parameter name="file_path">/path/to/file