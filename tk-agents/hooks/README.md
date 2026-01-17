# Claude Code Hooks for Task Tracking

**Status:** Reference Implementation (Not Enabled by Default)
**Created:** 2026-01-16
**Purpose:** Automatic task creation/completion for background agents

---

## Overview

These hooks automatically create and update tasks in `tasks.json` when background agents are launched and completed.

**Current Approach:** Manual workflow (CLAUDE.md instructions)
**Future Option:** Hook automation (these scripts)

---

## Files

| File | Purpose | Status |
|------|---------|--------|
| `pre-tool-use.sh` | Auto-create task when agent launches | Reference only |
| `post-tool-use.sh` | Auto-update task when agent completes | Reference only |
| `README.md` | This documentation | Current |

---

## How They Work

### Pre-Tool-Use Hook

**Trigger:** Before any tool execution
**Detection:** Bash tool with `run_in_background: true`
**Action:**
1. Parse agent description from tool input
2. Infer agent type (research, implementation, etc.)
3. Create task via CLI: `bun src/cli/task.ts add "Agent: <desc>"`
4. Store agent_id → task_id mapping

**Output:** Task created, mapping stored in `.agent-task-map.txt`

### Post-Tool-Use Hook

**Trigger:** After tool execution completes
**Detection:** Same as PreToolUse
**Action:**
1. Lookup task_id from agent_id mapping
2. Check tool execution success/failure
3. Update task: `complete` or `block "Failed"`
4. Clean up mapping entry

**Output:** Task status updated

---

## Installation (Future Phase 2)

### Option A: Global Hooks (All Projects)

```bash
mkdir -p ~/.claude/hooks
cp hooks/pre-tool-use.sh ~/.claude/hooks/
cp hooks/post-tool-use.sh ~/.claude/hooks/
chmod +x ~/.claude/hooks/*.sh
```

### Option B: Project-Specific Hooks

```bash
mkdir -p .claude/hooks
cp hooks/pre-tool-use.sh .claude/hooks/
cp hooks/post-tool-use.sh .claude/hooks/
chmod +x .claude/hooks/*.sh
```

---

## Requirements

### Dependencies

- **jq**: JSON parsing (`brew install jq`)
- **bun**: Task CLI runtime
- **tasks.json**: Must exist in project root

### Environment Variables

Hooks expect these environment variables (provided by Claude Code):

- `CLAUDE_PROJECT_DIR`: Project root directory
- Hook event data passed via stdin (JSON)

---

## Hook Event Schema

### PreToolUse Event

```json
{
  "event": "PreToolUse",
  "data": {
    "session_id": "...",
    "tool_name": "Bash",
    "tool_input": {
      "command": "...",
      "description": "Launch agent for X",
      "run_in_background": true
    }
  },
  "env": {
    "CLAUDE_PROJECT_DIR": "/path/to/project"
  }
}
```

### PostToolUse Event

```json
{
  "event": "PostToolUse",
  "data": {
    "session_id": "...",
    "tool_name": "Bash",
    "tool_input": { "run_in_background": true },
    "tool_output": {
      "success": true,
      "output": "..."
    }
  },
  "env": {
    "CLAUDE_PROJECT_DIR": "/path/to/project"
  }
}
```

### Required Response Schema

**PreToolUse:**
```json
{
  "continue": true,
  "hookSpecificOutput": {
    "permissionDecision": "allow"
  }
}
```

**PostToolUse:**
```json
{
  "continue": true
}
```

---

## Testing

### Test 1: Manual Hook Execution

```bash
# Simulate PreToolUse event
echo '{
  "data": {
    "tool_name": "Bash",
    "tool_input": {
      "description": "Research graph queries",
      "run_in_background": true
    },
    "session_id": "test-123"
  },
  "env": {
    "CLAUDE_PROJECT_DIR": "'$(pwd)'"
  }
}' | ./hooks/pre-tool-use.sh

# Check result
cat .agent-task-map.txt
bun src/cli/task.ts list --label agent
```

### Test 2: End-to-End with Background Agent

```bash
# Install hooks (global)
mkdir -p ~/.claude/hooks
cp hooks/*.sh ~/.claude/hooks/
chmod +x ~/.claude/hooks/*.sh

# Launch agent (in Claude Code)
/bg Research error handling patterns

# Verify task created
bun src/cli/task.ts list --label agent

# Wait for completion

# Verify task updated
bun src/cli/task.ts show task_X
```

---

## Limitations

### Current Limitations

1. **Agent Type Inference:** Heuristic-based (keywords in description)
2. **Error Handling:** Limited recovery from task creation failures
3. **Concurrent Agents:** Race conditions possible with shared mapping file
4. **Session Correlation:** Uses session_id (may not match agent_id)

### Known Issues

- Mapping file grows unbounded (no cleanup)
- No support for sub-agent hierarchy
- Cannot detect agent spawned by another agent
- Hook execution timing depends on Claude Code internals

---

## Troubleshooting

### Hook Not Triggering

**Check:**
1. Hook files are executable: `ls -l ~/.claude/hooks`
2. Hook location is correct (global vs project)
3. Claude Code hooks enabled (check settings)

**Debug:**
```bash
# Check hook logs
tail -f ~/.claude/hook-logs/*.log

# Manual test
echo '{}' | ./hooks/pre-tool-use.sh
```

### Task Not Created

**Check:**
1. `tasks.json` exists: `ls -l tasks.json`
2. Task CLI works: `bun src/cli/task.ts list`
3. jq installed: `which jq`

**Debug:**
```bash
# Test task creation
bun src/cli/task.ts add "Test task" --labels agent --json
```

### Mapping File Issues

**Check:**
```bash
# View mapping
cat .agent-task-map.txt

# Clean up stale entries
rm .agent-task-map.txt
```

---

## Migration from Manual to Automated

When enabling hooks (Phase 2):

1. **Backup existing tasks:**
   ```bash
   cp tasks.json tasks.json.backup
   ```

2. **Install hooks:**
   ```bash
   cp hooks/*.sh ~/.claude/hooks/
   chmod +x ~/.claude/hooks/*.sh
   ```

3. **Test with single agent:**
   ```bash
   /bg Test agent work
   # Verify task created
   ```

4. **Monitor for 1 week:**
   - Check mapping file growth
   - Verify task updates
   - Look for errors in hook logs

5. **Full rollout:**
   - Remove manual CLAUDE.md reminders
   - Keep instructions as documentation

---

## Performance

**Expected Overhead:**
- PreToolUse: ~10-50ms (task creation + file I/O)
- PostToolUse: ~10-30ms (task update + cleanup)

**Impact:**
- Minimal on interactive commands
- Negligible for background agents (already async)

---

## Security Considerations

1. **Hook Execution:** Hooks run with user permissions
2. **Input Validation:** Sanitize description text for shell safety
3. **File Access:** Limited to project directory
4. **No Network:** Hooks should not make external requests

---

## Future Enhancements

### Phase 2 Improvements

- [ ] Structured agent metadata capture
- [ ] Sub-agent hierarchy tracking
- [ ] Concurrent agent support (file locking)
- [ ] Hook error recovery
- [ ] Performance metrics

### Phase 3 Enhancements

- [ ] Agent duration tracking
- [ ] Deliverable file tracking
- [ ] Success rate analytics
- [ ] Dashboard integration

---

## References

- **Design Document:** `TASK_TRACKING_AUTOMATION.md`
- **Task CLI Spec:** `src/cli/TASK_CLI.spec.md`
- **CLAUDE.md:** Project instructions (manual workflow)
- **Hook Logs:** `~/.claude/.claude-backup/hook-logs/all-events.jsonl`

---

## Support

For issues or questions:
1. Review `TASK_TRACKING_AUTOMATION.md`
2. Test hooks manually (see Testing section)
3. Check Claude Code hook documentation
4. Fall back to manual workflow (CLAUDE.md)

---

**Remember:** These hooks are reference implementations. Start with manual workflow (CLAUDE.md) and enable hooks only when automation proves valuable.
