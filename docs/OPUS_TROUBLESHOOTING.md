# Opus API Troubleshooting Report

**Generated:** 2026-02-16T21:53:33Z
**Status:** Root cause identified
**Severity:** Configuration issue - Missing authentication

## Summary

The `/ai opus` command fails with `invalid_request_error` while `/ai kimi` succeeds. Root cause: The Opus model is configured to route through the Claude CLI gateway (`cli/claude`), but the CLI provider is not properly integrated with the daemon's specialized Claude CLI handler. Instead, it uses a generic CLI spawner that doesn't properly configure authentication environment variables. The Claude CLI requires `ANTHROPIC_API_KEY` to be set for MAX plan authentication, but this key is not present in the current environment and not being passed to the spawned CLI process.

## Investigation Results

### Error Analysis

**Opus Failure (ba35edd.output):**
```
[Daemon] Using route: cli/claude
[Daemon] Routing through http://localhost:3001/cli/claude/v1/messages
[Daemon] Model: claude-opus-4-6, Provider: claude-cli

Error: invalid_request_error

Trying fallback providers...
Attempting cloudflare...
Attempting anthropic...
```

**Kimi Success (b6e55aa.output):**
```
[Daemon] Using route: bln_ai/custom-nvidia-nim
[Daemon] Routing through http://localhost:3001/bln_ai/custom-nvidia-nim/v1/messages
[Daemon] Model: moonshotai/kimi-k2.5, Provider: custom-nvidia-nim
 Not much! Just here and ready to help. What's on your mind today?

[Model: moonshotai/kimi-k2.5, Tokens: 196, Time: 28.68s]
```

### Configuration Comparison

| Aspect | Kimi (Working) | Opus (Failing) |
|--------|----------------|----------------|
| **Route** | `bln_ai/custom-nvidia-nim` | `cli/claude` |
| **Gateway** | Cloudflare AI Gateway | CLI Gateway (local command execution) |
| **Integration** | custom-nvidia-nim | claude |
| **Provider** | custom-nvidia-nim (API-based) | claude-cli (CLI-based) |
| **Protocol** | OpenAI-compatible HTTP API | CLI command spawning |
| **Authentication** | NVIDIA_API_KEY (via HTTP headers) | ANTHROPIC_API_KEY (via environment) |
| **Handler** | Direct provider route (messages.ts:392) | Generic CLI route (messages.ts:688) |

### Key Findings

1. **Routing Architecture:**
   - Opus uses `gateway: "cli"` + `integration: "claude"` configuration
   - Routes to `http://localhost:3001/cli/claude/v1/messages`
   - Daemon router parses this as CLI gateway routing

2. **Handler Mismatch:**
   - Specialized handler exists: `/daemon/src/providers/claude-cli.ts` (handleClaudeCliRequest)
   - But it's **NOT USED** by the messages endpoint
   - Instead, generic handler at `messages.ts:688` (routeThroughCLI) is called
   - Generic handler spawns: `claude --print --model claude-opus-4-6 <prompt>`

3. **Authentication Gap:**
   - Generic CLI spawner uses basic `spawn()` with no environment setup
   - Doesn't pass `ANTHROPIC_API_KEY` to the CLI process
   - Claude CLI requires API key for MAX plan authentication
   - Environment check: `ANTHROPIC_API_KEY` is **NOT SET**

4. **Specialized Handler Exists But Unused:**
   - File: `/daemon/src/providers/claude-cli.ts`
   - Has proper environment handling (lines 140-159):
     - Builds clean environment
     - Passes `ANTHROPIC_API_KEY` if present
     - Uses stream-json format for proper request/response handling
     - Handles multi-turn conversations correctly
   - Never imported or called by messages.ts endpoint

### Daemon Logs

The daemon logs show only startup messages and no detailed request routing:
```
Starting Kimi Proxy daemon...
[SERVER] Listening on port 3001
Server running on port 3001
```

Request-level logging is not captured in the logs, only generic startup. The daemon is running but not logging the actual error details from the Claude CLI spawn.

## Root Cause

**Three-part failure:**

1. **Missing Environment Variable:**
   - `ANTHROPIC_API_KEY` is not set in the shell environment
   - Claude CLI requires this for MAX plan authentication
   - Generic CLI spawner doesn't inject this from config

2. **Wrong Handler Used:**
   - Messages endpoint uses generic `routeThroughCLI()` function
   - This spawns CLI with minimal args, no environment setup
   - Specialized `handleClaudeCliRequest()` exists but is never called
   - Integration gap: no connection between `gateway: "cli"` routing and specialized handler

3. **Incomplete CLI Integration:**
   - The CLI gateway route (`cli/claude`) is recognized by the router
   - But the messages endpoint doesn't know to use the specialized Claude CLI provider
   - Generic CLI handler is too simplistic for Claude CLI's requirements

**Technical Explanation:**

When a request comes in for `/cli/claude/v1/messages`:
1. Router parses: `gateway='cli'`, `integration='claude'`, `provider='claude-cli'`
2. Messages endpoint checks: `gateway === 'cli'` → calls `routeThroughCLI()`
3. Generic handler executes: `spawn('claude', ['--print', '--model', 'claude-opus-4-6', prompt])`
4. Claude CLI starts without `ANTHROPIC_API_KEY` in environment
5. CLI attempts to authenticate with Anthropic API → fails
6. CLI returns: `invalid_request_error`

## Recommended Fix

### Option 1: Set ANTHROPIC_API_KEY (Quick Fix)

**Action:**
```bash
# Add to ~/.zshrc or ~/.bashrc
export ANTHROPIC_API_KEY="sk-ant-xxxxx"

# Reload environment
source ~/.zshrc  # or ~/.bashrc

# Restart daemon
ai daemon restart
```

**Test:**
```bash
# Verify key is set
echo $ANTHROPIC_API_KEY

# Test opus
ai opus "test message"
```

**Pros:**
- Quick fix, works immediately
- Matches Kimi pattern (NVIDIA_API_KEY is set)

**Cons:**
- Doesn't fix the architectural issue
- Key is exposed in shell environment
- Generic CLI handler still doesn't properly configure environment

### Option 2: Connect Specialized Handler (Proper Fix)

**Action 1 - Update messages.ts to use specialized handler:**

File: `~/.claude/skills/ai/app/daemon/src/endpoints/messages.ts`

```typescript
// Add import at top
import { handleClaudeCliRequest, handleClaudeCliStreamingRequest } from '../providers/claude-cli';

// Replace routeThroughCLI() call (around line 126)
// OLD:
if (gateway === 'cli') {
  response = await routeThroughCLI(resolvedRequest, config, controller, eventLogger, convCtx);
}

// NEW:
if (gateway === 'cli' && provider === 'claude-cli') {
  // Use specialized Claude CLI handler
  if (anthropicRequest.stream) {
    const stream = await handleClaudeCliStreamingRequest(resolvedRequest, config);
    return new Response(stream, {
      status: 200,
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive',
        'Access-Control-Allow-Origin': '*',
      },
    });
  } else {
    const anthropicResponse = await handleClaudeCliRequest(resolvedRequest, config);
    return new Response(JSON.stringify(anthropicResponse), {
      status: 200,
      headers: {
        'Content-Type': 'application/json',
        'Access-Control-Allow-Origin': '*',
      },
    });
  }
} else if (gateway === 'cli') {
  // Fallback for other CLI providers
  response = await routeThroughCLI(resolvedRequest, config, controller, eventLogger, convCtx);
}
```

**Action 2 - Update config.ts to load ANTHROPIC_API_KEY:**

The specialized handler checks `process.env.ANTHROPIC_API_KEY` (line 157 in claude-cli.ts). Ensure the daemon config loads it:

File: `~/.claude/skills/ai/app/daemon/src/config.ts` (already has this at line 80)

**Action 3 - Restart daemon:**
```bash
ai daemon restart
```

**Pros:**
- Uses proper Claude CLI integration with stream-json format
- Handles environment properly (passes ANTHROPIC_API_KEY)
- Supports multi-turn conversations
- More robust error handling

**Cons:**
- Requires code modification
- Need to restart daemon

### Option 3: Alternative Route (Workaround)

**Action - Change Opus configuration to use direct Anthropic API:**

File: `~/.config/ai/models.json`

```json
{
  "models": {
    "opus": {
      "model": "claude-opus-4-6",
      "gateway": "_",
      "integration": "anthropic",
      "provider": "anthropic",
      "fallback": ["cloudflare"]
    }
  }
}
```

Then set `ANTHROPIC_API_KEY` as in Option 1.

**Test:**
```bash
ai opus "test message"
```

**Pros:**
- Bypasses CLI entirely
- Uses direct Anthropic API (more reliable)
- No code changes needed

**Cons:**
- Doesn't fix the CLI integration issue
- Loses CLI-specific features (session persistence, etc.)

## Verification

**After applying fix, test with:**

```bash
# Test Opus
ai opus "What is 2+2?"

# Compare with Kimi (should both work)
ai kimi "What is 2+2?"

# Check daemon logs for proper routing
tail -50 ~/.claude/skills/ai/app/daemon/daemon.log
```

**Expected output:**
```
[Daemon] Using route: cli/claude
[Daemon] Routing through http://localhost:3001/cli/claude/v1/messages
[Daemon] Model: claude-opus-4-6, Provider: claude-cli
[CLAUDE-CLI] handleClaudeCliRequest called, model: claude-opus-4-6
[CLAUDE-CLI] Process spawned, stdin type: object
[CLAUDE-CLI] 1 messages written to stdin
[CLAUDE-CLI] Success!
4
```

## Prevention

**Best Practices for Future:**

1. **Environment Variables:**
   - Document required environment variables in daemon README
   - Add startup validation that checks for required keys
   - Log warning if keys are missing

2. **Handler Integration:**
   - When adding specialized handlers, ensure they're wired into the routing layer
   - Add integration tests that verify each route configuration
   - Document the handler selection logic in messages.ts

3. **Error Messages:**
   - Enhance error messages to indicate missing authentication
   - Claude CLI should return more specific error (e.g., "Missing ANTHROPIC_API_KEY")
   - Daemon should catch and enhance CLI errors before returning to client

4. **Configuration Validation:**
   - Add `ai config validate` command that checks:
     - Model configurations reference valid providers
     - Required environment variables are set for each provider
     - Daemon routes are properly configured

5. **Documentation:**
   - Update AI skill README with authentication requirements per provider
   - Document the difference between CLI gateway and API gateway
   - Add troubleshooting section for common authentication errors

## Related Files

- **Model Config:** `~/.config/ai/models.json` (opus + kimi definitions)
- **Daemon Router:** `~/.claude/skills/ai/app/daemon/src/server/router.ts` (routing logic)
- **Messages Handler:** `~/.claude/skills/ai/app/daemon/src/endpoints/messages.ts` (request handling)
- **Specialized Provider:** `~/.claude/skills/ai/app/daemon/src/providers/claude-cli.ts` (unused handler)
- **Registry:** `~/.claude/skills/ai/app/registry.ts` (provider definitions)
- **AI CLI:** `~/.claude/skills/ai/app/ai` (client-side model resolution)

## References

- Task Outputs:
  - Opus failure: `/private/tmp/claude-501/-Users-bln-play-agentic-primer/tasks/ba35edd.output`
  - Kimi success: `/private/tmp/claude-501/-Users-bln-play-agentic-primer/tasks/b6e55aa.output`
- Daemon Log: `~/.claude/skills/ai/app/daemon/daemon.log`
- Daemon State: `~/.claude/skills/ai/app/daemon/.daemon.state`
