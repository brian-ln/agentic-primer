#!/usr/bin/env bun

/**
 * Local /bg implementation with fast launch workflow
 *
 * Usage: bun src/cli/bg.ts "your task description"
 *
 * This is a project-local implementation for testing the fast /bg workflow
 * before rolling it out as a global skill update.
 */

import { existsSync, readdirSync, statSync } from "fs";
import { join } from "path";

// Get task description from command line
const taskDescription = process.argv.slice(2).join(" ");

if (!taskDescription) {
  console.error("Usage: bun src/cli/bg.ts \"your task description\"");
  process.exit(1);
}

// Discover current session log path
function getCurrentSessionLogPath(): string {
  const cwd = process.cwd();
  const projectHash = cwd
    .split("/")
    .filter(p => p && p !== "Users")
    .join("-");

  const sessionDir = `${process.env.HOME}/.claude/projects/-${projectHash}`;

  if (!existsSync(sessionDir)) {
    console.warn(`⚠️  Session directory not found: ${sessionDir}`);
    return "";
  }

  const sessionFiles = readdirSync(sessionDir)
    .filter(f => f.endsWith(".jsonl"))
    .map(f => ({
      name: f,
      path: join(sessionDir, f),
      mtime: statSync(join(sessionDir, f)).mtime
    }))
    .sort((a, b) => b.mtime.getTime() - a.mtime.getTime());

  if (sessionFiles.length === 0) {
    console.warn("⚠️  No session files found in:", sessionDir);
    return "";
  }

  return sessionFiles[0].path;
}

// Generate short description (3-5 words)
function generateShortDescription(task: string): string {
  // Extract key action verbs and nouns
  const words = task.split(" ").slice(0, 5);
  return words.join(" ");
}

// Build enhanced agent prompt
function buildEnhancedPrompt(task: string, sessionLogPath: string): string {
  const agentId = `bg-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

  return `${task}

## EXECUTION CONTEXT: BACKGROUND SUBAGENT

You are executing as a BACKGROUND SUBAGENT in an isolated process.

**Your Constraints:**
- ❌ Cannot directly interact with user
- ❌ Cannot use interactive prompts
- ✅ All user communication via parent agent
- ✅ Use CLARIFICATION_NEEDED protocol when blocked

### Session Context Available

${sessionLogPath ? `Read context from: ${sessionLogPath}` : "⚠️  Session log not available - proceed with task description only"}

Use session logs to understand:
- Recent decisions and patterns
- Active files and changes
- Project conventions (actor worldview, YAGNI, etc.)
- Technical constraints (CozoDB, Bun, TypeScript)
- User goals and requirements

### Context Extraction Utilities

You have access to session log utilities at \`src/utils/session-log.ts\`:

\`\`\`typescript
import { extractSessionContext } from "../utils/session-log.ts";

const context = await extractSessionContext("${sessionLogPath}", {
  maxEntries: 100,
  includeDecisions: true,
  includeState: true,
  includeTopics: true,
  includeGoals: true,
  includeTasks: true,
  includeTimeline: true
});

// Use context to understand:
// - context.recentDecisions
// - context.conversationState
// - context.activeTopics
// - context.currentGoals
// - context.relatedTasks
// - context.timeline
\`\`\`

### CLARIFICATION_NEEDED Protocol

If you need information not available in session logs, signal clarification:

\`\`\`yaml
[CLARIFICATION_NEEDED]
agent_id: ${agentId}
timestamp: <ISO-8601>
blocked_at: "Specific point where you stopped"
reason: "Why you need clarification"

questions:
  - question_id: Q1
    text: "Specific question"
    context: "Why this matters and what you know"
    options: ["Option A", "Option B"]  # if multiple choice
    current_assumption: "What you'll assume if no answer"  # if optional

can_resume_with: "What information unblocks you"
current_state: |
  Completed:
  - What you've finished
  Blocked:
  - What's waiting
work_continues: true  # if you can do parallel work
parallel_work_available: |
  Can continue with:
  - What work doesn't need clarification
[/CLARIFICATION_NEEDED]
\`\`\`

**How to Use:**
1. Write clarification to stdout/output
2. Continue with parallel work if possible
3. Parent monitors and will forward questions to user
4. Wait for response, then resume

### Execution Guidelines

- **Start with session log analysis** - Spend 30-60 seconds extracting context
- **Prefer autonomy** - Only ask when truly blocked
- **Continue parallel work** - Don't wait idle during clarification
- **Document assumptions** - Include in completion report
- **Follow project conventions** - Actor worldview, YAGNI, simple scales
- **Signal completion properly** - Use COMPLETION_REPORT format

### Success Criteria

Your work is successful when:
- Deliverables match user intent (from task + session logs)
- Quality meets project standards (see CLAUDE.md)
- Documentation is clear and complete
- You used session logs effectively (0-1 clarifications ideally)
- You follow actor worldview patterns where applicable
- You signal completion properly

### Project Conventions (from session context)

Key patterns to follow:
- **Actor Model**: Design → Fitness → Optimize → Validate
- **Addressing**: \`primer.domain.entity_type_id\` (hierarchical + graph edges)
- **YAGNI**: Simple scales, no premature abstraction
- **Testing**: Pre-flight check mandatory, zero means zero
- **Documentation**: .spec.md + .model.lisp for formal modeling

## Execute Autonomously

Read session logs, extract context, validate task clarity, ask clarification if needed, and proceed autonomously!
`;
}

// Main execution
async function main() {
  console.log("🚀 Fast /bg Launch - Local Implementation\n");

  const sessionLogPath = getCurrentSessionLogPath();
  const shortDesc = generateShortDescription(taskDescription);
  const enhancedPrompt = buildEnhancedPrompt(taskDescription, sessionLogPath);

  console.log("Task:", taskDescription);
  console.log("Description:", shortDesc);
  if (sessionLogPath) {
    console.log("Session Log:", sessionLogPath);
  }
  console.log("\n📋 Enhanced Prompt Preview:");
  console.log("─".repeat(80));
  console.log(enhancedPrompt.split("\n").slice(0, 10).join("\n"));
  console.log("... (full prompt prepared)");
  console.log("─".repeat(80));

  console.log("\n✅ Ready to launch background agent!");
  console.log("\n⚠️  MANUAL STEP REQUIRED:");
  console.log("   Claude CLI doesn't expose Task tool programmatically yet.");
  console.log("   Copy the task description above and use:");
  console.log(`
   /bg ${taskDescription}

   Or in your session, invoke the Task tool with:
   - subagent_type: "general-purpose"
   - description: "${shortDesc}"
   - prompt: [Use enhanced prompt above]
   - run_in_background: true
  `);

  // TODO: When Claude Agent SDK supports it, launch directly:
  // const agent = await Task({
  //   subagent_type: "general-purpose",
  //   description: shortDesc,
  //   prompt: enhancedPrompt,
  //   run_in_background: true
  // });
  // console.log("Agent ID:", agent.id);
}

main().catch(console.error);
