# Workflows & Ways of Working - Knowledge Category

## Overview

Captures meta-knowledge about effective work organization, task breakdown strategies, agent collaboration patterns, and process insights discovered during Claude Code sessions.

## Why This Matters

Unlike technical decisions or code patterns, this category captures **how work was done**, not just **what was done**. This meta-knowledge helps future agents:
- Organize complex work more effectively
- Choose appropriate delegation strategies
- Learn from successful collaboration patterns
- Avoid ineffective approaches

## Database Schema

```sql
-- Workflows & Ways of Working
CREATE TABLE IF NOT EXISTS session_workflows (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  message_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  workflow_type TEXT, -- 'delegation' | 'organization' | 'planning' | 'collaboration' | 'tooling'
  description TEXT NOT NULL,
  effectiveness TEXT, -- 'effective' | 'ineffective' | 'mixed'
  context TEXT, -- When/why this approach was used
  tools_involved TEXT, -- JSON array of tool/skill names
  outcome TEXT, -- What resulted from this approach
  lessons TEXT, -- Key takeaways
  confidence REAL DEFAULT 0.0,
  embedding F32_BLOB(768),
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_workflows_session ON session_workflows(session_id);
CREATE INDEX IF NOT EXISTS idx_workflows_type ON session_workflows(workflow_type);
CREATE INDEX IF NOT EXISTS idx_workflows_effectiveness ON session_workflows(effectiveness);
CREATE INDEX IF NOT EXISTS idx_workflows_timestamp ON session_workflows(timestamp);
```

## Prototype Examples

Examples for generating the "workflow" prototype embedding:

```typescript
const WORKFLOW_EXAMPLES = [
  "Used /bg to delegate research while continuing with implementation in parallel",
  "Breaking the task into smaller subtasks made it easier to track progress",
  "Spawning multiple background agents in parallel sped up the work significantly",
  "Planning the architecture before coding saved time and prevented rework",
  "Using /reflect at end of session documented what was accomplished effectively",
  "Delegating file analysis to background agent while focusing on planning was efficient",
  "Sequential approach worked better than parallel for dependent tasks",
  "Creating skills for repeated workflows eliminated manual repetition",
  "Using Task tool to track progress kept the work organized",
  "Consolidating scattered files into unified structure improved discoverability"
];
```

## Semantic Detection Patterns

### Stage 1: Candidate Detection (Embedding Similarity + Keyword Patterns)

```typescript
export class WorkflowCandidateDetector {
  private static WORKFLOW_PATTERNS = [
    // Tool/Skill usage patterns
    /\/(bg|reflect|know|skillsmith)\b/i,
    /\b(background|parallel|sequential|delegat)\w*/i,

    // Organization patterns
    /\b(broke down|split into|organized|structured|consolidated)\b/i,
    /\b(workflow|approach|strategy|pattern|process)\b/i,

    // Planning patterns
    /\b(plan\w*|design\w*|architect\w*)\s+(first|before|ahead)/i,
    /\b(iterative|incremental|phased)\b/i,

    // Collaboration patterns
    /\b(agent|task|subagent|spawn\w*)\b/i,
    /\b(collaborated|coordinated|distributed)\b/i,

    // Effectiveness indicators
    /\b(worked well|effective|efficient|faster|saved time)\b/i,
    /\b(didn't work|ineffective|slower|wasted)\b/i,

    // Process reflection
    /\b(learned that|discovered|realized|found that)\b.*\b(approach|method|way)\b/i,
  ];

  static detectWorkflowCandidates(
    messages: Array<{ id: string; content: string; embedding: Float32Array }>,
    prototypeEmbedding: Float32Array,
    threshold: number = 0.65
  ): string[] {
    const candidates: string[] = [];

    for (const message of messages) {
      // Check cosine similarity
      const similarity = this.cosineSimilarity(message.embedding, prototypeEmbedding);

      // Check keyword patterns
      const hasWorkflowPattern = this.WORKFLOW_PATTERNS.some(pattern =>
        pattern.test(message.content)
      );

      if (similarity >= threshold || hasWorkflowPattern) {
        candidates.push(message.id);
      }
    }

    return candidates;
  }
}
```

## Stage 2: LLM Classification

### System Prompt

```typescript
const WORKFLOW_SYSTEM_PROMPT = `You are a workflow pattern classifier. Analyze messages to identify insights about work organization, task breakdown, delegation strategies, and collaboration patterns.

A workflow message describes:
- How work was organized (parallel, sequential, delegated)
- Task breakdown or planning strategies
- Collaboration with agents or background tasks
- Tool/skill usage patterns that worked well (or didn't)
- Process improvements or lessons learned
- Meta-insights about the work itself, not technical details

Extract structured information if this describes a workflow or way of working.`;
```

### User Prompt Template

```typescript
const WORKFLOW_USER_PROMPT = (content: string) => `Analyze this message for workflow insights:

"""
${content}
"""

Respond with JSON only:
{
  "isWorkflow": true/false,
  "confidence": 0.0-1.0,
  "workflow_type": "delegation|organization|planning|collaboration|tooling",
  "description": "concise description of the workflow/approach",
  "effectiveness": "effective|ineffective|mixed",
  "context": "when/why this approach was used",
  "tools_involved": ["bg", "reflect", "Task", ...],
  "outcome": "what resulted from this approach",
  "lessons": "key takeaways or insights",
  "metadata": {}
}

Classification criteria:
- "delegation": Using /bg, Task, or agents to handle work
- "organization": Breaking down, structuring, consolidating work
- "planning": Planning, designing, or architecting before implementation
- "collaboration": Multi-agent coordination or parallel work
- "tooling": Effective tool/skill usage patterns

Only classify as workflow if the message discusses HOW work was done, not WHAT was done.`;
```

## CLI Commands

### Query Workflows

Add to `cli.ts`:

```typescript
async function runWorkflows(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showWorkflowsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);
  const db = createClient({ url: `file:${DB_PATH}` });
  const command = cleanArgs[0] || 'recent';
  const arg = cleanArgs[1];

  try {
    let sql = '';
    let queryArgs: any[] = [];

    switch (command) {
      case 'today': {
        const startOfDay = new Date().setHours(0, 0, 0, 0);
        sql = 'SELECT * FROM session_workflows WHERE timestamp >= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay];
        break;
      }
      case 'recent': {
        const limit = parseInt(arg) || 10;
        sql = 'SELECT * FROM session_workflows ORDER BY timestamp DESC LIMIT ?';
        queryArgs = [limit];
        break;
      }
      case 'type': {
        if (!arg) {
          console.error('Usage: know workflows type <workflow-type>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_workflows WHERE workflow_type = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      case 'types': {
        sql = `SELECT workflow_type, COUNT(*) as count
               FROM session_workflows
               GROUP BY workflow_type
               ORDER BY count DESC`;
        break;
      }
      case 'effective': {
        sql = `SELECT * FROM session_workflows
               WHERE effectiveness = 'effective'
               ORDER BY confidence DESC, timestamp DESC
               LIMIT ${parseInt(arg) || 10}`;
        break;
      }
      case 'session': {
        if (!arg) {
          console.error('Usage: know workflows session <session-id>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_workflows WHERE session_id = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      default: {
        console.error('Usage: know workflows [today|recent [N]|type <type>|types|effective|session <id>]');
        process.exit(1);
      }
    }

    const result = await db.execute({ sql, args: queryArgs });

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'workflows',
        filter: command,
        count: result.rows.length,
        results: result.rows.map(row => ({
          id: row.id,
          session_id: row.session_id,
          timestamp: row.timestamp,
          workflow_type: row.workflow_type,
          description: row.description,
          effectiveness: row.effectiveness,
          context: row.context,
          tools_involved: JSON.parse(row.tools_involved || '[]'),
          outcome: row.outcome,
          lessons: row.lessons,
          confidence: row.confidence
        }))
      }, null, 2));
    } else {
      console.log(`\n📊 Workflows (${result.rows.length} found)\n`);

      result.rows.forEach(row => {
        const time = new Date(row.timestamp as number).toISOString().slice(11, 19);
        const effectivenessEmoji = row.effectiveness === 'effective' ? '✅' :
                                   row.effectiveness === 'ineffective' ? '❌' : '⚠️';

        console.log(`[${time}] ${effectivenessEmoji} ${row.description}`);
        console.log(`  Type: ${row.workflow_type} | Confidence: ${Math.round((row.confidence as number) * 100)}%`);

        if (row.tools_involved) {
          const tools = JSON.parse(row.tools_involved as string);
          if (tools.length > 0) {
            console.log(`  Tools: ${tools.join(', ')}`);
          }
        }

        if (row.outcome) {
          console.log(`  Outcome: ${row.outcome}`);
        }

        if (row.lessons) {
          console.log(`  Lessons: ${row.lessons}`);
        }

        console.log(`  Session: ${(row.session_id as string).slice(0, 8)}... | ID: ${(row.id as string).slice(0, 12)}...`);
        console.log();
      });
    }
  } finally {
    db.close();
  }
}

function showWorkflowsHelp() {
  console.log(`
Query Extracted Workflows

Usage:
  know workflows [filter] [--json]

Filters:
  today                Workflows from today
  recent [N]           Most recent N workflows (default: 10)
  type <type>          Filter by workflow type
  types                List all workflow types with counts
  effective            Most effective workflows
  session <id>         Workflows from specific session

Workflow Types:
  delegation     Using /bg, Task, or agents to handle work
  organization   Breaking down, structuring, consolidating
  planning       Planning/designing before implementation
  collaboration  Multi-agent coordination, parallel work
  tooling        Effective tool/skill usage patterns

Examples:
  know workflows recent 20
  know workflows type delegation
  know workflows effective
  know workflows types

Output Fields:
  - Description: What workflow/approach was used
  - Type: Category of workflow
  - Effectiveness: Whether it worked well
  - Tools Involved: Skills/tools used
  - Outcome: What resulted
  - Lessons: Key takeaways
  - Confidence: Classification confidence (0.0-1.0)

Description:
  Displays workflow patterns and ways of working extracted from sessions.
  Useful for learning effective work organization strategies.
  `);
}
```

### Add to Skill

Update `.claude/skills/know/SKILL.md`:

```markdown
### Query Commands
```bash
/know decisions [filter]     # Query architectural/technical decisions
/know learnings [filter]     # Query insights and discoveries
/know errors [filter]        # Query errors and their resolutions
/know workflows [filter]     # Query work organization patterns
/know discover              # See what knowledge exists
/know stats                 # System statistics
```

## Example Queries

```bash
# Find effective delegation patterns
./know workflows type delegation

# See what worked well
./know workflows effective

# All workflow types discovered
./know workflows types

# Workflows from today's session
./know workflows today
```

## Real Examples from Today's Session

Based on actual work done today, these would be extracted:

```json
{
  "workflow_type": "delegation",
  "description": "Used /bg to delegate knowledge category research while starting embedding indexer in parallel",
  "effectiveness": "effective",
  "tools_involved": ["bg", "Bash"],
  "outcome": "Both tasks completed independently, saved sequential time",
  "lessons": "Background delegation works well for independent research tasks",
  "confidence": 0.89
}
```

```json
{
  "workflow_type": "organization",
  "description": "Consolidated 5 separate skills (decisions, learnings, errors, extract, prototypes) into unified /know skill",
  "effectiveness": "effective",
  "tools_involved": ["Edit", "Write"],
  "outcome": "Simpler mental model, single entry point, easier discovery",
  "lessons": "Unification reduces cognitive load when commands share common purpose",
  "confidence": 0.92
}
```

```json
{
  "workflow_type": "planning",
  "description": "Migrated .clinerules to official .claude/rules/ after learning about standard",
  "effectiveness": "effective",
  "tools_involved": ["claude-code-guide", "WebSearch"],
  "outcome": "Following official standard enables automatic loading",
  "lessons": "Research official patterns before creating custom conventions",
  "confidence": 0.87
}
```

## Implementation Checklist

- [ ] Add `session_workflows` table to schema
- [ ] Generate workflow prototype embeddings from examples
- [ ] Implement WorkflowCandidateDetector with patterns
- [ ] Add workflow classification to SemanticMessageClassifier
- [ ] Create CLI query commands (runWorkflows, showWorkflowsHelp)
- [ ] Update /know skill with workflow examples
- [ ] Add workflow extraction to KnowledgeExtractor
- [ ] Test extraction on today's session
- [ ] Add to discover command output
- [ ] Update documentation

## Priority

**HIGH PRIORITY** - This is unique to Claude Code's agentic nature and has immediate, clear value for improving future agent effectiveness.
