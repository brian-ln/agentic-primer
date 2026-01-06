# Bootloader Generalization Options

This document explores how the GitHub-centric bootloader concept could be generalized to other platforms and contexts.

## Current Model: GitHub-Centric
- **Work Items**: GitHub Issues
- **Execution**: GitHub Actions
- **Version Control**: GitHub repositories
- **Triggers**: Issue labels, mentions, assignments
- **Output**: Pull Requests

---

## Option 1: Pure Git (No Platform Required)

### Concept
Bootstrap entirely on a local developer laptop using only Git, no hosting platform needed.

### Architecture
```
work-items/          # Git-tracked work items (markdown files)
  open/
    WI-001.md       # Each file is a work item
    WI-002.md
  in-progress/
  closed/
.git/hooks/         # Git hooks for automation
  post-commit       # Trigger on commits
  post-merge        # Trigger on merges
scripts/
  agent-runner.sh   # Local agent execution
```

### Triggers
- **Git hooks**: `post-commit`, `post-merge`, `pre-push`
- **File watchers**: Watch `work-items/open/` directory
- **Cron jobs**: Scheduled polling
- **Manual**: `./scripts/run-agent.sh WI-001`

### Workflow
1. Create work item: `echo "Build feature X" > work-items/open/WI-042.md`
2. Commit: `git commit -m "Add WI-042"`
3. Hook triggers: `.git/hooks/post-commit` detects new work item
4. Agent runs locally: Aider/Claude Code process work item
5. Results committed: Agent creates commit with changes
6. Move work item: `git mv work-items/open/WI-042.md work-items/closed/`

### Advantages
- **Zero dependencies**: Works on any laptop with Git
- **Offline-first**: No internet required (except LLM API calls)
- **Privacy**: All data stays local
- **Fast iteration**: No CI/CD wait times
- **Cost**: No hosting costs

### Disadvantages
- **Single machine**: No distributed collaboration (unless manually syncing)
- **Manual orchestration**: No built-in parallelization
- **State management**: Harder to coordinate multiple agents
- **No web UI**: Command-line only

### Bootstrap Command
```bash
# One-command bootstrap on blank repo
aider --message "Read BOOTLOADER.md and create a local Git-based automation system with:
- work-items/ directory structure
- .git/hooks/ for automation triggers
- scripts/agent-runner.sh for execution
- Work items as markdown files in Git"
```

---

## Option 2: GitLab

### Differences from GitHub
- **Work Items**: GitLab Issues
- **Execution**: GitLab CI/CD pipelines
- **Triggers**: Issue labels, mentions, merge request comments
- **Output**: Merge Requests

### Example `.gitlab-ci.yml`
```yaml
aider_agent:
  rules:
    - if: '$CI_PIPELINE_SOURCE == "issue"'
      when: always
  script:
    - pip install aider-chat
    - aider --yes --message "$ISSUE_DESCRIPTION"
    - git push origin HEAD:ai/issue-$ISSUE_IID
```

### Advantages
- Self-hosted option (GitLab CE/EE)
- Built-in CI/CD minutes (free tier)
- Tighter integration with merge requests

---

## Option 3: Gitea/Forgejo (Self-Hosted)

### Concept
Lightweight Git hosting for teams wanting full control.

### Architecture
- **Work Items**: Gitea Issues
- **Execution**: Gitea Actions (GitHub Actions compatible) or webhooks
- **Triggers**: Issue webhooks → HTTP endpoint → agent runner
- **Output**: Pull Requests

### Bootstrap Approach
```yaml
# .gitea/workflows/agent.yml (same syntax as GitHub Actions)
name: Agent Runner
on:
  issues:
    types: [opened, labeled]
jobs:
  agent:
    runs-on: self-hosted
    steps:
      - uses: actions/checkout@v4
      - run: aider --message "${{ github.event.issue.body }}"
```

### Advantages
- **Self-hosted**: Complete data sovereignty
- **Lightweight**: Lower resource requirements than GitLab
- **GitHub-compatible**: Can reuse GitHub Actions workflows
- **Open source**: Gitea (MIT), Forgejo (GPL fork)

---

## Option 4: Email-Driven (Universal)

### Concept
Work items are emails. Agent polls inbox, processes tasks, replies with results.

### Architecture
```
Inbox (IMAP)
  └─> Email with subject "[TASK] Build feature X"
       └─> Agent polls via IMAP
            └─> Parses email body as instructions
                 └─> Runs agent (Aider/Claude Code)
                      └─> Commits to Git
                           └─> Replies to email with results
```

### Example Agent Script
```python
import imaplib, email
import subprocess

# Connect to inbox
mail = imaplib.IMAP4_SSL('imap.gmail.com')
mail.login('bot@example.com', 'password')
mail.select('inbox')

# Find tasks
_, messages = mail.search(None, 'SUBJECT "[TASK]"')

for msg_id in messages[0].split():
    # Fetch email
    _, data = mail.fetch(msg_id, '(RFC822)')
    msg = email.message_from_bytes(data[0][1])
    task = msg.get_payload()

    # Run agent
    subprocess.run(['aider', '--yes', '--message', task])

    # Reply with results
    # ...
```

### Advantages
- **Universal**: Works with any email provider
- **Async**: Natural task queue (inbox)
- **Familiar UX**: Everyone knows email
- **Audit trail**: Email threading shows history

### Disadvantages
- **No rich UI**: Plain text/HTML only
- **State management**: Hard to track work item status
- **Authentication**: Email security is complex

---

## Option 5: Slack/Discord (Chat-Driven)

### Concept
Work items are chat messages. Bot listens to channels, processes tasks.

### Architecture
```
Slack Channel: #ai-tasks
  └─> Message: "@agent-bot build feature X"
       └─> Bot detects mention
            └─> Creates branch
                 └─> Runs agent
                      └─> Pushes commit
                           └─> Replies in thread with PR link
```

### Slack Bot Example
```python
from slack_bolt import App

app = App(token="xoxb-...")

@app.event("app_mention")
def handle_mention(event, say):
    task = event['text'].replace('<@BOT_ID>', '').strip()

    # Run agent
    subprocess.run(['aider', '--yes', '--message', task])

    # Reply
    say(f"✅ Completed! PR: https://github.com/...")
```

### Advantages
- **Real-time**: Immediate feedback
- **Context-rich**: Can @mention, thread discussions
- **Team-friendly**: Natural for remote teams
- **Integrations**: Slack/Discord have rich app ecosystems

### Disadvantages
- **Ephemeral**: Chat history may be deleted
- **Noisy**: Lots of chatter mixed with tasks
- **Access control**: Channel-based permissions are coarse

---

## Option 6: Linear/Jira/Asana (Project Management Tools)

### Concept
Work items are native to PM tool. Webhook triggers agent on task creation.

### Architecture (Linear Example)
```
Linear Issue Created
  └─> Webhook fires → https://agent.example.com/webhook
       └─> Agent fetches issue via API
            └─> Processes task
                 └─> Commits to Git
                      └─> Updates Linear issue status
```

### Example Webhook Handler
```javascript
app.post('/webhook', async (req, res) => {
  const issue = req.body.data;

  // Run agent
  execSync(`aider --yes --message "${issue.description}"`);

  // Update Linear
  await linear.updateIssue(issue.id, { state: 'Done' });

  res.send('OK');
});
```

### Advantages
- **Native PM**: Works where teams already track work
- **Rich metadata**: Estimates, labels, dependencies
- **Business-friendly**: Non-technical stakeholders comfortable
- **Reporting**: Built-in dashboards, metrics

### Disadvantages
- **API complexity**: Each tool has different API
- **Cost**: Most PM tools charge per-seat
- **Coupling**: Tight dependency on third-party service

---

## Option 7: File System Watcher (Dropbox/iCloud/Syncthing)

### Concept
Work items are files in a synced folder. Agent watches folder, processes new files.

### Architecture
```
~/Dropbox/ai-tasks/
  pending/
    task-001.txt  # Drop file here
  processing/     # Agent moves here while working
  completed/      # Agent moves here when done

Agent daemon watches ~/Dropbox/ai-tasks/pending/
```

### Watcher Script
```bash
#!/bin/bash
inotifywait -m ~/Dropbox/ai-tasks/pending -e create |
  while read dir action file; do
    task=$(cat "$dir/$file")
    mv "$dir/$file" ~/Dropbox/ai-tasks/processing/

    aider --yes --message "$task"

    mv ~/Dropbox/ai-tasks/processing/"$file" ~/Dropbox/ai-tasks/completed/
  done
```

### Advantages
- **Zero setup**: Uses existing file sync service
- **Cross-platform**: Works on any OS with Dropbox/iCloud
- **Simple**: Just drop a text file
- **No account needed**: Can use local file sync (Syncthing)

### Disadvantages
- **No collaboration**: Single-user oriented
- **Limited metadata**: Files lack structure
- **Sync conflicts**: Multiple agents could collide

---

## Option 8: IFTTT/Zapier/n8n (Workflow Automation)

### Concept
Use no-code workflow tools to route tasks to agent.

### Example (n8n Workflow)
```
Trigger: Google Sheets row added (task description in column A)
  └─> HTTP Request to agent endpoint
       └─> Agent runs
            └─> Update Google Sheets with "Done" status
```

### Advantages
- **No-code**: Non-programmers can set up
- **Integrations**: 1000+ services (Airtable, Notion, etc.)
- **Visual**: Flowchart-based design
- **Flexible**: Mix-and-match triggers/actions

### Disadvantages
- **Rate limits**: Free tiers are restrictive
- **Debugging**: Hard to troubleshoot visual workflows
- **Vendor lock-in**: Workflows not portable

---

## Option 9: Cron + Markdown TODOs (Minimalist)

### Concept
Keep work items in a `TODO.md` file. Cron job scans for unchecked items.

### TODO.md Format
```markdown
# Work Items

- [ ] Build feature X
- [ ] Fix bug Y
- [x] Completed task Z
```

### Cron Script
```bash
# crontab: */15 * * * * ~/agent-cron.sh

#!/bin/bash
# Extract first unchecked item
task=$(grep -m1 "^- \[ \]" TODO.md | sed 's/^- \[ \] //')

if [ -n "$task" ]; then
  # Mark as in-progress
  sed -i "s/^- \[ \] $task/- [~] $task/" TODO.md

  # Run agent
  aider --yes --message "$task"

  # Mark as done
  sed -i "s/^- \[~\] $task/- [x] $task/" TODO.md
fi
```

### Advantages
- **Ultra-simple**: One markdown file
- **No dependencies**: Works anywhere cron runs
- **Version controlled**: TODO.md is in Git
- **Readable**: Plain markdown, human-friendly

### Disadvantages
- **Race conditions**: Cron jobs could collide
- **Limited metadata**: No priorities, labels, etc.
- **Single file**: Doesn't scale to large task lists

---

## Option 10: Database-Driven (SQL/NoSQL)

### Concept
Work items stored in database. Agent polls for `status = 'pending'`.

### Schema (PostgreSQL)
```sql
CREATE TABLE work_items (
  id SERIAL PRIMARY KEY,
  title TEXT,
  description TEXT,
  status TEXT DEFAULT 'pending', -- pending, processing, done
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Agent Loop
```python
import psycopg2

while True:
    # Fetch next pending work item
    cursor.execute("SELECT * FROM work_items WHERE status = 'pending' LIMIT 1")
    item = cursor.fetchone()

    if item:
        # Mark as processing
        cursor.execute("UPDATE work_items SET status = 'processing' WHERE id = %s", (item['id'],))

        # Run agent
        subprocess.run(['aider', '--yes', '--message', item['description']])

        # Mark as done
        cursor.execute("UPDATE work_items SET status = 'done' WHERE id = %s", (item['id'],))

    time.sleep(60)
```

### Advantages
- **Scalable**: Database handles concurrency
- **Queryable**: Rich filtering, sorting, aggregation
- **Reliable**: ACID transactions prevent race conditions
- **Tool-agnostic**: Any app can insert work items

### Disadvantages
- **Infrastructure**: Requires database server
- **Complexity**: More moving parts than file-based
- **No built-in UI**: Need separate web app for browsing

---

## Comparison Matrix

| Option | Setup Complexity | Dependencies | Collaboration | Offline-Capable | Cost |
|--------|------------------|--------------|---------------|-----------------|------|
| **Pure Git** | Low | Git only | Local-first | Yes | Free |
| **GitLab** | Medium | GitLab account | Native | No | Free tier |
| **Gitea** | Medium | Self-host server | Native | No (LAN yes) | Free (hosting cost) |
| **Email** | Low | Email account | Email threading | No | Free |
| **Slack/Discord** | Low | Chat workspace | Real-time | No | Free tier |
| **Linear/Jira** | Medium | PM tool account | Native | No | $$$ |
| **File Sync** | Low | Dropbox/iCloud | Limited | Sync lag | Free tier |
| **IFTTT/Zapier** | Low | Workflow account | Via integrations | No | Free tier |
| **Cron + Markdown** | Very Low | Cron | Single-user | Yes | Free |
| **Database** | High | DB server | Via queries | No (LAN yes) | Hosting cost |

---

## Recommended Generalization Strategy

### Tier 1: Minimum Viable (Laptop Bootstrap)
**Goal**: Zero external dependencies, works offline

```bash
# Bootstrap command
aider --message "Create local Git-based automation:
- work-items/ directory (open/in-progress/closed)
- .git/hooks/post-commit to detect new work items
- scripts/agent-runner.sh to process work items
- Bootstrap itself as first work item"
```

**Use when**: Testing concept, no internet, privacy-critical

### Tier 2: Team Collaboration (Self-Hosted Git)
**Goal**: Small team, full control, low cost

Options:
- **Gitea** (lightweight, GitHub-compatible)
- **GitLab CE** (more features, heavier)

**Use when**: 2-10 person team, internal network, compliance requirements

### Tier 3: Cloud-Native (Hosted Git Platform)
**Goal**: Scale, integrations, managed infrastructure

Options:
- **GitHub** (most integrations, Copilot native)
- **GitLab.com** (CI/CD minutes, free tier generous)

**Use when**: Public projects, large teams, need integrations

### Tier 4: Custom Stack (Database + Webhooks)
**Goal**: Integrate with existing enterprise systems

Architecture:
```
PM Tool (Jira/Linear) → Webhook → Agent Service → Git → PM Tool
```

**Use when**: Enterprise environment, existing PM tools, custom workflows

---

## Implementation Roadmap

### Phase 1: Pure Git (Week 1)
- [ ] Create `work-items/` structure
- [ ] Write `.git/hooks/post-commit` detector
- [ ] Build `scripts/agent-runner.sh`
- [ ] Test: Create work item → commit → auto-process

### Phase 2: Add Remote Sync (Week 2)
- [ ] Support GitHub/GitLab/Gitea as remotes
- [ ] Detect platform-specific features (Actions, CI/CD)
- [ ] Auto-generate workflow files if platform detected

### Phase 3: Multi-Platform (Week 3)
- [ ] Abstract "work item" interface
- [ ] Adapters for: Issue, Email, Chat, File, Database
- [ ] Unified `agent-runner` that works with any adapter

### Phase 4: Bootstrapping (Week 4)
- [ ] Single-command bootstrap for each platform
- [ ] Interactive wizard: "What platform?" → generates setup
- [ ] Self-documenting: Reads BOOTLOADER.md, adapts to context

---

## Key Insights

### Generalization Axes
1. **Work Item Storage**: Issues, Files, Emails, Database, Chat
2. **Trigger Mechanism**: Webhooks, Polling, File Watchers, Cron, Hooks
3. **Execution Environment**: Cloud CI/CD, Local daemon, Serverless functions
4. **Output Format**: Pull Requests, Commits, Email replies, Chat messages

### Universal Patterns
All platforms need:
- **Work item CRUD**: Create, read, update work items
- **Trigger detection**: Know when new work arrives
- **Agent execution**: Run LLM-based agent on work item
- **Result persistence**: Save output (commit, reply, update status)

### Abstraction Layer
```python
class WorkItemAdapter:
    def fetch_pending(self) -> List[WorkItem]: ...
    def mark_in_progress(self, item: WorkItem): ...
    def save_result(self, item: WorkItem, result: str): ...

# Implementations
class GitHubAdapter(WorkItemAdapter): ...
class GitLabAdapter(WorkItemAdapter): ...
class EmailAdapter(WorkItemAdapter): ...
class FileAdapter(WorkItemAdapter): ...
```

---

## Bootstrapping Beyond GitHub

### The Meta Question
Can the bootloader bootstrap itself on ANY platform?

**Yes, if:**
1. Platform has work item concept (issues, tickets, files, messages)
2. Agent can detect new work (webhooks, polling, file watch)
3. Agent can read instructions (e.g., `BOOTLOADER.md`)
4. Agent can write output (commits, replies, status updates)

### Universal Bootstrap Command
```bash
# Works on Git, Email, Chat, PM tools, etc.
agent --message "Read BOOTLOADER.md and adapt it to <PLATFORM>.
Create automation system that:
1. Detects new work items on <PLATFORM>
2. Processes them with an LLM agent
3. Returns results in <PLATFORM>-native format
Bootstrap yourself as the first work item."
```

### Platform-Agnostic BOOTLOADER.md
Add section:
```markdown
## Auto-Detection

This bootloader auto-detects your platform and adapts:

- **Found .git/**: Pure Git mode (local automation)
- **Found .github/**: GitHub mode (Actions)
- **Found .gitlab-ci.yml**: GitLab mode (CI/CD)
- **Found imap credentials**: Email mode (inbox polling)
- **Found slack token**: Slack mode (bot listeners)

Run: `agent --bootstrap` and it figures out the rest.
```

---

## Next Steps

1. **Prototype Tier 1** (Pure Git): Prove local-first works
2. **Document patterns**: Write adapter interface spec
3. **Build CLI tool**: `agentic-primer bootstrap --platform=<git|github|gitlab|email>`
4. **Test on 3 platforms**: GitHub, local Git, email
5. **Iterate**: Refine based on what breaks

Would you like me to prototype any of these options, or drill deeper into a specific platform?
