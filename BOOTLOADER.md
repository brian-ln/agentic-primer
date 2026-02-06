# 🚀 Repository Automation Bootloader

Bootstrap a self-automating repository where GitHub Issues are executable work items. Choose your path:

## TL;DR - Quick Start

**Have GitHub Copilot subscription?** → Use Path 1 (Copilot Workspace or CLI)

**No subscription?** → Use Path 3 (Gemini - FREE!) or Path 4 (Aider)

**On GitHub Enterprise without Copilot?** → Use Path 4 (Aider with self-hosted runners)

**Just want to try it locally first?** → Jump to [Scenario 4](#scenario-4-want-to-test-locally-first)

---

## Path 1: GitHub Copilot Workspace (Zero Code)

**Requires:** GitHub Copilot Pro/Business/Enterprise subscription

**Note:** The old `gh copilot` CLI extension was deprecated Oct 25, 2025. Use Copilot Workspace (web-based) or new Copilot CLI instead.

### Option A: Copilot Workspace (Recommended)
**Step 1:** Create bootstrap issue via web UI or CLI:
```bash
gh issue create --title "Bootstrap: Build git-native issue automation system" \
  --body "Build automation system with workflows, templates, and docs for executable issues"
```

**Step 2:** Open in Copilot Workspace:
- Visit issue in browser
- Click "Open in Copilot Workspace" button
- Copilot analyzes and creates implementation plan
- Review and approve → Copilot creates PR

### Option B: New Copilot CLI
**Requires:** Active Copilot subscription + Copilot CLI installed

**Install:**
```bash
# macOS/Linux
brew install copilot-cli

# npm (all platforms)
npm install -g @github/copilot

# Windows
winget install GitHub.Copilot
```

**Authenticate:**
```bash
copilot /login
```

**Use:**
```bash
# Interactive session
copilot

# In the Copilot CLI prompt:
/chat Read BOOTLOADER.md and bootstrap the automation system
```

**Done.** Copilot builds system interactively.

### What if you don't have a Copilot subscription?
Skip to Path 2 (Claude Code - paid API), Path 3 (Gemini - FREE), or Path 4 (Aider - any LLM).

---

## Path 2: Claude Code Action (Official)

**Requires:** Anthropic API key

**Step 1:** Install via Claude CLI:
```bash
# In Claude Code
/install-github-app
```

**Or** manually:
1. Install: https://github.com/apps/claude
2. Add secret: `ANTHROPIC_API_KEY`
3. Create `.github/workflows/claude.yml`:

```yaml
name: Claude Code
on:
  issues:
    types: [opened, assigned]
  issue_comment:
    types: [created]
  pull_request:
    types: [opened]

permissions:
  contents: write
  pull-requests: write
  issues: write

jobs:
  claude:
    runs-on: ubuntu-latest
    steps:
      - uses: anthropics/claude-code-action@v1
        with:
          anthropic_api_key: ${{ secrets.ANTHROPIC_API_KEY }}
          github_token: ${{ secrets.GITHUB_TOKEN }}
```

**Step 2:** Create bootstrap issue and mention `@claude` or assign to Claude

**Done.** Claude builds system and creates PR.

---

## Path 3: Gemini CLI (FREE!)

**Requires:** Google AI Studio account (free)

**Step 1:** Install Gemini CLI and setup:
```bash
# Install Gemini CLI 0.1.18+
# Then run:
gemini-cli /setup-github
```

This automatically:
- Adds workflow from `google-github-actions/run-gemini-cli`
- Configures secrets
- Sets up triggers

**Step 2:** Create issue and mention `@gemini-cli`

**Done.** Gemini builds system (FREE in beta!)

---

## Path 4: Custom Agent (Aider, any LLM)

**For:** Aider, local models, or custom AI CLIs

**Step 1:** Set API key(s):
```bash
gh secret set ANTHROPIC_API_KEY
# or OPENAI_API_KEY, DEEPSEEK_API_KEY, etc.
```

**Step 2:** Create `.github/workflows/aider.yml`:
```yaml
name: Aider Agent
on:
  issues:
    types: [opened, labeled]

jobs:
  aider:
    if: contains(github.event.issue.labels.*.name, 'ai-task')
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Install Aider
        run: pip install aider-chat

      - name: Run Aider on Issue
        env:
          ANTHROPIC_API_KEY: ${{ secrets.ANTHROPIC_API_KEY }}
        run: |
          git config --global user.name "github-actions[bot]"
          git config --global user.email "github-actions[bot]@users.noreply.github.com"

          # Extract issue body
          ISSUE_BODY="${{ github.event.issue.body }}"

          # Run Aider with issue as prompt
          aider --yes --message "$ISSUE_BODY"

      - name: Create PR
        uses: peter-evans/create-pull-request@v6
        with:
          title: "AI: ${{ github.event.issue.title }}"
          body: "Fixes #${{ github.event.issue.number }}"
          branch: "ai/issue-${{ github.event.issue.number }}"
```

**Step 3:** Create issue with label `ai-task`

**Done.** Aider processes issue and creates PR.

---

## Comparison

| Path | Cost | Setup | Trigger | Best For |
|------|------|-------|---------|----------|
| Copilot Workspace | $$ (subscription req'd) | Web UI | Open in Workspace | GitHub Pro/Enterprise users |
| Copilot CLI | $$ (subscription req'd) | Install CLI | Interactive chat | Terminal-first developers with subscription |
| Claude Code | $ (API usage) | 1 command | @claude mention | Claude users |
| Gemini CLI | **FREE** (beta) | 1 command | @gemini-cli | Cost-conscious |
| Custom (Aider) | $ (API usage) | ~40 lines YAML | Labels | Full control, any LLM |

**Important Notes:**
- **GitHub Enterprise (GHE):** If using GHE, Copilot CLI may not be available or require org admin to enable it. Paths 2-4 work with any Git hosting.
- **No Copilot subscription?** Use Path 3 (Gemini - FREE) or Path 4 (Aider with your own API keys).
- **Old `gh copilot` extension:** Deprecated Oct 2025, use new Copilot CLI or Workspace instead.

---

## What Gets Built

All paths create a system with:
- `.github/workflows/` - Issue routing and execution
- `.github/ISSUE_TEMPLATE/` - Research, planning, implementation templates
- `docs/knowledge/` - Git-tracked knowledge base
- Complete documentation

**After bootstrap:** create issue → label/mention/assign → automated execution → PR created

---

## Troubleshooting: No Copilot Access?

### Scenario 1: No Copilot subscription
**Solution:** Use Paths 2-4 (Claude, Gemini, or Aider)

Gemini (Path 3) is **FREE** during beta and works identically to Copilot.

### Scenario 2: GitHub Enterprise Server (GHE) without Copilot
**Solution:** Use Path 4 (Aider) with self-hosted runners

Example workflow modification for GHE:
```yaml
jobs:
  aider:
    runs-on: self-hosted  # Use self-hosted runner
    # ... rest of Path 4 workflow
```

This works on any Git hosting (GitHub, GitLab, Gitea, etc.)

### Scenario 3: Copilot disabled by org admin
**Solution:** Two options:
1. Request admin enable Copilot CLI policy in org settings
2. Use Path 3 (Gemini) or Path 4 (Aider) instead - no org permissions needed

### Scenario 4: Want to test locally first
**Solution:** Skip GitHub Actions entirely:

```bash
# Install Aider locally
pip install aider-chat

# Run directly
aider --message "Bootstrap automation system per BOOTLOADER.md"

# Creates local commits, push when ready
git push origin -u ai/bootstrap
```

Then create PR manually via `gh pr create`.

---

## Sources

- [GitHub Copilot CLI](https://github.com/github/copilot-cli) - New agentic terminal assistant
- [GitHub Copilot CLI Docs](https://docs.github.com/en/copilot/how-tos/use-copilot-agents/use-copilot-cli) - Official documentation
- [gh-copilot (deprecated)](https://github.com/github/gh-copilot) - Old extension, archived Oct 2025
- [Claude Code Action](https://github.com/marketplace/actions/claude-code-action-official)
- [Gemini CLI GitHub Actions](https://blog.google/technology/developers/introducing-gemini-cli-github-actions/)
- [Aider AI](https://github.com/Aider-AI/aider)
