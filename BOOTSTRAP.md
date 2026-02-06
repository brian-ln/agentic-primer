# Bootstrap Prompt (30 words)

Create issue-driven development system:
- Issue template (.github/ISSUE_TEMPLATE/task.yml) for @copilot tasks
- CODEOWNERS (* @owner) for PR auto-assignment
- Knowledge base (docs/knowledge/) with patterns/decisions/insights structure
- README with workflow: issue → @copilot → PR → review via web UI

---

## Verification

Generated system must:
1. Process a test issue end-to-end without errors
2. Pass syntax validation (yamllint, shellcheck)
3. GitHub workflow triggers on issue creation

## Success Metrics

- 90%+ success rate across agents (Opus, Sonnet, Haiku)
- ≤10 min bootstrap time
- Works on fresh repos with zero manual setup
