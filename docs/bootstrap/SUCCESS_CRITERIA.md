# Success Criteria

## Goal

**Turn any git repository into an AI-executable workspace that bootstraps itself and remembers everything.**

## Definition of Done (Observable Outcomes)

1. **Functional Test**: System processes a test issue end-to-end without errors
2. **Syntax Valid**: All generated files pass automated validation (yamllint, shellcheck, markdownlint)
3. **Observable Behavior**: GitHub workflow actually triggers on issue creation
4. **Reliability**: 90%+ success rate across 20+ test runs
5. **Multi-Agent**: Works with ≥3 different AI agents (Opus, Sonnet, Haiku)
6. **Single-Command**: Bootstrap completes from bare repo with zero manual intervention
7. **Self-Improvement**: System creates ≥3 successful improvement PRs from its own logs

## Key Principle

Success is measured by **what the system does**, not what files it contains or how it's structured.

## Checkpoint 1: Minimal Viability

- [ ] Bootstrap executed (AI agent processed the seed)
- [ ] Files generated (verified by script)
- [ ] Syntax valid (yamllint, shellcheck pass)
- [ ] Functionally complete (test issue processed successfully)
- [ ] Results logged (pass/fail documented)
