# Design Patterns

This directory contains reusable design patterns, code templates, and architectural approaches used across the project.

## Purpose

Patterns provide proven solutions to recurring problems. They help:
- **Consistency** - Same problems solved the same way
- **Quality** - Battle-tested approaches reduce bugs
- **Onboarding** - New developers learn project conventions
- **AI Context** - GitHub Copilot leverages patterns for suggestions

## Structure

Each pattern document should include:

1. **Pattern Name** - Clear, descriptive name
2. **Problem** - What problem does this solve?
3. **Context** - When should this pattern be used?
4. **Solution** - The pattern itself (code examples, diagrams)
5. **Consequences** - Trade-offs, limitations, benefits
6. **Examples** - Real usage from the codebase
7. **Related Patterns** - Links to similar patterns
8. **References** - External resources, articles

## How to Use

### For Developers
1. Browse this directory before starting new features
2. Copy relevant pattern code as starting point
3. Adapt pattern to specific requirements
4. Link to pattern in PR descriptions

### For GitHub Copilot
Copilot automatically searches this directory when:
- Assigned to issues mentioning pattern keywords
- Generating code similar to existing patterns
- Suggesting completions in files matching pattern contexts

## Pattern Categories

- **API Patterns** - REST endpoint design, error handling
- **Data Patterns** - Database access, caching strategies
- **Security Patterns** - Authentication, authorization, input validation
- **Testing Patterns** - Test structure, mocking, fixtures
- **Architecture Patterns** - Service organization, dependency injection

## Contributing New Patterns

When adding a pattern:
1. Create descriptive filename: `{category}-{pattern-name}.md`
2. Use the template structure above
3. Include at least one working code example
4. Link from related decision records if applicable
5. Update this README's pattern list

## Example Patterns

- [API Error Handling](./api-error-handling.md) - Standard error response format

---

**Last Updated:** 2026-01-06
**Maintainer:** @owner
