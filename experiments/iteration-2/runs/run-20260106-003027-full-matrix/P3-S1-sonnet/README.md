# Issue-Driven Development with @copilot

This repository uses an issue-driven development workflow where AI agents (like @copilot) autonomously complete tasks and submit pull requests for human review.

## Workflow

```
1. Create Issue → 2. Assign @copilot → 3. Agent Works → 4. PR Created → 5. Human Reviews → 6. Merge
```

### Step-by-Step Process

#### 1. Create a Task Issue

Go to GitHub Issues and click "New Issue"

Select the **"Copilot Task"** template and fill out:
- **Task Description**: Clear explanation of what needs to be done
- **Acceptance Criteria**: Checklist defining "done"
- **Context**: Any relevant background, constraints, or related issues
- **Priority**: How urgent is this task
- **Files**: Which files are likely involved (optional)

The issue will automatically:
- Get labeled with `copilot-task`
- Be assigned to @copilot
- Appear in @copilot's queue

#### 2. @copilot Receives the Issue

The agent:
- Reads the issue description and acceptance criteria
- Checks the knowledge base for relevant patterns and decisions
- Plans the implementation approach
- Identifies files to modify or create

#### 3. @copilot Implements the Solution

The agent:
- Creates or modifies files as needed
- Writes or updates tests
- Updates documentation
- Follows existing patterns and conventions
- Records new insights in the knowledge base

#### 4. @copilot Creates a Pull Request

The agent:
- Creates a feature branch
- Commits changes with descriptive messages
- Opens a pull request with:
  - Summary of changes
  - How it addresses the issue
  - Testing performed
  - Link to original issue

The PR is automatically assigned to the repository owner (via CODEOWNERS).

#### 5. Human Reviews via Web UI

You review the PR on GitHub:
- Check the changes make sense
- Verify tests pass
- Ensure documentation is updated
- Request changes if needed

**All review happens through the GitHub web interface** - no CLI needed.

#### 6. Merge or Request Changes

- **If approved**: Merge the PR, issue auto-closes
- **If changes needed**: Comment on the PR, @copilot responds with updates

## Repository Structure

```
.github/
├── ISSUE_TEMPLATE/
│   └── task.yml          # Issue template for @copilot tasks
└── CODEOWNERS            # Auto-assigns PRs to repository owner

docs/
└── knowledge/            # AI-accessible knowledge base
    ├── patterns/         # Reusable design patterns
    ├── decisions/        # Architecture decision records (ADRs)
    └── insights/         # Learnings from completed work
```

## Knowledge Base

The knowledge base in `docs/knowledge/` helps @copilot and humans understand:
- **How to solve problems** (patterns)
- **Why decisions were made** (decisions)
- **What was learned** (insights)

### For @copilot
- Reads patterns before implementing features
- Follows documented decisions
- Adds insights after completing tasks

### For Humans
- Browse to understand project conventions
- Add entries when making decisions
- Reference in code reviews

See `docs/knowledge/README.md` for details.

## Setup

### First Time Setup

1. **Update CODEOWNERS**: Replace `@owner` with your GitHub username
   ```bash
   # Edit .github/CODEOWNERS
   * @your-github-username
   ```

2. **Enable Issues**: Ensure GitHub Issues are enabled in repository settings

3. **Configure @copilot**: Set up your AI agent with repository access

4. **Create Test Issue**: Verify workflow with a simple test task

### Testing the System

Create a test issue:
- **Title**: "Test: Verify issue-driven workflow"
- **Task**: Add a simple `hello.txt` file with "Hello, World!"
- **Acceptance Criteria**: File exists and contains the text

Assign to @copilot and verify:
- Agent receives and processes the issue
- PR is created automatically
- PR is assigned to you for review
- Changes are correct

## Tips for Writing Good Issues

### Be Specific
✅ "Add endpoint at /api/users/:id that returns user profile JSON"
❌ "Add user endpoint"

### Define Success
✅ Include checklist of acceptance criteria
❌ Vague "make it work" descriptions

### Provide Context
✅ Link related issues, mention constraints, reference docs
❌ Assume agent knows everything

### Scope Appropriately
✅ One clear task per issue
❌ "Rewrite entire application"

## Troubleshooting

### Issue not assigned to @copilot
- Check issue template is filled out correctly
- Verify @copilot has repository access
- Manually assign if needed

### PR not assigned for review
- Check `.github/CODEOWNERS` has correct username
- Ensure file is in repository root
- Verify GitHub username format: `@username`

### Agent doesn't follow patterns
- Check knowledge base is populated
- Ensure agent has read access to `docs/knowledge/`
- Add patterns/decisions as needed

### Changes don't match expectations
- Review issue description for clarity
- Add more detailed acceptance criteria
- Provide examples in issue context

## Benefits

### For Humans
- Focus on review instead of implementation
- Clear audit trail of all changes
- Knowledge preserved across time
- Work continues 24/7

### For @copilot
- Clear task specifications
- Reusable patterns and decisions
- Structured feedback via PR comments
- Knowledge base for context

### For the Team
- Institutional knowledge captured
- Consistent patterns enforced
- Decisions documented
- Insights shared

## Contributing

1. Create issue using template
2. Assign to @copilot or self
3. Follow existing patterns
4. Document decisions
5. Share insights

## License

(Add your license here)

## Questions?

Check the knowledge base first, then create an issue with the question.
