# Architecture Documentation

## System Overview

The Agentic Primer is a git-native issue automation system that transforms GitHub issues into executable workflows. The system uses GitHub Actions, issue templates, and a git-tracked knowledge base to automate research, planning, and implementation tasks.

## Core Principles

### 1. Git-Native
Everything lives in Git:
- Issues define work
- Branches isolate work
- PRs review work
- Merges complete work
- History tracks evolution

### 2. Automation-First
Minimize manual work:
- Templates structure input
- Workflows automate processes
- Documentation generates automatically
- Knowledge accumulates naturally

### 3. Knowledge-Driven
Build a knowledge base:
- Git-tracked documentation
- Linked issues and PRs
- Historical context
- Searchable archive

## System Components

### 1. Issue Templates

Location: `.github/ISSUE_TEMPLATE/`

**Purpose:** Provide structured input for automation

**Components:**
- `config.yml` - Template configuration
- `research.yml` - Research task template
- `planning.yml` - Planning task template
- `implementation.yml` - Implementation task template

**Design:**
- YAML form-based templates
- Required and optional fields
- Dropdown selections
- Textarea inputs
- Automatic labels

### 2. GitHub Actions Workflows

Location: `.github/workflows/`

**Purpose:** Automate issue processing and execution

#### Issue Router (`issue-router.yml`)

**Trigger:** Issue opened, labeled, or reopened

**Responsibilities:**
- Detect issue type from labels
- Route to appropriate workflow
- Comment with status and guidance

**Flow:**
```
Issue Event → Check Labels → Comment Status → Guide User
```

#### Research Automation (`research-automation.yml`)

**Trigger:** Issue with `research` label

**Responsibilities:**
- Extract research details from issue body
- Create research branch
- Generate research document template
- Commit and push document
- Create pull request
- Comment on issue

**Flow:**
```
Research Issue → Extract Details → Create Branch → Generate Doc → Commit → Create PR → Comment
```

**Output:**
- Branch: `research/issue-{number}`
- Document: `docs/knowledge/research/issue-{number}.md`
- PR: Auto-created with template content

#### Planning Automation (`planning-automation.yml`)

**Trigger:** Issue with `planning` label

**Responsibilities:**
- Extract planning details from issue body
- Create planning branch
- Generate planning document template
- Commit and push document
- Create pull request
- Comment on issue

**Flow:**
```
Planning Issue → Extract Details → Create Branch → Generate Doc → Commit → Create PR → Comment
```

**Output:**
- Branch: `planning/issue-{number}`
- Document: `docs/knowledge/planning/issue-{number}.md`
- PR: Auto-created with planning structure

#### Implementation Automation (`implementation-automation.yml`)

**Trigger:** Issue with `implementation` label

**Responsibilities:**
- Extract implementation details from issue body
- Create implementation branch
- Generate tracking document
- Commit and push document
- Create pull request
- Comment on issue

**Flow:**
```
Implementation Issue → Extract Details → Create Branch → Generate Doc → Commit → Create PR → Comment
```

**Output:**
- Branch: `implementation/issue-{number}`
- Document: `docs/knowledge/implementation/issue-{number}.md`
- PR: Auto-created for implementation

### 3. Knowledge Base

Location: `docs/knowledge/`

**Purpose:** Git-tracked documentation repository

**Structure:**
```
docs/knowledge/
├── README.md                    # Knowledge base overview
├── research/
│   ├── README.md               # Research docs guide
│   └── issue-{number}.md       # Research documents
├── planning/
│   ├── README.md               # Planning docs guide
│   └── issue-{number}.md       # Planning documents
└── implementation/
    ├── README.md               # Implementation docs guide
    └── issue-{number}.md       # Implementation tracking
```

**Document Types:**

1. **Research Documents**
   - Research questions and findings
   - Methodology and analysis
   - Recommendations
   - References and links

2. **Planning Documents**
   - Architecture overviews
   - Implementation plans
   - Task breakdowns
   - Timelines and milestones

3. **Implementation Documents**
   - Implementation tracking
   - Progress updates
   - Code changes
   - Testing notes

## Data Flow

### Research Flow

```
User Creates Research Issue
    ↓
Issue Router Detects 'research' Label
    ↓
Research Automation Workflow Starts
    ↓
Extract Issue Details (question, context, scope)
    ↓
Create Branch: research/issue-{number}
    ↓
Generate docs/knowledge/research/issue-{number}.md
    ↓
Commit Document to Branch
    ↓
Push Branch to Remote
    ↓
Create Pull Request
    ↓
Comment on Original Issue
    ↓
User Reviews/Enhances PR
    ↓
Merge PR → Knowledge Base Updated
    ↓
Issue Closed
```

### Planning Flow

```
User Creates Planning Issue
    ↓
Issue Router Detects 'planning' Label
    ↓
Planning Automation Workflow Starts
    ↓
Extract Issue Details (objective, requirements)
    ↓
Create Branch: planning/issue-{number}
    ↓
Generate docs/knowledge/planning/issue-{number}.md
    ↓
Commit Document to Branch
    ↓
Push Branch to Remote
    ↓
Create Pull Request
    ↓
Comment on Original Issue
    ↓
User Reviews/Enhances PR
    ↓
Merge PR → Knowledge Base Updated
    ↓
Issue Closed
```

### Implementation Flow

```
User Creates Implementation Issue
    ↓
Issue Router Detects 'implementation' Label
    ↓
Implementation Automation Workflow Starts
    ↓
Extract Issue Details (description, acceptance criteria)
    ↓
Create Branch: implementation/issue-{number}
    ↓
Generate docs/knowledge/implementation/issue-{number}.md
    ↓
Commit Document to Branch
    ↓
Push Branch to Remote
    ↓
Create Pull Request
    ↓
Comment on Original Issue
    ↓
User Implements Feature on Branch
    ↓
User Updates Tracking Document
    ↓
User Requests Review
    ↓
Merge PR → Code and Knowledge Updated
    ↓
Issue Closed
```

## Technical Details

### GitHub Actions

**Runtime:** Ubuntu latest

**Permissions Required:**
- `issues: write` - Comment on issues
- `contents: write` - Create branches, commit files
- `pull-requests: write` - Create PRs

**Key Actions Used:**
- `actions/checkout@v4` - Clone repository
- `actions/github-script@v7` - Execute JavaScript with GitHub API

### Issue Body Parsing

Workflows parse issue body using regex:
```javascript
const field = body.match(/### Field Name\s*\n\s*(.+?)(?=\n###|\n\n###|$)/s)?.[1]?.trim()
```

This extracts content between markdown headers.

### Branch Naming

Convention: `{type}/issue-{number}`

Examples:
- `research/issue-5`
- `planning/issue-10`
- `implementation/issue-15`

### Document Naming

Convention: `issue-{number}.md`

Examples:
- `docs/knowledge/research/issue-5.md`
- `docs/knowledge/planning/issue-10.md`
- `docs/knowledge/implementation/issue-15.md`

## Extensibility

### Adding New Issue Types

1. Create new template in `.github/ISSUE_TEMPLATE/`
2. Add new label
3. Create new workflow in `.github/workflows/`
4. Add directory in `docs/knowledge/`
5. Update router workflow
6. Document in README

### Customizing Document Templates

Edit the heredoc sections in workflow files:
```yaml
cat > docs/knowledge/research/issue-{number}.md << 'EOF'
# Your custom template
EOF
```

### Adding Workflow Steps

Insert new steps in workflow YAML:
```yaml
- name: New Step
  run: |
    # Your commands
```

### Integration Points

Possible integrations:
- **AI Services** - Enhance research/planning
- **Testing Tools** - Validate implementations
- **Deployment** - Auto-deploy on merge
- **Notifications** - Slack, email, etc.
- **Analytics** - Track workflow metrics

## Security Considerations

### Secrets Management

- Use GitHub Secrets for sensitive data
- Never commit credentials
- Limit workflow permissions
- Review generated content

### Input Validation

- Issue templates provide structure
- Workflows validate extracted data
- Markdown is safe for documents
- Shell commands use proper escaping

### Access Control

- Repository permissions control who can create issues
- Branch protection for sensitive branches
- PR reviews required for merges
- Audit trail in Git history

## Performance

### Workflow Execution

- Runs on GitHub's infrastructure
- Fast startup (< 30 seconds)
- Minimal resource usage
- Concurrent execution for multiple issues

### Scalability

- No storage limits (Git-native)
- Unlimited issues
- Parallel workflows
- Git scales well

## Monitoring

### Workflow Status

Check in Actions tab:
- Success/failure status
- Execution logs
- Performance metrics

### Issue Activity

Track via:
- Issue comments
- PR creation
- Branch activity
- Commit history

## Maintenance

### Regular Tasks

- Review generated documents
- Update templates as needed
- Refine workflows based on usage
- Clean up old branches (optional)

### Upgrades

- Update action versions
- Enhance templates
- Improve document structure
- Add new features

## Future Enhancements

Potential improvements:
- AI-powered content generation
- Multi-repository support
- Advanced search and indexing
- Metrics and analytics dashboard
- Integration with project management tools
- Automated testing of workflows
- Custom workflow generators

## Troubleshooting

### Common Issues

**Workflow doesn't trigger:**
- Check label is correct
- Verify workflow file syntax
- Check repository permissions

**PR creation fails:**
- Check branch doesn't exist
- Verify write permissions
- Check base branch exists

**Document not created:**
- Check directory exists
- Verify path in workflow
- Check commit succeeded

### Debug Process

1. Check Actions tab for workflow runs
2. Review workflow logs
3. Verify issue has correct label
4. Check permissions
5. Test with simple issue

## Glossary

- **Issue Template** - YAML form for structured issue creation
- **Workflow** - GitHub Actions automation definition
- **Knowledge Base** - Git-tracked documentation repository
- **Automation** - Triggered workflows that process issues
- **Git-Native** - Using Git as the primary storage and workflow mechanism

---

This architecture enables automated, scalable, git-native issue processing with a growing knowledge base.
