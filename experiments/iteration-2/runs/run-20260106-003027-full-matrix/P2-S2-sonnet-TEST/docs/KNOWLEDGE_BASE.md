# Knowledge Base Maintenance Guide

Guide for maintaining and updating the knowledge base that informs @copilot implementations.

## Overview

The knowledge base (KB) is a structured collection of project documentation that @copilot uses to make informed implementation decisions. It includes:

- **Architecture patterns** and design decisions
- **Coding standards** and style guidelines
- **Testing practices** and requirements
- **Technology stack** information
- **External dependencies** and integrations

---

## Structure

```
knowledge-base/
├── index.yml                    # Main index and metadata
├── architecture/
│   ├── patterns.yml            # Design patterns used
│   └── components.yml          # System components
├── practices/
│   ├── coding-standards.yml    # Code style and conventions
│   └── testing.yml             # Testing requirements
└── context/
    ├── tech-stack.yml          # Technologies used
    └── dependencies.yml        # External services
```

---

## File Formats

All KB files use YAML format for:
- Human readability
- Machine parseability
- Git-friendly diffs
- Structured data

### YAML Conventions

```yaml
# Top-level metadata
version: "1.0"
last_updated: "2026-01-06"

description: |
  Multi-line description
  using YAML block syntax

# Structured content
sections:
  - name: Section Name
    description: Short description
    details: |
      Longer explanation with examples

  - name: Another Section
    items:
      - item1
      - item2
```

---

## Content Categories

### 1. Architecture (`architecture/`)

**Purpose:** Document design patterns, system architecture, and component relationships.

**Files:**

#### `patterns.yml`

Documents common design patterns used in the project.

**Structure:**
```yaml
patterns:
  - name: Pattern Name
    description: What it is
    when_to_use: When to apply it
    example: |
      Code example
    files:
      - List of files using this pattern
    considerations:
      - Important notes

anti_patterns:
  - name: Anti-pattern Name
    description: What to avoid
    why_avoid: Reasons
    instead: Better approach
```

**When to Update:**
- New pattern adopted
- Pattern usage changes
- Anti-pattern identified
- Examples need updating

#### `components.yml`

Documents major system components and their interactions.

**Structure:**
```yaml
components:
  - name: Component Name
    path: src/path/to/component
    responsibility: What it does
    technologies:
      - Tech 1
      - Tech 2
    dependencies:
      - Other components it depends on
    patterns:
      - Patterns it uses

data_flow:
  - flow: Flow Name
    steps:
      - Step-by-step description

integration_points:
  - component: Component Name
    integrates_with:
      - External service
```

**When to Update:**
- New component added
- Component responsibilities change
- New integration points
- Data flow changes

### 2. Practices (`practices/`)

**Purpose:** Define coding standards, testing requirements, and team processes.

**Files:**

#### `coding-standards.yml`

Code style guidelines and conventions.

**Structure:**
```yaml
general_principles:
  - principle: Principle Name
    description: What it means
    guidelines:
      - Specific rules

naming_conventions:
  language:
    category:
      - Rule 1
      - Rule 2
    examples: |
      Good and bad examples

code_organization:
  structure:
    description: How to organize
    guidelines:
      - Guidelines

error_handling:
  approach: |
    General approach
  guidelines:
    - Specific rules
  example: |
    Code example
```

**When to Update:**
- Style guidelines change
- New language/framework adopted
- Team consensus on conventions
- Common mistakes identified

#### `testing.yml`

Testing standards and requirements.

**Structure:**
```yaml
testing_philosophy:
  - Guiding principle
  - Another principle

test_types:
  - name: Test Type
    description: What it tests
    when_to_write: When to write these tests
    frameworks:
      - Framework 1
    coverage_target: Percentage
    example: |
      Code example

test_structure:
  pattern: Pattern Name
  description: |
    How to structure tests
  example: |
    Code example
```

**When to Update:**
- Testing approach changes
- New frameworks adopted
- Coverage requirements change
- New test patterns emerge

### 3. Context (`context/`)

**Purpose:** Document technical stack, dependencies, and environmental context.

**Files:**

#### `tech-stack.yml`

Technologies, frameworks, and tools used.

**Structure:**
```yaml
frontend:
  framework:
    name: Framework Name
    version: "X.Y"
    features:
      - Feature 1
  styling:
    framework: CSS Framework
    approach: Approach description

backend:
  framework:
    name: Framework Name
  database:
    name: Database Name
    orm: ORM Name

tooling:
  linting:
    - tool: Tool Name
      config: Config file

environment:
  development:
    - Requirement 1
  commands: |
    npm install
    npm run dev
```

**When to Update:**
- Technology upgraded
- New tool adopted
- Framework changed
- Environment requirements change

#### `dependencies.yml`

External services, APIs, and integrations.

**Structure:**
```yaml
external_services:
  - name: Service Name
    purpose: What it's for
    integration_points:
      - Where it's used
    environment_variables:
      - ENV_VAR_1
    documentation: https://docs.example.com
    rate_limits:
      - Limit description
    error_handling: |
      How to handle errors

cloud_services:
  - name: Service Name
    purpose: What it's for
    features:
      - Feature 1

internal_dependencies:
  - name: Dependency Name
    location: path/to/dependency
    purpose: What it's for
```

**When to Update:**
- New service integrated
- Service configuration changes
- Rate limits change
- API endpoints change

---

## Updating the KB

### When to Update

Update the KB when:
- ✅ New pattern or practice adopted
- ✅ Architecture changes
- ✅ Technology upgraded or replaced
- ✅ Coding standards evolve
- ✅ New service integrated
- ✅ Common mistakes identified
- ✅ Team consensus on approach changes

Don't update for:
- ❌ Minor implementation details
- ❌ Temporary workarounds
- ❌ Project-specific data (issue numbers, user names)
- ❌ Things already documented elsewhere (official docs)

### How to Update

#### 1. Identify What Changed

Ask:
- What pattern/practice changed?
- Which KB file should be updated?
- Is this a new section or update to existing?
- Are examples needed?

#### 2. Edit the YAML File

```bash
# Edit the relevant KB file
code knowledge-base/architecture/patterns.yml
```

Follow the existing structure and conventions.

#### 3. Validate YAML Syntax

```bash
# Validate YAML syntax
yamllint knowledge-base/architecture/patterns.yml

# Or validate all KB files
find knowledge-base -name "*.yml" -exec yamllint {} \;
```

#### 4. Test with Query Script

```bash
# Test that KB can be queried
.github/scripts/knowledge-query.sh "pattern-name authentication"
```

Ensure your new content is returned when relevant keywords are used.

#### 5. Create PR

```bash
git checkout -b update-kb-patterns
git add knowledge-base/architecture/patterns.yml
git commit -m "docs: Update authentication patterns in KB"
git push origin update-kb-patterns
gh pr create --title "Update KB: Authentication patterns"
```

#### 6. Review Process

KB updates should be reviewed by:
- Technical lead (for architecture/patterns)
- Team members (for coding standards)
- Experienced developers (for testing practices)

---

## Best Practices

### Writing KB Entries

#### DO:
✅ **Be specific and actionable**
   - Bad: "Use good error handling"
   - Good: "Use try/catch for expected failures, define custom error classes"

✅ **Include examples**
   ```yaml
   example: |
     // Good example showing the pattern
     async function example() {
       try {
         return await riskyOperation();
       } catch (error) {
         if (error instanceof ValidationError) {
           // Handle expected error
         }
         throw error;
       }
     }
   ```

✅ **Explain WHY, not just WHAT**
   - Bad: "Use service layer"
   - Good: "Use service layer to separate business logic from data access, improving testability"

✅ **Keep it current**
   - Remove outdated information
   - Update version numbers
   - Archive deprecated patterns

✅ **Link to official docs**
   ```yaml
   documentation: https://nextjs.org/docs
   ```

#### DON'T:
❌ **Be vague or ambiguous**
❌ **Include project-specific details** (issue numbers, usernames)
❌ **Duplicate official documentation** (link to it instead)
❌ **Use jargon without explanation**
❌ **Let it go stale** (outdated KB is worse than no KB)

### Organizing Content

#### Hierarchical Structure

Organize from general to specific:

```yaml
# High-level category
patterns:
  # Specific pattern
  - name: Service Layer
    # General description
    description: Business logic encapsulation
    # When to use
    when_to_use: Complex logic, multiple data sources
    # Specific example
    example: |
      Code showing exact implementation
    # Related files
    files:
      - src/services/
```

#### Cross-References

Link related KB entries:

```yaml
patterns:
  - name: Authentication Middleware
    related:
      - architecture/components.yml#Authentication System
      - practices/coding-standards.yml#Error Handling
```

#### Metadata

Include metadata for maintenance:

```yaml
version: "1.0"
last_updated: "2026-01-06"
maintainer: "Engineering Team"
review_frequency: "Quarterly"
```

---

## Query System

### How Queries Work

The knowledge query script searches KB files for relevant context:

1. **Extract keywords** from query text
2. **Search KB files** for keyword matches
3. **Rank results** by relevance
4. **Return relevant sections**

### Query Examples

```bash
# Query for authentication patterns
.github/scripts/knowledge-query.sh "authentication oauth login"

# Query for testing practices
.github/scripts/knowledge-query.sh "testing unit integration jest"

# Query for error handling
.github/scripts/knowledge-query.sh "error handling try catch"
```

### Optimizing for Queries

Make KB entries queryable:

#### Use Descriptive Names

```yaml
# Good: Will match "authentication" queries
- name: Authentication Middleware

# Bad: Too generic
- name: Middleware
```

#### Include Relevant Keywords

```yaml
description: |
  JWT authentication with OAuth 2.0 support for GitHub and Google.
  Handles token generation, validation, refresh, and session management.
```

Keywords: JWT, OAuth, authentication, token, session, GitHub, Google

#### Tag Common Terms

```yaml
tags:
  - authentication
  - oauth
  - jwt
  - security
  - login
```

---

## Version Control

### Commit Messages

Use conventional commits for KB updates:

```bash
# New content
git commit -m "docs(kb): Add service layer pattern"

# Update existing
git commit -m "docs(kb): Update authentication to include OAuth"

# Remove outdated
git commit -m "docs(kb): Remove deprecated authentication pattern"

# Fix errors
git commit -m "fix(kb): Correct YAML syntax in patterns.yml"
```

### Change Log

Document significant KB changes in commits:

```yaml
# In the updated file
version: "1.1"
last_updated: "2026-01-06"

changelog:
  - version: "1.1"
    date: "2026-01-06"
    changes:
      - Added OAuth 2.0 pattern
      - Updated error handling examples
      - Removed deprecated session pattern

  - version: "1.0"
    date: "2025-12-01"
    changes:
      - Initial version
```

---

## Maintenance Schedule

### Weekly
- Review @copilot issues for KB gaps
- Check for outdated examples
- Fix reported issues

### Monthly
- Review recent PRs for emerging patterns
- Update technology versions
- Add new patterns adopted by team

### Quarterly
- Full KB review and update
- Remove deprecated content
- Reorganize if needed
- Solicit team feedback

---

## Common Issues

### KB Not Being Used

**Symptom:** @copilot implementations don't follow KB patterns

**Causes:**
- KB doesn't contain relevant patterns
- Query keywords don't match KB content
- KB is outdated

**Solution:**
1. Review issue description keywords
2. Ensure KB has relevant entries
3. Test query script with those keywords
4. Update KB if missing

### Query Returns Irrelevant Results

**Symptom:** KB query returns unrelated content

**Causes:**
- Overly generic keyword matches
- Poorly structured KB entries

**Solution:**
1. Make KB entries more specific
2. Use descriptive section names
3. Add tags for better matching

### KB Files Too Large

**Symptom:** KB files are hundreds of lines, hard to maintain

**Causes:**
- Too much detail in single file
- Not enough categorization

**Solution:**
1. Split into more granular files
2. Create subcategories
3. Link between files instead of duplicating

---

## Examples

### Example 1: Adding New Pattern

**Scenario:** Team adopts event-driven architecture pattern.

**Steps:**

1. **Edit `architecture/patterns.yml`:**

```yaml
patterns:
  - name: Event-Driven Updates
    description: Pub/sub pattern for real-time updates
    when_to_use: Real-time features, notifications, cross-service communication
    implementation: |
      - Event emitter for in-process events
      - WebSocket for client updates
      - Message queue for distributed systems
    example: |
      // Event emitter
      import { EventEmitter } from 'events';

      const events = new EventEmitter();

      events.on('user.created', (user) => {
        console.log('New user:', user);
        sendWelcomeEmail(user);
      });

      events.emit('user.created', newUser);
    files:
      - src/lib/events.ts
      - src/lib/websocket.ts
    considerations:
      - Events should be well-typed
      - Avoid circular event dependencies
      - Consider error handling for event handlers
```

2. **Update `index.yml`:**

```yaml
keywords:
  events: [architecture/patterns.yml]
  realtime: [architecture/patterns.yml]
  websocket: [architecture/patterns.yml]
```

3. **Test query:**

```bash
.github/scripts/knowledge-query.sh "event realtime websocket"
```

4. **Create PR:**

```bash
git add knowledge-base/architecture/patterns.yml knowledge-base/index.yml
git commit -m "docs(kb): Add event-driven architecture pattern"
git push origin add-event-pattern
gh pr create --title "KB: Add event-driven architecture pattern"
```

### Example 2: Updating Technology

**Scenario:** Upgraded from Next.js 13 to Next.js 14.

**Steps:**

1. **Edit `context/tech-stack.yml`:**

```yaml
frontend:
  framework:
    name: Next.js
    version: "14.x"  # Updated from 13.x
    features:
      - App Router (default)  # Updated
      - Server Components  # New
      - Server Actions  # New
      - React Server Components (RSC)  # New
```

2. **Update changelog:**

```yaml
changelog:
  - version: "1.1"
    date: "2026-01-06"
    changes:
      - "Updated Next.js from 13.x to 14.x"
      - "Added App Router as default"
      - "Added Server Components and Server Actions"
```

3. **Create PR:**

```bash
git add knowledge-base/context/tech-stack.yml
git commit -m "docs(kb): Update Next.js to v14"
git push origin update-nextjs-14
gh pr create --title "KB: Update tech stack for Next.js 14"
```

---

## FAQ

### Q: How much detail should KB entries have?

**A:** Enough to guide implementation, not so much it becomes a tutorial. Link to official docs for comprehensive info.

Good:
```yaml
- Use Prisma for database operations
- Schema location: prisma/schema.prisma
- Documentation: https://prisma.io/docs
```

Too much:
```yaml
- Use Prisma for database operations
- Install: npm install @prisma/client
- Step 1: Create schema file
- Step 2: Define models
- ... (10 more steps)
```

### Q: Should KB include project-specific information?

**A:** No. KB should be general patterns and practices, not specific to issues, users, or transient details.

Good:
```yaml
- Use service layer pattern for business logic
- Store services in src/services/
```

Bad:
```yaml
- Issue #123 requested service layer
- John prefers this pattern
- Current sprint includes refactoring
```

### Q: How do I know what to add to KB?

**A:** Add things that:
- Will be reused in multiple implementations
- Represent team consensus or decisions
- Help maintain consistency
- Would help a new team member

Don't add:
- One-off solutions
- Experimental approaches
- Obvious or standard practices
- Things already well-documented elsewhere

### Q: Can KB be too comprehensive?

**A:** Yes. KB should be focused and maintainable. If it's too large:
- Split into more files
- Remove redundant info
- Link to external docs instead of duplicating
- Archive deprecated content

---

**Last Updated:** 2026-01-06
**Version:** 1.0
