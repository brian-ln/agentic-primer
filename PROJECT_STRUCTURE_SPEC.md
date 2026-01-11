# Project Structure Specification

Complete specification for the Event System project structure, directory organization, file naming conventions, and structural patterns.

## Table of Contents

1. [Overview](#overview)
2. [Root Directory Structure](#root-directory-structure)
3. [Source Code Organization](#source-code-organization)
4. [Documentation Structure](#documentation-structure)
5. [Configuration Files](#configuration-files)
6. [Testing Structure](#testing-structure)
7. [Naming Conventions](#naming-conventions)
8. [File Type Standards](#file-type-standards)
9. [Module Organization Patterns](#module-organization-patterns)
10. [Directory Purpose Matrix](#directory-purpose-matrix)
11. [Expansion Guidelines](#expansion-guidelines)

---

## Overview

The Event System follows a layered architecture with clear separation of concerns. The project structure reflects this organization, grouping related functionality into well-defined directories with consistent naming patterns.

### Core Principles

1. **Separation of Concerns**: Each directory has a single, well-defined purpose
2. **Convention Over Configuration**: Predictable file locations and naming
3. **Scalability**: Structure supports growth without reorganization
4. **Clarity**: Directory names clearly indicate contents
5. **Consistency**: Uniform patterns across all modules

### Structure Philosophy

```
Top Level: Project configuration, entry points, runtime files
├── src/: Implementation (the "how")
├── docs/: Documentation (the "why" and "what")
├── tests/: Verification (the "does it work")
├── functions/: User-defined logic (the "custom behavior")
├── examples/: Demonstrations (the "show me")
└── scripts/: Automation (the "convenience tools")
```

---

## Root Directory Structure

### Complete Directory Tree

```
event-system/
├── .claude/                    # Claude Code configuration
│   └── hooks/                  # PreToolUse, PostToolUse hooks
├── .github/                    # GitHub configuration
│   ├── ISSUE_TEMPLATE/         # Issue templates
│   └── workflows/              # GitHub Actions
├── archive/                    # Historical artifacts
├── docs/                       # Documentation
│   └── knowledge/              # Git-tracked knowledge base
│       ├── decisions/          # Architecture Decision Records
│       ├── insights/           # Learnings and observations
│       └── patterns/           # Reusable patterns
├── examples/                   # Example code and demos
├── experiments/                # Experimental features
│   └── iteration-2/            # Organized by iteration
│       ├── criteria/           # Evaluation criteria
│       ├── prompts/            # Experiment prompts
│       └── runs/               # Experiment execution logs
├── functions/                  # User-defined functions
│   ├── *.js                    # Code functions
│   └── *.agent.js              # Agent functions
├── scripts/                    # Automation scripts
│   └── *.sh                    # Shell scripts
├── src/                        # Source code
│   ├── actors/                 # Actor implementations
│   ├── loop-prevention/        # Loop prevention modules
│   ├── cli.js                  # CLI entry point
│   ├── daemon.js               # Daemon orchestrator
│   └── protocol.js             # UAP implementation
├── tests/                      # Test suite
│   └── fixtures/               # Test data
├── config.json                 # System configuration
├── daemon.log                  # Daemon runtime log
├── events.jsonl                # Event store (append-only)
├── package.json                # Node.js metadata
└── README.md                   # Project overview
```

### Root-Level Files

| File | Purpose | Format | Managed By |
|------|---------|--------|------------|
| `config.json` | System configuration | JSON | User, System |
| `daemon.log` | Daemon output log | Text | System |
| `events.jsonl` | Event store | JSONL | System |
| `package.json` | Node.js package metadata | JSON | Developer |
| `README.md` | Project overview | Markdown | Developer |
| `.gitignore` | Git exclusions | Text | Developer |
| `.gitattributes` | Git file handling | Text | Developer |

### Root-Level Directories

| Directory | Purpose | Owner |
|-----------|---------|-------|
| `.claude/` | Claude Code configuration | Developer |
| `.github/` | GitHub configuration | Developer |
| `archive/` | Historical artifacts | System |
| `docs/` | Documentation | Developer |
| `examples/` | Example code | Developer |
| `experiments/` | Experimental features | Developer |
| `functions/` | User functions | User |
| `scripts/` | Automation tools | Developer |
| `src/` | Source code | Developer |
| `tests/` | Test suite | Developer |

---

## Source Code Organization

### src/ Directory Structure

```
src/
├── actors/                           # Actor implementations
│   ├── event-log.js                  # EventLogActor
│   ├── function-executor.js          # FunctionExecutorActor
│   ├── function-registry.js          # FunctionRegistryActor
│   ├── http-server.js                # HTTPServerActor
│   ├── http-server.test.js           # HTTPServerActor tests
│   ├── pattern-matcher.js            # PatternMatcherActor
│   └── pattern-matcher.test.js       # PatternMatcherActor tests
├── loop-prevention/                  # Loop prevention system
│   ├── ancestry-chain.js             # Ancestry tracking
│   ├── circuit-breaker.js            # Rate limiting
│   ├── depth-counter.js              # Depth tracking
│   ├── fingerprinting.js             # Duplicate detection
│   └── index.js                      # Coordinator
├── cli.js                            # CLI interface
├── daemon.js                         # DaemonActor (orchestrator)
├── loop-prevention.js                # Legacy loop prevention (deprecated)
└── protocol.js                       # Universal Actor Protocol
```

### Actor Module Organization

Each actor follows this internal structure:

```javascript
// actor-name.js

// 1. Imports
import { createWriteStream } from 'fs';
import { promises as fs } from 'fs';

// 2. Constants
const DEFAULT_CONFIG = {...};

// 3. Helper Functions
function generateId() {...}

// 4. Main Actor Class/Factory
export class ActorName {
  constructor(config = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.isRunning = false;
    // Other state
  }

  // Lifecycle methods
  async start() {...}
  async stop() {...}
  getStatus() {...}

  // Public API methods
  async operation() {...}

  // Private methods
  async _internalOperation() {...}
}

// 5. Exports
export default ActorName;
```

### Loop Prevention Module Organization

```
loop-prevention/
├── index.js                 # Coordinator (combines all mechanisms)
├── depth-counter.js         # Layer 1: Depth tracking
├── fingerprinting.js        # Layer 2: Duplicate detection
├── ancestry-chain.js        # Layer 3: Cycle detection
└── circuit-breaker.js       # Layer 4: Rate limiting
```

Each layer exports:
- Factory function (e.g., `createDepthCounter()`)
- Configuration interface
- Check function that returns `{ allowed: boolean, ...details }`

---

## Documentation Structure

### docs/ Directory

```
docs/
└── knowledge/                # Git-tracked knowledge base
    ├── README.md             # Knowledge base overview
    ├── decisions/            # Architecture Decision Records (ADRs)
    │   └── README.md         # Decision index
    ├── insights/             # Learnings and observations
    │   └── README.md         # Insights index
    └── patterns/             # Reusable patterns
        └── README.md         # Patterns index
```

### Documentation at Root Level

```
(root)/
├── ACTOR_LIFECYCLE_SPEC.md           # Actor lifecycle specification
├── ARCHITECTURE_COMPLETE.md          # Complete architecture docs
├── DELIVERABLES_SUMMARY.md           # Project deliverables
├── DEMONSTRATION.md                  # Working demonstrations
├── DOCUMENTATION_INDEX.md            # Documentation navigation
├── EVENT_SYSTEM_QUICK_REF.md         # Quick reference
├── MESSAGE_FLOWS.md                  # Message sequence diagrams
├── PROJECT_OVERVIEW.md               # High-level overview
├── PROJECT_STRUCTURE_SPEC.md         # This document
├── README.md                         # Primary entry point
└── (other legacy docs)
```

### Documentation Types and Naming

| Type | Naming Pattern | Example |
|------|----------------|---------|
| Specification | `*_SPEC.md` | `ACTOR_LIFECYCLE_SPEC.md` |
| Architecture | `ARCHITECTURE*.md` | `ARCHITECTURE_COMPLETE.md` |
| Quick Reference | `*_QUICK_REF.md` | `EVENT_SYSTEM_QUICK_REF.md` |
| Index/Navigation | `*_INDEX.md` | `DOCUMENTATION_INDEX.md` |
| Summary | `*_SUMMARY.md` | `DELIVERABLES_SUMMARY.md` |
| Guide | `*.md` | `DEMONSTRATION.md` |

---

## Configuration Files

### Configuration File Locations

| File | Location | Purpose | Format |
|------|----------|---------|--------|
| `config.json` | `/config.json` | System configuration | JSON |
| `package.json` | `/package.json` | Node.js metadata | JSON |
| `.gitignore` | `/.gitignore` | Git exclusions | Text |
| `.gitattributes` | `/.gitattributes` | Git file handling | Text |

### config.json Structure

```json
{
  "daemon": {
    "port": 0,
    "host": "127.0.0.1",
    "logFile": "daemon.log"
  },
  "eventLog": {
    "file": "events.jsonl",
    "checkpointInterval": 1000
  },
  "http": {
    "port": 3000,
    "host": "localhost"
  },
  "functions": {
    "directory": "functions",
    "autoDiscover": true
  },
  "loopPrevention": {
    "maxDepth": 50,
    "fingerprintCacheSize": 1000,
    "circuitBreakerThreshold": 100,
    "circuitBreakerWindow": 60000
  }
}
```

### Configuration Namespace Organization

```
Root Level Settings
├── daemon.*          → DaemonActor configuration
├── eventLog.*        → EventLogActor configuration
├── http.*            → HTTPServerActor configuration
├── functions.*       → FunctionRegistryActor configuration
└── loopPrevention.*  → Loop Prevention configuration
```

---

## Testing Structure

### tests/ Directory

```
tests/
├── fixtures/                # Test data
│   ├── sample-events.jsonl  # Sample event log
│   └── test-config.json     # Test configuration
├── event-log.test.js        # EventLogActor tests
├── function-executor.test.js # FunctionExecutorActor tests
├── function-registry.test.js # FunctionRegistryActor tests
├── http-server.test.js      # HTTPServerActor tests (in src/actors/)
├── integration.test.js      # Integration tests
├── loop-prevention.test.js  # Loop prevention tests
└── protocol.test.js         # UAP tests
```

### Test File Naming

| Test Type | Pattern | Example |
|-----------|---------|---------|
| Unit Test | `<module>.test.js` | `event-log.test.js` |
| Integration Test | `integration.test.js` | `integration.test.js` |
| Fixtures | `fixtures/<name>.<ext>` | `fixtures/sample-events.jsonl` |

### Test Organization Pattern

```javascript
// module-name.test.js

import { describe, it, expect, beforeEach, afterEach } from 'bun:test';
import { ModuleName } from '../src/module-name.js';

describe('ModuleName', () => {
  let module;

  beforeEach(() => {
    // Setup
    module = new ModuleName();
  });

  afterEach(() => {
    // Cleanup
  });

  describe('Lifecycle', () => {
    it('should start successfully', async () => {...});
    it('should stop successfully', async () => {...});
    it('should return status', () => {...});
  });

  describe('Operations', () => {
    it('should perform operation', async () => {...});
    it('should handle errors', async () => {...});
  });
});
```

---

## Naming Conventions

### File Naming

| Type | Pattern | Example |
|------|---------|---------|
| Actor | `<actor-name>.js` | `event-log.js` |
| Test | `<module>.test.js` | `event-log.test.js` |
| CLI Script | `<command>` | `event-system` |
| Shell Script | `<action>-<noun>.sh` | `create-experiment-run.sh` |
| Function | `<verb>-<noun>.js` | `send-email.js` |
| Agent Function | `<verb>-<noun>.agent.js` | `analyze-error.agent.js` |
| Doc (Spec) | `<TOPIC>_SPEC.md` | `ACTOR_LIFECYCLE_SPEC.md` |
| Doc (General) | `<TOPIC>.md` | `ARCHITECTURE_COMPLETE.md` |

### Directory Naming

| Type | Pattern | Example |
|------|---------|---------|
| Plural Nouns | `<nouns>/` | `actors/`, `tests/`, `functions/` |
| Compound | `<noun>-<noun>/` | `loop-prevention/` |
| Hidden | `.<name>/` | `.github/`, `.claude/` |

### Variable Naming (JavaScript)

| Type | Pattern | Example |
|------|---------|---------|
| Class | `PascalCase` | `EventLogActor` |
| Function | `camelCase` | `createMessage()` |
| Constant | `UPPER_SNAKE_CASE` | `DEFAULT_PORT` |
| Private Method | `_camelCase` | `_countEvents()` |
| Variable | `camelCase` | `eventCount` |
| File-scoped | `camelCase` | `const config = ...` |

### Event Type Naming

| Pattern | Example | Description |
|---------|---------|-------------|
| `<domain>.<action>` | `user.login` | Domain-scoped action |
| `<resource>.<lifecycle>` | `function.executed` | Resource lifecycle event |
| `<system>.<status>` | `system.started` | System status event |

---

## File Type Standards

### JavaScript Files (.js)

**Purpose**: Implementation code

**Structure**:
```javascript
// 1. Imports (external, then internal)
import { foo } from 'external-package';
import { bar } from './local-module.js';

// 2. Constants
const DEFAULT_VALUE = 42;

// 3. Types/Interfaces (JSDoc)
/**
 * @typedef {Object} Config
 * @property {number} port
 */

// 4. Implementation
export class MyClass {...}
export function myFunction() {...}

// 5. Default export (if applicable)
export default MyClass;
```

### Test Files (.test.js)

**Purpose**: Automated testing

**Structure**:
```javascript
import { describe, it, expect } from 'bun:test';

describe('Feature', () => {
  describe('Sub-feature', () => {
    it('should behave correctly', () => {
      // Arrange
      // Act
      // Assert
    });
  });
});
```

### Markdown Files (.md)

**Purpose**: Documentation

**Structure**:
```markdown
# Title

Brief description

## Table of Contents

1. [Section](#section)

## Section

Content

---

## Next Section

Content
```

### JSON Files (.json)

**Purpose**: Configuration and data

**Structure**:
```json
{
  "namespace": {
    "key": "value"
  }
}
```

**Standards**:
- 2-space indentation
- No trailing commas
- Keys in camelCase
- Nested objects for namespacing

### JSONL Files (.jsonl)

**Purpose**: Append-only event log

**Structure**:
```
{"id":"evt_001","timestamp":"...","type":"..."}
{"id":"evt_002","timestamp":"...","type":"..."}
```

**Standards**:
- One JSON object per line
- No pretty-printing
- Must be valid JSON on each line
- Lines separated by `\n`

### Shell Scripts (.sh)

**Purpose**: Automation

**Structure**:
```bash
#!/usr/bin/env bash
set -euo pipefail

# Description of what this script does

main() {
  # Implementation
}

main "$@"
```

---

## Module Organization Patterns

### Pattern 1: Single Actor Module

**Used for**: Simple, self-contained actors

```
actor-name.js
```

**Example**: `pattern-matcher.js`

### Pattern 2: Actor with Tests

**Used for**: Actors requiring co-located tests

```
actors/
├── actor-name.js
└── actor-name.test.js
```

**Example**: `http-server.js` + `http-server.test.js`

### Pattern 3: Multi-Module System

**Used for**: Complex subsystems with multiple components

```
subsystem/
├── index.js           # Coordinator/facade
├── component-1.js     # Individual component
├── component-2.js     # Individual component
└── component-3.js     # Individual component
```

**Example**: `loop-prevention/`

### Pattern 4: Function with Metadata

**Used for**: User-defined functions

```
functions/
├── function-name.js            # Code function
└── function-name.agent.js      # Agent function
```

**Code Function**:
```javascript
// Export default function
export default async function functionName(event, context) {...}

// Optional metadata export
export const metadata = {
  name: 'Function Name',
  description: 'What it does'
};
```

**Agent Function**:
```javascript
// Only metadata export required
export const metadata = {
  name: 'Agent Function Name',
  description: 'What it does',
  agentCommand: 'claude'
};
```

---

## Directory Purpose Matrix

### Core Directories

| Directory | Contents | Owner | Modify Frequency |
|-----------|----------|-------|------------------|
| `src/` | Implementation | Developer | High |
| `src/actors/` | Actor implementations | Developer | Medium |
| `src/loop-prevention/` | Loop prevention | Developer | Low |
| `tests/` | Test suite | Developer | High |
| `tests/fixtures/` | Test data | Developer | Low |
| `functions/` | User functions | User | Variable |
| `docs/` | Documentation | Developer | Medium |
| `docs/knowledge/` | Knowledge base | User/System | Variable |
| `examples/` | Example code | Developer | Low |
| `scripts/` | Automation | Developer | Low |

### Configuration Directories

| Directory | Contents | Owner | Modify Frequency |
|-----------|----------|-------|------------------|
| `.github/` | GitHub config | Developer | Low |
| `.github/workflows/` | Actions | Developer | Low |
| `.github/ISSUE_TEMPLATE/` | Issue templates | Developer | Low |
| `.claude/` | Claude config | Developer | Low |
| `.claude/hooks/` | Claude hooks | Developer | Low |

### Temporary/Generated Directories

| Directory | Contents | Owner | Lifecycle |
|-----------|----------|-------|-----------|
| `archive/` | Historical files | System | Permanent |
| `experiments/` | Experiments | Developer | Per-iteration |
| `experiments/iteration-N/runs/` | Experiment logs | System | Per-run |

---

## Expansion Guidelines

### Adding a New Actor

1. Create actor file: `src/actors/new-actor.js`
2. Implement lifecycle interface: `start()`, `stop()`, `getStatus()`
3. Create test file: `tests/new-actor.test.js`
4. Update `src/daemon.js` to instantiate actor
5. Update `ARCHITECTURE_COMPLETE.md` with actor documentation
6. Update `config.json` with actor configuration namespace

### Adding a New Loop Prevention Layer

1. Create layer file: `src/loop-prevention/new-layer.js`
2. Export factory: `export function createNewLayer(config) {...}`
3. Export check function: `check(event) => { allowed: boolean }`
4. Update `src/loop-prevention/index.js` coordinator
5. Add configuration to `config.json` under `loopPrevention.*`
6. Document in `ARCHITECTURE_COMPLETE.md`

### Adding a New Function

1. Create function file: `functions/verb-noun.js` (code) or `functions/verb-noun.agent.js` (agent)
2. Export default function (code) or metadata (agent)
3. Configure pattern to trigger function
4. Test via HTTP API or CLI
5. Document in knowledge base if reusable

### Adding a New Documentation Category

1. Create category directory: `docs/category/`
2. Create index: `docs/category/README.md`
3. Add entry in `DOCUMENTATION_INDEX.md`
4. Follow markdown conventions

### Adding a New Test Suite

1. Create test file: `tests/feature.test.js`
2. Follow test organization pattern
3. Use fixtures in `tests/fixtures/` for test data
4. Update test runner configuration if needed

---

## File Organization Rules

### Rule 1: Co-location of Related Files

Related files should be in the same directory or adjacent directories:

```
Good:
src/actors/
├── event-log.js
└── event-log.test.js

Bad:
src/actors/event-log.js
tests/event-log/event-log.test.js
```

### Rule 2: Flat Over Nested (When Possible)

Prefer flat structures over deep nesting:

```
Good:
functions/
├── send-email.js
├── log-activity.js
└── analyze-error.agent.js

Bad:
functions/
├── email/
│   └── send.js
├── logging/
│   └── activity.js
└── agents/
    └── analyze-error.agent.js
```

### Rule 3: Tests Mirror Implementation

Test file paths should mirror implementation paths:

```
src/actors/event-log.js       →  tests/event-log.test.js
src/actors/http-server.js     →  tests/http-server.test.js
src/loop-prevention/index.js  →  tests/loop-prevention.test.js
```

### Rule 4: Configuration at Root

Configuration files live at root, not buried:

```
Good:
/config.json
/package.json

Bad:
/config/system.json
/config/package.json
```

### Rule 5: Documentation at Root or docs/

Major documentation at root, detailed docs in `docs/`:

```
/README.md                      # Overview
/ARCHITECTURE_COMPLETE.md       # Major architecture
/docs/knowledge/decisions/      # Detailed ADRs
```

---

## Anti-Patterns to Avoid

### Anti-Pattern 1: Deep Nesting

```
Bad:
src/modules/actors/implementations/event-log/index.js

Good:
src/actors/event-log.js
```

### Anti-Pattern 2: Inconsistent Naming

```
Bad:
src/actors/eventLog.js        # camelCase
src/actors/http-server.js     # kebab-case
src/actors/PatternMatcher.js  # PascalCase

Good:
src/actors/event-log.js
src/actors/http-server.js
src/actors/pattern-matcher.js
```

### Anti-Pattern 3: Mixed Concerns

```
Bad:
src/
├── event-log.js
├── test-event-log.js
├── event-log-docs.md

Good:
src/actors/event-log.js
tests/event-log.test.js
docs/actors/event-log.md
```

### Anti-Pattern 4: Orphaned Files

```
Bad:
src/
├── actors/
├── old-event-log.js.bak
└── temp-test.js

Good:
src/actors/
(Move old files to archive/)
```

---

## Directory Ownership and Responsibilities

### Developer-Owned Directories

**Full control, versioned, reviewed**:
- `src/`
- `tests/`
- `docs/` (except `docs/knowledge/`)
- `scripts/`
- `examples/`
- `.github/`
- `.claude/`

### User-Owned Directories

**User customization, versioned**:
- `functions/`
- `docs/knowledge/` (collaborative)

### System-Owned Directories

**Generated, not versioned (usually)**:
- `node_modules/` (Git ignored)
- `archive/` (optional versioning)
- `experiments/iteration-N/runs/` (logs, usually ignored)

### Mixed Ownership

**Both developer and system write to these**:
- Root directory (config files, logs)
- `events.jsonl` (system writes, developer reads)
- `daemon.log` (system writes, developer reads)

---

## Version Control Strategy

### Always Committed

```
src/
tests/
docs/
scripts/
examples/
functions/
config.json (template)
package.json
README.md
.gitignore
.gitattributes
```

### Never Committed

```
node_modules/
*.log
events.jsonl (runtime data)
.DS_Store
*.swp
```

### Conditionally Committed

```
archive/             # Historical value
experiments/runs/    # Depends on whether logs are useful
config.local.json    # Local overrides (ignored)
.env                 # Secrets (ignored)
```

### .gitignore Structure

```
# Dependencies
node_modules/

# Logs
*.log
daemon.log

# Runtime Data
events.jsonl
test-events.jsonl

# OS Files
.DS_Store
Thumbs.db

# Editor Files
*.swp
*.swo
*~
.vscode/
.idea/

# Local Config
config.local.json
.env
```

---

## Migration and Reorganization

### When to Reorganize

Reorganize directory structure when:
1. Directories exceed 20 files (consider subdirectories)
2. Related files are separated (violates co-location)
3. Naming conventions change project-wide
4. New architectural layer emerges

### How to Reorganize Safely

1. **Plan**: Document current structure and target structure
2. **Branch**: Create reorganization branch
3. **Move**: Use `git mv` to preserve history
4. **Update**: Update all imports/references
5. **Test**: Run full test suite
6. **Document**: Update structure documentation
7. **Review**: Code review before merging
8. **Announce**: Notify team of changes

---

## Structure Validation Checklist

### Pre-Commit Checklist

- [ ] New files follow naming conventions
- [ ] Files in correct directories
- [ ] Tests co-located with implementation
- [ ] Documentation updated if structure changed
- [ ] No deep nesting (max 3 levels)
- [ ] No orphaned files
- [ ] `.gitignore` updated if needed

### Structure Health Checks

Run periodically:

```bash
# Check for orphaned files
find src/ -name "*.bak" -o -name "*~" -o -name "temp*"

# Check for deep nesting
find src/ -type f | awk -F/ '{print NF}' | sort -nr | head -1
# Should be ≤ 5

# Check for large directories
find src/ -type d -exec sh -c 'echo "$(find "$1" -maxdepth 1 -type f | wc -l) $1"' _ {} \; | sort -nr | head -5
# Should be < 20 files per directory

# Check for inconsistent naming
find src/ -type f -name "*.js" | grep -E '[A-Z]|_'
# Should be empty (except test files)
```

---

## Summary

The Event System project structure follows these key principles:

1. **Layered Organization**: Source, tests, docs, config at top level
2. **Co-location**: Related files stay together
3. **Convention**: Predictable naming and organization
4. **Scalability**: Structure supports growth
5. **Clarity**: Purpose is obvious from location

### Key Directories

- `src/` - Implementation
- `src/actors/` - Actor implementations
- `src/loop-prevention/` - Loop prevention system
- `tests/` - Test suite
- `functions/` - User-defined functions
- `docs/` - Documentation
- `scripts/` - Automation

### Key Files

- `config.json` - System configuration
- `events.jsonl` - Event store
- `daemon.log` - Runtime log
- `package.json` - Node.js metadata
- `README.md` - Project overview

### Naming Standards

- Files: `kebab-case.js`
- Directories: `kebab-case/`
- Classes: `PascalCase`
- Functions: `camelCase`
- Constants: `UPPER_SNAKE_CASE`

All aspects of the project structure are designed to support maintainability, scalability, and developer productivity.

---

**Version**: 1.0.0
**Last Updated**: 2026-01-11
**Status**: Complete
