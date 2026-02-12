# UGS Scope System Implementation

## Overview

Implement a 3-tier scope system for UGS: **user**, **project**, and **scratch** to organize nodes and provide isolation.

## Scope Definitions

### 1. **user:** scope
- **Purpose**: Personal, cross-project data and preferences
- **Examples**: personal contacts, bookmarks, preferences, tools
- **Isolation**: Private to individual user
- **Persistence**: Long-term, survives project cleanup

### 2. **project:** scope  
- **Purpose**: Project-specific data and collaboration
- **Examples**: requirements, tasks, milestones, team members
- **Isolation**: Shared within project team
- **Persistence**: Project lifecycle, archived on completion

### 3. **scratch:** scope
- **Purpose**: Temporary work, experiments, demos
- **Examples**: test data, prototypes, temporary calculations  
- **Isolation**: Often disposable, for experimentation
- **Persistence**: Short-term, regular cleanup

## Implementation Strategy

### Phase 1: Scope Prefixing
- Add scope prefix to node IDs: `user:profile`, `project:requirements`, `scratch:demo_data`
- Modify CLI to accept scope prefix: `ugs user: add-node profile`
- Add scope filtering to list/search operations
- Maintain backwards compatibility (no scope = scratch)

### Phase 2: Scoped Commands
- Add scope-aware commands: `ugs user: list-type contact`
- Environment variable support: `UGS_DEFAULT_SCOPE=project:`
- Scope validation and permissions
- Cross-scope linking with explicit addressing

### Phase 3: Advanced Features  
- Scope inheritance and sharing
- Scope-based access control
- Scope lifecycle management
- Import/export by scope

## CLI Syntax Design

```bash
# Explicit scope prefix
ugs user: add-node profile contact name="John Doe"
ugs project: add-node task1 requirement status=active  
ugs scratch: load-demo

# List nodes in specific scope
ugs user: list-type contact
ugs project: search "requirement" 

# Cross-scope operations  
ugs project: add-edge task1 user:profile assigned_to

# Default scope from environment
export UGS_DEFAULT_SCOPE=project:
ugs add-node task2 requirement  # creates project:task2
```

## Current State Analysis

Current UGS has ~33 nodes that need scope organization:
- Demo data → scratch:
- Future ideas → user: or scratch: 
- Work tasks → project:
- Personal preferences → user:

## Implementation Steps

1. **Modify Address class** to support scope prefixes
2. **Update CLI parser** to extract scope from commands
3. **Add scope filtering** to all list/search operations  
4. **Create scope migration** tool for existing data
5. **Add scope validation** and cross-scope reference handling
6. **Update help system** with scope documentation
