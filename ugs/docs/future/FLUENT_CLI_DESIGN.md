# UGS Fluent CLI Design - Enhanced Command Structure

## Current CLI Structure (Verbose) ❌
```bash
./ugs add-node alice person name=Alice,role=developer
./ugs update-node alice status=active,priority=high
./ugs list-type task
./ugs search "alice" 
./ugs path alice auth_proj
./ugs traverse alice 3
```

**Problems:**
- Repetitive `./ugs` prefix
- Verbose command names (`add-node`, `update-node`)
- No namespace context
- Commands don't flow naturally

## Proposed Fluent CLI Structure ✅

### Core Pattern: `ugs [namespace] <verb> <target> [args...]`

```bash
# Namespace-aware operations
ugs user:bln add grocery_list item=milk,item=bread
ugs project:ugs update adjacency_lists status=in_progress
ugs demo list tasks
ugs system get config

# Natural verb commands
ugs add person alice name=Alice                    # Instead of add-node
ugs update alice status=active,priority=high       # Instead of update-node
ugs list tasks                                     # Instead of list-type task
ugs find alice                                     # Instead of search alice
ugs path alice → auth_proj                         # Natural path notation
ugs explore alice depth=3                          # Instead of traverse

# Fluent chaining with pipes
ugs project:ugs list tasks | grep priority=high
ugs user:bln find ideas | ugs explore depth=2
```

### Implementation Strategy

#### Phase 1: Add Natural Verb Aliases
```bash
# Add fluent verb aliases to existing commands:
ugs add → ugs add-node
ugs update → ugs update-node  
ugs list → ugs list-type
ugs find → ugs search
ugs show → ugs get
ugs explore → ugs traverse
ugs link → ugs add-edge
```

#### Phase 2: Namespace Support  
```bash
# Parse namespace prefix in all commands:
ugs user:bln add idea "research topic"
# Internally: add-node user:bln.research_idea_1 idea description="research topic"

ugs project:ugs list tasks
# Internally: list-type task --filter=scope:project:ugs
```

#### Phase 3: Context Management
```bash
# Add namespace context switching:
ugs use user:bln              # Set UGS_NAMESPACE=user:bln
ugs add idea "new research"   # Auto-scoped to user:bln
ugs use project:ugs           # Switch context
ugs list tasks               # Now shows project:ugs tasks
```

## Current vs Fluent Examples

### Working with Current Graph:
```bash
# Current verbose way:
./ugs add-node user_idea_1 future_idea description="Vector embeddings"
./ugs update-node adjacency_lists status=in_progress
./ugs list-type task
./ugs search "priority=high"

# New fluent way:
ugs user:bln add idea "Vector embeddings"
ugs project:ugs update adjacency_lists status=in_progress  
ugs list tasks
ugs find priority=high
```

### With Namespace Context:
```bash
ugs use project:ugs
ugs list tasks                    # Shows project:ugs tasks
ugs update adjacency_lists status=in_progress
ugs add task property_indexing priority=high

ugs use user:bln  
ugs add idea "Filesystem integration"
ugs find ideas category=data_integration
```

This transforms UGS from a "database CLI" into a "graph workspace CLI" that feels natural and scales with complexity.
