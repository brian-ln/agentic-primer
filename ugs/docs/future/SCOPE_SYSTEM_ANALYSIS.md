# UGS Scope System - Immediate Need Analysis

## Current Problem: No Node Organization ❌

Looking at your current graph (29 nodes), everything exists in one flat namespace:
- Personal items (grocery lists, ideas) mix with work tasks  
- Demo data (alice, bob) exists alongside real work items
- Future ideas clutter the same space as current tasks
- No way to separate "user:bln" items from "project:auth" items

## Proposed Scope System ✅

### Basic Scope Types to Start:
1. **user:<userid>** - Personal/private items
2. **project:<projectid>** - Project-specific items  
3. **system** - UGS internal configuration
4. **demo** - Sample/test data
5. **global** - Shared/public items

### Scoped Addressing:
```bash
# Instead of flat IDs:
./ugs get alice                    # Ambiguous - which alice?
./ugs get grocery_list            # Could conflict across users

# With scopes:  
./ugs get user:bln.alice          # BLN's personal contact
./ugs get demo.alice              # Demo data
./ugs get user:bln.grocery_list   # BLN's grocery list
./ugs get project:auth.alice      # Alice assigned to auth project
```

## Immediate Implementation Strategy

### Phase 1: Scope-Aware Node IDs
```bash
# Add scope prefix support to existing commands:
./ugs add-node user:bln.grocery_list list item=milk,item=bread
./ugs add-node project:auth.task_1 task title="Login implementation" 
./ugs add-node system.config settings actor_mode=agent

# Backwards compatibility - no scope = global scope:
./ugs add-node alice person        # Same as global.alice
```

### Phase 2: Scope-Aware Queries
```bash
# Scope filtering:
./ugs list-type task --scope=project:auth     # Only auth project tasks
./ugs list-type list --scope=user:bln         # Only BLN's lists
./ugs search "grocery" --scope=user:bln       # Search within scope

# Cross-scope operations:
./ugs path user:bln.alice project:auth.task_1  # Cross-scope relationships
```

### Phase 3: Scope Management
```bash
# Scope operations:
./ugs scopes                           # List all scopes
./ugs scope create project:newproject  # Create new project scope
./ugs scope list user:bln             # List all nodes in BLN's scope
./ugs scope move alice user:bln.alice  # Move node to different scope
```

## Benefits for Current Use Cases

### Personal Knowledge Management:
```bash
./ugs add-node user:bln.grocery_list list
./ugs add-node user:bln.ideas.ai_research topic
./ugs add-node user:bln.contacts.alice person
./ugs add-edge user:bln.meal_plan user:bln.grocery_list depends_on
```

### Project Management:
```bash
./ugs add-node project:ugs.adjacency_lists task
./ugs add-node project:ugs.property_indexing task  
./ugs add-node project:auth.login_task task
./ugs add-edge project:ugs.dijkstra project:ugs.adjacency_lists depends_on
```

### System Organization:
```bash
./ugs add-node system.actor_config settings
./ugs add-node demo.alice person
./ugs add-node global.shared_standards reference
```

## Implementation Requirements

### Core Changes Needed:
1. **Node ID parsing** - Parse scope:id format
2. **Scope validation** - Ensure scope exists/is valid
3. **Query filtering** - Add --scope parameter to commands
4. **Index updates** - Include scope in indexing
5. **Backwards compatibility** - Handle scopeless IDs

### New Commands:
```bash
./ugs scopes                    # List all scopes with counts
./ugs scope create <scope>      # Create new scope
./ugs scope list <scope>        # List nodes in scope
./ugs scope stats <scope>       # Scope-specific statistics
./ugs scope move <old> <new>    # Move node between scopes
```

### Enhanced Existing Commands:
```bash
./ugs list-type task --scope=project:auth    # Scope-filtered queries
./ugs search "term" --scope=user:bln         # Scope-limited search
./ugs traverse start --scope-boundary=strict # Don't cross scopes
```

## Current Graph Reorganization

Your existing nodes could be organized as:
```
demo.alice, demo.bob, demo.charlie
demo.auth_proj, demo.api_proj
demo.task_1, demo.task_2, demo.task_3

project:ugs.adjacency_lists
project:ugs.property_indexing  
project:ugs.dijkstra_pathfinding
project:ugs.performance_benchmarks

user:bln.filesystem_integration     # Future idea
user:bln.vector_embeddings         # Future idea
user:bln.multi_tenant_vision       # Future idea

system._system_config
system._user_prefs
system.performance_target
```

## Integration with Current Enhancement Work

### Property Indexing Enhancement:
- Index scope as a special property
- Fast scope-filtered queries
- Cross-scope relationship discovery

### Navigation Enhancement:  
- Scope-aware traversal with boundary controls
- Cross-scope pathfinding when permitted
- Scope-local vs global navigation modes

### Multi-Tenant Future:
- Scopes become the foundation for domains
- User scopes = private domains
- Project scopes = shared collaborative domains
- System scope = administrative domain

## Priority Assessment

**Medium Priority** - Should be included in current enhancement phase because:
1. **Immediate value** - Organizes existing graph chaos
2. **Foundation for future** - Enables multi-tenant architecture
3. **Simple to implement** - Extends existing ID system
4. **Backwards compatible** - Doesn't break current usage
5. **Scalability** - Prevents namespace conflicts as graph grows

This transforms UGS from "everything in one bucket" to "organized, scoped workspaces" while maintaining the simple graph model.
