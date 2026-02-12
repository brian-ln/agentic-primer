# UGS Graph Exploration Guide - Current Capabilities

## Your Current Graph: 29 nodes, 26 edges

### 1. Discover Node Types (Current Method):
Try these known types:
```bash
./ugs list-type task           # Work items (8 found)
./ugs list-type person         # People (3 found)  
./ugs list-type project        # Projects (2 found)
./ugs list-type future_idea    # Innovation pipeline (6 found)
./ugs list-type success_criteria  # Goals (3 found)
./ugs list-type future_vision    # Long-term vision (2 found)
./ugs list-type system           # Config (1 found)
./ugs list-type preferences      # User prefs (1 found)
```

### 2. Find Entry Points (Good Starting Nodes):
```bash
# Demo people with lots of connections
./ugs traverse alice 3
./ugs traverse bob 2

# Current work items  
./ugs traverse adjacency_lists 2
./ugs traverse property_indexing 2

# Project hubs
./ugs traverse auth_proj 2
./ugs traverse api_proj 2
```

### 3. Explore Subgraphs:
```bash
# Work planning subgraph
./ugs traverse adjacency_lists 3    # Shows task dependencies

# Team/project subgraph  
./ugs traverse alice 3              # Shows person → tasks → projects

# Innovation pipeline
./ugs traverse multi_tenant_collaboration 2  # Future vision connections
```

### 4. Find Connections:
```bash  
# How are concepts connected?
./ugs path alice auth_proj          # Person to project path
./ugs path adjacency_lists graph_os # Current work to future vision
./ugs path vector_embeddings full_text_search # Innovation relationships
```

### 5. Property-Based Exploration:
```bash
# Find by priority
./ugs search "priority"       # Shows high/medium/low priority items
./ugs search "status"         # Shows todo/done/active items  
./ugs search "category"       # Shows categorized future ideas
```

### 6. Current Graph Structure Overview:

**Core Areas:**
- **Demo Data**: People (alice, bob, charlie) working on projects (auth, api)
- **Current Work**: Enhancement tasks with dependencies and success criteria
- **Future Pipeline**: 6 innovation ideas connected to platform vision
- **Configuration**: System settings and user preferences

**Key Hubs** (most connected):
- `alice` - Person with most task assignments
- `adjacency_lists` - Critical task with multiple dependencies  
- `auth_proj` - Project with multiple tasks
- `graph_os` - Future vision connecting multiple innovations

**Relationship Patterns:**
- Tasks have dependencies (`depends_on` edges)
- Tasks validate success criteria (`validates` edges) 
- People are assigned to tasks (`assigned_to` edges)
- Tasks belong to projects (`belongs_to` edges)
- Ideas extend current work (`extends` edges)
- Ideas enable future visions (`enables` edges)
