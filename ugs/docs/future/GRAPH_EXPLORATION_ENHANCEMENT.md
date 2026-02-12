# UGS Graph Exploration Enhancement Ideas

## Current Exploration Gaps Identified:

### 1. No "List All Nodes" Command
- `./ugs search ""` returns empty (should return all nodes)  
- Need: `./ugs nodes` or `./ugs list-all`

### 2. No Type Discovery Command  
- Must guess node types to use `./ugs list-type <type>`
- Need: `./ugs types` to show all available types with counts

### 3. No Subgraph Visualization
- `./ugs traverse` shows linear paths but not graph structure
- Need: Subgraph extraction with boundaries

### 4. No Entry Point Discovery
- No way to find "root" nodes or highly connected nodes
- Need: `./ugs entry-points` for graph navigation starting points

### 5. No Property Schema Discovery
- Can't see what properties exist across node types  
- Need: `./ugs schema` to show property patterns

### 6. No Graph Structure Overview
- Stats show counts but not structure (density, components, etc.)
- Need: `./ugs structure` for topology analysis

## Enhanced Exploration Commands to Add:

```bash
# Discovery commands
./ugs types                    # List all node/edge types with counts
./ugs nodes [limit]           # List all nodes (paginated)  
./ugs edges [limit]           # List all edges
./ugs schema [type]           # Show property schema for type
./ugs entry-points [limit]    # Find highly connected/root nodes

# Subgraph commands  
./ugs subgraph <start> <depth> [direction]  # Extract bounded subgraph
./ugs neighborhood <node>     # Show immediate neighbors with edge types
./ugs component <node>        # Find connected component containing node

# Structure analysis
./ugs structure              # Graph topology (density, components, etc.)
./ugs degree [node]          # Node connectivity analysis
./ugs clusters               # Find graph communities/clusters
```

## Current Workarounds for Exploration:

### Finding All Node Types:
1. Try common types: task, person, project, future_idea, etc.
2. Look at traverse results to discover new types
3. Check stats for total node count, then estimate coverage

### Finding Entry Points:
1. Use demo data: alice, auth_proj are good starting points
2. Try high-priority tasks: adjacency_lists, property_indexing  
3. Look for nodes with many connections via traverse

### Understanding Subgraphs:
1. Use `./ugs traverse <node> 2-3` from different starting points
2. Use `./ugs path <from> <to>` to see connection patterns
3. Use `./ugs list-type <type>` to see type clusters

### Current Graph Overview (29 nodes, 26 edges):
- **People**: alice, bob, charlie (demo data)
- **Tasks**: current work (adjacency_lists, etc.) + demo tasks  
- **Projects**: auth_proj, api_proj (demo data)
- **Future Ideas**: 6 captured innovation concepts
- **Success Criteria**: performance targets, compatibility requirements
- **System Config**: actor model configuration
- **Relationships**: dependencies, validation, assignment, project membership

## Immediate Enhancement Priorities:

1. **Add `types` command** - Most critical for discovery
2. **Add `nodes` command** - Essential for full graph exploration  
3. **Enhance `traverse`** - Better subgraph visualization
4. **Add `entry-points`** - Help users find starting places
5. **Add `schema`** - Understand data patterns
