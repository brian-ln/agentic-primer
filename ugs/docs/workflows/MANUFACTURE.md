# UGS MANUFACTURE - Optimized Build Process

## Objective
Build UGS v0.0.4 prototype in the most efficient way using parallel development and optimized task sequencing.

## Prerequisites Setup (Parallel - 2 minutes)
```bash
# Terminal 1: Environment
mkdir ugs-build && cd ugs-build
bun init -y
npm pkg set type="module"
npm pkg set bin.ugs="./ugs"

# Terminal 2: Directory structure
mkdir -p src data
touch src/graph.ts ugs README.md
chmod +x ugs
```

## Core Development (Parallel Tracks)

### Track A: Graph Engine (src/graph.ts) - 15 minutes
**Priority 1 - Foundation Classes**
```typescript
// Implement in this exact order (dependencies matter):
1. Address class with toString() and static isAddress()
2. Node class with Map-based properties and toJSON()
3. Edge class with from/to and weight support
4. UGSEvent interface for event sourcing
```

**Priority 2 - GraphStore Core**
```typescript
// Parallel implementation (independent):
- Parallel A1: Basic Maps (nodes, edges, adjacency)
- Parallel A2: Event array and persistEvent()
- Parallel A3: File I/O methods (loadSnapshot, replayWAL)
```

**Priority 3 - Graph Operations**
```typescript
// Sequential (each depends on previous):
1. addNode/addEdge with index updates
2. Basic search with text indexing  
3. Dijkstra pathfinding algorithm
4. Traverse with BFS/DFS
```

### Track B: CLI Interface (ugs) - 12 minutes
**Priority 1 - CLI Bootstrap**
```typescript
// Can start immediately:
1. Shebang and imports
2. UGSCLI class skeleton
3. OutputFormat interface and determineOutputFormat()
4. Basic output/error/success methods
```

**Priority 2 - Command Infrastructure**
```typescript
// Parallel command setup (independent):
- Parallel B1: setupCommands() with command Map
- Parallel B2: parseProperties() utility
- Parallel B3: Interactive mode with readline
- Parallel B4: Help system with topics Map
```

**Priority 3 - Command Implementations**
```typescript
// Implement in dependency order:
1. Essential: add-node, add-edge, get (depend on Track A Priority 2)
2. Search: search, list-type (depend on Track A Priority 3)  
3. Analysis: path, traverse, stats (depend on Track A Priority 3)
4. System: events, snapshot, help, load-demo (independent)
```

### Track C: Documentation & Polish - 8 minutes
**Parallel with development:**
- C1: Help topics (can write while coding)
- C2: Demo data structure (independent)
- C3: Package.json configuration (independent)
- C4: Basic README with usage examples

## Optimized Build Sequence

### Phase 1: Foundation (5 minutes)
```bash
# Parallel execution:
# Terminal 1: Track A Priority 1 (Address, Node, Edge classes)
# Terminal 2: Track B Priority 1 (CLI bootstrap)
# Terminal 3: Track C1 (Help content writing)
```

### Phase 2: Core Systems (10 minutes)
```bash
# Parallel execution:
# Terminal 1: Track A Priority 2 (GraphStore core)
# Terminal 2: Track B Priority 2 (Command infrastructure) 
# Terminal 3: Track C2,C3,C4 (Demo data, package.json, README)
```

### Phase 3: Integration (10 minutes)
```bash
# Sequential integration:
1. Track A Priority 3 (algorithms) - depends on GraphStore
2. Track B Priority 3 (commands) - depends on GraphStore + algorithms
3. Integration testing and bug fixes
```

### Phase 4: Verification (5 minutes)
```bash
# Test sequence:
./ugs load-demo
./ugs stats
./ugs search alice  
./ugs path alice auth_proj
UGS_ACTOR=human ./ugs help
UGS_ACTOR=agent ./ugs stats
```

## Critical Path Dependencies
```
Address/Node/Edge → GraphStore Core → Algorithms → CLI Commands → Testing
      ↓                    ↓              ↓           ↓
   5 min              +5 min         +10 min    +10 min = 30 min total
```

## Parallel Optimization Points

### What Can Be Done Simultaneously:
- ✅ **Help content** while coding core logic
- ✅ **Demo data structure** while building graph engine
- ✅ **CLI boilerplate** while implementing graph operations
- ✅ **Package configuration** while writing algorithms
- ✅ **Error handling patterns** while building commands

### What Must Be Sequential:
- ❌ **Commands cannot be implemented before GraphStore**
- ❌ **GraphStore cannot work without Node/Edge classes**
- ❌ **Pathfinding cannot work without adjacency indices**
- ❌ **Testing cannot happen without complete implementation**

## Resource Allocation Strategy

### Single Developer (30 minutes total):
1. **Minutes 0-5**: Foundation classes (Address, Node, Edge)
2. **Minutes 5-15**: GraphStore core + CLI bootstrap (parallel focus switching)
3. **Minutes 15-25**: Algorithms + Command implementation (sequential)
4. **Minutes 25-30**: Integration testing and polish

### Team of 3 (15 minutes total):
- **Developer A**: Graph engine track (src/graph.ts)
- **Developer B**: CLI interface track (ugs executable)  
- **Developer C**: Documentation and configuration track
- **Minutes 10-15**: Integration and testing (all developers)

## Quality Gates

### Gate 1 (10 minutes): Core Functionality
- [ ] Graph operations work (add-node, add-edge, get)
- [ ] Basic CLI responds to commands
- [ ] Event sourcing saves to files

### Gate 2 (20 minutes): Full Features  
- [ ] Search and pathfinding work
- [ ] Interactive mode functional
- [ ] Actor model switches output correctly

### Gate 3 (30 minutes): Production Ready
- [ ] All 12 commands implemented
- [ ] Help system complete
- [ ] Demo data loads and queries work
- [ ] Error handling graceful

## Optimization Techniques

### Development Speed:
- **Copy-paste base patterns** rather than writing from scratch
- **Use console.log debugging** instead of formal debugging
- **Implement happy path first**, error handling later
- **Use TypeScript any type** initially, refine later

### Code Efficiency:
- **Maps over arrays** for O(1) lookups
- **Set operations** for fast membership testing
- **JSON.stringify/parse** for simple persistence
- **Readline interface** for interactive mode

### Testing Strategy:
- **Demo data as primary test case** - if demo works, system works
- **Command-line testing** instead of unit tests initially
- **Agent/human output verification** with UGS_ACTOR switching
- **Event log inspection** to verify persistence

## Success Metrics
- **30 minutes or less** for complete implementation
- **12 working commands** with proper error handling  
- **Actor model** switches between structured/human output
- **Persistence** survives restart (events.wal + snapshot.json)
- **Demo scenario** completes successfully from start to finish

This process produces a **fully functional UGS v0.0.4 prototype** ready for enhancement to multi-actor session management.
