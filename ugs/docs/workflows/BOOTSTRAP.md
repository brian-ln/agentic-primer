# UGS BOOTSTRAP - Step-by-Step Recreation Sequence

## Context & Grounding
You are recreating the Universal Graph System (UGS) - a graph database with event sourcing, addressable nodes via @(id) notation, and agent-first CLI design. This document contains the exact sequence of steps that created the current v0.0.4 prototype.

## Prerequisites
- Bun runtime installed
- Basic understanding of graphs (nodes/edges)
- TypeScript familiarity helpful but not required

## Phase 1: Initial Design & Concept (Steps 1-3)

### Step 1: Create Initial Graph Structure
```javascript
// Create prototype.js with basic graph concept
class SimpleGraph {
  constructor() {
    this.nodes = new Map();
    this.edges = new Map();
  }
  
  addNode(id, data = {}) {
    this.nodes.set(id, { id, ...data, created: Date.now() });
    return this.nodes.get(id);
  }
  
  addEdge(id, from, to, data = {}) {
    this.edges.set(id, { id, from, to, ...data, created: Date.now() });
    return this.edges.get(id);
  }
}
```

### Step 2: Add Address Primitive Concept
```javascript
// Add @(id) addressing concept
class Address {
  constructor(id, version = null) {
    this.id = id;
    this.version = version;
  }
  
  toString() {
    return this.version ? `@(${this.id}:${this.version})` : `@(${this.id})`;
  }
}

function $(id, version = null) {
  return new Address(id, version);
}
```

### Step 3: Basic Graph Operations
```javascript
// Add essential operations to SimpleGraph
get(id) {
  return this.nodes.get(id) || this.edges.get(id);
}

traverse(startId, maxDepth = 2) {
  const visited = new Set();
  const results = [];
  
  const visit = (nodeId, depth) => {
    if (depth > maxDepth || visited.has(nodeId)) return;
    
    visited.add(nodeId);
    const node = this.nodes.get(nodeId);
    if (node) results.push({ node, depth });
    
    // Find connected edges
    for (const edge of this.edges.values()) {
      if (edge.from === nodeId) visit(edge.to, depth + 1);
      if (edge.to === nodeId) visit(edge.from, depth + 1);
    }
  };
  
  visit(startId, 0);
  return results;
}
```

## Phase 2: Enhanced Prototype (Steps 4-6)

### Step 4: Add Indexing System
```javascript
// Create enhanced-graph.js with indexing
class EnhancedGraph extends SimpleGraph {
  constructor() {
    super();
    this.adjacencyOut = new Map(); // node -> Set of outgoing edges
    this.adjacencyIn = new Map();  // node -> Set of incoming edges
    this.typeIndex = new Map();    // type -> Set of nodes
  }
  
  addNode(id, type = null, data = {}) {
    const node = super.addNode(id, { type, ...data });
    
    // Update type index
    if (type) {
      if (!this.typeIndex.has(type)) this.typeIndex.set(type, new Set());
      this.typeIndex.get(type).add(id);
    }
    
    return node;
  }
  
  addEdge(id, from, to, type = null, data = {}) {
    const edge = super.addEdge(id, from, to, { type, ...data });
    
    // Update adjacency indices
    if (!this.adjacencyOut.has(from)) this.adjacencyOut.set(from, new Set());
    if (!this.adjacencyIn.has(to)) this.adjacencyIn.set(to, new Set());
    
    this.adjacencyOut.get(from).add(id);
    this.adjacencyIn.get(to).add(id);
    
    return edge;
  }
}
```

### Step 5: Add Pathfinding Algorithm
```javascript
// Add Dijkstra's shortest path
findShortestPath(startId, endId) {
  const distances = new Map();
  const previous = new Map();
  const unvisited = new Set();
  
  // Initialize distances
  for (const nodeId of this.nodes.keys()) {
    distances.set(nodeId, nodeId === startId ? 0 : Infinity);
    unvisited.add(nodeId);
  }
  
  while (unvisited.size > 0) {
    // Find node with minimum distance
    let currentNode = null;
    let minDistance = Infinity;
    
    for (const node of unvisited) {
      if (distances.get(node) < minDistance) {
        minDistance = distances.get(node);
        currentNode = node;
      }
    }
    
    if (currentNode === null || currentNode === endId) break;
    
    unvisited.delete(currentNode);
    
    // Update distances to neighbors
    const outgoing = this.adjacencyOut.get(currentNode) || new Set();
    for (const edgeId of outgoing) {
      const edge = this.edges.get(edgeId);
      const neighbor = edge.to;
      const weight = edge.weight || 1;
      const altDistance = distances.get(currentNode) + weight;
      
      if (altDistance < distances.get(neighbor)) {
        distances.set(neighbor, altDistance);
        previous.set(neighbor, currentNode);
      }
    }
  }
  
  // Reconstruct path
  const path = [];
  let current = endId;
  while (current !== undefined) {
    path.unshift(current);
    current = previous.get(current);
  }
  
  return path.length > 1 ? { path, distance: distances.get(endId) } : null;
}
```

### Step 6: Add Analytics Functions
```javascript
// Graph analytics
getNodeDegree(nodeId) {
  const outDegree = (this.adjacencyOut.get(nodeId) || new Set()).size;
  const inDegree = (this.adjacencyIn.get(nodeId) || new Set()).size;
  return { in: inDegree, out: outDegree, total: inDegree + outDegree };
}

getMostConnected(limit = 5) {
  const degrees = [];
  for (const nodeId of this.nodes.keys()) {
    const degree = this.getNodeDegree(nodeId);
    degrees.push({ id: nodeId, ...degree });
  }
  return degrees.sort((a, b) => b.total - a.total).slice(0, limit);
}
```

## Phase 3: Production TypeScript Implementation (Steps 7-10)

### Step 7: Create TypeScript Graph Engine
```typescript
// Create src/graph.ts with full TypeScript implementation
export class Address {
  id: string;
  version: string | null;
  _isAddress: true = true;
  
  constructor(id: string, version: string | null = null) {
    this.id = id;
    this.version = version;
  }
  
  toString(): string {
    return this.version ? `@(${this.id}:${this.version})` : `@(${this.id})`;
  }
}

export class Node {
  id: string;
  type: string | null;
  properties: Map<string, any>;
  data: any;
  created: number;
  modified: number;
  
  constructor(id: string, type: string | null = null, properties: Record<string, any> = {}, data: any = null) {
    this.id = id;
    this.type = type;
    this.properties = new Map(Object.entries(properties));
    this.data = data;
    this.created = Date.now();
    this.modified = Date.now();
  }
  
  toJSON() {
    return {
      id: this.id,
      type: this.type,
      properties: Object.fromEntries(this.properties),
      data: this.data,
      created: this.created,
      modified: this.modified
    };
  }
}
```

### Step 8: Add Event Sourcing
```typescript
// Add event sourcing to GraphStore
interface UGSEvent {
  type: 'NodeAdded' | 'EdgeAdded' | 'NodeDeleted' | 'EdgeDeleted' | 'NodeUpdated' | 'EdgeUpdated';
  data: any;
  timestamp: number;
}

class GraphStore {
  private events: UGSEvent[] = [];
  private dataDir: string;
  
  async persistEvent(event: UGSEvent): Promise<void> {
    this.events.push(event);
    
    // Write to WAL
    const eventLine = JSON.stringify(event) + '\n';
    await appendFile(join(this.dataDir, 'events.wal'), eventLine);
  }
  
  async replayEvents(): Promise<void> {
    const walPath = join(this.dataDir, 'events.wal');
    if (!existsSync(walPath)) return;
    
    const content = await readFile(walPath, 'utf-8');
    const lines = content.trim().split('\n').filter(line => line);
    
    for (const line of lines) {
      const event = JSON.parse(line);
      this.applyEvent(event);
    }
  }
}
```

### Step 9: Add Snapshots for Performance
```typescript
// Add snapshot capability
async createSnapshot(): Promise<void> {
  const snapshot = {
    timestamp: Date.now(),
    eventCount: this.events.length,
    nodes: Array.from(this.nodes.values()).map(n => n.toJSON()),
    edges: Array.from(this.edges.values()).map(e => e.toJSON())
  };
  
  const snapshotPath = join(this.dataDir, 'snapshot.json');
  await writeFile(snapshotPath, JSON.stringify(snapshot, null, 2));
}

async loadSnapshot(): Promise<boolean> {
  const snapshotPath = join(this.dataDir, 'snapshot.json');
  if (!existsSync(snapshotPath)) return false;
  
  const snapshot = JSON.parse(await readFile(snapshotPath, 'utf-8'));
  
  // Restore nodes
  for (const nodeData of snapshot.nodes) {
    const node = new Node(nodeData.id, nodeData.type, nodeData.properties, nodeData.data);
    node.created = nodeData.created;
    node.modified = nodeData.modified;
    this.nodes.set(node.id, node);
  }
  
  return true;
}
```

### Step 10: Add Full-Text Search Index
```typescript
// Add text indexing
private textIndex: Map<string, Set<string>> = new Map();

private updateTextIndex(id: string, content: string): void {
  const words = content.toLowerCase().split(/\W+/).filter(word => word.length > 2);
  
  for (const word of words) {
    if (!this.textIndex.has(word)) this.textIndex.set(word, new Set());
    this.textIndex.get(word)!.add(id);
  }
}

search(query: string): Node[] {
  const words = query.toLowerCase().split(/\W+/).filter(word => word.length > 2);
  if (words.length === 0) return [];
  
  let candidates: Set<string> | null = null;
  
  for (const word of words) {
    const matches = this.textIndex.get(word) || new Set();
    candidates = candidates ? new Set([...candidates].filter(x => matches.has(x))) : matches;
  }
  
  return Array.from(candidates || [])
    .map(id => this.nodes.get(id))
    .filter(Boolean) as Node[];
}
```

## Phase 4: CLI Interface Development (Steps 11-15)

### Step 11: Create Basic CLI Structure
```typescript
// Create ugs executable with shebang
#!/usr/bin/env bun

import GraphStore from './src/graph.ts';

class UGSCLI {
  private store: GraphStore;
  private commands: Map<string, any> = new Map();
  
  constructor(dataDir?: string) {
    this.dataDir = dataDir || process.env.UGS_DATA_DIR || './data';
    this.store = new GraphStore(this.dataDir);
    this.setupCommands();
  }
  
  private setupCommands(): void {
    this.commands.set('add-node', {
      description: 'Add a new node: add-node <id> [type] [key=value,key=value...]',
      handler: async (args: string[]) => {
        const [id, type, propsStr] = args;
        const properties = this.parseProperties(propsStr);
        const node = await this.store.addNode(id, type || null, properties);
        console.log(`Added node: ${node.id}`);
      }
    });
  }
}
```

### Step 12: Add Actor Model Support
```typescript
// Add actor-aware output
interface OutputFormat {
  isAgent: boolean;
  structured: boolean;
  verbose: boolean;
}

private determineOutputFormat(): OutputFormat {
  const args = process.argv;
  
  if (args.includes('--human')) {
    return { isAgent: false, structured: false, verbose: true };
  }
  if (args.includes('--agent')) {
    return { isAgent: true, structured: true, verbose: false };
  }
  
  const actor = process.env.UGS_ACTOR?.toLowerCase();
  if (actor === 'human') {
    return { isAgent: false, structured: false, verbose: true };
  }
  
  return { isAgent: true, structured: true, verbose: false };
}

private output(data: any, humanMessage?: string): void {
  if (this.outputFormat.isAgent) {
    console.log(JSON.stringify(data, null, 0)); // Compact JSON
  } else {
    if (humanMessage) console.log(humanMessage);
    else console.log(JSON.stringify(data, null, 2)); // Pretty JSON
  }
}
```

### Step 13: Add All Essential Commands
```bash
# Commands to implement:
./ugs add-node <id> [type] [properties]     # Add node
./ugs add-edge <id> <from> <to> [type]      # Add edge
./ugs get <id>                              # Get by ID
./ugs search <query>                        # Full-text search
./ugs list-type <type>                      # List nodes by type
./ugs path <from> <to>                      # Shortest path
./ugs traverse <start> [depth]              # Graph traversal
./ugs stats                                 # Graph statistics
./ugs events [limit]                        # Event history
./ugs snapshot                              # Create snapshot
./ugs help [topic]                          # Help system
./ugs load-demo                             # Demo data
```

### Step 14: Add Interactive Mode
```typescript
// Interactive session support
private async startInteractiveMode(): Promise<void> {
  const readline = await import('readline');
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: this.outputFormat.isAgent ? '' : 'ugs> '
  });
  
  rl.on('line', async (line) => {
    const input = line.trim();
    if (input === 'exit' || input === 'quit') {
      await this.store.shutdown();
      rl.close();
      return;
    }
    
    // Process command
    const parts = input.split(/\s+/);
    const commandName = parts[0];
    const commandArgs = parts.slice(1);
    
    const command = this.commands.get(commandName);
    if (command) {
      await command.handler(commandArgs);
    }
    
    if (!this.outputFormat.isAgent) rl.prompt();
  });
}
```

### Step 15: Add Comprehensive Help System
```typescript
// Help topics for progressive discovery
private setupHelpTopics(): void {
  this.helpTopics.set('concepts', {
    name: 'Core Concepts',
    description: 'What UGS is and how it works',
    details: [
      'UGS is a graph database: nodes (entities) connected by edges (relationships)',
      'Event sourcing: all changes logged, snapshots for performance',
      'Addressing: @(id) notation for referencing any element'
    ],
    examples: [
      './ugs add-node alice person name=Alice',
      './ugs get alice',
      './ugs search Alice'
    ],
    seeAlso: ['getting-started', 'workflow']
  });
}
```

## Phase 5: Documentation & Polish (Steps 16-20)

### Step 16: Create Package Configuration
```json
{
  "name": "universal-graph-system",
  "version": "0.0.4",
  "description": "Graph database prototype with event sourcing",
  "type": "module",
  "bin": {
    "ugs": "./ugs"
  },
  "scripts": {
    "demo": "bun run ./ugs load-demo",
    "cli": "bun run ./ugs"
  }
}
```

### Step 17: Add Demo Data System
```typescript
// Demo data for testing
private async loadDemoData(): Promise<void> {
  // People
  await this.store.addNode('alice', 'person', { name: 'Alice Johnson', role: 'developer' });
  await this.store.addNode('bob', 'person', { name: 'Bob Smith', role: 'manager' });
  
  // Projects
  await this.store.addNode('auth_proj', 'project', { name: 'Authentication System' });
  
  // Tasks
  await this.store.addNode('task_1', 'task', { title: 'Implement Login', status: 'active' });
  
  // Relationships
  await this.store.addEdge('e1', 'alice', 'task_1', 'assigned_to');
  await this.store.addEdge('e4', 'task_1', 'auth_proj', 'belongs_to');
  
  console.log('Demo data loaded!');
}
```

### Step 18: Error Handling & Validation
```typescript
// Basic error handling
private error(message: string, code: number = 1): void {
  if (this.outputFormat.isAgent) {
    console.error(JSON.stringify({ error: message, code }));
  } else {
    console.error(`❌ Error: ${message}`);
  }
  process.exit(code);
}

private success(message: string, data?: any): void {
  if (this.outputFormat.isAgent) {
    this.output({ success: true, message, data });
  } else {
    console.log(`✓ ${message}`);
    if (data) this.output(data);
  }
}
```

### Step 19: Make Executable
```bash
# Make CLI executable
chmod +x ./ugs

# Test basic functionality
./ugs help
./ugs load-demo
./ugs stats
./ugs search alice
./ugs path alice auth_proj
```

### Step 20: Final Testing & Documentation
```bash
# Test all major features
./ugs add-node test_node person name=Test
./ugs add-edge test_edge alice test_node knows
./ugs get test_node
./ugs traverse alice 2
./ugs events 5
./ugs snapshot
```

## Current State: UGS v0.0.4 Prototype

### What You Now Have:
- ✅ **Working graph database** with nodes, edges, properties
- ✅ **Event sourcing** with WAL and snapshots
- ✅ **CLI interface** with 12 essential commands
- ✅ **Actor model** for agent vs human output
- ✅ **Basic indexing** for fast lookups
- ✅ **Pathfinding** with Dijkstra's algorithm
- ✅ **Full-text search** capability
- ✅ **Help system** with progressive discovery
- ✅ **Demo data** for testing

### What You Don't Have (Next Phase):
- ❌ **Multi-user session management**
- ❌ **Advanced property indexing**
- ❌ **Temporal versioning** (@(id:version) resolution)
- ❌ **Performance benchmarks**
- ❌ **Production error handling**
- ❌ **Automated testing**

## Success Verification:
```bash
./ugs load-demo
./ugs stats  # Should show nodes, edges, events
./ugs search alice  # Should find Alice
./ugs path alice auth_proj  # Should find connection path
UGS_ACTOR=human ./ugs help  # Should show human-friendly help
UGS_ACTOR=agent ./ugs stats  # Should show structured JSON
```

If all commands work, you have successfully recreated UGS v0.0.4!
