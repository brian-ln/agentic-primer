#!/usr/bin/env bun
/**
 * Universal Graph System (UGS) - Prototype Implementation
 * Self-executing script demonstrating core concepts
 */

// Address primitive representation
class Address {
  constructor(id) {
    this.id = id;
    this._isAddress = true;
  }
  
  toString() {
    return `@(${this.id})`;
  }
  
  static isAddress(obj) {
    return obj && obj._isAddress === true;
  }
}

// Factory function for clean syntax
function $(id) {
  return new Address(id);
}

// Core node structure
class Node {
  constructor(id, type = null, properties = {}, data = null) {
    this.id = id;
    this.type = type;
    this.properties = new Map(Object.entries(properties));
    this.data = data;
  }
  
  toJSON() {
    return {
      id: this.id,
      type: this.type,
      properties: Object.fromEntries(this.properties),
      data: this.data
    };
  }
}

// Core edge structure  
class Edge {
  constructor(id, from, to, type = null, properties = {}) {
    this.id = id;
    this.from = from;
    this.to = to;
    this.type = type;
    this.properties = new Map(Object.entries(properties));
  }
  
  toJSON() {
    return {
      id: this.id,
      from: this.from,
      to: this.to,
      type: this.type,
      properties: Object.fromEntries(this.properties)
    };
  }
}

// Event types for event sourcing
class Event {
  constructor(type, data, metadata = {}) {
    this.timestamp = Date.now();
    this.type = type;
    this.data = data;
    this.metadata = metadata;
  }
}

// Main graph store with event sourcing
class GraphStore {
  constructor() {
    this.nodes = new Map();
    this.edges = new Map(); 
    this.eventLog = [];
    this.indices = new Map(); // For fast property lookups
  }
  
  // Event sourcing - all changes go through events
  emit(eventType, data, metadata = {}) {
    const event = new Event(eventType, data, metadata);
    this.eventLog.push(event);
    this._applyEvent(event);
    return event;
  }
  
  _applyEvent(event) {
    switch(event.type) {
      case 'NodeAdded':
        this._addNode(event.data);
        break;
      case 'EdgeAdded': 
        this._addEdge(event.data);
        break;
      case 'NodeUpdated':
        this._updateNode(event.data);
        break;
      case 'EdgeUpdated':
        this._updateEdge(event.data);
        break;
      // Add more event types as needed
    }
  }
  
  _addNode(nodeData) {
    const node = new Node(
      nodeData.id, 
      nodeData.type, 
      nodeData.properties || {}, 
      nodeData.data
    );
    this.nodes.set(node.id, node);
    this._updateIndices('node', node);
  }
  
  _addEdge(edgeData) {
    const edge = new Edge(
      edgeData.id,
      edgeData.from,
      edgeData.to, 
      edgeData.type,
      edgeData.properties || {}
    );
    this.edges.set(edge.id, edge);
    this._updateIndices('edge', edge);
  }
  
  _updateIndices(elementType, element) {
    // Build indices for fast property-based lookups
    // Implementation details omitted for brevity
  }
  
  // Public API
  addNode(id, type, properties = {}, data = null) {
    return this.emit('NodeAdded', { id, type, properties, data });
  }
  
  addEdge(id, from, to, type = null, properties = {}) {
    return this.emit('EdgeAdded', { id, from, to, type, properties });
  }
  
  get(id) {
    return this.nodes.get(id) || this.edges.get(id);
  }
  
  // Address resolution
  resolve(address) {
    if (!Address.isAddress(address)) {
      return address; // Not an address, return as-is
    }
    return this.get(address.id);
  }
  
  // Deep resolution of properties containing addresses
  resolveDeep(obj) {
    if (Address.isAddress(obj)) {
      const resolved = this.resolve(obj);
      return resolved ? this.resolveDeep(resolved) : obj;
    }
    
    if (Array.isArray(obj)) {
      return obj.map(item => this.resolveDeep(item));
    }
    
    if (obj && typeof obj === 'object') {
      if (obj instanceof Map) {
        const result = new Map();
        for (const [key, value] of obj) {
          result.set(key, this.resolveDeep(value));
        }
        return result;
      } else {
        const result = {};
        for (const [key, value] of Object.entries(obj)) {
          result[key] = this.resolveDeep(value);
        }
        return result;
      }
    }
    
    return obj;
  }
  
  // Simple query interface
  findNodes(filter = {}) {
    const results = [];
    
    for (const node of this.nodes.values()) {
      let matches = true;
      
      for (const [key, expectedValue] of Object.entries(filter)) {
        const actualValue = node.properties.get(key) || node[key];
        
        // Handle address comparison
        if (Address.isAddress(expectedValue)) {
          if (!Address.isAddress(actualValue) || actualValue.id !== expectedValue.id) {
            matches = false;
            break;
          }
        } else if (actualValue !== expectedValue) {
          matches = false;
          break;
        }
      }
      
      if (matches) {
        results.push(node);
      }
    }
    
    return results;
  }
  
  // Graph traversal
  traverse(startId, options = {}) {
    const { 
      direction = 'out', // 'out', 'in', 'both'
      type = null,       // edge type filter
      depth = 1          // traversal depth
    } = options;
    
    const visited = new Set();
    const results = [];
    const queue = [{id: startId, currentDepth: 0}];
    
    while (queue.length > 0) {
      const {id, currentDepth} = queue.shift();
      
      if (visited.has(id) || currentDepth >= depth) {
        continue;
      }
      
      visited.add(id);
      const node = this.nodes.get(id);
      if (node) {
        results.push(node);
        
        // Find connected edges
        for (const edge of this.edges.values()) {
          let nextNodeId = null;
          
          if (direction === 'out' && edge.from === id) {
            nextNodeId = edge.to;
          } else if (direction === 'in' && edge.to === id) {
            nextNodeId = edge.from;
          } else if (direction === 'both' && (edge.from === id || edge.to === id)) {
            nextNodeId = edge.from === id ? edge.to : edge.from;
          }
          
          if (nextNodeId && (!type || edge.type === type)) {
            queue.push({id: nextNodeId, currentDepth: currentDepth + 1});
          }
        }
      }
    }
    
    return results;
  }
  
  // Export current state 
  snapshot() {
    return {
      nodes: Array.from(this.nodes.values()).map(n => n.toJSON()),
      edges: Array.from(this.edges.values()).map(e => e.toJSON()),
      eventLog: this.eventLog
    };
  }
  
  // Load from snapshot
  loadSnapshot(snapshot) {
    this.nodes.clear();
    this.edges.clear();
    this.eventLog = [];
    
    // Replay events to rebuild state
    for (const event of snapshot.eventLog) {
      this._applyEvent(event);
    }
  }
  
  // Pretty print for debugging
  inspect() {
    console.log('=== Graph Store State ===');
    console.log(`Nodes: ${this.nodes.size}`);
    console.log(`Edges: ${this.edges.size}`);
    console.log(`Events: ${this.eventLog.length}`);
    
    console.log('\nNodes:');
    for (const node of this.nodes.values()) {
      console.log(`  ${node.id} (${node.type || 'untyped'})`);
    }
    
    console.log('\nEdges:');
    for (const edge of this.edges.values()) {
      console.log(`  ${edge.from} -[${edge.type || 'untyped'}]-> ${edge.to}`);
    }
  }
}

// Demo and testing
function runDemo() {
  console.log('🚀 Universal Graph System - Prototype Demo\n');
  
  const store = new GraphStore();
  
  // Create some demo data with addresses
  console.log('Creating nodes and edges...');
  
  // People
  store.addNode('alice', 'person', {
    name: 'Alice Johnson',
    email: 'alice@company.com',
    role: 'Senior Developer'
  });
  
  store.addNode('bob', 'person', {
    name: 'Bob Smith', 
    email: 'bob@company.com',
    role: 'Product Manager'
  });
  
  // Tasks with address references
  store.addNode('task_auth', 'task', {
    title: 'Implement Authentication',
    assignee: $('alice'),                    // Address to alice
    reporter: $('bob'),                      // Address to bob
    priority: 'high',
    status: 'in_progress'
  });
  
  store.addNode('task_api', 'task', {
    title: 'Design REST API',
    assignee: $('alice'),
    reporter: $('bob'), 
    priority: 'medium',
    status: 'todo',
    depends_on: [$('task_auth')]             // Address dependency
  });
  
  // Add some edges for explicit relationships
  store.addEdge('edge_1', 'task_api', 'task_auth', 'depends_on');
  store.addEdge('edge_2', 'alice', 'bob', 'reports_to');
  
  // Show current state
  store.inspect();
  
  console.log('\n=== Query Examples ===');
  
  // Find Alice's tasks
  console.log('\nAlice\'s tasks:');
  const alicesTasks = store.findNodes({assignee: $('alice')});
  alicesTasks.forEach(task => {
    console.log(`  - ${task.properties.get('title')} (${task.properties.get('status')})`);
  });
  
  // Find high priority tasks
  console.log('\nHigh priority tasks:');
  const highPriorityTasks = store.findNodes({priority: 'high'});
  highPriorityTasks.forEach(task => {
    console.log(`  - ${task.properties.get('title')}`);
  });
  
  // Address resolution example
  console.log('\n=== Address Resolution ===');
  const authTask = store.get('task_auth');
  const assigneeAddress = authTask.properties.get('assignee');
  const resolvedAssignee = store.resolve(assigneeAddress);
  
  console.log(`Task: ${authTask.properties.get('title')}`);
  console.log(`Assignee Address: ${assigneeAddress}`);
  console.log(`Resolved Assignee: ${resolvedAssignee.properties.get('name')}`);
  
  // Deep resolution example
  console.log('\nDeep resolution of task properties:');
  const deepResolved = store.resolveDeep(authTask.properties);
  const propsObj = deepResolved instanceof Map ? Object.fromEntries(deepResolved) : deepResolved;
  console.log(JSON.stringify(propsObj, null, 2));
  
  // Graph traversal example
  console.log('\n=== Graph Traversal ===');
  console.log('Nodes connected to alice (depth 2):');
  const connected = store.traverse('alice', {direction: 'both', depth: 2});
  connected.forEach(node => {
    console.log(`  - ${node.id} (${node.type}): ${node.properties.get('name') || node.properties.get('title')}`);
  });
  
  // Event log inspection
  console.log('\n=== Event Log ===');
  console.log(`Total events: ${store.eventLog.length}`);
  store.eventLog.slice(-3).forEach(event => {
    console.log(`  ${new Date(event.timestamp).toISOString()} - ${event.type}: ${event.data.id}`);
  });
  
  return store;
}

// Self-execute if run directly
if (import.meta.main) {
  const store = runDemo();
  
  // Make store available globally for interactive use
  globalThis.store = store;
  globalThis.$ = $; // Address factory
  globalThis.Address = Address;
  globalThis.Node = Node;
  globalThis.Edge = Edge;
  
  console.log('\n💡 Interactive mode: ');
  console.log('  - `store` - The graph store instance');
  console.log('  - `$("id")` - Create address to id');
  console.log('  - `store.addNode("id", "type", {props})`');
  console.log('  - `store.findNodes({filter})`');
  console.log('  - `store.traverse("startId", {options})`');
}

export { GraphStore, Node, Edge, Address, $ };
