#!/usr/bin/env bun
/**
 * Universal Graph System (UGS) - Production Implementation with Full Persistence
 * Features: Event Sourcing, Write-Ahead Logging, Snapshots, Recovery
 */

import { writeFile, readFile, mkdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';

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
  
  static isAddress(obj: any): obj is Address {
    return obj != null && obj._isAddress === true;
  }
}

export function $(id: string, version: string | null = null): Address {
  return new Address(id, version);
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

export class Edge {
  id: string;
  from: string;
  to: string;
  type: string | null;
  weight: number;
  properties: Map<string, any>;
  created: number;

  constructor(id: string, from: string, to: string, type: string | null = null, properties: Record<string, any> = {}, weight: number = 1) {
    this.id = id;
    this.from = from;
    this.to = to;
    this.type = type;
    this.weight = weight;
    this.properties = new Map(Object.entries(properties));
    this.created = Date.now();
  }
  
  toJSON() {
    return {
      id: this.id,
      from: this.from,
      to: this.to,
      type: this.type,
      weight: this.weight,
      properties: Object.fromEntries(this.properties),
      created: this.created
    };
  }
}

interface GraphEvent {
  id: string;
  timestamp: number;
  type: string;
  data: any;
}

/**
 * State change event for subscriptions
 */
export interface StateChangeEvent {
  type: 'node_added' | 'node_updated' | 'node_deleted' | 'edge_added' | 'edge_deleted';
  nodeId?: string;
  edgeId?: string;
  timestamp: number;
}

/**
 * Simple EventEmitter for state changes
 */
class StateChangeEmitter {
  private listeners: Array<(event: StateChangeEvent) => void> = [];

  subscribe(listener: (event: StateChangeEvent) => void): () => void {
    this.listeners.push(listener);
    // Return unsubscribe function
    return () => {
      const index = this.listeners.indexOf(listener);
      if (index > -1) {
        this.listeners.splice(index, 1);
      }
    };
  }

  emit(event: StateChangeEvent): void {
    for (const listener of this.listeners) {
      try {
        listener(event);
      } catch (error) {
        console.error('Error in state change listener:', error);
      }
    }
  }

  getListenerCount(): number {
    return this.listeners.length;
  }
}

export class GraphStore {
  // Core storage
  nodes = new Map<string, Node>();
  edges = new Map<string, Edge>();
  eventLog: GraphEvent[] = [];

  // Performance indices
  adjacencyOut = new Map<string, Array<{ to: string; edgeId: string; type: string | null; weight: number }>>();
  adjacencyIn = new Map<string, Array<{ from: string; edgeId: string; type: string | null; weight: number }>>();
  typeIndex = new Map<string, Set<string>>();
  propIndex = new Map<string, Set<string>>();
  textIndex = new Map<string, Set<string>>();

  // State change notifications (for subscriptions)
  private stateChangeEmitter = new StateChangeEmitter();

  // Public port for subscribing to state changes
  ports = {
    stateChanges: {
      subscribe: (listener: (event: StateChangeEvent) => void) => {
        return this.stateChangeEmitter.subscribe(listener);
      },
      getListenerCount: () => this.stateChangeEmitter.getListenerCount()
    }
  };

  // Persistence
  protected dataDir: string;
  private walFile: string;
  private snapshotFile: string;
  private eventCounter: number = 0;
  private lastSnapshot: number = 0;
  private snapshotInterval: number = 100;
  
  stats = {
    nodeCount: 0,
    edgeCount: 0,
    indexCount: 0,
    eventCount: 0,
    lastSnapshot: 0,
    lastUpdate: Date.now()
  };
  
  constructor(dataDir: string = './data') {
    this.dataDir = dataDir;
    this.walFile = join(dataDir, 'events.wal');
    this.snapshotFile = join(dataDir, 'snapshot.json');
  }
  
  // === PERSISTENCE IMPLEMENTATION ===
  
  async initialize(): Promise<void> {
    try {
      await mkdir(this.dataDir, { recursive: true });
    } catch (e) {
      // Directory exists
    }
    
    await this.loadFromPersistence();
    
    return Promise.resolve();
  }
  
  private async loadFromPersistence(): Promise<void> {
    try {
      // Load snapshot first
      if (existsSync(this.snapshotFile)) {
        await this.loadSnapshot();
      }
      
      // Replay WAL
      if (existsSync(this.walFile)) {
        await this.replayWAL();
      }
    } catch (error) {
      // Continue without persistence
    }
  }
  
  private async loadSnapshot(): Promise<void> {
    const snapshotData = await readFile(this.snapshotFile, 'utf-8');
    const snapshot = JSON.parse(snapshotData);
    
    // Restore nodes
    for (const nodeData of snapshot.nodes) {
      const node = new Node(nodeData.id, nodeData.type, nodeData.properties, nodeData.data);
      node.created = nodeData.created;
      node.modified = nodeData.modified;
      this.nodes.set(node.id, node);
      this._indexNode(node);
    }
    
    // Restore edges
    for (const edgeData of snapshot.edges) {
      const edge = new Edge(edgeData.id, edgeData.from, edgeData.to, edgeData.type, edgeData.properties, edgeData.weight);
      edge.created = edgeData.created;
      this.edges.set(edge.id, edge);
      this._indexEdge(edge);
    }
    
    this.lastSnapshot = snapshot.eventCount || 0;
    this.eventCounter = this.lastSnapshot;
    this._updateStats();
  }
  
  private async replayWAL(): Promise<void> {
    const walData = await readFile(this.walFile, 'utf-8');
    const events = walData.trim().split('\n').filter(line => line.trim());
    
    for (const eventLine of events) {
      try {
        const event: GraphEvent = JSON.parse(eventLine);
        if (event.timestamp > this.lastSnapshot) {
          await this._applyEvent(event, false);
        }
      } catch (error) {
        // Skip invalid events
      }
    }
  }
  
  private async persistEvent(event: GraphEvent): Promise<void> {
    if (this.dataDir === ':memory:') return;
    const eventLine = JSON.stringify(event) + '\n';
    
    try {
      await writeFile(this.walFile, eventLine, { flag: 'a' });
    } catch (error) {
      throw new Error(`Failed to persist event: ${error.message}`);
    }
    
    if (this.eventCounter - this.lastSnapshot >= this.snapshotInterval) {
      await this.createSnapshot();
    }
  }
  
  private async createSnapshot(): Promise<void> {
    const snapshot = {
      timestamp: Date.now(),
      eventCount: this.eventCounter,
      nodes: Array.from(this.nodes.values()).map(n => n.toJSON()),
      edges: Array.from(this.edges.values()).map(e => e.toJSON()),
      stats: this.stats
    };
    
    await writeFile(this.snapshotFile, JSON.stringify(snapshot, null, 2));
    await writeFile(this.walFile, '');
    this.lastSnapshot = this.eventCounter;
  }
  
  private generateEventId(): string {
    this.eventCounter++;
    return crypto.randomUUID();
  }
  
  private async _applyEvent(event: GraphEvent, persist: boolean = true): Promise<void> {
    switch(event.type) {
      case 'NodeAdded':
        this._addNodeFromEvent(event.data);
        break;
      case 'EdgeAdded':
        this._addEdgeFromEvent(event.data);
        break;
      case 'NodeDeleted':
        this._deleteNodeFromEvent(event.data);
        break;
      case 'EdgeDeleted':
        this._deleteEdgeFromEvent(event.data);
        break;
      case 'NodeUpdated':
        this._updateNodeFromEvent(event.data);
        break;
      case 'NodeTagged':
        this._tagNodeFromEvent(event.data);
        break;
      case 'NodeUntagged':
        this._untagNodeFromEvent(event.data);
        break;
    }

    this.eventLog.push(event);
    this._updateStats();

    if (persist) {
      await this.persistEvent(event);
    }
  }
  
  // === CRUD OPERATIONS WITH EVENT SOURCING ===
  
  async addNode(id: string, type: string | null = null, properties: Record<string, any> = {}, data: any = null): Promise<Node> {
    const event: GraphEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type: 'NodeAdded',
      data: { id, type, properties, data }
    };
    
    await this._applyEvent(event);
    return this.nodes.get(id)!;
  }
  
  async addEdge(id: string, from: string, to: string, type: string | null = null, properties: Record<string, any> = {}, weight: number = 1): Promise<Edge> {
    const event: GraphEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type: 'EdgeAdded',
      data: { id, from, to, type, properties, weight }
    };
    
    await this._applyEvent(event);
    return this.edges.get(id)!;
  }
  
  async deleteNode(id: string): Promise<boolean> {
    if (!this.nodes.has(id)) return false;
    
    const event: GraphEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type: 'NodeDeleted',
      data: { id }
    };
    
    await this._applyEvent(event);
    return true;
  }
  
  async deleteEdge(id: string): Promise<boolean> {
    if (!this.edges.has(id)) return false;

    const event: GraphEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type: 'EdgeDeleted',
      data: { id }
    };

    await this._applyEvent(event);
    return true;
  }

  async updateNode(id: string, properties: Record<string, any>): Promise<Node | null> {
    const node = this.nodes.get(id);
    if (!node) return null;

    const event: GraphEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type: 'NodeUpdated',
      data: { id, properties }
    };

    await this._applyEvent(event);
    return this.nodes.get(id)!;
  }

  private _addNodeFromEvent(data: any): void {
    const node = new Node(data.id, data.type, data.properties || {}, data.data);
    this.nodes.set(node.id, node);
    this._indexNode(node);
    // Emit state change event
    this.stateChangeEmitter.emit({
      type: 'node_added',
      nodeId: node.id,
      timestamp: Date.now()
    });
  }
  
  private _addEdgeFromEvent(data: any): void {
    const edge = new Edge(data.id, data.from, data.to, data.type, data.properties || {}, data.weight || 1);
    this.edges.set(edge.id, edge);
    this._indexEdge(edge);
    // Emit state change event
    this.stateChangeEmitter.emit({
      type: 'edge_added',
      edgeId: edge.id,
      timestamp: Date.now()
    });
  }
  
  private _deleteNodeFromEvent(data: any): void {
    const node = this.nodes.get(data.id);
    if (node) {
      this.nodes.delete(data.id);
      this._unindexNode(node);
      // Emit state change event
      this.stateChangeEmitter.emit({
        type: 'node_deleted',
        nodeId: data.id,
        timestamp: Date.now()
      });
    }
  }
  
  private _deleteEdgeFromEvent(data: any): void {
    const edge = this.edges.get(data.id);
    if (edge) {
      this.edges.delete(data.id);
      this._unindexEdge(edge);
      // Emit state change event
      this.stateChangeEmitter.emit({
        type: 'edge_deleted',
        edgeId: data.id,
        timestamp: Date.now()
      });
    }
  }

  private _updateNodeFromEvent(data: any): void {
    const node = this.nodes.get(data.id);
    if (node) {
      // Unindex old values
      this._unindexNode(node);

      // Update properties
      for (const [key, value] of Object.entries(data.properties || {})) {
        node.properties.set(key, value);
      }
      node.modified = Date.now();

      // Reindex with new values
      this._indexNode(node);

      // Emit state change event
      this.stateChangeEmitter.emit({
        type: 'node_updated',
        nodeId: data.id,
        timestamp: Date.now()
      });
    }
  }

  get(id: string): Node | Edge | undefined {
    return this.nodes.get(id) || this.edges.get(id);
  }
  
  // === INDEXING SYSTEM ===
  
  protected _indexNode(node: Node): void {
    if (node.type) {
      if (!this.typeIndex.has(node.type)) {
        this.typeIndex.set(node.type, new Set());
      }
      this.typeIndex.get(node.type)!.add(node.id);
    }
    
    for (const [key, value] of node.properties) {
      const indexKey = `${key}:${value}`;
      if (!this.propIndex.has(indexKey)) {
        this.propIndex.set(indexKey, new Set());
      }
      this.propIndex.get(indexKey)!.add(node.id);
    }
    
    const textContent = [
      node.id,
      node.type || '',
      ...Array.from(node.properties.values()).map(v => String(v))
    ].join(' ');
    
    const tokens = textContent.toLowerCase().split(/\W+/).filter(token => token.length > 2);
    
    for (const token of tokens) {
      if (!this.textIndex.has(token)) {
        this.textIndex.set(token, new Set());
      }
      this.textIndex.get(token)!.add(node.id);
    }
  }
  
  private _unindexNode(node: Node): void {
    if (node.type && this.typeIndex.has(node.type)) {
      this.typeIndex.get(node.type)!.delete(node.id);
    }
    
    for (const [key, value] of node.properties) {
      const indexKey = `${key}:${value}`;
      if (this.propIndex.has(indexKey)) {
        this.propIndex.get(indexKey)!.delete(node.id);
      }
    }
    
    for (const tokenSet of this.textIndex.values()) {
      tokenSet.delete(node.id);
    }
  }
  
  protected _indexEdge(edge: Edge): void {
    if (!this.adjacencyOut.has(edge.from)) {
      this.adjacencyOut.set(edge.from, []);
    }
    this.adjacencyOut.get(edge.from)!.push({
      to: edge.to,
      edgeId: edge.id,
      type: edge.type,
      weight: edge.weight
    });
    
    if (!this.adjacencyIn.has(edge.to)) {
      this.adjacencyIn.set(edge.to, []);
    }
    this.adjacencyIn.get(edge.to)!.push({
      from: edge.from,
      edgeId: edge.id,
      type: edge.type,
      weight: edge.weight
    });
  }
  
  private _unindexEdge(edge: Edge): void {
    const outgoing = this.adjacencyOut.get(edge.from);
    if (outgoing) {
      const index = outgoing.findIndex(e => e.edgeId === edge.id);
      if (index >= 0) outgoing.splice(index, 1);
    }
    
    const incoming = this.adjacencyIn.get(edge.to);
    if (incoming) {
      const index = incoming.findIndex(e => e.edgeId === edge.id);
      if (index >= 0) incoming.splice(index, 1);
    }
  }
  
  protected _updateStats(): void {
    this.stats = {
      nodeCount: this.nodes.size,
      edgeCount: this.edges.size,
      indexCount: this.typeIndex.size + this.propIndex.size + this.textIndex.size,
      eventCount: this.eventLog.length,
      lastSnapshot: this.lastSnapshot,
      lastUpdate: Date.now()
    };
  }
  
  // === QUERY METHODS ===
  
  getByType(type: string): Node[] {
    const nodeIds = this.typeIndex.get(type) || new Set();
    return Array.from(nodeIds).map(id => this.nodes.get(id)!);
  }
  
  getByProperty(key: string, value: any): Node[] {
    const indexKey = `${key}:${value}`;
    const nodeIds = this.propIndex.get(indexKey) || new Set();
    return Array.from(nodeIds).map(id => this.nodes.get(id)!);
  }
  
  search(query: string): Node[] {
    const tokens = query.toLowerCase().split(/\W+/).filter(t => t.length > 2);
    if (tokens.length === 0) return [];
    
    let results = new Set(this.textIndex.get(tokens[0]) || []);
    
    for (let i = 1; i < tokens.length; i++) {
      const tokenResults = this.textIndex.get(tokens[i]) || new Set();
      results = new Set([...results].filter(id => tokenResults.has(id)));
    }
    
    return Array.from(results).map(id => this.nodes.get(id)!);
  }
  
  // === PATHFINDING ===
  
  findShortestPath(fromId: string, toId: string, _options: any = {}): { path: string[]; distance: number; nodes: Node[] } | null {
    if (fromId === toId) {
      return { path: [fromId], distance: 0, nodes: [this.nodes.get(fromId)!] };
    }
    
    const distances = new Map<string, number>();
    const previous = new Map<string, string>();
    const unvisited = new Set<string>();
    
    for (const nodeId of this.nodes.keys()) {
      distances.set(nodeId, nodeId === fromId ? 0 : Infinity);
      unvisited.add(nodeId);
    }
    
    while (unvisited.size > 0) {
      let currentNode: string | null = null;
      let minDist = Infinity;
      
      for (const nodeId of unvisited) {
        const dist = distances.get(nodeId)!;
        if (dist < minDist) {
          minDist = dist;
          currentNode = nodeId;
        }
      }
      
      if (currentNode === null || minDist === Infinity) break;
      if (currentNode === toId) break;
      
      unvisited.delete(currentNode);
      
      const neighbors = this.adjacencyOut.get(currentNode) || [];
      for (const neighbor of neighbors) {
        if (!unvisited.has(neighbor.to)) continue;
        
        const newDistance = distances.get(currentNode)! + neighbor.weight;
        if (newDistance < distances.get(neighbor.to)!) {
          distances.set(neighbor.to, newDistance);
          previous.set(neighbor.to, currentNode);
        }
      }
    }
    
    if (!previous.has(toId) && fromId !== toId) {
      return null;
    }
    
    const path: string[] = [];
    let current: string | undefined = toId;
    while (current !== undefined) {
      path.unshift(current);
      current = previous.get(current);
    }
    
    return {
      path,
      distance: distances.get(toId)!,
      nodes: path.map(id => this.nodes.get(id)!)
    };
  }
  
  // === TRAVERSAL ===
  
  traverse(startId: string, options: any = {}): Array<{ node: Node; depth: number; path: string[] }> {
    const {
      direction = 'out',
      edgeType,
      maxDepth = 3,
      algorithm = 'bfs',
      maxResults = 100
    } = options;
    
    const visited = new Set<string>();
    const results: Array<{ node: Node; depth: number; path: string[] }> = [];
    const queue = [{ nodeId: startId, depth: 0, path: [startId] }];
    
    while (queue.length > 0 && results.length < maxResults) {
      const { nodeId, depth, path } = algorithm === 'bfs' ? queue.shift()! : queue.pop()!;
      
      if (visited.has(nodeId) || depth > maxDepth) continue;
      
      visited.add(nodeId);
      const node = this.nodes.get(nodeId);
      
      if (node) {
        results.push({ node, depth, path: [...path] });
        
        const neighbors = this._getNeighbors(nodeId, direction, edgeType);
        for (const neighborId of neighbors) {
          if (!visited.has(neighborId)) {
            queue.push({
              nodeId: neighborId,
              depth: depth + 1,
              path: [...path, neighborId]
            });
          }
        }
      }
    }
    
    return results;
  }
  
  private _getNeighbors(nodeId: string, direction: string, edgeType?: string): string[] {
    const neighbors: string[] = [];
    
    if (direction === 'out' || direction === 'both') {
      const outgoing = this.adjacencyOut.get(nodeId) || [];
      for (const edge of outgoing) {
        if (!edgeType || edge.type === edgeType) {
          neighbors.push(edge.to);
        }
      }
    }
    
    if (direction === 'in' || direction === 'both') {
      const incoming = this.adjacencyIn.get(nodeId) || [];
      for (const edge of incoming) {
        if (!edgeType || edge.type === edgeType) {
          neighbors.push(edge.from);
        }
      }
    }
    
    return neighbors;
  }
  
  // === ANALYTICS ===
  
  getNodeDegree(nodeId: string): { in: number; out: number; total: number } {
    const outDegree = (this.adjacencyOut.get(nodeId) || []).length;
    const inDegree = (this.adjacencyIn.get(nodeId) || []).length;
    return { in: inDegree, out: outDegree, total: inDegree + outDegree };
  }
  
  getMostConnected(limit: number = 10): Array<{ id: string; in: number; out: number; total: number }> {
    return Array.from(this.nodes.keys())
      .map(id => ({ id, ...this.getNodeDegree(id) }))
      .sort((a, b) => b.total - a.total)
      .slice(0, limit);
  }

  /**
   * Compute all strongly connected components using iterative Tarjan's algorithm.
   * Returns an array of SCCs, each SCC being an array of node IDs.
   * SCCs are in reverse topological order (sink SCCs first).
   */
  findSCCs(): string[][] {
    const index = new Map<string, number>();
    const lowlink = new Map<string, number>();
    const onStack = new Set<string>();
    const stack: string[] = [];
    const sccs: string[][] = [];
    let counter = 0;

    const strongconnect = (root: string) => {
      // callStack entries: { nodeId, neighborIdx }
      const callStack: Array<{ nodeId: string; neighborIdx: number }> = [];

      index.set(root, counter);
      lowlink.set(root, counter);
      counter++;
      stack.push(root);
      onStack.add(root);
      callStack.push({ nodeId: root, neighborIdx: 0 });

      while (callStack.length > 0) {
        const frame = callStack[callStack.length - 1];
        const { nodeId } = frame;
        const neighbors = this.adjacencyOut.get(nodeId) ?? [];

        if (frame.neighborIdx < neighbors.length) {
          const w = neighbors[frame.neighborIdx].to;
          frame.neighborIdx++;

          if (!index.has(w)) {
            // Tree edge — recurse into w
            index.set(w, counter);
            lowlink.set(w, counter);
            counter++;
            stack.push(w);
            onStack.add(w);
            callStack.push({ nodeId: w, neighborIdx: 0 });
          } else if (onStack.has(w)) {
            // Back edge — update lowlink
            lowlink.set(nodeId, Math.min(lowlink.get(nodeId)!, index.get(w)!));
          }
        } else {
          // Done with nodeId — pop and propagate lowlink to parent
          callStack.pop();
          if (callStack.length > 0) {
            const parent = callStack[callStack.length - 1].nodeId;
            lowlink.set(
              parent,
              Math.min(lowlink.get(parent)!, lowlink.get(nodeId)!)
            );
          }

          // Check if nodeId is an SCC root
          if (lowlink.get(nodeId) === index.get(nodeId)) {
            const scc: string[] = [];
            let w: string;
            do {
              w = stack.pop()!;
              onStack.delete(w);
              scc.push(w);
            } while (w !== nodeId);
            sccs.push(scc);
          }
        }
      }
    };

    for (const nodeId of this.nodes.keys()) {
      if (!index.has(nodeId)) {
        strongconnect(nodeId);
      }
    }

    return sccs;
  }

  /**
   * Returns true if the graph contains at least one cycle.
   * If startId is provided, only checks nodes reachable from startId.
   */
  hasCycle(startId?: string): boolean {
    let nodesToCheck: Set<string>;

    if (startId) {
      const reachable = this.traverse(startId, { direction: 'out', maxDepth: 100000 });
      nodesToCheck = new Set(reachable.map((r: any) => r.node.id));
      nodesToCheck.add(startId);
    } else {
      nodesToCheck = new Set(this.nodes.keys());
    }

    const sccs = this.findSCCs();

    for (const scc of sccs) {
      const relevant = scc.filter(id => nodesToCheck.has(id));
      if (relevant.length > 1) return true;
      if (relevant.length === 1) {
        const id = relevant[0];
        const outgoing = this.adjacencyOut.get(id) ?? [];
        if (outgoing.some(e => e.to === id)) return true;
      }
    }
    return false;
  }

  /**
   * Return a new in-memory GraphStore containing only nodes and edges
   * satisfying the given predicates. Both endpoints of a retained edge
   * must satisfy nodePredicate. The returned store is ':memory:' —
   * no WAL, no persistence.
   */
  project(
    nodePredicate?: (node: any) => boolean,
    edgePredicate?: (edge: any) => boolean
  ): GraphStore {
    const projected = new GraphStore(':memory:');

    const includedNodeIds = new Set<string>();

    for (const [id, node] of this.nodes) {
      if (!nodePredicate || nodePredicate(node)) {
        includedNodeIds.add(id);
        projected.nodes.set(id, node);
      }
    }

    // Rebuild adjacency for projected nodes
    for (const id of includedNodeIds) {
      projected.adjacencyOut.set(id, []);
      projected.adjacencyIn.set(id, []);
    }

    for (const [id, edge] of this.edges) {
      if (
        includedNodeIds.has(edge.from) &&
        includedNodeIds.has(edge.to) &&
        (!edgePredicate || edgePredicate(edge))
      ) {
        projected.edges.set(id, edge);

        const outList = projected.adjacencyOut.get(edge.from) ?? [];
        outList.push({ to: edge.to, edgeId: id, type: edge.type, weight: edge.weight });
        projected.adjacencyOut.set(edge.from, outList);

        const inList = projected.adjacencyIn.get(edge.to) ?? [];
        inList.push({ from: edge.from, edgeId: id, type: edge.type, weight: edge.weight });
        projected.adjacencyIn.set(edge.to, inList);
      }
    }

    return projected;
  }

  // === UTILITIES ===
  
  async shutdown(): Promise<void> {
    await this.createSnapshot();
  }
  
  getEventHistory(limit: number = 100): GraphEvent[] {
    return this.eventLog.slice(-limit);
  }
  
  resolve(address: Address | any): Node | Edge | any {
    if (!Address.isAddress(address)) return address;
    return this.get(address.id);
  }

  // === TAG OPERATIONS ===

  /**
   * Add tags to a node. Tags are stored in the 'tags' property as an array.
   * Emits NODE_TAGGED event for each tag added.
   */
  async addTags(nodeId: string, tags: string[]): Promise<Node | null> {
    const node = this.nodes.get(nodeId);
    if (!node) return null;

    // Get existing tags or initialize empty array
    const existingTags: string[] = node.properties.get('tags') || [];

    // Find tags that don't already exist
    const newTags = tags.filter(tag => !existingTags.includes(tag));

    if (newTags.length === 0) {
      return node; // No new tags to add
    }

    const event: GraphEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type: 'NodeTagged',
      data: { id: nodeId, tags: newTags }
    };

    await this._applyEvent(event);
    return this.nodes.get(nodeId)!;
  }

  /**
   * Remove tags from a node.
   * Emits NODE_UNTAGGED event for each tag removed.
   */
  async removeTags(nodeId: string, tags: string[]): Promise<Node | null> {
    const node = this.nodes.get(nodeId);
    if (!node) return null;

    // Get existing tags
    const existingTags: string[] = node.properties.get('tags') || [];

    // Find tags that actually exist and will be removed
    const tagsToRemove = tags.filter(tag => existingTags.includes(tag));

    if (tagsToRemove.length === 0) {
      return node; // No tags to remove
    }

    const event: GraphEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type: 'NodeUntagged',
      data: { id: nodeId, tags: tagsToRemove }
    };

    await this._applyEvent(event);
    return this.nodes.get(nodeId)!;
  }

  /**
   * Get all nodes with a specific tag.
   */
  getByTag(tag: string): Node[] {
    const results: Node[] = [];

    for (const node of this.nodes.values()) {
      const tags: string[] = node.properties.get('tags') || [];
      if (tags.includes(tag)) {
        results.push(node);
      }
    }

    return results;
  }

  /**
   * Get nodes matching any or all of the specified tags.
   * @param tags - Array of tags to match
   * @param matchAll - If true, nodes must have ALL tags. If false, nodes must have ANY tag.
   */
  getByTags(tags: string[], matchAll: boolean = false): Node[] {
    if (tags.length === 0) return [];

    const results: Node[] = [];

    for (const node of this.nodes.values()) {
      const nodeTags: string[] = node.properties.get('tags') || [];

      if (matchAll) {
        // Node must have ALL specified tags
        const hasAllTags = tags.every(tag => nodeTags.includes(tag));
        if (hasAllTags) {
          results.push(node);
        }
      } else {
        // Node must have ANY of the specified tags
        const hasAnyTag = tags.some(tag => nodeTags.includes(tag));
        if (hasAnyTag) {
          results.push(node);
        }
      }
    }

    return results;
  }

  /**
   * Get tags for a specific node.
   */
  getNodeTags(nodeId: string): string[] {
    const node = this.nodes.get(nodeId);
    if (!node) return [];
    return node.properties.get('tags') || [];
  }

  private _tagNodeFromEvent(data: { id: string; tags: string[] }): void {
    const node = this.nodes.get(data.id);
    if (node) {
      // Unindex old values
      this._unindexNode(node);

      // Get existing tags or initialize
      const existingTags: string[] = node.properties.get('tags') || [];

      // Add new tags (avoiding duplicates)
      const updatedTags = [...existingTags, ...data.tags.filter(t => !existingTags.includes(t))];
      node.properties.set('tags', updatedTags);
      node.modified = Date.now();

      // Reindex with new values
      this._indexNode(node);
    }
  }

  private _untagNodeFromEvent(data: { id: string; tags: string[] }): void {
    const node = this.nodes.get(data.id);
    if (node) {
      // Unindex old values
      this._unindexNode(node);

      // Get existing tags
      const existingTags: string[] = node.properties.get('tags') || [];

      // Remove specified tags
      const updatedTags = existingTags.filter(t => !data.tags.includes(t));

      if (updatedTags.length > 0) {
        node.properties.set('tags', updatedTags);
      } else {
        node.properties.delete('tags');
      }
      node.modified = Date.now();

      // Reindex with new values
      this._indexNode(node);
    }
  }
}

export default GraphStore;
