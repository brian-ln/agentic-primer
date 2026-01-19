// Graph - The central store and message router using Address proxy actors

import { System } from "./actors/index.ts";
import type { Address, Message as ActorMessage, Response, System as SystemType } from "./actors/index.ts";
import type { Edge, EdgeType, Message, NodeProperties } from "./types";

/**
 * Graph - manages nodes (actors) and edges with both string IDs and Addresses
 *
 * Key design:
 * - Nodes are actors created via ActorFactory pattern
 * - Graph maintains bidirectional mapping: string ID <-> Address
 * - External API uses string IDs (for serialization, CLI)
 * - Internal messaging uses Address objects
 */
export class Graph {
  private system: SystemType;
  private nodes: Map<string, Address> = new Map(); // string ID -> Address
  private nodeProperties: Map<string, NodeProperties> = new Map(); // Cache for properties
  private edges: Map<string, Edge> = new Map();
  private edgeCounter = 0;

  constructor() {
    this.system = System();
  }

  /**
   * Register an actor with a string ID
   * @param id - String identifier for serialization and edges
   * @param address - Address returned from actor factory
   * @param properties - Node properties (stored for quick access)
   */
  registerNode(id: string, address: Address, properties: NodeProperties): void {
    this.nodes.set(id, address);
    this.nodeProperties.set(id, properties);
  }

  /**
   * Send a message to a node by string ID
   * @param nodeId - String identifier of the node
   * @param messageType - Type of message
   * @param payload - Message payload
   * @returns Response from the actor
   */
  async send(nodeId: string, messageType: string, payload: Record<string, unknown> = {}): Promise<unknown> {
    const address = this.nodes.get(nodeId);
    if (!address) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    const message: ActorMessage = {
      id: crypto.randomUUID(),
      type: messageType,
      payload,
    };

    const response = await address.send(message);

    if (!response.success) {
      throw new Error(response.error || "Actor message failed");
    }

    return response.data;
  }

  /**
   * Remove a node and all its edges
   */
  removeNode(nodeId: string): boolean {
    // Remove all edges connected to this node
    for (const [edgeId, edge] of this.edges) {
      if (edge.fromId === nodeId || edge.toId === nodeId) {
        this.edges.delete(edgeId);
      }
    }
    this.nodeProperties.delete(nodeId);
    return this.nodes.delete(nodeId);
  }

  /**
   * Get node address by string ID
   */
  getNode(nodeId: string): Address | undefined {
    return this.nodes.get(nodeId);
  }

  /**
   * Get node properties by string ID
   */
  getNodeProperties(nodeId: string): NodeProperties | undefined {
    return this.nodeProperties.get(nodeId);
  }

  /**
   * Update node properties
   */
  async updateNode(nodeId: string, properties: Partial<NodeProperties>): Promise<void> {
    const existing = this.nodeProperties.get(nodeId);
    if (!existing) {
      throw new Error(`Node not found: ${nodeId}`);
    }
    Object.assign(existing, properties);
  }

  /**
   * Delete node (alias for removeNode)
   */
  async deleteNode(nodeId: string): Promise<void> {
    this.removeNode(nodeId);
  }

  /**
   * Get all node IDs
   */
  getNodeIds(): string[] {
    return Array.from(this.nodes.keys());
  }

  /**
   * Add an edge between nodes
   */
  addEdge(fromId: string, toId: string, type: EdgeType, properties: Record<string, unknown> = {}): Edge {
    const id = `edge_${++this.edgeCounter}`;
    const edge: Edge = { id, fromId, toId, type, properties };
    this.edges.set(id, edge);
    return edge;
  }

  /**
   * Remove an edge by ID
   */
  removeEdge(edgeId: string): boolean;
  /**
   * Remove an edge by source, target, and type
   */
  removeEdge(fromId: string, toId: string, type: EdgeType): boolean;
  /**
   * Remove an edge (overloaded)
   */
  removeEdge(arg1: string, arg2?: string, arg3?: EdgeType): boolean {
    if (arg2 === undefined) {
      // Remove by edge ID
      return this.edges.delete(arg1);
    } else {
      // Remove by fromId, toId, type
      for (const [edgeId, edge] of this.edges) {
        if (edge.fromId === arg1 && edge.toId === arg2 && edge.type === arg3) {
          this.edges.delete(edgeId);
          return true;
        }
      }
      return false;
    }
  }

  /**
   * Get all edges from a node
   */
  getEdgesFrom(nodeId: string): Edge[] {
    return [...this.edges.values()].filter((e) => e.fromId === nodeId);
  }

  /**
   * Get all edges to a node
   */
  getEdgesTo(nodeId: string): Edge[] {
    return [...this.edges.values()].filter((e) => e.toId === nodeId);
  }

  /**
   * Get all edges connected to a node
   */
  getAllEdges(nodeId: string): Edge[] {
    return [...this.edges.values()].filter((e) => e.fromId === nodeId || e.toId === nodeId);
  }

  /**
   * Get child tasks (nodes with spawned_by edge pointing to this node)
   */
  getChildTasks(taskId: string): string[] {
    const children: string[] = [];
    for (const edge of this.edges.values()) {
      if (edge.toId === taskId && edge.type === "spawned_by") {
        children.push(edge.fromId);
      }
    }
    return children;
  }

  /**
   * Dump graph state for serialization
   */
  dump(): { nodes: NodeProperties[]; edges: Edge[] } {
    return {
      nodes: Array.from(this.nodeProperties.values()),
      edges: [...this.edges.values()],
    };
  }

  /**
   * Get the internal System (for testing/advanced usage)
   */
  getSystem(): SystemType {
    return this.system;
  }

  /**
   * Set edge counter (for loading from file)
   */
  setEdgeCounter(counter: number): void {
    this.edgeCounter = counter;
  }
}

// Singleton for convenience (can also instantiate directly)
export const graph = new Graph();
