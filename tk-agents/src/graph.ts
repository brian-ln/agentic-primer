// Graph - The central store and message router

import type { Edge, EdgeType, Message, NodeProperties } from "./types";

export type MessageHandler = (message: Message, graph: Graph) => unknown;

export interface NodeActor {
  properties: NodeProperties;
  handleMessage: MessageHandler;
}

export class Graph {
  private nodes: Map<string, NodeActor> = new Map();
  private edges: Map<string, Edge> = new Map();
  private edgeCounter = 0;

  // The only external interface - all interactions through SEND
  send(nodeId: string, messageType: string, payload: Record<string, unknown> = {}): unknown {
    const node = this.nodes.get(nodeId);
    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    const message = { type: messageType, payload } as Message;
    return node.handleMessage(message, this);
  }

  // Internal methods for node actors to use

  registerNode(actor: NodeActor): void {
    this.nodes.set(actor.properties.id, actor);
  }

  removeNode(nodeId: string): boolean {
    // Remove all edges connected to this node
    for (const [edgeId, edge] of this.edges) {
      if (edge.fromId === nodeId || edge.toId === nodeId) {
        this.edges.delete(edgeId);
      }
    }
    return this.nodes.delete(nodeId);
  }

  getNode(nodeId: string): NodeActor | undefined {
    return this.nodes.get(nodeId);
  }

  addEdge(fromId: string, toId: string, type: EdgeType, properties: Record<string, unknown> = {}): Edge {
    const id = `edge_${++this.edgeCounter}`;
    const edge: Edge = { id, fromId, toId, type, properties };
    this.edges.set(id, edge);
    return edge;
  }

  removeEdge(edgeId: string): boolean {
    return this.edges.delete(edgeId);
  }

  getEdgesFrom(nodeId: string): Edge[] {
    return [...this.edges.values()].filter((e) => e.fromId === nodeId);
  }

  getEdgesTo(nodeId: string): Edge[] {
    return [...this.edges.values()].filter((e) => e.toId === nodeId);
  }

  getAllEdges(nodeId: string): Edge[] {
    return [...this.edges.values()].filter((e) => e.fromId === nodeId || e.toId === nodeId);
  }

  // Query helpers
  getChildTasks(taskId: string): NodeActor[] {
    const children: NodeActor[] = [];
    for (const edge of this.edges.values()) {
      if (edge.toId === taskId && edge.type === "spawned_by") {
        const child = this.nodes.get(edge.fromId);
        if (child) children.push(child);
      }
    }
    return children;
  }

  getAllNodes(): NodeActor[] {
    return [...this.nodes.values()];
  }

  // Debug/visualization
  dump(): { nodes: NodeProperties[]; edges: Edge[] } {
    return {
      nodes: [...this.nodes.values()].map((n) => n.properties),
      edges: [...this.edges.values()],
    };
  }
}

// Singleton for convenience (can also instantiate directly)
export const graph = new Graph();
