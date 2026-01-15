// KnowledgeNode - Knowledge actor for storing and querying information

import type { Graph, NodeActor } from "./graph";
import type { Edge, GetResponse, KnowledgeProperties, Message, ObserveResponse } from "./types";

let knowledgeCounter = 0;

export interface CreateKnowledgeOptions {
  title: string;
  content?: string;
  sources?: string[];
}

export class KnowledgeNode implements NodeActor {
  properties: KnowledgeProperties;

  constructor(options: CreateKnowledgeOptions) {
    this.properties = {
      id: `knowledge_${++knowledgeCounter}`,
      type: "knowledge",
      createdAt: new Date(),
      title: options.title,
      content: options.content ?? "",
      sources: options.sources ?? [],
      version: 1,
    };
  }

  handleMessage(message: Message, graph: Graph): unknown {
    switch (message.type) {
      // Standard messages
      case "get":
        return this.handleGet(graph);
      case "observe":
        return this.handleObserve();
      case "update":
        return this.handleUpdate(message.payload as { properties: Partial<KnowledgeProperties> });
      case "link":
        return this.handleLink(message.payload as { toId: string; edgeType: Edge["type"]; properties?: Record<string, unknown> }, graph);
      case "unlink":
        return this.handleUnlink(message.payload as { edgeId: string }, graph);
      case "delete":
        return this.handleDelete(graph);

      // Knowledge-specific messages
      case "append":
        return this.handleAppend(message.payload as { data: string; source?: string });
      case "query":
        return this.handleQuery(message.payload as { question: string; context?: Record<string, unknown> });
      case "synthesize":
        return this.handleSynthesize(message.payload as { fromNodes: string[] }, graph);

      default:
        throw new Error(`Unknown message type: ${message.type}`);
    }
  }

  // Standard message handlers

  private handleGet(graph: Graph): GetResponse {
    return {
      id: this.properties.id,
      type: this.properties.type,
      properties: this.properties,
      edges: graph.getAllEdges(this.properties.id),
    };
  }

  private handleObserve(): ObserveResponse {
    const observations: string[] = [];

    observations.push(`Knowledge: "${this.properties.title}"`);
    observations.push(`Content length: ${this.properties.content.length} chars`);
    observations.push(`Sources: ${this.properties.sources.length}`);
    observations.push(`Version: ${this.properties.version}`);

    return {
      state: "active",
      observations,
      metadata: {
        version: this.properties.version,
        contentLength: this.properties.content.length,
        sourceCount: this.properties.sources.length,
      },
    };
  }

  private handleUpdate(payload: { properties: Partial<KnowledgeProperties> }): { success: boolean; updatedProperties: string[] } {
    const updatedProperties: string[] = [];

    for (const [key, value] of Object.entries(payload.properties)) {
      if (key !== "id" && key !== "type" && key !== "createdAt") {
        (this.properties as Record<string, unknown>)[key] = value;
        updatedProperties.push(key);
      }
    }

    return { success: true, updatedProperties };
  }

  private handleLink(payload: { toId: string; edgeType: Edge["type"]; properties?: Record<string, unknown> }, graph: Graph): { edgeId: string; success: boolean } {
    const edge = graph.addEdge(this.properties.id, payload.toId, payload.edgeType, payload.properties ?? {});
    return { edgeId: edge.id, success: true };
  }

  private handleUnlink(payload: { edgeId: string }, graph: Graph): { success: boolean } {
    const success = graph.removeEdge(payload.edgeId);
    return { success };
  }

  private handleDelete(graph: Graph): { success: boolean } {
    const success = graph.removeNode(this.properties.id);
    return { success };
  }

  // Knowledge-specific handlers

  private handleAppend(payload: { data: string; source?: string }): { success: boolean; version: number } {
    this.properties.content += (this.properties.content ? "\n\n" : "") + payload.data;
    this.properties.version++;

    if (payload.source) {
      this.properties.sources.push(payload.source);
    }

    return { success: true, version: this.properties.version };
  }

  private handleQuery(payload: { question: string; context?: Record<string, unknown> }): { answer: string; confidence: number; sources: string[] } {
    // Simple keyword matching for MVP
    // In a real implementation, this would use embeddings/LLM
    const question = payload.question.toLowerCase();
    const content = this.properties.content.toLowerCase();

    const keywords = question.split(/\s+/).filter((w) => w.length > 3);
    const matchCount = keywords.filter((kw) => content.includes(kw)).length;
    const confidence = keywords.length > 0 ? matchCount / keywords.length : 0;

    // Extract relevant snippet (naive approach)
    let answer = "";
    if (confidence > 0) {
      const sentences = this.properties.content.split(/[.!?]+/);
      for (const sentence of sentences) {
        const sentenceLower = sentence.toLowerCase();
        if (keywords.some((kw) => sentenceLower.includes(kw))) {
          answer = sentence.trim();
          break;
        }
      }
    }

    if (!answer && this.properties.content) {
      answer = this.properties.content.slice(0, 200) + (this.properties.content.length > 200 ? "..." : "");
    }

    return {
      answer: answer || "No relevant information found",
      confidence,
      sources: this.properties.sources,
    };
  }

  private handleSynthesize(payload: { fromNodes: string[] }, graph: Graph): { synthesis: string; sources: string[] } {
    const allSources: string[] = [...this.properties.sources];
    const contentParts: string[] = [this.properties.content];

    for (const nodeId of payload.fromNodes) {
      const node = graph.getNode(nodeId);
      if (node && node.properties.type === "knowledge") {
        const kProps = node.properties as KnowledgeProperties;
        contentParts.push(kProps.content);
        allSources.push(...kProps.sources);
      }
    }

    // Simple synthesis - concatenate with headers
    // Real implementation would use LLM for actual synthesis
    const synthesis = contentParts
      .filter((c) => c.length > 0)
      .map((c, i) => `[Source ${i + 1}]\n${c}`)
      .join("\n\n---\n\n");

    return {
      synthesis,
      sources: [...new Set(allSources)],
    };
  }
}

// Factory function for convenience
export function createKnowledge(options: CreateKnowledgeOptions, graph: Graph): KnowledgeNode {
  const knowledge = new KnowledgeNode(options);
  graph.registerNode(knowledge);
  return knowledge;
}
