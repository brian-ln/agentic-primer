// KnowledgeNode - Knowledge actor using ActorFactory pattern

import type { Graph } from "./graph";
import type { Address, ActorFactory, Message as ActorMessage, Response } from "./actors/index.ts";
import type { Edge, KnowledgeProperties } from "./types";

let knowledgeCounter = 0;

export interface CreateKnowledgeOptions {
  title: string;
  content?: string;
  sources?: string[];
}

interface KnowledgeActorData extends CreateKnowledgeOptions {
  graph: Graph;
}

/**
 * KnowledgeActor - ActorFactory that creates knowledge actors
 *
 * Returns an Address with knowledge message handling capabilities
 */
export const KnowledgeActor: ActorFactory<KnowledgeActorData> = (data) => {
  // Initialize knowledge properties
  const id = `knowledge_${++knowledgeCounter}`;
  const properties: KnowledgeProperties = {
    id,
    type: "knowledge",
    createdAt: new Date(),
    title: data.title,
    content: data.content ?? "",
    sources: data.sources ?? [],
    version: 1,
  };

  // Create actor with message handler
  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      try {
        const result = await handleMessage(message, properties, data.graph);
        return { success: true, data: result };
      } catch (error) {
        return {
          success: false,
          error: error instanceof Error ? error.message : String(error),
        };
      }
    },
  };

  // Register with graph's system and return Address
  const address = data.graph.getSystem().register(actor);

  // Register with graph (using string ID for serialization)
  data.graph.registerNode(id, address, properties);

  return address;
};

/**
 * Handle knowledge messages
 */
async function handleMessage(
  message: ActorMessage,
  properties: KnowledgeProperties,
  graph: Graph
): Promise<unknown> {
  const payload = message.payload as Record<string, unknown>;

  switch (message.type) {
    // Standard messages
    case "get":
      return handleGet(properties, graph);
    case "observe":
      return handleObserve(properties);
    case "update":
      return handleUpdate(payload as { properties: Partial<KnowledgeProperties> }, properties);
    case "link":
      return handleLink(
        payload as { toId: string; edgeType: string; properties?: Record<string, unknown> },
        properties,
        graph
      );
    case "unlink":
      return handleUnlink(payload as { edgeId: string }, graph);
    case "delete":
      return handleDelete(properties, graph);

    // Knowledge-specific messages
    case "append":
      return handleAppend(payload as { data: string; source?: string }, properties);
    case "query":
      return handleQuery(payload as { question: string; context?: Record<string, unknown> }, properties);
    case "synthesize":
      return handleSynthesize(payload as { fromNodes: string[] }, properties, graph);

    default:
      throw new Error(`Unknown message type: ${message.type}`);
  }
}

// Message handlers

function handleGet(properties: KnowledgeProperties, graph: Graph): {
  id: string;
  type: string;
  properties: KnowledgeProperties;
  edges: Edge[];
} {
  return {
    id: properties.id,
    type: properties.type,
    properties: properties,
    edges: graph.getAllEdges(properties.id),
  };
}

function handleObserve(properties: KnowledgeProperties): {
  state: string;
  observations: string[];
  metadata: Record<string, unknown>;
} {
  const observations: string[] = [];

  observations.push(`Knowledge: "${properties.title}"`);
  observations.push(`Content length: ${properties.content.length} chars`);
  observations.push(`Sources: ${properties.sources.length}`);
  observations.push(`Version: ${properties.version}`);

  return {
    state: "active",
    observations,
    metadata: {
      version: properties.version,
      contentLength: properties.content.length,
      sourceCount: properties.sources.length,
    },
  };
}

function handleUpdate(
  payload: { properties: Partial<KnowledgeProperties> },
  properties: KnowledgeProperties
): { success: boolean; updatedProperties: string[] } {
  const updatedProperties: string[] = [];

  for (const [key, value] of Object.entries(payload.properties)) {
    if (key !== "id" && key !== "type" && key !== "createdAt") {
      (properties as Record<string, unknown>)[key] = value;
      updatedProperties.push(key);
    }
  }

  return { success: true, updatedProperties };
}

function handleLink(
  payload: { toId: string; edgeType: string; properties?: Record<string, unknown> },
  properties: KnowledgeProperties,
  graph: Graph
): { edgeId: string; success: boolean } {
  const edge = graph.addEdge(properties.id, payload.toId, payload.edgeType as any, payload.properties ?? {});
  return { edgeId: edge.id, success: true };
}

function handleUnlink(payload: { edgeId: string }, graph: Graph): { success: boolean } {
  const success = graph.removeEdge(payload.edgeId);
  return { success };
}

function handleDelete(properties: KnowledgeProperties, graph: Graph): { success: boolean } {
  const success = graph.removeNode(properties.id);
  return { success };
}

// Knowledge-specific handlers

function handleAppend(
  payload: { data: string; source?: string },
  properties: KnowledgeProperties
): { success: boolean; version: number } {
  properties.content += (properties.content ? "\n\n" : "") + payload.data;
  properties.version++;

  if (payload.source) {
    properties.sources.push(payload.source);
  }

  return { success: true, version: properties.version };
}

function handleQuery(
  payload: { question: string; context?: Record<string, unknown> },
  properties: KnowledgeProperties
): { answer: string; confidence: number; sources: string[] } {
  // Simple keyword matching for MVP
  // In a real implementation, this would use embeddings/LLM
  const question = payload.question.toLowerCase();
  const content = properties.content.toLowerCase();

  const keywords = question.split(/\s+/).filter((w) => w.length > 3);
  const matchCount = keywords.filter((kw) => content.includes(kw)).length;
  const confidence = keywords.length > 0 ? matchCount / keywords.length : 0;

  // Extract relevant snippet (naive approach)
  let answer = "";
  if (confidence > 0) {
    const sentences = properties.content.split(/[.!?]+/);
    for (const sentence of sentences) {
      const sentenceLower = sentence.toLowerCase();
      if (keywords.some((kw) => sentenceLower.includes(kw))) {
        answer = sentence.trim();
        break;
      }
    }
  }

  if (!answer && properties.content) {
    answer = properties.content.slice(0, 200) + (properties.content.length > 200 ? "..." : "");
  }

  return {
    answer: answer || "No relevant information found",
    confidence,
    sources: properties.sources,
  };
}

function handleSynthesize(
  payload: { fromNodes: string[] },
  properties: KnowledgeProperties,
  graph: Graph
): { synthesis: string; sources: string[] } {
  const allSources: string[] = [...properties.sources];
  const contentParts: string[] = [properties.content];

  for (const nodeId of payload.fromNodes) {
    const nodeProps = graph.getNodeProperties(nodeId);
    if (nodeProps && nodeProps.type === "knowledge") {
      const kProps = nodeProps as KnowledgeProperties;
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

/**
 * Factory function for convenience
 * Creates a KnowledgeActor and returns its Address
 */
export function createKnowledge(options: CreateKnowledgeOptions, graph: Graph): Address {
  return KnowledgeActor({ ...options, graph });
}

/**
 * Get knowledge properties from graph by ID
 */
export function getKnowledgeProperties(knowledgeId: string, graph: Graph): KnowledgeProperties | undefined {
  return graph.getNodeProperties(knowledgeId) as KnowledgeProperties;
}
