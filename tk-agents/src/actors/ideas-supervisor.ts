// Ideas Supervisor Actor
// Manages idea clusters from semantic prompt analysis
// Actor Address: primer.ideas (supervisor)
// Child Actors: primer.ideas.cluster_N

import { StreamingDBSCAN, type Cluster, type PromptMetadata } from "../semantic/clustering.ts";
import { getEmbedding } from "../semantic/embeddings.ts";
import type { Graph } from "../graph.ts";

export interface IdeaCapture {
  id?: string;
  text: string;
  timestamp: string;
  interrupted: boolean;
  source?: string;
}

export interface ActorMessage {
  id: string;
  type: string;
  payload: any;
  sender?: string;
  timestamp?: string;
}

export interface ActorResponse {
  success: boolean;
  data?: any;
  error?: string;
  code?: string;
}

/**
 * Ideas Supervisor Actor
 * Handles semantic capture and clustering of user prompts
 */
export class IdeasSupervisor {
  private clustering: StreamingDBSCAN;
  private graph: Graph;
  private ideaCounter: number = 0;

  constructor(graph: Graph, epsilon: number = 0.3, minPoints: number = 2) {
    this.graph = graph;
    this.clustering = new StreamingDBSCAN(epsilon, minPoints);

    // Register supervisor in graph
    this.registerSupervisorInGraph();
  }

  /**
   * Register supervisor node in graph
   * Note: Graph uses actor-based registerNode, so we skip for now
   * The supervisor exists logically without a graph node
   */
  private registerSupervisorInGraph(): void {
    // TODO: Integrate with proper actor-based graph registration
    // For now, the supervisor exists as a logical entity
  }

  /**
   * Handle incoming actor messages
   */
  async handleMessage(msg: ActorMessage): Promise<ActorResponse> {
    try {
      switch (msg.type) {
        case "capture":
          return await this.captureIdea(msg.payload);

        case "query":
          return await this.queryIdeas(msg.payload);

        case "link_to_task":
          return await this.linkToTask(msg.payload);

        case "get_cluster":
          return this.getCluster(msg.payload);

        case "get_stats":
          return this.getStats();

        default:
          return {
            success: false,
            error: `Unknown message type: ${msg.type}`,
            code: "UNKNOWN_MESSAGE_TYPE",
          };
      }
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
        code: "INTERNAL_ERROR",
      };
    }
  }

  /**
   * Capture a new idea and assign to cluster
   */
  private async captureIdea(idea: IdeaCapture): Promise<ActorResponse> {
    if (!idea.text || idea.text.trim().length === 0) {
      return {
        success: false,
        error: "Idea text cannot be empty",
        code: "INVALID_REQUEST",
      };
    }

    try {
      // Generate embedding
      const embedding = await getEmbedding(idea.text);

      // Assign to cluster
      const metadata: PromptMetadata = {
        text: idea.text,
        timestamp: idea.timestamp || new Date().toISOString(),
        interrupted: idea.interrupted,
        source: idea.source,
      };

      const clusterId = await this.clustering.addPoint(embedding, metadata);

      // Register cluster actor in graph (if new)
      await this.registerClusterInGraph(clusterId);

      // Create idea node in graph
      const ideaId = this.createIdeaNode(idea, embedding, clusterId);

      return {
        success: true,
        data: {
          ideaId,
          clusterId,
          embedding: embedding.slice(0, 10), // Return first 10 dims for verification
        },
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
        code: "EMBEDDING_ERROR",
      };
    }
  }

  /**
   * Create idea node in graph
   * Note: Simplified for now - stores in clustering state
   */
  private createIdeaNode(
    idea: IdeaCapture,
    embedding: number[],
    clusterId: number
  ): string {
    const ideaId = idea.id || `idea_${++this.ideaCounter}`;
    // TODO: Integrate with proper actor-based graph when needed
    // For MVP, clustering state is sufficient
    return ideaId;
  }

  /**
   * Register cluster actor in graph
   * Note: Simplified for now - uses clustering state
   */
  private async registerClusterInGraph(clusterId: number): Promise<void> {
    // TODO: Integrate with proper actor-based graph when needed
    // For MVP, clustering provides the necessary state
  }

  /**
   * Query ideas by various criteria
   */
  private async queryIdeas(query: {
    keyword?: string;
    clusterId?: number;
    limit?: number;
    includeEmbeddings?: boolean;
  }): Promise<ActorResponse> {
    try {
      let clusters: Cluster[];

      if (query.clusterId !== undefined) {
        const cluster = this.clustering.getCluster(query.clusterId);
        clusters = cluster ? [cluster] : [];
      } else if (query.keyword) {
        clusters = this.clustering.findClustersByKeyword(query.keyword);
      } else {
        clusters = this.clustering.getAllClusters();
      }

      // Apply limit
      if (query.limit) {
        clusters = clusters.slice(0, query.limit);
      }

      // Format response
      const formattedClusters = clusters.map(cluster => ({
        id: cluster.id,
        address: `primer.ideas.cluster_${cluster.id}`,
        topicKeywords: cluster.topicKeywords,
        ideaCount: cluster.points.length,
        createdAt: cluster.createdAt.toISOString(),
        lastActive: cluster.lastActive.toISOString(),
        ideas: cluster.points.map(point => ({
          id: point.id,
          text: point.metadata.text,
          timestamp: point.metadata.timestamp,
          interrupted: point.metadata.interrupted,
          source: point.metadata.source,
          embedding: query.includeEmbeddings ? point.embedding : undefined,
        })),
      }));

      return {
        success: true,
        data: {
          clusters: formattedClusters,
          totalClusters: clusters.length,
        },
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
        code: "QUERY_ERROR",
      };
    }
  }

  /**
   * Link idea cluster to a task
   */
  private async linkToTask(payload: {
    clusterId: number;
    taskId: string;
  }): Promise<ActorResponse> {
    if (!payload.clusterId || !payload.taskId) {
      return {
        success: false,
        error: "Both clusterId and taskId are required",
        code: "INVALID_REQUEST",
      };
    }

    try {
      const clusterAddress = `primer.ideas.cluster_${payload.clusterId}`;

      // Verify cluster exists
      const cluster = this.clustering.getCluster(payload.clusterId);
      if (!cluster) {
        return {
          success: false,
          error: `Cluster ${payload.clusterId} not found`,
          code: "NOT_FOUND",
        };
      }

      // Create edge from cluster to task using correct API
      this.graph.addEdge(clusterAddress, payload.taskId, "inspired", {
        timestamp: new Date().toISOString(),
      });

      return {
        success: true,
        data: {
          clusterAddress,
          taskId: payload.taskId,
          edgeType: "inspired",
        },
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
        code: "LINK_ERROR",
      };
    }
  }

  /**
   * Get specific cluster details
   */
  private getCluster(payload: { clusterId: number }): ActorResponse {
    const cluster = this.clustering.getCluster(payload.clusterId);

    if (!cluster) {
      return {
        success: false,
        error: `Cluster ${payload.clusterId} not found`,
        code: "NOT_FOUND",
      };
    }

    return {
      success: true,
      data: {
        id: cluster.id,
        address: `primer.ideas.cluster_${cluster.id}`,
        topicKeywords: cluster.topicKeywords,
        ideaCount: cluster.points.length,
        createdAt: cluster.createdAt.toISOString(),
        lastActive: cluster.lastActive.toISOString(),
        similarityThreshold: cluster.similarityThreshold,
        ideas: cluster.points.map(point => ({
          id: point.id,
          text: point.metadata.text,
          timestamp: point.metadata.timestamp,
          interrupted: point.metadata.interrupted,
          source: point.metadata.source,
        })),
      },
    };
  }

  /**
   * Get clustering statistics
   */
  private getStats(): ActorResponse {
    const stats = this.clustering.getStats();

    return {
      success: true,
      data: {
        ...stats,
        supervisorAddress: "primer.ideas",
      },
    };
  }

  /**
   * Expire old clusters
   */
  expireOldClusters(maxAgeMs: number = 24 * 60 * 60 * 1000): number {
    return this.clustering.expireOldClusters(maxAgeMs);
  }

  /**
   * Get clustering instance (for testing)
   */
  getClustering(): StreamingDBSCAN {
    return this.clustering;
  }
}
