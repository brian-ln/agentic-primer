
/**
 * Quality Filter Actor - Filter high-signal content from YouTube feeds
 * Address: primer.signals.youtube.quality-filter
 */

import type { Message, Response } from "../actors/base.ts";
import type { Graph } from "../graph.ts";
import type { VideoMetadata, QualityScore, FilterDecision } from "./types.ts";
import { calculateQualityScore } from "./quality-scoring.ts";

export class QualityFilterActor {
  private graph: Graph;
  private autoProcessThreshold = 6.0;
  private reviewThreshold = 4.0;

  constructor(graph: Graph) {
    this.graph = graph;
  }

  /**
   * Handle actor messages
   */
  async send(message: Message): Promise<Response> {
    switch (message.type) {
      case "filter_video":
        return await this.filterVideo(message.payload as { metadata: VideoMetadata });
      
      case "set_thresholds":
        const { auto, review } = message.payload as { auto?: number, review?: number };
        if (auto !== undefined) this.autoProcessThreshold = auto;
        if (review !== undefined) this.reviewThreshold = review;
        return { success: true };

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  }

  /**
   * Filter a single video based on quality criteria
   */
  private async filterVideo(payload: { metadata: VideoMetadata }): Promise<Response> {
    try {
      const { metadata } = payload;
      const score = calculateQualityScore(metadata);
      
      let action: "auto-process" | "review" | "reject" = "reject";
      
      if (score.composite >= this.autoProcessThreshold) {
        action = "auto-process";
      } else if (score.composite >= this.reviewThreshold) {
        action = "review";
      }

      const decision: FilterDecision = {
        action,
        priority: score.composite >= 8.0 ? "high" : "normal",
        score,
        reason: this.getDecisionReason(action, score),
      };

      return {
        success: true,
        data: decision,
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  private getDecisionReason(action: string, score: QualityScore): string {
    if (action === "auto-process") return `High quality score (${score.composite.toFixed(1)}) with strong depth and relevance.`;
    if (action === "review") return `Moderate quality score (${score.composite.toFixed(1)}), needs manual review.`;
    return `Low quality score (${score.composite.toFixed(1)}), filtered out.`;
  }
}

/**
 * Create Quality Filter Actor
 */
export function createQualityFilter(graph: Graph): QualityFilterActor {
  return new QualityFilterActor(graph);
}
