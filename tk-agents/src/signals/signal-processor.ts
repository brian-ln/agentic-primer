import type { Message, Response } from "../actors/base.ts";
import type { Graph } from "../graph.ts";
import { QualityScoring } from "./quality-scoring.ts";
import type { YouTubeVideoMetadata, TranscriptData, SignalProperties } from "./types.ts";

export class SignalProcessorActor {
  private graph: Graph;
  private scoring: QualityScoring;

  constructor(graph: Graph) {
    this.graph = graph;
    this.scoring = new QualityScoring();
  }

  /**
   * Handle actor messages
   */
  async send(message: Message): Promise<Response> {
    switch (message.type) {
      case "process_signal":
        return await this.processSignal(message.payload as {
          videoId: string;
          metadata: YouTubeVideoMetadata;
          transcript?: TranscriptData;
        });
      
      case "get_signal":
        return await this.getSignal(message.payload as { signalId: string });

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  }

  /**
   * Process a signal: score it and decide if it should be captured
   */
  private async processSignal(payload: { 
    videoId: string;
    metadata: YouTubeVideoMetadata;
    transcript?: TranscriptData;
  }): Promise<Response> {
    try {
      const { videoId, metadata, transcript } = payload;
      
      // Calculate quality score using the dedicated algorithm class
      const scoreResult = this.scoring.calculateScore({
        ...metadata,
        hasTranscript: !!transcript?.transcript,
      });

      // Threshold check (composite score >= 0.5 normalized for capture)
      const THRESHOLD = 0.5;
      const shouldCapture = scoreResult.total >= THRESHOLD;

      if (!shouldCapture) {
        return { 
          success: true, 
          data: { 
            captured: false, 
            reason: "Quality score below threshold", 
            score: scoreResult 
          } 
        };
      }

      const signalId = `signal_youtube_${videoId}`;

      // Check if already captured
      const existing = this.graph.getNodeProperties(signalId);
      if (existing) {
        return { 
          success: true, 
          data: { 
            captured: true, 
            alreadyExists: true, 
            id: signalId,
            score: scoreResult.total
          } 
        };
      }

      // Create signal node properties
      const properties: SignalProperties = {
        type: "signal",
        source: "youtube",
        videoId,
        title: metadata.title,
        description: metadata.description,
        url: metadata.url,
        publishedAt: metadata.publishedAt,
        capturedAt: new Date().toISOString(),
        duration: metadata.duration,
        views: metadata.views,
        channel: metadata.channel,
        thumbnail: metadata.thumbnail,
        transcript: transcript?.transcript,
        transcriptSegments: transcript?.segments,
        transcriptUnavailable: !transcript?.transcript,
        qualityScore: scoreResult.total,
        scoreDetails: scoreResult.breakdown,
      };

      // Create a simple actor for the signal node
      const signalActor = {
        send: async (message: Message): Promise<Response> => {
          if (message.type === "get_properties") {
            return { success: true, data: properties };
          }
          return { success: false, error: "Unknown message" };
        },
      };

      // Register signal node in graph
      const System = (await import("../actors/system.ts")).System;
      const system = System();
      const address = system.actorOf(() => signalActor);

      this.graph.registerNode(signalId, address, properties);

      // Emit event for replay durability
      if (this.graph.eventLog) {
        this.graph.eventLog.append({
          timestamp: new Date().toISOString(),
          type: "signal_captured",
          nodeId: signalId,
          data: properties
        });
      }

      return {
        success: true,
        data: {
          captured: true,
          id: signalId,
          score: scoreResult.total,
        },
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Get signal details
   */
  private async getSignal(payload: { signalId: string }): Promise<Response> {
    const { signalId } = payload;
    const props = this.graph.getNodeProperties(signalId);
    if (!props) return { success: false, error: "Signal not found" };
    
    const edges = this.graph.getAllEdges(signalId);
    return { success: true, data: { id: signalId, ...props, edges } };
  }
}

/**
 * Create Signal Processor Actor
 */
export function createSignalProcessor(graph: Graph): SignalProcessorActor {
  return new SignalProcessorActor(graph);
}
