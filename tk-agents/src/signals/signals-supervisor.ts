/**
 * Signals Supervisor Actor - Coordinate signal detection and processing
 * Address: primer.signals.youtube
 * Following actor worldview patterns
 */

import type { Message, Response } from "../actors/base.ts";
import type { Graph } from "../graph.ts";
import type { FetchResult, SignalQuery, SignalStats } from "./types.ts";
import { createYouTubeFetcher } from "./youtube-fetcher.ts";
import { createYouTubeTranscriber } from "./youtube-transcriber.ts";
import { createSignalProcessor } from "./signal-processor.ts";

/**
 * Signals Supervisor Actor
 * Coordinates signal fetching, transcription, and processing
 */
export class SignalsSupervisorActor {
  private graph: Graph;
  private fetcher: ReturnType<typeof createYouTubeFetcher>;
  private transcriber: ReturnType<typeof createYouTubeTranscriber>;
  private processor: ReturnType<typeof createSignalProcessor>;

  constructor(graph: Graph) {
    this.graph = graph;
    this.fetcher = createYouTubeFetcher();
    this.transcriber = createYouTubeTranscriber();
    this.processor = createSignalProcessor(graph);
  }

  /**
   * Handle actor messages
   */
  async send(message: Message): Promise<Response> {
    switch (message.type) {
      case "fetch_signals":
        return await this.fetchSignals(message.payload as { videoIds?: string[] });

      case "fetch_from_playlist":
        return await this.fetchFromPlaylist(message.payload as { playlistId: string });

      case "process_video":
        return await this.processVideo(message.payload as { videoId: string });

      case "query":
        return await this.querySignals(message.payload as SignalQuery);

      case "get_stats":
        return await this.getStats();

      case "search_videos":
        return await this.searchVideos(message.payload as { query: string; maxResults?: number });

      default:
        return {
          success: false,
          error: `Unknown message type: ${message.type}`,
        };
    }
  }

  /**
   * Fetch and process signals from a YouTube playlist
   */
  private async fetchFromPlaylist(payload: { playlistId: string }): Promise<Response> {
    try {
      const { playlistId } = payload;

      if (!playlistId) {
        return {
          success: false,
          error: "Missing playlistId",
        };
      }

      // Fetch playlist videos
      const playlistResponse = await this.fetcher.send({
        id: crypto.randomUUID(),
        type: "fetch_playlist",
        payload: { playlistId },
      });

      if (!playlistResponse.success) {
        return {
          success: false,
          error: playlistResponse.error || "Failed to fetch playlist",
        };
      }

      const videos = playlistResponse.data.videos || [];
      const videoIds = videos.map((v: any) => v.videoId);

      // Store playlist metadata in graph
      const playlistNodeId = `playlist_${playlistId}`;
      this.graph.registerNode(
        playlistNodeId,
        "primer.signals.youtube",
        {
          type: "playlist",
          playlistId,
          title: `YouTube Playlist ${playlistId}`,
          videoCount: videos.length,
          fetchedAt: new Date().toISOString(),
        }
      );

      // Process all videos from playlist
      const result = await this.fetchSignals({ videoIds });

      // Link signals to playlist
      if (result.success && result.data.signals) {
        for (const signal of result.data.signals) {
          if (signal.signalId && !signal.error) {
            this.graph.addEdge(
              signal.signalId,
              playlistNodeId,
              "sourced_from",
              {
                sourceType: "playlist",
                fetchedAt: new Date().toISOString(),
              }
            );
          }
        }
      }

      return {
        success: true,
        data: {
          ...result.data,
          playlistId,
          playlistNodeId,
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
   * Fetch and process signals
   */
  private async fetchSignals(payload: { videoIds?: string[] }): Promise<Response> {
    try {
      const { videoIds = [] } = payload;

      if (videoIds.length === 0) {
        return {
          success: false,
          error: "No video IDs provided. Use search_videos first or provide specific video IDs.",
        };
      }

      const result: FetchResult = {
        fetched: 0,
        processed: 0,
        failed: 0,
        signals: [],
      };

      // Process each video
      for (const videoId of videoIds) {
        try {
          // Fetch video metadata
          const metadataResponse = await this.fetcher.send({
            id: crypto.randomUUID(),
            type: "fetch_video",
            payload: { videoId },
          });

          if (!metadataResponse.success) {
            result.failed++;
            result.signals.push({
              signalId: `signal_youtube_${videoId}`,
              title: "",
              url: `https://youtube.com/watch?v=${videoId}`,
              error: metadataResponse.error,
            });
            continue;
          }

          result.fetched++;
          const metadata = metadataResponse.data;

          // Fetch transcript
          const transcriptResponse = await this.transcriber.send({
            id: crypto.randomUUID(),
            type: "get_transcript",
            payload: { videoId },
          });

          const transcript = transcriptResponse.success ? transcriptResponse.data : undefined;

          // Process and store signal
          const processResponse = await this.processor.send({
            id: crypto.randomUUID(),
            type: "process_signal",
            payload: {
              videoId,
              metadata,
              transcript,
            },
          });

          if (processResponse.success) {
            result.processed++;
            result.signals.push({
              signalId: processResponse.data.signalId,
              title: processResponse.data.title,
              url: processResponse.data.url,
            });
          } else {
            result.failed++;
            result.signals.push({
              signalId: `signal_youtube_${videoId}`,
              title: metadata.title,
              url: metadata.url,
              error: processResponse.error,
            });
          }
        } catch (error) {
          result.failed++;
          result.signals.push({
            signalId: `signal_youtube_${videoId}`,
            title: "",
            url: `https://youtube.com/watch?v=${videoId}`,
            error: error instanceof Error ? error.message : String(error),
          });
        }
      }

      return {
        success: true,
        data: result,
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Process a single video
   */
  private async processVideo(payload: { videoId: string }): Promise<Response> {
    return this.fetchSignals({ videoIds: [payload.videoId] });
  }

  /**
   * Query signals
   */
  private async querySignals(query: SignalQuery): Promise<Response> {
    try {
      const {
        source = "youtube",
        keyword,
        fromDate,
        toDate,
        limit = 50,
        offset = 0,
      } = query;

      // Get all signal nodes from graph
      const allNodes = this.graph.getNodeIds();
      let signals = allNodes
        .filter((id) => id.startsWith("signal_"))
        .map((id) => ({
          id,
          ...this.graph.getNodeProperties(id),
        }))
        .filter((node) => node.type === "signal");

      // Filter by source
      if (source) {
        signals = signals.filter((s: any) => s.source === source);
      }

      // Filter by keyword (search in title, description, transcript)
      if (keyword) {
        const lowerKeyword = keyword.toLowerCase();
        signals = signals.filter((s: any) => {
          const title = (s.title || "").toLowerCase();
          const description = (s.description || "").toLowerCase();
          const transcript = (s.transcript || "").toLowerCase();
          return (
            title.includes(lowerKeyword) ||
            description.includes(lowerKeyword) ||
            transcript.includes(lowerKeyword)
          );
        });
      }

      // Filter by date range
      if (fromDate) {
        signals = signals.filter((s: any) => s.publishedAt >= fromDate);
      }
      if (toDate) {
        signals = signals.filter((s: any) => s.publishedAt <= toDate);
      }

      // Sort by capturedAt (newest first)
      signals.sort((a: any, b: any) => {
        const dateA = new Date(a.capturedAt || 0).getTime();
        const dateB = new Date(b.capturedAt || 0).getTime();
        return dateB - dateA;
      });

      // Pagination
      const total = signals.length;
      signals = signals.slice(offset, offset + limit);

      return {
        success: true,
        data: {
          signals,
          total,
          offset,
          limit,
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
   * Get signal statistics
   */
  private async getStats(): Promise<Response> {
    try {
      const allNodes = this.graph.getNodeIds();
      const signals = allNodes
        .filter((id) => id.startsWith("signal_"))
        .map((id) => this.graph.getNodeProperties(id))
        .filter((node) => node?.type === "signal");

      const sevenDaysAgo = new Date();
      sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);

      const stats: SignalStats = {
        total: signals.length,
        bySource: {},
        recentCount: 0,
        withTranscript: 0,
        linkedToTasks: 0,
        linkedToIdeas: 0,
      };

      for (const signal of signals) {
        const s = signal as any;

        // Count by source
        stats.bySource[s.source] = (stats.bySource[s.source] || 0) + 1;

        // Count recent
        if (s.capturedAt && new Date(s.capturedAt) >= sevenDaysAgo) {
          stats.recentCount++;
        }

        // Count with transcript
        if (s.transcript) {
          stats.withTranscript++;
        }

        // Count links
        if (s.linkedToTasks && s.linkedToTasks.length > 0) {
          stats.linkedToTasks++;
        }
        if (s.linkedToIdeas && s.linkedToIdeas.length > 0) {
          stats.linkedToIdeas++;
        }
      }

      return {
        success: true,
        data: stats,
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Search YouTube videos (doesn't store them yet)
   */
  private async searchVideos(payload: { query: string; maxResults?: number }): Promise<Response> {
    return this.fetcher.send({
      id: crypto.randomUUID(),
      type: "search_videos",
      payload,
    });
  }
}

/**
 * Create Signals Supervisor Actor
 */
export function createSignalsSupervisor(graph: Graph): SignalsSupervisorActor {
  return new SignalsSupervisorActor(graph);
}
