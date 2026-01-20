
/**
 * Channel Monitor Actor - Track and scan YouTube channel feeds
 * Address: primer.signals.youtube.channel-monitor
 */

import type { Message, Response } from "../actors/base.ts";
import type { Graph } from "../graph.ts";
import { createYouTubeFetcher } from "./youtube-fetcher.ts";

export class ChannelMonitorActor {
  private graph: Graph;
  private fetcher: ReturnType<typeof createYouTubeFetcher>;

  constructor(graph: Graph) {
    this.graph = graph;
    this.fetcher = createYouTubeFetcher();
  }

  /**
   * Handle actor messages
   */
  async send(message: Message): Promise<Response> {
    switch (message.type) {
      case "add_channel":
        return await this.addChannel(message.payload as { channelId: string, name?: string });
      
      case "list_channels":
        return await this.listChannels();

      case "remove_channel":
        return await this.removeChannel(message.payload as { id: string });

      case "scan_channel":
        return await this.scanChannel(message.payload as { id: string, full?: boolean });

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  }

  /**
   * Add a channel to be monitored
   */
  private async addChannel(payload: { channelId: string, name?: string }): Promise<Response> {
    try {
      const { channelId, name } = payload;
      const nodeId = `channel_youtube_${channelId}`;

      if (this.graph.getNodeProperties(nodeId)) {
        return { success: false, error: "Channel already exists" };
      }

      // In a real implementation, we would fetch channel metadata here
      const properties = {
        type: "channel",
        source: "youtube",
        channelId,
        title: name || `YouTube Channel ${channelId}`,
        addedAt: new Date().toISOString(),
        lastScanAt: null,
      };

      const actor = { send: async () => ({ success: true, data: {} }) };
      const address = this.graph.getSystem().register(actor);
      this.graph.registerNode(nodeId, address, properties);

      // Emit event for replay durability
      const eventLog = (this.graph as any).eventLog;
      if (eventLog) {
        eventLog.append({
          timestamp: new Date().toISOString(),
          type: "channel_created",
          nodeId,
          data: properties,
        });
      }

      return {
        success: true,
        data: { id: nodeId, ...properties },
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * List all monitored channels
   */
  private async listChannels(): Promise<Response> {
    const nodeIds = this.graph.getNodeIds();
    const channels = nodeIds
      .filter(id => typeof id === "string" && id.startsWith("channel_youtube_"))
      .map(id => ({ id, ...this.graph.getNodeProperties(id) }));
    
    return { success: true, data: channels };
  }

  /**
   * Remove a monitored channel
   */
  private async removeChannel(payload: { id: string }): Promise<Response> {
    const success = this.graph.removeNode(payload.id);
    return { success, data: { removed: success } };
  }

  /**
   * Scan a channel for new videos
   */
  private async scanChannel(payload: { id: string, full?: boolean }): Promise<Response> {
    try {
      const { id, full = false } = payload;
      const props = this.graph.getNodeProperties(id) as any;
      
      if (!props || props.type !== "channel") {
        return { success: false, error: "Channel not found" };
      }

      console.log(`Scanning channel ${props.title} (full=${full})...`);

      // Mock scanning logic for now
      // This would call this.fetcher to get video list
      const mockVideos = [
        { videoId: "mock_vid_1", title: "Autonomous Agents Deep Dive", duration: 1800 },
        { videoId: "mock_vid_2", title: "Surface Level Tech News", duration: 300 },
      ];

      // Update last scan timestamp
      props.lastScanAt = new Date().toISOString();

      return {
        success: true,
        data: {
          channel: props.title,
          found: mockVideos.length,
          videos: mockVideos,
        },
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }
}

/**
 * Create Channel Monitor Actor
 */
export function createChannelMonitor(graph: Graph): ChannelMonitorActor {
  return new ChannelMonitorActor(graph);
}
