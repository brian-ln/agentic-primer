/**
 * YouTube Fetcher Actor - Fetch videos from YouTube playlist
 * Address: primer.signals.youtube.fetcher
 * Following actor worldview patterns
 */

import type { Message, Response } from "../actors/base.ts";
import type { YouTubeVideoMetadata } from "./types.ts";
import { spawn } from "node:child_process";
import { promisify } from "node:util";

const execFile = promisify(spawn);

/**
 * YouTube Fetcher Actor
 * Effect actor - interfaces with external YouTube API
 */
export class YouTubeFetcherActor {
  private pythonScript: string;

  constructor() {
    // Path to YouTube skill script
    this.pythonScript = this.findYouTubeScript();
  }

  /**
   * Find YouTube skill script
   */
  private findYouTubeScript(): string {
    // Try to find the bln-cyborg-kit YouTube skill
    const possiblePaths = [
      `${process.env.HOME}/.claude/plugins/cache/bln-cyborg-kit/bln/1.2.0/skills/youtube/scripts/youtube`,
      `${process.env.HOME}/.claude/plugins/cache/local/bln-cyborg-kit/unknown/skills/youtube/scripts/youtube`,
    ];

    for (const path of possiblePaths) {
      try {
        if (require("node:fs").existsSync(path)) {
          return path;
        }
      } catch {
        // Continue searching
      }
    }

    throw new Error("YouTube skill script not found. Install bln-cyborg-kit plugin.");
  }

  /**
   * Handle actor messages
   */
  async send(message: Message): Promise<Response> {
    switch (message.type) {
      case "fetch_playlist":
        return await this.fetchPlaylist(message.payload as { playlistId: string });

      case "fetch_video":
        return await this.fetchVideo(message.payload as { videoId: string });

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
   * Fetch videos from a playlist
   */
  private async fetchPlaylist(payload: { playlistId: string }): Promise<Response> {
    try {
      const { playlistId } = payload;

      if (!playlistId) {
        return {
          success: false,
          error: "Missing playlistId",
        };
      }

      // Use YouTube InnerTube API to fetch playlist items
      const videos = await this.fetchPlaylistVideos(playlistId);

      if (!videos || videos.length === 0) {
        return {
          success: true,
          data: {
            playlistId,
            videos: [],
            count: 0,
          },
        };
      }

      // Fetch full metadata for each video
      const videosWithMetadata: YouTubeVideoMetadata[] = [];

      for (const video of videos) {
        const metadataResponse = await this.fetchVideo({ videoId: video.videoId });

        if (metadataResponse.success) {
          videosWithMetadata.push(metadataResponse.data as YouTubeVideoMetadata);
        } else {
          // Include partial metadata if full fetch fails
          videosWithMetadata.push({
            videoId: video.videoId,
            title: video.title || "",
            description: "",
            publishedAt: new Date().toISOString(),
            duration: 0,
            views: 0,
            channel: "",
            url: `https://youtube.com/watch?v=${video.videoId}`,
          });
        }
      }

      return {
        success: true,
        data: {
          playlistId,
          videos: videosWithMetadata,
          count: videosWithMetadata.length,
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
   * Fetch playlist items using InnerTube API
   * Handles pagination to get all videos
   */
  private async fetchPlaylistVideos(playlistId: string): Promise<Array<{ videoId: string; title?: string }>> {
    const allVideos: Array<{ videoId: string; title?: string }> = [];
    let continuationToken: string | null = null;
    const maxPages = 50; // Safety limit (50 pages × 100 items = 5000 videos max)
    let pageCount = 0;

    do {
      const videos = await this.fetchPlaylistPage(playlistId, continuationToken);

      if (!videos || videos.items.length === 0) {
        break;
      }

      allVideos.push(...videos.items);
      continuationToken = videos.continuation;
      pageCount++;

      // Safety check
      if (pageCount >= maxPages) {
        console.warn(`Reached maximum page limit (${maxPages}) for playlist ${playlistId}`);
        break;
      }

    } while (continuationToken);

    return allVideos;
  }

  /**
   * Fetch a single page of playlist items
   */
  private async fetchPlaylistPage(
    playlistId: string,
    continuationToken: string | null
  ): Promise<{ items: Array<{ videoId: string; title?: string }>; continuation: string | null }> {
    try {
      const url = `https://www.youtube.com/youtubei/v1/browse?key=${this.getInnerTubeApiKey()}`;

      const payload: any = {
        context: {
          client: {
            clientName: "WEB",
            clientVersion: "2.20231201.00.00",
          },
        },
      };

      if (continuationToken) {
        payload.continuation = continuationToken;
      } else {
        payload.browseId = `VL${playlistId}`;
      }

      const response = await fetch(url, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
        },
        body: JSON.stringify(payload),
      });

      if (!response.ok) {
        throw new Error(`YouTube API returned ${response.status}`);
      }

      const data = await response.json();

      // Extract video items from response
      const items: Array<{ videoId: string; title?: string }> = [];
      let nextContinuation: string | null = null;

      // Navigate the response structure
      const contents = continuationToken
        ? data?.onResponseReceivedActions?.[0]?.appendContinuationItemsAction?.continuationItems
        : data?.contents?.twoColumnBrowseResultsRenderer?.tabs?.[0]?.tabRenderer?.content?.sectionListRenderer?.contents?.[0]?.itemSectionRenderer?.contents?.[0]?.playlistVideoListRenderer?.contents;

      if (contents) {
        for (const item of contents) {
          if (item.playlistVideoRenderer) {
            const video = item.playlistVideoRenderer;
            const videoId = video.videoId;
            const title = video.title?.runs?.[0]?.text || video.title?.simpleText;

            if (videoId) {
              items.push({ videoId, title });
            }
          } else if (item.continuationItemRenderer) {
            // Extract continuation token for next page
            nextContinuation = item.continuationItemRenderer?.continuationEndpoint?.continuationCommand?.token;
          }
        }
      }

      return { items, continuation: nextContinuation };
    } catch (error) {
      console.error(`Error fetching playlist page: ${error instanceof Error ? error.message : String(error)}`);
      return { items: [], continuation: null };
    }
  }

  /**
   * Get InnerTube API key (same as used in YouTube script)
   */
  private getInnerTubeApiKey(): string {
    return "AIzaSyAO_FJ2SlqU8Q4STEHLGCilw_Y9_11qcW8";
  }

  /**
   * Fetch details for a specific video
   */
  private async fetchVideo(payload: { videoId: string }): Promise<Response> {
    try {
      const { videoId } = payload;

      if (!videoId) {
        return {
          success: false,
          error: "Missing videoId",
        };
      }

      // Call YouTube skill
      const result = await this.callYouTubeSkill("details", videoId);

      if (result.error) {
        return {
          success: false,
          error: result.error,
        };
      }

      // Transform to our metadata format
      const metadata: YouTubeVideoMetadata = {
        videoId: result.videoId || videoId,
        title: result.title || "",
        description: result.description || "",
        publishedAt: new Date().toISOString(), // YouTube skill doesn't provide this
        duration: result.duration || 0,
        views: result.views || 0,
        channel: result.author || "",
        url: result.url || `https://youtube.com/watch?v=${videoId}`,
        thumbnail: result.thumbnail,
      };

      return {
        success: true,
        data: metadata,
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Search YouTube videos
   */
  private async searchVideos(payload: { query: string; maxResults?: number }): Promise<Response> {
    try {
      const { query, maxResults = 10 } = payload;

      if (!query) {
        return {
          success: false,
          error: "Missing query",
        };
      }

      // Call YouTube skill
      const result = await this.callYouTubeSkill("search", query, maxResults);

      if (result.error) {
        return {
          success: false,
          error: result.error,
        };
      }

      // Transform results to our metadata format
      const videos: YouTubeVideoMetadata[] = (result.videos || []).map((video: any) => ({
        videoId: video.videoId,
        title: video.title,
        description: "", // Search doesn't provide full description
        publishedAt: new Date().toISOString(),
        duration: video.duration || 0,
        views: video.views || 0,
        channel: video.channel || "",
        url: video.url,
      }));

      return {
        success: true,
        data: {
          videos,
          query,
          count: videos.length,
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
   * Call YouTube skill Python script
   */
  private async callYouTubeSkill(action: string, arg1: string, arg2?: number): Promise<any> {
    try {
      const args = [this.pythonScript, action, arg1];
      if (arg2 !== undefined) {
        args.push(String(arg2));
      }

      const child = spawn("python3", args);

      let stdout = "";
      let stderr = "";

      child.stdout?.on("data", (data) => {
        stdout += data.toString();
      });

      child.stderr?.on("data", (data) => {
        stderr += data.toString();
      });

      const exitCode = await new Promise<number>((resolve) => {
        child.on("close", (code) => resolve(code || 0));
      });

      if (exitCode !== 0) {
        throw new Error(`YouTube skill failed: ${stderr}`);
      }

      // Parse JSON output
      return JSON.parse(stdout);
    } catch (error) {
      throw new Error(`Failed to call YouTube skill: ${error instanceof Error ? error.message : String(error)}`);
    }
  }
}

/**
 * Create YouTube Fetcher Actor
 */
export function createYouTubeFetcher(): YouTubeFetcherActor {
  return new YouTubeFetcherActor();
}
