/**
 * YouTube Transcriber Actor - Fetch video transcripts
 * Address: primer.signals.youtube.transcriber
 * Following actor worldview patterns
 */

import type { Message, Response } from "../actors/base.ts";
import type { TranscriptData, TranscriptSegment } from "./types.ts";
import { spawn } from "node:child_process";

/**
 * YouTube Transcriber Actor
 * Effect actor - interfaces with external YouTube transcript API
 */
export class YouTubeTranscriberActor {
  private pythonScript: string;

  constructor() {
    // Path to YouTube skill script
    this.pythonScript = this.findYouTubeScript();
  }

  /**
   * Find YouTube skill script
   */
  private findYouTubeScript(): string {
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
      case "get_transcript":
        return await this.getTranscript(message.payload as { videoId: string; language?: string });

      default:
        return {
          success: false,
          error: `Unknown message type: ${message.type}`,
        };
    }
  }

  /**
   * Get transcript for a video
   */
  private async getTranscript(payload: { videoId: string; language?: string }): Promise<Response> {
    try {
      const { videoId, language = "en" } = payload;

      if (!videoId) {
        return {
          success: false,
          error: "Missing videoId",
        };
      }

      // Call YouTube skill to get transcript
      const result = await this.callYouTubeSkill("transcript", videoId, language);

      if (result.error) {
        // Transcript not available
        return {
          success: true,
          data: {
            transcript: "",
            segments: [],
            transcriptUnavailable: true,
            error: result.error,
          },
        };
      }

      // Parse transcript data
      const segments: TranscriptSegment[] = (result.segments || []).map((seg: any) => ({
        text: seg.text,
        start: seg.start || 0,
        duration: seg.duration || 0,
      }));

      const fullTranscript = segments.map((s) => s.text).join(" ");

      const transcriptData: TranscriptData = {
        transcript: fullTranscript,
        segments,
        language: language,
      };

      return {
        success: true,
        data: transcriptData,
      };
    } catch (error) {
      // If transcript fetch fails, return success but mark as unavailable
      return {
        success: true,
        data: {
          transcript: "",
          segments: [],
          transcriptUnavailable: true,
          error: error instanceof Error ? error.message : String(error),
        },
      };
    }
  }

  /**
   * Call YouTube skill Python script
   */
  private async callYouTubeSkill(action: string, videoId: string, language?: string): Promise<any> {
    try {
      const args = [this.pythonScript, action, videoId];
      if (language) {
        args.push(language);
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
 * Create YouTube Transcriber Actor
 */
export function createYouTubeTranscriber(): YouTubeTranscriberActor {
  return new YouTubeTranscriberActor();
}
