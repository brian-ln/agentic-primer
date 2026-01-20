/**
 * Signal system type definitions
 * Following actor worldview and graph integration patterns
 */

/**
 * YouTube video metadata
 */
export interface YouTubeVideoMetadata {
  videoId: string;
  title: string;
  description: string;
  publishedAt: string;
  duration: number; // seconds
  views: number;
  channel: string;
  url: string;
  thumbnail?: string;
}

/**
 * Transcript segment with timestamp
 */
export interface TranscriptSegment {
  text: string;
  start: number; // seconds
  duration: number; // seconds
}

/**
 * Complete transcript data
 */
export interface TranscriptData {
  transcript: string; // Full text
  segments: TranscriptSegment[];
  language?: string;
}

/**
 * Signal node properties (stored in graph)
 */
export interface SignalProperties {
  type: "signal";
  source: "youtube" | "rss" | "web" | "manual";

  // YouTube-specific fields
  videoId?: string;
  title: string;
  description?: string;
  url: string;

  // Transcript
  transcript?: string;
  transcriptSegments?: TranscriptSegment[];
  transcriptUnavailable?: boolean;

  // Metadata
  publishedAt?: string;
  capturedAt: string;
  duration?: number;
  views?: number;
  channel?: string;
  thumbnail?: string;

  // Graph linking
  linkedToIdeas?: string[]; // Cluster IDs
  linkedToTasks?: string[]; // Task IDs
  linkedToKnowledge?: string[]; // Knowledge node IDs
}

/**
 * Signal fetch result
 */
export interface FetchResult {
  fetched: number;
  processed: number;
  failed: number;
  signals: Array<{
    signalId: string;
    title: string;
    url: string;
    error?: string;
  }>;
}

/**
 * Signal query parameters
 */
export interface SignalQuery {
  source?: string; // Filter by source
  keyword?: string; // Search in title/description/transcript
  fromDate?: string; // ISO date string
  toDate?: string; // ISO date string
  limit?: number;
  offset?: number;
}

/**
 * Signal statistics
 */
export interface SignalStats {
  total: number;
  bySource: Record<string, number>;
  recentCount: number; // Last 7 days
  withTranscript: number;
  linkedToTasks: number;
  linkedToIdeas: number;
}

/**
 * YouTube playlist configuration
 */
export interface YouTubeConfig {
  playlistId?: string;
  lastFetch?: string; // ISO date string
  fetchIntervalHours?: number; // Default: 1
}

/**
 * Signal system configuration
 */
export interface SignalConfig {
  youtube?: YouTubeConfig;
  // Future: rss, web, etc.
}
