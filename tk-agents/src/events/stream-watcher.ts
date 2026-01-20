/**
 * Event Stream Watcher
 *
 * Watches session and agent output files using Bun's fs.watch()
 * Provides structured event streaming with searchable interface.
 * Better than grep: time-based filtering, structured search, real-time updates.
 */

import { watch, type FSWatcher } from "fs";
import { readFileSync, existsSync, statSync } from "fs";
import { join } from "path";

export interface StreamEvent {
  timestamp: string;
  source: string; // file path
  type: string;
  data: unknown;
  raw?: string;
}

export interface SearchOptions {
  type?: string;
  source?: string;
  after?: string; // ISO timestamp or relative like "10min ago"
  before?: string;
  limit?: number;
}

/**
 * EventStream - Captures and stores events from watched files
 */
export class EventStream {
  private events: StreamEvent[] = [];
  private maxEvents: number;

  constructor(maxEvents: number = 10000) {
    this.maxEvents = maxEvents;
  }

  /**
   * Add an event to the stream
   */
  add(event: StreamEvent): void {
    this.events.push(event);

    // Keep only maxEvents most recent events
    if (this.events.length > this.maxEvents) {
      this.events = this.events.slice(-this.maxEvents);
    }
  }

  /**
   * Search events with filters
   */
  search(options: SearchOptions = {}): StreamEvent[] {
    let filtered = this.events;

    // Filter by type
    if (options.type) {
      filtered = filtered.filter((e) => e.type === options.type);
    }

    // Filter by source
    if (options.source) {
      filtered = filtered.filter((e) => e.source.includes(options.source!));
    }

    // Filter by time range
    if (options.after) {
      const afterTime = this.parseTimeFilter(options.after);
      filtered = filtered.filter(
        (e) => new Date(e.timestamp).getTime() >= afterTime
      );
    }

    if (options.before) {
      const beforeTime = this.parseTimeFilter(options.before);
      filtered = filtered.filter(
        (e) => new Date(e.timestamp).getTime() <= beforeTime
      );
    }

    // Apply limit
    if (options.limit) {
      filtered = filtered.slice(-options.limit);
    }

    return filtered;
  }

  /**
   * Get all events
   */
  getAll(): StreamEvent[] {
    return [...this.events];
  }

  /**
   * Get count of stored events
   */
  count(): number {
    return this.events.length;
  }

  /**
   * Clear all events
   */
  clear(): void {
    this.events = [];
  }

  /**
   * Parse time filter (ISO timestamp or relative like "10min ago")
   */
  private parseTimeFilter(timeStr: string): number {
    // Try ISO timestamp first
    if (timeStr.includes("T") || timeStr.includes("-")) {
      return new Date(timeStr).getTime();
    }

    // Parse relative time (e.g., "10min ago", "2h ago", "30s ago")
    const match = timeStr.match(/^(\d+)(s|min|h|d)\s+ago$/i);
    if (match) {
      const [, amount, unit] = match;
      const value = parseInt(amount, 10);

      const now = Date.now();
      switch (unit.toLowerCase()) {
        case "s":
          return now - value * 1000;
        case "min":
          return now - value * 60 * 1000;
        case "h":
          return now - value * 60 * 60 * 1000;
        case "d":
          return now - value * 24 * 60 * 60 * 1000;
      }
    }

    // Default to now if can't parse
    return Date.now();
  }
}

/**
 * FileWatcher - Watches a single file for changes
 */
class FileWatcher {
  private watcher: FSWatcher | null = null;
  private filePath: string;
  private lastSize: number = 0;
  private onNewContent: (content: string) => void;

  constructor(filePath: string, onNewContent: (content: string) => void) {
    this.filePath = filePath;
    this.onNewContent = onNewContent;

    // Initialize with current file size
    if (existsSync(filePath)) {
      this.lastSize = statSync(filePath).size;
    }
  }

  /**
   * Start watching the file
   */
  start(): void {
    if (this.watcher) {
      return; // Already watching
    }

    this.watcher = watch(this.filePath, (eventType) => {
      if (eventType === "change") {
        this.handleChange();
      }
    });
  }

  /**
   * Stop watching the file
   */
  stop(): void {
    if (this.watcher) {
      this.watcher.close();
      this.watcher = null;
    }
  }

  /**
   * Handle file change event
   */
  private handleChange(): void {
    if (!existsSync(this.filePath)) {
      return;
    }

    const stats = statSync(this.filePath);
    const currentSize = stats.size;

    // Only read if file grew (new content appended)
    if (currentSize > this.lastSize) {
      const content = readFileSync(this.filePath, "utf-8");
      const newContent = content.slice(this.lastSize);
      this.lastSize = currentSize;

      if (newContent.trim()) {
        this.onNewContent(newContent);
      }
    }
  }

  /**
   * Force read current file content
   */
  readCurrent(): string {
    if (!existsSync(this.filePath)) {
      return "";
    }

    return readFileSync(this.filePath, "utf-8");
  }
}

/**
 * StreamWatcher - Main event streaming system
 *
 * Watches multiple files, parses events, provides searchable interface.
 */
export class StreamWatcher {
  private watchers: Map<string, FileWatcher> = new Map();
  private stream: EventStream;
  private parsers: Map<string, (line: string, source: string) => StreamEvent | null> =
    new Map();

  constructor(maxEvents: number = 10000) {
    this.stream = new EventStream(maxEvents);

    // Register default parsers
    this.registerParser("jsonl", this.parseJsonLine.bind(this));
    this.registerParser("output", this.parseOutputLine.bind(this));
  }

  /**
   * Watch a file for new events
   */
  watch(filePath: string, parserType: string = "jsonl"): void {
    if (this.watchers.has(filePath)) {
      return; // Already watching
    }

    const watcher = new FileWatcher(filePath, (newContent) => {
      this.handleNewContent(filePath, newContent, parserType);
    });

    watcher.start();
    this.watchers.set(filePath, watcher);

    // Read existing content
    const existing = watcher.readCurrent();
    if (existing) {
      this.handleNewContent(filePath, existing, parserType, true);
    }
  }

  /**
   * Stop watching a file
   */
  unwatch(filePath: string): void {
    const watcher = this.watchers.get(filePath);
    if (watcher) {
      watcher.stop();
      this.watchers.delete(filePath);
    }
  }

  /**
   * Stop watching all files
   */
  unwatchAll(): void {
    for (const watcher of this.watchers.values()) {
      watcher.stop();
    }
    this.watchers.clear();
  }

  /**
   * Register a custom parser
   */
  registerParser(
    name: string,
    parser: (line: string, source: string) => StreamEvent | null
  ): void {
    this.parsers.set(name, parser);
  }

  /**
   * Search events
   */
  search(options: SearchOptions = {}): StreamEvent[] {
    return this.stream.search(options);
  }

  /**
   * Get all events
   */
  getAll(): StreamEvent[] {
    return this.stream.getAll();
  }

  /**
   * Get event count
   */
  count(): number {
    return this.stream.count();
  }

  /**
   * Clear all events
   */
  clear(): void {
    this.stream.clear();
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private handleNewContent(
    filePath: string,
    content: string,
    parserType: string,
    isInitial: boolean = false
  ): void {
    const parser = this.parsers.get(parserType);
    if (!parser) {
      console.warn(`No parser registered for type: ${parserType}`);
      return;
    }

    const lines = content.split("\n").filter((line) => line.trim());

    for (const line of lines) {
      const event = parser(line, filePath);
      if (event) {
        // For initial content, use file's timestamp if available
        if (isInitial && existsSync(filePath)) {
          const stats = statSync(filePath);
          event.timestamp = stats.mtime.toISOString();
        }

        this.stream.add(event);
      }
    }
  }

  // ============================================================================
  // Default Parsers
  // ============================================================================

  private parseJsonLine(line: string, source: string): StreamEvent | null {
    try {
      const data = JSON.parse(line);
      return {
        timestamp: data.timestamp || new Date().toISOString(),
        source,
        type: data.type || "unknown",
        data,
      };
    } catch {
      return null; // Invalid JSON, skip
    }
  }

  private parseOutputLine(line: string, source: string): StreamEvent | null {
    // Try to parse as JSON first
    try {
      const data = JSON.parse(line);
      return {
        timestamp: data.timestamp || new Date().toISOString(),
        source,
        type: data.type || "output",
        data,
        raw: line,
      };
    } catch {
      // Plain text line
      return {
        timestamp: new Date().toISOString(),
        source,
        type: "output",
        data: { text: line },
        raw: line,
      };
    }
  }
}

/**
 * Create a StreamWatcher and watch common agent/session files
 */
export function createStreamWatcher(
  projectPath: string,
  maxEvents: number = 10000
): StreamWatcher {
  const watcher = new StreamWatcher(maxEvents);

  // Watch events.jsonl if it exists
  const eventsPath = join(projectPath, "events.jsonl");
  if (existsSync(eventsPath)) {
    watcher.watch(eventsPath, "jsonl");
  }

  return watcher;
}

/**
 * Watch all agent output files in /tmp/claude directory
 */
export function watchAgentOutputs(
  watcher: StreamWatcher,
  projectPath: string
): void {
  // Construct /tmp/claude path from project path
  const sanitizedPath = projectPath.replace(/\//g, "-");
  const tmpDir = `/private/tmp/claude/${sanitizedPath}/tasks`;

  if (!existsSync(tmpDir)) {
    console.warn(`Agent output directory not found: ${tmpDir}`);
    return;
  }

  const { readdirSync } = require("fs");
  const files = readdirSync(tmpDir);

  for (const file of files) {
    if (file.endsWith(".output")) {
      const fullPath = join(tmpDir, file);
      watcher.watch(fullPath, "output");
    }
  }
}
