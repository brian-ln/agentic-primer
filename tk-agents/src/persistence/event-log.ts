// Event Log - Simple JSONL persistence with replay capability
//
// Design: DEAD SIMPLE append-only event log using JSONL format.
// One event per line. Read and parse for replay.
// No snapshotting, compression, or complex features (yet).

import { appendFileSync, readFileSync, existsSync, writeFileSync } from "node:fs";

/**
 * Event stored in the log
 */
export interface Event {
  /** ISO 8601 timestamp */
  timestamp: string;

  /** Event type (e.g., "actor_registered", "message_sent", "task_created") */
  type: string;

  /** ID of the node/actor this event relates to */
  nodeId: string;

  /** Event payload - arbitrary data */
  data: unknown;

  /** Optional metadata */
  metadata?: Record<string, unknown>;
}

/**
 * Simple append-only event log using JSONL format
 *
 * Features:
 * - Append events as JSON lines (one per line)
 * - Replay events by reading and parsing the file
 * - No snapshotting or compaction (keep it simple)
 *
 * File format:
 * ```
 * {"timestamp":"2026-01-16T12:00:00.000Z","type":"actor_registered","nodeId":"actor-1","data":{...}}
 * {"timestamp":"2026-01-16T12:00:01.000Z","type":"message_sent","nodeId":"actor-1","data":{...}}
 * ```
 */
export class EventLog {
  private filePath: string;

  /**
   * Create or open an event log
   * @param filePath Path to JSONL file
   */
  constructor(filePath: string) {
    this.filePath = filePath;

    // Create file if it doesn't exist
    if (!existsSync(this.filePath)) {
      writeFileSync(this.filePath, "", "utf-8");
    }
  }

  /**
   * Append an event to the log
   *
   * @param event Event to append
   */
  append(event: Event): void {
    // Ensure timestamp is set
    if (!event.timestamp) {
      event.timestamp = new Date().toISOString();
    }

    // Write as single JSON line
    const line = JSON.stringify(event) + "\n";
    appendFileSync(this.filePath, line, "utf-8");
  }

  /**
   * Replay all events by calling handler for each
   *
   * @param handler Function called for each event
   */
  replay(handler: (event: Event) => void): void {
    if (!existsSync(this.filePath)) {
      return; // No file, no events
    }

    const content = readFileSync(this.filePath, "utf-8");
    const lines = content.split("\n");

    for (const line of lines) {
      if (line.trim() === "") {
        continue; // Skip empty lines
      }

      try {
        const event = JSON.parse(line) as Event;
        handler(event);
      } catch (error) {
        console.error(`Failed to parse event line: ${line}`, error);
        // Continue processing other events
      }
    }
  }

  /**
   * Get all events as an array
   *
   * @returns Array of all events
   */
  getAllEvents(): Event[] {
    const events: Event[] = [];
    this.replay((event) => events.push(event));
    return events;
  }

  /**
   * Get events filtered by type
   *
   * @param eventType Event type to filter by
   * @returns Array of matching events
   */
  getEventsByType(eventType: string): Event[] {
    const events: Event[] = [];
    this.replay((event) => {
      if (event.type === eventType) {
        events.push(event);
      }
    });
    return events;
  }

  /**
   * Get events filtered by nodeId
   *
   * @param nodeId Node ID to filter by
   * @returns Array of matching events
   */
  getEventsByNode(nodeId: string): Event[] {
    const events: Event[] = [];
    this.replay((event) => {
      if (event.nodeId === nodeId) {
        events.push(event);
      }
    });
    return events;
  }

  /**
   * Get path to the log file
   */
  getPath(): string {
    return this.filePath;
  }
}

/**
 * Helper to create event log in a standard location
 *
 * @param name Log name (e.g., "main", "test")
 * @returns EventLog instance
 */
export function createEventLog(name: string): EventLog {
  return new EventLog(`./${name}.jsonl`);
}
