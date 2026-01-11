/**
 * EventLogActor - Append-only event storage with replay capability
 *
 * Responsibilities:
 * - Persist events to JSONL file
 * - Query events with filters
 * - Checkpoint for replay
 * - Replay events from checkpoint
 */

import { promises as fs } from 'fs';
import { createWriteStream, createReadStream } from 'fs';
import { createInterface } from 'readline';
import { join, dirname } from 'path';
import { PROTOCOLS, ACTIONS, createMessage, validateMessage } from '../protocol.js';

/**
 * Simple ULID generation (monotonic)
 * Format: 10 chars timestamp (base32) + 16 chars random (base32)
 */
function generateULID() {
  const ENCODING = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'; // Crockford's base32
  const TIME_LEN = 10;
  const RANDOM_LEN = 16;

  const now = Date.now();
  let timeStr = '';
  let timeValue = now;

  // Encode timestamp
  for (let i = TIME_LEN - 1; i >= 0; i--) {
    const mod = timeValue % 32;
    timeStr = ENCODING[mod] + timeStr;
    timeValue = Math.floor(timeValue / 32);
  }

  // Encode random
  let randomStr = '';
  for (let i = 0; i < RANDOM_LEN; i++) {
    randomStr += ENCODING[Math.floor(Math.random() * 32)];
  }

  return timeStr + randomStr;
}

/**
 * EventLogActor class
 */
export class EventLogActor {
  constructor(config = {}) {
    this.logPath = config.eventLog?.file || 'events.jsonl';
    this.checkpointInterval = config.eventLog?.checkpointInterval || 1000;
    this.writeStream = null;
    this.eventCount = 0;
    this.lastCheckpoint = 0;
    this.isInitialized = false;
  }

  /**
   * Start the actor - create log file if needed
   */
  async start() {
    try {
      // Ensure directory exists
      const logDir = dirname(this.logPath);
      await fs.mkdir(logDir, { recursive: true });

      // Check if log file exists and get current event count
      try {
        const stats = await fs.stat(this.logPath);
        if (stats.isFile()) {
          // Count existing events
          this.eventCount = await this._countEvents();
        }
      } catch (err) {
        if (err.code !== 'ENOENT') {
          throw err;
        }
        // File doesn't exist, will be created on first write
      }

      // Create append stream
      this.writeStream = createWriteStream(this.logPath, { flags: 'a' });

      this.writeStream.on('error', (err) => {
        console.error('Write stream error:', err);
      });

      this.isInitialized = true;
      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  }

  /**
   * Handle incoming messages
   */
  async handleMessage(message) {
    const validation = validateMessage(message);
    if (!validation.valid) {
      return {
        success: false,
        error: validation.error
      };
    }

    if (message.protocol !== PROTOCOLS.EVENT) {
      return {
        success: false,
        error: `Invalid protocol: ${message.protocol}, expected ${PROTOCOLS.EVENT}`
      };
    }

    switch (message.action) {
      case ACTIONS.APPEND:
        return await this.appendEvent(message.data);

      case ACTIONS.QUERY:
        return await this.queryEvents(message.data);

      case ACTIONS.CHECKPOINT:
        return await this.checkpoint();

      case 'replay':
        return await this.replayFrom(message.data);

      default:
        return {
          success: false,
          error: `Unknown action: ${message.action}`
        };
    }
  }

  /**
   * Append event to JSONL log
   */
  async appendEvent(eventData) {
    if (!this.isInitialized) {
      await this.initialize();
    }

    try {
      // Add ULID if not present
      const event = {
        id: eventData.id || `evt_${generateULID()}`,
        timestamp: eventData.timestamp || new Date().toISOString(),
        type: eventData.type,
        data: eventData.data || {},
        metadata: {
          source: eventData.metadata?.source || 'unknown',
          triggeredBy: eventData.metadata?.triggeredBy || null,
          depth: eventData.metadata?.depth || 0,
          fingerprint: eventData.metadata?.fingerprint || null,
          ...eventData.metadata
        }
      };

      // Validate required fields
      if (!event.type) {
        return {
          success: false,
          error: 'Event type is required'
        };
      }

      // Write to JSONL (one line per event)
      const line = JSON.stringify(event) + '\n';

      return new Promise((resolve, reject) => {
        this.writeStream.write(line, (err) => {
          if (err) {
            resolve({
              success: false,
              error: `Write error: ${err.message}`
            });
          } else {
            this.eventCount++;

            // Auto-checkpoint at interval
            if (this.eventCount - this.lastCheckpoint >= this.checkpointInterval) {
              this.lastCheckpoint = this.eventCount;
            }

            resolve({
              success: true,
              eventId: event.id,
              eventCount: this.eventCount
            });
          }
        });
      });
    } catch (error) {
      return {
        success: false,
        error: `Append error: ${error.message}`
      };
    }
  }

  /**
   * Query events with filters
   *
   * @param {Object} options - Query options
   * @param {Function} options.filter - Filter function (event) => boolean
   * @param {number} options.limit - Max events to return
   * @param {number} options.offset - Skip first N events
   * @param {boolean} options.reverse - Read in reverse order
   */
  async queryEvents(options = {}) {
    try {
      const {
        filter = () => true,
        limit = Infinity,
        offset = 0,
        reverse = false
      } = options;

      const events = [];
      let count = 0;
      let skipped = 0;

      // Read entire file first if reverse order needed
      const allEvents = [];

      await this._readEvents((event) => {
        if (reverse) {
          allEvents.push(event);
          return true; // Continue reading
        } else {
          // Apply filter
          if (filter(event)) {
            if (skipped < offset) {
              skipped++;
            } else if (count < limit) {
              events.push(event);
              count++;
            } else {
              return false; // Stop reading
            }
          }
          return true; // Continue reading
        }
      });

      // Handle reverse order
      if (reverse) {
        allEvents.reverse();
        for (const event of allEvents) {
          if (filter(event)) {
            if (skipped < offset) {
              skipped++;
            } else if (count < limit) {
              events.push(event);
              count++;
            } else {
              break;
            }
          }
        }
      }

      return {
        success: true,
        events,
        count: events.length,
        total: this.eventCount
      };
    } catch (error) {
      return {
        success: false,
        error: `Query error: ${error.message}`,
        events: []
      };
    }
  }

  /**
   * Create checkpoint at current position
   */
  async checkpoint() {
    try {
      this.lastCheckpoint = this.eventCount;

      return {
        success: true,
        checkpoint: this.lastCheckpoint,
        eventCount: this.eventCount
      };
    } catch (error) {
      return {
        success: false,
        error: `Checkpoint error: ${error.message}`
      };
    }
  }

  /**
   * Replay events from checkpoint
   *
   * @param {Object} options - Replay options
   * @param {number} options.fromCheckpoint - Start from this checkpoint (event count)
   * @param {Function} options.handler - Handler function for each event
   */
  async replayFrom(options = {}) {
    try {
      const {
        fromCheckpoint = 0,
        handler = (event) => event
      } = options;

      const replayedEvents = [];
      let count = 0;

      await this._readEvents((event) => {
        if (count >= fromCheckpoint) {
          const result = handler(event);
          replayedEvents.push(result || event);
        }
        count++;
        return true; // Continue reading
      });

      return {
        success: true,
        replayedCount: replayedEvents.length,
        totalCount: count,
        events: replayedEvents
      };
    } catch (error) {
      return {
        success: false,
        error: `Replay error: ${error.message}`,
        events: []
      };
    }
  }

  /**
   * Internal: Read events from JSONL file
   */
  async _readEvents(handler) {
    return new Promise((resolve, reject) => {
      try {
        const stream = createReadStream(this.logPath, { encoding: 'utf8' });
        const rl = createInterface({
          input: stream,
          crlfDelay: Infinity
        });

        let lineNumber = 0;

        rl.on('line', (line) => {
          lineNumber++;

          if (!line.trim()) {
            return; // Skip empty lines
          }

          try {
            const event = JSON.parse(line);
            const shouldContinue = handler(event);

            if (shouldContinue === false) {
              rl.close();
              stream.close();
            }
          } catch (parseError) {
            console.error(`Parse error on line ${lineNumber}: ${parseError.message}`);
            // Continue reading despite parse errors
          }
        });

        rl.on('close', () => {
          resolve();
        });

        rl.on('error', (error) => {
          reject(error);
        });

        stream.on('error', (error) => {
          if (error.code === 'ENOENT') {
            resolve(); // File doesn't exist yet, no events to read
          } else {
            reject(error);
          }
        });
      } catch (error) {
        reject(error);
      }
    });
  }

  /**
   * Internal: Count events in log file
   */
  async _countEvents() {
    let count = 0;

    await this._readEvents(() => {
      count++;
      return true;
    });

    return count;
  }

  /**
   * Stop the actor and close write stream gracefully
   */
  async stop() {
    return new Promise((resolve) => {
      if (this.writeStream) {
        this.writeStream.end(() => {
          this.isInitialized = false;
          resolve({ success: true });
        });
      } else {
        resolve({ success: true });
      }
    });
  }

  /**
   * Get actor status
   */
  getStatus() {
    return {
      isRunning: this.isInitialized,
      eventCount: this.eventCount,
      logPath: this.logPath
    };
  }
}

/**
 * Factory function to create EventLogActor instance
 */
export function createEventLogActor(config) {
  return new EventLogActor(config);
}

export default EventLogActor;
