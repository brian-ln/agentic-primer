/**
 * EventLogActor Tests
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { EventLogActor } from '../src/actors/event-log.js';
import { PROTOCOLS, ACTIONS, createMessage } from '../src/protocol.js';
import { promises as fs } from 'fs';
import { join } from 'path';

const TEST_LOG_FILE = 'test-events.jsonl';

describe('EventLogActor', () => {
  let actor;

  beforeEach(async () => {
    // Clean up test file
    try {
      await fs.unlink(TEST_LOG_FILE);
    } catch (err) {
      // Ignore if file doesn't exist
    }

    actor = new EventLogActor({
      eventLog: {
        file: TEST_LOG_FILE,
        checkpointInterval: 5
      }
    });

    await actor.initialize();
  });

  afterEach(async () => {
    await actor.close();

    // Clean up test file
    try {
      await fs.unlink(TEST_LOG_FILE);
    } catch (err) {
      // Ignore
    }
  });

  test('should initialize successfully', async () => {
    expect(actor.isInitialized).toBe(true);
    expect(actor.eventCount).toBe(0);
  });

  test('should append event', async () => {
    const message = createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      type: 'test.event',
      data: { message: 'Hello, world!' }
    });

    const result = await actor.handleMessage(message);

    expect(result.success).toBe(true);
    expect(result.eventId).toBeDefined();
    expect(result.eventCount).toBe(1);
  });

  test('should append multiple events', async () => {
    for (let i = 0; i < 3; i++) {
      const message = createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
        type: 'test.event',
        data: { count: i }
      });

      await actor.handleMessage(message);
    }

    expect(actor.eventCount).toBe(3);
  });

  test('should query all events', async () => {
    // Add some events
    for (let i = 0; i < 5; i++) {
      const message = createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
        type: 'test.event',
        data: { count: i }
      });

      await actor.handleMessage(message);
    }

    // Query all
    const queryMessage = createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, {});
    const result = await actor.handleMessage(queryMessage);

    expect(result.success).toBe(true);
    expect(result.events.length).toBe(5);
    expect(result.total).toBe(5);
  });

  test('should query with filter', async () => {
    // Add events
    await actor.handleMessage(createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      type: 'user.login',
      data: { userId: '123' }
    }));

    await actor.handleMessage(createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      type: 'user.logout',
      data: { userId: '123' }
    }));

    await actor.handleMessage(createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      type: 'user.login',
      data: { userId: '456' }
    }));

    // Query with filter
    const queryMessage = createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, {
      filter: (event) => event.type === 'user.login'
    });

    const result = await actor.handleMessage(queryMessage);

    expect(result.success).toBe(true);
    expect(result.events.length).toBe(2);
    expect(result.events[0].type).toBe('user.login');
    expect(result.events[1].type).toBe('user.login');
  });

  test('should query with limit', async () => {
    // Add events
    for (let i = 0; i < 10; i++) {
      await actor.handleMessage(createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
        type: 'test.event',
        data: { count: i }
      }));
    }

    // Query with limit
    const queryMessage = createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, {
      limit: 3
    });

    const result = await actor.handleMessage(queryMessage);

    expect(result.success).toBe(true);
    expect(result.events.length).toBe(3);
    expect(result.total).toBe(10);
  });

  test('should query with offset', async () => {
    // Add events
    for (let i = 0; i < 5; i++) {
      await actor.handleMessage(createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
        type: 'test.event',
        data: { count: i }
      }));
    }

    // Query with offset
    const queryMessage = createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, {
      offset: 2,
      limit: 2
    });

    const result = await actor.handleMessage(queryMessage);

    expect(result.success).toBe(true);
    expect(result.events.length).toBe(2);
    expect(result.events[0].data.count).toBe(2);
    expect(result.events[1].data.count).toBe(3);
  });

  test('should create checkpoint', async () => {
    // Add events
    for (let i = 0; i < 3; i++) {
      await actor.handleMessage(createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
        type: 'test.event',
        data: { count: i }
      }));
    }

    // Create checkpoint
    const checkpointMessage = createMessage(PROTOCOLS.EVENT, ACTIONS.CHECKPOINT, {});
    const result = await actor.handleMessage(checkpointMessage);

    expect(result.success).toBe(true);
    expect(result.checkpoint).toBe(3);
    expect(result.eventCount).toBe(3);
  });

  test('should replay from checkpoint', async () => {
    // Add events
    for (let i = 0; i < 5; i++) {
      await actor.handleMessage(createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
        type: 'test.event',
        data: { count: i }
      }));
    }

    // Replay from checkpoint 2
    const replayMessage = createMessage(PROTOCOLS.EVENT, 'replay', {
      fromCheckpoint: 2,
      handler: (event) => event
    });

    const result = await actor.handleMessage(replayMessage);

    expect(result.success).toBe(true);
    expect(result.replayedCount).toBe(3); // Events 2, 3, 4
    expect(result.totalCount).toBe(5);
  });

  test('should handle file I/O errors gracefully', async () => {
    // Try to append with invalid event data
    const message = createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      // Missing required 'type' field
      data: { message: 'This should fail' }
    });

    const result = await actor.handleMessage(message);

    expect(result.success).toBe(false);
    expect(result.error).toContain('Event type is required');
  });

  test('should reject invalid protocol', async () => {
    const message = createMessage('invalid.protocol', ACTIONS.APPEND, {
      type: 'test.event'
    });

    const result = await actor.handleMessage(message);

    expect(result.success).toBe(false);
    expect(result.error).toContain('Invalid protocol');
  });

  test('should reject invalid action', async () => {
    const message = createMessage(PROTOCOLS.EVENT, 'invalid_action', {
      type: 'test.event'
    });

    const result = await actor.handleMessage(message);

    expect(result.success).toBe(false);
    expect(result.error).toContain('Unknown action');
  });

  test('should generate ULID for events', async () => {
    const message = createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      type: 'test.event',
      data: {}
    });

    const result = await actor.handleMessage(message);

    expect(result.success).toBe(true);
    expect(result.eventId).toMatch(/^evt_[0-9A-Z]{26}$/);
  });

  test('should preserve event metadata', async () => {
    const message = createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      type: 'test.event',
      data: { msg: 'test' },
      metadata: {
        source: 'cli',
        depth: 5,
        triggeredBy: 'evt_123'
      }
    });

    await actor.handleMessage(message);

    // Query to verify
    const queryMessage = createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, {});
    const result = await actor.handleMessage(queryMessage);

    expect(result.events[0].metadata.source).toBe('cli');
    expect(result.events[0].metadata.depth).toBe(5);
    expect(result.events[0].metadata.triggeredBy).toBe('evt_123');
  });

  test('should handle empty log file', async () => {
    const queryMessage = createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, {});
    const result = await actor.handleMessage(queryMessage);

    expect(result.success).toBe(true);
    expect(result.events.length).toBe(0);
    expect(result.total).toBe(0);
  });
});
