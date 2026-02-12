#!/usr/bin/env bun
/**
 * Benchmark: Message Creation Overhead
 *
 * Measures the performance of createMessage() and message generation utilities.
 * Target: P95 < 1µs for message creation (negligible overhead)
 */

import { bench, run } from 'mitata';
import {
  createMessage,
  address,
  generateMessageId,
  generateCorrelationId,
  createResponse,
  createErrorResponse,
  type Message,
} from '../message.ts';

// Sample message for response creation
const sampleMessage: Message = {
  id: 'msg_test_123',
  pattern: 'ask',
  to: address('target'),
  from: address('sender'),
  type: 'test',
  payload: { data: 'test' },
  correlationId: 'corr_test_123',
  timestamp: Date.now(),
};

bench('createMessage (tell pattern)', () => {
  createMessage(
    address('target'),
    'test-message',
    { data: 'Hello, World!' },
    { pattern: 'tell' }
  );
});

bench('createMessage (ask pattern)', () => {
  createMessage(
    address('target'),
    'test-message',
    { data: 'Hello, World!' },
    {
      pattern: 'ask',
      from: address('sender'),
      correlationId: generateCorrelationId(),
    }
  );
});

bench('createMessage with metadata', () => {
  createMessage(
    address('target'),
    'test-message',
    { data: 'Hello, World!' },
    {
      pattern: 'tell',
      metadata: { userId: 'user123', sessionId: 'session456' },
    }
  );
});

bench('address() creation', () => {
  address('some-node-id');
});

bench('generateMessageId()', () => {
  generateMessageId();
});

bench('generateCorrelationId()', () => {
  generateCorrelationId();
});

bench('createResponse()', () => {
  createResponse(sampleMessage, { result: 'success' }, true);
});

bench('createErrorResponse()', () => {
  createErrorResponse(sampleMessage, 'Something went wrong');
});

// Memory allocation test: Create many messages
bench('batch: 100 messages', () => {
  const messages = [];
  for (let i = 0; i < 100; i++) {
    messages.push(
      createMessage(
        address(`target-${i}`),
        'batch-test',
        { index: i },
        { pattern: 'tell' }
      )
    );
  }
});

// Run benchmarks if this is the main module
if (import.meta.main) {
  console.log('📊 Message Creation Benchmarks\n');
  console.log('Target: Message creation overhead < 1µs (P95)\n');
  await run();
}
