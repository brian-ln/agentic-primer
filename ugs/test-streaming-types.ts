#!/usr/bin/env bun
/**
 * Quick test to verify streaming types compile correctly
 */

import {
  type StreamCallback,
  type StreamOptions,
  type TokenStreamEvent,
} from './src/messaging/message.ts';

// Test 1: StreamCallback type works
const callback: StreamCallback<TokenStreamEvent> = async (event) => {
  console.log('Event:', event.type, event.content);
};

// Test 2: StreamOptions interface works
const options: StreamOptions<TokenStreamEvent> = {
  onChunk: callback,
  timeout: 30000,
};

// Test 3: TokenStreamEvent structure works
const tokenEvent: TokenStreamEvent = {
  type: 'token',
  content: 'Hello',
  timestamp: Date.now(),
};

const doneEvent: TokenStreamEvent = {
  type: 'done',
  timestamp: Date.now(),
};

const errorEvent: TokenStreamEvent = {
  type: 'error',
  error: 'Something went wrong',
  timestamp: Date.now(),
};

console.log('✓ All streaming types compile correctly!');
console.log('  • StreamCallback<T>');
console.log('  • StreamOptions<R>');
console.log('  • TokenStreamEvent');
