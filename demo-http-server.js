#!/usr/bin/env bun
/**
 * HTTP Server Demo
 *
 * Demonstrates the HTTPServerActor with all actors integrated
 */

import { HTTPServerActor } from './src/actors/http-server.js';
import { EventLogActor } from './src/actors/event-log.js';
import { createPatternMatcher } from './src/actors/pattern-matcher.js';
import FunctionRegistryActor from './src/actors/function-registry.js';

console.log('Starting HTTP Server Demo...\n');

// Create actors
const eventLog = new EventLogActor({
  eventLog: {
    file: 'demo-events.jsonl'
  }
});

const functionRegistry = new FunctionRegistryActor();
const patternMatcher = createPatternMatcher();

// Initialize event log
await eventLog.initialize();
console.log('✓ Event log initialized');

// Register some sample patterns
patternMatcher.registerPattern({
  id: 'user-login',
  predicate: 'event.type === "user.login"',
  priority: 10,
  metadata: { description: 'Matches user login events' }
});

patternMatcher.registerPattern({
  id: 'user-logout',
  predicate: 'event.type === "user.logout"',
  priority: 10,
  metadata: { description: 'Matches user logout events' }
});

console.log('✓ Registered 2 sample patterns');

// Register some sample functions
functionRegistry.registerFunction('send-email', {
  type: 'code',
  path: '/functions/send-email.js',
  metadata: {
    name: 'Send Email',
    description: 'Sends an email notification'
  }
});

functionRegistry.registerFunction('notify-slack', {
  type: 'agent',
  agentCommand: 'claude',
  metadata: {
    name: 'Notify Slack',
    description: 'Posts a message to Slack'
  }
});

console.log('✓ Registered 2 sample functions');

// Create and start HTTP server
const httpServer = new HTTPServerActor({
  http: {
    port: 3000,
    host: 'localhost'
  }
});

httpServer.setActors({
  eventLog,
  functionRegistry,
  patternMatcher
});

const result = await httpServer.initialize();

if (result.success) {
  console.log(`\n✓ HTTP server running at ${result.url}\n`);
  console.log('Available endpoints:');
  console.log('  POST   /events       - Emit a new event');
  console.log('  GET    /events       - Query events (supports ?type=, ?limit=, ?offset=)');
  console.log('  GET    /functions    - List registered functions (supports ?type=)');
  console.log('  GET    /patterns     - List registered patterns (supports ?sortByPriority=)');
  console.log('  GET    /health       - Health check');
  console.log('\nTry it out:');
  console.log('  curl http://localhost:3000/health');
  console.log('  curl http://localhost:3000/patterns');
  console.log('  curl http://localhost:3000/functions');
  console.log('  curl -X POST http://localhost:3000/events -H "Content-Type: application/json" -d \'{"type":"user.login","data":{"userId":"123"}}\'');
  console.log('  curl http://localhost:3000/events');
  console.log('\nPress CTRL+C to stop the server.');
} else {
  console.error('Failed to start server:', result.error);
  process.exit(1);
}

// Handle shutdown
process.on('SIGINT', async () => {
  console.log('\n\nShutting down...');
  await httpServer.close();
  await eventLog.close();
  console.log('✓ Server stopped');
  process.exit(0);
});
