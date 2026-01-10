/**
 * HTTPServerActor Tests
 *
 * Tests for HTTP REST API server
 */

import { HTTPServerActor } from './http-server.js';
import { EventLogActor } from './event-log.js';
import { createPatternMatcher } from './pattern-matcher.js';
import FunctionRegistryActor from './function-registry.js';
import { unlinkSync } from 'fs';

const TEST_PORT = 3001; // Use different port for testing
const TEST_LOG_FILE = './test-http-events.jsonl';

console.log('Starting HTTPServerActor tests...\n');

// Test 1: Initialize HTTP server
console.log('Test 1: Initialize HTTP server');
const httpServer = new HTTPServerActor({
  http: {
    port: TEST_PORT,
    host: 'localhost'
  }
});

const initResult = await httpServer.initialize();
console.log('✓ Should initialize successfully:', initResult.success);
console.log('✓ Should be running on correct port:', initResult.port === TEST_PORT);

// Test 2: Health check endpoint
console.log('\nTest 2: Health check endpoint');
const healthResponse = await fetch(`http://localhost:${TEST_PORT}/health`);
const healthData = await healthResponse.json();
console.log('✓ Should return 200 status:', healthResponse.status === 200);
console.log('✓ Should return ok status:', healthData.status === 'ok');
console.log('✓ Should report actors as unavailable:',
  healthData.actors.eventLog === 'unavailable' &&
  healthData.actors.functionRegistry === 'unavailable' &&
  healthData.actors.patternMatcher === 'unavailable'
);

// Test 3: Set up actors
console.log('\nTest 3: Set up actors');
const eventLog = new EventLogActor({
  eventLog: {
    file: TEST_LOG_FILE
  }
});
await eventLog.initialize();

const functionRegistry = new FunctionRegistryActor();
const patternMatcher = createPatternMatcher();

httpServer.setActors({
  eventLog,
  functionRegistry,
  patternMatcher
});

const healthResponse2 = await fetch(`http://localhost:${TEST_PORT}/health`);
const healthData2 = await healthResponse2.json();
console.log('✓ Should report actors as available:',
  healthData2.actors.eventLog === 'available' &&
  healthData2.actors.functionRegistry === 'available' &&
  healthData2.actors.patternMatcher === 'available'
);

// Test 4: POST /events - Create event
console.log('\nTest 4: POST /events - Create event');
const postResponse = await fetch(`http://localhost:${TEST_PORT}/events`, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({
    type: 'test.event',
    data: { message: 'Hello from HTTP' },
    metadata: { source: 'http-test' }
  })
});

const postData = await postResponse.json();
console.log('✓ Should return 201 status:', postResponse.status === 201);
console.log('✓ Should return success:', postData.success === true);
console.log('✓ Should return event ID:', postData.eventId?.startsWith('evt_'));
console.log('✓ Should return event count:', typeof postData.eventCount === 'number');

// Test 5: POST /events - Invalid JSON
console.log('\nTest 5: POST /events - Invalid JSON');
const invalidJsonResponse = await fetch(`http://localhost:${TEST_PORT}/events`, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: 'not valid json'
});

console.log('✓ Should return 400 status:', invalidJsonResponse.status === 400);

// Test 6: POST /events - Missing type
console.log('\nTest 6: POST /events - Missing type');
const missingTypeResponse = await fetch(`http://localhost:${TEST_PORT}/events`, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({
    data: { message: 'Missing type field' }
  })
});

const missingTypeData = await missingTypeResponse.json();
console.log('✓ Should return 400 status:', missingTypeResponse.status === 400);
console.log('✓ Should return error message:', missingTypeData.error === 'Event type is required');

// Test 7: GET /events - Query all events
console.log('\nTest 7: GET /events - Query all events');
const getEventsResponse = await fetch(`http://localhost:${TEST_PORT}/events`);
const getEventsData = await getEventsResponse.json();

console.log('✓ Should return 200 status:', getEventsResponse.status === 200);
console.log('✓ Should return success:', getEventsData.success === true);
console.log('✓ Should return events array:', Array.isArray(getEventsData.events));
console.log('✓ Should have at least 1 event:', getEventsData.count >= 1);

// Test 8: GET /events - Query with type filter
console.log('\nTest 8: GET /events - Query with type filter');
const getFilteredResponse = await fetch(`http://localhost:${TEST_PORT}/events?type=test.event`);
const getFilteredData = await getFilteredResponse.json();

console.log('✓ Should return 200 status:', getFilteredResponse.status === 200);
console.log('✓ Should filter by type:',
  getFilteredData.events.every(e => e.type === 'test.event')
);

// Test 9: GET /events - Query with limit
console.log('\nTest 9: GET /events - Query with limit');
const getLimitedResponse = await fetch(`http://localhost:${TEST_PORT}/events?limit=1`);
const getLimitedData = await getLimitedResponse.json();

console.log('✓ Should return 200 status:', getLimitedResponse.status === 200);
console.log('✓ Should respect limit:', getLimitedData.count <= 1);

// Test 10: GET /functions - Empty registry
console.log('\nTest 10: GET /functions - Empty registry');
const getFunctionsResponse = await fetch(`http://localhost:${TEST_PORT}/functions`);
const getFunctionsData = await getFunctionsResponse.json();

console.log('✓ Should return 200 status:', getFunctionsResponse.status === 200);
console.log('✓ Should return success:', getFunctionsData.success === true);
console.log('✓ Should return empty array:', getFunctionsData.count === 0);

// Test 11: GET /functions - With registered functions
console.log('\nTest 11: GET /functions - With registered functions');
functionRegistry.registerFunction('test-function', {
  type: 'code',
  path: '/path/to/test.js',
  metadata: { name: 'Test Function' }
});

const getFunctionsResponse2 = await fetch(`http://localhost:${TEST_PORT}/functions`);
const getFunctionsData2 = await getFunctionsResponse2.json();

console.log('✓ Should return 200 status:', getFunctionsResponse2.status === 200);
console.log('✓ Should have 1 function:', getFunctionsData2.count === 1);
console.log('✓ Should include test-function:',
  getFunctionsData2.functions.some(f => f.functionId === 'test-function')
);

// Test 12: GET /functions - Filter by type
console.log('\nTest 12: GET /functions - Filter by type');
functionRegistry.registerFunction('test-agent', {
  type: 'agent',
  agentCommand: 'claude',
  metadata: { name: 'Test Agent' }
});

const getCodeFunctionsResponse = await fetch(`http://localhost:${TEST_PORT}/functions?type=code`);
const getCodeFunctionsData = await getCodeFunctionsResponse.json();

console.log('✓ Should return 200 status:', getCodeFunctionsResponse.status === 200);
console.log('✓ Should filter by type:',
  getCodeFunctionsData.functions.every(f => f.type === 'code')
);

// Test 13: GET /patterns - Empty patterns
console.log('\nTest 13: GET /patterns - Empty patterns');
const getPatternsResponse = await fetch(`http://localhost:${TEST_PORT}/patterns`);
const getPatternsData = await getPatternsResponse.json();

console.log('✓ Should return 200 status:', getPatternsResponse.status === 200);
console.log('✓ Should return success:', getPatternsData.success === true);
console.log('✓ Should return empty array:', getPatternsData.count === 0);

// Test 14: GET /patterns - With registered patterns
console.log('\nTest 14: GET /patterns - With registered patterns');
patternMatcher.registerPattern({
  id: 'test-pattern',
  predicate: 'event.type === "test.event"',
  priority: 10,
  metadata: { name: 'Test Pattern' }
});

const getPatternsResponse2 = await fetch(`http://localhost:${TEST_PORT}/patterns`);
const getPatternsData2 = await getPatternsResponse2.json();

console.log('✓ Should return 200 status:', getPatternsResponse2.status === 200);
console.log('✓ Should have 1 pattern:', getPatternsData2.count === 1);
console.log('✓ Should include test-pattern:',
  getPatternsData2.patterns.some(p => p.id === 'test-pattern')
);

// Test 15: GET /patterns - Sort by priority
console.log('\nTest 15: GET /patterns - Sort by priority');
patternMatcher.registerPattern({
  id: 'high-priority',
  predicate: 'true',
  priority: 100
});

const getSortedPatternsResponse = await fetch(
  `http://localhost:${TEST_PORT}/patterns?sortByPriority=true`
);
const getSortedPatternsData = await getSortedPatternsResponse.json();

console.log('✓ Should return 200 status:', getSortedPatternsResponse.status === 200);
console.log('✓ Should sort by priority:',
  getSortedPatternsData.patterns[0].id === 'high-priority'
);

// Test 16: 404 for unknown routes
console.log('\nTest 16: 404 for unknown routes');
const notFoundResponse = await fetch(`http://localhost:${TEST_PORT}/unknown`);
console.log('✓ Should return 404 status:', notFoundResponse.status === 404);

// Test 17: CORS headers
console.log('\nTest 17: CORS headers');
const corsResponse = await fetch(`http://localhost:${TEST_PORT}/health`);
const corsHeaders = corsResponse.headers;

console.log('✓ Should have CORS headers:',
  corsHeaders.get('Access-Control-Allow-Origin') === '*'
);

// Test 18: OPTIONS request (preflight)
console.log('\nTest 18: OPTIONS request (preflight)');
const optionsResponse = await fetch(`http://localhost:${TEST_PORT}/events`, {
  method: 'OPTIONS'
});

console.log('✓ Should return 204 status:', optionsResponse.status === 204);
console.log('✓ Should have CORS headers:',
  optionsResponse.headers.get('Access-Control-Allow-Methods')?.includes('POST')
);

// Test 19: Get server status
console.log('\nTest 19: Get server status');
const status = httpServer.getStatus();

console.log('✓ Should report running:', status.isRunning === true);
console.log('✓ Should report correct port:', status.port === TEST_PORT);
console.log('✓ Should have URL:', status.url === `http://localhost:${TEST_PORT}`);
console.log('✓ Should have start time:', typeof status.startTime === 'string');

// Cleanup
console.log('\nCleaning up...');
await httpServer.close();
await eventLog.close();

// Clean up test file
try {
  unlinkSync(TEST_LOG_FILE);
} catch (err) {
  // Ignore if file doesn't exist
}

console.log('\n✓ All HTTP server tests completed successfully!');
