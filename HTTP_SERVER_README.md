# HTTPServerActor

A Bun-based HTTP REST API server for the event system, providing programmatic access to events, functions, and patterns.

## Implementation

**File**: `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/http-server.js`

## Features

- **Built on Bun's native HTTP server** - Fast, lightweight, no external dependencies
- **RESTful API** - Standard HTTP methods and JSON payloads
- **CORS enabled** - Browser-friendly with wildcard origin support
- **Proper status codes** - 200, 201, 400, 404, 503 as appropriate
- **Actor integration** - Connects to EventLogActor, FunctionRegistryActor, and PatternMatcherActor
- **Query parameters** - Filter, limit, offset, and type filtering support
- **Graceful shutdown** - Clean server stop with resource cleanup

## API Endpoints

### POST /events
Emit a new event to the event log.

**Request Body**:
```json
{
  "type": "user.login",
  "data": {
    "userId": "123",
    "username": "alice"
  },
  "metadata": {
    "source": "web-app"
  }
}
```

**Response** (201 Created):
```json
{
  "success": true,
  "eventId": "evt_01JH2K3M4N5P6Q7R8S9T0V",
  "eventCount": 42
}
```

**Errors**:
- `400` - Invalid JSON or missing required fields
- `503` - Event log not available

### GET /events
Query events with optional filters.

**Query Parameters**:
- `type` - Filter by event type (e.g., `?type=user.login`)
- `limit` - Max number of events to return (default: 100)
- `offset` - Skip first N events (default: 0)
- `reverse` - Read in reverse order (e.g., `?reverse=true`)

**Example**:
```bash
curl "http://localhost:3000/events?type=user.login&limit=10"
```

**Response** (200 OK):
```json
{
  "success": true,
  "events": [
    {
      "id": "evt_01JH2K3M4N5P6Q7R8S9T0V",
      "timestamp": "2026-01-10T14:10:15.123Z",
      "type": "user.login",
      "data": { "userId": "123" },
      "metadata": { "source": "web-app" }
    }
  ],
  "count": 1,
  "total": 42
}
```

**Errors**:
- `503` - Event log not available

### GET /functions
List registered functions.

**Query Parameters**:
- `type` - Filter by function type: `code` or `agent`

**Example**:
```bash
curl "http://localhost:3000/functions?type=code"
```

**Response** (200 OK):
```json
{
  "success": true,
  "functions": [
    {
      "functionId": "send-email",
      "type": "code",
      "path": "/functions/send-email.js",
      "metadata": {
        "name": "Send Email",
        "description": "Sends an email notification"
      }
    }
  ],
  "count": 1
}
```

**Errors**:
- `503` - Function registry not available

### GET /patterns
List registered event patterns.

**Query Parameters**:
- `sortByPriority` - Sort by priority descending (e.g., `?sortByPriority=true`)

**Example**:
```bash
curl "http://localhost:3000/patterns?sortByPriority=true"
```

**Response** (200 OK):
```json
{
  "success": true,
  "patterns": [
    {
      "id": "user-login",
      "predicate": "event.type === 'user.login'",
      "priority": 10,
      "metadata": {
        "description": "Matches user login events"
      }
    }
  ],
  "count": 1
}
```

**Errors**:
- `503` - Pattern matcher not available

### GET /health
Health check endpoint for monitoring.

**Response** (200 OK):
```json
{
  "status": "ok",
  "uptime": 42000,
  "startTime": "2026-01-10T14:10:15.123Z",
  "actors": {
    "eventLog": "available",
    "functionRegistry": "available",
    "patternMatcher": "available"
  }
}
```

## Configuration

Configure the HTTP server via `config.json`:

```json
{
  "http": {
    "port": 3000,
    "host": "localhost"
  }
}
```

**Defaults**:
- `port`: 3000
- `host`: "localhost"

## Usage

### Standalone Demo

Run the demo server with sample data:

```bash
./demo-http-server.js
```

This starts a server with:
- 2 sample patterns (user.login, user.logout)
- 2 sample functions (send-email, notify-slack)
- Event log ready to accept events

### Programmatic Usage

```javascript
import { HTTPServerActor } from './src/actors/http-server.js';
import { EventLogActor } from './src/actors/event-log.js';
import FunctionRegistryActor from './src/actors/function-registry.js';
import { createPatternMatcher } from './src/actors/pattern-matcher.js';

// Create actors
const eventLog = new EventLogActor({ eventLog: { file: 'events.jsonl' } });
const functionRegistry = new FunctionRegistryActor();
const patternMatcher = createPatternMatcher();

await eventLog.initialize();

// Create HTTP server
const httpServer = new HTTPServerActor({
  http: { port: 3000, host: 'localhost' }
});

// Connect actors
httpServer.setActors({ eventLog, functionRegistry, patternMatcher });

// Start server
const result = await httpServer.initialize();
console.log(`Server running at ${result.url}`);

// Graceful shutdown
process.on('SIGINT', async () => {
  await httpServer.close();
  await eventLog.close();
});
```

## Testing

Run the test suite:

```bash
bun test src/actors/http-server.test.js
```

Tests cover:
- Server initialization and shutdown
- All API endpoints
- Query parameters and filtering
- Error handling (400, 404, 503)
- CORS headers
- JSON parsing
- Actor integration

## Implementation Details

### HTTP Method Routing

Requests are routed based on method + path:

```javascript
if (method === 'POST' && path === '/events') {
  return await this.handlePostEvent(request, corsHeaders);
}
if (method === 'GET' && path === '/events') {
  return await this.handleGetEvents(request, corsHeaders);
}
// ... etc
```

### JSON Body Parsing

Uses Bun's native `request.json()`:

```javascript
const body = await request.json();
```

Handles invalid JSON with 400 error:

```javascript
catch (error) {
  if (error.name === 'SyntaxError') {
    return this.jsonResponse(
      { error: 'Invalid JSON in request body' },
      400,
      corsHeaders
    );
  }
}
```

### CORS Headers

All responses include CORS headers for browser compatibility:

```javascript
const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type',
};
```

Preflight OPTIONS requests return 204:

```javascript
if (method === 'OPTIONS') {
  return new Response(null, {
    status: 204,
    headers: corsHeaders
  });
}
```

### Status Codes

- `200` - Successful GET request
- `201` - Event created successfully
- `204` - OPTIONS preflight success (no content)
- `400` - Bad request (invalid JSON, missing fields)
- `404` - Route not found
- `500` - Internal server error
- `503` - Service unavailable (actor not connected)

### Actor Integration

The HTTP server acts as a facade over the other actors:

```javascript
// POST /events delegates to EventLogActor
const result = await this.eventLog.handleMessage(
  createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, body)
);

// GET /functions delegates to FunctionRegistryActor
const result = this.functionRegistry.listFunctions(filters);

// GET /patterns delegates to PatternMatcherActor
const patterns = this.patternMatcher.listPatterns(options);
```

## Performance Characteristics

- **Bun's HTTP server** - High-performance native implementation
- **Zero-copy JSON** - Efficient parsing and serialization
- **Non-blocking I/O** - Async/await for all operations
- **Graceful shutdown** - Clean resource cleanup

## Next Steps

Potential enhancements:

1. **Authentication** - Add API key or JWT support
2. **Rate limiting** - Prevent abuse
3. **WebSocket support** - Real-time event streaming
4. **Pagination** - Cursor-based pagination for large result sets
5. **OpenAPI spec** - Auto-generated API documentation
6. **Metrics** - Request count, latency tracking
7. **Request validation** - JSON schema validation

## Architecture Notes

The HTTPServerActor follows the actor model:

- **Stateless request handling** - No request state stored
- **Message passing** - Communicates with other actors via UAP messages
- **Isolation** - Does not directly access other actors' internal state
- **Supervision** - Can be restarted independently without affecting event log

This makes the HTTP interface a clean, replaceable component that can be swapped for gRPC, GraphQL, or other protocols without changing the core event system.
