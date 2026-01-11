# HTTPServerActor - FIT Decision Tables

FIT-style decision tables for testing HTTPServerActor behavior declaratively.

## Table 1: Lifecycle State Transitions

Tests the core lifecycle methods: start(), stop(), getStatus()

| Initial State | Action | Expected State | Expected Success | Expected Error/Message | Port Available |
|--------------|--------|----------------|------------------|----------------------|----------------|
| stopped | start() | running | true | url returned | yes |
| stopped | stop() | stopped | true | - | - |
| stopped | getStatus() | stopped | - | isRunning=false | - |
| running | start() | running | false | already running | - |
| running | stop() | stopped | true | - | - |
| running | getStatus() | running | - | isRunning=true, url set | - |
| stopped | start() with port 0 | running | true | random port assigned | yes |
| stopped | start() with used port | error | false | port in use | no |

## Table 2: Endpoint Routing - Method and Path Matching

Tests HTTP request routing to correct handlers based on method and path.

| HTTP Method | Path | Expected Handler | Expected Status |
|------------|------|-----------------|-----------------|
| POST | /events | handlePostEvent | 201 or 400 |
| GET | /events | handleGetEvents | 200 |
| GET | /functions | handleGetFunctions | 200 or 503 |
| GET | /patterns | handleGetPatterns | 200 or 503 |
| GET | /health | handleGetHealth | 200 |
| OPTIONS | /events | CORS preflight | 204 |
| OPTIONS | /functions | CORS preflight | 204 |
| GET | /unknown | Not Found | 404 |
| POST | /unknown | Not Found | 404 |
| DELETE | /events | Not Found | 404 |

## Table 3: POST /events - Request Validation

Tests request body validation for event posting.

| Request Body | Content-Type | Expected Status | Expected Response Contains |
|-------------|--------------|-----------------|---------------------------|
| {"type": "user.created", "data": {}} | application/json | 201 | success: true, eventId |
| {"type": "order.placed"} | application/json | 201 | success: true |
| {"data": {}} | application/json | 400 | Event type is required |
| {} | application/json | 400 | Event type is required |
| null | application/json | 400 | must be a JSON object |
| "string" | application/json | 400 | must be a JSON object |
| invalid json | application/json | 400 | Invalid JSON |
| {"type": "test"} | text/plain | 400 | Invalid JSON |

## Table 4: POST /events - EventLog Integration

Tests integration with EventLogActor for event posting.

| EventLog Available | Request Valid | EventLog Response | Expected Status | Response Contains |
|-------------------|---------------|-------------------|-----------------|-------------------|
| yes | yes | success | 201 | eventId, eventCount |
| yes | yes | error | 400 | error message |
| no | yes | - | 503 | Event log not available |
| yes | no | - | 400 | validation error |

## Table 5: GET /events - Query Parameters

Tests event querying with URL query parameters.

| Query String | Expected EventLog Query | Expected Status |
|-------------|------------------------|-----------------|
| (empty) | {limit: 100, offset: 0, reverse: false} | 200 |
| ?limit=10 | {limit: 10, offset: 0, reverse: false} | 200 |
| ?limit=5&offset=10 | {limit: 5, offset: 10, reverse: false} | 200 |
| ?reverse=true | {limit: 100, offset: 0, reverse: true} | 200 |
| ?type=user.created | filter by type user.created | 200 |
| ?limit=abc | {limit: NaN, ...} | 200 (handled) |

## Table 6: GET /events - EventLog Integration

Tests integration with EventLogActor for event querying.

| EventLog Available | EventLog Returns | Expected Status | Response Contains |
|-------------------|------------------|-----------------|-------------------|
| yes | success with events | 200 | events array, count, total |
| yes | success empty | 200 | events: [], count: 0 |
| yes | error | 500 | error message |
| no | - | 503 | Event log not available |

## Table 7: GET /functions - FunctionRegistry Integration

Tests integration with FunctionRegistryActor.

| FunctionRegistry Available | Query Params | Expected Status | Response Contains |
|---------------------------|--------------|-----------------|-------------------|
| yes | (empty) | 200 | functions array, count |
| yes | ?type=code | 200 | filtered functions |
| yes | ?type=agent | 200 | filtered functions |
| no | (any) | 503 | Function registry not available |

## Table 8: GET /patterns - PatternMatcher Integration

Tests integration with PatternMatcherActor.

| PatternMatcher Available | Query Params | Expected Status | Response Contains |
|-------------------------|--------------|-----------------|-------------------|
| yes | (empty) | 200 | patterns array, count |
| yes | ?sortByPriority=true | 200 | sorted patterns |
| yes | ?sortByPriority=false | 200 | unsorted patterns |
| no | (any) | 503 | Pattern matcher not available |

## Table 9: GET /health - Health Check

Tests health check endpoint with various actor availability states.

| EventLog | FunctionRegistry | PatternMatcher | Expected Status | Response Status | Uptime Present |
|----------|------------------|----------------|-----------------|-----------------|----------------|
| available | available | available | 200 | ok | yes |
| available | available | unavailable | 200 | ok | yes |
| available | unavailable | available | 200 | ok | yes |
| unavailable | unavailable | unavailable | 200 | ok | yes |
| available | available | available | 200 | ok | yes |

## Table 10: CORS Headers

Tests CORS header presence in responses.

| Request Method | Path | Expected CORS Headers Present |
|---------------|------|------------------------------|
| OPTIONS | /events | Access-Control-Allow-Origin, Methods, Headers |
| POST | /events | Access-Control-Allow-Origin |
| GET | /events | Access-Control-Allow-Origin |
| GET | /health | Access-Control-Allow-Origin |
| GET | /unknown | Access-Control-Allow-Origin |

## Table 11: Error Handling - Server Errors

Tests error handling for various server error conditions.

| Scenario | Expected Behavior | Expected Status |
|----------|------------------|-----------------|
| Handler throws exception | 500 Internal Server Error | 500 |
| Invalid JSON response | 500 Internal Server Error | 500 |
| Actor method throws | Error caught, returned to client | 400/500 |
| Network error during request | Handled by Bun | varies |

## Table 12: Response Format

Tests JSON response formatting for all endpoints.

| Endpoint | Expected Content-Type | Expected Format |
|----------|----------------------|-----------------|
| POST /events | application/json | {"success": bool, ...} |
| GET /events | application/json | {"success": bool, "events": [...]} |
| GET /functions | application/json | {"success": bool, "functions": [...]} |
| GET /patterns | application/json | {"success": bool, "patterns": [...]} |
| GET /health | application/json | {"status": "ok", ...} |
| 404 response | application/json | {"error": "Not Found", ...} |

## Table 13: Actor Injection

Tests the setActors() method for dependency injection.

| EventLog Injected | FunctionRegistry Injected | PatternMatcher Injected | Expected Behavior |
|------------------|---------------------------|------------------------|-------------------|
| yes | yes | yes | All endpoints work |
| no | yes | yes | /events returns 503 |
| yes | no | yes | /functions returns 503 |
| yes | yes | no | /patterns returns 503 |
| no | no | no | Only /health works fully |

## Table 14: Port Configuration

Tests server port configuration options.

| Config Port | Expected Behavior | Port in URL |
|------------|------------------|-------------|
| 3000 | Listen on 3000 | 3000 |
| 8080 | Listen on 8080 | 8080 |
| 0 | Random available port | random |
| undefined | Default to 3000 | 3000 |

## Table 15: Concurrent Requests

Tests handling of multiple concurrent requests.

| Concurrent Requests | Request Type | Expected Behavior |
|--------------------|--------------|-------------------|
| 10 POST /events | All valid | All succeed with unique IDs |
| 10 GET /events | All identical | All return same data |
| 5 POST + 5 GET | Mixed | All handled correctly |
| 100 GET /health | Health checks | All return 200 |

## Table 16: Edge Cases

Tests boundary conditions and edge cases.

| Test Case | Expected Behavior |
|-----------|------------------|
| Very large request body (10MB) | Handle or reject gracefully |
| Request with no body | Handle based on endpoint |
| Malformed URL | 400 or 404 |
| Request timeout | Handled by Bun |
| Stop server during request | Connection closed |
| Start/stop/start cycle | Server restarts successfully |

## Implementation Notes

### Table Format
Each table represents a specific test scenario or behavior category. Tables can be executed by:
1. Setting up the actor and dependencies (Initial State)
2. Making HTTP requests (Action)
3. Verifying responses (Expected columns)

### Execution Strategy
- Tables 1-2: Core functionality - lifecycle and routing
- Tables 3-9: Endpoint behavior - individual endpoint tests
- Tables 10-12: Response handling - CORS, errors, format
- Tables 13-16: Integration and edge cases

### Success Criteria
- All lifecycle transitions work correctly
- Requests are routed to correct handlers
- Request validation works properly
- Actor integration works when available
- Graceful degradation when actors unavailable
- CORS headers present in all responses
- Error cases handled gracefully
- Edge cases handled properly
