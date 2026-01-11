Feature: HTTPServerActor - REST API server for event system
  As an API consumer
  I want to interact with the event system via HTTP
  So that I can emit events and query system state

  Background:
    Given a clean test environment
    And mock actor instances for eventLog, functionRegistry, and patternMatcher

  # Lifecycle Transitions
  Scenario: Start HTTP server successfully
    Given an HTTPServerActor with default configuration
    When I start the server
    Then the server should be running
    And the server should listen on the configured port
    And the start result should include the server URL

  Scenario: Start server with port 0 for dynamic port assignment
    Given an HTTPServerActor configured with port 0
    When I start the server
    Then the server should be running
    And the server should be assigned a random available port
    And the start result should include the actual port number

  Scenario: Prevent starting an already running server
    Given an HTTPServerActor that is running
    When I attempt to start the server again
    Then the start should fail
    And the error should indicate "Server is already running"

  Scenario: Stop HTTP server gracefully
    Given an HTTPServerActor that is running
    When I stop the server
    Then the server should not be running
    And the stop result should indicate success

  Scenario: Get status of running server
    Given an HTTPServerActor that is running
    When I get the server status
    Then the status should show isRunning as true
    And the status should include port, host, and URL
    And the status should include uptime

  # Happy Paths - POST /events
  Scenario: Successfully emit a new event via POST /events
    Given an HTTPServerActor with eventLog actor configured
    When I POST to "/events" with valid event {"type": "user.created", "data": {"userId": "123"}}
    Then the response status should be 201
    And the response should include success true
    And the response should include an eventId
    And the event should be passed to EventLogActor

  Scenario: Emit event with full metadata
    Given an HTTPServerActor with eventLog actor configured
    When I POST to "/events" with event including metadata
    Then the response status should be 201
    And the event should preserve the metadata

  # Happy Paths - GET /events
  Scenario: Query all events via GET /events
    Given an HTTPServerActor with eventLog actor configured
    And EventLogActor has 10 events
    When I GET "/events"
    Then the response status should be 200
    And the response should include success true
    And the response should include an events array
    And the response should include count and total fields

  Scenario: Query events with limit parameter
    Given an HTTPServerActor with eventLog actor configured
    When I GET "/events?limit=5"
    Then the response status should be 200
    And the EventLogActor query should receive limit 5

  Scenario: Query events with offset parameter
    Given an HTTPServerActor with eventLog actor configured
    When I GET "/events?offset=10"
    Then the response status should be 200
    And the EventLogActor query should receive offset 10

  Scenario: Query events with type filter
    Given an HTTPServerActor with eventLog actor configured
    When I GET "/events?type=user.created"
    Then the response status should be 200
    And the EventLogActor query should receive a filter for type "user.created"

  Scenario: Query events with reverse parameter
    Given an HTTPServerActor with eventLog actor configured
    When I GET "/events?reverse=true"
    Then the response status should be 200
    And the EventLogActor query should receive reverse true

  # Happy Paths - GET /functions
  Scenario: List all registered functions via GET /functions
    Given an HTTPServerActor with functionRegistry actor configured
    And FunctionRegistryActor has 5 registered functions
    When I GET "/functions"
    Then the response status should be 200
    And the response should include success true
    And the response should include functions array with 5 items
    And the response should include count field

  Scenario: Filter functions by type
    Given an HTTPServerActor with functionRegistry actor configured
    When I GET "/functions?type=code"
    Then the response status should be 200
    And the FunctionRegistryActor should receive type filter "code"

  # Happy Paths - GET /patterns
  Scenario: List all registered patterns via GET /patterns
    Given an HTTPServerActor with patternMatcher actor configured
    And PatternMatcherActor has 3 registered patterns
    When I GET "/patterns"
    Then the response status should be 200
    And the response should include success true
    And the response should include patterns array with 3 items
    And the response should include count field

  Scenario: List patterns sorted by priority
    Given an HTTPServerActor with patternMatcher actor configured
    When I GET "/patterns?sortByPriority=true"
    Then the response status should be 200
    And the PatternMatcherActor should receive sortByPriority true

  # Happy Paths - GET /health
  Scenario: Check health status
    Given an HTTPServerActor that is running
    When I GET "/health"
    Then the response status should be 200
    And the response should include status "ok"
    And the response should include uptime
    And the response should include startTime
    And the response should include actors availability status

  # Error Paths - POST /events
  Scenario: Reject POST /events with invalid JSON
    Given an HTTPServerActor with eventLog actor configured
    When I POST to "/events" with malformed JSON body
    Then the response status should be 400
    And the error should indicate "Invalid JSON in request body"

  Scenario: Reject POST /events with non-object body
    Given an HTTPServerActor with eventLog actor configured
    When I POST to "/events" with body "just a string"
    Then the response status should be 400
    And the error should indicate "Request body must be a JSON object"

  Scenario: Reject POST /events without event type
    Given an HTTPServerActor with eventLog actor configured
    When I POST to "/events" with {"data": {"userId": "123"}}
    Then the response status should be 400
    And the error should indicate "Event type is required"

  Scenario: Handle EventLogActor unavailable
    Given an HTTPServerActor without eventLog actor configured
    When I POST to "/events" with valid event
    Then the response status should be 503
    And the error should indicate "Event log not available"

  Scenario: Handle EventLogActor error during append
    Given an HTTPServerActor with eventLog actor configured
    And EventLogActor returns an error on append
    When I POST to "/events" with valid event
    Then the response status should be 400
    And the error should be from EventLogActor

  # Error Paths - GET /events
  Scenario: Handle EventLogActor unavailable on GET /events
    Given an HTTPServerActor without eventLog actor configured
    When I GET "/events"
    Then the response status should be 503
    And the error should indicate "Event log not available"

  Scenario: Handle EventLogActor error during query
    Given an HTTPServerActor with eventLog actor configured
    And EventLogActor returns an error on query
    When I GET "/events"
    Then the response status should be 500
    And the error should be from EventLogActor

  # Error Paths - GET /functions
  Scenario: Handle FunctionRegistryActor unavailable
    Given an HTTPServerActor without functionRegistry actor configured
    When I GET "/functions"
    Then the response status should be 503
    And the error should indicate "Function registry not available"

  # Error Paths - GET /patterns
  Scenario: Handle PatternMatcherActor unavailable
    Given an HTTPServerActor without patternMatcher actor configured
    When I GET "/patterns"
    Then the response status should be 503
    And the error should indicate "Pattern matcher not available"

  # Error Paths - Unknown Routes
  Scenario: Handle unknown GET route
    Given an HTTPServerActor that is running
    When I GET "/unknown-route"
    Then the response status should be 404
    And the error should indicate "Not Found"

  Scenario: Handle unknown POST route
    Given an HTTPServerActor that is running
    When I POST to "/unknown-route"
    Then the response status should be 404
    And the error should indicate "Not Found"

  Scenario: Handle unsupported HTTP method
    Given an HTTPServerActor that is running
    When I send a DELETE request to "/events"
    Then the response status should be 404

  # CORS Support
  Scenario: Handle preflight OPTIONS request
    Given an HTTPServerActor that is running
    When I send OPTIONS request to "/events"
    Then the response status should be 204
    And the response should include CORS headers
    And Access-Control-Allow-Origin should be "*"
    And Access-Control-Allow-Methods should include "GET, POST, OPTIONS"

  Scenario: Include CORS headers in all responses
    Given an HTTPServerActor that is running
    When I GET "/health"
    Then the response should include CORS headers
    And Access-Control-Allow-Origin should be "*"

  # Concurrent Operations
  Scenario: Handle multiple concurrent POST /events requests
    Given an HTTPServerActor with eventLog actor configured
    When I send 20 concurrent POST requests to "/events"
    Then all requests should eventually complete
    And each request should receive a unique eventId
    And all events should be passed to EventLogActor

  Scenario: Handle mixed concurrent requests
    Given an HTTPServerActor with all actors configured
    When I send concurrent requests to different endpoints
    Then all requests should be processed independently
    And responses should be returned without interference

  # Resource Management
  Scenario: Gracefully handle server errors
    Given an HTTPServerActor that is running
    And an internal server error occurs during request processing
    When I GET "/events"
    Then the response status should be 500
    And the error should indicate "Internal Server Error"

  Scenario: Measure server uptime correctly
    Given an HTTPServerActor that is running for 5 seconds
    When I GET "/health"
    Then the uptime should be approximately 5000 milliseconds

  # Edge Cases
  Scenario: Parse complex query parameters
    Given an HTTPServerActor with eventLog actor configured
    When I GET "/events?limit=10&offset=5&reverse=true&type=test.event"
    Then the response status should be 200
    And all query parameters should be correctly parsed

  Scenario: Handle empty events query result
    Given an HTTPServerActor with eventLog actor configured
    And EventLogActor has no events
    When I GET "/events"
    Then the response status should be 200
    And the events array should be empty
    And the count should be 0

  Scenario: Return proper JSON formatting
    Given an HTTPServerActor that is running
    When I GET "/health"
    Then the response Content-Type should be "application/json"
    And the response body should be valid JSON
    And the response should be pretty-printed (indented)

  Scenario: Handle very large event payloads
    Given an HTTPServerActor with eventLog actor configured
    When I POST to "/events" with a 10MB event payload
    Then the request should be processed
    And the response should be returned without timeout
