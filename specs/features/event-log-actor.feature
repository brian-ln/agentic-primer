Feature: EventLogActor - Append-only event storage with replay capability
  As an event system component
  I want to persist events to JSONL storage
  So that events can be queried and replayed

  Background:
    Given a clean test environment
    And a temporary event log file path

  # Lifecycle Transitions
  Scenario: Start EventLogActor successfully
    Given an EventLogActor with default configuration
    When I start the actor
    Then the actor should be initialized
    And the start result should indicate success
    And the log directory should exist

  Scenario: Stop EventLogActor gracefully
    Given an EventLogActor that is running
    When I stop the actor
    Then the write stream should be closed
    And the actor should not be initialized
    And the stop result should indicate success

  Scenario: Get status of running EventLogActor
    Given an EventLogActor that is running
    When I get the actor status
    Then the status should show isRunning as true
    And the status should include the event count
    And the status should include the log path

  Scenario: Prevent starting an already running EventLogActor
    Given an EventLogActor that is running
    When I attempt to start the actor again
    Then the actor should remain in running state
    And no errors should occur

  # Happy Paths - Append Operations
  Scenario: Append a single event successfully
    Given an EventLogActor that is running
    When I append an event with type "user.created" and data {"userId": "123"}
    Then the append should succeed
    And the response should include an event ID
    And the event count should be 1
    And the event should be written to the log file

  Scenario: Append multiple events in sequence
    Given an EventLogActor that is running
    When I append 5 events with different types
    Then all appends should succeed
    And the event count should be 5
    And all events should be in the log file

  Scenario: Append event with auto-generated ULID
    Given an EventLogActor that is running
    When I append an event without an ID
    Then the append should succeed
    And the event should have an auto-generated ID starting with "evt_"
    And the ID should be a valid ULID format

  Scenario: Append event with metadata enrichment
    Given an EventLogActor that is running
    When I append an event with partial metadata
    Then the append should succeed
    And the event should have default metadata fields
    And the metadata should include source, triggeredBy, and depth

  # Happy Paths - Query Operations
  Scenario: Query all events without filters
    Given an EventLogActor with 10 existing events
    When I query events without filters
    Then the query should succeed
    And I should receive up to 100 events (default limit)
    And the total count should be 10

  Scenario: Query events with type filter
    Given an EventLogActor with mixed event types
    And 3 events of type "user.created"
    And 2 events of type "order.placed"
    When I query events with filter for type "user.created"
    Then the query should succeed
    And I should receive 3 events
    And all events should have type "user.created"

  Scenario: Query events with limit
    Given an EventLogActor with 20 existing events
    When I query events with limit 5
    Then the query should succeed
    And I should receive exactly 5 events

  Scenario: Query events with offset
    Given an EventLogActor with 10 existing events
    When I query events with offset 5
    Then the query should succeed
    And I should skip the first 5 events
    And I should receive the remaining 5 events

  Scenario: Query events in reverse order
    Given an EventLogActor with events A, B, C in chronological order
    When I query events with reverse option
    Then the query should succeed
    And I should receive events in order C, B, A

  # Happy Paths - Checkpoint and Replay
  Scenario: Create a checkpoint at current position
    Given an EventLogActor with 50 existing events
    When I create a checkpoint
    Then the checkpoint should succeed
    And the checkpoint should be at event count 50

  Scenario: Replay events from checkpoint
    Given an EventLogActor with 100 existing events
    And a checkpoint at position 50
    When I replay events from the checkpoint
    Then the replay should succeed
    And I should receive 50 events (from position 50 onwards)
    And the replayed count should be 50

  Scenario: Replay with custom handler function
    Given an EventLogActor with 10 existing events
    When I replay events with a transformation handler
    Then the replay should succeed
    And each event should be processed by the handler
    And the handler should receive all 10 events

  # Error Paths
  Scenario: Append event without required type field
    Given an EventLogActor that is running
    When I append an event without a type field
    Then the append should fail
    And the error should indicate "Event type is required"

  Scenario: Handle invalid message protocol
    Given an EventLogActor that is running
    When I send a message with protocol "INVALID_PROTOCOL"
    Then the message handling should fail
    And the error should indicate invalid protocol

  Scenario: Handle unknown action
    Given an EventLogActor that is running
    When I send a valid EVENT protocol message with unknown action "INVALID_ACTION"
    Then the message handling should fail
    And the error should indicate unknown action

  Scenario: Handle corrupted JSONL file during query
    Given an EventLogActor that is running
    And the log file contains malformed JSON on line 3
    When I query all events
    Then the query should succeed with partial results
    And the corrupted line should be skipped
    And an error should be logged for line 3

  Scenario: Handle missing log file during query
    Given an EventLogActor with a non-existent log file
    When I query events
    Then the query should succeed
    And I should receive an empty event list

  # Concurrent Operations
  Scenario: Handle concurrent append operations
    Given an EventLogActor that is running
    When 10 concurrent append operations are submitted
    Then all appends should eventually complete
    And the event count should be 10
    And all events should be in the log file
    And no events should be lost

  Scenario: Query while appending events
    Given an EventLogActor that is running
    And events are being continuously appended
    When I query events during the append operations
    Then the query should succeed
    And I should receive events up to the query point
    And the query should not block append operations

  # Resource Management
  Scenario: Auto-checkpoint at configured interval
    Given an EventLogActor with checkpointInterval of 100
    When I append 150 events
    Then a checkpoint should be auto-created at event 100
    And the last checkpoint should be at 100

  Scenario: Handle write stream errors gracefully
    Given an EventLogActor that is running
    And the write stream encounters an error
    When I append an event
    Then the append should fail gracefully
    And the error should be returned in the response

  Scenario: Reopen log file on restart
    Given an EventLogActor with an existing log file containing 50 events
    When I stop the actor
    And I start the actor again
    Then the actor should initialize successfully
    And the event count should be 50
    And new appends should continue from position 51

  Scenario: Count existing events on initialization
    Given a log file with 25 existing events
    When I create and start a new EventLogActor
    Then the actor should count existing events during start
    And the initial event count should be 25

  # Edge Cases
  Scenario: Handle empty event data
    Given an EventLogActor that is running
    When I append an event with type "test.event" and empty data object
    Then the append should succeed
    And the event should have an empty data field

  Scenario: Handle large event payloads
    Given an EventLogActor that is running
    When I append an event with 1MB of data
    Then the append should succeed
    And the event should be fully written to the log

  Scenario: Query with complex filter function
    Given an EventLogActor with events having various metadata depths
    When I query events with filter for depth greater than 2
    Then the query should succeed
    And only events with depth > 2 should be returned

  Scenario: Replay from beginning with fromCheckpoint 0
    Given an EventLogActor with 20 existing events
    When I replay events from checkpoint 0
    Then the replay should succeed
    And I should receive all 20 events
