Feature: PatternMatcherActor - Event pattern matching with JavaScript predicates
  As an event processing system
  I want to match events against registered patterns
  So that I can trigger appropriate actions

  Background:
    Given a clean test environment
    And a PatternMatcherActor instance

  # Lifecycle Transitions
  Scenario: Start PatternMatcherActor successfully
    Given a PatternMatcherActor that is not running
    When I start the actor
    Then the actor should be running
    And the start result should indicate success

  Scenario: Prevent starting an already running PatternMatcherActor
    Given a PatternMatcherActor that is running
    When I attempt to start the actor again
    Then the start should fail
    And the error should indicate "PatternMatcherActor is already running"

  Scenario: Stop PatternMatcherActor successfully
    Given a PatternMatcherActor that is running
    When I stop the actor
    Then the actor should not be running
    And the stop result should indicate success

  Scenario: Stop an already stopped PatternMatcherActor
    Given a PatternMatcherActor that is not running
    When I stop the actor
    Then the stop should succeed
    And the message should indicate "PatternMatcherActor was not running"

  Scenario: Get status of running PatternMatcherActor
    Given a PatternMatcherActor that is running
    When I get the actor status
    Then the status should show isRunning as true
    And the status should include patternCount

  # Happy Paths - Pattern Registration
  Scenario: Register a simple pattern successfully
    Given a PatternMatcherActor that is running
    When I register a pattern with id "user-pattern" and predicate "event.type === 'user.created'"
    Then the registration should succeed
    And the pattern should be stored
    And the pattern should have default priority 0

  Scenario: Register a pattern with custom priority
    Given a PatternMatcherActor that is running
    When I register a pattern with id "high-priority" and priority 100
    Then the registration should succeed
    And the pattern should have priority 100

  Scenario: Register a pattern with metadata
    Given a PatternMatcherActor that is running
    When I register a pattern with metadata {"description": "User creation handler"}
    Then the registration should succeed
    And the pattern should preserve the metadata

  Scenario: Register multiple patterns
    Given a PatternMatcherActor that is running
    When I register 5 different patterns
    Then all registrations should succeed
    And the pattern count should be 5

  # Happy Paths - Pattern Unregistration
  Scenario: Unregister an existing pattern
    Given a PatternMatcherActor with a registered pattern "test-pattern"
    When I unregister pattern "test-pattern"
    Then the unregistration should succeed
    And the pattern should be removed
    And the pattern count should decrease by 1

  # Happy Paths - Pattern Listing
  Scenario: List all registered patterns
    Given a PatternMatcherActor with 3 registered patterns
    When I list all patterns
    Then I should receive an array of 3 patterns
    And each pattern should have id, predicate, priority, and metadata

  Scenario: List patterns sorted by priority
    Given a PatternMatcherActor with patterns of different priorities
    And pattern "low" with priority 10
    And pattern "high" with priority 100
    And pattern "medium" with priority 50
    When I list patterns with sortByPriority option
    Then the patterns should be in order: high, medium, low
    And priorities should be in descending order

  Scenario: List patterns with stable sorting
    Given a PatternMatcherActor with patterns of same priority
    And pattern "alpha" with priority 10
    And pattern "beta" with priority 10
    When I list patterns with sortByPriority option
    Then the patterns should be sorted by id alphabetically as tiebreaker

  # Happy Paths - Event Matching
  Scenario: Match event against a simple pattern
    Given a PatternMatcherActor with pattern "user-created" predicate "event.type === 'user.created'"
    When I match event {"type": "user.created", "data": {"userId": "123"}}
    Then the match should succeed
    And the matches array should contain pattern "user-created"

  Scenario: Match event against multiple patterns
    Given a PatternMatcherActor with 3 patterns that match user events
    When I match a user event
    Then the match should succeed
    And the matches array should contain all 3 patterns

  Scenario: No match for non-matching event
    Given a PatternMatcherActor with pattern "user-created" predicate "event.type === 'user.created'"
    When I match event {"type": "order.placed"}
    Then the match should succeed
    And the matches array should be empty

  Scenario: Match with complex predicate
    Given a PatternMatcherActor with pattern "high-value-order" predicate "event.type === 'order.placed' && event.data.amount > 1000"
    When I match event {"type": "order.placed", "data": {"amount": 1500}}
    Then the match should succeed
    And the matches array should contain pattern "high-value-order"

  Scenario: Match returns matches in priority order
    Given a PatternMatcherActor with multiple matching patterns
    And pattern "p1" with priority 100
    And pattern "p2" with priority 50
    And pattern "p3" with priority 200
    When I match an event that matches all patterns
    Then the matches should be in priority order: p3, p1, p2

  Scenario: Match includes pattern metadata
    Given a PatternMatcherActor with pattern "test" with metadata {"handler": "userHandler"}
    When I match a matching event
    Then the match result should include the pattern metadata

  # Happy Paths - Pattern Retrieval
  Scenario: Get pattern by ID
    Given a PatternMatcherActor with registered pattern "test-pattern"
    When I get pattern "test-pattern"
    Then I should receive the pattern details
    And the pattern should have id "test-pattern"

  Scenario: Get pattern count
    Given a PatternMatcherActor with 7 registered patterns
    When I get the pattern count
    Then the count should be 7

  Scenario: Clear all patterns
    Given a PatternMatcherActor with 5 registered patterns
    When I clear all patterns
    Then the clear should succeed
    And the cleared count should be 5
    And the pattern count should be 0

  # Error Paths - Registration
  Scenario: Reject registration with missing pattern object
    Given a PatternMatcherActor that is running
    When I attempt to register a null pattern
    Then the registration should fail
    And the error should indicate "Pattern must be an object"

  Scenario: Reject registration with missing id
    Given a PatternMatcherActor that is running
    When I attempt to register a pattern without an id
    Then the registration should fail
    And the error should indicate "Pattern must have an id string"

  Scenario: Reject registration with non-string id
    Given a PatternMatcherActor that is running
    When I attempt to register a pattern with numeric id 123
    Then the registration should fail
    And the error should indicate "Pattern must have an id string"

  Scenario: Reject registration with missing predicate
    Given a PatternMatcherActor that is running
    When I attempt to register a pattern without a predicate
    Then the registration should fail
    And the error should indicate "Pattern must have a predicate string"

  Scenario: Reject registration with non-string predicate
    Given a PatternMatcherActor that is running
    When I attempt to register a pattern with function predicate
    Then the registration should fail
    And the error should indicate "Pattern must have a predicate string"

  Scenario: Reject duplicate pattern ID
    Given a PatternMatcherActor with registered pattern "existing-pattern"
    When I attempt to register another pattern with id "existing-pattern"
    Then the registration should fail
    And the error should indicate "Pattern with id 'existing-pattern' already exists"

  Scenario: Reject invalid predicate syntax
    Given a PatternMatcherActor that is running
    When I attempt to register a pattern with predicate "event.type ==="
    Then the registration should fail
    And the error should indicate "Invalid predicate syntax"

  Scenario: Reject pattern with syntax error in predicate
    Given a PatternMatcherActor that is running
    When I attempt to register a pattern with predicate "function() { invalid syntax"
    Then the registration should fail
    And the error should include syntax error details

  # Error Paths - Unregistration
  Scenario: Reject unregistration with missing ID
    Given a PatternMatcherActor that is running
    When I attempt to unregister a pattern with null id
    Then the unregistration should fail
    And the error should indicate "Pattern ID must be a string"

  Scenario: Reject unregistration of non-existent pattern
    Given a PatternMatcherActor that is running
    When I attempt to unregister pattern "nonexistent"
    Then the unregistration should fail
    And the error should indicate "Pattern 'nonexistent' not found"

  # Error Paths - Matching
  Scenario: Reject matching with null event
    Given a PatternMatcherActor that is running
    When I attempt to match a null event
    Then the match should fail
    And the error should indicate "Event must be an object"

  Scenario: Reject matching with non-object event
    Given a PatternMatcherActor that is running
    When I attempt to match a string event "test"
    Then the match should fail
    And the error should indicate "Event must be an object"

  Scenario: Handle predicate runtime errors gracefully
    Given a PatternMatcherActor with pattern "error-prone" predicate "event.data.nested.deep.property === 'value'"
    When I match event {"type": "test"} without nested property
    Then the match should succeed
    And the matches array should be empty
    And the errors array should contain the runtime error
    And the error should include pattern "error-prone"

  Scenario: Continue matching after predicate error
    Given a PatternMatcherActor with 3 patterns
    And pattern "p1" with valid predicate
    And pattern "p2" with error-prone predicate
    And pattern "p3" with valid predicate
    When I match an event
    Then the match should succeed
    And the matches should include patterns p1 and p3
    And the errors should include error from p2

  # Error Paths - Retrieval
  Scenario: Get non-existent pattern returns null
    Given a PatternMatcherActor that is running
    When I get pattern "nonexistent"
    Then the result should be null

  # Concurrent Operations
  Scenario: Register patterns concurrently
    Given a PatternMatcherActor that is running
    When I register 10 patterns concurrently
    Then all unique registrations should succeed
    And the pattern count should be 10

  Scenario: Match events concurrently
    Given a PatternMatcherActor with 5 registered patterns
    When I match 20 events concurrently
    Then all matches should complete
    And each match should evaluate all patterns

  Scenario: Concurrent register and match operations
    Given a PatternMatcherActor that is running
    When patterns are being registered concurrently with match operations
    Then all operations should complete successfully
    And matches should use patterns available at match time

  # Resource Management
  Scenario: Handle patterns with complex predicates efficiently
    Given a PatternMatcherActor that is running
    When I register a pattern with a very long predicate
    Then the registration should succeed
    And subsequent matches should execute efficiently

  Scenario: Manage memory with many patterns
    Given a PatternMatcherActor that is running
    When I register 1000 patterns
    Then all registrations should succeed
    And pattern retrieval should remain fast

  # Edge Cases
  Scenario: Pattern predicate accessing event metadata
    Given a PatternMatcherActor with pattern predicate "event.metadata.depth > 5"
    When I match event with metadata depth 6
    Then the match should succeed
    And the pattern should match

  Scenario: Pattern predicate with complex boolean logic
    Given a PatternMatcherActor with pattern predicate "(event.type === 'A' || event.type === 'B') && event.data.value > 10"
    When I match event {"type": "A", "data": {"value": 15}}
    Then the match should succeed
    And the pattern should match

  Scenario: Pattern with zero priority
    Given a PatternMatcherActor that is running
    When I register a pattern with priority 0
    Then the registration should succeed
    And the pattern should have priority 0

  Scenario: Pattern with negative priority
    Given a PatternMatcherActor that is running
    When I register a pattern with priority -10
    Then the registration should succeed
    And the pattern should have priority -10
    And it should be sorted after positive priorities

  Scenario: Empty metadata object
    Given a PatternMatcherActor that is running
    When I register a pattern without metadata field
    Then the registration should succeed
    And the pattern should have empty metadata object
