Feature: FunctionExecutorActor - Code and agent function execution
  As an event system component
  I want to execute functions in response to events
  So that event-driven workflows can be automated

  Background:
    Given a clean test environment
    And a FunctionExecutorActor instance

  # Lifecycle Transitions
  Scenario: Start FunctionExecutorActor successfully
    Given a FunctionExecutorActor that is not running
    When I start the actor
    Then the actor should be running
    And the start result should indicate success

  Scenario: Prevent starting an already running FunctionExecutorActor
    Given a FunctionExecutorActor that is running
    When I attempt to start the actor again
    Then the start should fail
    And the error should indicate "FunctionExecutorActor is already running"

  Scenario: Stop FunctionExecutorActor successfully
    Given a FunctionExecutorActor that is running
    When I stop the actor
    Then the actor should not be running
    And the stop result should indicate success

  Scenario: Stop an already stopped FunctionExecutorActor
    Given a FunctionExecutorActor that is not running
    When I stop the actor
    Then the stop should succeed
    And the message should indicate "FunctionExecutorActor was not running"

  Scenario: Get status of running FunctionExecutorActor
    Given a FunctionExecutorActor that is running
    When I get the actor status
    Then the status should show isRunning as true
    And the status should show hasEmitCallback status

  # Happy Paths - Code Function Execution
  Scenario: Execute a simple code function successfully
    Given a FunctionExecutorActor that is running
    And a code function at "/tmp/test-func.js" that returns "success"
    When I execute the function with event {"type": "test.event"}
    Then the execution should succeed
    And the result should be "success"
    And the response should include executionTime
    And a "function.executed" event should be emitted

  Scenario: Execute code function with event data
    Given a FunctionExecutorActor that is running
    And a code function that accesses event.data
    When I execute the function with event {"type": "user.created", "data": {"userId": "123"}}
    Then the execution should succeed
    And the function should receive the event data
    And the function should process userId "123"

  Scenario: Execute code function with context
    Given a FunctionExecutorActor that is running
    And a code function that uses context.logger
    When I execute the function with event and context
    Then the execution should succeed
    And the function should have access to logger
    And the function should have access to emit
    And the function should have access to config

  Scenario: Function emits new events via context.emit
    Given a FunctionExecutorActor with emit callback configured
    And a code function that emits 3 events
    When I execute the function
    Then the execution should succeed
    And 3 events should be emitted via the callback
    And each emitted event should have depth tracking
    And each emitted event should have triggeredBy field

  Scenario: Function uses context.logger for output
    Given a FunctionExecutorActor that is running
    And a code function that logs messages
    When I execute the function
    Then the execution should succeed
    And log messages should be written with function ID prefix

  Scenario: Function receives custom config
    Given a FunctionExecutorActor with config {"maxRetries": 3}
    And a code function that accesses context.config
    When I execute the function
    Then the execution should succeed
    And the function should receive the config
    And config.maxRetries should be 3

  Scenario: Track execution time for functions
    Given a FunctionExecutorActor that is running
    And a code function that sleeps for 100ms
    When I execute the function
    Then the execution should succeed
    And the executionTime should be approximately 100ms

  Scenario: Execute async function successfully
    Given a FunctionExecutorActor that is running
    And an async code function
    When I execute the function
    Then the execution should succeed
    And the function should complete asynchronously

  # Happy Paths - Agent Function Execution
  Scenario: Execute an agent function successfully
    Given a FunctionExecutorActor that is running
    And an agent function with agentCommand "echo"
    When I execute the agent function with event {"type": "test.event"}
    Then the execution should succeed
    And the result should include response field
    And the functionType should be "agent"
    And a "function.executed" event should be emitted

  Scenario: Agent function receives event as prompt
    Given a FunctionExecutorActor that is running
    And a mock agent command that echoes stdin
    When I execute an agent function with event data
    Then the execution should succeed
    And the agent should receive the event as JSON in the prompt

  Scenario: Agent function with system prompt
    Given a FunctionExecutorActor that is running
    And an agent function with config.systemPrompt
    When I execute the agent function
    Then the execution should succeed
    And the prompt should include the system prompt
    And the prompt should include the event data

  Scenario: Agent function with custom task
    Given a FunctionExecutorActor that is running
    And an agent function with config.task "Analyze this event"
    When I execute the agent function
    Then the execution should succeed
    And the prompt should include the task

  Scenario: Agent function tracks execution time
    Given a FunctionExecutorActor that is running
    And an agent function that takes 200ms
    When I execute the agent function
    Then the execution should succeed
    And the executionTime should be approximately 200ms

  # Happy Paths - Depth Tracking
  Scenario: Track event depth for emitted events
    Given a FunctionExecutorActor with emit callback configured
    And a code function that emits an event
    And the triggering event has depth 2
    When I execute the function
    Then the execution should succeed
    And the emitted event should have depth 3

  Scenario: Track event depth for error events
    Given a FunctionExecutorActor with emit callback configured
    And a code function that throws an error
    And the triggering event has depth 1
    When I execute the function
    Then the execution should fail
    And the error event should have depth 2

  # Error Paths - Validation
  Scenario: Reject execution without functionId
    Given a FunctionExecutorActor that is running
    When I attempt to execute without functionId
    Then the execution should fail
    And the error should indicate "functionId is required"

  Scenario: Reject execution without functionPath
    Given a FunctionExecutorActor that is running
    When I attempt to execute without functionPath
    Then the execution should fail
    And the error should indicate "functionPath is required"

  Scenario: Reject execution without event
    Given a FunctionExecutorActor that is running
    When I attempt to execute without event
    Then the execution should fail
    And the error should indicate "event is required"

  # Error Paths - Import Errors
  Scenario: Handle non-existent function file
    Given a FunctionExecutorActor that is running
    When I execute a function with path "/nonexistent/function.js"
    Then the execution should fail
    And the error should indicate import error
    And the phase should be "import"
    And a "function.error" event should be emitted

  Scenario: Handle module with syntax errors
    Given a FunctionExecutorActor that is running
    And a code function file with syntax errors
    When I execute the function
    Then the execution should fail
    And the error should indicate import error
    And a "function.error" event should be emitted

  Scenario: Handle module without default export
    Given a FunctionExecutorActor that is running
    And a code function file without default export
    When I execute the function
    Then the execution should fail
    And the error should indicate "Module does not export a default function"
    And the phase should be "validation"

  Scenario: Handle module with non-function default export
    Given a FunctionExecutorActor that is running
    And a code function file exporting an object instead of function
    When I execute the function
    Then the execution should fail
    And the error should indicate "Module does not export a default function"

  # Error Paths - Execution Errors
  Scenario: Handle function runtime errors
    Given a FunctionExecutorActor that is running
    And a code function that throws an error
    When I execute the function
    Then the execution should fail
    And the error should include the error message
    And the error should include stack trace
    And the phase should be "execution"
    And a "function.error" event should be emitted

  Scenario: Handle function that rejects Promise
    Given a FunctionExecutorActor that is running
    And an async code function that rejects
    When I execute the function
    Then the execution should fail
    And the error should be captured
    And a "function.error" event should be emitted

  Scenario: Handle unexpected errors during execution
    Given a FunctionExecutorActor that is running
    And execution encounters an unexpected error
    When I execute the function
    Then the execution should fail
    And the error should indicate "Unexpected error"
    And the phase should be "unexpected"

  # Error Paths - Agent Execution
  Scenario: Handle agent command not found
    Given a FunctionExecutorActor that is running
    And an agent function with nonexistent agentCommand "nonexistent-command"
    When I execute the agent function
    Then the execution should fail
    And the error should indicate agent execution failure

  Scenario: Handle agent exit with non-zero code
    Given a FunctionExecutorActor that is running
    And an agent command that exits with code 1
    When I execute the agent function
    Then the execution should fail
    And the error should indicate "Agent failed with exit code 1"
    And the phase should be "agent-execution"
    And stderr should be included in the error

  Scenario: Handle agent execution errors
    Given a FunctionExecutorActor that is running
    And an agent function that encounters an error
    When I execute the agent function
    Then the execution should fail
    And the error should indicate agent execution error
    And the phase should be "agent-error"

  # Error Paths - Emit Callback
  Scenario: Handle missing emit callback gracefully
    Given a FunctionExecutorActor without emit callback
    And a code function that calls context.emit
    When I execute the function
    Then the execution should succeed
    But no events should be emitted
    And a warning should be logged

  Scenario: Handle emit callback errors
    Given a FunctionExecutorActor with emit callback that throws errors
    And a code function that emits an event
    When I execute the function
    Then the execution should succeed
    But the emit error should be handled gracefully

  # Error Paths - Message Handling
  Scenario: Handle invalid UAP message
    Given a FunctionExecutorActor that is running
    When I send a message with invalid structure
    Then the message handling should fail
    And the error should indicate validation error

  Scenario: Handle wrong protocol
    Given a FunctionExecutorActor that is running
    When I send a message with protocol "EVENT" instead of "FUNCTION"
    Then the message handling should fail
    And the error should indicate "Invalid protocol"

  Scenario: Handle unknown action
    Given a FunctionExecutorActor that is running
    When I send a valid FUNCTION message with unknown action "INVALID"
    Then the message handling should fail
    And the error should indicate "Unknown action: INVALID"

  # Concurrent Operations
  Scenario: Execute multiple code functions concurrently
    Given a FunctionExecutorActor that is running
    When I execute 10 code functions concurrently
    Then all executions should complete
    And each execution should be isolated
    And all results should be captured correctly

  Scenario: Execute multiple agent functions concurrently
    Given a FunctionExecutorActor that is running
    When I execute 5 agent functions concurrently
    Then all executions should complete
    And each agent should execute independently

  Scenario: Handle concurrent executions with emit
    Given a FunctionExecutorActor with emit callback configured
    And multiple functions that emit events
    When I execute the functions concurrently
    Then all executions should complete
    And all emitted events should be tracked
    And events should not interfere with each other

  # Resource Management
  Scenario: Clean up resources after execution
    Given a FunctionExecutorActor that is running
    And a code function that allocates resources
    When I execute the function
    Then the execution should complete
    And resources should be released

  Scenario: Handle memory-intensive functions
    Given a FunctionExecutorActor that is running
    And a code function that uses significant memory
    When I execute the function
    Then the execution should complete
    And memory should be released after completion

  Scenario: Timeout protection for long-running functions
    Given a FunctionExecutorActor that is running
    And a code function that runs indefinitely
    When I execute the function with timeout
    Then the execution should be terminated
    And a timeout error should be returned

  # Edge Cases
  Scenario: Execute function that returns undefined
    Given a FunctionExecutorActor that is running
    And a code function that returns undefined
    When I execute the function
    Then the execution should succeed
    And the result should be undefined

  Scenario: Execute function that returns null
    Given a FunctionExecutorActor that is running
    And a code function that returns null
    When I execute the function
    Then the execution should succeed
    And the result should be null

  Scenario: Execute function that returns complex object
    Given a FunctionExecutorActor that is running
    And a code function that returns nested objects
    When I execute the function
    Then the execution should succeed
    And the result should preserve the object structure

  Scenario: Function emits events with custom metadata
    Given a FunctionExecutorActor with emit callback configured
    And a code function that emits events with metadata
    When I execute the function
    Then the execution should succeed
    And emitted events should preserve custom metadata
    And depth tracking should be added to metadata

  Scenario: Execute function with empty event data
    Given a FunctionExecutorActor that is running
    And a code function
    When I execute with event {"type": "test.event", "data": {}}
    Then the execution should succeed
    And the function should receive empty data object

  Scenario: Logger with DEBUG environment variable
    Given a FunctionExecutorActor that is running
    And DEBUG environment variable is set
    And a code function that calls logger.debug
    When I execute the function
    Then the execution should succeed
    And debug messages should be logged

  Scenario: Logger without DEBUG environment variable
    Given a FunctionExecutorActor that is running
    And DEBUG environment variable is not set
    And a code function that calls logger.debug
    When I execute the function
    Then the execution should succeed
    And debug messages should not be logged

  Scenario: Agent prompt with all components
    Given a FunctionExecutorActor that is running
    When I execute an agent function with systemPrompt, task, and event
    Then the prompt should include system prompt
    And the prompt should include event JSON
    And the prompt should include task
    And components should be separated by blank lines

  Scenario: Agent captures stderr separately from stdout
    Given a FunctionExecutorActor that is running
    And an agent that writes to both stdout and stderr
    When I execute the agent function
    Then the execution should succeed
    And result.response should contain stdout
    And result.stderr should contain stderr output

  Scenario: Execute with absolute path resolution
    Given a FunctionExecutorActor that is running
    When I execute with relative path "./test-func.js"
    Then the path should be resolved to absolute path
    And the execution should use the absolute path
