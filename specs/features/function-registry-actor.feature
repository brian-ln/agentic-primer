Feature: FunctionRegistryActor - Function catalog management
  As an event system component
  I want to track available functions and their metadata
  So that functions can be discovered and executed

  Background:
    Given a clean test environment
    And a FunctionRegistryActor instance

  # Lifecycle Transitions
  Scenario: Start FunctionRegistryActor successfully
    Given a FunctionRegistryActor that is not running
    When I start the actor
    Then the actor should be running
    And the start result should indicate success

  Scenario: Prevent starting an already running FunctionRegistryActor
    Given a FunctionRegistryActor that is running
    When I attempt to start the actor again
    Then the start should fail
    And the error should indicate "FunctionRegistryActor is already running"

  Scenario: Stop FunctionRegistryActor successfully
    Given a FunctionRegistryActor that is running
    When I stop the actor
    Then the actor should not be running
    And the stop result should indicate success

  Scenario: Stop an already stopped FunctionRegistryActor
    Given a FunctionRegistryActor that is not running
    When I stop the actor
    Then the stop should succeed
    And the message should indicate "FunctionRegistryActor was not running"

  Scenario: Get status of running FunctionRegistryActor
    Given a FunctionRegistryActor that is running
    When I get the actor status
    Then the status should show isRunning as true
    And the status should include functionCount
    And the status should include list of function IDs

  # Happy Paths - Code Function Registration
  Scenario: Register a code function successfully
    Given a FunctionRegistryActor that is running
    When I register function "myFunction" with type "code" and path "/path/to/function.js"
    Then the registration should succeed
    And the function should be stored
    And the function type should be "code"
    And the function path should be "/path/to/function.js"

  Scenario: Register code function with full metadata
    Given a FunctionRegistryActor that is running
    When I register a code function with metadata {"name": "User Handler", "description": "Handles user events", "author": "dev-team"}
    Then the registration should succeed
    And the function metadata should be preserved

  Scenario: Register code function with maxStackDepth
    Given a FunctionRegistryActor that is running
    When I register a code function with maxStackDepth 5
    Then the registration should succeed
    And the function should have maxStackDepth 5

  Scenario: Register code function with auto-generated metadata
    Given a FunctionRegistryActor that is running
    When I register function "testFunc" without metadata
    Then the registration should succeed
    And the function name should default to "testFunc"
    And the description should be empty
    And the author should be empty

  # Happy Paths - Agent Function Registration
  Scenario: Register an agent function successfully
    Given a FunctionRegistryActor that is running
    When I register function "myAgent" with type "agent" and agentCommand "claude"
    Then the registration should succeed
    And the function type should be "agent"
    And the agentCommand should be "claude"

  Scenario: Register agent function with custom agent command
    Given a FunctionRegistryActor that is running
    When I register an agent function with agentCommand "custom-agent-cli"
    Then the registration should succeed
    And the agentCommand should be "custom-agent-cli"

  # Happy Paths - Function Unregistration
  Scenario: Unregister an existing function
    Given a FunctionRegistryActor with registered function "testFunc"
    When I unregister function "testFunc"
    Then the unregistration should succeed
    And the function should be removed
    And the result should indicate the function existed

  Scenario: Unregister a non-existent function
    Given a FunctionRegistryActor that is running
    When I unregister function "nonexistent"
    Then the unregistration should succeed
    And the result should indicate the function did not exist

  # Happy Paths - Function Retrieval
  Scenario: Get function by ID
    Given a FunctionRegistryActor with registered function "testFunc"
    When I get function "testFunc"
    Then I should receive the function details
    And the function should include type, path, and metadata

  Scenario: Get function includes registeredAt timestamp
    Given a FunctionRegistryActor that is running
    When I register a function
    Then the function should have a registeredAt timestamp
    And the timestamp should be in ISO format

  # Happy Paths - Function Listing
  Scenario: List all registered functions
    Given a FunctionRegistryActor with 5 registered functions
    When I list all functions
    Then I should receive 5 functions
    And each function should have functionId field

  Scenario: List functions filtered by type "code"
    Given a FunctionRegistryActor with mixed function types
    And 3 functions of type "code"
    And 2 functions of type "agent"
    When I list functions with type filter "code"
    Then I should receive 3 functions
    And all functions should have type "code"

  Scenario: List functions filtered by type "agent"
    Given a FunctionRegistryActor with mixed function types
    And 3 functions of type "code"
    And 2 functions of type "agent"
    When I list functions with type filter "agent"
    Then I should receive 2 functions
    And all functions should have type "agent"

  Scenario: List functions returns count
    Given a FunctionRegistryActor with 10 registered functions
    When I list all functions
    Then the response should include count field
    And the count should be 10

  # Happy Paths - Directory Scanning
  Scenario: Scan directory for code functions
    Given a FunctionRegistryActor that is running
    And a directory with 3 .js files
    When I scan the directory
    Then the scan should succeed
    And 3 functions should be discovered
    And all discovered functions should have type "code"

  Scenario: Scan directory for agent functions
    Given a FunctionRegistryActor that is running
    And a directory with 2 .agent.js files
    When I scan the directory
    Then the scan should succeed
    And 2 functions should be discovered
    And all discovered functions should have type "agent"

  Scenario: Scan directory with mixed function types
    Given a FunctionRegistryActor that is running
    And a directory with 3 .js files and 2 .agent.js files
    When I scan the directory
    Then the scan should succeed
    And 5 functions should be discovered
    And 3 should be type "code"
    And 2 should be type "agent"

  Scenario: Scan directory recursively
    Given a FunctionRegistryActor that is running
    And a directory with nested subdirectories containing functions
    When I scan the directory with recursive option
    Then the scan should succeed
    And all functions in subdirectories should be discovered

  Scenario: Scan directory non-recursively
    Given a FunctionRegistryActor that is running
    And a directory with nested subdirectories containing functions
    When I scan the directory without recursive option
    Then the scan should succeed
    And only functions in the top-level directory should be discovered

  Scenario: Scan directory with overwrite option
    Given a FunctionRegistryActor with registered function "existing"
    And a directory containing "existing.js"
    When I scan the directory with overwrite option
    Then the scan should succeed
    And the function "existing" should be overwritten
    And the discovered count should include "existing"

  Scenario: Scan directory without overwrite skips existing functions
    Given a FunctionRegistryActor with registered function "existing"
    And a directory containing "existing.js"
    When I scan the directory without overwrite option
    Then the scan should succeed
    And the function "existing" should be skipped
    And the skipped count should include "existing"

  Scenario: Scan directory ignores non-.js files
    Given a FunctionRegistryActor that is running
    And a directory with .js files and .txt files
    When I scan the directory
    Then the scan should succeed
    And only .js files should be discovered
    And .txt files should be ignored

  # Happy Paths - Statistics
  Scenario: Get registry statistics
    Given a FunctionRegistryActor with 5 code functions and 3 agent functions
    When I get the registry stats
    Then the stats should show totalFunctions 8
    And the stats should show codeFunctions 5
    And the stats should show agentFunctions 3

  Scenario: Clear all functions
    Given a FunctionRegistryActor with 10 registered functions
    When I clear all functions
    Then the function count should be 0

  # Error Paths - Registration
  Scenario: Reject registration with missing functionId
    Given a FunctionRegistryActor that is running
    When I attempt to register a function without functionId
    Then the registration should fail
    And the error should indicate "functionId is required and must be a string"

  Scenario: Reject registration with non-string functionId
    Given a FunctionRegistryActor that is running
    When I attempt to register a function with numeric functionId 123
    Then the registration should fail
    And the error should indicate "functionId is required and must be a string"

  Scenario: Reject registration with missing metadata
    Given a FunctionRegistryActor that is running
    When I attempt to register a function without metadata
    Then the registration should fail
    And the error should indicate "metadata is required and must be an object"

  Scenario: Reject registration with non-object metadata
    Given a FunctionRegistryActor that is running
    When I attempt to register a function with string metadata "invalid"
    Then the registration should fail
    And the error should indicate "metadata is required and must be an object"

  Scenario: Reject registration with invalid type
    Given a FunctionRegistryActor that is running
    When I attempt to register a function with type "invalid"
    Then the registration should fail
    And the error should indicate "metadata.type must be \"code\" or \"agent\""

  Scenario: Reject registration with missing type
    Given a FunctionRegistryActor that is running
    When I attempt to register a function without type in metadata
    Then the registration should fail
    And the error should indicate "metadata.type must be \"code\" or \"agent\""

  Scenario: Reject code function without path
    Given a FunctionRegistryActor that is running
    When I attempt to register a code function without path
    Then the registration should fail
    And the error should indicate "metadata.path is required for code functions"

  Scenario: Reject agent function without agentCommand
    Given a FunctionRegistryActor that is running
    When I attempt to register an agent function without agentCommand
    Then the registration should fail
    And the error should indicate "metadata.agentCommand is required for agent functions"

  # Error Paths - Retrieval
  Scenario: Get non-existent function returns error
    Given a FunctionRegistryActor that is running
    When I get function "nonexistent"
    Then the get should fail
    And the error should indicate "Function 'nonexistent' not found"

  Scenario: Get function with missing functionId
    Given a FunctionRegistryActor that is running
    When I attempt to get a function without functionId
    Then the get should fail
    And the error should indicate "functionId is required and must be a string"

  # Error Paths - Unregistration
  Scenario: Unregister function with missing functionId
    Given a FunctionRegistryActor that is running
    When I attempt to unregister a function without functionId
    Then the unregistration should fail
    And the error should indicate "functionId is required and must be a string"

  # Error Paths - Scanning
  Scenario: Scan non-existent directory
    Given a FunctionRegistryActor that is running
    When I scan a non-existent directory "/path/to/nonexistent"
    Then the scan should fail
    And the error should indicate "Failed to scan directory"

  Scenario: Scan directory with permission errors on subdirectory
    Given a FunctionRegistryActor that is running
    And a directory with an inaccessible subdirectory
    When I scan the directory recursively
    Then the scan should succeed
    And the errors array should include the permission error
    And accessible functions should still be discovered

  # Error Paths - Message Handling
  Scenario: Handle invalid UAP message
    Given a FunctionRegistryActor that is running
    When I send a message with invalid structure
    Then the message handling should fail
    And the error should indicate validation error

  Scenario: Handle unknown action
    Given a FunctionRegistryActor that is running
    When I send a valid message with unknown action "INVALID_ACTION"
    Then the message handling should fail
    And the error should indicate "Unknown action: INVALID_ACTION"

  # Concurrent Operations
  Scenario: Register multiple functions concurrently
    Given a FunctionRegistryActor that is running
    When I register 20 functions concurrently with unique IDs
    Then all registrations should succeed
    And the function count should be 20

  Scenario: Concurrent register and unregister operations
    Given a FunctionRegistryActor with 10 registered functions
    When concurrent register and unregister operations occur
    Then all operations should complete successfully
    And the final state should be consistent

  Scenario: Concurrent list operations
    Given a FunctionRegistryActor with registered functions
    When multiple list operations occur concurrently
    Then all list operations should return consistent results

  # Resource Management
  Scenario: Handle large number of functions
    Given a FunctionRegistryActor that is running
    When I register 1000 functions
    Then all registrations should succeed
    And function retrieval should remain performant

  Scenario: Handle functions with very long paths
    Given a FunctionRegistryActor that is running
    When I register a function with a 500-character path
    Then the registration should succeed
    And the path should be fully preserved

  # Edge Cases
  Scenario: Function ID with special characters
    Given a FunctionRegistryActor that is running
    When I register function "my-func_v2.handler"
    Then the registration should succeed
    And the function ID should be preserved exactly

  Scenario: Function path with spaces
    Given a FunctionRegistryActor that is running
    When I register a function with path "/path with spaces/function.js"
    Then the registration should succeed
    And the path should be preserved with spaces

  Scenario: Auto-discovered function naming
    Given a FunctionRegistryActor that is running
    And a file named "my-handler.js"
    When I scan the directory
    Then the function should be registered as "my-handler"

  Scenario: Auto-discovered agent function naming
    Given a FunctionRegistryActor that is running
    And a file named "my-agent.agent.js"
    When I scan the directory
    Then the function should be registered as "my-agent"
    And the type should be "agent"

  Scenario: Empty directory scan
    Given a FunctionRegistryActor that is running
    And an empty directory
    When I scan the directory
    Then the scan should succeed
    And the discovered count should be 0

  Scenario: Scan reports skipped, discovered, and errors separately
    Given a FunctionRegistryActor with 2 registered functions
    And a directory with 5 new functions and 2 existing functions
    When I scan without overwrite
    Then the scan should succeed
    And discovered count should be 5
    And skipped count should be 2
    And the scan result should include all three arrays
