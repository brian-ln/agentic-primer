Feature: DaemonActor - Event system orchestrator
  As a system administrator
  I want to manage the event system lifecycle
  So that all actors are coordinated and managed centrally

  Background:
    Given a clean test environment
    And a temporary config file

  # Lifecycle Transitions
  Scenario: Start DaemonActor successfully
    Given a DaemonActor with valid configuration
    When I start the daemon
    Then the daemon state should be RUNNING
    And the start result should indicate success
    And the configuration should be loaded
    And signal handlers should be registered
    And the daemon should have a process ID

  Scenario: Prevent starting an already running daemon
    Given a DaemonActor in RUNNING state
    When I attempt to start the daemon again
    Then the start should fail
    And the error should indicate "Cannot start daemon in running state"

  Scenario: Prevent starting a daemon in STARTING state
    Given a DaemonActor in STARTING state
    When I attempt to start the daemon again
    Then the start should fail
    And the error should indicate "Cannot start daemon in starting state"

  Scenario: Stop DaemonActor gracefully
    Given a DaemonActor in RUNNING state
    When I stop the daemon
    Then the daemon state should be STOPPED
    And all actors should be stopped
    And the stop result should indicate success

  Scenario: Stop an already stopped daemon
    Given a DaemonActor in STOPPED state
    When I stop the daemon
    Then the stop should succeed
    And the state should remain STOPPED

  Scenario: Prevent stopping a daemon in STOPPING state
    Given a DaemonActor in STOPPING state
    When I attempt to stop the daemon again
    Then the stop should succeed
    And the state should remain STOPPING

  Scenario: Get status of running daemon
    Given a DaemonActor in RUNNING state
    When I get the daemon status
    Then the status should show state RUNNING
    And the status should include pid
    And the status should include uptime
    And the status should include startTime
    And the status should include config
    And the status should include actor statuses

  # Happy Paths - Configuration Loading
  Scenario: Load configuration from file successfully
    Given a DaemonActor with config path "./config.json"
    And a valid config file exists
    When I start the daemon
    Then the configuration should be loaded
    And the config should be available in daemon state
    And the start should succeed

  Scenario: Configuration includes event log settings
    Given a DaemonActor with config containing eventLog settings
    When I start the daemon
    Then the EventLogActor should be spawned with the config

  Scenario: Configuration with custom event log file path
    Given a config with eventLog.file set to "/tmp/custom-events.jsonl"
    When I start the daemon
    Then the EventLogActor should use the custom file path

  Scenario: Configuration with custom checkpoint interval
    Given a config with eventLog.checkpointInterval set to 500
    When I start the daemon
    Then the EventLogActor should use checkpoint interval 500

  # Happy Paths - Actor Spawning
  Scenario: Spawn EventLogActor on startup
    Given a DaemonActor with valid configuration
    When I start the daemon
    Then the EventLogActor should be spawned
    And the EventLogActor should be started
    And the daemon should track the EventLogActor

  Scenario: Handle EventLogActor not yet implemented
    Given a DaemonActor with valid configuration
    And EventLogActor is not available
    When I start the daemon
    Then the daemon should start successfully
    And a placeholder should be created for EventLogActor
    And the actor state should be "not_implemented"

  Scenario: Stop all actors on daemon shutdown
    Given a DaemonActor in RUNNING state with spawned actors
    When I stop the daemon
    Then all actors should be stopped gracefully
    And the EventLogActor should be stopped

  # Happy Paths - Signal Handling
  Scenario: Handle SIGINT for graceful shutdown
    Given a DaemonActor in RUNNING state
    When the process receives SIGINT
    Then the daemon should initiate graceful shutdown
    And all actors should be stopped
    And the process should exit with code 0

  Scenario: Handle SIGTERM for graceful shutdown
    Given a DaemonActor in RUNNING state
    When the process receives SIGTERM
    Then the daemon should initiate graceful shutdown
    And all actors should be stopped
    And the process should exit with code 0

  Scenario: Handle uncaught exceptions
    Given a DaemonActor in RUNNING state
    When an uncaught exception occurs
    Then the daemon state should change to ERROR
    And the error should be recorded
    And the daemon should stop gracefully
    And the process should exit with code 1

  Scenario: Handle unhandled promise rejections
    Given a DaemonActor in RUNNING state
    When an unhandled promise rejection occurs
    Then the daemon state should change to ERROR
    And the error should be recorded
    And the daemon should stop gracefully
    And the process should exit with code 1

  # Happy Paths - Shutdown Handlers
  Scenario: Register custom shutdown handlers
    Given a DaemonActor in RUNNING state
    When I register a custom shutdown handler
    Then the handler should be stored

  Scenario: Execute shutdown handlers on stop
    Given a DaemonActor with 3 registered shutdown handlers
    When I stop the daemon
    Then all shutdown handlers should be executed
    And handlers should execute in registration order

  Scenario: Continue shutdown despite handler errors
    Given a DaemonActor with shutdown handlers
    And one handler throws an error
    When I stop the daemon
    Then the daemon should stop successfully
    And the error should be logged
    And other handlers should still execute

  # Happy Paths - Status and Monitoring
  Scenario: Track daemon uptime
    Given a DaemonActor that has been running for 5 seconds
    When I get the daemon status
    Then the uptime should be approximately 5000 milliseconds

  Scenario: Include actor status in daemon status
    Given a DaemonActor with spawned EventLogActor
    When I get the daemon status
    Then the status should include actors.eventLog
    And the eventLog status should show its state

  Scenario: Status includes error information
    Given a DaemonActor in ERROR state
    And the error was "Failed to initialize"
    When I get the daemon status
    Then the status should include the error message

  Scenario: Status with unspawned actors
    Given a DaemonActor in RUNNING state
    And EventLogActor was not spawned
    When I get the daemon status
    Then actors.eventLog should show "not_spawned"

  # Error Paths - Configuration
  Scenario: Handle missing config file
    Given a DaemonActor with config path "./nonexistent.json"
    When I start the daemon
    Then the start should fail
    And the daemon state should be ERROR
    And the error should indicate "Failed to load config"

  Scenario: Handle invalid JSON in config file
    Given a DaemonActor with config file containing invalid JSON
    When I start the daemon
    Then the start should fail
    And the daemon state should be ERROR
    And the error should indicate JSON parse error

  Scenario: Handle config file read permission error
    Given a DaemonActor with config file without read permissions
    When I start the daemon
    Then the start should fail
    And the daemon state should be ERROR

  # Error Paths - Actor Spawning
  Scenario: Handle EventLogActor spawn failure
    Given a DaemonActor with valid configuration
    And EventLogActor fails to start
    When I start the daemon
    Then the start should fail
    And the daemon state should be ERROR
    And the error should be recorded

  Scenario: Continue shutdown if actor stop fails
    Given a DaemonActor in RUNNING state
    And EventLogActor stop() throws an error
    When I stop the daemon
    Then the daemon should continue shutdown process
    And the error should be logged
    And the daemon state should be STOPPED

  # Error Paths - State Transitions
  Scenario: Handle error during startup
    Given a DaemonActor with valid configuration
    And an error occurs during startup
    When I start the daemon
    Then the daemon state should be ERROR
    And the error should be recorded
    And the start should fail

  Scenario: Handle error during shutdown
    Given a DaemonActor in RUNNING state
    And an error occurs during shutdown
    When I stop the daemon
    Then the daemon state should be ERROR
    And the error should be recorded
    And the stop should fail

  # Concurrent Operations
  Scenario: Handle concurrent status requests
    Given a DaemonActor in RUNNING state
    When multiple status requests are made concurrently
    Then all requests should return consistent status

  Scenario: Prevent concurrent start operations
    Given a DaemonActor in STOPPED state
    When concurrent start operations are attempted
    Then only one start should succeed
    And others should fail with state error

  # Resource Management
  Scenario: Clean up resources on normal shutdown
    Given a DaemonActor with multiple spawned actors
    When I stop the daemon
    Then all actor resources should be cleaned up
    And shutdown handlers should execute
    And the daemon should be in STOPPED state

  Scenario: Clean up resources on error shutdown
    Given a DaemonActor in RUNNING state
    When an uncaught exception causes shutdown
    Then the daemon should attempt to clean up resources
    And actors should be stopped
    And the process should exit

  Scenario: Handle signal during startup
    Given a DaemonActor that is starting
    When a SIGTERM is received during startup
    Then the startup should be interrupted
    And graceful shutdown should be initiated

  # Edge Cases
  Scenario: Daemon with default config path
    Given a DaemonActor created without config path parameter
    When I start the daemon
    Then the config path should default to "./config.json"

  Scenario: Empty config file
    Given a DaemonActor with config file containing {}
    When I start the daemon
    Then the start should succeed
    And default values should be used

  Scenario: Multiple signal handlers registration
    Given a DaemonActor that is starting
    When signal handlers are registered
    Then handlers should be registered for SIGINT
    And handlers should be registered for SIGTERM
    And handlers should be registered for uncaughtException
    And handlers should be registered for unhandledRejection

  Scenario: Actor state reporting when not implemented
    Given a DaemonActor with EventLogActor placeholder
    When I get the daemon status
    Then actors.eventLog should report "not_implemented"

  Scenario: Daemon started directly via CLI
    Given the daemon is run as main module
    When the daemon starts
    Then it should load configuration
    And it should spawn actors
    And it should keep the process alive
    And it should log startup messages

  Scenario: Debug logging enabled
    Given a DaemonActor in RUNNING state
    And DEBUG environment variable is set
    When the daemon runs
    Then status should be logged every 10 seconds

  Scenario: Status tracking with no startTime
    Given a DaemonActor in STOPPED state
    When I get the daemon status
    Then the uptime should be 0
    And startTime should be null

  Scenario: Resolve absolute config path
    Given a DaemonActor with relative config path "./config.json"
    When configuration is loaded
    Then the config path should be resolved to absolute path

  Scenario: Graceful shutdown timeout
    Given a DaemonActor in RUNNING state
    And an actor takes very long to stop
    When I stop the daemon with timeout
    Then the daemon should wait for graceful shutdown
    And should force stop after timeout

  Scenario: Process ID tracking
    Given a DaemonActor in RUNNING state
    When I get the daemon status
    Then the status should include the current process.pid

  Scenario: Actor spawn order
    Given a DaemonActor with valid configuration
    When I start the daemon
    Then actors should be spawned in defined order
    And EventLogActor should be spawned first

  Scenario: State machine integrity
    Given a DaemonActor in any state
    When state transitions occur
    Then valid state transitions should be allowed
    And invalid state transitions should be prevented
    And the state machine should remain consistent
