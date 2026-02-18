/**
 * FSM (Finite State Machine) Validation Module
 *
 * Runtime validation for connection state transitions based on state-machine.json.
 *
 * Strategy:
 * - TEST MODE (NODE_ENV=test or vitest): Strict validation - THROW on illegal transitions
 * - PRODUCTION MODE: Log-only validation - LOG violations but continue
 *
 * This allows safe rollout of FSM validation without breaking production,
 * while ensuring tests catch state machine violations immediately.
 */

import type { ConnectionState } from '../types';

// Import state machine specification
import stateMachineSpec from '../../spec/connection/state-machine.json';

export interface TransitionValidationResult {
  valid: boolean;
  errors?: string[];
  allowedTransitions?: Array<{ event: string; target: ConnectionState }>;
  fromState?: ConnectionState;
  toState?: ConnectionState;
  event?: string;
}

export interface FSMValidationMode {
  mode: 'strict' | 'log-only';
  failOnError: boolean;
}

/**
 * FSMValidator class
 *
 * Validates connection state transitions against the state machine specification.
 * Supports strict mode (throw on error) and log-only mode (warn only).
 */
export class FSMValidator {
  private stateMachine: typeof stateMachineSpec;
  private mode: FSMValidationMode;

  constructor(mode: FSMValidationMode = { mode: 'log-only', failOnError: false }) {
    this.mode = mode;
    this.stateMachine = stateMachineSpec;

    console.log(JSON.stringify({
      event: 'fsm_validator_initialized',
      mode: this.mode.mode,
      failOnError: this.mode.failOnError,
      initialState: this.stateMachine.initial,
      states: Object.keys(this.stateMachine.states),
      timestamp: Date.now(),
    }));
  }

  /**
   * Validate state transition
   *
   * @param fromState - Current state
   * @param toState - Target state
   * @param event - Event triggering the transition
   * @returns Validation result
   * @throws Error if in strict mode and transition is invalid
   */
  isValidTransition(
    fromState: ConnectionState,
    toState: ConnectionState,
    event: string
  ): TransitionValidationResult {
    // Validate that fromState exists in the state machine
    if (!(fromState in this.stateMachine.states)) {
      const error = `Unknown source state: ${fromState}`;
      return this.handleInvalidTransition(fromState, toState, event, [error]);
    }

    // Get allowed transitions from the current state
    const stateConfig = this.stateMachine.states[fromState as keyof typeof this.stateMachine.states];

    // Check if state has any transitions defined
    if (!('on' in stateConfig)) {
      const error = `State ${fromState} has no transitions defined (final state)`;
      return this.handleInvalidTransition(fromState, toState, event, [error]);
    }

    const transitions = stateConfig.on as Record<string, { target: string; guard?: string; description?: string }>;

    // Check if the event is valid for this state
    if (!(event in transitions)) {
      const allowedEvents = Object.keys(transitions);
      const allowedTransitions = Object.entries(transitions).map(([evt, config]) => ({
        event: evt,
        target: config.target as ConnectionState,
      }));

      const error = `Event '${event}' not allowed in state '${fromState}'. Allowed events: ${allowedEvents.join(', ')}`;
      return this.handleInvalidTransition(fromState, toState, event, [error], allowedTransitions);
    }

    // Check if the target state matches the expected state for this event
    const expectedTarget = transitions[event].target;
    if (expectedTarget !== toState) {
      const error = `Invalid transition: ${fromState} --[${event}]--> ${toState}. Expected target: ${expectedTarget}`;
      const allowedTransitions = [{
        event,
        target: expectedTarget as ConnectionState,
      }];
      return this.handleInvalidTransition(fromState, toState, event, [error], allowedTransitions);
    }

    // Transition is valid
    if (this.mode.mode === 'strict') {
      console.log(JSON.stringify({
        event: 'fsm_validation_success',
        fromState,
        toState,
        transitionEvent: event,
        timestamp: Date.now(),
      }));
    }

    return {
      valid: true,
      fromState,
      toState,
      event,
    };
  }

  /**
   * Handle invalid transition
   *
   * @param fromState - Current state
   * @param toState - Attempted target state
   * @param event - Event that triggered the transition
   * @param errors - List of validation errors
   * @param allowedTransitions - List of allowed transitions from current state
   * @returns Validation result with errors
   * @throws Error if in strict mode
   */
  private handleInvalidTransition(
    fromState: ConnectionState,
    toState: ConnectionState,
    event: string,
    errors: string[],
    allowedTransitions?: Array<{ event: string; target: ConnectionState }>
  ): TransitionValidationResult {
    const errorDetail = {
      fromState,
      toState,
      event,
      errors,
      allowedTransitions,
    };

    if (this.mode.failOnError) {
      // Test mode: throw error to fail the test
      console.error('[FSM_VALIDATION_FAILED]', JSON.stringify(errorDetail));
      throw new Error(`FSM validation failed: ${errors.join(', ')}`);
    } else {
      // Production mode: log warning but don't block
      console.warn('[FSM_VALIDATION_WARNING]', JSON.stringify(errorDetail));
    }

    return {
      valid: false,
      errors,
      allowedTransitions,
      fromState,
      toState,
      event,
    };
  }

  /**
   * Get current validation mode
   */
  getMode(): FSMValidationMode {
    return this.mode;
  }

  /**
   * Get the initial state from the state machine
   */
  getInitialState(): ConnectionState {
    return this.stateMachine.initial as ConnectionState;
  }

  /**
   * Get all valid states from the state machine
   */
  getValidStates(): ConnectionState[] {
    return Object.keys(this.stateMachine.states) as ConnectionState[];
  }

  /**
   * Get all valid transitions from a given state
   */
  getValidTransitionsFromState(state: ConnectionState): Array<{ event: string; target: ConnectionState }> {
    const stateConfig = this.stateMachine.states[state as keyof typeof this.stateMachine.states];

    if (!('on' in stateConfig)) {
      return []; // Final state - no transitions
    }

    const transitions = stateConfig.on as Record<string, { target: string }>;
    return Object.entries(transitions).map(([event, config]) => ({
      event,
      target: config.target as ConnectionState,
    }));
  }
}

/**
 * Detect validation mode based on environment
 *
 * Priority:
 * 1. FSM_VALIDATION_MODE env var (override)
 * 2. Test environment detection (NODE_ENV=test, VITEST=true, or vitest globals)
 * 3. Default to log-only (production safe)
 *
 * @returns FSMValidationMode configuration
 */
export function getFSMValidationMode(): FSMValidationMode {
  // Check for explicit override
  const envMode = process.env.FSM_VALIDATION_MODE;
  if (envMode === 'strict') {
    return { mode: 'strict', failOnError: true };
  }
  if (envMode === 'log-only') {
    return { mode: 'log-only', failOnError: false };
  }

  // Auto-detect test environment
  const isTest =
    process.env.NODE_ENV === 'test' ||
    process.env.VITEST === 'true' ||
    typeof (globalThis as any).describe === 'function';

  return isTest
    ? { mode: 'strict', failOnError: true }
    : { mode: 'log-only', failOnError: false };
}

// Singleton instance
let validatorInstance: FSMValidator | null = null;

/**
 * Get singleton validator instance
 *
 * Uses auto-detected validation mode based on environment.
 * Call resetFSMValidator() to force re-initialization with new mode.
 */
export function getFSMValidator(): FSMValidator {
  if (!validatorInstance) {
    validatorInstance = new FSMValidator(getFSMValidationMode());
  }
  return validatorInstance;
}

/**
 * Reset validator singleton (for testing)
 *
 * Forces re-initialization on next getFSMValidator() call.
 */
export function resetFSMValidator(): void {
  validatorInstance = null;
}
