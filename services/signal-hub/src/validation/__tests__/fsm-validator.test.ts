/**
 * FSM Validator Tests
 *
 * Tests for runtime FSM validation:
 * - Strict mode (test environment)
 * - Log-only mode (production environment)
 * - All 8 valid transitions from state-machine.json
 * - Invalid transitions
 * - Guard condition documentation
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  FSMValidator,
  getFSMValidationMode,
  getFSMValidator,
  resetFSMValidator,
  type FSMValidationMode,
} from '../fsm-validator';
import type { ConnectionState } from '../../types';

describe('FSM Validator', () => {
  afterEach(() => {
    resetFSMValidator();
  });

  describe('getFSMValidationMode', () => {
    it('should detect test environment and return strict mode', () => {
      const mode = getFSMValidationMode();
      expect(mode.mode).toBe('strict');
      expect(mode.failOnError).toBe(true);
    });

    it('should respect FSM_VALIDATION_MODE=strict override', () => {
      const originalEnv = process.env.FSM_VALIDATION_MODE;
      process.env.FSM_VALIDATION_MODE = 'strict';

      const mode = getFSMValidationMode();
      expect(mode.mode).toBe('strict');
      expect(mode.failOnError).toBe(true);

      process.env.FSM_VALIDATION_MODE = originalEnv;
    });

    it('should respect FSM_VALIDATION_MODE=log-only override', () => {
      const originalEnv = process.env.FSM_VALIDATION_MODE;
      process.env.FSM_VALIDATION_MODE = 'log-only';

      const mode = getFSMValidationMode();
      expect(mode.mode).toBe('log-only');
      expect(mode.failOnError).toBe(false);

      process.env.FSM_VALIDATION_MODE = originalEnv;
    });
  });

  describe('FSMValidator - State Machine Metadata', () => {
    let validator: FSMValidator;

    beforeEach(() => {
      validator = new FSMValidator({ mode: 'strict', failOnError: true });
    });

    it('should return correct initial state', () => {
      expect(validator.getInitialState()).toBe('connecting');
    });

    it('should return all valid states', () => {
      const states = validator.getValidStates();
      expect(states).toEqual(['connecting', 'connected', 'disconnecting', 'disconnected']);
    });

    it('should return valid transitions from connecting state', () => {
      const transitions = validator.getValidTransitionsFromState('connecting');
      expect(transitions).toHaveLength(3);
      expect(transitions).toContainEqual({ event: 'hub:connect_success', target: 'connected' });
      expect(transitions).toContainEqual({ event: 'hub:connect_fail', target: 'disconnected' });
      expect(transitions).toContainEqual({ event: 'websocket_close', target: 'disconnected' });
    });

    it('should return valid transitions from connected state', () => {
      const transitions = validator.getValidTransitionsFromState('connected');
      expect(transitions).toHaveLength(3);
      expect(transitions).toContainEqual({ event: 'hub:disconnect', target: 'disconnecting' });
      expect(transitions).toContainEqual({ event: 'websocket_close', target: 'disconnected' });
      expect(transitions).toContainEqual({ event: 'heartbeat_timeout', target: 'disconnected' });
    });

    it('should return valid transitions from disconnecting state', () => {
      const transitions = validator.getValidTransitionsFromState('disconnecting');
      expect(transitions).toHaveLength(2);
      expect(transitions).toContainEqual({ event: 'websocket_close', target: 'disconnected' });
      expect(transitions).toContainEqual({ event: 'cleanup_timeout', target: 'disconnected' });
    });

    it('should return empty array for disconnected final state', () => {
      const transitions = validator.getValidTransitionsFromState('disconnected');
      expect(transitions).toHaveLength(0);
    });
  });

  describe('FSMValidator - Valid Transitions (All 8 from state-machine.json)', () => {
    let validator: FSMValidator;

    beforeEach(() => {
      validator = new FSMValidator({ mode: 'strict', failOnError: true });
    });

    it('Transition 1: connecting → connected (hub:connect_success)', () => {
      const result = validator.isValidTransition('connecting', 'connected', 'hub:connect_success');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('Transition 2: connecting → disconnected (hub:connect_fail)', () => {
      const result = validator.isValidTransition('connecting', 'disconnected', 'hub:connect_fail');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('Transition 3: connecting → disconnected (websocket_close)', () => {
      const result = validator.isValidTransition('connecting', 'disconnected', 'websocket_close');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('Transition 4: connected → disconnecting (hub:disconnect)', () => {
      const result = validator.isValidTransition('connected', 'disconnecting', 'hub:disconnect');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('Transition 5: connected → disconnected (websocket_close)', () => {
      const result = validator.isValidTransition('connected', 'disconnected', 'websocket_close');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('Transition 6: connected → disconnected (heartbeat_timeout)', () => {
      const result = validator.isValidTransition('connected', 'disconnected', 'heartbeat_timeout');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('Transition 7: disconnecting → disconnected (websocket_close)', () => {
      const result = validator.isValidTransition('disconnecting', 'disconnected', 'websocket_close');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('Transition 8: disconnecting → disconnected (cleanup_timeout)', () => {
      const result = validator.isValidTransition('disconnecting', 'disconnected', 'cleanup_timeout');
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });
  });

  describe('FSMValidator - Invalid Transitions (Strict Mode)', () => {
    let validator: FSMValidator;

    beforeEach(() => {
      validator = new FSMValidator({ mode: 'strict', failOnError: true });
    });

    it('should throw on invalid transition: disconnected → connected', () => {
      expect(() => {
        validator.isValidTransition('disconnected', 'connected', 'hub:connect_success');
      }).toThrow(/FSM validation failed/);
    });

    it('should throw on invalid transition: connecting → connecting', () => {
      expect(() => {
        validator.isValidTransition('connecting', 'connecting', 'hub:connect_success');
      }).toThrow(/FSM validation failed/);
    });

    it('should throw on invalid event for state: connected → connected (invalid event)', () => {
      expect(() => {
        validator.isValidTransition('connected', 'connected', 'hub:connect_success');
      }).toThrow(/FSM validation failed/);
    });

    it('should throw on wrong target state for event: connecting → disconnecting (hub:connect_success)', () => {
      expect(() => {
        validator.isValidTransition('connecting', 'disconnecting', 'hub:connect_success');
      }).toThrow(/FSM validation failed/);
    });

    it('should throw on unknown source state', () => {
      expect(() => {
        validator.isValidTransition('unknown' as ConnectionState, 'connected', 'hub:connect_success');
      }).toThrow(/Unknown source state/);
    });

    it('should throw on transition from final state: disconnected → anything', () => {
      expect(() => {
        validator.isValidTransition('disconnected', 'connecting', 'websocket_open');
      }).toThrow(/final state/);
    });

    it('should provide allowed transitions in error when wrong event used', () => {
      expect(() => {
        validator.isValidTransition('connected', 'disconnected', 'hub:connect_success');
      }).toThrow(/Event 'hub:connect_success' not allowed in state 'connected'/);
    });
  });

  describe('FSMValidator - Log-Only Mode', () => {
    let validator: FSMValidator;

    beforeEach(() => {
      validator = new FSMValidator({ mode: 'log-only', failOnError: false });
    });

    it('should not throw on invalid transition in log-only mode', () => {
      expect(() => {
        validator.isValidTransition('disconnected', 'connected', 'hub:connect_success');
      }).not.toThrow();
    });

    it('should return validation result with errors in log-only mode', () => {
      const result = validator.isValidTransition('disconnected', 'connected', 'hub:connect_success');

      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
      expect(result.errors!.length).toBeGreaterThan(0);
    });

    it('should still validate correct transitions in log-only mode', () => {
      const result = validator.isValidTransition('connecting', 'connected', 'hub:connect_success');

      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('should include allowed transitions in error result for log-only mode', () => {
      const result = validator.isValidTransition('connected', 'connected', 'invalid_event');

      expect(result.valid).toBe(false);
      expect(result.allowedTransitions).toBeDefined();
      expect(result.allowedTransitions!.length).toBe(3); // connected has 3 valid transitions
    });
  });

  describe('Guard Conditions Documentation', () => {
    let validator: FSMValidator;

    beforeEach(() => {
      validator = new FSMValidator({ mode: 'strict', failOnError: true });
    });

    it('Transition 1: hub:connect_success requires validJWT && versionMatch guards', () => {
      // This test documents the guard conditions for hub:connect_success
      // Guards are checked in the handler, not in FSM validator
      const result = validator.isValidTransition('connecting', 'connected', 'hub:connect_success');
      expect(result.valid).toBe(true);
      // Note: Actual guard checking happens in handleConnect handler
    });

    it('Transition 2: hub:connect_fail requires invalidJWT || versionMismatch guards', () => {
      // This test documents the guard conditions for hub:connect_fail
      const result = validator.isValidTransition('connecting', 'disconnected', 'hub:connect_fail');
      expect(result.valid).toBe(true);
      // Note: Actual guard checking happens in handleConnect handler
    });

    it('Transition 6: heartbeat_timeout requires noHeartbeatFor60s guard', () => {
      // This test documents the guard condition for heartbeat_timeout
      const result = validator.isValidTransition('connected', 'disconnected', 'heartbeat_timeout');
      expect(result.valid).toBe(true);
      // Note: Actual guard checking happens in heartbeat monitor (not yet implemented)
    });

    it('Transition 8: cleanup_timeout requires cleanupTookTooLong guard', () => {
      // This test documents the guard condition for cleanup_timeout
      const result = validator.isValidTransition('disconnecting', 'disconnected', 'cleanup_timeout');
      expect(result.valid).toBe(true);
      // Note: Actual guard checking happens in cleanup monitor (not yet implemented)
    });
  });

  describe('Singleton Validator', () => {
    it('should return the same instance on multiple getFSMValidator calls', () => {
      const validator1 = getFSMValidator();
      const validator2 = getFSMValidator();

      expect(validator1).toBe(validator2);
    });

    it('should create new instance after resetFSMValidator', () => {
      const validator1 = getFSMValidator();
      resetFSMValidator();
      const validator2 = getFSMValidator();

      expect(validator1).not.toBe(validator2);
    });
  });
});
