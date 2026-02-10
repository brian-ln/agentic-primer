/**
 * Supervision Types for Actor System
 *
 * Erlang/OTP-inspired supervision hierarchy, restart strategies,
 * lifecycle hooks, and error handling.
 *
 * Ported from simplify/src/messaging/supervision/types.ts
 * Decoupled from concrete Actor class - uses MessageHandler interface.
 */

import type { MessageHandler, Message } from '../message.ts';

export type RestartStrategyType = 'one-for-one' | 'one-for-all' | 'rest-for-one';

export interface RestartStrategy {
  type: RestartStrategyType;
  /** Max restart attempts within time window. @default 3 */
  maxRestarts: number;
  /** Time window in seconds for restart counting. @default 60 */
  withinSeconds: number;
  /** Child start order for rest-for-one strategy. */
  childOrder?: string[];
}

export type ErrorSeverity = 'transient' | 'permanent' | 'fatal';

export interface ErrorClassification {
  severity: ErrorSeverity;
  shouldRestart: boolean;
  escalate: boolean;
  reason?: string;
}

export type ErrorClassifier = (
  error: Error,
  actor: MessageHandler,
  message?: Message
) => ErrorClassification;

export interface HealthCheckConfig {
  /** Interval between checks in ms. @default 30000 */
  interval: number;
  /** Timeout per check in ms. @default 5000 */
  timeout: number;
  /** Consecutive failures before restart. @default 3 */
  threshold: number;
  method?: 'ping' | 'state' | 'custom';
  customCheck?: (actor: MessageHandler) => Promise<boolean>;
}

export interface SupervisionOptions {
  strategy?: RestartStrategy;
  healthCheck?: HealthCheckConfig;
  errorClassifier?: ErrorClassifier;
  autoStart?: boolean;
  preRestartTimeout?: number;
  postRestartTimeout?: number;
}

export type ChildStatus = 'starting' | 'running' | 'restarting' | 'suspended' | 'terminated';

export interface SupervisionStatus {
  actorId: string;
  status: ChildStatus;
  restartCount: number;
  windowStart: number;
  healthCheckFailures: number;
  lastError?: Error;
  lastRestart?: number;
  lastHealthCheck?: number;
}

export interface RestartEvent {
  actorId: string;
  timestamp: number;
  error: Error;
  message?: Message;
  strategy: RestartStrategyType;
  success: boolean;
  duration: number;
  checkpoint?: unknown;
}

export interface EscalationEvent {
  childId: string;
  supervisorId: string;
  parentId: string;
  timestamp: number;
  error: Error;
  reason: 'max-restarts-exceeded' | 'permanent-error' | 'fatal-error' | 'supervisor-failure';
  restartCount: number;
}

/** Lifecycle hooks for supervised actors. */
export interface SupervisedActorHooks {
  preRestart?(error: Error, message?: Message): Promise<unknown | void>;
  postRestart?(checkpoint?: unknown): Promise<void>;
  healthCheck?(): Promise<boolean>;
}

export interface SupervisionMetrics {
  totalRestarts: number;
  restartsByActor: Map<string, number>;
  restartsByStrategy: Map<RestartStrategyType, number>;
  escalationCount: number;
  averageRestartTime: number;
  healthCheckFailures: number;
  activeRestarts: number;
  suspendedActors: number;
}

export const DEFAULT_RESTART_STRATEGY: RestartStrategy = {
  type: 'one-for-one',
  maxRestarts: 3,
  withinSeconds: 60,
};

export const DEFAULT_HEALTH_CHECK: HealthCheckConfig = {
  interval: 30000,
  timeout: 5000,
  threshold: 3,
  method: 'ping',
};

export const defaultErrorClassifier: ErrorClassifier = (
  error: Error,
  _actor: MessageHandler,
  _message?: Message
): ErrorClassification => {
  if (error.name === 'NetworkError' || error.message.includes('ECONNREFUSED')) {
    return { severity: 'transient', shouldRestart: true, escalate: false, reason: 'Network failure' };
  }
  if (error.name === 'TimeoutError' || error.message.includes('timed out')) {
    return { severity: 'transient', shouldRestart: true, escalate: false, reason: 'Timeout' };
  }
  if (error.name === 'ConfigError' || error.message.includes('configuration')) {
    return { severity: 'permanent', shouldRestart: false, escalate: true, reason: 'Configuration error' };
  }
  if (error.name === 'OutOfMemoryError' || error.message.includes('out of memory')) {
    return { severity: 'fatal', shouldRestart: false, escalate: true, reason: 'Out of memory' };
  }
  if (error instanceof TypeError) {
    return { severity: 'permanent', shouldRestart: false, escalate: true, reason: 'Type error (likely bug)' };
  }
  return { severity: 'transient', shouldRestart: true, escalate: false, reason: 'Unknown error (treating as transient)' };
};
