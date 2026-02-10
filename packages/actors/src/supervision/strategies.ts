/**
 * Restart Strategy Implementations
 *
 * One-for-One, One-for-All, Rest-for-One restart strategies.
 * Ported from simplify/src/messaging/supervision/strategies.ts
 */

import type { MessageHandler } from '../message.ts';
import type { RestartStrategy, RestartStrategyType, SupervisionStatus } from './types.ts';

export interface RestartDecision {
  strategy: RestartStrategyType;
  actorsToRestart: string[];
  reason: string;
  shouldEscalate: boolean;
}

export interface StrategyContext {
  failedActorId: string;
  children: Map<string, MessageHandler>;
  statuses: Map<string, SupervisionStatus>;
  strategy: RestartStrategy;
  timestamp: number;
}

/** Restart only the failed child. */
export function applyOneForOne(context: StrategyContext): RestartDecision {
  const { failedActorId, statuses, strategy, timestamp } = context;

  const status = statuses.get(failedActorId);
  if (!status) {
    return {
      strategy: 'one-for-one',
      actorsToRestart: [],
      reason: `Actor ${failedActorId} not found`,
      shouldEscalate: true,
    };
  }

  const windowExpired = (timestamp - status.windowStart) > strategy.withinSeconds * 1000;
  if (windowExpired) {
    status.restartCount = 0;
    status.windowStart = timestamp;
  }

  if (status.restartCount >= strategy.maxRestarts) {
    return {
      strategy: 'one-for-one',
      actorsToRestart: [],
      reason: `${failedActorId} exceeded max restarts (${strategy.maxRestarts}/${strategy.withinSeconds}s)`,
      shouldEscalate: true,
    };
  }

  return {
    strategy: 'one-for-one',
    actorsToRestart: [failedActorId],
    reason: `Restarting ${failedActorId} (${status.restartCount + 1}/${strategy.maxRestarts})`,
    shouldEscalate: false,
  };
}

/** Restart all children when any fails. */
export function applyOneForAll(context: StrategyContext): RestartDecision {
  const { failedActorId, children, statuses, strategy, timestamp } = context;

  const status = statuses.get(failedActorId);
  if (!status) {
    return {
      strategy: 'one-for-all',
      actorsToRestart: [],
      reason: `Actor ${failedActorId} not found`,
      shouldEscalate: true,
    };
  }

  const windowExpired = (timestamp - status.windowStart) > strategy.withinSeconds * 1000;
  if (windowExpired) {
    status.restartCount = 0;
    status.windowStart = timestamp;
  }

  if (status.restartCount >= strategy.maxRestarts) {
    return {
      strategy: 'one-for-all',
      actorsToRestart: [],
      reason: `${failedActorId} exceeded max restarts`,
      shouldEscalate: true,
    };
  }

  return {
    strategy: 'one-for-all',
    actorsToRestart: Array.from(children.keys()),
    reason: `Restarting all children due to ${failedActorId} failure`,
    shouldEscalate: false,
  };
}

/** Restart failed child and all children started after it. */
export function applyRestForOne(context: StrategyContext): RestartDecision {
  const { failedActorId, children, statuses, strategy, timestamp } = context;

  const status = statuses.get(failedActorId);
  if (!status) {
    return {
      strategy: 'rest-for-one',
      actorsToRestart: [],
      reason: `Actor ${failedActorId} not found`,
      shouldEscalate: true,
    };
  }

  const windowExpired = (timestamp - status.windowStart) > strategy.withinSeconds * 1000;
  if (windowExpired) {
    status.restartCount = 0;
    status.windowStart = timestamp;
  }

  if (status.restartCount >= strategy.maxRestarts) {
    return {
      strategy: 'rest-for-one',
      actorsToRestart: [],
      reason: `${failedActorId} exceeded max restarts`,
      shouldEscalate: true,
    };
  }

  const childOrder = strategy.childOrder || Array.from(children.keys());
  const failedIndex = childOrder.indexOf(failedActorId);

  if (failedIndex === -1) {
    return {
      strategy: 'rest-for-one',
      actorsToRestart: Array.from(children.keys()),
      reason: `${failedActorId} not in child order, restarting all`,
      shouldEscalate: false,
    };
  }

  const actorsToRestart = childOrder.slice(failedIndex).filter(id => children.has(id));

  return {
    strategy: 'rest-for-one',
    actorsToRestart,
    reason: `Restarting ${failedActorId} and ${actorsToRestart.length - 1} dependents`,
    shouldEscalate: false,
  };
}

/** Dispatch to the appropriate strategy. */
export function applyRestartStrategy(context: StrategyContext): RestartDecision {
  switch (context.strategy.type) {
    case 'one-for-one':
      return applyOneForOne(context);
    case 'one-for-all':
      return applyOneForAll(context);
    case 'rest-for-one':
      return applyRestForOne(context);
    default:
      throw new Error(`Unknown restart strategy: ${context.strategy.type}`);
  }
}

/** Check if restart is allowed within limits. */
export function isRestartAllowed(
  status: SupervisionStatus,
  strategy: RestartStrategy,
  timestamp: number
): boolean {
  const windowExpired = (timestamp - status.windowStart) > strategy.withinSeconds * 1000;
  if (windowExpired) return true;
  return status.restartCount < strategy.maxRestarts;
}

/** Reset restart counter for an actor. */
export function resetRestartCounter(status: SupervisionStatus, timestamp: number): void {
  status.restartCount = 0;
  status.windowStart = timestamp;
  status.healthCheckFailures = 0;
}

/** Increment restart counter, resetting window if expired. */
export function incrementRestartCounter(
  status: SupervisionStatus,
  strategy: RestartStrategy,
  timestamp: number
): number {
  const windowExpired = (timestamp - status.windowStart) > strategy.withinSeconds * 1000;
  if (windowExpired) {
    status.restartCount = 1;
    status.windowStart = timestamp;
  } else {
    status.restartCount++;
  }
  status.lastRestart = timestamp;
  return status.restartCount;
}

/** Calculate exponential backoff delay before restart. */
export function calculateRestartBackoff(
  restartCount: number,
  baseDelay: number = 1000,
  maxDelay: number = 30000
): number {
  return Math.min(baseDelay * Math.pow(2, restartCount - 1), maxDelay);
}

/** Format restart strategy for logging. */
export function formatRestartStrategy(strategy: RestartStrategy): string {
  let description = strategy.type;
  if (strategy.childOrder) {
    description += ` (order: ${strategy.childOrder.join(' → ')})`;
  }
  description += ` [max ${strategy.maxRestarts}/${strategy.withinSeconds}s]`;
  return description;
}
