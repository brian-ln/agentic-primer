/**
 * Actor system types.
 *
 * Merged type system:
 * - simplify's Address, Message, MessageHandler are in message.ts (the foundation)
 * - This file adds: functional behaviors (from brianln.ai), system config,
 *   and simple supervision directives for the ActorSystem layer.
 */

import type { Address, Message, MessageResponse, MessageHandler } from './message.ts';

/**
 * Functional actor behavior - processes messages and returns new state.
 * From brianln.ai's functional pattern, complementing simplify's class-based Actor.
 */
export type ActorBehavior<State = any, Protocol = any> = (
  state: State,
  message: Message<Protocol>,
  context: ActorContext
) => State | Promise<State>;

/**
 * Context provided to functional behaviors.
 */
export interface ActorContext {
  self: Address;
  send: (to: Address, type: string, payload: unknown) => void;
  ask: <R>(to: Address, type: string, payload: unknown, timeout?: number) => Promise<R>;
  spawn: <S, P>(behavior: ActorBehavior<S, P>, initialState: S, name?: string) => Address;
  stop: (addr: Address) => void;
}

/**
 * Internal actor instance for functional behaviors.
 * Tracks state, mailbox, and processing status.
 */
export interface ActorInstance<State = any, Protocol = any> {
  address: Address;
  state: State;
  behavior: ActorBehavior<State, Protocol>;
  mailbox: Array<Message<Protocol>>;
  processing: boolean;
  supervisor?: Address;
}

/**
 * Simple supervision directive for the ActorSystem layer.
 * For detailed supervision, use supervision/types.ts instead.
 */
export type SupervisionDirective = 'Resume' | 'Restart' | 'Stop' | 'Escalate';

/**
 * Simple supervision strategy for ActorSystem.
 */
export interface SupervisionStrategy {
  onFailure: (actorAddr: Address, error: Error, message: Message) => SupervisionDirective;
}

/**
 * Actor system configuration.
 */
export interface ActorSystemConfig {
  name: string;
  supervisionStrategy?: SupervisionStrategy;
  deadLetterQueueSize?: number;
}

/**
 * Dead letter queue entry.
 */
export interface DeadLetterEntry {
  address: Address;
  message: Message;
  error: Error;
  timestamp: number;
}
