/**
 * Core type definitions for the actor system
 * Following ACTOR_SPEC.md
 */

/**
 * Message sent between actors
 */
export interface Message {
  id: string;
  type: string;
  payload: unknown;
}

/**
 * Response from an actor
 */
export interface Response {
  success: boolean;
  data?: unknown;
  error?: string;
}

/**
 * Actor address - flexible addressing for future evolution
 * Currently string, but can be extended (PIDs, UUIDs, etc.)
 */
export type Address = string;

/**
 * Actor interface - has a send method for receiving messages
 */
export interface Actor {
  send: (message: Message) => Promise<Response>;
}

/**
 * Send function - the core messaging primitive
 * Actors use this to send messages to other actors by address
 */
export type SendFunction = (
  targetAddress: Address,
  message: Message
) => Promise<Response>;

/**
 * Actor factory signature - pure function that creates actors
 * @param data - Actor's initial data/state
 * @param send - Injected send function for actor-to-actor communication
 */
export type ActorFactory<TData> = (data: TData, send: SendFunction) => Actor;
