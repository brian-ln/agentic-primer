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
 * Address - first-class object with identity and behavior
 * Addresses are returned by actor factories and have a built-in .send() method
 */
export type Address = {
  readonly __id: symbol; // Internal unique identifier
  send: (message: Message) => Promise<Response>;
};

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
 * @param data - Actor's initial data/state (must include system: System)
 * @returns Address - First-class address with .send() method
 */
export type ActorFactory<TData> = (data: TData) => Address;
