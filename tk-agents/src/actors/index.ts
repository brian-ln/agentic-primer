/**
 * Actor System - Public API
 *
 * Core Principles:
 * 1. Actors are pure functions - all dependencies explicit
 * 2. Send is the only primitive - send(targetId, message)
 * 3. Two APIs for bridging:
 *    - External: actor.send(message)
 *    - Internal: send(targetId, message)
 * 4. Systems ARE actors - uniform composition
 */

export type { Actor, Message, Response, SendFunction, ActorFactory } from "./base.ts";
export type { System } from "./system.ts";
export { createSystem } from "./system.ts";
