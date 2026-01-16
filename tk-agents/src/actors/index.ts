/**
 * Actor System - Public API
 *
 * Core Principles:
 * 1. Addresses are first-class objects - identity (__id) and behavior (.send())
 * 2. Actor factories return addresses - single call creates, registers, returns
 * 3. Two send styles work:
 *    - Ergonomic: address.send(message)
 *    - Explicit: system.send(address, message)
 * 4. System in data - pure functions with explicit dependencies
 * 5. No magic strings - type-safe addresses
 */

export type { Actor, Address, Message, Response, SendFunction, ActorFactory } from "./base.ts";
export type { System } from "./system.ts";
export { System } from "./system.ts";
