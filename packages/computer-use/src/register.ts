/**
 * register.ts — installs system/screenshot actor into a compatible UGS router.
 *
 * Deliberately does NOT import from ugs/ — the router is duck-typed so that
 * computer-use has no circular dependency on the UGS package.
 *
 * Usage:
 *   import { register } from '@agentic-primer/computer-use/register';
 *   register(router);                            // registers at 'system/screenshot'
 *   register(router, { path: 'tools/screen' }); // custom path
 */

import { ScreenshotActor } from "./screenshot-actor.js";
import type { NodeAttachFn } from "./screenshot-actor.js";

export type { NodeAttachFn };

export interface RegisterOptions {
  /** Path to register the actor at (default: 'system/screenshot') */
  path?: string;
  /** Optional callback to attach captured artifacts to graph nodes */
  attachFn?: NodeAttachFn;
}

/**
 * Register a ScreenshotActor on the provided router.
 *
 * The router parameter is duck-typed: any object that has
 * `registerActor(path: string, actor: any): void` is compatible.
 * This includes MessageRouter from @agentic-primer/ugs.
 */
export function register(
  router: { registerActor(path: string, actor: any): void },
  options: RegisterOptions = {},
): void {
  const actor = new ScreenshotActor({ attachFn: options.attachFn });
  router.registerActor(options.path ?? 'system/screenshot', actor);
}
