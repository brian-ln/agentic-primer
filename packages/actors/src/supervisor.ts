/**
 * SupervisorBase - Hierarchical path-based message routing
 *
 * Routes messages to children based on hierarchical paths.
 * Ported from simplify/src/messaging/supervisor-base.ts
 */

import { Actor } from './actor.ts';
import type { IMessageRouter } from './interfaces.ts';
import type {
  Message,
  MessageResponse,
  MessageHandler,
} from './message.ts';
import {
  address,
  parseAddress,
  createResponse,
  createErrorResponse,
} from './message.ts';
import { parsePath, validatePath } from './path-resolver.ts';

/**
 * Supervisor that routes messages hierarchically by path delegation.
 *
 * Routing logic:
 * 1. Single segment matching local name → handle locally
 * 2. Remaining segments → delegate to child
 * 3. Child not found → return error
 */
export class SupervisorBase extends Actor {
  protected children = new Map<string, MessageHandler>();
  protected localName: string;
  protected fullPath: string;

  constructor(localName: string, router: IMessageRouter) {
    super(localName, router);
    this.localName = localName;
    this.fullPath = localName;
  }

  addChild(childName: string, child: MessageHandler): void {
    if (!validatePath(childName)) {
      throw new Error(`Invalid child name: ${childName}`);
    }
    this.children.set(childName, child);
  }

  removeChild(childName: string): boolean {
    const removed = this.children.delete(childName);
    if (removed) {
      this.router.invalidatePath(`${this.fullPath}/${childName}`);
    }
    return removed;
  }

  getChild(childName: string): MessageHandler | undefined {
    return this.children.get(childName);
  }

  getChildren(): Map<string, MessageHandler> {
    return new Map(this.children);
  }

  async receive(message: Message): Promise<MessageResponse> {
    const targetPath = parseAddress(message.to);
    const segments = parsePath(targetPath);

    if (!validatePath(targetPath)) {
      return createErrorResponse(message, `Invalid path: ${targetPath}`);
    }

    if (segments.length === 0) {
      return createErrorResponse(message, 'Invalid empty path');
    }

    // Message for this supervisor
    if (segments.length === 1 && segments[0] === this.localName) {
      return this.handleMessage(message);
    }

    // Delegate to child
    const [firstSegment, ...remainingSegments] = segments;
    const actualChildName =
      firstSegment === this.localName && remainingSegments.length > 0
        ? remainingSegments[0]
        : firstSegment;

    const child = this.children.get(actualChildName);

    if (!child) {
      const available = Array.from(this.children.keys()).join(', ');
      return createErrorResponse(
        message,
        `Child not found: ${actualChildName} (available: ${available || 'none'})`
      );
    }

    const childPath =
      firstSegment === this.localName && remainingSegments.length > 0
        ? remainingSegments.slice(1).join('/')
        : remainingSegments.join('/');

    const childMessage: Message = {
      ...message,
      to: address(childPath || actualChildName),
    };

    if (!childPath || childPath === actualChildName) {
      this.router.cacheActor(`${this.fullPath}/${actualChildName}`, child);
    }

    return await child.receive(childMessage);
  }

  /** Override to handle messages addressed to this supervisor. */
  protected async handleMessage(message: Message): Promise<MessageResponse> {
    return createResponse(message, {
      supervisor: this.localName,
      children: Array.from(this.children.keys()),
    });
  }
}

/**
 * LeafActor - Endpoint in hierarchy with custom behavior function.
 */
export class LeafActor extends Actor {
  protected localName: string;
  protected behavior: (message: Message) => Promise<unknown>;

  constructor(
    localName: string,
    router: IMessageRouter,
    behavior: (message: Message) => Promise<unknown>
  ) {
    super(localName, router);
    this.localName = localName;
    this.behavior = behavior;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      const result = await this.behavior(message);
      return createResponse(message, result);
    } catch (error: unknown) {
      return createErrorResponse(
        message,
        error instanceof Error ? error.message : String(error)
      );
    }
  }
}
