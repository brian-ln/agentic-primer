/**
 * UIActor - Web Component base class with actor behavior
 *
 * Integrates:
 * - Actor model (mailbox, sequential message processing)
 * - TC39 Signals (reactive state via @preact/signals-core)
 * - Web Components (custom elements with shadow DOM)
 * - lit-html (fine-grained DOM updates)
 * - Service discovery (declarative service dependencies)
 *
 * Ported from brianln.ai/src/actors/UIActor.ts
 * Adapted to use @agentic-primer/actors types (Address, Message, ActorSystem)
 */

import { signal, effect, computed, type Signal, type ReadonlySignal } from '@preact/signals-core';
import { html as litHtml, render as litRender, type TemplateResult } from 'lit-html';
import type {
  Address,
  Message,
  ActorBehavior,
  ActorSystem,
} from '@agentic-primer/actors';

// Module-level actor system reference (set via setActorSystem)
let _actorSystem: ActorSystem | null = null;

/**
 * Set the global actor system for UIActors.
 * Must be called before any UIActor is connected to the DOM.
 */
export function setActorSystem(system: ActorSystem): void {
  _actorSystem = system;
}

/**
 * Get the global actor system. Throws if not set.
 */
export function getActorSystem(): ActorSystem {
  if (!_actorSystem) {
    throw new Error('ActorSystem not initialized. Call setActorSystem() first.');
  }
  return _actorSystem;
}

/**
 * Message envelope for the UIActor mailbox.
 */
interface MailboxEntry<Protocol> {
  type: string;
  payload: Protocol;
  timestamp: number;
}

/**
 * Base class for UI actors.
 * Extend this to create reactive Web Components that are also actors.
 *
 * @example
 * ```typescript
 * class CounterElement extends UIActor<{ count: number }, CounterProtocol> {
 *   static services = ['counter-service'];
 *
 *   constructor() {
 *     super({ count: 0 }, 'counter-ui');
 *   }
 *
 *   protected handle(message: CounterProtocol): void {
 *     if (message.type === 'STATE_UPDATE') {
 *       this.setState({ count: message.payload.count });
 *     }
 *   }
 *
 *   protected render(): TemplateResult {
 *     return html`<span>${this.getState().count}</span>`;
 *   }
 * }
 * customElements.define('counter-element', CounterElement);
 * ```
 */
export abstract class UIActor<State, Protocol> extends HTMLElement {
  /** Actor address in the system. Set after initialization. */
  public actorAddress!: Address;

  /** Reactive state signal */
  protected readonly state: Signal<State>;

  /** Mailbox for sequential message processing */
  private mailbox: Array<MailboxEntry<Protocol>> = [];
  private processing = false;
  private initialized = false;

  /** Signal utilities exposed to subclasses */
  protected computed = computed;
  protected signal = signal;

  private initialState: State;
  private actorName?: string;

  /** Bound service actor addresses */
  protected boundServices = new Map<string, Address>();

  /** Declarative service dependencies (override in subclass) */
  static services: string[] = [];

  constructor(initialState: State, name?: string) {
    super();
    this.state = signal(initialState);
    this.initialState = initialState;
    this.actorName = name;
    this.attachShadow({ mode: 'open' });
  }

  /**
   * Initialize actor system connection.
   * Called from connectedCallback after ActorSystem is guaranteed to exist.
   */
  private initializeActor(): void {
    if (this.initialized) return;

    try {
      const system = getActorSystem();

      // Spawn a functional behavior that delegates to this.receive()
      this.actorAddress = system.spawn(
        async (state: State, message: Message, context) => {
          await this.receive(message.payload as Protocol);
          return this.state.value;
        },
        this.initialState,
        this.actorName
      );

      this.initialized = true;

      // Request declared service dependencies
      const constructor = this.constructor as typeof UIActor;
      const declaredServices = constructor.services || [];

      for (const serviceName of declaredServices) {
        const actorId = this.actorName || '';
        system.requestService(serviceName, actorId, (domainAddr: Address) => {
          this.boundServices.set(serviceName, domainAddr);
          this.onServiceBound(serviceName, domainAddr);
        });
      }

      this.onConnected();
    } catch {
      // ActorSystem not ready yet, retry
      queueMicrotask(() => this.initializeActor());
    }
  }

  /** Add message to mailbox and process sequentially */
  async receive(message: Protocol): Promise<void> {
    const envelope: MailboxEntry<Protocol> = {
      type: typeof message === 'object' && message !== null && 'type' in message
        ? (message as any).type
        : 'MESSAGE',
      payload: message,
      timestamp: Date.now(),
    };

    this.mailbox.push(envelope);
    await this.processMailbox();
  }

  private async processMailbox(): Promise<void> {
    if (this.processing) return;
    this.processing = true;

    while (this.mailbox.length > 0) {
      const entry = this.mailbox.shift()!;
      try {
        await this.handle(entry.payload);
      } catch (error) {
        this.onError(error as Error, entry);
      }
    }

    this.processing = false;
  }

  /** Send message to another actor */
  protected sendMessage(to: Address, type: string, payload: unknown): void {
    const system = getActorSystem();
    system.send(to, type, payload);
  }

  /** Send message and wait for response */
  protected async askActor<R>(
    to: Address,
    type: string,
    payload: unknown,
    timeout?: number
  ): Promise<R> {
    const system = getActorSystem();
    return system.ask<R>(to, type, payload, timeout);
  }

  /** Update reactive state */
  protected setState(updates: Partial<State> | ((prev: State) => State)): void {
    if (typeof updates === 'function') {
      this.state.value = updates(this.state.value);
    } else {
      this.state.value = { ...this.state.value, ...updates };
    }
  }

  /** Read current state */
  protected getState(): State {
    return this.state.value;
  }

  // --- Web Component Lifecycle ---

  connectedCallback(): void {
    this.initializeActor();

    effect(() => {
      const template = this.render();
      if (this.shadowRoot) {
        litRender(template, this.shadowRoot);
        this.attachEventListeners();
      }
    });
  }

  disconnectedCallback(): void {
    this.boundServices.clear();
    this.onDisconnected();

    if (this.initialized) {
      const system = getActorSystem();
      system.stop(this.actorAddress);
    }
  }

  // --- Abstract methods (subclass must implement) ---

  /** Handle actor messages */
  protected abstract handle(message: Protocol): Promise<void> | void;

  /** Render UI using lit-html templates */
  protected abstract render(): TemplateResult;

  // --- Optional overrides ---

  /** Attach DOM event listeners after render */
  protected attachEventListeners(): void {}

  /** Called when connected to DOM and actor system */
  protected onConnected(): void {}

  /** Called when a declared service is bound */
  protected onServiceBound(serviceName: string, addr: Address): void {}

  /** Called when disconnected from DOM */
  protected onDisconnected(): void {}

  /** Called on message handling error */
  protected onError(error: Error, entry: MailboxEntry<Protocol>): void {
    console.error(`[${this.actorAddress}] Error handling message:`, error);
  }

  // --- DOM helpers ---

  protected $(selector: string): HTMLElement | null {
    return this.shadowRoot?.querySelector(selector) || null;
  }

  protected $$(selector: string): NodeListOf<Element> {
    return this.shadowRoot?.querySelectorAll(selector) || ([] as unknown as NodeListOf<Element>);
  }
}

// Re-export signal utilities for subclasses
export { signal, computed, effect, litHtml as html, litRender as render };
export type { Signal, ReadonlySignal, TemplateResult };
