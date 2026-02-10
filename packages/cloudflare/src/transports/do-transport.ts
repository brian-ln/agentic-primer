/**
 * Inter-DO Transport
 *
 * Implements ITransport for communication between Durable Objects.
 * Messages are sent by obtaining a DO stub and calling stub.fetch()
 * with the serialized actor message as a JSON POST body.
 *
 * Supports three ID strategies:
 * - 'singleton': All messages go to a single DO instance (e.g., Brain)
 * - 'per-entity': Each entity gets its own DO instance (e.g., per-user agents)
 * - custom function: Caller provides (address) => DurableObjectId mapping
 */

import type { ITransport, ISerde, ConnectionState } from '@agentic-primer/actors';
import { JsonSerde } from '@agentic-primer/actors';
import { parseDOAddress } from '../types.ts';

/** Configuration for DOTransport */
export interface DOTransportConfig {
  /** DO namespace binding (e.g., env.BRAIN) */
  namespace: DurableObjectNamespace;
  /** How to derive DO ID from actor address */
  idStrategy: 'singleton' | 'per-entity' | ((address: string) => DurableObjectId);
  /** Serde for message serialization. Defaults to JsonSerde. */
  serde?: ISerde;
}

export class DOTransport implements ITransport {
  private readonly config: DOTransportConfig;
  private readonly serde: ISerde;
  private _state: ConnectionState = 'disconnected';
  private receiveHandler?: (sender: string, message: unknown) => void;

  constructor(config: DOTransportConfig) {
    this.config = config;
    this.serde = config.serde ?? new JsonSerde();
  }

  get state(): ConnectionState {
    return this._state;
  }

  async connect(_remoteAddress: string): Promise<void> {
    this._state = 'connecting';
    // DO transport is connectionless — connection is implicit on first send.
    // Mark as connected immediately.
    this._state = 'connected';
  }

  async disconnect(): Promise<void> {
    this._state = 'disconnecting';
    this._state = 'disconnected';
  }

  /**
   * Send a message to a Durable Object actor.
   *
   * 1. Parse the recipient address to extract DO identity + actor path
   * 2. Derive DurableObjectId using the configured strategy
   * 3. Get DO stub and POST the message to /actor-message
   */
  async send(recipient: string, message: unknown): Promise<void> {
    const { doName } = parseDOAddress(recipient);
    const id = this.resolveId(doName, recipient);
    const stub = this.config.namespace.get(id);

    const body = JSON.stringify(message);
    const response = await stub.fetch('https://do-internal/actor-message', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body,
    });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(
        `DOTransport: Failed to deliver message to ${recipient}: ${response.status} ${errorText}`
      );
    }

    // Mark connected after first successful send
    if (this._state !== 'connected') {
      this._state = 'connected';
    }
  }

  onReceive(handler: (sender: string, message: unknown) => void): void {
    this.receiveHandler = handler;
  }

  /**
   * Resolve a DurableObjectId from the DO name using the configured strategy.
   */
  private resolveId(doName: string, fullAddress: string): DurableObjectId {
    const strategy = this.config.idStrategy;

    if (typeof strategy === 'function') {
      return strategy(fullAddress);
    }

    switch (strategy) {
      case 'singleton':
        return this.config.namespace.idFromName(doName);
      case 'per-entity':
        return this.config.namespace.idFromName(doName);
      default:
        throw new Error(`DOTransport: Unknown ID strategy: ${strategy}`);
    }
  }
}
