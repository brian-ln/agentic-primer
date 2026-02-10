/**
 * Worker-to-Worker Transport
 *
 * Implements ITransport for communication between Cloudflare Workers
 * via service bindings. Messages are sent as JSON POST requests to
 * the /actor-message endpoint on the target worker.
 *
 * Simpler than DOTransport — just forwards messages through
 * the service binding's Fetcher interface.
 */

import type { ITransport, ISerde, ConnectionState } from '@agentic-primer/actors';
import { JsonSerde } from '@agentic-primer/actors';

/** Configuration for WorkerTransport */
export interface WorkerTransportConfig {
  /** Service binding (e.g., env.SIGNAL_HUB) */
  binding: Fetcher;
  /** Serde for message serialization. Defaults to JsonSerde. */
  serde?: ISerde;
}

export class WorkerTransport implements ITransport {
  private readonly config: WorkerTransportConfig;
  private readonly serde: ISerde;
  private _state: ConnectionState = 'disconnected';
  private receiveHandler?: (sender: string, message: unknown) => void;

  constructor(config: WorkerTransportConfig) {
    this.config = config;
    this.serde = config.serde ?? new JsonSerde();
  }

  get state(): ConnectionState {
    return this._state;
  }

  async connect(_remoteAddress: string): Promise<void> {
    this._state = 'connecting';
    // Service bindings are always available — no actual connection needed.
    this._state = 'connected';
  }

  async disconnect(): Promise<void> {
    this._state = 'disconnecting';
    this._state = 'disconnected';
  }

  /**
   * Send a message to a Worker via service binding.
   * POST to /actor-message with JSON body.
   */
  async send(recipient: string, message: unknown): Promise<void> {
    const body = JSON.stringify(message);

    const response = await this.config.binding.fetch(
      'https://worker-internal/actor-message',
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body,
      }
    );

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(
        `WorkerTransport: Failed to deliver message to ${recipient}: ${response.status} ${errorText}`
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
}
