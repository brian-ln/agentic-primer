/**
 * Browser Actor Test Wrapper
 *
 * Wrapper around SignalHubClient from packages/signal-hub-client/ for integration tests.
 * Uses the browser client in a Node.js environment (compatible with ws library).
 */

import { SignalHubClient } from '../../../../packages/signal-hub-client/src/SignalHubClient.js';
import type { SignalHubClientOptions } from '../../../../packages/signal-hub-client/src/types.js';
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';
import { WebSocket } from 'ws';

// Polyfill WebSocket for Node.js environment
if (typeof globalThis.WebSocket === 'undefined') {
  (globalThis as any).WebSocket = WebSocket;
}

export interface BrowserActorOptions {
  url: string;
  jwt: string;
  actorAddress: CanonicalAddress;
  capabilities: string[];
  metadata?: Record<string, unknown>;
}

export class BrowserActorWrapper {
  private client: SignalHubClient;
  private actorAddress: CanonicalAddress;
  private capabilities: string[];
  private metadata: Record<string, unknown>;
  private receivedMessages: SharedMessage[] = [];
  private messageHandlers: Array<(msg: SharedMessage) => void> = [];

  constructor(options: BrowserActorOptions) {
    this.actorAddress = options.actorAddress;
    this.capabilities = options.capabilities;
    this.metadata = options.metadata ?? {};

    const config: SignalHubClientOptions = {
      url: options.url,
      jwt: options.jwt,
      protocolVersion: '0.1.0',
      heartbeatInterval: 25000,
      autoReconnect: false, // Disable for tests
      maxReconnectAttempts: 0,
    };

    this.client = new SignalHubClient(config);

    // Capture incoming messages
    this.client.on('message', (event: any) => {
      const msg = event.message;
      this.receivedMessages.push(msg);
      this.messageHandlers.forEach(handler => handler(msg));
    });
  }

  /**
   * Connect to Signal Hub and register actor
   */
  async connect(): Promise<void> {
    await this.client.connect();
    await this.client.registerActor({
      address: this.actorAddress,
      capabilities: this.capabilities,
      metadata: this.metadata,
    });
  }

  /**
   * Send message to another actor
   */
  async send(to: CanonicalAddress, type: string, payload: unknown): Promise<void> {
    await this.client.send(this.actorAddress, to, type, payload);
  }

  /**
   * Send message and wait for acknowledgment
   */
  async sendWithAck(to: CanonicalAddress, type: string, payload: unknown): Promise<void> {
    await this.client.sendWithAck(this.actorAddress, to, type, payload);
  }

  /**
   * Subscribe to a topic
   */
  async subscribe(topic: string): Promise<void> {
    await this.client.subscribe(this.actorAddress, topic);
  }

  /**
   * Unsubscribe from a topic
   */
  async unsubscribe(topic: string): Promise<void> {
    await this.client.unsubscribe(this.actorAddress, topic);
  }

  /**
   * Publish to a topic
   */
  async publish(topic: string, type: string, payload: unknown): Promise<void> {
    await this.client.publish(this.actorAddress, topic, type, payload);
  }

  /**
   * Broadcast message to all actors
   */
  async broadcast(type: string, payload: unknown): Promise<void> {
    await this.client.broadcast(this.actorAddress, type, payload);
  }

  /**
   * Discover actors by capability (simplified test interface)
   * For full control, use getClient().discover(from, pattern, capability, limit)
   */
  async discover(capability?: string, limit?: number): Promise<any[]> {
    // Pass '*' as pattern to match all, and capability as filter
    return this.client.discover(this.actorAddress, '*', capability, limit);
  }

  /**
   * Wait for a specific message type
   */
  async waitForMessage(
    predicate: (msg: SharedMessage) => boolean,
    timeout: number = 5000
  ): Promise<SharedMessage> {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.messageHandlers = this.messageHandlers.filter(h => h !== handler);
        reject(new Error(`Timeout waiting for message (${timeout}ms)`));
      }, timeout);

      const handler = (msg: SharedMessage) => {
        if (predicate(msg)) {
          clearTimeout(timer);
          this.messageHandlers = this.messageHandlers.filter(h => h !== handler);
          resolve(msg);
        }
      };

      this.messageHandlers.push(handler);

      // Check if message already received
      const existing = this.receivedMessages.find(predicate);
      if (existing) {
        clearTimeout(timer);
        this.messageHandlers = this.messageHandlers.filter(h => h !== handler);
        resolve(existing);
      }
    });
  }

  /**
   * Get all received messages
   */
  getReceivedMessages(): SharedMessage[] {
    return [...this.receivedMessages];
  }

  /**
   * Clear received messages
   */
  clearReceivedMessages(): void {
    this.receivedMessages = [];
  }

  /**
   * Get connection state
   */
  getState(): string {
    return this.client.connectionState;
  }

  /**
   * Get session ID
   */
  getSessionId(): string | null {
    return this.client.session;
  }

  /**
   * Get actor identity
   */
  getActorIdentity(): CanonicalAddress | null {
    return this.actorAddress;
  }

  /**
   * Disconnect from Signal Hub
   */
  async disconnect(): Promise<void> {
    await this.client.disconnect();
  }

  /**
   * Get the underlying client (for advanced usage)
   */
  getClient(): SignalHubClient {
    return this.client;
  }
}

/**
 * Create a browser actor instance for testing
 */
export async function createBrowserActor(options: BrowserActorOptions): Promise<BrowserActorWrapper> {
  const actor = new BrowserActorWrapper(options);
  await actor.connect();
  return actor;
}
