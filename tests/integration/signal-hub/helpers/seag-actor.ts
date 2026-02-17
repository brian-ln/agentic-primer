/**
 * SEAG Actor Test Wrapper
 *
 * Wrapper around SignalHubClient from ugs/ for integration tests.
 */

import { SignalHubClient } from '../../../../ugs/src/messaging/signal-hub/client';
import type { SignalHubConfig } from '../../../../ugs/src/messaging/signal-hub/types';
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';

export interface SeagActorOptions {
  url: string;
  jwt: string;
  actorAddress: CanonicalAddress;
  capabilities: string[];
  metadata?: Record<string, unknown>;
}

export class SeagActorWrapper {
  private client: SignalHubClient;
  private actorAddress: CanonicalAddress;
  private capabilities: string[];
  private metadata: Record<string, unknown>;
  private receivedMessages: SharedMessage[] = [];

  constructor(options: SeagActorOptions) {
    this.actorAddress = options.actorAddress;
    this.capabilities = options.capabilities;
    this.metadata = options.metadata ?? {};

    const config: SignalHubConfig = {
      url: options.url,
      jwt: options.jwt,
      protocolVersion: '0.1.0',
      heartbeatInterval: 25000,
      reconnect: {
        enabled: false, // Disable for tests
        maxAttempts: 0,
        initialDelay: 1000,
        maxDelay: 30000,
        multiplier: 2,
      },
      messageQueue: {
        enabled: true,
        maxSize: 1000,
        defaultTtl: 60000,
      },
    };

    this.client = new SignalHubClient(config);

    // Capture incoming messages
    this.client.on('message', (msg: SharedMessage) => {
      this.receivedMessages.push(msg);
    });
  }

  /**
   * Connect to Signal Hub and register actor
   */
  async connect(): Promise<void> {
    await this.client.connect();
    await this.client.registerActor(this.actorAddress, this.capabilities, this.metadata);
  }

  /**
   * Send message to another actor
   */
  send(to: CanonicalAddress, type: string, payload: unknown): void {
    this.client.send({
      to,
      type,
      payload,
      from: this.actorAddress,
    });
  }

  /**
   * Send message and wait for acknowledgment
   */
  async sendWithAck(to: CanonicalAddress, type: string, payload: unknown): Promise<SharedMessage> {
    return this.client.sendWithAck({
      to,
      type,
      payload,
      from: this.actorAddress,
    });
  }

  /**
   * Broadcast message to all registered actors
   */
  async broadcast(
    type: string,
    data: unknown,
    options?: { excludeSelf?: boolean; targetCapability?: string }
  ): Promise<void> {
    await this.client.broadcast(this.actorAddress, type, data, options);
  }

  /**
   * Publish message to topic subscribers
   */
  async publish(topic: string, type: string, data: unknown): Promise<void> {
    await this.client.publish(this.actorAddress, topic, type, data);
  }

  /**
   * Subscribe to a topic
   */
  async subscribe(topic: string, durable?: boolean): Promise<string> {
    return this.client.subscribe(this.actorAddress, topic, durable);
  }

  /**
   * Unsubscribe from a topic
   */
  async unsubscribe(subscriptionId: string): Promise<void> {
    await this.client.unsubscribe(this.actorAddress, subscriptionId);
  }

  /**
   * Discover actors by capability (simplified test interface)
   * For full control, use getClient().discover(from, pattern, capability, limit)
   */
  async discover(capability?: string, limit?: number): Promise<Array<{ address: CanonicalAddress; capabilities: string[] }>> {
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
        reject(new Error(`Timeout waiting for message (${timeout}ms)`));
      }, timeout);

      const handler = (msg: SharedMessage) => {
        if (predicate(msg)) {
          clearTimeout(timer);
          this.client.off('message', handler);
          resolve(msg);
        }
      };

      this.client.on('message', handler);

      // Check if message already received
      const existing = this.receivedMessages.find(predicate);
      if (existing) {
        clearTimeout(timer);
        this.client.off('message', handler);
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
    return this.client.getState();
  }

  /**
   * Get session ID
   */
  getSessionId(): string | null {
    return this.client.getSessionId();
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
 * Create a SEAG actor instance for testing
 */
export async function createSeagActor(options: SeagActorOptions): Promise<SeagActorWrapper> {
  const actor = new SeagActorWrapper(options);
  await actor.connect();
  return actor;
}
