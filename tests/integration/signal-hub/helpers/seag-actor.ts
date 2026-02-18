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
  private messageHandlers: Array<(msg: SharedMessage) => void> = [];

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
      this.messageHandlers.forEach(handler => handler(msg));
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
   *
   * @param to - Target actor's canonical address (from registry)
   * @param type - Message type (e.g., 'app:task:assigned')
   * @param payload - Application-specific payload data
   * @param metadata - Optional message metadata
   *
   * @example
   * actor.send('runtime:browser/actor-123', 'task:assigned', { taskId: '456' });
   */
  send(to: CanonicalAddress, type: string, payload: unknown, metadata?: Record<string, unknown>): void {
    this.client.send({
      to,
      type,
      payload,
      from: this.actorAddress,
      ...(metadata && { metadata }),
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
   * Wait for a specific message matching predicate
   *
   * @param predicate - Function to test each message (receives SharedMessage, NOT MessageEvent)
   * @param timeout - Timeout in milliseconds (default: 5000)
   * @returns Promise resolving to the matched SharedMessage
   *
   * @example
   * // Wait for hub:registered confirmation
   * const msg = await actor.waitForMessage(
   *   msg => msg.type === 'hub:registered',
   *   5000
   * );
   * expect(msg.payload.address).toBe('runtime:seag/actor-123');
   *
   * @example
   * // Wait for application message
   * const taskMsg = await actor.waitForMessage(
   *   msg => msg.type === 'task:assigned' && msg.payload.taskId === '456',
   *   10000
   * );
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
   * Reconnect to Signal Hub and re-register actor
   */
  async reconnect(): Promise<void> {
    await this.client.connect();
    await this.client.registerActor(this.actorAddress, this.capabilities, this.metadata);
  }

  /**
   * Register event listener
   *
   * **IMPORTANT:** Event signatures are transformed by this wrapper:
   * - 'message': Handler receives `SharedMessage` (already parsed from JSON, NOT MessageEvent)
   * - 'connected': Handler receives no arguments
   * - 'disconnected': Handler receives no arguments
   * - 'error': Handler receives `Error` object
   *
   * @param event - Event name ('message' | 'connected' | 'disconnected' | 'error')
   * @param handler - Event handler with signature specific to event type
   *
   * @example
   * // Listen for all messages (receives SharedMessage, not MessageEvent)
   * actor.on('message', (msg: SharedMessage) => {
   *   console.log('Received:', msg.type, msg.payload);
   * });
   *
   * @example
   * // Listen for connection events
   * actor.on('connected', () => {
   *   console.log('Connected to hub');
   * });
   *
   * @example
   * // Listen for errors
   * actor.on('error', (error: Error) => {
   *   console.error('Actor error:', error);
   * });
   */
  on(event: string, handler: (...args: any[]) => void): void {
    if (event === 'message') {
      this.messageHandlers.push(handler);
    } else {
      this.client.on(event as any, handler);
    }
  }

  /**
   * Remove event listener
   * For 'message' events, removes from internal messageHandlers
   * For other events, delegates to underlying client
   */
  off(event: string, handler: (...args: any[]) => void): void {
    if (event === 'message') {
      this.messageHandlers = this.messageHandlers.filter(h => h !== handler);
    } else {
      this.client.off(event as any, handler);
    }
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
