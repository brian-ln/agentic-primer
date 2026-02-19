/**
 * RemoteTransportAdapter — ITransport wrapper around RemoteTransport
 *
 * Wraps the browser-native RemoteTransport so that MessageRouter can use it
 * as a registered ITransport (e.g. via registerTransport/registerBridge).
 *
 * RemoteTransport.send() accepts { to, from, type, payload, id }.
 * ITransport.send() accepts (recipient: string, message: unknown).
 * This adapter bridges the two calling conventions.
 */

import type { ITransport, ConnectionState } from '@agentic-primer/actors';
import { RemoteTransport, type RemoteTransportConfig, type RemoteMessage } from './remote-transport.ts';

export class RemoteTransportAdapter implements ITransport {
  private readonly transport: RemoteTransport;
  private receiveHandler: ((sender: string, message: unknown) => void) | null = null;

  /**
   * Accepts either an already-constructed RemoteTransport instance or a
   * RemoteTransportConfig to construct one internally.
   */
  constructor(transportOrConfig: RemoteTransport | RemoteTransportConfig) {
    if (transportOrConfig instanceof RemoteTransport) {
      this.transport = transportOrConfig;
    } else {
      this.transport = new RemoteTransport(transportOrConfig);
    }

    // Wire the transport's onMessage callback to our receive handler.
    // We capture the original onMessage so existing subscribers still fire.
    const originalOnMessage = this.transport.config.onMessage;
    this.transport.config.onMessage = (message: RemoteMessage) => {
      originalOnMessage(message);
      if (this.receiveHandler) {
        this.receiveHandler(message.from ?? message.to, message);
      }
    };
  }

  /**
   * Connect the underlying WebSocket.
   * @param remoteAddress — WebSocket URL. When provided it overrides the URL
   *   that was set in the config at construction time.
   */
  async connect(remoteAddress: string): Promise<void> {
    if (remoteAddress) {
      this.transport.config.url = remoteAddress;
    }
    await this.transport.connect();
  }

  /** Disconnect the underlying WebSocket. */
  async disconnect(): Promise<void> {
    this.transport.disconnect();
  }

  /**
   * Send an actor message to a remote recipient.
   *
   * The router passes deserialized actor message objects; we extract the
   * fields required by RemoteMessage and forward to transport.send().
   */
  async send(recipient: string, message: unknown): Promise<void> {
    const msg = message as Partial<RemoteMessage>;
    const remoteMessage: RemoteMessage = {
      to: recipient,
      from: msg.from,
      type: msg.type ?? 'message',
      payload: msg.payload ?? message,
      id: msg.id,
    };
    this.transport.send(remoteMessage);
  }

  /**
   * Register a handler that fires whenever the transport receives a message.
   * Only one handler is supported (matching ITransport contract); subsequent
   * calls replace the previous handler.
   */
  onReceive(handler: (sender: string, message: unknown) => void): void {
    this.receiveHandler = handler;
  }

  /** Maps transport connection status to ITransport ConnectionState. */
  get state(): ConnectionState {
    return this.transport.isConnected() ? 'connected' : 'disconnected';
  }
}
