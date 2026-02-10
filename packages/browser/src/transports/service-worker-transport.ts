/**
 * ServiceWorkerTransport - Routes actor messages through a Service Worker
 *
 * The Service Worker maintains a single WebSocket connection to the server.
 * Browser actors communicate with it via MessageChannel for bidirectional messaging.
 *
 * Implements the ITransport interface from @agentic-primer/actors.
 *
 * Ported from brianln.ai/src/actors/transports/WebSocketTransport.ts
 */

import type { ITransport, ConnectionState } from '@agentic-primer/actors';

interface TransportMessage {
  type: 'ACTOR_MESSAGE' | 'ACTOR_REPLY';
  target: string;
  sender?: string;
  payload: unknown;
  correlationId?: string;
}

export class ServiceWorkerTransport implements ITransport {
  private receiveHandler?: (sender: string, message: unknown) => void;
  private connected = false;
  private messageChannel?: MessageChannel;

  get state(): ConnectionState {
    return this.connected ? 'connected' : 'disconnected';
  }

  async connect(remoteAddress: string): Promise<void> {
    if (this.connected) {
      return;
    }

    const registration = await navigator.serviceWorker.ready;

    if (!registration.active) {
      throw new Error('Service Worker not active');
    }

    this.messageChannel = new MessageChannel();

    this.messageChannel.port1.onmessage = (event: MessageEvent<TransportMessage>) => {
      this.handleServiceWorkerMessage(event.data);
    };

    registration.active.postMessage(
      {
        type: 'TRANSPORT_CONNECT',
        remoteAddress,
      },
      [this.messageChannel.port2]
    );

    this.connected = true;
  }

  async disconnect(): Promise<void> {
    if (!this.connected) {
      return;
    }

    if (navigator.serviceWorker.controller) {
      navigator.serviceWorker.controller.postMessage({
        type: 'TRANSPORT_DISCONNECT',
      });
    }

    this.messageChannel?.port1.close();
    this.connected = false;
  }

  async send(recipient: string, message: unknown): Promise<void> {
    if (!this.connected) {
      throw new Error('ServiceWorkerTransport not connected');
    }

    if (!navigator.serviceWorker.controller) {
      throw new Error('No Service Worker controller');
    }

    const transportMessage: TransportMessage = {
      type: 'ACTOR_MESSAGE',
      target: recipient,
      payload: message,
    };

    navigator.serviceWorker.controller.postMessage(transportMessage);
  }

  onReceive(handler: (sender: string, message: unknown) => void): void {
    this.receiveHandler = handler;
  }

  private handleServiceWorkerMessage(data: TransportMessage): void {
    if (!this.receiveHandler) {
      return;
    }

    const sender = data.sender || data.target;
    this.receiveHandler(sender, data.payload);
  }
}
