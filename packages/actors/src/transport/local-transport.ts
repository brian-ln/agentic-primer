/**
 * LocalTransport - Direct delivery for same-runtime actors.
 * Ported from brianln.ai/src/actors/transports/LocalTransport.ts.
 * No serialization needed, fastest transport.
 */

import type { ITransport, ConnectionState } from '../interfaces.ts';

export class LocalTransport implements ITransport {
  private receiveHandler?: (sender: string, message: unknown) => void;
  private _state: ConnectionState = 'connected';

  async connect(_remoteAddress: string): Promise<void> {
    // Local transport is always connected
    this._state = 'connected';
  }

  async disconnect(): Promise<void> {
    this._state = 'disconnected';
  }

  async send(recipient: string, message: unknown): Promise<void> {
    // For local transport, invoke the receive handler directly
    if (this.receiveHandler) {
      this.receiveHandler(recipient, message);
    }
  }

  onReceive(handler: (sender: string, message: unknown) => void): void {
    this.receiveHandler = handler;
  }

  get state(): ConnectionState {
    return this._state;
  }
}
