/**
 * SignalHubTransport - ITransport adapter for SignalHubClient
 *
 * Bridges @agentic-primer/actors ITransport interface to SignalHubClient,
 * enabling the actors MessageRouter to dispatch messages over Signal Hub
 * via registerTransport('hub', new SignalHubTransport(client)).
 *
 * Usage in ugs (via BridgeRoute):
 * ```typescript
 * const transport = new SignalHubTransport(signalHubClient);
 * router.registerBridge({ prefix: 'hub://', transport, serde: new JsonSerde() });
 * ```
 *
 * The BridgeRoute serializes the Message to bytes and calls transport.send().
 * This transport deserializes back to SharedMessage and dispatches via the client.
 */

import type { ITransport, ConnectionState } from '@agentic-primer/actors';
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';
import { SignalHubClient } from './SignalHubClient.js';
import type { SignalHubClientOptions } from './types.js';

/**
 * ITransport implementation backed by SignalHubClient.
 *
 * send(recipient, message) contract:
 * - recipient: target actor address (e.g. "hub://my-actor" or canonical address)
 * - message: serialized SharedMessage bytes (Uint8Array) from ISerde, OR raw SharedMessage object
 *
 * Inbound messages are surfaced via the onReceive handler registered at construction.
 */
export class SignalHubTransport implements ITransport {
  private client: SignalHubClient;
  private receiveHandler: ((sender: string, message: unknown) => void) | null = null;

  constructor(client: SignalHubClient) {
    this.client = client;

    // Bridge inbound hub messages to the actors receive handler
    this.client.on('message', (event) => {
      if (!this.receiveHandler) return;
      const msg = event.message;
      const sender = msg.from ?? ('@(unknown)' as CanonicalAddress);
      this.receiveHandler(sender, msg);
    });
  }

  get state(): ConnectionState {
    const clientState = this.client.connectionState;
    // Map SignalHubClient ConnectionState to actors ConnectionState
    switch (clientState) {
      case 'connected': return 'connected';
      case 'connecting': return 'connecting';
      case 'disconnecting': return 'disconnecting';
      default: return 'disconnected';
    }
  }

  async connect(remoteAddress: string): Promise<void> {
    await this.client.connect();
    // remoteAddress is the hub URL — SignalHubClient uses its configured URL
    // but we accept this parameter for ITransport compliance.
    void remoteAddress;
  }

  async disconnect(): Promise<void> {
    await this.client.disconnect();
  }

  /**
   * Send a message to a recipient via Signal Hub.
   *
   * @param recipient - Target canonical address (e.g. "@(my-actor)")
   * @param message - SharedMessage (as object) or serialized bytes (Uint8Array)
   */
  async send(recipient: string, message: unknown): Promise<void> {
    let sharedMsg: SharedMessage;

    if (message instanceof Uint8Array) {
      // Deserialize bytes to SharedMessage (JsonSerde produces UTF-8 JSON)
      const text = new TextDecoder().decode(message);
      sharedMsg = JSON.parse(text) as SharedMessage;
    } else if (isSharedMessage(message)) {
      sharedMsg = message;
    } else {
      throw new TypeError(`SignalHubTransport.send: expected SharedMessage or Uint8Array, got ${typeof message}`);
    }

    const from = sharedMsg.from ?? ('@(unknown)' as CanonicalAddress);
    const to = (recipient as CanonicalAddress) ?? sharedMsg.to;

    await this.client.send(from, to, sharedMsg.type, sharedMsg.payload, {
      traceId: (sharedMsg.metadata as any)?.traceId,
      priority: (sharedMsg.metadata as any)?.priority,
      ttl: sharedMsg.ttl ?? undefined,
    });
  }

  onReceive(handler: (sender: string, message: unknown) => void): void {
    this.receiveHandler = handler;
  }
}

/**
 * Factory: create a SignalHubTransport from options (constructs client internally).
 */
export function createSignalHubTransport(options: SignalHubClientOptions): SignalHubTransport {
  const client = new SignalHubClient(options);
  return new SignalHubTransport(client);
}

function isSharedMessage(value: unknown): value is SharedMessage {
  return (
    value !== null &&
    typeof value === 'object' &&
    'type' in (value as object) &&
    'to' in (value as object)
  );
}
