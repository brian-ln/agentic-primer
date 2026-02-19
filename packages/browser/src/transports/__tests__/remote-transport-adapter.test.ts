import { describe, it, expect, mock, beforeEach } from 'bun:test';
import { RemoteTransportAdapter } from '../remote-transport-adapter.ts';
import { RemoteTransport, type RemoteMessage, type RemoteTransportConfig } from '../remote-transport.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function makeConfig(overrides: Partial<RemoteTransportConfig> = {}): RemoteTransportConfig {
  return {
    url: 'ws://localhost:9999',
    reconnectAttempts: 0,
    heartbeatInterval: 60_000,
    ...overrides,
  };
}

/**
 * Build a minimal RemoteTransport mock.
 * We patch the methods after construction so we don't need to touch internals.
 */
function makeMockTransport(connected = false): RemoteTransport {
  const config = makeConfig();
  const transport = new RemoteTransport(config);

  // Override connect / disconnect / send / isConnected
  transport.connect = mock(() => Promise.resolve());
  transport.disconnect = mock(() => {});
  transport.send = mock(() => {});
  transport.isConnected = mock(() => connected);

  return transport;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('RemoteTransportAdapter', () => {
  let mockTransport: RemoteTransport;
  let adapter: RemoteTransportAdapter;

  beforeEach(() => {
    mockTransport = makeMockTransport(false);
    adapter = new RemoteTransportAdapter(mockTransport);
  });

  // -------------------------------------------------------------------------
  // 1. connect()
  // -------------------------------------------------------------------------
  describe('connect()', () => {
    it('calls transport.connect()', async () => {
      await adapter.connect('ws://localhost:9999');
      expect(mockTransport.connect).toHaveBeenCalledTimes(1);
    });

    it('updates transport.config.url with the provided address', async () => {
      await adapter.connect('ws://newhost:8080');
      expect(mockTransport.config.url).toBe('ws://newhost:8080');
    });
  });

  // -------------------------------------------------------------------------
  // 2. disconnect()
  // -------------------------------------------------------------------------
  describe('disconnect()', () => {
    it('calls transport.disconnect()', async () => {
      await adapter.disconnect();
      expect(mockTransport.disconnect).toHaveBeenCalledTimes(1);
    });
  });

  // -------------------------------------------------------------------------
  // 3. send()
  // -------------------------------------------------------------------------
  describe('send()', () => {
    it('calls transport.send() with the correct RemoteMessage shape', async () => {
      const message = {
        type: 'ping',
        payload: { value: 1 },
        from: 'actor/source',
        id: 'msg-xyz',
      };

      await adapter.send('actor/target', message);

      expect(mockTransport.send).toHaveBeenCalledTimes(1);
      const [remoteMsg] = (mockTransport.send as ReturnType<typeof mock>).mock.calls[0];
      expect(remoteMsg).toEqual({
        to: 'actor/target',
        from: 'actor/source',
        type: 'ping',
        payload: { value: 1 },
        id: 'msg-xyz',
      });
    });

    it('uses recipient as the "to" field regardless of message.to', async () => {
      await adapter.send('the/recipient', { type: 'foo', payload: null });
      const [remoteMsg] = (mockTransport.send as ReturnType<typeof mock>).mock.calls[0];
      expect(remoteMsg.to).toBe('the/recipient');
    });

    it('defaults type to "message" when not present', async () => {
      await adapter.send('dest', { payload: 'raw' });
      const [remoteMsg] = (mockTransport.send as ReturnType<typeof mock>).mock.calls[0];
      expect(remoteMsg.type).toBe('message');
    });
  });

  // -------------------------------------------------------------------------
  // 4. onReceive
  // -------------------------------------------------------------------------
  describe('onReceive()', () => {
    it('fires the handler when the transport receives a message', () => {
      const received: Array<{ sender: string; message: unknown }> = [];
      adapter.onReceive((sender, message) => {
        received.push({ sender, message });
      });

      // Simulate an inbound message by invoking the config.onMessage callback
      // that the adapter wired up.
      const inbound: RemoteMessage = {
        to: 'browser/actor',
        from: 'server/actor',
        type: 'reply',
        payload: { ok: true },
        id: 'r-1',
      };
      mockTransport.config.onMessage(inbound);

      expect(received).toHaveLength(1);
      expect(received[0].sender).toBe('server/actor');
      expect(received[0].message).toEqual(inbound);
    });

    it('uses "to" as sender when "from" is absent', () => {
      const received: Array<{ sender: string }> = [];
      adapter.onReceive((sender) => received.push({ sender }));

      const inbound: RemoteMessage = {
        to: 'browser/actor',
        from: undefined,
        type: 'event',
        payload: {},
      };
      mockTransport.config.onMessage(inbound);

      expect(received[0].sender).toBe('browser/actor');
    });

    it('replaces the previous handler on a second call', () => {
      const calls1: unknown[] = [];
      const calls2: unknown[] = [];

      adapter.onReceive(() => calls1.push(1));
      adapter.onReceive(() => calls2.push(2));

      const inbound: RemoteMessage = { to: 'x', type: 'y', payload: {} };
      mockTransport.config.onMessage(inbound);

      expect(calls1).toHaveLength(0);
      expect(calls2).toHaveLength(1);
    });
  });

  // -------------------------------------------------------------------------
  // 5. state getter
  // -------------------------------------------------------------------------
  describe('state', () => {
    it('returns "connected" when transport.isConnected() is true', () => {
      mockTransport.isConnected = mock(() => true);
      expect(adapter.state).toBe('connected');
    });

    it('returns "disconnected" when transport.isConnected() is false', () => {
      mockTransport.isConnected = mock(() => false);
      expect(adapter.state).toBe('disconnected');
    });
  });

  // -------------------------------------------------------------------------
  // 6. Config-based construction
  // -------------------------------------------------------------------------
  describe('config-based construction', () => {
    it('constructs a RemoteTransport from a config object', async () => {
      const config = makeConfig({ url: 'ws://example.com' });
      // We cannot easily intercept the internal transport here, so just
      // verify the adapter is functional (no throw on construction).
      const adapterFromConfig = new RemoteTransportAdapter(config);
      expect(adapterFromConfig).toBeDefined();
      expect(adapterFromConfig.state).toBe('disconnected');
    });
  });
});
