#!/usr/bin/env bun
import { describe, test, expect, beforeEach } from 'bun:test';
import {
  BaseChannelActor,
  type ChannelConfig,
  type ChannelStatus,
  type InboundChannelMessage,
  type OutboundChannelMessage,
} from '../channels/ChannelActor';
import type { Message, Address } from '@agentic-primer/actors';
import { address, createMessage } from '@agentic-primer/actors';
import { MessageRouter } from '../router';
import GraphStore from '../../graph';
import { ProgramManager } from '../../entities/program';

// Mock ChannelActor implementation for testing
class MockChannelActor extends BaseChannelActor {
  public _address: Address;
  public router: MessageRouter;
  public connected = false;
  public sentMessages: OutboundChannelMessage[] = [];

  constructor(config: ChannelConfig, router: MessageRouter) {
    super(config);
    this._address = address(config.id);
    this.router = router;
  }

  get address() {
    return this._address;
  }

  async tell(to: Address, type: string, payload: any): Promise<void> {
    const message = createMessage(to, type, payload, {
      pattern: 'tell',
      from: this.address,
    });
    await this.router.tell(message);
  }

  async ask<T = any>(to: Address, type: string, payload: any): Promise<any> {
    const message = createMessage(to, type, payload, {
      pattern: 'ask',
      from: this.address,
    });
    return await this.router.ask(message);
  }

  async connect(): Promise<void> {
    this.updateStatus({ status: 'connecting' });
    await new Promise(resolve => setTimeout(resolve, 10)); // Simulate connection delay
    this.connected = true;
    this.updateStatus({
      status: 'connected',
      connected: true,
      authenticated: true,
      lastConnected: Date.now(),
    });
  }

  async disconnect(): Promise<void> {
    this.connected = false;
    this.updateStatus({
      status: 'disconnected',
      connected: false,
      authenticated: false,
    });
  }

  async send(message: OutboundChannelMessage): Promise<string> {
    if (!this.connected) {
      throw new Error('Not connected');
    }

    this.sentMessages.push(message);
    this._status.messagesSent++;
    this._status.lastSent = Date.now();

    return `msg_${Date.now()}`;
  }

  // Test helper to simulate incoming message
  async simulateInbound(message: InboundChannelMessage): Promise<void> {
    await this.handleInboundMessage(message);
  }
}

describe('ChannelActor', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let config: ChannelConfig;
  let channel: MockChannelActor;

  beforeEach(() => {
    store = new GraphStore(':memory:');
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    config = {
      id: 'test-channel',
      name: 'Test Channel',
      platform: 'test',
      autoConnect: false,
      autoReconnect: true,
      reconnect: {
        initialDelay: 100,
        maxDelay: 1000,
        multiplier: 2,
        maxAttempts: 3,
      },
      platformConfig: {
        apiKey: 'test-key',
      },
    };

    channel = new MockChannelActor(config, router);
  });

  describe('Configuration', () => {
    test('initializes with config', () => {
      expect(channel.config).toEqual(config);
      expect(channel.config.id).toBe('test-channel');
      expect(channel.config.platform).toBe('test');
    });

    test('initializes with default status', () => {
      const status = channel.getStatus();
      expect(status.status).toBe('disconnected');
      expect(status.connected).toBe(false);
      expect(status.authenticated).toBe(false);
      expect(status.messagesSent).toBe(0);
      expect(status.messagesReceived).toBe(0);
      expect(status.reconnectAttempts).toBe(0);
    });
  });

  describe('Lifecycle', () => {
    test('connects successfully', async () => {
      await channel.connect();

      const status = channel.getStatus();
      expect(status.status).toBe('connected');
      expect(status.connected).toBe(true);
      expect(status.authenticated).toBe(true);
      expect(status.lastConnected).toBeGreaterThan(0);
    });

    test('disconnects successfully', async () => {
      await channel.connect();
      await channel.disconnect();

      const status = channel.getStatus();
      expect(status.status).toBe('disconnected');
      expect(status.connected).toBe(false);
    });

    test('handles connect via actor message', async () => {
      const message = createMessage(channel.address, 'connect', {}, {
        pattern: 'ask',
        correlationId: 'test-corr-1',
      });

      const response = await channel.receive(message);

      expect(response.success).toBe(true);
      expect(response.payload?.status).toBe('connected');
      expect(channel.getStatus().connected).toBe(true);
    });

    test('handles disconnect via actor message', async () => {
      await channel.connect();

      const message = createMessage(channel.address, 'disconnect', {}, {
        pattern: 'ask',
        correlationId: 'test-corr-2',
      });

      const response = await channel.receive(message);

      expect(response.success).toBe(true);
      expect(response.payload?.status).toBe('disconnected');
      expect(channel.getStatus().connected).toBe(false);
    });

    test('handles status query via actor message', async () => {
      const message = createMessage(channel.address, 'status', {}, {
        pattern: 'ask',
        correlationId: 'test-corr-3',
      });

      const response = await channel.receive(message);

      expect(response.success).toBe(true);
      expect(response.payload).toHaveProperty('status');
      expect(response.payload).toHaveProperty('connected');
      expect(response.payload).toHaveProperty('messagesSent');
    });
  });

  describe('Messaging', () => {
    test('sends message when connected', async () => {
      await channel.connect();

      const outbound: OutboundChannelMessage = {
        to: { id: 'user123', type: 'user' },
        content: [{ type: 'text', text: 'Hello, world!' }],
      };

      const messageId = await channel.send(outbound);

      expect(messageId).toMatch(/^msg_\d+$/);
      expect(channel.sentMessages).toHaveLength(1);
      expect(channel.sentMessages[0]).toEqual(outbound);
      expect(channel.getStatus().messagesSent).toBe(1);
      expect(channel.getStatus().lastSent).toBeGreaterThan(0);
    });

    test('throws error when sending while disconnected', async () => {
      const outbound: OutboundChannelMessage = {
        to: { id: 'user123', type: 'user' },
        content: [{ type: 'text', text: 'Hello, world!' }],
      };

      await expect(channel.send(outbound)).rejects.toThrow('Not connected');
    });

    test('handles send via actor message', async () => {
      await channel.connect();

      const outbound: OutboundChannelMessage = {
        to: { id: 'user456', type: 'user' },
        content: [{ type: 'text', text: 'Test message' }],
      };

      const message = createMessage(channel.address, 'send', outbound, {
        pattern: 'ask',
        correlationId: 'test-corr-4',
      });

      const response = await channel.receive(message);

      expect(response.success).toBe(true);
      expect(response.payload?.messageId).toMatch(/^msg_\d+$/);
      expect(channel.sentMessages).toHaveLength(1);
    });

    test('receives inbound messages', async () => {
      let receivedMessage: InboundChannelMessage | undefined;
      channel.onMessage((msg) => {
        receivedMessage = msg;
      });

      const inbound: InboundChannelMessage = {
        id: 'msg_inbound_1',
        from: { id: 'user789', type: 'user', name: 'Test User' },
        content: [{ type: 'text', text: 'Hello from user' }],
        timestamp: Date.now(),
      };

      await channel.simulateInbound(inbound);

      expect(receivedMessage).toEqual(inbound);
      expect(channel.getStatus().messagesReceived).toBe(1);
      expect(channel.getStatus().lastReceived).toBeGreaterThan(0);
    });

    test('handles multiple content types', async () => {
      await channel.connect();

      const outbound: OutboundChannelMessage = {
        to: { id: 'user999', type: 'user' },
        content: [
          { type: 'text', text: 'Check this out:' },
          { type: 'image', url: 'https://example.com/image.jpg', caption: 'Nice pic' },
        ],
      };

      const messageId = await channel.send(outbound);

      expect(messageId).toBeDefined();
      expect(channel.sentMessages[0].content).toHaveLength(2);
    });
  });

  describe('Status and Monitoring', () => {
    test('tracks message counts', async () => {
      await channel.connect();

      // Send 3 messages
      for (let i = 0; i < 3; i++) {
        await channel.send({
          to: { id: `user${i}`, type: 'user' },
          content: [{ type: 'text', text: `Message ${i}` }],
        });
      }

      // Receive 2 messages
      for (let i = 0; i < 2; i++) {
        await channel.simulateInbound({
          id: `inbound_${i}`,
          from: { id: `user${i}`, type: 'user' },
          content: [{ type: 'text', text: `Inbound ${i}` }],
          timestamp: Date.now(),
        });
      }

      const status = channel.getStatus();
      expect(status.messagesSent).toBe(3);
      expect(status.messagesReceived).toBe(2);
    });

    test('tracks timestamps', async () => {
      const beforeConnect = Date.now();
      await channel.connect();
      const afterConnect = Date.now();

      const status = channel.getStatus();
      expect(status.lastConnected).toBeGreaterThanOrEqual(beforeConnect);
      expect(status.lastConnected).toBeLessThanOrEqual(afterConnect);

      await channel.send({
        to: { id: 'user1', type: 'user' },
        content: [{ type: 'text', text: 'Test' }],
      });

      expect(channel.getStatus().lastSent).toBeGreaterThanOrEqual(afterConnect);
    });

    test('preserves metadata', async () => {
      channel.updateStatus({
        metadata: {
          deviceId: 'device-123',
          version: '1.0.0',
        },
      });

      const status = channel.getStatus();
      expect(status.metadata).toEqual({
        deviceId: 'device-123',
        version: '1.0.0',
      });
    });
  });

  describe('Actor Protocol', () => {
    test('returns error for unknown message type', async () => {
      const message = createMessage(channel.address, 'unknown-type', {}, {
        pattern: 'ask',
        correlationId: 'test-corr-5',
      });

      const response = await channel.receive(message);

      expect(response.success).toBe(false);
      expect(response.error).toContain('Unknown message type');
    });

    test('includes correlation ID in responses', async () => {
      const message = createMessage(channel.address, 'status', {}, {
        pattern: 'ask',
        correlationId: 'test-corr-6',
      });

      const response = await channel.receive(message);

      expect(response.correlationId).toBe('test-corr-6');
    });

    test('includes sender address in responses', async () => {
      const message = createMessage(channel.address, 'status', {}, {
        pattern: 'ask',
        correlationId: 'test-corr-7',
      });

      const response = await channel.receive(message);

      expect(response.from).toEqual(channel.address);
    });
  });

  describe('Supervision Hooks', () => {
    test('preRestart returns checkpoint', async () => {
      await channel.connect();

      const error = new Error('Test error');
      const checkpoint = await channel.preRestart!(error);

      expect(checkpoint).toHaveProperty('config');
      expect(checkpoint).toHaveProperty('status');
      expect(checkpoint.config.id).toBe('test-channel');
      expect(channel.getStatus().connected).toBe(false); // Disconnected
    });

    test('postRestart reconnects if previously connected', async () => {
      const checkpoint = {
        config,
        status: { connected: true },
        reconnectAttempts: 0,
      };

      await channel.postRestart!(checkpoint);

      expect(channel.getStatus().connected).toBe(true);
    });

    test('postRestart skips reconnect if previously disconnected', async () => {
      const checkpoint = {
        config,
        status: { connected: false },
        reconnectAttempts: 0,
      };

      await channel.postRestart!(checkpoint);

      expect(channel.getStatus().connected).toBe(false);
    });

    test('healthCheck returns connection status', async () => {
      expect(await channel.healthCheck!()).toBe(false);

      await channel.connect();
      expect(await channel.healthCheck!()).toBe(true);

      await channel.disconnect();
      expect(await channel.healthCheck!()).toBe(false);
    });
  });

  describe('Content Types', () => {
    test('supports text content', async () => {
      await channel.connect();

      await channel.send({
        to: { id: 'user1', type: 'user' },
        content: [{ type: 'text', text: 'Hello' }],
      });

      expect(channel.sentMessages[0].content[0]).toEqual({
        type: 'text',
        text: 'Hello',
      });
    });

    test('supports image content', async () => {
      await channel.connect();

      await channel.send({
        to: { id: 'user1', type: 'user' },
        content: [{
          type: 'image',
          url: 'https://example.com/img.jpg',
          caption: 'Photo',
        }],
      });

      expect(channel.sentMessages[0].content[0]).toEqual({
        type: 'image',
        url: 'https://example.com/img.jpg',
        caption: 'Photo',
      });
    });

    test('supports file content', async () => {
      await channel.connect();

      await channel.send({
        to: { id: 'user1', type: 'user' },
        content: [{
          type: 'file',
          url: 'https://example.com/doc.pdf',
          filename: 'document.pdf',
          mimeType: 'application/pdf',
        }],
      });

      expect(channel.sentMessages[0].content[0].type).toBe('file');
    });

    test('supports location content', async () => {
      await channel.connect();

      await channel.send({
        to: { id: 'user1', type: 'user' },
        content: [{
          type: 'location',
          latitude: 37.7749,
          longitude: -122.4194,
          name: 'San Francisco',
        }],
      });

      expect(channel.sentMessages[0].content[0].type).toBe('location');
    });
  });
});
