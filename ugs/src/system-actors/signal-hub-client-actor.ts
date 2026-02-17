#!/usr/bin/env bun
/**
 * SignalHubClientActor - SEAG integration for Signal Hub protocol
 *
 * Bridges SEAG's local actor system with remote Signal Hub:
 * - Auto-registers local actors with Signal Hub on connect
 * - Routes messages from local actors → Signal Hub
 * - Routes messages from Signal Hub → local actors
 * - Handles reconnection and re-registration transparently
 *
 * Message flow:
 *
 * Local Actor → Signal Hub:
 *   1. Local actor sends message to remote actor
 *   2. Router detects remote address (via bridge prefix)
 *   3. Routes to SignalHubClientActor
 *   4. SignalHubClientActor wraps in hub:send
 *   5. SignalHubClient sends via WebSocket
 *
 * Signal Hub → Local Actor:
 *   1. SignalHubClient receives message
 *   2. Emits 'message' event
 *   3. SignalHubClientActor handles event
 *   4. Routes to local actor via MessageRouter
 *
 * Registered at: @(bridges/signal-hub-client)
 *
 * Message types:
 *   'connect'           - Connect to Signal Hub
 *   'disconnect'        - Disconnect from Signal Hub
 *   'register-actor'    - Register a local actor
 *   'unregister-actor'  - Unregister a local actor
 *   'status'            - Get connection status
 *   'hub:send'          - Send message via Signal Hub (internal)
 */

import { Actor, createResponse, createErrorResponse, address } from '@agentic-primer/actors';
import type { Message, MessageResponse, MessageRouter, Address } from '@agentic-primer/actors';
import { SignalHubClient } from '../messaging/signal-hub/client.ts';
import type { SignalHubConfig } from '../messaging/signal-hub/types.ts';
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';
import { simplifyToShared, sharedToSimplify } from '@agentic-primer/protocols';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface SignalHubClientActorConfig extends SignalHubConfig {
  /** Auto-connect on start */
  autoConnect: boolean;

  /** Auto-register actors matching these prefixes */
  autoRegisterPrefixes: string[];
}

// ---------------------------------------------------------------------------
// SignalHubClientActor
// ---------------------------------------------------------------------------

export class SignalHubClientActor extends Actor {
  private client: SignalHubClient;
  private config: SignalHubClientActorConfig;

  constructor(router: MessageRouter, config: SignalHubClientActorConfig) {
    super('bridges/signal-hub-client', router);
    this.config = config;
    this.client = new SignalHubClient(config);

    // Set up event handlers
    this.setupEventHandlers();
  }

  /**
   * Start the actor. Call after registering with router.
   */
  async start(): Promise<void> {
    if (this.config.autoConnect) {
      await this.client.connect();
      await this.autoRegisterActors();
    }
  }

  /**
   * Stop the actor. Clean up connections.
   */
  async stop(): Promise<void> {
    await this.client.disconnect();
  }

  async receive(message: Message): Promise<MessageResponse> {
    switch (message.type) {
      case 'connect':
        return this.handleConnect(message);

      case 'disconnect':
        return this.handleDisconnect(message);

      case 'register-actor':
        return this.handleRegisterActor(message);

      case 'unregister-actor':
        return this.handleUnregisterActor(message);

      case 'status':
        return this.handleStatus(message);

      case 'hub:send':
        return this.handleHubSend(message);

      default:
        return createErrorResponse(message, `Unknown message type: ${message.type}`);
    }
  }

  // ---------------------------------------------------------------------------
  // Message Handlers
  // ---------------------------------------------------------------------------

  private async handleConnect(message: Message): Promise<MessageResponse> {
    try {
      await this.client.connect();
      await this.autoRegisterActors();

      return createResponse(message, {
        connected: true,
        sessionId: this.client.getSessionId(),
      });
    } catch (error) {
      return createErrorResponse(
        message,
        `Connection failed: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  private async handleDisconnect(message: Message): Promise<MessageResponse> {
    try {
      await this.client.disconnect();
      return createResponse(message, { connected: false });
    } catch (error) {
      return createErrorResponse(
        message,
        `Disconnect failed: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  private async handleRegisterActor(message: Message): Promise<MessageResponse> {
    const { actorAddress, capabilities, metadata } = message.payload as {
      actorAddress: string;
      capabilities: string[];
      metadata?: Record<string, unknown>;
    };

    if (!actorAddress || !capabilities) {
      return createErrorResponse(message, 'actorAddress and capabilities required');
    }

    try {
      await this.client.registerActor(
        actorAddress as CanonicalAddress,
        capabilities,
        metadata
      );

      return createResponse(message, { registered: true, actorAddress });
    } catch (error) {
      return createErrorResponse(
        message,
        `Registration failed: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  private async handleUnregisterActor(message: Message): Promise<MessageResponse> {
    const { actorAddress } = message.payload as { actorAddress: string };

    if (!actorAddress) {
      return createErrorResponse(message, 'actorAddress required');
    }

    try {
      await this.client.unregisterActor(actorAddress as CanonicalAddress);
      return createResponse(message, { unregistered: true, actorAddress });
    } catch (error) {
      return createErrorResponse(
        message,
        `Unregistration failed: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  private handleStatus(message: Message): MessageResponse {
    const connectionInfo = this.client.getConnectionInfo();
    const registeredActors = this.client.getRegisteredActors();

    return createResponse(message, {
      state: this.client.getState(),
      sessionId: this.client.getSessionId(),
      connectionInfo,
      registeredActors,
      actorCount: registeredActors.length,
    });
  }

  private handleHubSend(message: Message): MessageResponse {
    // This is called when a local actor wants to send a message to a remote actor
    const { to, type, payload, metadata } = message.payload as {
      to: string;
      type: string;
      payload: unknown;
      metadata?: Record<string, unknown>;
    };

    if (!to || !type) {
      return createErrorResponse(message, 'to and type required');
    }

    try {
      this.client.send({
        to: to as CanonicalAddress,
        type,
        payload,
        from: message.from as CanonicalAddress,
        metadata,
      });

      return createResponse(message, { sent: true });
    } catch (error) {
      return createErrorResponse(
        message,
        `Send failed: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  // ---------------------------------------------------------------------------
  // Event Handlers
  // ---------------------------------------------------------------------------

  private setupEventHandlers(): void {
    // Handle incoming messages from Signal Hub
    this.client.on('message', (msg: SharedMessage) => {
      this.handleIncomingMessage(msg);
    });

    // Handle connection events
    this.client.on('connected', (sessionId: string) => {
      this.logInfo('Connected to Signal Hub', { sessionId });
    });

    this.client.on('disconnected', (reason: string) => {
      this.logInfo('Disconnected from Signal Hub', { reason });
    });

    this.client.on('reconnecting', (attempt: number) => {
      this.logInfo('Reconnecting to Signal Hub', { attempt });
    });

    this.client.on('error', (error: Error) => {
      this.logError('Signal Hub error', { error: error.message });
    });

    this.client.on('actorRegistered', (actorAddress: CanonicalAddress) => {
      this.logInfo('Actor registered with Signal Hub', { actorAddress });
    });

    this.client.on('actorUnregistered', (actorAddress: CanonicalAddress) => {
      this.logInfo('Actor unregistered from Signal Hub', { actorAddress });
    });
  }

  private handleIncomingMessage(sharedMsg: SharedMessage): void {
    try {
      // Convert SharedMessage to simplify Message
      const simplifyMsg = sharedToSimplify(sharedMsg);

      // Route to local actor via MessageRouter
      // The router will handle delivering to the correct actor based on 'to' address
      const message: Message = {
        ...simplifyMsg,
        // Ensure it has required fields
        id: simplifyMsg.id,
        from: simplifyMsg.from,
        to: simplifyMsg.to,
        type: simplifyMsg.type,
        payload: simplifyMsg.payload,
        timestamp: simplifyMsg.timestamp,
        pattern: simplifyMsg.pattern as 'tell' | 'ask',
      };

      // Use router.tell to deliver message
      this.router.tell(message);
    } catch (error) {
      this.logError('Failed to handle incoming message', {
        error: error instanceof Error ? error.message : String(error),
      });
    }
  }

  // ---------------------------------------------------------------------------
  // Auto-registration
  // ---------------------------------------------------------------------------

  private async autoRegisterActors(): Promise<void> {
    // Get all actors from router that match autoRegisterPrefixes
    const allActors = this.getLocalActors();

    for (const actorAddress of allActors) {
      if (this.shouldAutoRegister(actorAddress)) {
        try {
          // Query actor for capabilities
          const capabilities = await this.queryActorCapabilities(actorAddress);
          await this.client.registerActor(
            actorAddress as CanonicalAddress,
            capabilities
          );
        } catch (error) {
          this.logError('Failed to auto-register actor', {
            actorAddress,
            error: error instanceof Error ? error.message : String(error),
          });
        }
      }
    }
  }

  private shouldAutoRegister(actorAddress: Address): boolean {
    const addrStr = typeof actorAddress === 'string' ? actorAddress : actorAddress.id;

    // Check if address matches any auto-register prefix
    for (const prefix of this.config.autoRegisterPrefixes) {
      if (addrStr.startsWith(prefix)) {
        return true;
      }
    }

    return false;
  }

  private getLocalActors(): Address[] {
    // This would need to be implemented based on how MessageRouter exposes registered actors
    // For now, return empty array (will be enhanced when integrating with router)
    return [];
  }

  private async queryActorCapabilities(actorAddress: Address): Promise<string[]> {
    // Query actor for capabilities via introspection protocol
    try {
      const response = await this.router.ask({
        id: crypto.randomUUID(),
        from: this.address,
        to: actorAddress,
        type: 'introspect',
        payload: null,
        timestamp: Date.now(),
        pattern: 'ask',
      });

      if (response.success && response.payload?.capabilities) {
        return response.payload.capabilities as string[];
      }
    } catch (error) {
      // Fallback to default capabilities
    }

    return ['compute']; // Default capability
  }
}
