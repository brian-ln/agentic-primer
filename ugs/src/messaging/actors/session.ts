#!/usr/bin/env bun
/**
 * SessionActor - Manages conversation sessions and agent execution
 *
 * Wraps UGS Session entity as an actor that can:
 * - Receive user messages
 * - Execute agent runs
 * - Invoke tools
 * - Return responses
 */

import type GraphStore from '@src/graph.ts';
import type { SessionManager, Session } from '@src/entities/session.ts';
import { ProgramManager } from '@src/entities/program.ts';
import { MessageRouter } from '../router.ts';
import { Actor } from '../actor.ts';
import {
  type Message,
  type MessageResponse,
  type StreamCallback,
  type TokenStreamEvent,
  address,
} from '@agentic-primer/actors';
import type { ModelManager } from '@src/entities/model.ts';

export class SessionActor extends Actor {
  private sessionId: string;
  private sessionManager: SessionManager;
  private programManager: ProgramManager;
  private store: GraphStore;
  private modelManager?: ModelManager;

  constructor(
    sessionId: string,
    sessionManager: SessionManager,
    programManager: ProgramManager,
    store: GraphStore,
    router: MessageRouter,
    modelManager?: ModelManager
  ) {
    super(sessionId, router);
    this.sessionId = sessionId;
    this.sessionManager = sessionManager;
    this.programManager = programManager;
    this.store = store;
    this.modelManager = modelManager;
  }

  /**
   * Receive a message
   * Handles: user-message, query-history, get-context
   */
  async receive(message: Message): Promise<MessageResponse> {
    switch (message.type) {
      case 'user-message':
        return await this.handleUserMessage(message);

      case 'query-history':
        return await this.handleQueryHistory(message);

      case 'get-context':
        return await this.handleGetContext(message);

      default:
        return {
          id: message.id + '_response',
          correlationId: message.correlationId || message.id,
          from: this.address,
          to: message.from || this.address,
          success: false,
          error: `Unknown message type: ${message.type}`,
          timestamp: Date.now(),
        };
    }
  }

  /**
   * Handle user message - execute agent
   */
  private async handleUserMessage(message: Message): Promise<MessageResponse> {
    try {
      const userMessage = message.payload.message || message.payload;

      // In a real implementation, this would:
      // 1. Add message to session history
      // 2. Invoke agent (Claude/GPT/etc) via program
      // 3. Stream tool calls to ToolActors
      // 4. Return final response

      // For now, simple echo with session context
      const session = this.sessionManager.getSession(this.sessionId);

      const response = {
        role: 'assistant',
        content: `[Session ${this.sessionId}] Received: ${userMessage}`,
        timestamp: Date.now(),
        sessionContext: session ? {
          messageCount: session.messages?.length || 0,
          created: session.created,
        } : null,
      };

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: response,
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }

  /**
   * Query session history
   */
  private async handleQueryHistory(message: Message): Promise<MessageResponse> {
    try {
      const session = this.sessionManager.getSession(this.sessionId);

      if (!session) {
        return {
          id: message.id + '_response',
          correlationId: message.correlationId || message.id,
          from: this.address,
          to: message.from || this.address,
          success: false,
          error: 'Session not found',
          timestamp: Date.now(),
        };
      }

      const history = {
        sessionId: this.sessionId,
        messages: session.messages || [],
        created: session.created,
        modified: session.modified,
      };

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: history,
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }

  /**
   * Get session context (metadata, stats)
   */
  private async handleGetContext(message: Message): Promise<MessageResponse> {
    try {
      const session = this.sessionManager.getSession(this.sessionId);

      if (!session) {
        return {
          id: message.id + '_response',
          correlationId: message.correlationId || message.id,
          from: this.address,
          to: message.from || this.address,
          success: false,
          error: 'Session not found',
          timestamp: Date.now(),
        };
      }

      const context = {
        id: this.sessionId,
        created: session.created,
        modified: session.modified,
        messageCount: session.messages?.length || 0,
        state: session.state || 'active',
      };

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: context,
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }

  /**
   * Stream inference response with real-time tokens
   */
  async stream(payload: any, onChunk: StreamCallback<TokenStreamEvent>): Promise<void> {
    try {
      // Check if payload requests inference
      const message = payload.message || payload.content || payload;
      if (typeof message !== 'string') {
        await onChunk({
          type: 'error',
          error: 'Invalid payload: expected message string',
          timestamp: Date.now(),
        });
        return;
      }

      // Get session to find model
      const session = this.sessionManager.getSession(this.sessionId);
      if (!session) {
        await onChunk({
          type: 'error',
          error: `Session not found: ${this.sessionId}`,
          timestamp: Date.now(),
        });
        return;
      }

      // Extract model ID from session config
      const modelRef = session.config?.defaultModel;
      if (!modelRef) {
        await onChunk({
          type: 'error',
          error: 'Session has no model configured',
          timestamp: Date.now(),
        });
        return;
      }

      // Parse model ID from @(id) format
      const modelId = modelRef.match(/@\(([^)]+)\)/)?.[1] || modelRef;

      // Check if model manager is available
      if (!this.modelManager) {
        await onChunk({
          type: 'error',
          error: 'ModelManager not available for streaming',
          timestamp: Date.now(),
        });
        return;
      }

      // Invoke model with streaming
      await this.modelManager.invokeModel(modelId, {
        message,
        system: payload.system,
        situation: payload.situation,
        stream: true,
        onToken: async (token: string) => {
          // Forward token to callback
          await onChunk({
            type: 'token',
            content: token,
            timestamp: Date.now(),
          });
        },
      });

      // Signal completion
      await onChunk({
        type: 'done',
        timestamp: Date.now(),
      });
    } catch (error: any) {
      // Handle errors gracefully
      await onChunk({
        type: 'error',
        error: error.message,
        timestamp: Date.now(),
      });
    }
  }
}

export default SessionActor;
