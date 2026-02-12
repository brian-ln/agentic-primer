/**
 * ChatRoom Widget Actor
 *
 * Container managing messages and users.
 * Orchestrates chat flow by:
 * - Receiving messages from ChatInput
 * - Creating ChatMessage components
 * - Broadcasting user list changes
 * - Managing chat state
 */

import { ActorMixin } from '../../../../src/messaging/browser/widget-actor.ts';
import type { Message, MessageResponse } from '../../../../src/messaging/message.ts';
import type { Channel } from '../../../../src/messaging/channel.ts';
import { createPortChannel } from '../../../../src/messaging/channels/index.ts';
import { address } from '../../../../src/messaging/message.ts';
import { signal } from './signal.ts';

interface ChatMessageData {
  id: string;
  text: string;
  sender: string;
  timestamp: number;
}

/**
 * ChatRoom component orchestrates the chat application.
 *
 * Protocol:
 * - 'send-message' { text: string, sender: string } - Add new message to room
 * - 'user-joined' { username: string } - Add user to room
 * - 'user-left' { username: string } - Remove user from room
 * - 'clear-messages' - Clear all messages
 *
 * Ports:
 * - 'messages' - Broadcasts new messages to subscribers
 * - 'users' - Broadcasts user list changes to subscribers
 */
export class ChatRoom extends ActorMixin(HTMLElement) {
  // Reactive state
  private messages = signal<ChatMessageData[]>([]);
  private users = signal<Set<string>>(new Set());

  // Ports for broadcasting
  private messagesPort = createPortChannel<ChatMessageData>();
  private usersPort = createPortChannel<string[]>();

  // DOM container for messages
  private messagesContainer: HTMLElement | null = null;

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback(): void {
    super.connectedCallback();
    this.render();

    // Subscribe to state changes
    this.messages.subscribe(() => this.renderMessages());
    this.users.subscribe(() => this.broadcastUserList());
  }

  /**
   * Actor protocol: receive messages.
   */
  async receive(msg: Message): Promise<MessageResponse> {
    switch (msg.type) {
      case 'send-message': {
        const { text, sender } = msg.payload;

        // Create message data
        const messageData: ChatMessageData = {
          id: `msg-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
          text,
          sender,
          timestamp: Date.now()
        };

        // Add to local state
        const currentMessages = this.messages.get();
        this.messages.set([...currentMessages, messageData]);

        // Broadcast to subscribers
        await this.messagesPort.send(messageData);

        return {
          id: `resp-${Date.now()}`,
          correlationId: msg.id,
          from: this.address,
          to: msg.from!,
          success: true,
          payload: { messageId: messageData.id },
          timestamp: Date.now()
        };
      }

      case 'user-joined': {
        const { username } = msg.payload;
        const currentUsers = this.users.get();

        if (!currentUsers.has(username)) {
          const newUsers = new Set(currentUsers);
          newUsers.add(username);
          this.users.set(newUsers);
        }

        return {
          id: `resp-${Date.now()}`,
          correlationId: msg.id,
          from: this.address,
          to: msg.from!,
          success: true,
          timestamp: Date.now()
        };
      }

      case 'user-left': {
        const { username } = msg.payload;
        const currentUsers = this.users.get();

        if (currentUsers.has(username)) {
          const newUsers = new Set(currentUsers);
          newUsers.delete(username);
          this.users.set(newUsers);
        }

        return {
          id: `resp-${Date.now()}`,
          correlationId: msg.id,
          from: this.address,
          to: msg.from!,
          success: true,
          timestamp: Date.now()
        };
      }

      case 'clear-messages': {
        this.messages.set([]);
        this.renderMessages();

        return {
          id: `resp-${Date.now()}`,
          correlationId: msg.id,
          from: this.address,
          to: msg.from!,
          success: true,
          timestamp: Date.now()
        };
      }

      default:
        return {
          id: `resp-${Date.now()}`,
          correlationId: msg.id,
          from: this.address,
          to: msg.from!,
          success: false,
          error: `Unknown message type: ${msg.type}`,
          timestamp: Date.now()
        };
    }
  }

  /**
   * Expose ports for external subscription.
   */
  port(name: string): Channel<any> {
    if (name === 'messages') {
      return this.messagesPort;
    }
    if (name === 'users') {
      return this.usersPort;
    }
    throw new Error(`Unknown port: ${name}`);
  }

  /**
   * Broadcast current user list to all subscribers.
   */
  private async broadcastUserList(): Promise<void> {
    const userList = Array.from(this.users.get());
    await this.usersPort.send(userList);
  }

  /**
   * Render messages as ChatMessage components.
   */
  private renderMessages(): void {
    if (!this.messagesContainer) {
      this.messagesContainer = this.shadowRoot?.querySelector('.messages-container') || null;
    }

    if (!this.messagesContainer) return;

    const messageList = this.messages.get();

    // Clear existing messages
    this.messagesContainer.innerHTML = '';

    if (messageList.length === 0) {
      this.messagesContainer.innerHTML = `
        <div class="empty-state">
          No messages yet. Start the conversation!
        </div>
      `;
      return;
    }

    // Create ChatMessage components for each message
    messageList.forEach((msgData) => {
      const messageElement = document.createElement('chat-message') as any;
      messageElement.id = msgData.id;
      this.messagesContainer!.appendChild(messageElement);

      // Send update message to the ChatMessage actor
      // This demonstrates actor-to-actor communication
      setTimeout(async () => {
        try {
          const msgAddress = address(`widgets/chat-message:${msgData.id}`);
          await this.sendMessage(msgAddress, 'update', msgData);
        } catch (error) {
          console.warn('Failed to update message component:', error);
        }
      }, 0);
    });

    // Auto-scroll to bottom
    this.messagesContainer.scrollTop = this.messagesContainer.scrollHeight;
  }

  /**
   * Render the component structure.
   */
  private render(): void {
    if (!this.shadowRoot) return;

    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: flex;
          flex-direction: column;
          height: 100%;
          background: white;
        }

        .header {
          padding: 1rem;
          background: #007bff;
          color: white;
          font-size: 1.2rem;
          font-weight: bold;
          border-bottom: 2px solid #0056b3;
        }

        .messages-container {
          flex: 1;
          overflow-y: auto;
          padding: 1rem;
          background: white;
        }

        .empty-state {
          color: #999;
          font-style: italic;
          text-align: center;
          padding: 2rem;
        }

        /* Scrollbar styling */
        .messages-container::-webkit-scrollbar {
          width: 8px;
        }

        .messages-container::-webkit-scrollbar-track {
          background: #f1f1f1;
        }

        .messages-container::-webkit-scrollbar-thumb {
          background: #888;
          border-radius: 4px;
        }

        .messages-container::-webkit-scrollbar-thumb:hover {
          background: #555;
        }
      </style>

      <div class="header">Chat Room</div>
      <div class="messages-container"></div>
    `;

    // Get container reference
    this.messagesContainer = this.shadowRoot.querySelector('.messages-container');
  }
}

// Register custom element
customElements.define('chat-room', ChatRoom);
