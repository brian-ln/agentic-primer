/**
 * ChatMessage Widget Actor
 *
 * Displays a single chat message with sender, text, and timestamp.
 * Leaf component (no ports, only receives messages).
 */

import { ActorMixin } from '../../../../src/messaging/browser/widget-actor.ts';
import type { Message, MessageResponse } from '../../../../src/messaging/message.ts';
import { signal, effect } from './signal.ts';

/**
 * ChatMessage component displays individual messages.
 *
 * Protocol:
 * - 'update' { text: string, sender: string, timestamp: number } - Update message content
 * - 'clear' - Clear message (optional, for future use)
 */
export class ChatMessage extends ActorMixin(HTMLElement) {
  // Reactive state using TC39 Signals
  private text = signal<string>('');
  private sender = signal<string>('');
  private timestamp = signal<number>(0);

  constructor() {
    super();

    // Attach shadow DOM for encapsulation
    this.attachShadow({ mode: 'open' });

    // Render on state changes
    effect(() => this.render());
  }

  /**
   * Called when component is added to DOM.
   * Auto-registers with actor registry.
   */
  connectedCallback(): void {
    super.connectedCallback();
    this.render();
  }

  /**
   * Actor protocol: receive messages.
   */
  async receive(msg: Message): Promise<MessageResponse> {
    switch (msg.type) {
      case 'update': {
        const { text, sender, timestamp } = msg.payload;
        this.text.set(text);
        this.sender.set(sender);
        this.timestamp.set(timestamp);
        this.render();
        return {
          id: `resp-${Date.now()}`,
          correlationId: msg.id,
          from: this.address,
          to: msg.from!,
          success: true,
          timestamp: Date.now()
        };
      }

      case 'clear': {
        this.text.set('');
        this.sender.set('');
        this.timestamp.set(0);
        this.render();
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
   * Render the component.
   */
  private render(): void {
    if (!this.shadowRoot) return;

    const textValue = this.text.get();
    const senderValue = this.sender.get();
    const timestampValue = this.timestamp.get();

    const timeStr = timestampValue
      ? new Date(timestampValue).toLocaleTimeString()
      : '';

    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: block;
          padding: 0.75rem;
          margin: 0.5rem 0;
          background: #f5f5f5;
          border-radius: 8px;
          border-left: 3px solid #007bff;
        }

        .message-header {
          display: flex;
          justify-content: space-between;
          margin-bottom: 0.5rem;
        }

        .sender {
          font-weight: bold;
          color: #007bff;
        }

        .timestamp {
          font-size: 0.85rem;
          color: #666;
        }

        .text {
          color: #333;
          line-height: 1.5;
        }

        .empty {
          color: #999;
          font-style: italic;
        }
      </style>

      <div class="message-header">
        <span class="sender">${senderValue || 'Unknown'}</span>
        <span class="timestamp">${timeStr}</span>
      </div>
      <div class="text ${textValue ? '' : 'empty'}">
        ${textValue || 'No message'}
      </div>
    `;
  }
}

// Register custom element
customElements.define('chat-message', ChatMessage);
