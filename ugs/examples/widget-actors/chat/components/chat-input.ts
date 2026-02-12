/**
 * ChatInput Widget Actor
 *
 * Message composition widget with input field and submit button.
 * Exposes 'submit' port that broadcasts when user submits message.
 */

import { ActorMixin } from '../../../../src/messaging/browser/widget-actor.ts';
import type { Message, MessageResponse } from '../../../../src/messaging/message.ts';
import type { Channel } from '../../../../src/messaging/channel.ts';
import { createPortChannel } from '../../../../src/messaging/channels/index.ts';
import { signal } from './signal.ts';

/**
 * ChatInput component for message composition.
 *
 * Protocol:
 * - 'clear' - Clear input field
 * - 'set-username' { username: string } - Set current username
 *
 * Ports:
 * - 'submit' - Broadcasts { text: string, sender: string } when user submits
 */
export class ChatInput extends ActorMixin(HTMLElement) {
  // Reactive state
  private username = signal<string>('Anonymous');
  private inputValue = signal<string>('');

  // Port for broadcasting submit events
  private submitPort = createPortChannel<{ text: string; sender: string }>();

  // DOM references
  private input: HTMLInputElement | null = null;
  private button: HTMLButtonElement | null = null;

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback(): void {
    super.connectedCallback();
    this.render();
    this.attachEventHandlers();
  }

  /**
   * Actor protocol: receive messages.
   */
  async receive(msg: Message): Promise<MessageResponse> {
    switch (msg.type) {
      case 'clear': {
        this.inputValue.set('');
        if (this.input) {
          this.input.value = '';
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

      case 'set-username': {
        const { username } = msg.payload;
        this.username.set(username);
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
    if (name === 'submit') {
      return this.submitPort;
    }
    throw new Error(`Unknown port: ${name}`);
  }

  /**
   * Handle message submission.
   */
  private async handleSubmit(): Promise<void> {
    if (!this.input) return;

    const text = this.input.value.trim();
    if (!text) return;

    // Broadcast submit event to all subscribers
    await this.submitPort.send({
      text,
      sender: this.username.get()
    });

    // Clear input
    this.input.value = '';
    this.inputValue.set('');
  }

  /**
   * Attach event handlers to DOM elements.
   */
  private attachEventHandlers(): void {
    if (!this.shadowRoot) return;

    this.input = this.shadowRoot.querySelector('input');
    this.button = this.shadowRoot.querySelector('button');

    if (!this.input || !this.button) return;

    // Handle button click
    this.button.addEventListener('click', () => {
      this.handleSubmit();
    });

    // Handle Enter key in input
    this.input.addEventListener('keypress', (e: KeyboardEvent) => {
      if (e.key === 'Enter') {
        this.handleSubmit();
      }
    });

    // Track input value
    this.input.addEventListener('input', (e: Event) => {
      const target = e.target as HTMLInputElement;
      this.inputValue.set(target.value);
    });
  }

  /**
   * Render the component.
   */
  private render(): void {
    if (!this.shadowRoot) return;

    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: block;
          padding: 1rem;
          background: white;
          border-top: 2px solid #e0e0e0;
        }

        .input-container {
          display: flex;
          gap: 0.5rem;
        }

        input {
          flex: 1;
          padding: 0.75rem;
          border: 2px solid #e0e0e0;
          border-radius: 4px;
          font-size: 1rem;
          outline: none;
          transition: border-color 0.2s;
        }

        input:focus {
          border-color: #007bff;
        }

        button {
          padding: 0.75rem 1.5rem;
          background: #007bff;
          color: white;
          border: none;
          border-radius: 4px;
          font-size: 1rem;
          font-weight: bold;
          cursor: pointer;
          transition: background 0.2s;
        }

        button:hover {
          background: #0056b3;
        }

        button:active {
          background: #004085;
        }
      </style>

      <div class="input-container">
        <input
          type="text"
          placeholder="Type your message..."
          autocomplete="off"
        />
        <button>Send</button>
      </div>
    `;
  }
}

// Register custom element
customElements.define('chat-input', ChatInput);
