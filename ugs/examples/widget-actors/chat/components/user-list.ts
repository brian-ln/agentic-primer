/**
 * UserList Widget Actor
 *
 * Displays list of connected users in a sidebar.
 * Display-only component (receives updates, no ports).
 */

import { ActorMixin } from '../../../../src/messaging/browser/widget-actor.ts';
import type { Message, MessageResponse } from '../../../../src/messaging/message.ts';
import { signal } from './signal.ts';

/**
 * UserList component displays connected users.
 *
 * Protocol:
 * - 'update-users' { users: string[] } - Update the user list
 * - 'add-user' { username: string } - Add a single user
 * - 'remove-user' { username: string } - Remove a single user
 */
export class UserList extends ActorMixin(HTMLElement) {
  // Reactive state
  private users = signal<string[]>([]);

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback(): void {
    super.connectedCallback();
    this.render();

    // Subscribe to users signal for reactive rendering
    this.users.subscribe(() => this.render());
  }

  /**
   * Actor protocol: receive messages.
   */
  async receive(msg: Message): Promise<MessageResponse> {
    switch (msg.type) {
      case 'update-users': {
        const { users } = msg.payload;
        if (Array.isArray(users)) {
          this.users.set([...users]);
          return {
            id: `resp-${Date.now()}`,
            correlationId: msg.id,
            from: this.address,
            to: msg.from!,
            success: true,
            timestamp: Date.now()
          };
        }
        return {
          id: `resp-${Date.now()}`,
          correlationId: msg.id,
          from: this.address,
          to: msg.from!,
          success: false,
          error: 'Invalid users array',
          timestamp: Date.now()
        };
      }

      case 'add-user': {
        const { username } = msg.payload;
        const currentUsers = this.users.get();
        if (!currentUsers.includes(username)) {
          this.users.set([...currentUsers, username]);
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

      case 'remove-user': {
        const { username } = msg.payload;
        const currentUsers = this.users.get();
        this.users.set(currentUsers.filter(u => u !== username));
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

    const userList = this.users.get();
    const userCount = userList.length;

    const userItems = userList
      .map(
        (username) => `
        <li class="user-item">
          <span class="user-status"></span>
          <span class="username">${username}</span>
        </li>
      `
      )
      .join('');

    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: block;
          padding: 1rem;
          background: #f9f9f9;
          border-left: 2px solid #e0e0e0;
        }

        .header {
          font-weight: bold;
          font-size: 1.1rem;
          margin-bottom: 0.5rem;
          color: #333;
        }

        .user-count {
          font-size: 0.9rem;
          color: #666;
          margin-bottom: 1rem;
        }

        ul {
          list-style: none;
          padding: 0;
          margin: 0;
        }

        .user-item {
          display: flex;
          align-items: center;
          padding: 0.5rem;
          margin-bottom: 0.25rem;
          border-radius: 4px;
          transition: background 0.2s;
        }

        .user-item:hover {
          background: #e8e8e8;
        }

        .user-status {
          width: 8px;
          height: 8px;
          border-radius: 50%;
          background: #28a745;
          margin-right: 0.5rem;
        }

        .username {
          color: #333;
        }

        .empty-state {
          color: #999;
          font-style: italic;
          padding: 1rem;
          text-align: center;
        }
      </style>

      <div class="header">Users</div>
      <div class="user-count">${userCount} online</div>
      ${
        userCount > 0
          ? `<ul>${userItems}</ul>`
          : '<div class="empty-state">No users online</div>'
      }
    `;
  }
}

// Register custom element
customElements.define('user-list', UserList);
