/**
 * Chat Application Initialization
 *
 * Wires together Widget Actors to create a functioning chat application.
 * Demonstrates:
 * - Point-to-point messaging (ChatInput → ChatRoom)
 * - Broadcasting via ports (ChatRoom → UserList)
 * - Actor registry usage
 */

import { actorRegistry } from '../../../src/messaging/browser/actor-registry.ts';
import { address } from '../../../src/messaging/message.ts';

// Import components (registers them as custom elements)
import './components/chat-message.ts';
import './components/chat-input.ts';
import './components/chat-room.ts';
import './components/user-list.ts';

/**
 * Initialize the chat application.
 * Sets up actor communication flows.
 */
export async function initChatApp() {
  console.log('Initializing Widget Actor Chat Demo...');

  // Get references to Widget Actors
  const chatInput = document.getElementById('chat-input') as any;
  const chatRoom = document.getElementById('chat-room') as any;
  const userList = document.getElementById('user-list') as any;

  if (!chatInput || !chatRoom || !userList) {
    console.error('Missing required components in DOM');
    return;
  }

  // Wait for components to be fully registered
  await customElements.whenDefined('chat-input');
  await customElements.whenDefined('chat-room');
  await customElements.whenDefined('user-list');

  console.log('Components registered. Setting up actor communication...');

  // Set up username (could be from a login form in real app)
  const defaultUsername = `User${Math.floor(Math.random() * 1000)}`;

  try {
    await actorRegistry.send(chatInput.address, {
      id: `msg-${Date.now()}-${Math.random()}`,
      pattern: 'tell' as const,
      from: address('domain/app:initializer'),
      to: chatInput.address,
      type: 'set-username',
      payload: { username: defaultUsername },
      timestamp: Date.now()
    });

    // Add user to room
    await actorRegistry.send(chatRoom.address, {
      id: `msg-${Date.now()}-${Math.random()}`,
      pattern: 'tell' as const,
      from: address('domain/app:initializer'),
      to: chatRoom.address,
      type: 'user-joined',
      payload: { username: defaultUsername },
      timestamp: Date.now()
    });

    console.log(`User "${defaultUsername}" joined the chat`);
  } catch (error) {
    console.error('Failed to initialize user:', error);
  }

  // Wire ChatInput submit port → ChatRoom send-message
  // This demonstrates port subscription pattern
  const submitPort = chatInput.port('submit');
  const submitChannel = submitPort.subscribe();

  (async () => {
    try {
      for await (const { text, sender } of submitChannel) {
        console.log(`ChatInput submitted: "${text}" from ${sender}`);

        // Send message to ChatRoom actor
        await actorRegistry.send(chatRoom.address, {
          id: `msg-${Date.now()}-${Math.random()}`,
          pattern: 'tell' as const,
          from: chatInput.address,
          to: chatRoom.address,
          type: 'send-message',
          payload: { text, sender },
          timestamp: Date.now()
        });
      }
    } catch (error) {
      console.error('ChatInput subscription error:', error);
    }
  })();

  // Wire ChatRoom users port → UserList updates
  // This demonstrates pub/sub broadcasting
  const usersPort = chatRoom.port('users');
  const usersChannel = usersPort.subscribe();

  (async () => {
    try {
      for await (const users of usersChannel) {
        console.log('User list updated:', users);

        // Update UserList component
        await actorRegistry.send(userList.address, {
          id: `msg-${Date.now()}-${Math.random()}`,
          pattern: 'tell' as const,
          from: chatRoom.address,
          to: userList.address,
          type: 'update-users',
          payload: { users },
          timestamp: Date.now()
        });
      }
    } catch (error) {
      console.error('Users subscription error:', error);
    }
  })();

  // Log actor registry state
  console.log('Registered actors:', actorRegistry.list());
  console.log('Chat app initialized successfully!');

  // Expose debug utilities to window
  (window as any).chatDebug = {
    registry: actorRegistry,
    actors: {
      chatInput: chatInput.address,
      chatRoom: chatRoom.address,
      userList: userList.address
    },
    sendTestMessage: async (text: string) => {
      await actorRegistry.send(chatRoom.address, {
        id: `msg-${Date.now()}-${Math.random()}`,
        pattern: 'tell' as const,
        from: address('domain/debug:console'),
        to: chatRoom.address,
        type: 'send-message',
        payload: { text, sender: 'Debug' },
        timestamp: Date.now()
      });
    },
    addUser: async (username: string) => {
      await actorRegistry.send(chatRoom.address, {
        id: `msg-${Date.now()}-${Math.random()}`,
        pattern: 'tell' as const,
        from: address('domain/debug:console'),
        to: chatRoom.address,
        type: 'user-joined',
        payload: { username },
        timestamp: Date.now()
      });
    },
    removeUser: async (username: string) => {
      await actorRegistry.send(chatRoom.address, {
        id: `msg-${Date.now()}-${Math.random()}`,
        pattern: 'tell' as const,
        from: address('domain/debug:console'),
        to: chatRoom.address,
        type: 'user-left',
        payload: { username },
        timestamp: Date.now()
      });
    },
    clearMessages: async () => {
      await actorRegistry.send(chatRoom.address, {
        id: `msg-${Date.now()}-${Math.random()}`,
        pattern: 'tell' as const,
        from: address('domain/debug:console'),
        to: chatRoom.address,
        type: 'clear-messages',
        payload: {},
        timestamp: Date.now()
      });
    }
  };

  console.log('Debug utilities available at window.chatDebug');
}

// Auto-initialize when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => initChatApp());
} else {
  initChatApp();
}
