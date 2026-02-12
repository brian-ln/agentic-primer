# Widget Actor Chat Demo

A comprehensive browser-based chat application demonstrating the **Widget Actor** pattern: Web Components that communicate via the unified actor protocol.

## Overview

This demo shows how Widget Actors enable component collaboration through message-passing instead of direct DOM manipulation. Each component is an independent actor with its own address, message handler, and optional reactive ports.

## Architecture

### Widget Actor Pattern

Widget Actors combine three key concepts:

1. **Web Components** - Encapsulated, reusable UI elements
2. **Actor Protocol** - Asynchronous message-passing communication
3. **TC39 Signals** - Reactive internal state management

```
┌─────────────────────────────────────────────────────────────┐
│                     Widget Actor                            │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │
│  │  HTMLElement │  │Actor Protocol│  │ TC39 Signals │    │
│  │   (view)     │  │ (messaging)  │  │  (reactivity)│    │
│  └──────────────┘  └──────────────┘  └──────────────┘    │
│                                                             │
│  Address: @(component-type:id)                             │
│  Messages: receive(msg) → response                         │
│  Ports: port(name) → Channel<T>                            │
└─────────────────────────────────────────────────────────────┘
```

### Components

#### 1. ChatMessage (Leaf Component)

Displays individual messages.

**Protocol:**
- `update` `{ text, sender, timestamp }` - Update message content
- `clear` - Clear the message

**No ports** (leaf component)

**Address:** `@(chat-message:msg-id)`

#### 2. ChatInput (Input Component)

Message composition widget with input field and submit button.

**Protocol:**
- `clear` - Clear input field
- `set-username` `{ username }` - Set current username

**Ports:**
- `submit` - Broadcasts `{ text, sender }` when user submits

**Address:** `@(chat-input:chat-input)`

**DOM Events → Actor Messages:**
- Button click → port broadcast
- Enter key → port broadcast

#### 3. ChatRoom (Orchestrator)

Central coordinator managing messages and users.

**Protocol:**
- `send-message` `{ text, sender }` - Add new message
- `user-joined` `{ username }` - Add user
- `user-left` `{ username }` - Remove user
- `clear-messages` - Clear all messages

**Ports:**
- `messages` - Broadcasts new messages
- `users` - Broadcasts user list changes

**Address:** `@(chat-room:chat-room)`

#### 4. UserList (Display Component)

Displays connected users in sidebar.

**Protocol:**
- `update-users` `{ users: string[] }` - Update user list
- `add-user` `{ username }` - Add single user
- `remove-user` `{ username }` - Remove single user

**No ports** (display only)

**Address:** `@(user-list:user-list)`

## Message Flow

### Sending a Message

```
User types message and presses Enter
  ↓
ChatInput (DOM event handler)
  ↓
ChatInput.submitPort.send({ text, sender })
  ↓
App subscription handler
  ↓
actorRegistry.send(chatRoom.address, { type: 'send-message', ... })
  ↓
ChatRoom.receive(msg)
  ↓
ChatRoom creates ChatMessage component
  ↓
ChatRoom.messagesPort.send(messageData)
  ↓
actorRegistry.send(chatMessage.address, { type: 'update', ... })
  ↓
ChatMessage.receive(msg)
  ↓
ChatMessage updates internal signals
  ↓
ChatMessage renders updated UI
```

### User List Updates

```
ChatRoom user state changes
  ↓
ChatRoom.users signal subscriber triggered
  ↓
ChatRoom.usersPort.send(userList)
  ↓
App subscription handler
  ↓
actorRegistry.send(userList.address, { type: 'update-users', ... })
  ↓
UserList.receive(msg)
  ↓
UserList updates internal signals
  ↓
UserList renders updated UI
```

## Key Patterns Demonstrated

### 1. Point-to-Point Messaging

Direct actor-to-actor communication via registry:

```typescript
await actorRegistry.send(targetAddress, {
  id: generateMessageId(),
  pattern: 'tell',
  from: this.address,
  to: targetAddress,
  type: 'send-message',
  payload: { text: 'Hello' },
  timestamp: Date.now()
});
```

### 2. Broadcasting via Ports

One-to-many communication using pub/sub:

```typescript
// Producer side (ChatInput)
private submitPort = createPortChannel<{ text: string; sender: string }>();

await this.submitPort.send({ text, sender });

// Consumer side (App)
const submitChannel = chatInput.port('submit').subscribe();
for await (const data of submitChannel) {
  // Handle submission
}
```

### 3. Signal-to-Channel Bridges

DOM events become actor messages:

```typescript
// ChatInput component
this.button.addEventListener('click', () => {
  this.handleSubmit(); // Broadcasts to port
});

// App wires port to ChatRoom actor
for await (const { text, sender } of submitChannel) {
  await actorRegistry.send(chatRoom.address, {
    type: 'send-message',
    payload: { text, sender }
  });
}
```

### 4. Reactive State Management

Internal component state uses TC39 Signals:

```typescript
class ChatMessage extends ActorMixin(HTMLElement) {
  private text = signal<string>('');
  private sender = signal<string>('');

  async receive(msg: Message) {
    if (msg.type === 'update') {
      this.text.set(msg.payload.text);
      this.sender.set(msg.payload.sender);
      this.render(); // Updates UI
    }
  }
}
```

### 5. Lifecycle Management

Automatic registration/cleanup:

```typescript
// ActorMixin handles this automatically
connectedCallback() {
  super.connectedCallback(); // Auto-registers with actorRegistry
}

disconnectedCallback() {
  super.disconnectedCallback(); // Auto-unregisters
}
```

## Usage

### Running the Demo

Serve the directory with a development server:

```bash
# From project root
bun --hot examples/widget-actors/chat/
```

Or with any static server:

```bash
# Python
python -m http.server 8000

# Node.js
npx serve
```

Open `http://localhost:8000` (or appropriate port) in your browser.

### Interacting with the Demo

1. **Type messages** in the input field and press Enter or click Send
2. **View messages** appearing in the chat room
3. **See user list** in the right sidebar
4. **Open debug console** by clicking the "Debug Console" button

### Debug Console Commands

Open browser DevTools console and use:

```javascript
// Send a test message
chatDebug.sendTestMessage("Hello from console!");

// Add a user
chatDebug.addUser("Alice");

// Remove a user
chatDebug.removeUser("Alice");

// Clear all messages
chatDebug.clearMessages();

// List registered actors
chatDebug.registry.list();
// Returns: ["@(chat-input:chat-input)", "@(chat-room:chat-room)", ...]

// Inspect specific actor
chatDebug.actors.chatRoom
// Returns: "@(chat-room:chat-room)"
```

## File Structure

```
examples/widget-actors/chat/
├── index.html              # Demo page
├── README.md               # This file
├── app.ts                  # App initialization & wiring
└── components/
    ├── signal.ts           # TC39 Signal implementation
    ├── chat-message.ts     # Message display component
    ├── chat-input.ts       # Input widget
    ├── chat-room.ts        # Room orchestrator
    └── user-list.ts        # User sidebar
```

## Implementation Details

### Signal Implementation

Minimal TC39 Signals-compatible implementation:

```typescript
class Signal<T> {
  private value: T;
  private subscribers = new Set<(val: T) => void>();

  get(): T { return this.value; }

  set(newValue: T): void {
    if (this.value !== newValue) {
      this.value = newValue;
      this.subscribers.forEach(fn => fn(newValue));
    }
  }

  subscribe(fn: (val: T) => void): () => void {
    this.subscribers.add(fn);
    return () => this.subscribers.delete(fn);
  }
}
```

### Actor Registration

ActorMixin handles automatic registration:

```typescript
// Automatically generates address from tag name and ID
this.address = address(`${tagName}:${elementId}`);

// Auto-registers on mount
connectedCallback() {
  actorRegistry.register(this.address, this);
}

// Auto-unregisters on unmount
disconnectedCallback() {
  actorRegistry.unregister(this.address);
}
```

### Port Creation

Components create ports for broadcasting:

```typescript
class ChatRoom extends ActorMixin(HTMLElement) {
  private messagesPort = createPortChannel<MessageData>();

  port(name: string): Channel<any> {
    if (name === 'messages') return this.messagesPort;
    throw new Error(`Unknown port: ${name}`);
  }

  async broadcastMessage(msg: MessageData) {
    await this.messagesPort.send(msg);
  }
}
```

### Message Handling

Components implement receive() for actor protocol:

```typescript
async receive(msg: Message): Promise<MessageResponse> {
  switch (msg.type) {
    case 'send-message': {
      // Handle message
      return {
        id: generateMessageId(),
        correlationId: msg.id,
        from: this.address,
        to: msg.from!,
        success: true,
        timestamp: Date.now()
      };
    }
    default:
      return {
        success: false,
        error: `Unknown message type: ${msg.type}`
      };
  }
}
```

## Benefits of Widget Actors

### 1. Decoupling

Components don't know about each other's implementation. They only know message protocols.

### 2. Composability

New components can subscribe to existing ports without modifying existing code.

### 3. Testability

Each component can be tested in isolation by sending messages.

### 4. Debugging

Message flow is explicit and inspectable via actor registry.

### 5. Scalability

Add new components without increasing coupling. Communication remains consistent.

## Future Enhancements

This demo focuses on core Widget Actor patterns. Potential additions:

- **Backend Integration** - WebSocket bridge to backend actors
- **Persistence** - IndexedDB for message history
- **Authentication** - Real user login
- **Rich Media** - Image/file sharing
- **Notifications** - Desktop notifications via Notification API
- **Typing Indicators** - User activity status
- **Message Reactions** - Emoji reactions via actor messages
- **Private Messages** - Direct messaging between users

## Related Concepts

- **TC39 Signals Proposal** - Standard for reactive state
- **Actor Model** - Concurrent computation via message-passing
- **Web Components** - Standard for reusable UI components
- **Channel Pattern** - Async communication primitive

## Learn More

- [Actor Model](https://en.wikipedia.org/wiki/Actor_model)
- [TC39 Signals Proposal](https://github.com/tc39/proposal-signals)
- [Web Components](https://developer.mozilla.org/en-US/docs/Web/Web_Components)
- [MessagePort API](https://developer.mozilla.org/en-US/docs/Web/API/MessagePort)

## License

This demo is part of the agentic-primer project.
