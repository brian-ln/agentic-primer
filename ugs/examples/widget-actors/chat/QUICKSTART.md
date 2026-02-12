# Widget Actor Chat Demo - Quick Start

Get the demo running in 2 minutes.

## Step 1: Start Server

From the project root:

```bash
cd /Users/bln/play/agentic-primer/simplify
bun --hot examples/widget-actors/chat/
```

You should see:

```
$ bun --hot examples/widget-actors/chat/
Listening on http://localhost:3000
```

## Step 2: Open Browser

Navigate to: **http://localhost:3000/examples/widget-actors/chat/**

You should see:
- Purple gradient header: "Widget Actor Chat Demo"
- Empty chat room
- User list showing "1 online"
- Message input at bottom

## Step 3: Send Your First Message

1. Click the input field at the bottom
2. Type: `Hello Widget Actors!`
3. Press **Enter** or click **Send**

You should see:
- Your message appears in the chat
- Shows your username (e.g., "User742")
- Shows current timestamp
- Input field clears

## Step 4: Open Debug Console

1. Click the **"Debug Console"** button (bottom left)
2. See registered actors:
   - `@(chat-input:chat-input)`
   - `@(chat-room:chat-room)`
   - `@(user-list:user-list)`

## Step 5: Try Console Commands

Open browser DevTools (F12) and type:

```javascript
// Send a test message
chatDebug.sendTestMessage("Testing the actor protocol!");

// Add users
chatDebug.addUser("Alice");
chatDebug.addUser("Bob");

// See user list update to "3 online"

// Remove a user
chatDebug.removeUser("Alice");

// Clear all messages
chatDebug.clearMessages();
```

## That's It!

You're now running a fully functional Widget Actor chat application.

## Next Steps

- **Read the architecture:** See `README.md` for detailed explanation
- **Explore components:** Check `components/` directory
- **Run tests:** Follow `TESTING.md` for comprehensive testing
- **View implementation:** See `IMPLEMENTATION.md` for technical details

## Common Issues

### Page won't load
- Ensure dev server is running
- Check URL: `http://localhost:3000/examples/widget-actors/chat/`
- Look for port conflicts

### Components don't register
- Open DevTools Console
- Look for JavaScript errors
- Verify all `.ts` files loaded

### Messages don't send
- Check Debug Console shows 3 actors
- Look for console errors
- Verify actor registry: `chatDebug.registry.list()`

## Key Concepts in 30 Seconds

**Widget Actors** = Web Components + Actor Protocol + Reactive Signals

```
User types → DOM Event → ChatInput → Port Broadcast
                                         ↓
                                    App Listener
                                         ↓
                                    ChatRoom Actor
                                         ↓
                                    Creates ChatMessage
                                         ↓
                                    Updates UserList
```

Every component is an **actor** with:
- Unique address: `@(component-type:id)`
- Message handler: `receive(msg)`
- Optional ports: `port(name)`
- Reactive state: TC39 Signals

## Explore the Code

Start with these files:

1. **index.html** - See how components are used
2. **app.ts** - See how actors are wired together
3. **components/chat-input.ts** - See DOM events → actor messages
4. **components/chat-room.ts** - See port broadcasting

## More Help

- Full docs: `README.md`
- Testing guide: `TESTING.md`
- Implementation details: `IMPLEMENTATION.md`

Enjoy exploring Widget Actors!
