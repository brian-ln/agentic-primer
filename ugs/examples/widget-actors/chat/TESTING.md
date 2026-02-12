# Testing the Widget Actor Chat Demo

## Quick Start

### 1. Start Development Server

From the project root:

```bash
cd /Users/bln/play/agentic-primer/simplify
bun --hot examples/widget-actors/chat/
```

The application will be available at `http://localhost:3000` (or the port Bun assigns).

### 2. Open in Browser

Navigate to `http://localhost:3000/examples/widget-actors/chat/` in your web browser.

## Manual Testing Checklist

### Basic Functionality

- [ ] **Page loads without errors**
  - Open browser DevTools Console (F12)
  - Verify no JavaScript errors
  - Check console for "Chat app initialized successfully!"

- [ ] **Components render correctly**
  - Chat room header visible
  - Message input field at bottom
  - User list sidebar on right
  - Send button present

- [ ] **Actor registration**
  - Open Debug Console (click button)
  - Verify 3 actors registered:
    - `@(chat-input:chat-input)`
    - `@(chat-room:chat-room)`
    - `@(user-list:user-list)`

### Message Flow

- [ ] **Send a message**
  - Type "Hello World" in input field
  - Press Enter or click Send
  - Message appears in chat room
  - Message shows correct sender name
  - Message shows timestamp
  - Input field clears after send

- [ ] **Multiple messages**
  - Send 5+ messages
  - All messages appear in order
  - Messages auto-scroll to bottom
  - Each message has unique styling

- [ ] **Empty message handling**
  - Try sending empty message
  - Should not create a message (or show validation)

### User Management

- [ ] **Initial user**
  - User list shows 1 user on load
  - Username shows pattern "UserXXX" (random)
  - Green status indicator visible

- [ ] **Add users via console**
  ```javascript
  chatDebug.addUser("Alice")
  chatDebug.addUser("Bob")
  ```
  - User list updates to show 3 users
  - Count shows "3 online"

- [ ] **Remove users via console**
  ```javascript
  chatDebug.removeUser("Alice")
  ```
  - User list updates
  - Count decrements

### Actor Communication

- [ ] **Port broadcasting**
  - Open browser console
  - Watch for "ChatInput submitted: ..." logs
  - Watch for "User list updated: ..." logs
  - Verify message flow is logged

- [ ] **Actor registry**
  ```javascript
  chatDebug.registry.list()
  ```
  - Returns array of registered actors
  - Includes all component addresses

- [ ] **Direct actor messaging**
  ```javascript
  chatDebug.sendTestMessage("Testing actor protocol")
  ```
  - Message appears in chat room
  - Sender shows "Debug"

### UI/UX

- [ ] **Responsive design**
  - Resize browser window
  - Layout adapts appropriately
  - Mobile view stacks user list below chat

- [ ] **Shadow DOM encapsulation**
  - Inspect elements in DevTools
  - Each component has shadow root
  - Styles are scoped per component

- [ ] **Keyboard shortcuts**
  - Enter key submits message
  - Tab navigates between input and button

### Debug Features

- [ ] **Debug console toggle**
  - Click "Debug Console" button
  - Panel appears with actor list
  - Auto-updates every 2 seconds

- [ ] **Console commands**
  All commands work without errors:
  ```javascript
  chatDebug.sendTestMessage("Test")
  chatDebug.addUser("TestUser")
  chatDebug.removeUser("TestUser")
  chatDebug.clearMessages()
  chatDebug.registry.list()
  ```

## Browser Console Testing

Open DevTools Console and run these tests:

### Test 1: Actor Registration

```javascript
// List all registered actors
const actors = chatDebug.registry.list();
console.log('Registered actors:', actors);

// Should show 3 actors
console.assert(actors.length >= 3, 'Expected at least 3 actors');
```

### Test 2: Send Multiple Messages

```javascript
// Send 5 test messages
for (let i = 0; i < 5; i++) {
  await chatDebug.sendTestMessage(`Test message ${i + 1}`);
  await new Promise(r => setTimeout(r, 100));
}
```

### Test 3: User List Management

```javascript
// Add multiple users
const users = ['Alice', 'Bob', 'Charlie', 'Diana'];
for (const user of users) {
  await chatDebug.addUser(user);
}

// Verify user count
// Should show in UI: "5 online" (4 new + 1 default)

// Remove users
for (const user of users) {
  await chatDebug.removeUser(user);
}
```

### Test 4: Clear and Reset

```javascript
// Clear all messages
await chatDebug.clearMessages();

// Verify chat room is empty
// Should show "No messages yet. Start the conversation!"
```

### Test 5: Actor Lookup

```javascript
// Look up specific actors
const chatRoom = chatDebug.registry.lookup('@(chat-room:chat-room)');
console.log('ChatRoom actor:', chatRoom);

const chatInput = chatDebug.registry.lookup('@(chat-input:chat-input)');
console.log('ChatInput actor:', chatInput);
```

## Testing Actor Protocol Directly

### Send Messages to Actors

```javascript
// Get actor addresses
const roomAddress = chatDebug.actors.chatRoom;
const inputAddress = chatDebug.actors.chatInput;

// Send message directly to ChatRoom
await chatDebug.registry.send(roomAddress, {
  id: `msg-${Date.now()}`,
  pattern: 'tell',
  from: '@(test:console)',
  to: roomAddress,
  type: 'send-message',
  payload: { text: 'Direct actor message!', sender: 'Test' },
  timestamp: Date.now()
});

// Clear input field
await chatDebug.registry.send(inputAddress, {
  id: `msg-${Date.now()}`,
  pattern: 'tell',
  from: '@(test:console)',
  to: inputAddress,
  type: 'clear',
  payload: {},
  timestamp: Date.now()
});
```

## Performance Testing

### Message Throughput

```javascript
// Send 100 messages rapidly
console.time('100 messages');
for (let i = 0; i < 100; i++) {
  await chatDebug.sendTestMessage(`Perf test ${i}`);
}
console.timeEnd('100 messages');

// Should complete in < 5 seconds
// Messages should render smoothly
```

### Memory Leaks

```javascript
// Add and remove 1000 messages
for (let batch = 0; batch < 10; batch++) {
  for (let i = 0; i < 100; i++) {
    await chatDebug.sendTestMessage(`Batch ${batch} msg ${i}`);
  }
  await chatDebug.clearMessages();
}

// Monitor DevTools Memory tab
// Heap should stabilize after GC
```

## Browser Compatibility

Test in multiple browsers:

- [ ] Chrome/Chromium
- [ ] Firefox
- [ ] Safari
- [ ] Edge

Verify:
- Web Components support (native)
- Shadow DOM works
- ES Modules load correctly
- No console errors

## Network Testing

### Offline Behavior

1. Open DevTools Network tab
2. Enable "Offline" mode
3. Verify app still works (local only)
4. All interactions should function normally

### Module Loading

1. Open Network tab
2. Reload page
3. Verify modules load:
   - app.ts
   - chat-message.ts
   - chat-input.ts
   - chat-room.ts
   - user-list.ts
   - signal.ts

## Error Cases

### Invalid Messages

```javascript
// Send invalid message type
await chatDebug.registry.send(
  chatDebug.actors.chatRoom,
  {
    id: 'test',
    pattern: 'tell',
    from: '@(test)',
    to: chatDebug.actors.chatRoom,
    type: 'invalid-type',
    payload: {},
    timestamp: Date.now()
  }
);

// Should return error response, not crash
```

### Missing Actors

```javascript
// Try to send to non-existent actor
try {
  await chatDebug.registry.send('@(does-not-exist)', {
    id: 'test',
    pattern: 'tell',
    to: '@(does-not-exist)',
    type: 'test',
    payload: {},
    timestamp: Date.now()
  });
} catch (error) {
  console.log('Expected error:', error.message);
  // Should throw ActorNotFoundError
}
```

## Visual Testing

### Component Rendering

1. Open DevTools Elements tab
2. Inspect `<chat-message>` elements
3. Verify:
   - Shadow DOM present
   - Styles applied correctly
   - Content renders properly

### CSS Isolation

1. Inspect multiple `<chat-message>` components
2. Verify each has independent shadow root
3. Styles don't leak between components

## Integration Testing

### Full User Flow

1. Load page
2. Observe initial state:
   - 1 user (random UserXXX)
   - No messages
3. Type and send first message
4. Verify message appears with correct:
   - Text
   - Sender
   - Timestamp
5. Add user via console
6. Verify user list updates
7. Send more messages
8. Clear messages via console
9. Verify chat clears

## Debugging Tips

### Console Logging

Watch for these log messages:

```
Initializing Widget Actor Chat Demo...
Components registered. Setting up actor communication...
User "UserXXX" joined the chat
ChatInput submitted: "message text" from UserXXX
User list updated: ["UserXXX"]
Registered actors: [...]
Chat app initialized successfully!
```

### Common Issues

**Issue:** Components don't render
- Check console for module loading errors
- Verify file paths in imports
- Ensure dev server is serving from correct directory

**Issue:** Messages don't appear
- Check actor registry in debug console
- Verify ChatRoom actor is registered
- Look for JavaScript errors in console

**Issue:** User list not updating
- Check ChatRoom users port subscription
- Verify UserList actor receives messages
- Look for connection between port and actor

## Success Criteria

All tests pass when:

✅ Page loads without errors
✅ All 3 actors register successfully
✅ Messages send and display correctly
✅ User list updates properly
✅ Actor communication works (console logs show message flow)
✅ Debug console shows registered actors
✅ All debug commands execute without errors
✅ Shadow DOM encapsulation works
✅ Responsive design functions
✅ No memory leaks after stress testing

## Cleanup

After testing:

```bash
# Stop dev server (Ctrl+C)
# Clear browser cache if needed
# Close debug console
```

## Automated Testing (Future)

Potential test frameworks:

- **Unit Tests:** Vitest for component logic
- **Integration Tests:** Playwright for E2E flows
- **Visual Tests:** Percy or Chromatic for screenshot diffs

See `package.json` for test scripts once implemented.
