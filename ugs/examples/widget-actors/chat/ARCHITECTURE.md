# Widget Actor Chat - Architecture Diagram

## System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        Browser Actor Registry                            │
│                     @(chat-input), @(chat-room), @(user-list)           │
└─────────────────────────────────────────────────────────────────────────┘
                                    ▲
                                    │ register/send
                                    │
┌───────────────────────────────────┼─────────────────────────────────────┐
│                            App Orchestration                             │
│  • Wires actor communication flows                                       │
│  • Subscribes to ports                                                   │
│  • Routes messages via registry                                          │
└──────────────────────────────────────────────────────────────────────────┘
```

## Component Architecture

```
┌──────────────────────────────────────────────────────────────────────────┐
│                                                                          │
│  ┌─────────────────┐         ┌──────────────────┐                      │
│  │   ChatInput     │         │    ChatRoom      │                      │
│  │  @(chat-input:  │         │  @(chat-room:    │                      │
│  │   chat-input)   │         │   chat-room)     │                      │
│  │                 │         │                  │                      │
│  │  Ports:         │         │  Ports:          │                      │
│  │  • submit       │──────┐  │  • messages      │────┐                 │
│  │                 │      │  │  • users         │─┐  │                 │
│  │  State:         │      │  │                  │ │  │                 │
│  │  • username     │      │  │  State:          │ │  │                 │
│  │  • inputValue   │      │  │  • messages[]    │ │  │                 │
│  └─────────────────┘      │  │  • users Set     │ │  │                 │
│           │               │  └──────────────────┘ │  │                 │
│           │ DOM Events    │           │           │  │                 │
│           ▼               │           │ creates   │  │                 │
│  ┌─────────────────┐     │           ▼           │  │                 │
│  │  Input/Button   │     │  ┌──────────────────┐ │  │                 │
│  │   <input>       │     │  │  ChatMessage     │ │  │                 │
│  │   <button>      │     │  │  @(chat-message: │ │  │                 │
│  └─────────────────┘     │  │   msg-id)        │ │  │                 │
│                          │  │                  │ │  │                 │
│                          │  │  Protocol:       │ │  │                 │
│                          │  │  • update        │ │  │                 │
│                          │  │                  │ │  │                 │
│                          │  │  State:          │ │  │                 │
│                          │  │  • text          │ │  │                 │
│                          │  │  • sender        │ │  │                 │
│                          │  │  • timestamp     │ │  │                 │
│                          │  └──────────────────┘ │  │                 │
│                          │                       │  │                 │
│                          ▼                       │  │                 │
│                    ┌──────────────┐              │  │                 │
│                    │ App          │              │  │                 │
│                    │ Subscriber   │              │  │                 │
│                    └──────────────┘              │  │                 │
│                          │                       │  │                 │
│                          │ forwards to           │  │                 │
│                          ▼                       ▼  ▼                 │
│                    ┌────────────────┐      ┌──────────────┐          │
│                    │   ChatRoom     │      │  UserList    │          │
│                    │   via send()   │      │  @(user-list:│          │
│                    └────────────────┘      │   user-list) │          │
│                                            │              │          │
│                                            │  Protocol:   │          │
│                                            │  • update-   │          │
│                                            │    users     │          │
│                                            │              │          │
│                                            │  State:      │          │
│                                            │  • users[]   │          │
│                                            └──────────────┘          │
│                                                                          │
└──────────────────────────────────────────────────────────────────────────┘
```

## Message Flow: Sending a Chat Message

```
 User Action                    Actor Events                 UI Updates
─────────────────────────────────────────────────────────────────────────

    [User]
      │
      │ types "Hello"
      ▼
 ┌──────────┐
 │  Input   │
 │  Field   │
 └──────────┘
      │
      │ presses Enter
      ▼
 ┌──────────────┐
 │  ChatInput   │ ◄─── DOM Event Handler
 │   Actor      │
 └──────────────┘
      │
      │ handleSubmit()
      ▼
 ┌──────────────┐
 │ submitPort   │ ◄─── Port Broadcast
 │   .send()    │      { text, sender }
 └──────────────┘
      │
      │ for await (... of channel)
      ▼
 ┌──────────────┐
 │     App      │ ◄─── Subscription Handler
 │  Listener    │
 └──────────────┘
      │
      │ actorRegistry.send()
      ▼
 ┌──────────────┐
 │  ChatRoom    │ ◄─── receive({ type: 'send-message' })
 │    Actor     │
 └──────────────┘
      │
      ├─────────────────────┬────────────────────┐
      │                     │                    │
      ▼                     ▼                    ▼
 ┌──────────┐        ┌──────────┐        ┌──────────┐
 │ Create   │        │ Update   │        │Broadcast │
 │ChatMsg   │        │ messages │        │ messages │
 │Component │        │  signal  │        │   port   │
 └──────────┘        └──────────┘        └──────────┘
      │                     │
      │                     │ signal.set()
      │                     ▼
      │              ┌──────────┐
      │              │ render() │ ◄─── Signal Subscription
      │              └──────────┘
      │                     │
      │                     ▼
      │              ┌──────────┐
      │              │ Update   │
      │              │ Shadow   │
      │              │   DOM    │
      │              └──────────┘
      │
      │ actorRegistry.send()
      ▼
 ┌──────────────┐
 │ ChatMessage  │ ◄─── receive({ type: 'update' })
 │    Actor     │
 └──────────────┘
      │
      │ set signals
      ▼
 ┌──────────────┐
 │   render()   │ ◄─── Reactive Update
 └──────────────┘
      │
      ▼
 ┌──────────────┐
 │   Message    │
 │  Displayed   │ ◄─── User Sees Result
 └──────────────┘
```

## Data Flow Patterns

### 1. Point-to-Point (Direct Message)

```
┌────────────┐                      ┌────────────┐
│   Sender   │                      │  Receiver  │
│   Actor    │                      │   Actor    │
└────────────┘                      └────────────┘
      │                                    ▲
      │   actorRegistry.send()             │
      │                                    │
      │   Message {                        │
      │     from: @(sender),               │
      │     to: @(receiver),               │
      │     type: 'action',                │
      │     payload: { ... }               │
      │   }                                │
      │                                    │
      └────────────────────────────────────┘

Example: App → ChatRoom
```

### 2. Pub/Sub Broadcasting (Port)

```
                    ┌────────────┐
                    │  Producer  │
                    │   Actor    │
                    └────────────┘
                          │
                          │ port.send(data)
                          ▼
                    ┌────────────┐
                    │    Port    │
                    │  Channel   │
                    └────────────┘
                          │
         ┌────────────────┼────────────────┐
         ▼                ▼                ▼
    ┌─────────┐      ┌─────────┐      ┌─────────┐
    │  Sub 1  │      │  Sub 2  │      │  Sub 3  │
    │ Channel │      │ Channel │      │ Channel │
    └─────────┘      └─────────┘      └─────────┘
         │                │                │
         ▼                ▼                ▼
    ┌─────────┐      ┌─────────┐      ┌─────────┐
    │Consumer │      │Consumer │      │Consumer │
    │    1    │      │    2    │      │    3    │
    └─────────┘      └─────────┘      └─────────┘

Example: ChatInput.submitPort → multiple consumers
```

### 3. DOM Event → Actor Message

```
┌────────────┐
│    DOM     │
│   Element  │
└────────────┘
      │
      │ addEventListener()
      ▼
┌────────────┐
│   Event    │
│  Handler   │
└────────────┘
      │
      │ port.send()
      ▼
┌────────────┐
│    Port    │
│ Broadcast  │
└────────────┘
      │
      │ subscription
      ▼
┌────────────┐
│    App     │
│  Listener  │
└────────────┘
      │
      │ actorRegistry.send()
      ▼
┌────────────┐
│   Target   │
│   Actor    │
└────────────┘

Example: Button click → ChatInput → ChatRoom
```

### 4. Signal Reactivity

```
┌────────────┐
│  Actor     │
│ receives   │
│  message   │
└────────────┘
      │
      │ signal.set(newValue)
      ▼
┌────────────┐
│   Signal   │
│   Updates  │
└────────────┘
      │
      │ notifies subscribers
      ▼
┌────────────┐
│  render()  │
│   called   │
└────────────┘
      │
      │ updates Shadow DOM
      ▼
┌────────────┐
│    UI      │
│  Updates   │
└────────────┘

Example: ChatMessage.text.set() → render()
```

## Actor Lifecycle

```
Component Created
      │
      ▼
constructor()
  • Generate address: @(tagName:id)
  • Create ports
  • Initialize signals
      │
      ▼
connectedCallback()
  • Register with actorRegistry
  • Attach event handlers
  • Start subscriptions
      │
      ▼
═══════════════════════
   Active Lifetime
   • receive() messages
   • Send to other actors
   • Broadcast to ports
   • React to signals
═══════════════════════
      │
      ▼
disconnectedCallback()
  • Unregister from registry
  • Close ports
  • End subscriptions
  • Cleanup resources
      │
      ▼
Component Destroyed
```

## Technology Stack

```
┌─────────────────────────────────────────────────────────┐
│                     Application                          │
│                   (Widget Actors)                        │
└─────────────────────────────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
     ┌─────────┐    ┌─────────┐    ┌─────────┐
     │   Web   │    │  Actor  │    │   TC39  │
     │Components│    │Protocol │    │ Signals │
     └─────────┘    └─────────┘    └─────────┘
          │               │               │
          ▼               ▼               ▼
     ┌─────────┐    ┌─────────┐    ┌─────────┐
     │ Shadow  │    │Channels │    │Reactive │
     │   DOM   │    │  Ports  │    │  State  │
     └─────────┘    └─────────┘    └─────────┘
          │               │               │
          └───────────────┼───────────────┘
                          ▼
          ┌───────────────────────────────┐
          │      Browser Platform          │
          │  • Custom Elements             │
          │  • ES Modules                  │
          │  • Async Iteration             │
          │  • TypeScript                  │
          └───────────────────────────────┘
```

## Communication Protocols

### Actor Message Format

```typescript
Message {
  id: string              // "msg-1234-5678"
  pattern: 'tell'         // 'tell' | 'ask' | 'stream'
  from: Address           // "@(sender:id)"
  to: Address             // "@(receiver:id)"
  type: string            // "send-message"
  payload: any            // { text: "Hello" }
  timestamp: number       // Date.now()
}
```

### Actor Response Format

```typescript
MessageResponse {
  id: string              // "resp-1234-5678"
  correlationId: string   // Links to original message
  from: Address           // "@(responder:id)"
  to: Address             // "@(original-sender:id)"
  success: boolean        // true/false
  payload?: any           // Response data
  error?: string          // Error message if failed
  timestamp: number       // Date.now()
}
```

### Port Broadcast Data

```typescript
// ChatInput submit port
{ text: string, sender: string }

// ChatRoom messages port
{ id: string, text: string, sender: string, timestamp: number }

// ChatRoom users port
string[] // Array of usernames
```

## Scaling Patterns

### Adding New Components

1. Create new Widget Actor class
2. Implement `receive()` handler
3. Add to HTML
4. Wire in `app.ts`

**No changes needed to existing components!**

### Adding New Message Types

1. Add case to component's `receive()` switch
2. Handle new payload structure
3. Update component state/signals

**No protocol changes needed!**

### Adding New Subscribers

1. Get port from producer
2. Subscribe to channel
3. Handle broadcasted data

**Producer doesn't need to know about subscribers!**

## Benefits Visualized

```
Traditional Approach          Widget Actor Approach
─────────────────────────────────────────────────────

┌────────────────┐            ┌────────────────┐
│   Component A  │            │   Component A  │
│                │            │   @(comp-a)    │
│  • Direct DOM  │            │                │
│    access to B │            │  • Sends       │
│  • Tight       │            │    messages    │
│    coupling    │            │  • Decoupled   │
└────────────────┘            └────────────────┘
        │                             │
        │ querySelector()             │ send()
        │ .textContent =              │
        ▼                             ▼
┌────────────────┐            ┌────────────────┐
│   Component B  │            │Actor Registry  │
│                │            └────────────────┘
│  • No         │                     │
│    isolation   │                    │ route
└────────────────┘                    ▼
                              ┌────────────────┐
                              │   Component B  │
                              │   @(comp-b)    │
                              │                │
                              │  • Isolated    │
                              │  • Testable    │
                              └────────────────┘
```

## Summary

The Widget Actor architecture provides:

1. **Decoupling** - Components communicate via messages, not direct references
2. **Composability** - New components integrate without modifying existing code
3. **Testability** - Each component can be tested independently
4. **Debuggability** - Message flow is explicit and inspectable
5. **Scalability** - Add components without increasing coupling

This demo proves the pattern works in real browsers with real Web Components.
