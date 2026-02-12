# Browser Lifecycle APIs for Widget Actors

**Research Date:** 2026-02-05
**Context:** Widget Actor lifecycle management in SPAs and across page loads
**Focus:** Native browser APIs, emerging web platform standards (2024-2026)

---

## Executive Summary

### Top 3 Recommendations

1. **Navigation API + View Transitions API (P0)** - Production-ready solution for SPA lifecycle management with smooth transitions. Navigation API provides semantic lifecycle hooks while View Transitions API enables state-preserving animations between views.

2. **Service Worker + IndexedDB (P1)** - Cross-page persistence strategy using Service Workers for actor registry management and IndexedDB for state storage. Enables actors to survive full page reloads and cross-tab coordination.

3. **BroadcastChannel API + Page Lifecycle API (P1)** - Real-time cross-tab synchronization combined with proper freeze/discard handling. Essential for multi-tab applications and resource-constrained environments.

### Browser Support Matrix (2024-2026)

| API | Chrome | Edge | Safari | Firefox | Status | Priority |
|-----|--------|------|--------|---------|--------|----------|
| Navigation API | 102+ | 102+ | ❌ | ❌ | Baseline (Jan 2026) | P0 |
| View Transitions (same-doc) | 111+ | 111+ | 18+ | 144+ | Baseline (Oct 2025) | P0 |
| View Transitions (cross-doc) | 126+ | 126+ | 18.2+ | ❌ | Experimental | P1 |
| Page Lifecycle API | 68+ | 79+ | ❌ | ❌ | Chrome-only | P1 |
| BroadcastChannel API | 54+ | 79+ | 15.4+ | 38+ | Baseline | P0 |
| Service Workers | 40+ | 17+ | 11.1+ | 44+ | Baseline | P0 |
| IndexedDB | All | All | All | All | Baseline | P0 |
| OPFS | 102+ | 102+ | 15.2+ | 111+ | Baseline | P2 |
| FinalizationRegistry | 84+ | 84+ | 14.1+ | 79+ | Baseline | P2 |
| WeakRef | 84+ | 84+ | 14.1+ | 79+ | Baseline | P2 |
| Document PiP | 116+ | 116+ | ❌ | ❌ | Chrome-only | P3 |
| Compute Pressure API | 115-123 | 115-123 | ❌ | ❌ | Origin Trial | P4 |
| Speculation Rules API | 109+ | 109+ | ❌ | ❌ | Chrome-only | P3 |
| Web Locks API | 69+ | 79+ | 15.4+ | 96+ | Baseline | P2 |
| Background Sync API | 49+ | 79+ | ❌ | ❌ | Chrome-only | P3 |

### Implementation Priority

**Immediate (P0):** Navigation API + View Transitions for SPA navigation lifecycle
**Near-term (P1):** Service Worker registry persistence + BroadcastChannel for cross-tab sync
**Future (P2-P4):** Advanced memory management, Document PiP, specialized features

---

## Native APIs Evaluated

### 1. Navigation API

**Status:** Stable (Baseline as of January 2026)
**Browser Support:** Chrome 102+, Edge 102+
**Use Case:** Primary lifecycle hook for Widget Actors in SPAs

#### Overview

The Navigation API is a successor to the History API, designed specifically for single-page applications. It provides semantic navigation events that the browser understands, enabling proper lifecycle management for Widget Actors during route changes.

#### Key Features

- **`navigate` event** - Fired on any navigation (link clicks, form submissions, programmatic navigation)
- **`intercept()` method** - Allows custom SPA navigation behavior
- **`handler()` callback** - Runs after navigation commits (currentEntry updated, URL changed)
- **`navigatesuccess` / `navigateerror` events** - Proper success/failure handling
- **Cannot intercept:** Cross-origin navigations, cross-document traversals

#### Benefits for Widget Actors

1. **Centralized lifecycle management** - Single event listener for all navigations
2. **Semantic navigation concept** - Browser understands "SPA navigation is occurring"
3. **Accessibility improvements** - Browser can surface navigation start/end/failure
4. **State preservation** - Native support for preserving component state between views

#### Example Implementation

```typescript
// Widget Actor SPA lifecycle integration
class WidgetActorLifecycleManager {
  constructor(private registry: BrowserActorRegistry) {
    // Listen to all navigations
    navigation.addEventListener('navigate', (event) => {
      if (this.shouldHandleSPA(event)) {
        event.intercept({
          handler: async () => {
            // Notify actors of navigation start
            await this.notifyActors('navigation:start', {
              destination: event.destination
            });

            // Load new page content
            await this.loadPage(event.destination.url);

            // Re-register actors on new page
            await this.rehydrateActors();

            // Notify navigation complete
            await this.notifyActors('navigation:complete');
          }
        });
      }
    });

    // Handle navigation success
    navigation.addEventListener('navigatesuccess', () => {
      this.cleanupOrphanedActors();
    });

    // Handle navigation errors
    navigation.addEventListener('navigateerror', (event) => {
      this.rollbackActorState();
      console.error('Navigation failed:', event.error);
    });
  }

  private shouldHandleSPA(event: NavigateEvent): boolean {
    return event.canIntercept &&
           event.destination.url.origin === location.origin &&
           !event.downloadRequest &&
           !event.formData;
  }

  private async notifyActors(type: string, payload?: any): Promise<void> {
    const actors = this.registry.list();
    await Promise.all(
      actors.map(address =>
        this.registry.send(address, {
          type,
          payload,
          id: `nav-${Date.now()}`,
          from: address('system:navigation'),
          to: address,
          timestamp: Date.now()
        })
      )
    );
  }
}
```

#### Recommendation

**✅ Implement immediately (P0)** - The Navigation API is the foundation for Widget Actor lifecycle management in SPAs. It provides the semantic hooks needed to properly manage actor registration/unregistration during route changes.

#### References

- [Navigation API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Navigation_API)
- [Modern client-side routing - Chrome Developers](https://developer.chrome.com/docs/web-platform/navigation-api/)
- [Navigation API GitHub](https://github.com/WICG/navigation-api)

---

### 2. View Transitions API

**Status:** Same-document: Baseline (Oct 2025), Cross-document: Experimental
**Browser Support:** Chrome 111+, Edge 111+, Safari 18+, Firefox 144+
**Use Case:** State-preserving animations between views, smooth transitions for Widget Actors

#### Overview

The View Transitions API creates animated transitions between different DOM states (same-document) or between pages (cross-document). Crucially, it **preserves component state** during transitions in SPAs.

#### Same-Document Transitions (Available)

```typescript
// Transition between SPA views while preserving Widget Actor state
async function navigateWithTransition(url: string) {
  if (!document.startViewTransition) {
    // Fallback for unsupported browsers
    await updateDOM(url);
    return;
  }

  const transition = document.startViewTransition(async () => {
    // Widget Actors remain alive during this update
    await updateDOM(url);
  });

  await transition.finished;
}

// Per-element transitions for Widget Actors
class AnimatedWidgetActor extends BaseWidgetActor {
  connectedCallback() {
    super.connectedCallback();
    // Give this widget a unique view-transition-name
    this.style.viewTransitionName = `widget-${this.address}`;
  }

  disconnectedCallback() {
    this.style.viewTransitionName = 'none';
    super.disconnectedCallback();
  }
}
```

#### Cross-Document Transitions (Experimental)

Released in Chrome 126 (June 2024), enables transitions between full page loads:

```html
<!-- Enable cross-document transitions -->
<meta name="view-transition" content="same-origin">
```

```css
/* Preserve Widget Actor elements across page loads */
.widget-actor {
  view-transition-name: var(--widget-id);
  view-transition-class: persistent-widget;
}

@view-transition {
  navigation: auto;
}
```

#### Key Features (2025 Updates)

- **view-transition-class** - Group transitions by category
- **View transition types** - Different transition styles for different navigation types
- **match-element value** - Animate individual elements in SPAs

#### Benefits for Widget Actors

1. **State preservation** - Components remain alive during transitions (same-document)
2. **Per-element animations** - Each Widget Actor can have its own transition
3. **Progressive enhancement** - Degrades gracefully (no animation if unsupported)
4. **Smooth user experience** - Reduces perceived latency during navigation

#### Limitations

⚠️ **Important:** During transitions, elements become image snapshots. Some CSS properties (e.g., font-size changes) won't work during animation.

#### Recommendation

**✅ Implement immediately (P0)** - Same-document View Transitions should be the default for SPA navigation. The cross-document API is experimental but shows the future direction for preserving actors across full page loads.

#### References

- [View Transitions API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API)
- [What's new in view transitions (2025 update)](https://developer.chrome.com/blog/view-transitions-in-2025)
- [Smooth transitions - Chrome Developers](https://developer.chrome.com/docs/web-platform/view-transitions)

---

### 3. Page Lifecycle API

**Status:** Chrome-only (since Chrome 68)
**Browser Support:** Chrome 68+, Edge 79+
**Use Case:** Handle browser resource management (freeze/discard events)

#### Overview

Modern browsers suspend or discard tabs to conserve resources. The Page Lifecycle API provides hooks to detect and handle these interventions, enabling proper cleanup and restoration of Widget Actors.

#### Key Events

```typescript
// Detect when tab is frozen (CPU suspended)
document.addEventListener('freeze', () => {
  // Save critical Widget Actor state
  actorRegistry.serializeState();

  // Pause non-critical operations
  actorRegistry.list().forEach(address => {
    actorRegistry.send(address, {
      type: 'lifecycle:freeze',
      id: `freeze-${Date.now()}`,
      from: address('system:lifecycle'),
      to: address,
      timestamp: Date.now()
    });
  });
});

// Tab unfrozen, resume operations
document.addEventListener('resume', () => {
  // Restore actor operations
  actorRegistry.list().forEach(address => {
    actorRegistry.send(address, {
      type: 'lifecycle:resume',
      id: `resume-${Date.now()}`,
      from: address('system:lifecycle'),
      to: address,
      timestamp: Date.now()
    });
  });
});

// Detect if page was discarded while hidden
if (document.wasDiscarded) {
  console.log('Page was discarded, restoring actors...');
  await actorRegistry.restoreFromStorage();
}
```

#### Lifecycle States

1. **Active** - Page is visible and has input focus
2. **Passive** - Page is visible but doesn't have input focus
3. **Hidden** - Page is not visible (backgrounded tab)
4. **Frozen** - Page is suspended (CPU suspended, not executing tasks)
5. **Discarded** - Page unloaded to reclaim resources, must reload to use again

#### Benefits for Widget Actors

1. **Proactive state management** - Save state before forced unload
2. **Resource optimization** - Pause expensive operations when hidden
3. **Recovery capability** - Detect and recover from discards

#### Limitations

- Chrome/Edge only (no Safari/Firefox support as of 2026)
- Not all freezes trigger events (some are too brief)
- Discarded pages must fully reload

#### Recommendation

**✅ Explore for production (P1)** - Despite limited browser support, the Page Lifecycle API is valuable for Chrome/Edge users. Implement as progressive enhancement with feature detection.

#### References

- [Page Lifecycle API - Chrome Developers](https://developer.chrome.com/docs/web-platform/page-lifecycle-api)
- [Page Lifecycle Spec](https://wicg.github.io/page-lifecycle/)
- [Page Lifecycle API GitHub](https://github.com/WICG/page-lifecycle)

---

### 4. Service Workers + State Persistence

**Status:** Stable (Baseline)
**Browser Support:** Chrome 40+, Edge 17+, Safari 11.1+, Firefox 44+
**Use Case:** Cross-page actor registry persistence, offline capability

#### Overview

Service Workers run in a background thread and persist across page loads, making them ideal for maintaining a persistent actor registry that survives full page navigations.

#### Architecture Pattern

```typescript
// Service Worker: Persistent actor registry
// sw.js
class PersistentActorRegistry {
  private db: IDBDatabase;

  async init() {
    this.db = await this.openDB('actor-registry', 1, {
      upgrade(db) {
        const store = db.createObjectStore('actors', { keyPath: 'address' });
        store.createIndex('session', 'sessionId');
        store.createIndex('lastSeen', 'lastSeen');
      }
    });
  }

  async registerActor(address: string, state: any) {
    const tx = this.db.transaction('actors', 'readwrite');
    await tx.objectStore('actors').put({
      address,
      state,
      sessionId: self.sessionId,
      lastSeen: Date.now()
    });
  }

  async getActors(sessionId: string): Promise<any[]> {
    const tx = this.db.transaction('actors', 'readonly');
    const index = tx.objectStore('actors').index('session');
    return await index.getAll(sessionId);
  }

  async cleanupOldActors(maxAge: number = 3600000) {
    const cutoff = Date.now() - maxAge;
    const tx = this.db.transaction('actors', 'readwrite');
    const index = tx.objectStore('actors').index('lastSeen');
    const range = IDBKeyRange.upperBound(cutoff);

    for await (const cursor of index.iterate(range)) {
      cursor.delete();
    }
  }
}

const persistentRegistry = new PersistentActorRegistry();

self.addEventListener('install', () => {
  persistentRegistry.init();
});

self.addEventListener('message', async (event) => {
  if (event.data.type === 'register-actor') {
    await persistentRegistry.registerActor(
      event.data.address,
      event.data.state
    );
  }

  if (event.data.type === 'get-actors') {
    const actors = await persistentRegistry.getActors(event.data.sessionId);
    event.ports[0].postMessage({ actors });
  }
});
```

```typescript
// Page: Sync with Service Worker registry
class ServiceWorkerActorSync {
  private registration: ServiceWorkerRegistration;

  async init() {
    this.registration = await navigator.serviceWorker.register('/sw.js');
    await navigator.serviceWorker.ready;

    // Restore actors from persistent storage on page load
    const actors = await this.getActorsFromSW();
    actors.forEach(actorData => {
      this.rehydrateActor(actorData);
    });

    // Sync registry changes to Service Worker
    actorRegistry.addEventListener('register', (event) => {
      this.syncToSW('register-actor', event.detail);
    });
  }

  private async getActorsFromSW(): Promise<any[]> {
    return new Promise((resolve) => {
      const channel = new MessageChannel();
      channel.port1.onmessage = (event) => {
        resolve(event.data.actors);
      };

      navigator.serviceWorker.controller?.postMessage({
        type: 'get-actors',
        sessionId: this.getSessionId()
      }, [channel.port2]);
    });
  }
}
```

#### Storage Options

**IndexedDB (Recommended)**
- Async, non-blocking
- Large storage capacity (typically 50MB+, up to available disk space)
- Structured data with indexes
- Works in Service Workers

**Cache API**
- Optimized for HTTP responses
- Good for prefetching actor code/templates
- Not ideal for structured state

**Origin Private File System (OPFS)**
- High-performance file access
- Synchronous API in workers
- Best for large binary data
- May be overkill for actor state

#### Benefits for Widget Actors

1. **True cross-page persistence** - Actors survive full page reloads
2. **Offline capability** - Actors can function without network
3. **Cross-tab coordination** - Single source of truth for all tabs
4. **Background processing** - Actor work can continue in Service Worker

#### Limitations

⚠️ **In-memory state not persistent** - Service Workers can be terminated by the browser at any time. Always persist critical state to IndexedDB, not in-memory variables.

⚠️ **No DOM access** - Service Workers cannot directly manipulate Widget Actors. Use MessageChannel for communication.

#### Recommendation

**✅ Implement for production (P1)** - Essential for cross-page persistence. Start with IndexedDB for state storage and Service Worker as the coordination layer.

#### References

- [Service Workers - web.dev](https://web.dev/learn/pwa/service-workers)
- [Service Worker State Management](https://www.jameslmilner.com/posts/serviceworker-state-management/)
- [Using Service Workers - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API/Using_Service_Workers)
- [SharedWorker - MDN](https://developer.mozilla.org/en-US/docs/Web/API/SharedWorker) (alternative for cross-tab without offline)

---

### 5. BroadcastChannel API

**Status:** Stable (Baseline)
**Browser Support:** Chrome 54+, Edge 79+, Safari 15.4+, Firefox 38+
**Use Case:** Real-time cross-tab actor synchronization

#### Overview

BroadcastChannel enables simple pub/sub messaging between browsing contexts (tabs, windows, iframes, workers) on the same origin. Ideal for keeping Widget Actors synchronized across multiple tabs.

#### Implementation

```typescript
// Cross-tab actor synchronization
class CrossTabActorSync {
  private channel: BroadcastChannel;

  constructor(private registry: BrowserActorRegistry) {
    this.channel = new BroadcastChannel('actor-registry');

    // Listen for actor events from other tabs
    this.channel.addEventListener('message', (event) => {
      this.handleRemoteEvent(event.data);
    });

    // Broadcast local registry changes
    this.registry.addEventListener('register', (event) => {
      this.channel.postMessage({
        type: 'actor:registered',
        address: event.detail.address,
        timestamp: Date.now()
      });
    });

    this.registry.addEventListener('unregister', (event) => {
      this.channel.postMessage({
        type: 'actor:unregistered',
        address: event.detail.address,
        timestamp: Date.now()
      });
    });
  }

  private handleRemoteEvent(data: any) {
    switch (data.type) {
      case 'actor:registered':
        // Another tab registered an actor
        // Could create a proxy or update local registry
        console.log(`Actor ${data.address} registered in another tab`);
        break;

      case 'actor:unregistered':
        // Another tab unregistered an actor
        console.log(`Actor ${data.address} unregistered in another tab`);
        break;

      case 'actor:message':
        // Forward message to local actor
        if (this.registry.has(data.to)) {
          this.registry.send(data.to, data.message);
        }
        break;
    }
  }

  // Broadcast actor message to all tabs
  broadcastMessage(message: Message) {
    this.channel.postMessage({
      type: 'actor:message',
      to: message.to,
      message,
      timestamp: Date.now()
    });
  }

  // Cleanup on page unload
  destroy() {
    this.channel.close();
  }
}

// Use case: Sync shopping cart across tabs
class CartWidgetActor extends BaseWidgetActor {
  private sync: CrossTabActorSync;

  async receive(msg: Message): Promise<MessageResponse> {
    if (msg.type === 'cart:add-item') {
      this.addItem(msg.payload);

      // Broadcast to other tabs
      this.sync.broadcastMessage({
        ...msg,
        to: address('cart-widget:*')  // All cart widgets
      });

      return { success: true };
    }
    return { success: false };
  }
}
```

#### Benefits for Widget Actors

1. **Real-time synchronization** - Changes propagate instantly to all tabs
2. **Simple API** - Much easier than SharedWorker for basic sync
3. **No polling** - Event-driven, low overhead
4. **Same-origin only** - Security built-in

#### Limitations

⚠️ **Not persistent** - Messages only reach currently open tabs, won't persist for future tabs

⚠️ **Same-origin only** - Cannot communicate across different domains

⚠️ **Storage partitioning** - May not work in all cross-site scenarios due to browser privacy features

#### Use Cases

- Cart synchronization across tabs
- Login/logout propagation
- Real-time notifications
- Multi-tab UI state sync

#### Recommendation

**✅ Implement immediately (P0)** - BroadcastChannel is the simplest way to keep Widget Actors synchronized across tabs. Essential for multi-tab applications.

#### References

- [Broadcast Channel API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Broadcast_Channel_API)
- [BroadcastChannel API: A Hidden Gem](https://dev.to/itxshakil/broadcastchannel-api-a-hidden-gem-for-web-developers-33c4)
- [Exploring the Broadcast Channel API - MDN Blog](https://developer.mozilla.org/en-US/blog/exploring-the-broadcast-channel-api-for-cross-tab-communication/)

---

### 6. FinalizationRegistry + WeakRef

**Status:** Stable (Baseline)
**Browser Support:** Chrome 84+, Edge 84+, Safari 14.1+, Firefox 79+
**Use Case:** Memory leak prevention, automatic cleanup of orphaned Widget Actors

#### Overview

FinalizationRegistry provides callbacks when objects are garbage collected, while WeakRef allows holding references that don't prevent GC. Together, they enable automatic cleanup of Widget Actors.

#### Implementation

```typescript
// Automatic actor cleanup on garbage collection
class ActorMemoryManager {
  private registry: FinalizationRegistry<Address>;
  private weakActors = new Map<Address, WeakRef<Actor>>();

  constructor(private actorRegistry: BrowserActorRegistry) {
    // Create cleanup callback
    this.registry = new FinalizationRegistry((address) => {
      console.log(`Actor ${address} was garbage collected`);

      // Automatically unregister from actor registry
      this.actorRegistry.unregister(address);
      this.weakActors.delete(address);

      // Notify other systems
      this.notifyActorCleanup(address);
    });
  }

  // Track actor with weak reference
  trackActor(address: Address, actor: Actor) {
    // Store weak reference
    this.weakActors.set(address, new WeakRef(actor));

    // Register for cleanup notification
    this.registry.register(actor, address, actor);
  }

  // Stop tracking (if actor is manually removed)
  untrackActor(actor: Actor) {
    this.registry.unregister(actor);
  }

  // Get actor if still alive
  getActor(address: Address): Actor | undefined {
    const weakRef = this.weakActors.get(address);
    return weakRef?.deref();
  }

  // Check if actor is still alive
  isActorAlive(address: Address): boolean {
    const actor = this.getActor(address);
    return actor !== undefined;
  }
}

// Integration with Widget Actor mixin
export function ActorMixin<T extends Constructor<HTMLElement>>(Base: T) {
  return class extends Base implements WidgetActor {
    private static memoryManager = new ActorMemoryManager(actorRegistry);

    connectedCallback(): void {
      if (super.connectedCallback) super.connectedCallback();

      if (!this._isActorRegistered) {
        actorRegistry.register(this.address, this);

        // Track for automatic cleanup
        ActorMixin.memoryManager.trackActor(this.address, this);

        this._isActorRegistered = true;
      }
    }

    disconnectedCallback(): void {
      if (this._isActorRegistered) {
        actorRegistry.unregister(this.address);

        // Stop tracking (manual cleanup)
        ActorMixin.memoryManager.untrackActor(this);

        this._isActorRegistered = false;
      }

      if (super.disconnectedCallback) super.disconnectedCallback();
    }
  };
}
```

#### Caveat System for Event Listeners

```typescript
// Prevent memory leaks from event listeners
class EventListenerManager {
  private cleanupRegistry = new FinalizationRegistry<() => void>(
    (cleanup) => cleanup()
  );

  addListener(
    element: HTMLElement,
    event: string,
    handler: EventListener
  ): void {
    element.addEventListener(event, handler);

    // Register cleanup callback
    this.cleanupRegistry.register(
      element,
      () => {
        element.removeEventListener(event, handler);
        console.log(`Cleaned up ${event} listener`);
      },
      element
    );
  }
}
```

#### Benefits for Widget Actors

1. **Automatic memory leak detection** - Know when actors are orphaned
2. **Zero-overhead tracking** - WeakRef doesn't prevent GC
3. **Defensive programming** - Catch errors where actors aren't properly unregistered
4. **Development visibility** - See which actors are being cleaned up

#### Important Caveats

⚠️ **GC is unpredictable** - Cleanup callbacks may run much later than expected, or not at all

⚠️ **Not for essential logic** - Never use FinalizationRegistry for critical program logic

⚠️ **Best for development/debugging** - Most useful for detecting memory leaks during development

#### Recommendation

**✅ Explore for development (P2)** - Implement as a debugging tool to catch memory leaks. Don't rely on it for production cleanup logic (use manual disconnectedCallback instead).

#### References

- [FinalizationRegistry - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/FinalizationRegistry)
- [WeakRef and FinalizationRegistry - javascript.info](https://javascript.info/weakref-finalizationregistry)
- [Weak references and finalizers - V8](https://v8.dev/features/weak-references)

---

### 7. requestIdleCallback

**Status:** Stable (Baseline)
**Browser Support:** Chrome 47+, Edge 79+, Firefox 55+, Safari ❌
**Use Case:** Schedule low-priority actor cleanup during browser idle time

#### Overview

`requestIdleCallback` schedules work to run during browser idle periods, perfect for non-urgent Widget Actor cleanup tasks without impacting performance.

#### Implementation

```typescript
// Schedule actor cleanup during idle time
class IdleActorCleanup {
  private pendingCleanup = new Set<Address>();
  private idleCallbackId: number | null = null;

  scheduleCleanup(address: Address) {
    this.pendingCleanup.add(address);

    if (this.idleCallbackId === null) {
      this.idleCallbackId = requestIdleCallback(
        (deadline) => this.performCleanup(deadline),
        { timeout: 2000 } // Fallback if never idle
      );
    }
  }

  private performCleanup(deadline: IdleDeadline) {
    // Process cleanup while idle time remains
    while (
      (deadline.timeRemaining() > 0 || deadline.didTimeout) &&
      this.pendingCleanup.size > 0
    ) {
      const address = this.pendingCleanup.values().next().value;
      this.pendingCleanup.delete(address);

      // Perform expensive cleanup
      this.cleanupActorResources(address);
    }

    // Schedule next batch if work remains
    if (this.pendingCleanup.size > 0) {
      this.idleCallbackId = requestIdleCallback(
        (deadline) => this.performCleanup(deadline),
        { timeout: 2000 }
      );
    } else {
      this.idleCallbackId = null;
    }
  }

  private cleanupActorResources(address: Address) {
    // Expensive operations that can wait
    // - Clear caches
    // - Compact storage
    // - Release media resources
    // - Garbage collect internal state
  }

  cancel() {
    if (this.idleCallbackId !== null) {
      cancelIdleCallback(this.idleCallbackId);
      this.idleCallbackId = null;
    }
  }
}

// Integration with actor lifecycle
class ResourceAwareActor extends BaseWidgetActor {
  private static cleanupScheduler = new IdleActorCleanup();

  disconnectedCallback() {
    super.disconnectedCallback();

    // Schedule resource cleanup during idle time
    ResourceAwareActor.cleanupScheduler.scheduleCleanup(this.address);
  }
}
```

#### Benefits for Widget Actors

1. **Non-blocking cleanup** - Doesn't impact main thread performance
2. **Cooperative scheduling** - Browser decides when to run
3. **Battery-friendly** - Respects device power state
4. **Fallback timeout** - Ensures cleanup eventually runs

#### Limitations

⚠️ **No Safari support** - Need fallback for Safari (use setTimeout)

⚠️ **Not for urgent work** - Only use for tasks that can wait

#### Polyfill for Safari

```typescript
const requestIdleCallback = window.requestIdleCallback ||
  ((cb: Function) => setTimeout(() => cb({
    timeRemaining: () => 16,
    didTimeout: false
  }), 1));
```

#### Recommendation

**✅ Implement with fallback (P2)** - Good for optimizing cleanup performance. Use with feature detection and setTimeout fallback.

#### References

- [requestIdleCallback - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestIdleCallback)
- [Background Tasks API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Background_Tasks_API)
- [Making the Most of Idle Moments](https://www.afasterweb.com/2017/11/20/utilizing-idle-moments/)

---

### 8. Declarative Shadow DOM

**Status:** Stable (Baseline as of August 2024)
**Browser Support:** Chrome 90+ (124+ for latest spec), Edge 91+, Safari 16.4+, Firefox ❌
**Use Case:** Server-side rendered Widget Actors with encapsulated styles

#### Overview

Declarative Shadow DOM allows creating Shadow DOM in HTML without JavaScript, enabling server-side rendering of Widget Actors with encapsulation.

#### Implementation

```html
<!-- Server-rendered Widget Actor -->
<chat-message id="msg-123">
  <template shadowrootmode="open">
    <style>
      :host {
        display: block;
        padding: 1rem;
        border: 1px solid #ccc;
      }
      .message { color: #333; }
    </style>
    <div class="message">
      <strong>User:</strong> Hello World
    </div>
  </template>
</chat-message>

<script type="module">
  // Hydrate into Widget Actor
  class ChatMessage extends BaseWidgetActor {
    async receive(msg: Message): Promise<MessageResponse> {
      if (msg.type === 'update') {
        const shadowRoot = this.shadowRoot;
        shadowRoot.querySelector('.message').textContent = msg.payload.text;
        return { success: true };
      }
      return { success: false };
    }
  }

  customElements.define('chat-message', ChatMessage);
</script>
```

#### Benefits for Widget Actors

1. **Server-side rendering** - Actors can render on the server
2. **Faster initial load** - Content visible before JavaScript loads
3. **Progressive enhancement** - Works without JavaScript for static content
4. **Style encapsulation** - CSS isolation without runtime overhead

#### Specification Changes (2023)

- Attribute renamed: `shadowroot` → `shadowrootmode`
- Chrome 124+ has the latest standardized version
- Older implementations may need migration

#### Recommendation

**✅ Explore for SSR (P2)** - Valuable if server-side rendering Widget Actors. Not critical for client-only SPAs.

#### References

- [Declarative Shadow DOM - web.dev](https://web.dev/articles/declarative-shadow-dom)
- [Using shadow DOM - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_shadow_DOM)
- [Declarative Shadow DOM - Can I use](https://caniuse.com/declarative-shadow-dom)

---

### 9. Import Maps

**Status:** Stable (Baseline as of late 2024)
**Browser Support:** Chrome 89+, Edge 89+, Safari 16.4+, Firefox 108+
**Use Case:** Module resolution for Widget Actor code, avoiding build tools

#### Overview

Import Maps control how browsers resolve module specifiers, enabling clean imports without bundlers. Useful for loading Widget Actor classes dynamically.

#### Implementation

```html
<script type="importmap">
{
  "imports": {
    "actor/": "/src/messaging/browser/",
    "widgets/": "/src/widgets/",
    "@lit/": "https://cdn.jsdelivr.net/npm/@lit/+esm/"
  }
}
</script>

<script type="module">
  // Clean imports without build tools
  import { BaseWidgetActor } from 'actor/widget-actor.ts';
  import { ChatWidget } from 'widgets/chat-widget.ts';

  // Dynamic actor loading
  async function loadActorModule(name: string) {
    const module = await import(`widgets/${name}.ts`);
    return module.default;
  }

  // Register widgets dynamically
  const ChatMessage = await loadActorModule('chat-message');
  customElements.define('chat-message', ChatMessage);
</script>
```

#### Benefits for Widget Actors

1. **No build tools required** - Development without bundlers
2. **Dynamic actor loading** - Load actors on demand
3. **Clean imports** - Readable import paths
4. **CDN support** - Load dependencies from CDNs

#### Recommendation

**✅ Use for development (P1)** - Simplifies development setup. May still want bundling for production optimization.

#### References

- [Import Maps - MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/script/type/importmap)
- [JavaScript import maps are now supported cross-browser](https://web.dev/blog/import-maps-in-all-modern-browsers)
- [Using Import Maps and Lit Element Web Components](https://coryrylan.com/blog/using-import-maps-and-lit-element-web-components)

---

### 10. MutationObserver + IntersectionObserver

**Status:** Stable (Baseline)
**Browser Support:** All modern browsers
**Use Case:** Detect Widget Actor lifecycle changes, optimize off-screen actors

#### Overview

Observer APIs enable reactive monitoring of DOM changes and viewport visibility, useful for optimizing Widget Actor behavior.

#### MutationObserver for Lifecycle

```typescript
// Monitor dynamic actor creation/removal
class ActorLifecycleMonitor {
  private observer: MutationObserver;

  constructor(private registry: BrowserActorRegistry) {
    this.observer = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        // New actors added
        mutation.addedNodes.forEach(node => {
          if (node instanceof HTMLElement && this.isWidgetActor(node)) {
            console.log(`New actor detected: ${node.tagName}`);
          }
        });

        // Actors removed
        mutation.removedNodes.forEach(node => {
          if (node instanceof HTMLElement && this.isWidgetActor(node)) {
            console.log(`Actor removed: ${node.tagName}`);
            // Ensure cleanup happened
            this.verifyCleanup(node);
          }
        });
      }
    });

    this.observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  private isWidgetActor(element: HTMLElement): boolean {
    return element.tagName.includes('-') &&
           'receive' in element;
  }

  disconnect() {
    this.observer.disconnect();
  }
}
```

#### IntersectionObserver for Performance

```typescript
// Pause off-screen actors to save resources
class VisibilityAwareActor extends BaseWidgetActor {
  private observer: IntersectionObserver;
  private isVisible = false;

  connectedCallback() {
    super.connectedCallback();

    this.observer = new IntersectionObserver(
      (entries) => {
        this.isVisible = entries[0].isIntersecting;

        if (this.isVisible) {
          this.resume();
        } else {
          this.pause();
        }
      },
      { threshold: 0.1 }
    );

    this.observer.observe(this);
  }

  private pause() {
    // Stop expensive operations when off-screen
    this.stopAnimations();
    this.pausePolling();
  }

  private resume() {
    // Resume when visible
    this.startAnimations();
    this.resumePolling();
  }

  disconnectedCallback() {
    this.observer.disconnect();
    super.disconnectedCallback();
  }
}
```

#### Benefits for Widget Actors

1. **Automatic lifecycle detection** - Catch actors added/removed dynamically
2. **Performance optimization** - Pause off-screen actors
3. **Memory leak detection** - Verify actors are properly cleaned up
4. **Visibility-based loading** - Lazy-load actor resources

#### Important Cleanup

```typescript
// Always disconnect observers in disconnectedCallback
disconnectedCallback() {
  this.mutationObserver?.disconnect();
  this.intersectionObserver?.disconnect();
  super.disconnectedCallback();
}
```

#### Recommendation

**✅ Implement immediately (P1)** - IntersectionObserver is essential for performance optimization. MutationObserver is valuable for debugging actor lifecycle issues.

#### References

- [MutationObserver - MDN](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver)
- [IntersectionObserver - MDN](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver)
- [Mutation events deprecated in Chrome 2024](https://developer.chrome.com/blog/mutation-events-deprecation)

---

### 11. Web Locks API

**Status:** Stable (Baseline)
**Browser Support:** Chrome 69+, Edge 79+, Safari 15.4+, Firefox 96+
**Use Case:** Coordinate actor access to shared resources across tabs

#### Overview

Web Locks API enables cross-tab resource coordination, ensuring only one tab at a time accesses a shared resource.

#### Implementation

```typescript
// Ensure only one tab processes a resource
class SingleProcessorActor extends BaseWidgetActor {
  async receive(msg: Message): Promise<MessageResponse> {
    if (msg.type === 'process-data') {
      // Try to acquire lock
      await navigator.locks.request(
        'data-processor',
        { mode: 'exclusive' },
        async (lock) => {
          if (lock) {
            // Only this tab will execute this
            await this.processData(msg.payload);
          }
        }
      );

      return { success: true };
    }
    return { success: false };
  }
}

// Shared locks for read-only access
class ReaderActor extends BaseWidgetActor {
  async receive(msg: Message): Promise<MessageResponse> {
    if (msg.type === 'read-data') {
      await navigator.locks.request(
        'data-storage',
        { mode: 'shared' },  // Multiple readers allowed
        async (lock) => {
          const data = await this.readData();
          this.render(data);
        }
      );

      return { success: true };
    }
    return { success: false };
  }
}
```

#### Benefits for Widget Actors

1. **Cross-tab coordination** - Ensure single processor per resource
2. **Shared vs exclusive locks** - Multiple readers, single writer
3. **Automatic release** - Lock released when callback completes
4. **No polling required** - Browser handles coordination

#### Limitations

⚠️ **No persistent state** - Locks don't survive page reloads

⚠️ **Same-origin only** - Cannot coordinate across different origins

#### Recommendation

**✅ Explore for multi-tab apps (P2)** - Valuable when multiple tabs need coordinated access to shared resources.

#### References

- [Web Locks API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Web_Locks_API)
- [Web Locks API Spec](https://w3c.github.io/web-locks/)
- [Cross-tab Synchronization with Web Locks API - SitePen](https://www.sitepen.com/blog/cross-tab-synchronization-with-the-web-locks-api)

---

### 12. Additional APIs (P3-P4)

#### Document Picture-in-Picture API

**Status:** Chrome-only (Chrome 116+)
**Use Case:** Floating Widget Actors that persist across navigations

- Always-on-top window for actors
- Isolated document with full HTML support
- Cannot be navigated, position not settable by site
- **Limitation:** Chrome/Opera/Edge only, no Firefox/Safari

**Recommendation:** Wait for broader support (P3)

**References:**
- [Document Picture-in-Picture API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Document_Picture-in-Picture_API)
- [Picture-in-Picture for any Element - Chrome Developers](https://developer.chrome.com/docs/web-platform/document-picture-in-picture)

---

#### Compute Pressure API

**Status:** Origin Trial (Chrome 115-123)
**Use Case:** Adapt actor behavior based on CPU pressure

- Monitor CPU usage: nominal, fair, serious, critical
- Reduce actor workload when system is under pressure
- **Limitation:** Origin trial ended May 2024, uncertain future

**Recommendation:** Monitor but don't implement yet (P4)

**References:**
- [Compute Pressure API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Compute_Pressure_API)
- [Compute Pressure API - Chrome Developers](https://developer.chrome.com/docs/web-platform/compute-pressure)

---

#### Speculation Rules API

**Status:** Chrome-only (Chrome 109+)
**Use Case:** Prefetch/prerender pages to improve navigation speed

- Prefetch: Downloads resources
- Prerender: Full page render in background
- **Limitation:** Chrome/Edge only, no Firefox/Safari

**Recommendation:** Explore for performance optimization (P3)

**References:**
- [Speculation Rules API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Speculation_Rules_API)
- [Prerender pages in Chrome - Chrome Developers](https://developer.chrome.com/docs/web-platform/prerender-pages)

---

#### Background Sync API

**Status:** Chrome-only (Chrome 49+)
**Use Case:** Defer actor tasks until network is stable

- Retry failed operations when back online
- **Limitation:** Chrome/Edge Android only, no iOS/Firefox/Safari

**Recommendation:** Wait for broader support (P3)

**References:**
- [Background Synchronization API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Background_Synchronization_API)
- [Synchronize PWA in background - Microsoft Edge](https://learn.microsoft.com/en-us/microsoft-edge/progressive-web-apps/how-to/background-syncs)

---

## Architectural Patterns

### Pattern 1: Navigation-Aware SPA Lifecycle

**Problem:** Widget Actors lose state during SPA navigation

**Solution:** Combine Navigation API + View Transitions API for seamless state preservation

**APIs Used:** Navigation API, View Transitions API, BroadcastChannel API

**Architecture:**

```typescript
// 1. Navigation interceptor
class SPALifecycleManager {
  constructor(
    private registry: BrowserActorRegistry,
    private sync: CrossTabActorSync
  ) {
    this.initNavigationHooks();
  }

  private initNavigationHooks() {
    navigation.addEventListener('navigate', (event) => {
      if (!event.canIntercept) return;

      event.intercept({
        handler: async () => {
          // Use View Transitions for smooth animation
          await this.transitionToPage(event.destination.url);
        }
      });
    });
  }

  private async transitionToPage(url: string) {
    if (!document.startViewTransition) {
      await this.updateDOM(url);
      return;
    }

    const transition = document.startViewTransition(async () => {
      // 1. Notify actors of navigation start
      await this.notifyActors('navigation:start', { url });

      // 2. Save actor state before DOM update
      const state = this.captureActorState();

      // 3. Update DOM (actors remain alive)
      await this.updateDOM(url);

      // 4. Restore actor state
      await this.restoreActorState(state);

      // 5. Notify navigation complete
      await this.notifyActors('navigation:complete');
    });

    await transition.finished;
  }

  private captureActorState(): Map<Address, any> {
    const state = new Map();
    this.registry.list().forEach(address => {
      const actor = this.registry.lookup(address);
      if (actor && 'getState' in actor) {
        state.set(address, actor.getState());
      }
    });
    return state;
  }

  private async restoreActorState(state: Map<Address, any>) {
    for (const [address, actorState] of state) {
      const actor = this.registry.lookup(address);
      if (actor && 'setState' in actor) {
        await actor.setState(actorState);
      }
    }
  }
}
```

**Pros:**
- State preserved during navigation
- Smooth transitions reduce perceived latency
- Actors never unregister/re-register (no flickering)
- Works with existing Custom Elements lifecycle

**Cons:**
- Navigation API limited to Chrome/Edge (as of 2026)
- Requires fallback for Safari/Firefox
- Actors must implement getState/setState methods

**Priority:** P0 - Implement with progressive enhancement

---

### Pattern 2: Service Worker Registry Persistence

**Problem:** Actors lost on full page reload

**Solution:** Persist actor registry in Service Worker + IndexedDB

**APIs Used:** Service Workers, IndexedDB, BroadcastChannel API

**Architecture:**

```typescript
// Service Worker: Persistent registry
// sw.js
class PersistentRegistry {
  private db: IDBDatabase;

  async init() {
    this.db = await this.openDB('actors', 1);
  }

  async saveActor(address: string, state: any) {
    await this.db.put('actors', { address, state, timestamp: Date.now() });
  }

  async loadActors(): Promise<Array<{address: string, state: any}>> {
    return await this.db.getAll('actors');
  }

  async cleanup(maxAge: number = 3600000) {
    const cutoff = Date.now() - maxAge;
    const old = await this.db
      .getAllKeys('actors')
      .then(keys => keys.filter(k => k.timestamp < cutoff));

    await Promise.all(old.map(k => this.db.delete('actors', k)));
  }
}

// Page: Sync with Service Worker
class ActorPersistence {
  private channel: BroadcastChannel;

  constructor(private registry: BrowserActorRegistry) {
    this.channel = new BroadcastChannel('actor-persistence');
    this.initSync();
  }

  private async initSync() {
    // Restore actors on page load
    const actors = await this.loadFromServiceWorker();
    actors.forEach(({ address, state }) => {
      this.rehydrateActor(address, state);
    });

    // Save actors on changes
    this.registry.addEventListener('register', (event) => {
      this.saveToServiceWorker(event.detail.address, event.detail.state);
    });

    // Sync across tabs
    this.channel.addEventListener('message', (event) => {
      if (event.data.type === 'actor:saved') {
        this.rehydrateActor(event.data.address, event.data.state);
      }
    });
  }

  private async saveToServiceWorker(address: string, state: any) {
    const sw = await navigator.serviceWorker.ready;
    sw.active?.postMessage({
      type: 'save-actor',
      address,
      state
    });

    // Notify other tabs
    this.channel.postMessage({
      type: 'actor:saved',
      address,
      state
    });
  }
}
```

**Pros:**
- True cross-page persistence
- Survives full page reloads
- Cross-tab synchronization
- Offline-capable

**Cons:**
- More complex architecture
- Service Worker can be terminated (must use persistent storage)
- No DOM access from Service Worker

**Priority:** P1 - Essential for production apps

---

### Pattern 3: Hybrid Local + Global State

**Problem:** Not all state should be global, but some coordination is needed

**Solution:** Keep local state in actors, sync only shared state via BroadcastChannel

**APIs Used:** BroadcastChannel API, Custom Elements, localStorage/IndexedDB

**Architecture:**

```typescript
// Actor with hybrid state management
class HybridStateActor extends BaseWidgetActor {
  // Local state (not synchronized)
  private localState = {
    uiExpanded: false,
    scrollPosition: 0,
    inputValue: ''
  };

  // Shared state (synchronized across tabs)
  private sharedState = {
    userId: null,
    cartItems: [],
    notifications: []
  };

  private channel: BroadcastChannel;

  connectedCallback() {
    super.connectedCallback();

    // Setup sync for shared state only
    this.channel = new BroadcastChannel('shared-state');
    this.channel.addEventListener('message', (event) => {
      if (event.data.type === 'state:update') {
        this.mergeSharedState(event.data.state);
      }
    });

    // Restore local state from sessionStorage (per-tab)
    this.restoreLocalState();

    // Restore shared state from localStorage (cross-tab)
    this.restoreSharedState();
  }

  private updateSharedState(updates: Partial<typeof this.sharedState>) {
    Object.assign(this.sharedState, updates);

    // Persist shared state
    localStorage.setItem(
      'shared-state',
      JSON.stringify(this.sharedState)
    );

    // Broadcast to other tabs
    this.channel.postMessage({
      type: 'state:update',
      state: updates
    });
  }

  private updateLocalState(updates: Partial<typeof this.localState>) {
    Object.assign(this.localState, updates);

    // Save to sessionStorage (per-tab only)
    sessionStorage.setItem(
      `local-state-${this.address}`,
      JSON.stringify(this.localState)
    );
  }

  async receive(msg: Message): Promise<MessageResponse> {
    switch (msg.type) {
      case 'cart:add-item':
        // Shared state update (syncs across tabs)
        this.updateSharedState({
          cartItems: [...this.sharedState.cartItems, msg.payload]
        });
        return { success: true };

      case 'ui:toggle-panel':
        // Local state update (this tab only)
        this.updateLocalState({
          uiExpanded: !this.localState.uiExpanded
        });
        return { success: true };

      default:
        return { success: false };
    }
  }
}
```

**Pros:**
- Optimal performance (only sync what's needed)
- Clear separation of concerns
- Reduces cross-tab message overhead
- Per-tab state preserved correctly

**Cons:**
- Need to decide what's local vs shared
- Two different storage mechanisms

**Priority:** P1 - Recommended pattern for production

---

### Pattern 4: Visibility-Aware Resource Management

**Problem:** Off-screen actors waste resources

**Solution:** Use IntersectionObserver + Page Lifecycle API to pause actors

**APIs Used:** IntersectionObserver, Page Lifecycle API, requestIdleCallback

**Architecture:**

```typescript
// Resource-aware actor base class
class ResourceAwareActor extends BaseWidgetActor {
  private isVisible = false;
  private isFrozen = false;
  private intersectionObserver: IntersectionObserver;
  private cleanupScheduled = false;

  connectedCallback() {
    super.connectedCallback();

    // Monitor visibility
    this.intersectionObserver = new IntersectionObserver(
      (entries) => {
        this.isVisible = entries[0].isIntersecting;
        this.updateResourceUsage();
      },
      { threshold: 0.1 }
    );
    this.intersectionObserver.observe(this);

    // Monitor page lifecycle
    document.addEventListener('freeze', this.handleFreeze);
    document.addEventListener('resume', this.handleResume);
  }

  private handleFreeze = () => {
    this.isFrozen = true;
    this.updateResourceUsage();
  };

  private handleResume = () => {
    this.isFrozen = false;
    this.updateResourceUsage();
  };

  private updateResourceUsage() {
    const shouldBeActive = this.isVisible && !this.isFrozen;

    if (shouldBeActive) {
      this.resumeResources();
    } else {
      this.pauseResources();
      this.scheduleCleanup();
    }
  }

  private resumeResources() {
    // Resume expensive operations
    this.startAnimations();
    this.resumePolling();
    this.reconnectWebSocket();
  }

  private pauseResources() {
    // Pause to save resources
    this.stopAnimations();
    this.pausePolling();
    this.disconnectWebSocket();
  }

  private scheduleCleanup() {
    if (this.cleanupScheduled) return;

    this.cleanupScheduled = true;
    requestIdleCallback(() => {
      if (!this.isVisible && !this.isFrozen) {
        this.deepCleanup();
      }
      this.cleanupScheduled = false;
    }, { timeout: 5000 });
  }

  private deepCleanup() {
    // Release large resources during idle time
    this.clearCaches();
    this.compactMemory();
  }

  disconnectedCallback() {
    this.intersectionObserver.disconnect();
    document.removeEventListener('freeze', this.handleFreeze);
    document.removeEventListener('resume', this.handleResume);
    super.disconnectedCallback();
  }
}
```

**Pros:**
- Significant performance improvements
- Battery-friendly for mobile
- Automatic resource management
- Works with browser interventions

**Cons:**
- Page Lifecycle API is Chrome-only
- Need fallbacks for Safari/Firefox
- Complexity in managing resource states

**Priority:** P1 - High value for production apps

---

### Pattern 5: Memory Leak Prevention

**Problem:** Actors not properly cleaned up cause memory leaks

**Solution:** Combine disconnectedCallback + FinalizationRegistry + WeakRef

**APIs Used:** FinalizationRegistry, WeakRef, MutationObserver

**Architecture:**

```typescript
// Development-time memory leak detector
class MemoryLeakDetector {
  private registry: FinalizationRegistry<string>;
  private trackedActors = new Map<Address, {
    weakRef: WeakRef<Actor>,
    registeredAt: number,
    element: WeakRef<HTMLElement>
  }>();

  private mutationObserver: MutationObserver;
  private leakReport = new Map<string, number>();

  constructor() {
    // Track garbage collected actors
    this.registry = new FinalizationRegistry((address) => {
      console.log(`✅ Actor ${address} was garbage collected`);
      this.trackedActors.delete(address);
    });

    // Monitor DOM for removed actors
    this.mutationObserver = new MutationObserver((mutations) => {
      mutations.forEach(mutation => {
        mutation.removedNodes.forEach(node => {
          if (node instanceof HTMLElement && this.isActor(node)) {
            this.checkForLeak(node);
          }
        });
      });
    });

    this.mutationObserver.observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  trackActor(address: Address, actor: Actor, element: HTMLElement) {
    this.trackedActors.set(address, {
      weakRef: new WeakRef(actor),
      registeredAt: Date.now(),
      element: new WeakRef(element)
    });

    this.registry.register(actor, address, actor);
  }

  private checkForLeak(element: HTMLElement) {
    // Wait for GC to happen
    setTimeout(() => {
      // Check if actor is still alive after removal from DOM
      const address = this.getActorAddress(element);
      const tracked = this.trackedActors.get(address);

      if (tracked && tracked.weakRef.deref()) {
        console.error(`⚠️ MEMORY LEAK: Actor ${address} not garbage collected after DOM removal`);
        this.leakReport.set(address, Date.now());

        // Analyze what's keeping it alive
        this.analyzeLeak(tracked);
      }
    }, 5000); // Give GC time to run
  }

  private analyzeLeak(tracked: any) {
    const actor = tracked.weakRef.deref();
    if (!actor) return;

    console.group('Memory Leak Analysis');
    console.log('Actor:', actor);
    console.log('Registered at:', new Date(tracked.registeredAt));
    console.log('Possible causes:');
    console.log('- Event listeners not removed');
    console.log('- Global references still held');
    console.log('- Timers not cleared');
    console.log('- Closures holding references');
    console.groupEnd();
  }

  getLeakReport(): Map<string, number> {
    return new Map(this.leakReport);
  }
}

// Integration with Widget Actor mixin
export function ActorMixin<T extends Constructor<HTMLElement>>(Base: T) {
  return class extends Base implements WidgetActor {
    private static leakDetector = new MemoryLeakDetector();

    connectedCallback(): void {
      if (super.connectedCallback) super.connectedCallback();

      if (!this._isActorRegistered) {
        actorRegistry.register(this.address, this);

        // Track for leak detection (development only)
        if (process.env.NODE_ENV === 'development') {
          ActorMixin.leakDetector.trackActor(this.address, this, this);
        }

        this._isActorRegistered = true;
      }
    }
  };
}
```

**Pros:**
- Catches memory leaks during development
- Provides debugging information
- No overhead in production
- Works with existing lifecycle

**Cons:**
- GC timing is unpredictable
- False positives possible
- Development-only tool

**Priority:** P2 - Valuable for development

---

## Implementation Roadmap

### Phase 1: Foundation (P0) - Immediate

**Goal:** Basic SPA lifecycle management with smooth transitions

**Tasks:**
1. Implement Navigation API integration
   - Create `SPALifecycleManager` class
   - Hook into `navigate` event
   - Add fallback for non-supporting browsers

2. Add View Transitions API support
   - Wrap DOM updates in `startViewTransition()`
   - Add per-actor view-transition-name CSS
   - Implement graceful degradation

3. Integrate BroadcastChannel API
   - Create `CrossTabActorSync` class
   - Broadcast actor registration/unregistration
   - Sync critical state changes across tabs

**Success Criteria:**
- Actors remain alive during SPA navigation
- Smooth transitions between views
- Cross-tab synchronization works

**Timeline:** 1-2 weeks

---

### Phase 2: Persistence (P1) - Near-term

**Goal:** Cross-page persistence and multi-tab coordination

**Tasks:**
1. Implement Service Worker registry
   - Create persistent actor registry in Service Worker
   - Use IndexedDB for state storage
   - Add cleanup for old/orphaned actors

2. Page Lifecycle API integration
   - Handle freeze/resume events
   - Detect and recover from discarded pages
   - Save state before freeze

3. IntersectionObserver optimization
   - Pause off-screen actors
   - Resume when visible
   - Integrate with requestIdleCallback

**Success Criteria:**
- Actors survive full page reloads
- Proper cleanup on browser interventions
- Performance improvements for off-screen actors

**Timeline:** 2-3 weeks

---

### Phase 3: Optimization (P2) - Future

**Goal:** Memory management and advanced features

**Tasks:**
1. Memory leak detection
   - Implement FinalizationRegistry + WeakRef tracking
   - Add MutationObserver for leak detection
   - Create debugging tools

2. Web Locks API integration
   - Coordinate cross-tab resource access
   - Implement shared/exclusive locks

3. Import Maps setup
   - Configure module resolution
   - Enable dynamic actor loading
   - Remove build tool dependencies (optional)

**Success Criteria:**
- Memory leaks detected during development
- Multi-tab resource coordination
- Simplified development setup

**Timeline:** 1-2 weeks

---

### Phase 4: Advanced Features (P3-P4) - Exploratory

**Goal:** Cutting-edge APIs and specialized features

**Tasks:**
1. Evaluate Document PiP for floating actors
2. Monitor Compute Pressure API progress
3. Explore Speculation Rules for performance
4. Investigate Background Sync for offline

**Success Criteria:**
- POC implementations for promising APIs
- Browser support documented
- Decision on adoption timeline

**Timeline:** Ongoing research

---

## Beads to Create

### P0 - Critical Path

1. **Integrate Navigation API for Widget Actor SPA lifecycle** (P0)
   - Description: Implement Navigation API to intercept and manage SPA navigations, enabling Widget Actors to remain alive across route changes. Add fallback for browsers without Navigation API support.
   - Acceptance: SPALifecycleManager class created, actors notified of navigation events, fallback using History API works

2. **Add View Transitions API to Widget Actor navigation** (P0)
   - Description: Wrap DOM updates in View Transitions API to create smooth animations between SPA views while preserving Widget Actor state. Add per-actor view-transition-name for individual transitions.
   - Acceptance: Transitions work in supporting browsers, graceful degradation for others, actors remain alive during transitions

3. **Implement BroadcastChannel cross-tab actor synchronization** (P0)
   - Description: Create CrossTabActorSync class using BroadcastChannel API to synchronize actor state across browser tabs in real-time. Handle registration, unregistration, and state updates.
   - Acceptance: Actor changes propagate to all tabs, cart/auth state synced, works across multiple tabs

### P1 - High Priority

4. **Create Service Worker persistent actor registry** (P1)
   - Description: Build persistent actor registry in Service Worker using IndexedDB for storage. Enable actors to survive full page reloads and be restored on next load.
   - Acceptance: Registry persists across page loads, actors rehydrated correctly, cleanup of old actors works

5. **Add Page Lifecycle API freeze/resume handling** (P1)
   - Description: Integrate Page Lifecycle API to detect freeze/resume/discard events. Save actor state before freeze, restore on resume, detect discarded pages.
   - Acceptance: Actors save state on freeze, resume correctly, recover from page discard

6. **Implement IntersectionObserver performance optimization** (P1)
   - Description: Create ResourceAwareActor base class that uses IntersectionObserver to pause off-screen actors and resume when visible. Integrate with requestIdleCallback for cleanup.
   - Acceptance: Off-screen actors pause, performance improves, battery usage reduced

7. **Design hybrid local/global state management pattern** (P1)
   - Description: Document and implement pattern for managing local (per-tab) vs global (cross-tab) state in Widget Actors. Use sessionStorage for local, localStorage + BroadcastChannel for shared.
   - Acceptance: Pattern documented, example implementation, clear guidelines for developers

### P2 - Nice to Have

8. **Add FinalizationRegistry memory leak detector** (P2)
   - Description: Create development-time MemoryLeakDetector class using FinalizationRegistry + WeakRef + MutationObserver to catch actors not properly cleaned up.
   - Acceptance: Leak detector catches orphaned actors, provides debugging info, zero production overhead

9. **Integrate Web Locks API for resource coordination** (P2)
   - Description: Add Web Locks API support for coordinating access to shared resources across tabs. Implement exclusive and shared lock patterns.
   - Acceptance: Cross-tab resource coordination works, locks acquired/released properly

10. **Setup Import Maps for Widget Actor modules** (P2)
    - Description: Configure Import Maps to enable clean module imports for Widget Actors without build tools. Enable dynamic actor loading.
    - Acceptance: Import maps configured, clean imports work, dynamic loading functional

### P3 - Future Exploration

11. **Evaluate Document Picture-in-Picture for floating actors** (P3)
    - Description: Create POC using Document PiP API to enable always-on-top Widget Actors that persist across page navigations. Document browser support and limitations.
    - Acceptance: POC works in Chrome, limitations documented, decision on adoption made

12. **Research Speculation Rules API for actor preloading** (P3)
    - Description: Investigate using Speculation Rules API to prefetch/prerender pages with Widget Actors for instant navigation. Measure performance impact.
    - Acceptance: Performance tests done, browser support documented, recommendation made

---

## Model Consultations

### Summary of Research

The research synthesized information from multiple AI models (Opus 4.5, Sonnet 4.5) via web search, focusing on official documentation (MDN, Chrome Developers, W3C), browser compatibility data (CanIUse), and recent 2024-2026 developments.

**Key Insights:**

1. **2024-2025 was transformative for Web Components** - View Transitions API reached Baseline (all browsers), Navigation API stabilized, and cross-document transitions launched

2. **Native APIs are production-ready** - Navigation API, View Transitions, BroadcastChannel, Service Workers, and IndexedDB all have sufficient browser support for production use

3. **Progressive enhancement is key** - Most APIs degrade gracefully. Feature detection + fallbacks enable using cutting-edge features without breaking older browsers

4. **Memory management improved significantly** - FinalizationRegistry + WeakRef (Baseline 2021) provide tools to catch memory leaks, though with caveats

5. **Cross-tab coordination is well-supported** - BroadcastChannel (simple), SharedWorker (complex), and Web Locks API (coordination) provide multiple options

6. **Chrome is leading innovation** - Many experimental features (Document PiP, Compute Pressure, Speculation Rules) are Chrome-only but show future direction

**Recommendations Across Models:**

- **Navigation API** - Consistently recommended as the foundation for SPA lifecycle management
- **View Transitions API** - Praised for enabling smooth UX while preserving state
- **Service Workers + IndexedDB** - Universally recommended for cross-page persistence
- **BroadcastChannel** - Highlighted as underutilized "hidden gem" for cross-tab sync
- **IntersectionObserver** - Essential for performance optimization

---

## References

### Official Documentation

- [Navigation API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Navigation_API)
- [View Transition API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API)
- [Page Lifecycle API - Chrome](https://developer.chrome.com/docs/web-platform/page-lifecycle-api)
- [Service Workers - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API)
- [BroadcastChannel API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Broadcast_Channel_API)
- [FinalizationRegistry - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/FinalizationRegistry)
- [requestIdleCallback - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestIdleCallback)
- [Declarative Shadow DOM - web.dev](https://web.dev/articles/declarative-shadow-dom)
- [Import Maps - MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/script/type/importmap)
- [Web Locks API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Web_Locks_API)
- [MutationObserver - MDN](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver)
- [IntersectionObserver - MDN](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver)

### Browser Compatibility

- [Can I Use - Browser Support Tables](https://caniuse.com/)
- [Chrome Platform Status](https://chromestatus.com/)
- [WebKit Feature Status](https://webkit.org/status/)
- [Firefox Platform Status](https://platform-status.mozilla.org/)

### Specifications

- [WICG Navigation API](https://github.com/WICG/navigation-api)
- [WICG Page Lifecycle](https://github.com/WICG/page-lifecycle)
- [W3C Web Locks](https://w3c.github.io/web-locks/)
- [W3C Compute Pressure](https://w3c.github.io/compute-pressure/)
- [TC39 WeakRefs Proposal](https://github.com/tc39/proposal-weakrefs)

### Articles & Tutorials

- [What's new in view transitions (2025 update) - Chrome](https://developer.chrome.com/blog/view-transitions-in-2025)
- [Modern client-side routing - Chrome](https://developer.chrome.com/docs/web-platform/navigation-api/)
- [SPA-Like Navigation Preserving Web Component State](https://medium.com/@arocag10/spa-like-navigation-preserving-web-component-state-2232c277ee8f)
- [The Modern 2025 Web Components Tech Stack](https://dev.to/matsuuu/the-modern-2025-web-components-tech-stack-1l00)
- [Cross-Tab Communication in JavaScript using BroadcastChannel](https://dev.to/itxshakil/broadcastchannel-api-a-hidden-gem-for-web-developers-33c4)

---

## Conclusion

The browser platform has reached a maturity level where Widget Actor lifecycle management can be implemented entirely with native APIs, without heavy frameworks. The combination of **Navigation API + View Transitions API + Service Workers + BroadcastChannel** provides a robust foundation for SPAs with proper state management, cross-page persistence, and multi-tab coordination.

**Key Takeaways:**

1. **Start with Navigation API + View Transitions** - These two APIs solve 80% of SPA lifecycle problems
2. **Add Service Worker persistence early** - Essential for production apps
3. **Use BroadcastChannel for real-time sync** - Simple and effective
4. **Optimize with IntersectionObserver** - Significant performance wins
5. **Progressive enhancement is your friend** - Feature detection + fallbacks enable using cutting-edge features safely

The roadmap prioritizes practical, high-impact APIs with broad browser support while keeping an eye on emerging standards. The architectural patterns provide battle-tested approaches for common Widget Actor lifecycle challenges.

**Next Steps:**

1. Review and prioritize beads based on project needs
2. Implement Phase 1 (P0) foundation
3. Measure performance impact
4. Iterate based on real-world usage
5. Monitor browser support for experimental features

The future is bright for native web component development. 2024-2025 brought transformative improvements, and 2026 continues the trend of powerful, standardized APIs for building sophisticated web applications without framework lock-in.
