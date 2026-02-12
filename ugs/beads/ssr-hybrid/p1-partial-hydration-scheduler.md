# P1: Partial Hydration Scheduler for Widget Actors

## Problem

All Widget Actors hydrate immediately on page load, even when:
- Not visible in viewport
- Not needed for initial interaction
- User may never interact with them

This causes unnecessary JavaScript execution and poor TTI.

## Solution

Build priority-based hydration scheduler that hydrates Widget Actors based on visibility, user interaction, or idle time.

## Benefits

- ✅ 500-800ms TTI improvement
- ✅ Reduced CPU usage on initial load
- ✅ Better battery life on mobile
- ✅ Customizable hydration strategies per Widget Actor

## Technical Approach

### Hydration Metadata System

```typescript
// src/messaging/browser/hydration-scheduler.ts
interface HydrationMetadata {
  priority: 'critical' | 'high' | 'low' | 'never';
  trigger: 'immediate' | 'idle' | 'visible' | 'interaction';
}

class HydrationScheduler {
  private queue: Array<{
    element: HTMLElement;
    metadata: HydrationMetadata;
  }> = [];

  scheduleHydration(element: HTMLElement, metadata: HydrationMetadata) {
    this.queue.push({ element, metadata });
    this.processQueue();
  }

  private processQueue() {
    // Sort by priority
    this.queue.sort((a, b) => {
      const priorities = { critical: 0, high: 1, low: 2, never: 999 };
      return priorities[a.metadata.priority] - priorities[b.metadata.priority];
    });

    this.queue.forEach(({ element, metadata }) => {
      switch (metadata.trigger) {
        case 'immediate':
          this.hydrate(element);
          break;

        case 'idle':
          requestIdleCallback(() => this.hydrate(element));
          break;

        case 'visible':
          const observer = new IntersectionObserver((entries) => {
            if (entries[0].isIntersecting) {
              this.hydrate(element);
              observer.disconnect();
            }
          });
          observer.observe(element);
          break;

        case 'interaction':
          element.addEventListener('click', () => this.hydrate(element), { once: true });
          break;
      }
    });
  }

  private hydrate(element: HTMLElement) {
    const widgetType = element.getAttribute('data-widget-type');
    if (!widgetType) return;

    // Dynamic import Widget Actor class
    import(`../widgets/${widgetType}.js`).then(module => {
      const WidgetClass = module.default;
      customElements.define(widgetType, WidgetClass);
    });
  }
}

export const hydrationScheduler = new HydrationScheduler();
```

### Server-Rendered HTML with Hydration Hints

```html
<!-- Critical: Hydrate immediately -->
<header-widget
  data-widget-type="header-widget"
  data-hydration-priority="critical"
  data-hydration-trigger="immediate"
>
  <template shadowrootmode="open">
    <!-- Static HTML -->
  </template>
</header-widget>

<!-- Low priority: Hydrate when visible -->
<product-card
  data-widget-type="product-card"
  data-hydration-priority="low"
  data-hydration-trigger="visible"
>
  <template shadowrootmode="open">
    <!-- Static HTML -->
  </template>
</product-card>

<!-- Interactive: Hydrate on first interaction -->
<chat-widget
  data-widget-type="chat-widget"
  data-hydration-priority="high"
  data-hydration-trigger="interaction"
>
  <template shadowrootmode="open">
    <!-- Static HTML -->
  </template>
</chat-widget>
```

### Bootstrap Script

```typescript
// Scan page and schedule hydration
document.querySelectorAll('[data-widget-type]').forEach(element => {
  hydrationScheduler.scheduleHydration(element as HTMLElement, {
    priority: element.getAttribute('data-hydration-priority') as any,
    trigger: element.getAttribute('data-hydration-trigger') as any
  });
});
```

## Implementation Steps

1. Create `src/messaging/browser/hydration-scheduler.ts`
2. Implement priority-based scheduling logic
3. Add IntersectionObserver for visibility-based hydration
4. Add requestIdleCallback for idle hydration
5. Add event-based hydration triggers
6. Update SSR rendering to include hydration metadata
7. Add tests for each hydration strategy
8. Measure TTI improvement

## Acceptance Criteria

- ✅ Critical Widget Actors hydrate immediately
- ✅ Low-priority Widget Actors hydrate when visible
- ✅ Idle hydration works with requestIdleCallback
- ✅ Interaction-based hydration defers until first click
- ✅ 500-800ms TTI improvement measured
- ✅ Tests pass for all hydration strategies

**Priority:** P1
**Complexity:** Medium
**Performance Impact:** High (500-800ms TTI improvement)
