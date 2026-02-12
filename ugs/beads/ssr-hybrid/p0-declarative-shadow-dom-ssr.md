# P0: Implement Declarative Shadow DOM for Widget Actor SSR

## Problem

Widget Actors currently only render client-side. This causes:
- Flash of Unstyled Content (FOUC)
- Poor SEO (no HTML content on initial load)
- Slow First Contentful Paint (FCP)
- No progressive enhancement baseline

## Solution

Implement server-side rendering for Widget Actors using Declarative Shadow DOM (DSD), which reached Baseline browser support in August 2024.

## Benefits

- ✅ Zero hydration cost for styles (CSS already applied)
- ✅ No FOUC (styled on first paint)
- ✅ SEO-friendly (Shadow DOM content in HTML)
- ✅ Streaming compatible (works with chunked delivery)
- ✅ Progressive enhancement (HTML works before JS)

## Technical Approach

### 1. SSR Rendering Utility

Create utility to generate DSD HTML for Widget Actors:

```typescript
// src/messaging/browser/ssr-utils.ts
interface WidgetActorSSROptions {
  tagName: string;
  id: string;
  styles: string;
  content: string;
  attributes?: Record<string, string>;
}

export function renderWidgetActorSSR(options: WidgetActorSSROptions): string {
  const { tagName, id, styles, content, attributes = {} } = options;

  const attrs = Object.entries(attributes)
    .map(([key, value]) => `${key}="${value}"`)
    .join(' ');

  return `
    <${tagName} id="${id}" ${attrs}>
      <template shadowrootmode="open">
        <style>${styles}</style>
        ${content}
      </template>
    </${tagName}>
  `;
}
```

### 2. Update Widget Actor Mixin for DSD Hydration

Modify `widget-actor.ts` to detect and use existing Shadow DOM.

### 3. Example Widget Actor with SSR Support

Implement ProductCard example with both SSR and CSR paths.

## Implementation Steps

1. Create `src/messaging/browser/ssr-utils.ts` with rendering utilities
2. Update `widget-actor.ts` ActorMixin to detect existing Shadow DOM
3. Add `renderShadowDOM()` and `attachEventListeners()` protected methods
4. Create example Widget Actor with SSR support
5. Build server-side rendering endpoint
6. Add tests for SSR rendering
7. Document SSR pattern in README

## Acceptance Criteria

- ✅ Widget Actors render server-side with Declarative Shadow DOM
- ✅ No FOUC on initial page load
- ✅ Styles applied immediately (before JavaScript loads)
- ✅ Client-side hydration works for both SSR and CSR paths
- ✅ Event listeners attach correctly after hydration

**Priority:** P0
**Complexity:** Low
**Performance Impact:** High (30-50% FCP improvement)
