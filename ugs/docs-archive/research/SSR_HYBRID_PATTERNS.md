# SSR & Hybrid Rendering Patterns for Widget Actors

**Research Date:** 2026-02-05
**Context:** Modern SSR, streaming, islands, resumability, and hybrid rendering patterns
**Focus:** "Into the page" methods (initial load/SSR) complementing "on the page" (post-load) patterns
**Complements:** [BROWSER_LIFECYCLE_RESEARCH.md](./BROWSER_LIFECYCLE_RESEARCH.md) (client-side lifecycle)

---

## Executive Summary

### Research Scope

This document covers **"into the page"** patterns—how Widget Actors get onto the page efficiently via SSR, streaming, islands, and hybrid approaches. It complements the existing client-side lifecycle research (Navigation API, View Transitions, BroadcastChannel) which covers **"on the page"** patterns.

### Top 5 Recommendations for Widget Actors

1. **Declarative Shadow DOM + Streaming HTML (P0)** - Use native DSD for SSR-friendly Widget Actors with streaming support. Zero JavaScript required for initial render, progressive enhancement for interactivity.

2. **Islands Architecture via Astro/Enhance (P0)** - Adopt islands pattern for selective hydration. Server-render static Widget Actors, hydrate only interactive ones. Reduces JavaScript by 60-80%.

3. **Progressive Enhancement Pattern (P0)** - Build HTML-first Widget Actors that work without JavaScript, then enhance. Essential for accessibility, SEO, and resilience.

4. **Streaming SSR with Suspense Boundaries (P1)** - Implement out-of-order streaming for slow-loading Widget Actors. Improves FCP by 40%, reduces perceived latency.

5. **Qwik Resumability Concepts (P2)** - Learn from Qwik's serialization approach for fine-grained lazy loading. Consider for high-complexity Widget Actor apps.

### Browser/Framework Support Matrix (2024-2026)

| Pattern | Native API | Framework Required | Browser Support | Maturity |
|---------|-----------|-------------------|----------------|----------|
| Declarative Shadow DOM | ✅ Yes | ❌ No | Chrome 90+, Safari 16.4+, Edge 91+ | **Baseline 2024** |
| Streaming HTML | ✅ Yes (fetch streams) | ⚠️ Optional | All modern | Baseline |
| Progressive Enhancement | ✅ Yes (HTML) | ❌ No | All | Baseline |
| Islands Architecture | ❌ No | ✅ Yes (Astro, Fresh, Enhance) | All | Framework-dependent |
| React Suspense Streaming | ❌ No | ✅ Yes (React 18+) | All | React 18+ |
| Resumability | ❌ No | ✅ Yes (Qwik only) | All | Qwik-specific |
| Partial Hydration | ❌ No | ✅ Yes (Multiple) | All | Framework-dependent |

### Performance Impact Summary

**Metrics (compared to full client-side rendering):**

- **First Contentful Paint (FCP):** 40-60% improvement with streaming SSR
- **Largest Contentful Paint (LCP):** 45% improvement with partial hydration
- **Time to Interactive (TTI):** 500-800ms improvement with islands/progressive hydration
- **JavaScript Bundle Size:** 60-80% reduction with islands architecture
- **Hydration Cost:** Near-zero with resumability (Qwik), 70% reduction with islands

**Trade-offs:**

- ✅ Faster initial load, better Core Web Vitals
- ✅ Better SEO, accessibility baseline
- ⚠️ Increased server complexity (SSR infrastructure)
- ⚠️ Potential TTFB increase if server-side rendering is slow

---

## 1. Streaming HTML & Progressive Rendering

### Overview

Streaming SSR progressively sends HTML chunks to the browser as they're generated, rather than waiting for the entire page. Enables users to see content faster and browser to start rendering immediately.

### Key Concepts (2024-2026)

**Streaming Fundamentals:**
- Send HTML in chunks (TTFB reduced significantly)
- Browser renders progressively (FCP improved by 40%+)
- Works with all modern browsers (native fetch streams API)
- Compatible with HTTP/2 and HTTP/3

**Out-of-Order Streaming:**
- Components stream as they complete, not in DOM order
- React 18+ uses hidden `<template>` tags with IDs
- JavaScript swaps placeholders when data arrives
- Critical for slow database queries or API calls

### Widget Actor Integration

#### Pattern 1: Streaming Widget Actor Shells

```typescript
// Server: Stream Widget Actor HTML progressively
export async function renderWidgetActorStream(req: Request): Promise<Response> {
  const stream = new ReadableStream({
    async start(controller) {
      // 1. Send shell immediately (static HTML)
      controller.enqueue(encoder.encode(`
        <!DOCTYPE html>
        <html>
        <head>
          <link rel="stylesheet" href="/widget-actors.css">
        </head>
        <body>
          <header-widget>
            <template shadowrootmode="open">
              <style>:host { display: block; }</style>
              <nav>Navigation Ready</nav>
            </template>
          </header-widget>
      `));

      // 2. Stream slow-loading Widget Actors as they complete
      const productPromise = fetchProducts();
      const reviewsPromise = fetchReviews();

      // Send products when ready (out-of-order)
      productPromise.then(products => {
        controller.enqueue(encoder.encode(`
          <product-list id="products">
            <template shadowrootmode="open">
              <style>/* Product styles */</style>
              ${products.map(p => `<product-card>${p.name}</product-card>`).join('')}
            </template>
          </product-list>
        `));
      });

      // Send reviews when ready (may arrive before products)
      reviewsPromise.then(reviews => {
        controller.enqueue(encoder.encode(`
          <review-list id="reviews">
            <template shadowrootmode="open">
              ${reviews.map(r => `<review-card>${r.text}</review-card>`).join('')}
            </template>
          </review-list>
        `));
      });

      // 3. Send closing tags and hydration script
      await Promise.all([productPromise, reviewsPromise]);
      controller.enqueue(encoder.encode(`
          <script type="module" src="/widget-actors.js"></script>
        </body>
        </html>
      `));

      controller.close();
    }
  });

  return new Response(stream, {
    headers: { 'Content-Type': 'text/html; charset=utf-8' }
  });
}
```

#### Pattern 2: Suspense-Like Boundaries for Widget Actors

```html
<!-- Server: Placeholder for slow Widget Actor -->
<product-details id="product-123" data-suspense>
  <template shadowrootmode="open">
    <div class="skeleton">Loading product details...</div>
  </template>
</product-details>

<template id="product-123-content" style="display:none">
  <product-details id="product-123">
    <template shadowrootmode="open">
      <style>/* Product styles */</style>
      <h1>Organic Mango</h1>
      <p>$3.99</p>
    </template>
  </product-details>
</template>

<script>
  // Swap placeholder with real content when available
  const placeholder = document.getElementById('product-123');
  const content = document.getElementById('product-123-content');

  if (placeholder && content) {
    placeholder.replaceWith(content.content.cloneNode(true));
  }
</script>
```

### Framework Examples

#### Next.js Streaming (React 18+)

```tsx
// app/page.tsx (Next.js App Router)
import { Suspense } from 'react';

export default function ProductPage() {
  return (
    <>
      {/* Sends immediately */}
      <HeaderWidget />

      {/* Streams when ready */}
      <Suspense fallback={<ProductListSkeleton />}>
        <ProductListWidget />
      </Suspense>

      {/* Streams independently */}
      <Suspense fallback={<ReviewsSkeleton />}>
        <ReviewsWidget />
      </Suspense>
    </>
  );
}

async function ProductListWidget() {
  const products = await fetchProducts(); // Server-side data fetching
  return <product-list products={products} />;
}
```

#### Remix Deferred Data

```tsx
// routes/products.$id.tsx
import { defer } from '@remix-run/node';
import { Await, useLoaderData } from '@remix-run/react';

export async function loader({ params }) {
  // Fast data: block rendering
  const product = await fetchProduct(params.id);

  // Slow data: defer (stream later)
  const reviews = fetchReviews(params.id); // No await!

  return defer({ product, reviews });
}

export default function ProductRoute() {
  const { product, reviews } = useLoaderData();

  return (
    <>
      <product-details product={product} />

      <Suspense fallback={<ReviewsSkeleton />}>
        <Await resolve={reviews}>
          {(resolvedReviews) => (
            <review-list reviews={resolvedReviews} />
          )}
        </Await>
      </Suspense>
    </>
  );
}
```

### Benefits for Widget Actors

1. **Faster FCP/LCP** - Critical Widget Actors render immediately
2. **Non-blocking** - Slow Widget Actors don't block fast ones
3. **Better UX** - Progressive loading feels faster (perceived performance)
4. **SEO-friendly** - Search engines see complete HTML

### Implementation Complexity

- **Low (Native Streams):** Use `ReadableStream` API directly
- **Medium (React/Next.js):** Requires React 18+ and Suspense boundaries
- **Low (Remix):** Built-in `defer()` support

### Recommendation

**✅ Implement immediately (P0)** for production Widget Actor apps. Use native streams for framework-agnostic approach, or React Suspense for React-based stacks.

### References

- [Streaming Server-Side Rendering](https://www.patterns.dev/react/streaming-ssr/)
- [Next.js 15 Streaming Handbook](https://www.freecodecamp.org/news/the-nextjs-15-streaming-handbook/)
- [Revisiting HTML streaming for modern web performance](https://calendar.perfplanet.com/2025/revisiting-html-streaming-for-modern-web-performance/)
- [Out Of Order Streaming by Theo Browne](https://gitnation.com/contents/out-of-order-streaming-the-secret-powering-modern-react)

---

## 2. Islands Architecture

### Overview

Islands architecture renders HTML pages on server, injects placeholders around highly dynamic regions, then hydrates only interactive "islands" on the client. Also known as **partial** or **selective hydration**.

### Key Concepts

**Core Principles:**
- Server renders all HTML (fast initial load)
- Only interactive components ship JavaScript
- Static components stay static (zero hydration cost)
- 60-80% JavaScript reduction typical

**Island Types:**
1. **Static Islands** - Pure HTML, no hydration
2. **Interactive Islands** - Hydrated with JavaScript
3. **Nested Islands** - Islands within islands

### Framework Comparison

#### Astro (Most Popular for Islands)

```astro
---
// src/pages/product.astro
import HeaderWidget from '../components/HeaderWidget.astro';
import ProductList from '../components/ProductList.jsx';
import ReviewForm from '../components/ReviewForm.svelte';
---

<html>
<head>
  <title>Product Page</title>
</head>
<body>
  <!-- Static: No JavaScript sent -->
  <HeaderWidget />

  <!-- Island: Hydrates on client with "client:load" -->
  <ProductList client:load products={products} />

  <!-- Island: Hydrates only when visible -->
  <ReviewForm client:visible />

  <!-- Island: Hydrates on user interaction -->
  <ChatWidget client:idle />
</body>
</html>
```

**Astro Hydration Strategies:**
- `client:load` - Hydrate immediately on page load
- `client:idle` - Hydrate when main thread is idle
- `client:visible` - Hydrate when element enters viewport
- `client:media` - Hydrate when media query matches
- `client:only` - Skip SSR, client-only rendering

#### Fresh (Deno Framework)

```tsx
// islands/ProductList.tsx (Fresh)
import { Signal } from "@preact/signals";

export default function ProductList({ products }: { products: Signal<Product[]> }) {
  // Only files in /islands directory ship JavaScript
  return (
    <product-list>
      {products.value.map(p => (
        <product-card onClick={() => addToCart(p)}>
          {p.name} - ${p.price}
        </product-card>
      ))}
    </product-list>
  );
}
```

Fresh uses **file-based islands** - only components in `/islands` directory are interactive.

#### Enhance.dev (HTML-First Web Components)

```html
<!-- enhance/elements/product-card.html -->
<template>
  <style>
    :host { display: block; border: 1px solid #ccc; }
  </style>
  <h3>${state.name}</h3>
  <p>${state.price}</p>
  <button>Add to Cart</button>
</template>

<script type="module">
  // Progressive enhancement - only runs client-side
  class ProductCard extends HTMLElement {
    connectedCallback() {
      this.querySelector('button').addEventListener('click', () => {
        this.dispatchEvent(new CustomEvent('add-to-cart', {
          detail: { id: this.dataset.id }
        }));
      });
    }
  }

  customElements.define('product-card', ProductCard);
</script>
```

Enhance server-renders Web Components as HTML, then progressively enhances.

### Widget Actor Integration

#### Pattern: Island-Based Widget Actor System

```typescript
// islands/InteractiveWidgetActor.ts
import { BaseWidgetActor } from '../messaging/browser/widget-actor.ts';

// Mark as island (only this ships to client)
export class InteractiveProductCard extends BaseWidgetActor {
  async receive(msg: Message): Promise<MessageResponse> {
    if (msg.type === 'add-to-cart') {
      // Interactive behavior
      await this.addToCart(msg.payload);
      return { success: true };
    }
    return { success: false };
  }
}

customElements.define('interactive-product-card', InteractiveProductCard);
```

```typescript
// components/StaticWidgetActor.astro (server-only)
---
// This NEVER ships JavaScript to client
export interface Props {
  product: Product;
}

const { product } = Astro.props;
---

<product-card data-static>
  <h3>{product.name}</h3>
  <p>${product.price}</p>
  <!-- Static content only -->
</product-card>
```

```astro
---
// pages/product-listing.astro
import StaticProductCard from '../components/StaticWidgetActor.astro';
import InteractiveProductCard from '../islands/InteractiveWidgetActor.ts';
---

<html>
<body>
  <!-- Static cards (no JS) -->
  {products.map(p => (
    <StaticProductCard product={p} />
  ))}

  <!-- Interactive cart widget (hydrated) -->
  <InteractiveProductCard client:load />
</body>
</html>
```

### Benefits for Widget Actors

1. **Massive JavaScript reduction** - Only interactive Widget Actors ship JS
2. **Faster TTI** - 500-800ms improvement in mobile tests
3. **Better Core Web Vitals** - 45% better LCP in real-world apps
4. **Framework flexibility** - Use React, Svelte, Vue islands together

### Implementation Complexity

- **Low (Astro):** File-based routing, clear island boundaries
- **Low (Fresh):** Convention-based, automatic island detection
- **Medium (Enhance):** Web Components focus, requires understanding SSR

### Framework Lock-in Assessment

- **Astro:** Low lock-in (can use any framework for islands)
- **Fresh:** Medium lock-in (Preact/Deno ecosystem)
- **Enhance:** Low lock-in (standard Web Components)

### Recommendation

**✅ Implement immediately (P0)** for content-heavy Widget Actor apps. Use **Astro** for maximum flexibility, **Enhance** for standards-first approach.

### References

- [Islands Architecture - Astro Docs](https://docs.astro.build/en/concepts/islands/)
- [Astro Islands Architecture Explained](https://strapi.io/blog/astro-islands-architecture-explained-complete-guide)
- [Islands Architecture Pattern](https://www.patterns.dev/vanilla/islands-architecture/)
- [Island Architecture with Web Components - Enhance.dev](https://enhance.dev/blog/posts/2024-07-09-island-architecture-with-web-components)

---

## 3. Qwik Resumability

### Overview

Qwik's **resumability** approach pauses execution on server and resumes on client without replaying application logic. Fundamentally different from hydration—no JavaScript execution until user interaction.

### Key Concepts

**Resumability vs Hydration:**

| Hydration (Traditional) | Resumability (Qwik) |
|------------------------|---------------------|
| Re-execute components client-side | Serialize state server-side |
| Re-attach event listeners | Serialize listeners to HTML |
| Rebuild component tree | Serialize component graph |
| **Cost:** O(n) JavaScript execution | **Cost:** O(1) startup |

**Serialization Strategy:**

Qwik serializes three critical pieces into HTML:

1. **Event Listeners** - Component boundary info in HTML attributes
2. **Component Tree** - Subscription graph between stores and components
3. **Application State** - JSON-serialized reactive state

### How It Works

```tsx
// Qwik component
import { component$, useSignal } from '@builder.io/qwik';

export const Counter = component$(() => {
  const count = useSignal(0);

  return (
    <button onClick$={() => count.value++}>
      Count: {count.value}
    </button>
  );
});
```

**Server renders to:**

```html
<!-- Qwik serializes everything needed to resume -->
<button
  on:click="./chunk-abc123.js#Counter_onClick[0]"
  q:id="1"
  q:key="abc"
>
  Count: 0
</button>

<script type="qwik/json">
{
  "ctx": {},
  "objs": [{"count": 0}],
  "subs": [["1", "count"]]
}
</script>
```

**On first click:**
1. Qwik runtime (1KB) intercepts click
2. Loads `chunk-abc123.js` (only the click handler)
3. Deserializes state from JSON
4. Executes handler with existing state
5. Updates DOM reactively

**Zero hydration cost** - No component code runs until user interaction.

### Serialization Limitations

JSON serialization creates DAGs (Directed Acyclic Graphs), not arbitrary graphs:
- ❌ Circular references not supported directly
- ✅ Qwik handles circular refs via special serialization
- ❌ Functions cannot be serialized (use `$()` wrapper)
- ✅ State, arrays, objects fully supported

### Widget Actor Conceptual Integration

**Note:** This is conceptual—Qwik is a full framework, not a drop-in pattern for Widget Actors. However, the concepts can inspire fine-grained lazy loading.

```typescript
// Conceptual: Qwik-inspired Widget Actor
class ResumableWidgetActor extends HTMLElement {
  // Serialize actor state to HTML attribute
  serializeState() {
    const state = {
      address: this.address,
      messages: this.messageQueue,
      subscriptions: this.subscriptions
    };
    this.setAttribute('data-actor-state', JSON.stringify(state));
  }

  // Resume from serialized state (no re-execution)
  resumeFromState() {
    const serialized = this.getAttribute('data-actor-state');
    if (!serialized) return;

    const state = JSON.parse(serialized);
    this.address = state.address;
    this.messageQueue = state.messages;
    this.subscriptions = state.subscriptions;

    // Re-attach listeners WITHOUT re-executing component logic
    this.attachSerializedListeners();
  }

  connectedCallback() {
    // Check if resuming from SSR
    if (this.hasAttribute('data-actor-state')) {
      this.resumeFromState();
    } else {
      // Fresh initialization
      this.initialize();
    }
  }
}
```

### Benefits of Resumability Approach

1. **Instant interactivity** - No JavaScript execution until interaction
2. **Perfect for slow devices** - Minimal CPU usage on load
3. **Scalable to large apps** - O(1) startup regardless of app size
4. **Better mobile performance** - Critical for battery life

### Implementation Complexity

- **High (Full Qwik Framework):** Complete paradigm shift
- **Medium (Concepts Only):** Apply serialization patterns to Widget Actors

### When to Use Qwik

✅ **Good fit:**
- Large, complex Widget Actor apps (1000+ components)
- Mobile-first applications
- E-commerce with many product cards
- Interactive dashboards with lazy sections

❌ **Not needed:**
- Small apps (<100 components)
- Apps already using islands effectively
- When SSR not critical

### Recommendation

**⏸️ Learn concepts (P2)** - Study Qwik's serialization approach for inspiration. Consider adopting for high-complexity Widget Actor apps where islands architecture isn't enough.

### References

- [Resumable - Qwik Documentation](https://qwik.dev/docs/concepts/resumable/)
- [Resumable JavaScript with Qwik](https://dev.to/this-is-learning/resumable-javascript-with-qwik-2i29)
- [Think Qwik - Qwik Documentation](https://qwik.dev/docs/concepts/think-qwik/)

---

## 4. Progressive Enhancement

### Overview

**Progressive enhancement** builds baseline HTML experiences that work without JavaScript, then layers in JavaScript for enhanced interactivity. Core principle: **HTML-first, JavaScript as enhancement**.

### Key Principles (2024 Perspective)

1. **HTML is the foundation** - Semantic markup provides baseline functionality
2. **CSS for presentation** - Style without JavaScript
3. **JavaScript enhances** - Add interactivity, don't replace HTML
4. **Resilience** - App degrades gracefully when JS fails/disabled

**Modern Relevance:**
- 1-2% of users have JavaScript disabled
- Network failures can prevent JS loading
- Search engines prefer HTML content
- Accessibility tools rely on semantic HTML

### Widget Actor Integration

#### Pattern 1: HTML-First Widget Actors

```html
<!-- Server-rendered Widget Actor (works without JS) -->
<shopping-cart>
  <template shadowrootmode="open">
    <style>
      :host { display: block; }
      .cart-item { display: flex; gap: 1rem; }
    </style>

    <!-- Baseline HTML form (works without JS) -->
    <form action="/api/cart/add" method="POST">
      <ul class="cart-items">
        <li class="cart-item">
          <span>Organic Mango</span>
          <span>$3.99</span>
          <button type="submit" name="remove" value="mango-123">
            Remove
          </button>
        </li>
      </ul>

      <input type="text" name="product-id" placeholder="Product ID">
      <button type="submit">Add to Cart</button>
    </form>
  </template>
</shopping-cart>

<script type="module">
  // Progressive enhancement: Intercept form submission for AJAX
  class ShoppingCart extends HTMLElement {
    connectedCallback() {
      const form = this.shadowRoot.querySelector('form');

      // Only enhance if JavaScript available
      form.addEventListener('submit', async (e) => {
        e.preventDefault(); // Prevent full page reload

        const formData = new FormData(form);
        const response = await fetch(form.action, {
          method: form.method,
          body: formData
        });

        // Update cart with JSON response
        const data = await response.json();
        this.updateCartUI(data);
      });
    }
  }

  customElements.define('shopping-cart', ShoppingCart);
</script>
```

**Key Points:**
- ✅ Form works without JavaScript (server-side POST)
- ✅ JavaScript enhances with AJAX (no page reload)
- ✅ Button remains functional regardless of JS state

#### Pattern 2: Progressive Disclosure

```typescript
// Widget Actor with progressive disclosure
class ExpandableWidget extends HTMLElement {
  connectedCallback() {
    // Use <details> for baseline expand/collapse (no JS needed)
    this.innerHTML = `
      <details>
        <summary>Product Details</summary>
        <div class="content">
          ${this.dataset.content}
        </div>
      </details>
    `;

    // Progressive enhancement: Add animations and transitions
    if ('animate' in HTMLElement.prototype) {
      const details = this.querySelector('details');
      details.addEventListener('toggle', () => {
        const content = this.querySelector('.content');
        if (details.open) {
          content.animate([
            { opacity: 0, transform: 'translateY(-10px)' },
            { opacity: 1, transform: 'translateY(0)' }
          ], { duration: 300, easing: 'ease-out' });
        }
      });
    }
  }
}

customElements.define('expandable-widget', ExpandableWidget);
```

### Framework Examples

#### Enhance.dev (HTML-First by Design)

```html
<!-- server/pages/product.html -->
<enhance:page>
  <product-card id="mango-123">
    <!-- Server-rendered HTML (works without JS) -->
    <h3>Organic Mango</h3>
    <p>$3.99</p>

    <!-- Standard form (progressive baseline) -->
    <form action="/api/cart" method="POST">
      <input type="hidden" name="product-id" value="mango-123">
      <button type="submit">Add to Cart</button>
    </form>
  </product-card>
</enhance:page>

<!-- app/elements/product-card.mjs -->
export default function ProductCard({ html, state }) {
  return html`
    <style>
      :host {
        display: block;
        border: 1px solid #ccc;
      }
    </style>
    <slot></slot>
  `
}

// Browser enhancement (optional)
if (typeof window !== 'undefined') {
  class ProductCard extends HTMLElement {
    connectedCallback() {
      // Enhance form with AJAX
      const form = this.querySelector('form');
      form.addEventListener('submit', this.handleSubmit.bind(this));
    }

    async handleSubmit(e) {
      e.preventDefault();
      // AJAX cart update
    }
  }

  customElements.define('product-card', ProductCard);
}
```

#### WebC (11ty)

```html
<!-- components/widget-card.webc -->
<template webc:type="11ty" 11ty:type="njk">
  <article class="widget-card">
    <h2>{{ title }}</h2>
    <p>{{ description }}</p>

    <!-- Baseline: Standard link -->
    <a href="/widgets/{{ id }}">View Details</a>
  </article>
</template>

<script webc:type="js" webc:is="script">
  // Progressive enhancement: Add prefetch
  if ('IntersectionObserver' in window) {
    const links = document.querySelectorAll('.widget-card a');
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const link = entry.target;
          link.addEventListener('mouseenter', () => {
            // Prefetch on hover
            const prefetch = document.createElement('link');
            prefetch.rel = 'prefetch';
            prefetch.href = link.href;
            document.head.appendChild(prefetch);
          }, { once: true });
          observer.unobserve(link);
        }
      });
    });

    links.forEach(link => observer.observe(link));
  }
</script>
```

### Benefits for Widget Actors

1. **Resilience** - Apps work even when JavaScript fails
2. **Accessibility** - Screen readers get semantic HTML
3. **SEO** - Search engines index content fully
4. **Performance** - Baseline experience loads instantly
5. **Developer confidence** - Clear separation of concerns

### Implementation Complexity

- **Low (HTML-First):** Start with forms, links, `<details>`
- **Low (CSS Enhancement):** Use `:hover`, `:focus-visible`
- **Medium (JavaScript Enhancement):** Intercept events carefully

### Recommendation

**✅ Implement immediately (P0)** for all Widget Actors. Progressive enhancement should be **default mindset**, not optional pattern.

### References

- [HTML Web Components Make Progressive Enhancement Easier](https://css-tricks.com/html-web-components-make-progressive-enhancement-and-css-encapsulation-easier/)
- [Progressively Enhancing a Web Component](https://gomakethings.com/progressively-enhancing-a-web-component/)
- [Web Components as Progressive Enhancement](https://cloudfour.com/thinks/web-components-as-progressive-enhancement/)
- [Progressive Enhancement - Piccalilli](https://piccalil.li/blog/its-about-time-i-tried-to-explain-what-progressive-enhancement-actually-is/)

---

## 5. React Server Components (RSC)

### Overview

React Server Components are components that render **only on the server**, never ship JavaScript to the client. Fundamentally different from SSR—RSCs remain server-only across navigations.

### Key Concepts

**Server vs Client Components:**

| Server Components | Client Components |
|------------------|-------------------|
| Render on server | Render on client |
| Zero JavaScript to browser | Ship JavaScript bundle |
| Can access databases directly | Browser APIs only |
| Cannot use hooks/state | Full React features |
| Async by default | Sync rendering |

**Server/Client Boundary:**
- Marked with `'use client'` directive
- Server components can render client components as children
- ❌ Client components **cannot** import server components directly
- ✅ Can pass server components as props to client components

### Architecture

```tsx
// app/products/page.tsx (Server Component - default)
import { ProductCard } from './ProductCard'; // Client component
import { getProducts } from '@/lib/db';

export default async function ProductsPage() {
  // Direct database access (server-only)
  const products = await getProducts();

  return (
    <div>
      <h1>Products</h1>
      {products.map(product => (
        <ProductCard key={product.id} product={product} />
      ))}
    </div>
  );
}
```

```tsx
// app/products/ProductCard.tsx (Client Component)
'use client'; // Marks boundary

import { useState } from 'react';

export function ProductCard({ product }) {
  const [quantity, setQuantity] = useState(1);

  return (
    <product-card>
      <h3>{product.name}</h3>
      <p>${product.price}</p>

      <input
        type="number"
        value={quantity}
        onChange={(e) => setQuantity(Number(e.target.value))}
      />

      <button onClick={() => addToCart(product.id, quantity)}>
        Add to Cart
      </button>
    </product-card>
  );
}
```

### Streaming with RSC

```tsx
// app/product/[id]/page.tsx
import { Suspense } from 'react';

export default async function ProductPage({ params }) {
  // Fast data: wait for it
  const product = await getProduct(params.id);

  return (
    <>
      <product-details product={product} />

      {/* Slow data: stream later */}
      <Suspense fallback={<ReviewsSkeleton />}>
        <Reviews productId={params.id} />
      </Suspense>

      <Suspense fallback={<RecommendationsSkeleton />}>
        <Recommendations productId={params.id} />
      </Suspense>
    </>
  );
}

// Server Component (streams independently)
async function Reviews({ productId }) {
  const reviews = await getReviews(productId); // Slow query
  return <review-list reviews={reviews} />;
}
```

### RSC Payload Format

When RSCs stream, React sends **RSC Payload** (compact binary):

```json
{
  "chunks": [
    {
      "type": "component",
      "id": "ProductCard:1",
      "props": {"name": "Mango", "price": 3.99},
      "children": []
    }
  ]
}
```

Browser receives RSC payload, not HTML. React runtime reconstructs components.

### Widget Actor Integration

**Challenge:** RSC is React-specific, Widget Actors are framework-agnostic.

**Possible Hybrid:**

```tsx
// Server Component renders Web Component shell
export async function ProductWidget({ id }) {
  const product = await getProduct(id);

  // Render Web Component with data attributes
  return (
    <product-widget
      data-id={product.id}
      data-name={product.name}
      data-price={product.price}
    >
      {/* Fallback HTML */}
      <h3>{product.name}</h3>
      <p>${product.price}</p>
    </product-widget>
  );
}
```

```typescript
// Client: Widget Actor hydrates from data attributes
class ProductWidget extends BaseWidgetActor {
  connectedCallback() {
    super.connectedCallback();

    // Read server-rendered data
    const data = {
      id: this.dataset.id,
      name: this.dataset.name,
      price: Number(this.dataset.price)
    };

    this.hydrate(data);
  }
}
```

### Benefits

1. **Zero client JavaScript** for static Widget Actors
2. **Direct server access** - No API routes needed
3. **Automatic code splitting** - Only client components ship JS
4. **Streaming support** - Suspense boundaries work seamlessly

### Limitations

1. ❌ **React-only** - Cannot use with vanilla Widget Actors
2. ❌ **Framework lock-in** - Deep integration with React
3. ❌ **Complexity** - Server/client boundary mental model
4. ❌ **Tooling required** - Needs Next.js or similar bundler

### Implementation Complexity

- **High (Full RSC):** Requires Next.js App Router or custom setup
- **Medium (Hybrid):** Render Web Components from RSCs

### Recommendation

**⏸️ Consider for React stacks (P2)** - If already using React/Next.js, RSCs provide excellent performance. For framework-agnostic Widget Actors, prefer islands architecture + DSD.

### References

- [Server Components - React Documentation](https://react.dev/reference/rsc/server-components)
- [React Server Components in Practice](https://medium.com/@vyakymenko/react-server-components-in-practice-next-js-d1c3c8a4971f)
- [Making Sense of React Server Components](https://www.joshwcomeau.com/react/server-components/)
- [RSC Payload Explanation](https://edspencer.net/2024/7/12/promises-across-the-void-react-server-components)

---

## 6. Partial Hydration

### Overview

**Partial hydration** is a pragmatic middle ground—server-render the entire page, then hydrate incrementally (component-by-component) rather than all at once. Prioritizes critical components, defers non-critical.

### Key Concepts

**Hydration Spectrum:**

```
Full Hydration → Progressive Hydration → Partial Hydration → Islands → Resumability
(Slowest)                                                                    (Fastest)
```

**Progressive vs Partial:**
- **Progressive:** Hydrate over time (still hydrates everything eventually)
- **Partial:** Some components **never hydrate** (remain static HTML)

### Framework Support (2024-2025)

#### React 18 Selective Hydration

```tsx
import { Suspense, lazy } from 'react';

const HeavyWidget = lazy(() => import('./HeavyWidget'));

export default function App() {
  return (
    <>
      {/* Hydrates immediately */}
      <header-widget />

      {/* Hydrates independently when code loads */}
      <Suspense fallback={<div>Loading...</div>}>
        <HeavyWidget />
      </Suspense>

      {/* Hydrates after HeavyWidget, even if code loads first */}
      <Suspense fallback={<div>Loading comments...</div>}>
        <CommentsWidget />
      </Suspense>
    </>
  );
}
```

React 18 features:
- **Streaming SSR** - Send HTML before all components ready
- **Selective Hydration** - Hydrate components as code loads
- **Priority Hydration** - User interactions prioritize hydration

#### Angular 18 Partial Hydration

```typescript
// app.config.ts
import { provideClientHydration, withIncrementalHydration } from '@angular/platform-browser';

export const appConfig = {
  providers: [
    provideClientHydration(withIncrementalHydration())
  ]
};
```

```typescript
// product-card.component.ts
@Component({
  selector: 'product-card',
  template: `
    <div>
      <h3>{{ product.name }}</h3>
      <p>{{ product.price }}</p>
      <button (click)="addToCart()">Add to Cart</button>
    </div>
  `,
  // Defer hydration until visible
  hydration: { deferUntil: 'visible' }
})
export class ProductCardComponent {
  @Input() product!: Product;

  addToCart() {
    // Interactive behavior
  }
}
```

Angular 18 strategies:
- `deferUntil: 'idle'` - Hydrate when browser idle
- `deferUntil: 'visible'` - Hydrate when in viewport
- `deferUntil: 'interaction'` - Hydrate on first user interaction

#### Marko (Pioneer of Partial Hydration)

```marko
<!-- product-list.marko -->
<div>
  <h1>Products</h1>

  <!-- Static: Never hydrates -->
  <for|product| of=input.products>
    <product-card-static product=product />
  </for>

  <!-- Interactive: Hydrates automatically -->
  <cart-widget/>
</div>
```

Marko automatically detects which components need hydration based on:
- Event listeners
- State management
- Dynamic behavior

Marko was **first framework** to implement automatic partial hydration (pre-2020).

### Widget Actor Integration

#### Pattern: Hydration Priority System

```typescript
// Hydration priority metadata
interface HydrationMetadata {
  priority: 'critical' | 'high' | 'low' | 'never';
  trigger: 'immediate' | 'idle' | 'visible' | 'interaction';
}

class HydrationManager {
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
    // Import Widget Actor class and instantiate
    const widgetType = element.getAttribute('data-widget-type');
    import(`./widgets/${widgetType}.js`).then(module => {
      const WidgetClass = module.default;
      customElements.define(widgetType, WidgetClass);
    });
  }
}
```

```html
<!-- Server-rendered with hydration hints -->
<product-card
  data-widget-type="product-card"
  data-hydration-priority="low"
  data-hydration-trigger="visible"
>
  <template shadowrootmode="open">
    <style>/* Styles */</style>
    <h3>Organic Mango</h3>
    <p>$3.99</p>
    <button>Add to Cart</button>
  </template>
</product-card>

<script type="module">
  const manager = new HydrationManager();

  document.querySelectorAll('[data-widget-type]').forEach(element => {
    manager.scheduleHydration(element, {
      priority: element.dataset.hydrationPriority,
      trigger: element.dataset.hydrationTrigger
    });
  });
</script>
```

### Performance Impact

**Real-world measurements (2024-2025 studies):**

- **TTI improvement:** 500-800ms on mobile (progressive hydration)
- **LCP improvement:** 45% in production apps (partial hydration)
- **JavaScript reduction:** 30-50% (selective hydration)

### Benefits for Widget Actors

1. **Granular control** - Choose what hydrates and when
2. **Better mobile performance** - Critical for low-end devices
3. **Framework flexibility** - React, Angular, Marko all support it
4. **Progressive enhancement** - Baseline HTML always works

### Implementation Complexity

- **Low (Angular 18):** Built-in directives
- **Medium (React 18):** Requires Suspense boundaries
- **Medium (Custom):** Build hydration scheduler

### Recommendation

**✅ Implement for production (P1)** - Partial hydration bridges full hydration and islands. Use Angular 18/React 18 built-ins, or build custom scheduler for Widget Actors.

### References

- [Progressive Hydration Pattern](https://www.patterns.dev/react/progressive-hydration/)
- [Selective Hydration - React](https://www.patterns.dev/react/react-selective-hydration/)
- [Angular 18 Partial Hydration](https://dev.to/this-is-angular/angular-18-improving-application-performance-with-partial-hydration-and-ssr-2nie)
- [Progressive Hydration: The Future of Web Performance](https://devtechinsights.com/progressive-hydration-web-performance-2025/)

---

## 7. HTMX & Hypermedia-Driven Architecture

### Overview

HTMX returns HTML from server for every interaction, not JSON. **Server-driven UI** where HTML is the application state format. Minimal client-side JavaScript (9KB gzipped).

### Key Concepts

**HATEOAS (Hypermedia as Engine of Application State):**
- Server sends HTML fragments
- Client updates DOM directly
- No client-side state management
- No JSON/JavaScript rebuilding UI

**HTMX Attributes:**
- `hx-get`, `hx-post` - AJAX requests
- `hx-trigger` - When to send request
- `hx-target` - Where to put response
- `hx-swap` - How to swap content

### Basic Example

```html
<!-- HTMX-powered Widget Actor -->
<shopping-cart>
  <div id="cart-contents">
    <div class="cart-item">
      <span>Organic Mango</span>
      <span>$3.99</span>
      <button
        hx-delete="/api/cart/items/mango-123"
        hx-target="#cart-contents"
        hx-swap="outerHTML"
      >
        Remove
      </button>
    </div>
  </div>

  <form
    hx-post="/api/cart/items"
    hx-target="#cart-contents"
    hx-swap="outerHTML"
  >
    <input type="text" name="product-id" placeholder="Product ID">
    <button type="submit">Add to Cart</button>
  </form>
</shopping-cart>
```

**Server response (HTML fragment):**

```html
<div id="cart-contents">
  <div class="cart-item">
    <span>Organic Mango</span>
    <span>$3.99</span>
    <button hx-delete="/api/cart/items/mango-123" ...>Remove</button>
  </div>
  <div class="cart-item">
    <span>Fresh Banana</span>
    <span>$1.99</span>
    <button hx-delete="/api/cart/items/banana-456" ...>Remove</button>
  </div>
</div>
```

### Advanced Patterns

#### Out-of-Band Updates

```html
<!-- Update multiple parts of page from single request -->
<button
  hx-post="/api/cart/add/mango-123"
  hx-target="#cart-contents"
>
  Add to Cart
</button>

<!-- Server response includes out-of-band update -->
<div id="cart-contents">
  <!-- Cart contents -->
</div>

<!-- Out-of-band: Update cart count in header -->
<span id="cart-count" hx-swap-oob="true">
  3 items
</span>
```

#### WebSocket Integration

```html
<shopping-cart
  hx-ext="ws"
  ws-connect="/ws/cart"
>
  <div id="cart-contents">
    <!-- Server pushes updates via WebSocket -->
  </div>
</shopping-cart>
```

### Widget Actor Integration

#### Pattern: HTMX + Web Components

```typescript
// Widget Actor with HTMX enhancement
class HTMXWidgetActor extends HTMLElement {
  connectedCallback() {
    this.innerHTML = `
      <div class="widget-content">
        <button
          hx-get="/api/widget/${this.id}/data"
          hx-target=".widget-content"
          hx-swap="innerHTML"
        >
          Load Data
        </button>
      </div>
    `;

    // Initialize HTMX on this element
    if (typeof htmx !== 'undefined') {
      htmx.process(this);
    }
  }
}

customElements.define('htmx-widget', HTMXWidgetActor);
```

#### Server-Side Widget Actor Rendering

```typescript
// Server: Render Widget Actor HTML fragments
app.get('/api/widget/:id/data', async (req, res) => {
  const data = await fetchWidgetData(req.params.id);

  // Return HTML fragment (not JSON)
  res.send(`
    <div class="widget-data">
      <h3>${data.title}</h3>
      <p>${data.description}</p>

      <button
        hx-post="/api/widget/${req.params.id}/update"
        hx-target=".widget-data"
      >
        Update
      </button>
    </div>
  `);
});
```

### Benefits for Widget Actors

1. **Massive JavaScript reduction** - 67% smaller codebases reported
2. **Simpler architecture** - Server owns business logic
3. **Better SEO** - HTML responses indexable
4. **Progressive enhancement** - Forms work without HTMX

### Limitations

1. ❌ **Round-trip latency** - Every interaction hits server
2. ❌ **Offline capability** - Requires network for interactions
3. ❌ **Complex state management** - Server must track everything
4. ⚠️ **Not suitable for real-time apps** - WebSocket helps but limited

### When to Use HTMX

✅ **Good fit:**
- Content-heavy applications
- CRUD operations
- Admin dashboards
- Form-heavy workflows

❌ **Not suitable:**
- Real-time collaboration
- Offline-first apps
- Complex client-side state

### Implementation Complexity

- **Very Low (Basic):** Add HTMX script, use attributes
- **Low (Advanced):** Out-of-band updates, WebSocket
- **Medium (Server):** Design HTML fragment endpoints

### Recommendation

**✅ Explore for content apps (P1)** - HTMX is excellent for server-driven Widget Actors where round-trip latency is acceptable. Not suitable for highly interactive apps.

### References

- [HTMX Documentation](https://htmx.org/docs/)
- [HTMX Examples](https://htmx.org/examples/)
- [Build Server-Driven Web Apps with htmx](https://strapi.io/blog/build-server-driven-web-apps-with-htmx)
- [Creating Server-Driven Web Apps with htmx](https://blog.logrocket.com/htmx-server-driven-web-apps/)

---

## 8. Declarative Shadow DOM (Revisited for SSR)

### Overview

Declarative Shadow DOM (DSD) enables server-side rendering of Web Components with Shadow DOM. Reached **Baseline status August 2024** (Chrome, Safari, Edge). Critical for SSR-friendly Widget Actors.

### Key Features for SSR

**Streaming Support:**
- DSD is **streamable** like regular HTML
- Shadow roots are colocated with parent elements
- Can be sent in HTML chunks (works with streaming SSR)

**Universal Browser Support (2024):**
- Chrome 90+ (124+ for latest spec)
- Safari 16.4+
- Edge 91+
- Firefox: Shipped February 2024 ✅

### Basic SSR Example

```html
<!-- Server-rendered Widget Actor with DSD -->
<product-card id="mango-123">
  <template shadowrootmode="open">
    <style>
      :host {
        display: block;
        padding: 1rem;
        border: 1px solid #ccc;
      }
      h3 { margin: 0; }
    </style>

    <h3>Organic Mango</h3>
    <p>$3.99</p>
    <button class="add-to-cart">Add to Cart</button>
  </template>
</product-card>

<script type="module">
  // Hydrate: Add interactivity to existing Shadow DOM
  class ProductCard extends HTMLElement {
    connectedCallback() {
      // Shadow DOM already exists from SSR
      const button = this.shadowRoot.querySelector('.add-to-cart');
      button.addEventListener('click', () => {
        this.dispatchEvent(new CustomEvent('add-to-cart', {
          detail: { id: this.id }
        }));
      });
    }
  }

  customElements.define('product-card', ProductCard);
</script>
```

### Streaming DSD

```typescript
// Server: Stream Widget Actors with DSD
export async function streamWidgetActors(req: Request): Promise<Response> {
  const encoder = new TextEncoder();

  const stream = new ReadableStream({
    async start(controller) {
      // Send header immediately
      controller.enqueue(encoder.encode(`
        <!DOCTYPE html>
        <html>
        <head><title>Products</title></head>
        <body>
          <h1>Product Catalog</h1>
      `));

      // Stream products as they load (each is DSD-compatible)
      const products = await fetchProducts();

      for (const product of products) {
        controller.enqueue(encoder.encode(`
          <product-card id="${product.id}">
            <template shadowrootmode="open">
              <style>
                :host { display: block; padding: 1rem; }
              </style>
              <h3>${product.name}</h3>
              <p>$${product.price}</p>
              <button>Add to Cart</button>
            </template>
          </product-card>
        `));

        // Small delay to simulate streaming
        await new Promise(resolve => setTimeout(resolve, 10));
      }

      // Close stream
      controller.enqueue(encoder.encode(`
          <script type="module" src="/hydrate-widgets.js"></script>
        </body>
        </html>
      `));

      controller.close();
    }
  });

  return new Response(stream, {
    headers: { 'Content-Type': 'text/html; charset=utf-8' }
  });
}
```

### Hydration Strategy

```typescript
// Hydration script: Upgrade DSD components to Widget Actors
class BaseWidgetActor extends HTMLElement {
  connectedCallback() {
    // Check if Shadow DOM already exists (from DSD)
    if (!this.shadowRoot) {
      // Client-side rendering fallback
      this.attachShadow({ mode: 'open' });
      this.shadowRoot.innerHTML = this.getTemplate();
    }

    // Add interactivity (works for both SSR and CSR)
    this.attachEventListeners();
  }

  attachEventListeners() {
    // Override in subclass
  }

  getTemplate() {
    // Override in subclass (fallback for non-SSR)
    return '<p>Loading...</p>';
  }
}

class ProductCard extends BaseWidgetActor {
  attachEventListeners() {
    const button = this.shadowRoot.querySelector('button');
    button?.addEventListener('click', () => {
      console.log('Add to cart:', this.id);
    });
  }
}

customElements.define('product-card', ProductCard);
```

### Server-Side Rendering Helpers

```typescript
// Utility: Generate DSD HTML for Widget Actors
interface WidgetActorSSROptions {
  tagName: string;
  id: string;
  styles: string;
  content: string;
  attributes?: Record<string, string>;
}

function renderWidgetActorSSR(options: WidgetActorSSROptions): string {
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

// Usage
const productCardHTML = renderWidgetActorSSR({
  tagName: 'product-card',
  id: 'mango-123',
  styles: `
    :host { display: block; padding: 1rem; }
    h3 { margin: 0; }
  `,
  content: `
    <h3>Organic Mango</h3>
    <p>$3.99</p>
    <button>Add to Cart</button>
  `,
  attributes: {
    'data-price': '3.99',
    'data-category': 'fruit'
  }
});
```

### Benefits for Widget Actors

1. **Zero hydration cost for styles** - CSS already applied
2. **No FOUC (Flash of Unstyled Content)** - Styled on first paint
3. **SEO-friendly** - Shadow DOM content in HTML
4. **Streaming compatible** - Works with chunked HTML delivery
5. **Progressive enhancement** - HTML works before JS loads

### Browser Support Caveats

**Spec Changes (2023-2024):**
- Attribute renamed: `shadowroot` → `shadowrootmode`
- Chrome 124+ has standardized version
- Older implementations may need polyfills

**Polyfill:**

```typescript
// Polyfill for browsers without DSD support
(function() {
  if (HTMLTemplateElement.prototype.hasOwnProperty('shadowRootMode')) {
    return; // Native support
  }

  // Polyfill: Convert <template shadowrootmode> to attachShadow()
  document.querySelectorAll('template[shadowrootmode]').forEach(template => {
    const mode = template.getAttribute('shadowrootmode');
    const parent = template.parentNode;

    if (parent && !parent.shadowRoot) {
      const shadowRoot = parent.attachShadow({ mode: mode as 'open' | 'closed' });
      shadowRoot.appendChild(template.content.cloneNode(true));
      template.remove();
    }
  });
})();
```

### Implementation Complexity

- **Low (Basic DSD):** Add `<template shadowrootmode="open">`
- **Low (Streaming):** Use ReadableStream with DSD templates
- **Medium (Hydration):** Build upgrade system for existing Shadow DOM

### Recommendation

**✅ Implement immediately (P0)** - DSD is now Baseline (all major browsers). Essential for SSR-friendly Widget Actors with style encapsulation.

### References

- [Declarative Shadow DOM - web.dev](https://web.dev/articles/declarative-shadow-dom)
- [Web Components and SSR - 2024 Edition](https://dev.to/stuffbreaker/web-components-and-ssr-2024-edition-1nel)
- [Declarative Shadow DOM: Magic Pill for SSR](https://hackernoon.com/declarative-shadow-dom-the-magic-pill-for-server-side-rendering-and-web-components)
- [12 Days of Web: Declarative Shadow DOM](https://12daysofweb.dev/2024/declarative-shadow-dom/)

---

## Implementation Priorities & Beads

### Priority Matrix

| Pattern | Priority | Complexity | Browser Support | Framework Lock-in | Performance Impact |
|---------|---------|-----------|----------------|------------------|-------------------|
| Declarative Shadow DOM | **P0** | Low | ✅ Baseline 2024 | None | High (FOUC eliminated) |
| Progressive Enhancement | **P0** | Low | ✅ Universal | None | High (resilience) |
| Streaming HTML | **P0** | Low-Medium | ✅ Universal | Optional | High (40% FCP improvement) |
| Islands Architecture | **P0** | Low-Medium | ✅ Universal | Medium (Astro/Enhance) | Very High (60-80% JS reduction) |
| Partial Hydration | **P1** | Medium | ✅ Universal | Medium (React/Angular) | High (45% LCP improvement) |
| HTMX Hypermedia | **P1** | Low | ✅ Universal | None | High (67% codebase reduction) |
| Qwik Resumability | **P2** | High | ✅ Universal | High (Qwik) | Very High (O(1) startup) |
| React Server Components | **P2** | High | ✅ Universal | High (React) | High (zero client JS) |

### Decision Tree: Which Pattern to Use?

```
START: What's your use case?

├─ Need framework-agnostic solution?
│  ├─ YES: Choose DSD + Progressive Enhancement + Streaming HTML (P0)
│  └─ NO: Continue
│
├─ Already using React/Next.js?
│  ├─ YES: Use React Server Components + Streaming Suspense (P2)
│  └─ NO: Continue
│
├─ Content-heavy with mostly static content?
│  ├─ YES: Islands Architecture (Astro/Enhance) (P0)
│  └─ NO: Continue
│
├─ Need offline capability and complex client state?
│  ├─ YES: Partial Hydration + Service Workers (P1)
│  └─ NO: Continue
│
├─ Server-driven CRUD application?
│  ├─ YES: HTMX + DSD (P1)
│  └─ NO: Continue
│
├─ Large-scale app (1000+ components)?
│  ├─ YES: Consider Qwik Resumability (P2)
│  └─ NO: Use Streaming HTML + Partial Hydration (P1)
```

### Integration Roadmap for Widget Actors

#### Phase 1: Foundation (P0) - Weeks 1-2

**Goal:** Server-render Widget Actors with streaming and style encapsulation

**Tasks:**
1. Implement DSD templates for all Widget Actors
2. Build SSR rendering utilities (`renderWidgetActorSSR()`)
3. Add progressive enhancement baseline (HTML-first forms)
4. Implement streaming HTML endpoint

**Success Criteria:**
- Widget Actors render server-side with Shadow DOM
- No FOUC on initial load
- Forms work without JavaScript
- HTML streams progressively

#### Phase 2: Selective Loading (P0-P1) - Weeks 3-4

**Goal:** Reduce JavaScript bundle via islands or partial hydration

**Option A: Islands Architecture (Astro/Enhance)**
- Setup Astro project with Widget Actor integration
- Mark interactive vs static Widget Actors
- Implement `client:*` directives for hydration strategies

**Option B: Custom Partial Hydration**
- Build hydration scheduler (priority-based)
- Add `data-hydration-*` attributes to components
- Implement IntersectionObserver-based hydration

**Success Criteria:**
- 60-80% reduction in JavaScript bundle size
- TTI improvement of 500-800ms
- Only interactive Widget Actors hydrate

#### Phase 3: Advanced Patterns (P1-P2) - Weeks 5-6

**Goal:** Optimize for specific use cases

**For Content Apps:**
- Integrate HTMX for server-driven interactions
- Build HTML fragment endpoints
- Implement out-of-band updates

**For Complex Apps:**
- Study Qwik resumability patterns
- Implement serialization for Widget Actor state
- Build lazy-loading infrastructure

**Success Criteria:**
- Pattern-specific performance targets met
- Developer experience improved
- Clear documentation for team

---

## Conclusion

Modern "into the page" patterns have matured significantly in 2024-2026, providing robust alternatives to traditional client-side rendering:

### Key Takeaways

1. **Declarative Shadow DOM is game-changing** - Universal browser support (Aug 2024) makes SSR-friendly Web Components viable
2. **Islands architecture is production-ready** - 60-80% JavaScript reduction with frameworks like Astro/Enhance
3. **Streaming HTML is essential** - 40% FCP improvement with minimal complexity
4. **Progressive enhancement is non-negotiable** - HTML-first approach ensures resilience and accessibility
5. **Framework choice matters less** - Native patterns (DSD, streaming) work everywhere

### Recommended Stack for Widget Actors

**Minimal (Framework-Agnostic):**
- Declarative Shadow DOM for SSR
- Native streaming HTML (ReadableStream)
- Progressive enhancement baseline
- Manual hydration priority system

**Optimal (With Framework):**
- Astro for islands architecture
- Declarative Shadow DOM for Web Components
- Progressive enhancement by default
- Built-in hydration strategies

**Advanced (For Scale):**
- Qwik for resumability (1000+ components)
- Or: Islands + partial hydration + streaming
- Or: React Server Components (if React stack)

### Next Steps

1. ✅ **Start with DSD + Streaming** (P0) - Immediate wins with low complexity
2. ✅ **Add Islands Architecture** (P0) - Major JavaScript reduction
3. ⏸️ **Evaluate Partial Hydration** (P1) - If islands not sufficient
4. ⏸️ **Consider Advanced Patterns** (P2) - Qwik/RSC for specific needs

The future of Widget Actors is **HTML-first, progressively enhanced, selectively interactive**. The tools are ready; the browser platform is mature; the performance gains are proven.

---

## Sources

All research based on 2024-2026 web searches and official documentation:

**Streaming & SSR:**
- [Streaming Server-Side Rendering](https://www.patterns.dev/react/streaming-ssr/)
- [Next.js 15 Streaming Handbook](https://www.freecodecamp.org/news/the-nextjs-15-streaming-handbook/)
- [Revisiting HTML streaming for modern web performance](https://calendar.perfplanet.com/2025/revisiting-html-streaming-for-modern-web-performance/)
- [Next.js Streaming Explained](https://u11d.com/blog/nextjs-streaming-vs-csr-vs-ssr/)

**Islands Architecture:**
- [Islands Architecture - Astro Docs](https://docs.astro.build/en/concepts/islands/)
- [Astro Islands Architecture Explained](https://strapi.io/blog/astro-islands-architecture-explained-complete-guide)
- [Islands Architecture Pattern](https://www.patterns.dev/vanilla/islands-architecture/)
- [Island Architecture with Web Components - Enhance.dev](https://enhance.dev/blog/posts/2024-07-09-island-architecture-with-web-components)

**Resumability:**
- [Resumable - Qwik Documentation](https://qwik.dev/docs/concepts/resumable/)
- [Resumable JavaScript with Qwik](https://dev.to/this-is-learning/resumable-javascript-with-qwik-2i29)
- [Think Qwik](https://qwik.dev/docs/concepts/think-qwik/)

**Progressive Enhancement:**
- [HTML Web Components Make Progressive Enhancement Easier](https://css-tricks.com/html-web-components-make-progressive-enhancement-and-css-encapsulation-easier/)
- [Progressively Enhancing a Web Component](https://gomakethings.com/progressively-enhancing-a-web-component/)
- [Web Components as Progressive Enhancement](https://cloudfour.com/thinks/web-components-as-progressive-enhancement/)
- [Progressive Enhancement - Piccalilli](https://piccalil.li/blog/its-about-time-i-tried-to-explain-what-progressive-enhancement-actually-is/)

**React Server Components:**
- [Server Components - React](https://react.dev/reference/rsc/server-components/)
- [React Server Components in Practice](https://medium.com/@vyakymenko/react-server-components-in-practice-next-js-d1c3c8a4971f)
- [Making Sense of React Server Components](https://www.joshwcomeau.com/react/server-components/)
- [Promises across the void: RSC](https://edspencer.net/2024/7/12/promises-across-the-void-react-server-components)

**Partial Hydration:**
- [Progressive Hydration Pattern](https://www.patterns.dev/react/progressive-hydration/)
- [Selective Hydration - React](https://www.patterns.dev/react/react-selective-hydration/)
- [Angular 18 Partial Hydration](https://dev.to/this-is-angular/angular-18-improving-application-performance-with-partial-hydration-and-ssr-2nie)
- [Progressive Hydration: The Future](https://devtechinsights.com/progressive-hydration-web-performance-2025/)

**HTMX:**
- [HTMX Documentation](https://htmx.org/docs/)
- [HTMX Examples](https://htmx.org/examples/)
- [Build Server-Driven Web Apps with htmx](https://strapi.io/blog/build-server-driven-web-apps-with-htmx)
- [Creating Server-Driven Web Apps](https://blog.logrocket.com/htmx-server-driven-web-apps/)

**Declarative Shadow DOM:**
- [Declarative Shadow DOM - web.dev](https://web.dev/articles/declarative-shadow-dom)
- [Web Components and SSR - 2024 Edition](https://dev.to/stuffbreaker/web-components-and-ssr-2024-edition-1nel)
- [DSD: Magic Pill for SSR](https://hackernoon.com/declarative-shadow-dom-the-magic-pill-for-server-side-rendering-and-web-components)
- [12 Days of Web: Declarative Shadow DOM](https://12daysofweb.dev/2024/declarative-shadow-dom/)
