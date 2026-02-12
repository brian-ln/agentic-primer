# P0: Islands Architecture with Astro for Widget Actors

## Problem

All Widget Actors currently ship JavaScript to client, even static/non-interactive ones. This causes:
- Large JavaScript bundles (100-300KB+)
- Slow Time to Interactive (TTI)
- Wasted bandwidth and battery
- Unnecessary hydration cost

## Solution

Adopt islands architecture using Astro to selectively hydrate only interactive Widget Actors. Static Widget Actors remain pure HTML.

## Benefits

- ✅ 60-80% JavaScript bundle reduction
- ✅ 500-800ms TTI improvement
- ✅ 45% better LCP in production apps
- ✅ Framework flexibility (use React, Svelte, Vue islands together)
- ✅ Automatic code splitting per island

## Technical Approach

### Astro Project Setup

```bash
npm create astro@latest widget-actor-app
cd widget-actor-app
npm install
```

### Static Widget Actor (Pure HTML)

```astro
---
// src/components/StaticProductCard.astro
export interface Props {
  product: {
    id: string;
    name: string;
    price: number;
  };
}

const { product } = Astro.props;
---

<product-card id={product.id} data-static>
  <h3>{product.name}</h3>
  <p>${product.price}</p>
  <a href={`/products/${product.id}`}>View Details</a>
</product-card>

<style>
  product-card {
    display: block;
    padding: 1rem;
    border: 1px solid #ccc;
  }
</style>
```

### Interactive Widget Actor (Island)

```tsx
// src/islands/InteractiveCartWidget.tsx
import { BaseWidgetActor } from '../messaging/browser/widget-actor';
import type { Message, MessageResponse } from '../messaging/message';

export class InteractiveCartWidget extends BaseWidgetActor {
  async receive(msg: Message): Promise<MessageResponse> {
    if (msg.type === 'add-to-cart') {
      await this.addToCart(msg.payload);
      return { success: true };
    }
    return { success: false };
  }

  private async addToCart(item: any) {
    // Interactive behavior
    const response = await fetch('/api/cart/add', {
      method: 'POST',
      body: JSON.stringify(item)
    });
    this.updateUI(await response.json());
  }
}

customElements.define('cart-widget', InteractiveCartWidget);
```

### Page with Islands

```astro
---
// src/pages/products.astro
import StaticProductCard from '../components/StaticProductCard.astro';
import InteractiveCartWidget from '../islands/InteractiveCartWidget.tsx';

const products = await fetchProducts();
---

<html>
<head>
  <title>Products</title>
</head>
<body>
  <h1>Product Catalog</h1>

  <!-- Static: No JavaScript sent -->
  {products.map(p => (
    <StaticProductCard product={p} />
  ))}

  <!-- Island: Hydrates on client -->
  <InteractiveCartWidget client:load />

  <!-- Island: Hydrates when visible -->
  <InteractiveCartWidget client:visible />

  <!-- Island: Hydrates on user interaction -->
  <InteractiveCartWidget client:idle />
</body>
</html>
```

## Astro Hydration Strategies

- `client:load` - Hydrate immediately on page load
- `client:idle` - Hydrate when main thread idle (requestIdleCallback)
- `client:visible` - Hydrate when element enters viewport (IntersectionObserver)
- `client:media` - Hydrate when media query matches (e.g., mobile only)
- `client:only` - Skip SSR, client-only rendering

## Implementation Steps

1. Initialize Astro project
2. Migrate static Widget Actors to `.astro` components
3. Move interactive Widget Actors to `/islands` directory
4. Configure hydration strategies for each island
5. Measure JavaScript bundle size reduction
6. Add tests for island hydration
7. Document island patterns in README

## Acceptance Criteria

- ✅ Static Widget Actors ship zero JavaScript
- ✅ Interactive Widget Actors hydrate selectively
- ✅ 60-80% JavaScript bundle size reduction
- ✅ TTI improvement of 500-800ms
- ✅ Different hydration strategies work correctly
- ✅ Tests pass for both static and interactive Widget Actors

**Priority:** P0
**Complexity:** Low-Medium
**Performance Impact:** Very High (60-80% JS reduction)
**Framework Lock-in:** Medium (Astro-specific, but can use any framework for islands)
