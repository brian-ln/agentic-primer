# P0: Implement Streaming HTML for Widget Actors

## Problem

Traditional SSR waits for all content before sending response, causing:
- High Time to First Byte (TTFB)
- Slow First Contentful Paint (FCP)
- Poor perceived performance (blank screen while waiting)

## Solution

Implement streaming HTML with out-of-order delivery for Widget Actors using native `ReadableStream` API.

## Benefits

- ✅ 40-60% FCP improvement (shell sent immediately)
- ✅ Non-blocking (slow Widget Actors don't block fast ones)
- ✅ Better UX (progressive loading feels faster)
- ✅ SEO-friendly (complete HTML delivered)
- ✅ Works with all modern browsers

## Technical Approach

### Streaming Endpoint

```typescript
export async function streamWidgetActorsPage(req: Request): Promise<Response> {
  const encoder = new TextEncoder();

  const stream = new ReadableStream({
    async start(controller) {
      // 1. Send shell immediately
      controller.enqueue(encoder.encode(`
        <!DOCTYPE html>
        <html>
        <head><title>Products</title></head>
        <body><h1>Product Catalog</h1>
      `));

      // 2. Stream Widget Actors as they complete
      const productsPromise = fetchProducts();
      const reviewsPromise = fetchReviews();

      // Send products when ready
      productsPromise.then(products => {
        products.forEach(p => {
          controller.enqueue(encoder.encode(
            renderWidgetActorSSR({
              tagName: 'product-card',
              id: p.id,
              styles: '/* styles */',
              content: `<h3>${p.name}</h3><p>$${p.price}</p>`
            })
          ));
        });
      });

      // Send reviews when ready (may arrive before products)
      reviewsPromise.then(reviews => {
        controller.enqueue(encoder.encode(`
          <review-list>
            ${reviews.map(r => `<review-card>${r.text}</review-card>`).join('')}
          </review-list>
        `));
      });

      // 3. Wait for all content, then close
      await Promise.all([productsPromise, reviewsPromise]);

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

## Implementation Steps

1. Create streaming utilities in `src/messaging/browser/streaming-ssr.ts`
2. Implement `streamWidgetActorsPage()` endpoint
3. Add Suspense-like placeholders for slow Widget Actors
4. Integrate with Declarative Shadow DOM rendering
5. Add tests for streaming behavior
6. Measure FCP/TTFB improvements

## Acceptance Criteria

- ✅ HTML shell sent immediately (TTFB < 100ms)
- ✅ Widget Actors stream progressively as data arrives
- ✅ Fast Widget Actors don't wait for slow ones
- ✅ Complete HTML delivered to browser
- ✅ 40%+ FCP improvement measured

**Priority:** P0
**Complexity:** Low-Medium
**Performance Impact:** High (40-60% FCP improvement)
