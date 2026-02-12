# P0: Progressive Enhancement Baseline for Widget Actors

## Problem

Widget Actors currently require JavaScript to function. This causes:
- Broken experience when JS fails/disabled
- Poor accessibility (screen readers need semantic HTML)
- Zero SEO value (content not in HTML)
- Network resilience issues

## Solution

Build HTML-first Widget Actors that work without JavaScript, then enhance with interactivity.

## Benefits

- ✅ Resilient (works even when JS fails)
- ✅ Accessible (semantic HTML for screen readers)
- ✅ SEO-friendly (content indexable)
- ✅ Faster baseline experience
- ✅ Battery-friendly (less JS execution)

## Technical Approach

### HTML-First Widget Actor Pattern

```typescript
class ShoppingCartActor extends BaseWidgetActor {
  protected renderShadowDOM(): void {
    this.shadowRoot!.innerHTML = `
      <style>/* styles */</style>

      <!-- Baseline: Standard HTML form (works without JS) -->
      <form action="/api/cart/add" method="POST">
        <ul class="cart-items">
          ${this.renderCartItems()}
        </ul>

        <input type="text" name="product-id" placeholder="Product ID" required>
        <button type="submit">Add to Cart</button>
      </form>
    `;
  }

  protected attachEventListeners(): void {
    const form = this.shadowRoot!.querySelector('form');

    // Progressive enhancement: Intercept for AJAX
    form?.addEventListener('submit', async (e) => {
      e.preventDefault(); // Only if JS available

      const formData = new FormData(form as HTMLFormElement);
      const response = await fetch(form.action, {
        method: 'POST',
        body: formData
      });

      // Update cart with JSON response
      const data = await response.json();
      this.updateCartUI(data);
    });
  }
}
```

### Server Endpoint (handles both POST and AJAX)

```typescript
app.post('/api/cart/add', async (req, res) => {
  const { productId } = req.body;
  await addToCart(req.session, productId);

  // Detect AJAX request
  if (req.headers['content-type']?.includes('application/json')) {
    // AJAX: Return JSON
    res.json({ success: true, cart: req.session.cart });
  } else {
    // Standard form: Redirect back
    res.redirect('/cart');
  }
});
```

## Implementation Steps

1. Audit all Widget Actors for JavaScript dependencies
2. Refactor interactive Widget Actors to use HTML forms as baseline
3. Add progressive enhancement layer (AJAX interception)
4. Update server endpoints to handle both form POST and AJAX
5. Add tests for both JavaScript-enabled and JavaScript-disabled paths
6. Document progressive enhancement pattern

## Acceptance Criteria

- ✅ All interactive Widget Actors work with HTML forms (no JS)
- ✅ JavaScript enhances with AJAX (no page reload)
- ✅ Server endpoints handle both POST and AJAX
- ✅ Tests pass for both JS-enabled and JS-disabled scenarios
- ✅ Screen readers can use all functionality

**Priority:** P0
**Complexity:** Low
**Performance Impact:** High (resilience + accessibility)
