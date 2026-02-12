# Design Question: Model Situations

## Current Implementation

`situations` is a feature in Model that allows named parameter presets:

```typescript
// Model definition
situations: {
  creative: { temperature: 0.9, maxTokens: 500 },
  precise: { temperature: 0.1, maxTokens: 50 },
}

// At invocation time
invokeModel('my-model', { message: '...', situation: 'precise' })
```

## Question

Is this a Model concern or a runtime/invocation concern?

## Options

1. **Keep in Model** - Convenient, self-contained model configuration
2. **Move to Agent config** - Agent decides which presets to use for its tasks
3. **Separate "Preset" entity** - Reusable across models
4. **Remove entirely** - Just pass parameters directly at invocation time

## Considerations

- Mixing model metadata with usage patterns
- Who owns the "how to use this model" decision?
- Does this add value or just complexity?

## Status

Parking this for later discussion.
