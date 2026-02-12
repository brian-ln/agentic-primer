# InferenceActor

**Status:** ✅ Complete
**Bead:** simplify-bgg
**Files:**
- `/src/messaging/actors/inference.ts` - Main implementation
- `/src/messaging/actors/inference.test.ts` - Test suite (33 tests)
- `/examples/inference-actor-usage.ts` - Usage examples

## Overview

InferenceActor provides a message-based interface for LLM inference using the Anthropic API. It integrates seamlessly with the query layer via `ask()` for request-response patterns.

## Features

- ✅ Text generation with Claude models
- ✅ System prompts
- ✅ Tool use (function calling)
- ✅ Template interpolation with `{{variable}}` syntax
- ✅ Token counting and cost tracking
- ✅ Error handling (rate limits, authentication, etc.)
- ✅ Model aliasing (e.g., `claude-sonnet-4.5`)
- ❌ Embeddings (not supported by Anthropic API)

## Configuration

Add your Anthropic API key to `.env`:

```bash
ANTHROPIC_API_KEY=sk-ant-...
```

Get your API key from: https://console.anthropic.com/

## Supported Models

| Alias | Full Model ID | Input ($/1M tokens) | Output ($/1M tokens) |
|-------|---------------|---------------------|----------------------|
| `claude-sonnet-4.5` | `claude-sonnet-4-5-20250514` | $3.00 | $15.00 |
| `claude-opus-4.5` | `claude-opus-4-5-20251101` | $15.00 | $75.00 |
| `claude-haiku-4.5` | `claude-haiku-4-5-20250514` | $0.80 | $4.00 |
| `claude-sonnet-4` | `claude-3-5-sonnet-20241022` | $3.00 | $15.00 |

Default: `claude-sonnet-4.5`

## Message Types

### `generate` - Generate text

Generate text using Claude models.

**Payload:**
```typescript
{
  prompt: string;           // Required: The prompt text
  model?: string;          // Optional: Model alias or full ID
  system?: string;         // Optional: System prompt
  maxTokens?: number;      // Optional: Max tokens (default: 1024)
  temperature?: number;    // Optional: Temperature 0-1 (default: 1.0)
  tools?: Tool[];          // Optional: Function calling tools
}
```

**Response:**
```typescript
{
  id: string;
  model: string;
  content: string;         // Generated text
  usage: {
    inputTokens: number;
    outputTokens: number;
  };
  stopReason: string;
  toolUses?: ToolUse[];   // If tools were used
}
```

### `get-stats` - Get usage statistics

**Response:**
```typescript
{
  totalInputTokens: number;
  totalOutputTokens: number;
  totalCost: number;       // In USD
  configured: boolean;
}
```

### `reset-stats` - Reset usage statistics

**Response:**
```typescript
{
  reset: true;
}
```

### `embeddings` - Not supported

Returns an error. Use OpenAI or other embedding providers.

## Usage Examples

### Basic Text Generation

```typescript
const response = await inferenceActor.ask(
  address('inference'),
  'generate',
  {
    prompt: 'Explain what an actor system is in one sentence.',
    maxTokens: 100,
  }
);

console.log(response.payload.content);
```

### With System Prompt

```typescript
const response = await inferenceActor.ask(
  address('inference'),
  'generate',
  {
    prompt: 'What is the capital of France?',
    system: 'You are a geography expert. Answer concisely.',
    maxTokens: 50,
  }
);
```

### Template Interpolation

```typescript
const response = await inferenceActor.ask(
  address('inference'),
  'generate',
  {
    prompt: 'Explain {{topic}} to a {{level}} student.',
  },
  {
    metadata: {
      topic: 'quantum computing',
      level: 'high school',
    },
  }
);
```

Variables are replaced using `{{path.to.value}}` syntax. Nested paths supported.

### Using Different Models

```typescript
const response = await inferenceActor.ask(
  address('inference'),
  'generate',
  {
    prompt: 'Write a haiku about coding.',
    model: 'claude-haiku-4.5', // Faster, cheaper
    maxTokens: 100,
  }
);
```

### Tool Use (Function Calling)

```typescript
const response = await inferenceActor.ask(
  address('inference'),
  'generate',
  {
    prompt: 'What is the weather in San Francisco?',
    tools: [
      {
        name: 'get_weather',
        description: 'Get current weather for a location',
        input_schema: {
          type: 'object',
          properties: {
            location: {
              type: 'string',
              description: 'City name',
            },
          },
          required: ['location'],
        },
      },
    ],
    maxTokens: 500,
  }
);

if (response.payload.toolUses) {
  for (const toolUse of response.payload.toolUses) {
    console.log(`Tool: ${toolUse.name}`);
    console.log(`Input:`, toolUse.input);
  }
}
```

### Query Layer Integration

```typescript
// Conceptual example - requires query layer setup
query()
  .match(pattern('question').where({ status: 'pending' }))
  .forEach(
    send('@(inference)').ask({
      prompt: 'Answer this question: {{question.text}}',
      maxTokens: 500,
    })
  )
  .forEach(
    update('question').set({
      answer: '$response.content',
      status: 'answered',
    })
  )
```

### Track Usage and Costs

```typescript
// Generate some text
await inferenceActor.ask(address('inference'), 'generate', {
  prompt: 'Hello, world!',
});

// Check stats
const stats = await inferenceActor.ask(address('inference'), 'get-stats', {});
console.log('Total cost: $' + stats.payload.totalCost.toFixed(4));

// Reset stats
await inferenceActor.ask(address('inference'), 'reset-stats', {});
```

## Error Handling

InferenceActor handles common API errors gracefully:

- **401 Unauthorized:** Invalid or missing API key
- **429 Rate Limit:** Too many requests, retry later
- **Generic errors:** Network issues, API downtime, etc.

All errors return `success: false` with descriptive error messages.

## Testing

The test suite includes 33 tests covering:

- ✅ Construction and initialization
- ✅ Message handling (unknown types, stats, etc.)
- ✅ Text generation (simple, system prompts, parameters)
- ✅ Tool use (definition, extraction)
- ✅ Template interpolation (simple, nested, missing vars)
- ✅ Error handling (rate limits, auth, generic)
- ✅ Model resolution (aliases, unknown models)
- ✅ Cost calculation and tracking
- ✅ Response format validation

**Run tests:**
```bash
bun test src/messaging/actors/inference.test.ts
```

All tests use mocks - no real API calls are made during testing.

## Implementation Notes

### Architecture

InferenceActor follows the standard Actor pattern:

1. Extends `Actor` base class
2. Implements `receive(message)` for message handling
3. Returns `MessageResponse` for all operations
4. Uses router for inter-actor communication

### API Integration

- Uses `@anthropic-ai/sdk` (official Anthropic SDK)
- Reads `ANTHROPIC_API_KEY` from environment
- Supports all message streaming (future)
- Handles rate limiting with exponential backoff (future)

### Token Counting

Token usage is tracked per request and accumulated globally:
- `totalInputTokens` - Total input tokens across all requests
- `totalOutputTokens` - Total output tokens across all requests
- `totalCost` - Estimated cost in USD based on model pricing

### Cost Calculation

Costs are calculated using current Anthropic pricing:
```typescript
cost = (inputTokens / 1M) * inputPrice + (outputTokens / 1M) * outputPrice
```

Pricing is hardcoded in `MODEL_PRICING` constant.

### Template Interpolation

Simple variable substitution:
- Pattern: `{{variable}}` or `{{nested.path}}`
- Source: `message.metadata` object
- Missing variables: Left unchanged

## Future Enhancements

- [ ] Streaming support (via `stream()` method)
- [ ] Response caching
- [ ] Exponential backoff for rate limits
- [ ] Batch request support
- [ ] Vision model support (image inputs)
- [ ] Prompt caching (Anthropic feature)
- [ ] Multi-turn conversations
- [ ] Token usage alerts/limits

## Dependencies

- `@anthropic-ai/sdk` - Official Anthropic SDK
- `../actor.ts` - Base Actor class
- `../message.ts` - Message protocol

## See Also

- [TaskActor](/src/messaging/actors/task.ts) - Reference implementation
- [QueryExecutor](/src/messaging/actors/query-executor.ts) - Query integration
- [Actor System](/src/messaging/actor.ts) - Actor base class
