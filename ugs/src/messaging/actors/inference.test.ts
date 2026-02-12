#!/usr/bin/env bun
/**
 * InferenceActor Tests
 * Tests for src/messaging/actors/inference.ts
 * Target: 30+ tests with mocked API calls
 */

import { test, expect, describe, beforeEach, mock } from 'bun:test';
import { InferenceActor } from './inference.ts';
import { MessageRouter } from '../router.ts';
import { address, generateMessageId, type Message } from '@agentic-primer/actors';
import type GraphStore from '../../graph.ts';
import type { ProgramManager } from '../../entities/program.ts';
import Anthropic from '@anthropic-ai/sdk';

// Mock implementations
function createMockGraphStore(): GraphStore {
  const nodes = new Map();
  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
  } as any as GraphStore;
}

function createMockProgramManager(): ProgramManager {
  return {} as any as ProgramManager;
}

// Helper to create test message
function testMessage(type: string, payload: any, metadata?: Record<string, any>): Message {
  return {
    id: generateMessageId(),
    pattern: 'ask',
    type,
    payload,
    from: address('test'),
    to: address('inference'),
    timestamp: Date.now(),
    metadata,
  };
}

// Mock Anthropic client
const createMockAnthropicClient = () => {
  return {
    messages: {
      create: mock(async (params: Anthropic.MessageCreateParams) => {
        return {
          id: 'msg_test123',
          type: 'message',
          role: 'assistant',
          model: params.model,
          content: [
            {
              type: 'text',
              text: 'This is a test response',
            },
          ],
          stop_reason: 'end_turn',
          usage: {
            input_tokens: 10,
            output_tokens: 20,
          },
        };
      }),
    },
  };
};

describe('InferenceActor - Construction', () => {
  test('creates inference actor with id and router', () => {
    const router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    const actor = new InferenceActor('inference', router, 'sk-test-key');
    expect(actor).toBeDefined();
    expect(actor.address).toBe('@(inference)');
  });

  test('creates actor without API key', () => {
    const router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    const actor = new InferenceActor('inference', router);
    expect(actor).toBeDefined();
  });

  test('reads API key from environment', () => {
    const originalKey = process.env.ANTHROPIC_API_KEY;
    process.env.ANTHROPIC_API_KEY = 'sk-env-test-key';

    const router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    const actor = new InferenceActor('inference', router);
    expect(actor).toBeDefined();

    if (originalKey) {
      process.env.ANTHROPIC_API_KEY = originalKey;
    } else {
      delete process.env.ANTHROPIC_API_KEY;
    }
  });
});

describe('InferenceActor - Message Handling', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
  });

  test('handles unknown message type', async () => {
    const msg = testMessage('unknown-type', {});
    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Unknown message type');
  });

  test('handles get-stats message', async () => {
    const msg = testMessage('get-stats', {});
    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload).toBeDefined();
    expect(response.payload.totalInputTokens).toBe(0);
    expect(response.payload.totalOutputTokens).toBe(0);
    expect(response.payload.totalCost).toBe(0);
    expect(response.payload.configured).toBe(true);
  });

  test('handles reset-stats message', async () => {
    const msg = testMessage('reset-stats', {});
    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.reset).toBe(true);
  });
});

describe('InferenceActor - Generate Text', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');

    // Mock the Anthropic client
    (actor as any).client = createMockAnthropicClient();
  });

  test('generates text with simple prompt', async () => {
    const msg = testMessage('generate', {
      prompt: 'Hello, world!',
    });

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.content).toBe('This is a test response');
    expect(response.payload.id).toBe('msg_test123');
  });

  test('returns error if prompt is missing', async () => {
    const msg = testMessage('generate', {});
    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Missing required field: prompt');
  });

  test('returns error if API key not configured', async () => {
    const actorNoKey = new InferenceActor('inference', router);
    const msg = testMessage('generate', { prompt: 'Test' });
    const response = await actorNoKey.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('API key not configured');
  });

  test('uses default model when not specified', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test prompt',
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.model).toContain('sonnet');
  });

  test('uses specified model', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test prompt',
      model: 'claude-haiku-4.5',
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.model).toContain('haiku');
  });

  test('includes system prompt when provided', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test prompt',
      system: 'You are a helpful assistant',
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.system).toBe('You are a helpful assistant');
  });

  test('uses custom maxTokens', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test prompt',
      maxTokens: 500,
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.max_tokens).toBe(500);
  });

  test('uses custom temperature', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test prompt',
      temperature: 0.5,
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.temperature).toBe(0.5);
  });

  test('tracks token usage', async () => {
    const msg = testMessage('generate', {
      prompt: 'Test prompt',
    });

    await actor.receive(msg);

    const statsMsg = testMessage('get-stats', {});
    const statsResponse = await actor.receive(statsMsg);

    expect(statsResponse.payload.totalInputTokens).toBe(10);
    expect(statsResponse.payload.totalOutputTokens).toBe(20);
    expect(statsResponse.payload.totalCost).toBeGreaterThan(0);
  });

  test('resets statistics', async () => {
    // Generate some usage
    await actor.receive(testMessage('generate', { prompt: 'Test' }));

    // Reset
    await actor.receive(testMessage('reset-stats', {}));

    // Check stats are zero
    const statsMsg = testMessage('get-stats', {});
    const statsResponse = await actor.receive(statsMsg);

    expect(statsResponse.payload.totalInputTokens).toBe(0);
    expect(statsResponse.payload.totalOutputTokens).toBe(0);
    expect(statsResponse.payload.totalCost).toBe(0);
  });
});

describe('InferenceActor - Tool Use', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
  });

  test('includes tools in request when provided', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const tools = [
      {
        name: 'get_weather',
        description: 'Get weather information',
        input_schema: {
          type: 'object' as const,
          properties: {
            location: { type: 'string' },
          },
          required: ['location'],
        },
      },
    ];

    const msg = testMessage('generate', {
      prompt: 'What is the weather?',
      tools,
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.tools).toBeDefined();
    expect(callArgs.tools).toHaveLength(1);
  });

  test('extracts tool uses from response', async () => {
    const mockClient = {
      messages: {
        create: mock(async () => ({
          id: 'msg_test123',
          type: 'message',
          role: 'assistant',
          model: 'claude-sonnet-4-5-20250514',
          content: [
            {
              type: 'tool_use',
              id: 'tool_123',
              name: 'get_weather',
              input: { location: 'San Francisco' },
            },
          ],
          stop_reason: 'tool_use',
          usage: { input_tokens: 10, output_tokens: 20 },
        })),
      },
    };

    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'What is the weather?',
      tools: [
        {
          name: 'get_weather',
          description: 'Get weather',
          input_schema: {
            type: 'object' as const,
            properties: { location: { type: 'string' } },
          },
        },
      ],
    });

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.toolUses).toBeDefined();
    expect(response.payload.toolUses).toHaveLength(1);
    expect(response.payload.toolUses[0].name).toBe('get_weather');
    expect(response.payload.toolUses[0].input.location).toBe('San Francisco');
  });
});

describe('InferenceActor - Template Interpolation', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
    (actor as any).client = createMockAnthropicClient();
  });

  test('interpolates simple variables', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage(
      'generate',
      {
        prompt: 'Hello, {{name}}!',
      },
      { name: 'Alice' }
    );

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.messages[0].content).toBe('Hello, Alice!');
  });

  test('interpolates nested variables', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage(
      'generate',
      {
        prompt: 'User: {{user.name}}, Age: {{user.age}}',
      },
      { user: { name: 'Bob', age: 30 } }
    );

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.messages[0].content).toBe('User: Bob, Age: 30');
  });

  test('leaves unmatched variables unchanged', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage(
      'generate',
      {
        prompt: 'Hello, {{missing}}!',
      },
      { name: 'Alice' }
    );

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.messages[0].content).toBe('Hello, {{missing}}!');
  });
});

describe('InferenceActor - Error Handling', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
  });

  test('handles rate limit error (429)', async () => {
    const mockClient = {
      messages: {
        create: mock(async () => {
          const error: any = new Error('Rate limit exceeded');
          error.status = 429;
          throw error;
        }),
      },
    };

    (actor as any).client = mockClient;

    const msg = testMessage('generate', { prompt: 'Test' });
    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Rate limit');
  });

  test('handles authentication error (401)', async () => {
    const mockClient = {
      messages: {
        create: mock(async () => {
          const error: any = new Error('Invalid API key');
          error.status = 401;
          throw error;
        }),
      },
    };

    (actor as any).client = mockClient;

    const msg = testMessage('generate', { prompt: 'Test' });
    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Authentication failed');
  });

  test('handles generic API error', async () => {
    const mockClient = {
      messages: {
        create: mock(async () => {
          throw new Error('Server error');
        }),
      },
    };

    (actor as any).client = mockClient;

    const msg = testMessage('generate', { prompt: 'Test' });
    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('API error');
  });
});

describe('InferenceActor - Model Resolution', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
    (actor as any).client = createMockAnthropicClient();
  });

  test('resolves claude-sonnet-4.5 alias', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test',
      model: 'claude-sonnet-4.5',
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.model).toBe('claude-sonnet-4-5-20250514');
  });

  test('resolves claude-opus-4.5 alias', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test',
      model: 'claude-opus-4.5',
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.model).toBe('claude-opus-4-5-20251101');
  });

  test('resolves claude-haiku-4.5 alias', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test',
      model: 'claude-haiku-4.5',
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.model).toBe('claude-haiku-4-5-20250514');
  });

  test('passes through unknown model names', async () => {
    const mockClient = createMockAnthropicClient();
    (actor as any).client = mockClient;

    const msg = testMessage('generate', {
      prompt: 'Test',
      model: 'claude-custom-model',
    });

    await actor.receive(msg);

    expect(mockClient.messages.create).toHaveBeenCalled();
    const callArgs = mockClient.messages.create.mock.calls[0][0];
    expect(callArgs.model).toBe('claude-custom-model');
  });
});

describe('InferenceActor - Embeddings', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
  });

  test('returns error for embeddings request (not supported)', async () => {
    const msg = testMessage('embeddings', {
      text: 'Test text',
    });

    const response = await actor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('not supported');
  });
});

describe('InferenceActor - Cost Calculation', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
    (actor as any).client = createMockAnthropicClient();
  });

  test('calculates cost for token usage', async () => {
    const msg = testMessage('generate', {
      prompt: 'Test prompt',
    });

    await actor.receive(msg);

    const statsMsg = testMessage('get-stats', {});
    const statsResponse = await actor.receive(statsMsg);

    // Should have some cost > 0 based on 10 input + 20 output tokens
    expect(statsResponse.payload.totalCost).toBeGreaterThan(0);
    expect(statsResponse.payload.totalCost).toBeLessThan(1); // Should be very small
  });

  test('accumulates cost across multiple requests', async () => {
    // First request
    await actor.receive(testMessage('generate', { prompt: 'Test 1' }));

    // Second request
    await actor.receive(testMessage('generate', { prompt: 'Test 2' }));

    const statsMsg = testMessage('get-stats', {});
    const statsResponse = await actor.receive(statsMsg);

    expect(statsResponse.payload.totalInputTokens).toBe(20);
    expect(statsResponse.payload.totalOutputTokens).toBe(40);
  });
});

describe('InferenceActor - Response Format', () => {
  let router: MessageRouter;
  let actor: InferenceActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new InferenceActor('inference', router, 'sk-test-key');
    (actor as any).client = createMockAnthropicClient();
  });

  test('returns properly formatted InferenceResult', async () => {
    const msg = testMessage('generate', {
      prompt: 'Test prompt',
    });

    const response = await actor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.id).toBeDefined();
    expect(response.payload.model).toBeDefined();
    expect(response.payload.content).toBeDefined();
    expect(response.payload.usage).toBeDefined();
    expect(response.payload.usage.inputTokens).toBeGreaterThan(0);
    expect(response.payload.usage.outputTokens).toBeGreaterThan(0);
    expect(response.payload.stopReason).toBeDefined();
  });

  test('includes multiple text blocks concatenated', async () => {
    const mockClient = {
      messages: {
        create: mock(async () => ({
          id: 'msg_test123',
          type: 'message',
          role: 'assistant',
          model: 'claude-sonnet-4-5-20250514',
          content: [
            { type: 'text', text: 'First part' },
            { type: 'text', text: 'Second part' },
          ],
          stop_reason: 'end_turn',
          usage: { input_tokens: 10, output_tokens: 20 },
        })),
      },
    };

    (actor as any).client = mockClient;

    const msg = testMessage('generate', { prompt: 'Test' });
    const response = await actor.receive(msg);

    expect(response.payload.content).toBe('First part\nSecond part');
  });
});
