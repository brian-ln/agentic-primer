/**
 * LocalLLMClient Tests
 * Task: agentic-primer-t49.8
 * Epic: agentic-primer-9ad
 *
 * Comprehensive tests for LocalLLMClient:
 * - Constructor and initialization
 * - chat() method - Chat completions
 * - chatJSON() method - Structured JSON outputs
 * - complete() method - Simple text completions
 * - isAvailable() method - Connection checking
 * - listModels() method - Model discovery
 * - Error handling (connection failures, timeouts, invalid responses)
 * - Request/response parsing
 * - Message formatting
 * - Temperature and max_tokens parameters
 *
 * Target: 70%+ coverage (0% → 70%+)
 */

import { describe, test, expect, beforeEach, mock } from 'bun:test';
import { LocalLLMClient } from '../classification/LocalLLMClient';
import type { ChatMessage, LLMOptions } from '../classification/LocalLLMClient';

// Mock fetch globally
const mockFetch = mock();
globalThis.fetch = mockFetch as any;

describe('LocalLLMClient - Constructor and Initialization', () => {
  beforeEach(() => {
    mockFetch.mockClear();
    delete process.env.LLM_BASE_URL;
    delete process.env.LLM_MODEL;
  });

  test('should initialize with default values', () => {
    const client = new LocalLLMClient();
    expect(client).toBeDefined();
  });

  test('should use custom baseUrl from options', () => {
    const client = new LocalLLMClient({
      baseUrl: 'http://custom:8080/v1'
    });
    expect(client).toBeDefined();
  });

  test('should use custom model from options', () => {
    const client = new LocalLLMClient({
      model: 'custom-model'
    });
    expect(client).toBeDefined();
  });

  test('should use custom temperature from options', () => {
    const client = new LocalLLMClient({
      temperature: 0.5
    });
    expect(client).toBeDefined();
  });

  test('should use custom maxTokens from options', () => {
    const client = new LocalLLMClient({
      maxTokens: 2000
    });
    expect(client).toBeDefined();
  });

  test('should use environment variables when no options provided', () => {
    process.env.LLM_BASE_URL = 'http://env:9000/v1';
    process.env.LLM_MODEL = 'env-model';

    const client = new LocalLLMClient();
    expect(client).toBeDefined();
  });

  test('should prefer options over environment variables', () => {
    process.env.LLM_BASE_URL = 'http://env:9000/v1';
    process.env.LLM_MODEL = 'env-model';

    const client = new LocalLLMClient({
      baseUrl: 'http://options:7000/v1',
      model: 'options-model'
    });
    expect(client).toBeDefined();
  });

  test('should handle temperature value of 0', () => {
    const client = new LocalLLMClient({
      temperature: 0
    });
    expect(client).toBeDefined();
  });

  test('should handle all options together', () => {
    const client = new LocalLLMClient({
      baseUrl: 'http://localhost:1234/v1',
      model: 'llama-3.2-3b',
      temperature: 0.7,
      maxTokens: 500
    });
    expect(client).toBeDefined();
  });
});

describe('LocalLLMClient - chat() method', () => {
  let client: LocalLLMClient;

  beforeEach(() => {
    mockFetch.mockClear();
    client = new LocalLLMClient({
      baseUrl: 'http://localhost:1234/v1',
      model: 'test-model',
      temperature: 0.1,
      maxTokens: 1000
    });
  });

  test('should make successful chat completion request', async () => {
    const mockResponse = {
      choices: [
        {
          message: {
            content: 'This is a test response'
          }
        }
      ],
      usage: {
        prompt_tokens: 10,
        completion_tokens: 5,
        total_tokens: 15
      }
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockResponse
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'You are a helpful assistant' },
      { role: 'user', content: 'Hello' }
    ];

    const result = await client.chat(messages);

    expect(result.content).toBe('This is a test response');
    expect(result.usage).toBeDefined();
    expect(result.usage?.promptTokens).toBe(10);
    expect(result.usage?.completionTokens).toBe(5);
    expect(result.usage?.totalTokens).toBe(15);
  });

  test('should send correct request payload', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: 'response' } }]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'user', content: 'Test message' }
    ];

    await client.chat(messages);

    expect(mockFetch).toHaveBeenCalledTimes(1);
    const call = mockFetch.mock.calls[0];
    expect(call[0]).toBe('http://localhost:1234/v1/chat/completions');

    const requestInit = call[1];
    expect(requestInit.method).toBe('POST');
    expect(requestInit.headers['Content-Type']).toBe('application/json');

    const body = JSON.parse(requestInit.body);
    expect(body.model).toBe('test-model');
    expect(body.messages).toEqual(messages);
    expect(body.temperature).toBe(0.1);
    expect(body.max_tokens).toBe(1000);
    expect(body.stream).toBe(false);
  });

  test('should handle response without usage data', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: 'Response without usage'
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chat(messages);

    expect(result.content).toBe('Response without usage');
    expect(result.usage).toBeUndefined();
  });

  test('should throw error on HTTP error response', async () => {
    mockFetch.mockResolvedValue({
      ok: false,
      status: 500,
      text: async () => 'Internal Server Error'
    });

    const messages: ChatMessage[] = [
      { role: 'user', content: 'Test' }
    ];

    await expect(client.chat(messages)).rejects.toThrow('LLM API error: 500 Internal Server Error');
  });

  test('should throw error when no choices in response', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: []
      })
    });

    const messages: ChatMessage[] = [
      { role: 'user', content: 'Test' }
    ];

    await expect(client.chat(messages)).rejects.toThrow('No response from LLM');
  });

  test('should throw error when choices is undefined', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({})
    });

    const messages: ChatMessage[] = [
      { role: 'user', content: 'Test' }
    ];

    await expect(client.chat(messages)).rejects.toThrow('No response from LLM');
  });

  test('should throw connection error when fetch fails', async () => {
    mockFetch.mockRejectedValue(new Error('fetch failed'));

    const messages: ChatMessage[] = [
      { role: 'user', content: 'Test' }
    ];

    await expect(client.chat(messages)).rejects.toThrow(
      'Failed to connect to LM Studio. Make sure it\'s running at http://localhost:1234/v1'
    );
  });

  test('should rethrow non-connection errors', async () => {
    mockFetch.mockRejectedValue(new Error('Some other error'));

    const messages: ChatMessage[] = [
      { role: 'user', content: 'Test' }
    ];

    await expect(client.chat(messages)).rejects.toThrow('Some other error');
  });

  test('should handle multiple messages', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: 'response' } }]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'System prompt' },
      { role: 'user', content: 'First message' },
      { role: 'assistant', content: 'First response' },
      { role: 'user', content: 'Second message' }
    ];

    const result = await client.chat(messages);

    expect(result.content).toBe('response');
    expect(mockFetch).toHaveBeenCalledTimes(1);

    const body = JSON.parse(mockFetch.mock.calls[0][1].body);
    expect(body.messages).toEqual(messages);
  });
});

describe('LocalLLMClient - chatJSON() method', () => {
  let client: LocalLLMClient;

  beforeEach(() => {
    mockFetch.mockClear();
    client = new LocalLLMClient({
      baseUrl: 'http://localhost:1234/v1',
      model: 'test-model'
    });
  });

  test('should parse valid JSON response', async () => {
    const jsonResponse = { type: 'decision', confidence: 0.95 };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: JSON.stringify(jsonResponse)
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'You are a classifier' },
      { role: 'user', content: 'Classify this' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual(jsonResponse);
  });

  test('should add JSON instructions to system message', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: '{"result": "test"}' } }]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Original system prompt' },
      { role: 'user', content: 'Test' }
    ];

    await client.chatJSON(messages);

    const body = JSON.parse(mockFetch.mock.calls[0][1].body);
    const systemMessage = body.messages[0];

    expect(systemMessage.content).toContain('Original system prompt');
    expect(systemMessage.content).toContain('You must respond with valid JSON only');
    expect(systemMessage.content).toContain('Do not include any text before or after the JSON object');
  });

  test('should add schema to system message when provided', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: '{"field": "value"}' } }]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Classify' },
      { role: 'user', content: 'Test' }
    ];

    const schema = '{ field: string }';
    await client.chatJSON(messages, schema);

    const body = JSON.parse(mockFetch.mock.calls[0][1].body);
    const systemMessage = body.messages[0];

    expect(systemMessage.content).toContain('Expected schema:');
    expect(systemMessage.content).toContain(schema);
  });

  test('should extract JSON from response with extra text', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: 'Here is the JSON: {"result": "extracted"} - that\'s it'
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual({ result: 'extracted' });
  });

  test('should fix "null" string to null', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: '{"value": "null"}'
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual({ value: null });
  });

  test('should fix "true" string to boolean true', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: '{"active": "true"}'
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual({ active: true });
  });

  test('should fix "false" string to boolean false', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: '{"enabled": "false"}'
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual({ enabled: false });
  });

  test('should throw error on invalid JSON', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: 'This is not JSON'
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    await expect(client.chatJSON(messages)).rejects.toThrow('Failed to parse LLM JSON response');
  });

  test('should handle nested objects', async () => {
    const nestedJson = {
      outer: {
        inner: {
          value: 'test'
        }
      }
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: JSON.stringify(nestedJson)
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual(nestedJson);
  });

  test('should handle arrays in JSON', async () => {
    const arrayJson = {
      items: ['item1', 'item2', 'item3']
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: JSON.stringify(arrayJson)
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual(arrayJson);
  });

  test('should handle complex JSON with multiple fixes needed', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: '{"enabled": "true", "value": "null", "active": "false"}'
            }
          }
        ]
      })
    });

    const messages: ChatMessage[] = [
      { role: 'system', content: 'Test' },
      { role: 'user', content: 'Test' }
    ];

    const result = await client.chatJSON(messages);

    expect(result).toEqual({
      enabled: true,
      value: null,
      active: false
    });
  });
});

describe('LocalLLMClient - complete() method', () => {
  let client: LocalLLMClient;

  beforeEach(() => {
    mockFetch.mockClear();
    client = new LocalLLMClient({
      baseUrl: 'http://localhost:1234/v1'
    });
  });

  test('should complete with user prompt only', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: 'Completion response'
            }
          }
        ]
      })
    });

    const result = await client.complete('What is 2+2?');

    expect(result).toBe('Completion response');
    expect(mockFetch).toHaveBeenCalledTimes(1);

    const body = JSON.parse(mockFetch.mock.calls[0][1].body);
    expect(body.messages).toHaveLength(1);
    expect(body.messages[0]).toEqual({
      role: 'user',
      content: 'What is 2+2?'
    });
  });

  test('should complete with system prompt and user prompt', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: 'System-aware response'
            }
          }
        ]
      })
    });

    const result = await client.complete(
      'What is 2+2?',
      'You are a math teacher'
    );

    expect(result).toBe('System-aware response');
    expect(mockFetch).toHaveBeenCalledTimes(1);

    const body = JSON.parse(mockFetch.mock.calls[0][1].body);
    expect(body.messages).toHaveLength(2);
    expect(body.messages[0]).toEqual({
      role: 'system',
      content: 'You are a math teacher'
    });
    expect(body.messages[1]).toEqual({
      role: 'user',
      content: 'What is 2+2?'
    });
  });

  test('should handle empty prompt', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: 'Empty prompt response'
            }
          }
        ]
      })
    });

    const result = await client.complete('');

    expect(result).toBe('Empty prompt response');
  });

  test('should handle long prompt', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [
          {
            message: {
              content: 'Long prompt response'
            }
          }
        ]
      })
    });

    const longPrompt = 'a'.repeat(1000);
    const result = await client.complete(longPrompt);

    expect(result).toBe('Long prompt response');

    const body = JSON.parse(mockFetch.mock.calls[0][1].body);
    expect(body.messages[0].content).toBe(longPrompt);
  });
});

describe('LocalLLMClient - isAvailable() method', () => {
  let client: LocalLLMClient;

  beforeEach(() => {
    mockFetch.mockClear();
    client = new LocalLLMClient({
      baseUrl: 'http://localhost:1234/v1'
    });
  });

  test('should return true when LM Studio is available', async () => {
    mockFetch.mockResolvedValue({
      ok: true
    });

    const result = await client.isAvailable();

    expect(result).toBe(true);
    expect(mockFetch).toHaveBeenCalledTimes(1);
    expect(mockFetch.mock.calls[0][0]).toBe('http://localhost:1234/v1/models');
    expect(mockFetch.mock.calls[0][1].method).toBe('GET');
  });

  test('should return false when LM Studio is not available', async () => {
    mockFetch.mockResolvedValue({
      ok: false
    });

    const result = await client.isAvailable();

    expect(result).toBe(false);
  });

  test('should return false on connection error', async () => {
    mockFetch.mockRejectedValue(new Error('Connection refused'));

    const result = await client.isAvailable();

    expect(result).toBe(false);
  });

  test('should return false on timeout', async () => {
    mockFetch.mockImplementation(() => {
      return new Promise((_, reject) => {
        setTimeout(() => reject(new Error('timeout')), 3000);
      });
    });

    const result = await client.isAvailable();

    expect(result).toBe(false);
  });

  test('should use timeout signal in request', async () => {
    mockFetch.mockResolvedValue({
      ok: true
    });

    await client.isAvailable();

    expect(mockFetch).toHaveBeenCalledTimes(1);
    const requestInit = mockFetch.mock.calls[0][1];
    expect(requestInit.signal).toBeDefined();
  });
});

describe('LocalLLMClient - listModels() method', () => {
  let client: LocalLLMClient;

  beforeEach(() => {
    mockFetch.mockClear();
    client = new LocalLLMClient({
      baseUrl: 'http://localhost:1234/v1'
    });
  });

  test('should return list of model IDs', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [
          { id: 'llama-3.2-3b-instruct' },
          { id: 'mistral-7b' },
          { id: 'phi-2' }
        ]
      })
    });

    const result = await client.listModels();

    expect(result).toEqual([
      'llama-3.2-3b-instruct',
      'mistral-7b',
      'phi-2'
    ]);
    expect(mockFetch).toHaveBeenCalledTimes(1);
    expect(mockFetch.mock.calls[0][0]).toBe('http://localhost:1234/v1/models');
  });

  test('should return empty array when response not ok', async () => {
    mockFetch.mockResolvedValue({
      ok: false
    });

    const result = await client.listModels();

    expect(result).toEqual([]);
  });

  test('should return empty array on connection error', async () => {
    mockFetch.mockRejectedValue(new Error('Connection refused'));

    const result = await client.listModels();

    expect(result).toEqual([]);
  });

  test('should return empty array when data is undefined', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({})
    });

    const result = await client.listModels();

    expect(result).toEqual([]);
  });

  test('should return empty array when data is empty', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: []
      })
    });

    const result = await client.listModels();

    expect(result).toEqual([]);
  });

  test('should handle single model', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [
          { id: 'single-model' }
        ]
      })
    });

    const result = await client.listModels();

    expect(result).toEqual(['single-model']);
  });

  test('should handle models with additional metadata', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [
          {
            id: 'model-1',
            name: 'Model 1',
            description: 'First model',
            size: '3B'
          },
          {
            id: 'model-2',
            name: 'Model 2',
            description: 'Second model',
            size: '7B'
          }
        ]
      })
    });

    const result = await client.listModels();

    expect(result).toEqual(['model-1', 'model-2']);
  });
});

describe('LocalLLMClient - Integration scenarios', () => {
  let client: LocalLLMClient;

  beforeEach(() => {
    mockFetch.mockClear();
  });

  test('should work with different base URLs', async () => {
    const client1 = new LocalLLMClient({
      baseUrl: 'http://localhost:1234/v1'
    });
    const client2 = new LocalLLMClient({
      baseUrl: 'http://192.168.1.100:8080/v1'
    });

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: 'response' } }]
      })
    });

    await client1.chat([{ role: 'user', content: 'test' }]);
    expect(mockFetch.mock.calls[0][0]).toBe('http://localhost:1234/v1/chat/completions');

    await client2.chat([{ role: 'user', content: 'test' }]);
    expect(mockFetch.mock.calls[1][0]).toBe('http://192.168.1.100:8080/v1/chat/completions');
  });

  test('should handle sequential requests', async () => {
    client = new LocalLLMClient();

    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: 'First response' } }]
      })
    }).mockResolvedValueOnce({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: 'Second response' } }]
      })
    });

    const result1 = await client.chat([{ role: 'user', content: 'First' }]);
    const result2 = await client.chat([{ role: 'user', content: 'Second' }]);

    expect(result1.content).toBe('First response');
    expect(result2.content).toBe('Second response');
    expect(mockFetch).toHaveBeenCalledTimes(2);
  });

  test('should handle error then success', async () => {
    client = new LocalLLMClient();

    mockFetch.mockRejectedValueOnce(new Error('fetch failed'))
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          choices: [{ message: { content: 'Success after retry' } }]
        })
      });

    await expect(client.chat([{ role: 'user', content: 'test' }])).rejects.toThrow();

    const result = await client.chat([{ role: 'user', content: 'test' }]);
    expect(result.content).toBe('Success after retry');
  });

  test('should handle different temperature values', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: 'response' } }]
      })
    });

    const client1 = new LocalLLMClient({ temperature: 0 });
    await client1.chat([{ role: 'user', content: 'test' }]);
    let body = JSON.parse(mockFetch.mock.calls[0][1].body);
    expect(body.temperature).toBe(0);

    const client2 = new LocalLLMClient({ temperature: 0.5 });
    await client2.chat([{ role: 'user', content: 'test' }]);
    body = JSON.parse(mockFetch.mock.calls[1][1].body);
    expect(body.temperature).toBe(0.5);

    const client3 = new LocalLLMClient({ temperature: 1.0 });
    await client3.chat([{ role: 'user', content: 'test' }]);
    body = JSON.parse(mockFetch.mock.calls[2][1].body);
    expect(body.temperature).toBe(1.0);
  });

  test('should handle different maxTokens values', async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        choices: [{ message: { content: 'response' } }]
      })
    });

    const client1 = new LocalLLMClient({ maxTokens: 100 });
    await client1.chat([{ role: 'user', content: 'test' }]);
    let body = JSON.parse(mockFetch.mock.calls[0][1].body);
    expect(body.max_tokens).toBe(100);

    const client2 = new LocalLLMClient({ maxTokens: 2000 });
    await client2.chat([{ role: 'user', content: 'test' }]);
    body = JSON.parse(mockFetch.mock.calls[1][1].body);
    expect(body.max_tokens).toBe(2000);
  });
});
