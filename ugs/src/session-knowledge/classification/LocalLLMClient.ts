#!/usr/bin/env bun
/**
 * LocalLLMClient - Interface for LLM API (LM Studio or Cloudflare AI Gateway)
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.4
 */

export interface LLMOptions {
  baseUrl?: string;
  model?: string;
  temperature?: number;
  maxTokens?: number;
}

export interface ChatMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

export interface LLMResponse {
  content: string;
  usage?: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
}

/**
 * Client for local LM Studio API (OpenAI-compatible)
 *
 * Usage:
 * - Start LM Studio with a model loaded
 * - Default endpoint: http://localhost:1234/v1
 * - Free inference, ~500ms per classification
 */
export class LocalLLMClient {
  private baseUrl: string;
  private model: string;
  private temperature: number;
  private maxTokens: number;

  constructor(options?: LLMOptions) {
    this.baseUrl = options?.baseUrl || process.env.LLM_BASE_URL || 'http://localhost:1234/v1';
    // Use qwen model if available, fallback to llama
    this.model = options?.model || process.env.LLM_MODEL || 'qwen/qwen3-4b-2507';
    this.temperature = options?.temperature ?? 0.1;
    this.maxTokens = options?.maxTokens ?? 1000;
  }

  /**
   * Call local LLM with chat messages
   */
  async chat(messages: ChatMessage[]): Promise<LLMResponse> {
    try {
      const response = await fetch(`${this.baseUrl}/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          model: this.model,
          messages,
          temperature: this.temperature,
          max_tokens: this.maxTokens,
          stream: false
        })
      });

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`LLM API error: ${response.status} ${error}`);
      }

      const data = await response.json();
      const choice = data.choices?.[0];

      if (!choice) {
        throw new Error('No response from LLM');
      }

      return {
        content: choice.message.content,
        usage: data.usage ? {
          promptTokens: data.usage.prompt_tokens,
          completionTokens: data.usage.completion_tokens,
          totalTokens: data.usage.total_tokens
        } : undefined
      };
    } catch (error) {
      if (error instanceof Error && error.message.includes('fetch failed')) {
        throw new Error(
          'Failed to connect to LM Studio. Make sure it\'s running at ' + this.baseUrl
        );
      }
      throw error;
    }
  }

  /**
   * Call LLM with structured JSON output
   *
   * Expects the LLM to return valid JSON matching the provided schema.
   * Uses temperature=0.1 for more deterministic outputs.
   */
  async chatJSON<T = any>(
    messages: ChatMessage[],
    schema?: string
  ): Promise<T> {
    // Add JSON formatting instruction to system message
    const systemMessage = messages.find(m => m.role === 'system');
    if (systemMessage) {
      systemMessage.content += '\n\nYou must respond with valid JSON only. Do not include any text before or after the JSON object. Use null for missing values, not the string "null".';
      if (schema) {
        systemMessage.content += `\n\nExpected schema:\n${schema}`;
      }
    }

    const response = await this.chat(messages);

    try {
      // Try to extract JSON from response (in case LLM adds extra text)
      const jsonMatch = response.content.match(/\{[\s\S]*\}/);
      let jsonStr = jsonMatch ? jsonMatch[0] : response.content;

      // Fix common LLM JSON formatting issues
      jsonStr = jsonStr
        .replace(/: "null"/g, ': null')           // "null" string to null
        .replace(/: "true"/g, ': true')           // "true" string to boolean
        .replace(/: "false"/g, ': false')         // "false" string to boolean
        .replace(/: "\{([^}]*)\}"/g, ': {$1}')   // Nested quoted objects
        .replace(/: "\[([^\]]*)\]"/g, ': [$1]'); // Nested quoted arrays

      return JSON.parse(jsonStr);
    } catch (error) {
      throw new Error(`Failed to parse LLM JSON response: ${response.content}`);
    }
  }

  /**
   * Simple text completion (single user message)
   */
  async complete(prompt: string, systemPrompt?: string): Promise<string> {
    const messages: ChatMessage[] = [];

    if (systemPrompt) {
      messages.push({ role: 'system', content: systemPrompt });
    }

    messages.push({ role: 'user', content: prompt });

    const response = await this.chat(messages);
    return response.content;
  }

  /**
   * Check if LM Studio is available
   */
  async isAvailable(): Promise<boolean> {
    try {
      const response = await fetch(`${this.baseUrl}/models`, {
        method: 'GET',
        signal: AbortSignal.timeout(2000)
      });
      return response.ok;
    } catch {
      return false;
    }
  }

  /**
   * Get available models from LM Studio
   */
  async listModels(): Promise<string[]> {
    try {
      const response = await fetch(`${this.baseUrl}/models`);
      if (!response.ok) return [];

      const data = await response.json();
      return data.data?.map((m: any) => m.id) || [];
    } catch {
      return [];
    }
  }
}
