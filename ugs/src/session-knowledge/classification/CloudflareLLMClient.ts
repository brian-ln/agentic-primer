#!/usr/bin/env bun
/**
 * CloudflareLLMClient - Cloudflare AI Gateway client using ai-gateway-provider
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.4
 *
 * Uses the same pattern as src/entities/model.ts for consistency
 */

import { createAiGateway } from 'ai-gateway-provider';
import { createUnified } from 'ai-gateway-provider/providers/unified';
import { generateText } from 'ai';

export interface CloudflareLLMOptions {
  accountId?: string;
  gatewayId?: string;
  apiToken?: string;
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
 * Client for Cloudflare AI Gateway (unified provider)
 *
 * Recommended model: @cf/meta/llama-3.2-3b-instruct
 * Cost: ~$0.08/M input tokens, ~$0.27/M output tokens
 *
 * Free tier: 10,000 neurons/day
 */
export class CloudflareLLMClient {
  private accountId: string;
  private gatewayId: string;
  private apiToken: string;
  private model: string;
  private temperature: number;
  private maxTokens: number;

  constructor(options?: CloudflareLLMOptions) {
    // Support both /ai config names and standard names
    this.accountId = options?.accountId
      || process.env.CLOUDFLARE_ACCOUNT_ID
      || process.env.CF_ACCOUNT_ID
      || '';
    this.gatewayId = options?.gatewayId
      || process.env.CLOUDFLARE_GATEWAY_ID
      || process.env.CF_GATEWAY_NAME
      || '';
    this.apiToken = options?.apiToken
      || process.env.CLOUDFLARE_API_TOKEN
      || process.env.CF_AIG_TOKEN
      || '';
    this.model = options?.model || process.env.LLM_MODEL || '@cf/meta/llama-3.2-3b-instruct';
    this.temperature = options?.temperature ?? 0.1;
    this.maxTokens = options?.maxTokens ?? 1000;

    if (!this.accountId || !this.gatewayId || !this.apiToken) {
      throw new Error(
        'Cloudflare credentials required: CF_ACCOUNT_ID/CLOUDFLARE_ACCOUNT_ID, CF_GATEWAY_NAME/CLOUDFLARE_GATEWAY_ID, CF_AIG_TOKEN/CLOUDFLARE_API_TOKEN'
      );
    }
  }

  /**
   * Call LLM with chat messages
   */
  async chat(messages: ChatMessage[]): Promise<LLMResponse> {
    try {
      // For @cf/ models (Workers AI), use direct API, not unified provider
      if (this.model.startsWith('@cf/')) {
        return await this.chatWorkersAI(messages);
      }

      // For other models (anthropic/claude, openai/gpt, etc.), use unified provider
      const aigateway = createAiGateway({
        accountId: this.accountId,
        gateway: this.gatewayId,
        apiKey: this.apiToken,
      });

      const unified = createUnified();
      const aiModel = aigateway(unified(this.model));

      const systemMessage = messages.find(m => m.role === 'system');
      const userMessages = messages.filter(m => m.role !== 'system');

      const result = await generateText({
        model: aiModel,
        system: systemMessage?.content,
        messages: userMessages.map(m => ({
          role: m.role as 'user' | 'assistant',
          content: m.content
        })),
        temperature: this.temperature,
        maxTokens: this.maxTokens,
      });

      return {
        content: result.text,
        usage: result.usage ? {
          promptTokens: result.usage.promptTokens,
          completionTokens: result.usage.completionTokens,
          totalTokens: result.usage.totalTokens
        } : undefined
      };
    } catch (error) {
      if (error instanceof Error) {
        throw new Error(`Cloudflare AI error: ${error.message}`);
      }
      throw error;
    }
  }

  /**
   * Call Workers AI directly (for @cf/ models)
   */
  private async chatWorkersAI(messages: ChatMessage[]): Promise<LLMResponse> {
    const url = `https://api.cloudflare.com/client/v4/accounts/${this.accountId}/ai/run/${this.model}`;

    const response = await fetch(url, {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${this.apiToken}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        messages: messages.map(m => ({
          role: m.role,
          content: m.content
        })),
        temperature: this.temperature,
        max_tokens: this.maxTokens,
      })
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Workers AI error: ${response.status} ${error}`);
    }

    const data = await response.json();

    if (!data.success || !data.result?.response) {
      throw new Error(`Workers AI error: ${JSON.stringify(data)}`);
    }

    return {
      content: data.result.response,
      usage: {
        promptTokens: data.result.usage?.prompt_tokens || 0,
        completionTokens: data.result.usage?.completion_tokens || 0,
        totalTokens: data.result.usage?.total_tokens || 0
      }
    };
  }

  /**
   * Call LLM with structured JSON output
   */
  async chatJSON<T = any>(
    messages: ChatMessage[],
    schema?: string
  ): Promise<T> {
    // Add JSON formatting instruction
    const systemMessage = messages.find(m => m.role === 'system');
    if (systemMessage) {
      systemMessage.content += '\n\nYou must respond with valid JSON only. Do not include any text before or after the JSON object. Use null for missing values, not the string "null".';
      if (schema) {
        systemMessage.content += `\n\nExpected schema:\n${schema}`;
      }
    }

    const response = await this.chat(messages);

    try {
      // Extract JSON from response
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
      throw new Error(`Failed to parse JSON response: ${response.content}`);
    }
  }

  /**
   * Simple text completion
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
   * Check if Cloudflare AI Gateway is available
   */
  async isAvailable(): Promise<boolean> {
    try {
      const testMessage = { role: 'user' as const, content: 'test' };
      await this.chat([testMessage]);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Get provider info
   */
  getProviderInfo(): { provider: string; model: string; accountId: string; gatewayId: string } {
    return {
      provider: 'cloudflare-ai-gateway',
      model: this.model,
      accountId: this.accountId,
      gatewayId: this.gatewayId
    };
  }
}
