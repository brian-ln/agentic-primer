#!/usr/bin/env bun
/**
 * InferenceActor - LLM inference via Anthropic API
 *
 * Provides message-based interface for LLM calls, embeddings, and reasoning.
 * Integrates with query layer via ask() for request-response patterns.
 *
 * Examples:
 *   @(inference) - Main inference actor
 *
 * Message Types:
 *   - generate: Generate text with Claude
 *   - embeddings: Get text embeddings (not supported by Anthropic SDK)
 */

import { Actor } from '../actor.ts';
import type { MessageRouter } from '../router.ts';
import type { Message, MessageResponse } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import Anthropic from '@anthropic-ai/sdk';

/**
 * Tool definition for Claude API
 */
export interface Tool {
  name: string;
  description: string;
  input_schema: {
    type: 'object';
    properties: Record<string, any>;
    required?: string[];
  };
}

/**
 * Tool use result from Claude
 */
export interface ToolUse {
  id: string;
  name: string;
  input: Record<string, any>;
}

/**
 * Inference result
 */
export interface InferenceResult {
  id: string;
  model: string;
  content: string;
  usage: {
    inputTokens: number;
    outputTokens: number;
  };
  stopReason: string;
  toolUses?: ToolUse[];
}

/**
 * Generate message payload
 */
export interface GeneratePayload {
  model?: string;
  prompt: string;
  system?: string;
  maxTokens?: number;
  temperature?: number;
  tools?: Tool[];
}

/**
 * Embeddings message payload
 */
export interface EmbeddingsPayload {
  text: string;
  model?: string;
}

/**
 * Model pricing (per million tokens)
 */
const MODEL_PRICING: Record<string, { input: number; output: number }> = {
  'claude-sonnet-4.5': { input: 3.0, output: 15.0 },
  'claude-opus-4.5': { input: 15.0, output: 75.0 },
  'claude-haiku-4.5': { input: 0.8, output: 4.0 },
  'claude-sonnet-4': { input: 3.0, output: 15.0 },
  'claude-3-5-sonnet-20241022': { input: 3.0, output: 15.0 },
};

/**
 * InferenceActor - Manages LLM inference through Anthropic API
 */
export class InferenceActor extends Actor {
  private client: Anthropic | null = null;
  private apiKey: string | null = null;
  private defaultModel = 'claude-sonnet-4.5';
  private totalInputTokens = 0;
  private totalOutputTokens = 0;
  private totalCost = 0;

  constructor(id: string, router: MessageRouter, apiKey?: string) {
    super(id, router);
    this.apiKey = apiKey || process.env.ANTHROPIC_API_KEY || null;

    if (this.apiKey) {
      this.client = new Anthropic({
        apiKey: this.apiKey,
      });
    }
  }

  /**
   * Handle incoming messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'generate':
          return await this.handleGenerate(message, payload);

        case 'embeddings':
          return await this.handleEmbeddings(message, payload);

        case 'get-stats':
          return await this.handleGetStats(message);

        case 'reset-stats':
          return await this.handleResetStats(message);

        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message);
    }
  }

  /**
   * Generate text with Claude
   */
  private async handleGenerate(message: Message, payload: GeneratePayload): Promise<MessageResponse> {
    if (!this.client) {
      return createErrorResponse(message, 'Anthropic API key not configured. Set ANTHROPIC_API_KEY environment variable.');
    }

    const { prompt, system, model, maxTokens, temperature, tools } = payload;

    if (!prompt) {
      return createErrorResponse(message, 'Missing required field: prompt');
    }

    const modelId = this.resolveModel(model || this.defaultModel);

    try {
      const messages: Anthropic.MessageParam[] = [
        {
          role: 'user',
          content: this.interpolateTemplate(prompt, message.metadata || {}),
        },
      ];

      const requestParams: Anthropic.MessageCreateParams = {
        model: modelId,
        messages,
        max_tokens: maxTokens || 1024,
        temperature: temperature !== undefined ? temperature : 1.0,
      };

      if (system) {
        requestParams.system = system;
      }

      if (tools && tools.length > 0) {
        requestParams.tools = tools as Anthropic.Tool[];
      }

      const response = await this.client.messages.create(requestParams);

      // Extract text content
      const textContent = response.content
        .filter((block): block is Anthropic.TextBlock => block.type === 'text')
        .map(block => block.text)
        .join('\n');

      // Extract tool uses
      const toolUses = response.content
        .filter((block): block is Anthropic.ToolUseBlock => block.type === 'tool_use')
        .map(block => ({
          id: block.id,
          name: block.name,
          input: block.input as Record<string, any>,
        }));

      const result: InferenceResult = {
        id: response.id,
        model: response.model,
        content: textContent,
        usage: {
          inputTokens: response.usage.input_tokens,
          outputTokens: response.usage.output_tokens,
        },
        stopReason: response.stop_reason || 'end_turn',
        toolUses: toolUses.length > 0 ? toolUses : undefined,
      };

      // Track usage
      this.totalInputTokens += result.usage.inputTokens;
      this.totalOutputTokens += result.usage.outputTokens;
      this.totalCost += this.calculateCost(modelId, result.usage.inputTokens, result.usage.outputTokens);

      return createResponse(message, result);
    } catch (error: any) {
      if (error.status === 429) {
        return createErrorResponse(message, 'Rate limit exceeded. Please try again later.');
      }
      if (error.status === 401) {
        return createErrorResponse(message, 'Authentication failed. Check your API key.');
      }
      return createErrorResponse(message, `API error: ${error.message}`);
    }
  }

  /**
   * Get embeddings (not supported by Anthropic SDK - returns error)
   */
  private async handleEmbeddings(message: Message, payload: EmbeddingsPayload): Promise<MessageResponse> {
    return createErrorResponse(
      message,
      'Embeddings are not supported by Anthropic API. Use OpenAI or other embedding providers.'
    );
  }

  /**
   * Get usage statistics
   */
  private async handleGetStats(message: Message): Promise<MessageResponse> {
    return createResponse(message, {
      totalInputTokens: this.totalInputTokens,
      totalOutputTokens: this.totalOutputTokens,
      totalCost: this.totalCost,
      configured: this.client !== null,
    });
  }

  /**
   * Reset usage statistics
   */
  private async handleResetStats(message: Message): Promise<MessageResponse> {
    this.totalInputTokens = 0;
    this.totalOutputTokens = 0;
    this.totalCost = 0;
    return createResponse(message, { reset: true });
  }

  /**
   * Resolve model alias to full model ID
   */
  private resolveModel(model: string): string {
    const modelMap: Record<string, string> = {
      'claude-sonnet-4.5': 'claude-sonnet-4-5-20250514',
      'claude-opus-4.5': 'claude-opus-4-5-20251101',
      'claude-haiku-4.5': 'claude-haiku-4-5-20250514',
      'claude-sonnet-4': 'claude-3-5-sonnet-20241022',
    };

    return modelMap[model] || model;
  }

  /**
   * Calculate cost based on token usage
   */
  private calculateCost(model: string, inputTokens: number, outputTokens: number): number {
    // Try to find pricing for the model (use base name)
    const baseModel = Object.keys(MODEL_PRICING).find(key => model.includes(key.replace('claude-', '')));
    const pricing = baseModel ? MODEL_PRICING[baseModel] : MODEL_PRICING['claude-sonnet-4.5'];

    const inputCost = (inputTokens / 1_000_000) * pricing.input;
    const outputCost = (outputTokens / 1_000_000) * pricing.output;

    return inputCost + outputCost;
  }

  /**
   * Simple template interpolation (replaces {{var}} with metadata values)
   */
  private interpolateTemplate(template: string, metadata: Record<string, any>): string {
    return template.replace(/\{\{(\w+(?:\.\w+)*)\}\}/g, (match, path) => {
      const value = this.getNestedValue(metadata, path);
      return value !== undefined ? String(value) : match;
    });
  }

  /**
   * Get nested value from object using dot notation
   */
  private getNestedValue(obj: any, path: string): any {
    const parts = path.split('.');
    let current = obj;

    for (const part of parts) {
      if (current && typeof current === 'object' && part in current) {
        current = current[part];
      } else {
        return undefined;
      }
    }

    return current;
  }
}

export default InferenceActor;
