#!/usr/bin/env bun
/**
 * Model Entity for Universal Graph System
 *
 * Models are inference configurations that reference Providers.
 * They support situational parameter overrides for different use cases.
 * State machine: draft -> published -> deprecated
 */

import GraphStore, { Node } from '../graph.ts';
import { ProviderManager, Provider } from './provider.ts';
import { generateText, streamText } from 'ai';
import { createAiGateway } from 'ai-gateway-provider';
import { createUnified } from 'ai-gateway-provider/providers/unified';
import { getCurrentContext } from '../context.ts';
import { withRateLimit, llmRateLimiter } from '../session-knowledge/security/rate-limiter';

// Streaming options for model invocation
export interface StreamingOptions {
  stream?: boolean;
  onToken?: (token: string) => void;
}

// Model lifecycle states (same as Program)
export type ModelLifecycle = 'draft' | 'published' | 'deprecated';

// Model type discriminant
export type ModelType = 'inference' | 'embedding';

// Model event types
export type ModelEventType =
  | 'MODEL_CREATED'
  | 'MODEL_CONFIGURED'
  | 'MODEL_PUBLISHED'
  | 'MODEL_DEPRECATED'
  | 'MODEL_INFERENCE_COMPLETED'
  | 'MODEL_EMBEDDING_COMPLETED'
  | 'MODEL_TOKEN_RECEIVED';

// Situational parameter overrides (inference only)
export interface SituationParams {
  temperature?: number;
  maxTokens?: number;
  topP?: number;
}

// Base config shared by all model types
export interface BaseModelConfig {
  name: string;
  backendModel: string;  // "claude-sonnet-4-5", "gpt-4o", "@cf/baai/bge-base-en-v1.5", etc.
  provider: string;      // "@(cf-gateway)" - reference to provider
}

// Inference model config
export interface InferenceModelConfig extends BaseModelConfig {
  modelType: 'inference';
  temperature?: number;
  maxTokens?: number;
  topP?: number;
  situations?: Record<string, SituationParams>;
}

// Embedding model config
export interface EmbeddingModelConfig extends BaseModelConfig {
  modelType: 'embedding';
  dimensions: number;  // Fixed by model (e.g., 768 for bge-base-en-v1.5)
}

// Union type for model config
export type ModelConfig = InferenceModelConfig | EmbeddingModelConfig;

// Model interface
export interface Model {
  id: string;
  type: 'program';
  programType: 'model';
  lifecycle: ModelLifecycle;
  config: ModelConfig;
  version: number;
  created: number;
  modified: number;
}

// Type guards
export function isInferenceModel(model: Model): model is Model & { config: InferenceModelConfig } {
  return model.config.modelType === 'inference';
}

export function isEmbeddingModel(model: Model): model is Model & { config: EmbeddingModelConfig } {
  return model.config.modelType === 'embedding';
}

// Model event structure
export interface ModelEvent {
  id: string;
  timestamp: number;
  type: ModelEventType;
  modelId: string;
  data: any;
}

// Inference result
export interface InferenceResult {
  success: boolean;
  text?: string;
  usage?: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
  error?: string;
  duration: number;
  timestamp: number;
  model: string;
  situation?: string;
}

/**
 * ModelManager - Manages model lifecycle and inference using GraphStore
 */
export class ModelManager {
  private store: GraphStore;
  private providerManager: ProviderManager;
  private modelEvents: ModelEvent[] = [];
  private eventCounter = 0;

  constructor(store: GraphStore, providerManager: ProviderManager) {
    this.store = store;
    this.providerManager = providerManager;
  }

  /**
   * Get the underlying GraphStore
   */
  getStore(): GraphStore {
    return this.store;
  }

  /**
   * Generate unique event ID
   */
  private generateEventId(): string {
    return `model_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit a model event
   */
  private async emitEvent(type: ModelEventType, modelId: string, data: any): Promise<void> {
    const event: ModelEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      modelId,
      data
    };

    this.modelEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'model_event', {
      eventType: type,
      modelId,
      ...data
    });
  }

  /**
   * Create a new inference model in draft state
   */
  async createModel(
    id: string,
    backendModel: string,
    providerId: string,
    options: {
      name?: string;
      temperature?: number;
      maxTokens?: number;
      topP?: number;
      situations?: Record<string, SituationParams>;
    } = {}
  ): Promise<Model> {
    // Check if model already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Model already exists: ${id}`);
    }

    // Verify provider exists
    const provider = this.providerManager.getProvider(providerId);
    if (!provider) {
      throw new Error(`Provider not found: ${providerId}`);
    }

    const config: InferenceModelConfig = {
      modelType: 'inference',
      name: options.name || id,
      backendModel,
      provider: `@(${providerId})`,
      temperature: options.temperature,
      maxTokens: options.maxTokens,
      topP: options.topP,
      situations: options.situations
    };

    const model: Model = {
      id,
      type: 'program',
      programType: 'model',
      lifecycle: 'draft',
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'model', {
      programType: 'model',
      modelType: 'inference',
      name: config.name,
      backendModel,
      provider: config.provider,
      temperature: options.temperature,
      maxTokens: options.maxTokens,
      topP: options.topP,
      situations: options.situations ? JSON.stringify(options.situations) : undefined,
      lifecycle: model.lifecycle,
      version: model.version
    });

    // Create edge to provider
    await this.store.addEdge(`${id}_uses_${providerId}`, id, providerId, 'uses_provider');

    await this.emitEvent('MODEL_CREATED', id, {
      name: config.name,
      modelType: 'inference',
      backendModel,
      provider: config.provider,
      lifecycle: model.lifecycle,
      version: model.version
    });

    return model;
  }

  /**
   * Create a new embedding model in draft state
   */
  async createEmbeddingModel(
    id: string,
    backendModel: string,
    providerId: string,
    dimensions: number,
    options: {
      name?: string;
    } = {}
  ): Promise<Model> {
    // Check if model already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Model already exists: ${id}`);
    }

    // Verify provider exists
    const provider = this.providerManager.getProvider(providerId);
    if (!provider) {
      throw new Error(`Provider not found: ${providerId}`);
    }

    const config: EmbeddingModelConfig = {
      modelType: 'embedding',
      name: options.name || id,
      backendModel,
      provider: `@(${providerId})`,
      dimensions
    };

    const model: Model = {
      id,
      type: 'program',
      programType: 'model',
      lifecycle: 'draft',
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'model', {
      programType: 'model',
      modelType: 'embedding',
      name: config.name,
      backendModel,
      provider: config.provider,
      dimensions,
      lifecycle: model.lifecycle,
      version: model.version
    });

    // Create edge to provider
    await this.store.addEdge(`${id}_uses_${providerId}`, id, providerId, 'uses_provider');

    await this.emitEvent('MODEL_CREATED', id, {
      name: config.name,
      modelType: 'embedding',
      backendModel,
      provider: config.provider,
      dimensions,
      lifecycle: model.lifecycle,
      version: model.version
    });

    return model;
  }

  /**
   * Get a model by ID
   */
  getModel(id: string): Model | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'model') {
      return null;
    }

    return this.nodeToModel(node);
  }

  /**
   * Convert a Node to a Model
   */
  private nodeToModel(node: Node): Model {
    const modelType = (node.properties.get('modelType') || 'inference') as ModelType;
    const situationsStr = node.properties.get('situations');

    // Handle legacy 'state' property for backwards compatibility
    const lifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ModelLifecycle;

    const baseConfig = {
      name: node.properties.get('name') || node.id,
      backendModel: node.properties.get('backendModel') || '',
      provider: node.properties.get('provider') || ''
    };

    let config: ModelConfig;
    if (modelType === 'embedding') {
      config = {
        ...baseConfig,
        modelType: 'embedding',
        dimensions: node.properties.get('dimensions') || 0
      } as EmbeddingModelConfig;
    } else {
      config = {
        ...baseConfig,
        modelType: 'inference',
        temperature: node.properties.get('temperature'),
        maxTokens: node.properties.get('maxTokens'),
        topP: node.properties.get('topP'),
        situations: situationsStr ? JSON.parse(situationsStr) : undefined
      } as InferenceModelConfig;
    }

    return {
      id: node.id,
      type: 'program',
      programType: 'model',
      lifecycle,
      config,
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Configure a model (only allowed in draft lifecycle)
   */
  async configureModel(
    id: string,
    updates: {
      name?: string;
      temperature?: number;
      maxTokens?: number;
      topP?: number;
      situation?: { name: string; params: SituationParams };
    }
  ): Promise<Model> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'model') {
      throw new Error(`Model not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ModelLifecycle;
    if (currentLifecycle !== 'draft') {
      throw new Error(`Cannot configure model in ${currentLifecycle} lifecycle. Only draft models can be configured.`);
    }

    const modelType = (node.properties.get('modelType') || 'inference') as ModelType;
    if (modelType === 'embedding') {
      // Embedding models don't support inference params
      if (updates.temperature !== undefined || updates.maxTokens !== undefined ||
          updates.topP !== undefined || updates.situation !== undefined) {
        throw new Error('Cannot configure inference parameters on embedding model');
      }
    }

    // Build properties to update
    const propsToUpdate: Record<string, any> = {};

    if (updates.name !== undefined) {
      propsToUpdate.name = updates.name;
    }
    if (updates.temperature !== undefined) {
      propsToUpdate.temperature = updates.temperature;
    }
    if (updates.maxTokens !== undefined) {
      propsToUpdate.maxTokens = updates.maxTokens;
    }
    if (updates.topP !== undefined) {
      propsToUpdate.topP = updates.topP;
    }

    // Handle situation updates
    if (updates.situation) {
      const situationsStr = node.properties.get('situations');
      const situations: Record<string, SituationParams> = situationsStr
        ? JSON.parse(situationsStr)
        : {};
      situations[updates.situation.name] = updates.situation.params;
      propsToUpdate.situations = JSON.stringify(situations);
    }

    // Increment version
    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    // Persist the update
    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('MODEL_CONFIGURED', id, {
      updates: Object.keys(updates),
      newVersion: currentVersion + 1
    });

    return this.getModel(id)!;
  }

  /**
   * Publish a model (transition from draft to published)
   */
  async publishModel(id: string): Promise<Model> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'model') {
      throw new Error(`Model not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ModelLifecycle;
    if (currentLifecycle !== 'draft') {
      throw new Error(`Cannot publish model in ${currentLifecycle} lifecycle. Only draft models can be published.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'published' });

    await this.emitEvent('MODEL_PUBLISHED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'published'
    });

    return this.getModel(id)!;
  }

  /**
   * Deprecate a model (transition from published to deprecated)
   */
  async deprecateModel(id: string): Promise<Model> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'model') {
      throw new Error(`Model not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ModelLifecycle;
    if (currentLifecycle !== 'published') {
      throw new Error(`Cannot deprecate model in ${currentLifecycle} lifecycle. Only published models can be deprecated.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'deprecated' });

    await this.emitEvent('MODEL_DEPRECATED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'deprecated'
    });

    return this.getModel(id)!;
  }

  /**
   * Extract provider ID from reference string
   */
  private extractProviderId(providerRef: string): string {
    // Handle @(provider-id) format
    const match = providerRef.match(/@\(([^)]+)\)/);
    return match ? match[1] : providerRef;
  }

  /**
   * Get effective params for a situation (inference models only)
   */
  private getEffectiveParams(model: Model, situation?: string): { temperature?: number; maxTokens?: number; topP?: number } {
    if (!isInferenceModel(model)) {
      return {};
    }

    const config = model.config as InferenceModelConfig;
    const baseParams = {
      temperature: config.temperature,
      maxTokens: config.maxTokens,
      topP: config.topP
    };

    if (situation && config.situations && config.situations[situation]) {
      const situationParams = config.situations[situation];
      return {
        ...baseParams,
        ...situationParams
      };
    }

    return baseParams;
  }

  /**
   * Prepare the AI model client for invocation
   * Returns the model configuration needed for generateText/streamText
   */
  private prepareModelClient(id: string, situation?: string): {
    aiModel: any;
    model: Model;
    effectiveParams: { temperature?: number; maxTokens?: number; topP?: number };
  } {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'model') {
      throw new Error(`Model not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ModelLifecycle;
    if (currentLifecycle !== 'published') {
      throw new Error(`Cannot invoke model in ${currentLifecycle} lifecycle. Only published models can be invoked.`);
    }

    const model = this.nodeToModel(node);

    // Only inference models can be invoked via this method
    if (!isInferenceModel(model)) {
      throw new Error(`Cannot invoke embedding model via invokeModel. Use embedWithModel instead.`);
    }

    const providerId = this.extractProviderId(model.config.provider);
    const provider = this.providerManager.getProvider(providerId);

    if (!provider) {
      throw new Error(`Provider not found: ${providerId}`);
    }

    if (provider.lifecycle !== 'published') {
      throw new Error(`Cannot use provider in ${provider.lifecycle} lifecycle. Provider must be published.`);
    }

    const effectiveParams = this.getEffectiveParams(model, situation);

    // Get credentials from context (support both /ai config and standard names)
    const ctx = getCurrentContext();
    const cfToken = ctx.getCredential('CLOUDFLARE_API_TOKEN') || ctx.getCredential('CF_AIG_TOKEN');
    if (!cfToken) {
      throw new Error('CLOUDFLARE_API_TOKEN or CF_AIG_TOKEN not found in context or environment');
    }

    // Cloudflare AI Gateway with Unified Billing:
    // - Uses ai-gateway-provider with unified provider
    // - Authenticates via apiKey (becomes cf-aig-authorization header)
    // - No provider API keys needed (gateway handles billing via credits)
    // - Model format: "provider/model-name"
    const aigateway = createAiGateway({
      accountId: provider.config.accountId,
      gateway: provider.config.gatewayId,
      apiKey: cfToken,
    });

    const unified = createUnified();

    // Determine provider prefix for unified billing model name
    let providerPrefix: string;
    if (model.config.backendModel.startsWith('claude') || model.config.backendModel.includes('anthropic')) {
      providerPrefix = 'anthropic';
    } else if (model.config.backendModel.startsWith('gpt') || model.config.backendModel.startsWith('o1') || model.config.backendModel.includes('openai')) {
      providerPrefix = 'openai';
    } else if (model.config.backendModel.includes('gemini') || model.config.backendModel.includes('google')) {
      providerPrefix = 'google-ai-studio';
    } else {
      providerPrefix = 'openai';
    }

    // Format model name for unified billing: "provider/model-name"
    const unifiedModelName = model.config.backendModel.includes('/')
      ? model.config.backendModel
      : `${providerPrefix}/${model.config.backendModel}`;

    const aiModel = aigateway(unified(unifiedModelName));

    return { aiModel, model, effectiveParams };
  }

  /**
   * Invoke a model for inference (only published models can be invoked)
   * Supports both batch (generateText) and streaming (streamText) modes.
   *
   * When stream=true, tokens are emitted via onToken callback and MODEL_TOKEN_RECEIVED events.
   *
   * Throws errors for validation failures (model not published, provider not published).
   * Returns error results for network/API failures.
   */
  async invokeModel(
    id: string,
    options: {
      message: string;
      system?: string;
      situation?: string;
    } & StreamingOptions
  ): Promise<InferenceResult> {
    // Validation errors should throw (not return error results) for backward compatibility
    // These are programming errors, not runtime errors
    const { aiModel, model, effectiveParams } = this.prepareModelClient(id, options.situation);

    const startTime = Date.now();

    try {

      const messages = [
        ...(options.system ? [{ role: 'system' as const, content: options.system }] : []),
        { role: 'user' as const, content: options.message }
      ];

      // Use streaming if requested
      if (options.stream) {
        const streamResult = streamText({
          model: aiModel,
          messages,
          temperature: effectiveParams.temperature,
          maxOutputTokens: effectiveParams.maxTokens,
          topP: effectiveParams.topP,
        });

        // Collect text and emit token events
        let fullText = '';
        for await (const textPart of streamResult.textStream) {
          fullText += textPart;

          // Call onToken callback if provided
          if (options.onToken) {
            options.onToken(textPart);
          }

          // Emit token event (don't await to avoid blocking the stream)
          this.emitEvent('MODEL_TOKEN_RECEIVED', id, {
            token: textPart,
            situation: options.situation
          });
        }

        // Get final usage info
        const usage = await streamResult.usage;

        const inferenceResult: InferenceResult = {
          success: true,
          text: fullText,
          usage: {
            promptTokens: usage?.inputTokens || 0,
            completionTokens: usage?.outputTokens || 0,
            totalTokens: (usage?.inputTokens || 0) + (usage?.outputTokens || 0)
          },
          duration: Date.now() - startTime,
          timestamp: Date.now(),
          model: model.config.backendModel,
          situation: options.situation
        };

        await this.emitEvent('MODEL_INFERENCE_COMPLETED', id, {
          success: true,
          duration: inferenceResult.duration,
          usage: inferenceResult.usage,
          situation: options.situation,
          streamed: true
        });

        return inferenceResult;
      }

      // Non-streaming (batch) mode with retry resilience
      const result = await withRateLimit(
        () => generateText({
          model: aiModel,
          messages,
          temperature: effectiveParams.temperature,
          maxOutputTokens: effectiveParams.maxTokens,
          topP: effectiveParams.topP,
        }),
        llmRateLimiter,
        3  // 3 retries with exponential backoff = 4 total attempts
      );

      const inferenceResult: InferenceResult = {
        success: true,
        text: result.text,
        usage: {
          promptTokens: result.usage?.inputTokens || 0,
          completionTokens: result.usage?.outputTokens || 0,
          totalTokens: (result.usage?.inputTokens || 0) + (result.usage?.outputTokens || 0)
        },
        duration: Date.now() - startTime,
        timestamp: Date.now(),
        model: model.config.backendModel,
        situation: options.situation
      };

      await this.emitEvent('MODEL_INFERENCE_COMPLETED', id, {
        success: true,
        duration: inferenceResult.duration,
        usage: inferenceResult.usage,
        situation: options.situation,
        streamed: false
      });

      return inferenceResult;

    } catch (error: any) {
      const inferenceResult: InferenceResult = {
        success: false,
        error: error.message,
        duration: Date.now() - startTime,
        timestamp: Date.now(),
        model: id,  // Use id since we may not have model info on error
        situation: options.situation
      };

      await this.emitEvent('MODEL_INFERENCE_COMPLETED', id, {
        success: false,
        error: error.message,
        duration: inferenceResult.duration,
        situation: options.situation
      });

      return inferenceResult;
    }
  }

  /**
   * List all models, optionally filtered by lifecycle or model type
   */
  listModels(filter?: { lifecycle?: ModelLifecycle; modelType?: ModelType }): Model[] {
    const modelNodes = this.store.getByType('model');
    let models = modelNodes.map(node => this.nodeToModel(node));

    if (filter) {
      if (filter.lifecycle) {
        models = models.filter(m => m.lifecycle === filter.lifecycle);
      }
      if (filter.modelType) {
        models = models.filter(m => m.config.modelType === filter.modelType);
      }
    }

    return models;
  }

  /**
   * Get all model events
   */
  getModelEvents(limit?: number): ModelEvent[] {
    const events = this.modelEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific model
   */
  getModelEventHistory(modelId: string): ModelEvent[] {
    return this.modelEvents.filter(e => e.modelId === modelId);
  }
}

export default ModelManager;
