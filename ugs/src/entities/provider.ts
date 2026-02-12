#!/usr/bin/env bun
/**
 * Provider Entity for Universal Graph System
 *
 * Providers route API calls to LLM services (Cloudflare AI Gateway).
 * State machine: draft -> published -> deprecated
 */

import GraphStore, { Node } from '../graph.ts';

// Provider lifecycle states (same as Program)
export type ProviderLifecycle = 'draft' | 'published' | 'deprecated';

// Provider types (currently only Cloudflare AI Gateway)
export type ProviderType = 'cloudflare-ai-gateway';

// Provider event types
export type ProviderEventType =
  | 'PROVIDER_CREATED'
  | 'PROVIDER_CONFIGURED'
  | 'PROVIDER_PUBLISHED'
  | 'PROVIDER_DEPRECATED';

// Provider config
export interface ProviderConfig {
  providerType: ProviderType;
  accountId: string;
  gatewayId: string;
}

// Provider interface
export interface Provider {
  id: string;
  type: 'program';
  programType: 'provider';
  lifecycle: ProviderLifecycle;
  config: ProviderConfig;
  version: number;
  created: number;
  modified: number;
}

// Provider event structure
export interface ProviderEvent {
  id: string;
  timestamp: number;
  type: ProviderEventType;
  providerId: string;
  data: any;
}

/**
 * ProviderManager - Manages provider lifecycle using GraphStore
 */
export class ProviderManager {
  private store: GraphStore;
  private providerEvents: ProviderEvent[] = [];
  private eventCounter = 0;

  constructor(store: GraphStore) {
    this.store = store;
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
    return `prov_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit a provider event
   */
  private async emitEvent(type: ProviderEventType, providerId: string, data: any): Promise<void> {
    const event: ProviderEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      providerId,
      data
    };

    this.providerEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'provider_event', {
      eventType: type,
      providerId,
      ...data
    });
  }

  /**
   * Create a new provider in draft lifecycle
   */
  async createProvider(
    id: string,
    providerType: ProviderType,
    options: {
      accountId?: string;
      gatewayId?: string;
    } = {}
  ): Promise<Provider> {
    // Check if provider already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Provider already exists: ${id}`);
    }

    // Get account and gateway from env if not provided
    const accountId = options.accountId || process.env.CLOUDFLARE_ACCOUNT_ID || '';
    const gatewayId = options.gatewayId || process.env.CLOUDFLARE_GATEWAY_ID || '';

    const config: ProviderConfig = {
      providerType,
      accountId,
      gatewayId
    };

    const provider: Provider = {
      id,
      type: 'program',
      programType: 'provider',
      lifecycle: 'draft',
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'provider', {
      programType: 'provider',
      providerType,
      accountId,
      gatewayId,
      lifecycle: provider.lifecycle,
      version: provider.version
    });

    await this.emitEvent('PROVIDER_CREATED', id, {
      providerType,
      lifecycle: provider.lifecycle,
      version: provider.version
    });

    return provider;
  }

  /**
   * Get a provider by ID
   */
  getProvider(id: string): Provider | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'provider') {
      return null;
    }

    return this.nodeToProvider(node);
  }

  /**
   * Convert a Node to a Provider
   */
  private nodeToProvider(node: Node): Provider {
    // Handle legacy 'state' property for backwards compatibility
    const lifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ProviderLifecycle;

    return {
      id: node.id,
      type: 'program',
      programType: 'provider',
      lifecycle,
      config: {
        providerType: (node.properties.get('providerType') || 'cloudflare-ai-gateway') as ProviderType,
        accountId: node.properties.get('accountId') || '',
        gatewayId: node.properties.get('gatewayId') || ''
      },
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Configure a provider (only allowed in draft lifecycle)
   */
  async configureProvider(
    id: string,
    updates: {
      accountId?: string;
      gatewayId?: string;
    }
  ): Promise<Provider> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'provider') {
      throw new Error(`Provider not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ProviderLifecycle;
    if (currentLifecycle !== 'draft') {
      throw new Error(`Cannot configure provider in ${currentLifecycle} lifecycle. Only draft providers can be configured.`);
    }

    // Build properties to update
    const propsToUpdate: Record<string, any> = {};

    if (updates.accountId !== undefined) {
      propsToUpdate.accountId = updates.accountId;
    }
    if (updates.gatewayId !== undefined) {
      propsToUpdate.gatewayId = updates.gatewayId;
    }

    // Increment version
    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    // Persist the update
    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('PROVIDER_CONFIGURED', id, {
      updates: Object.keys(updates),
      newVersion: currentVersion + 1
    });

    return this.getProvider(id)!;
  }

  /**
   * Publish a provider (transition from draft to published)
   */
  async publishProvider(id: string): Promise<Provider> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'provider') {
      throw new Error(`Provider not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ProviderLifecycle;
    if (currentLifecycle !== 'draft') {
      throw new Error(`Cannot publish provider in ${currentLifecycle} lifecycle. Only draft providers can be published.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'published' });

    await this.emitEvent('PROVIDER_PUBLISHED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'published'
    });

    return this.getProvider(id)!;
  }

  /**
   * Deprecate a provider (transition from published to deprecated)
   */
  async deprecateProvider(id: string): Promise<Provider> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'provider') {
      throw new Error(`Provider not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as ProviderLifecycle;
    if (currentLifecycle !== 'published') {
      throw new Error(`Cannot deprecate provider in ${currentLifecycle} lifecycle. Only published providers can be deprecated.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'deprecated' });

    await this.emitEvent('PROVIDER_DEPRECATED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'deprecated'
    });

    return this.getProvider(id)!;
  }

  /**
   * List all providers, optionally filtered by lifecycle
   */
  listProviders(lifecycle?: ProviderLifecycle): Provider[] {
    const providerNodes = this.store.getByType('provider');
    let providers = providerNodes.map(node => this.nodeToProvider(node));

    if (lifecycle) {
      providers = providers.filter(p => p.lifecycle === lifecycle);
    }

    return providers;
  }

  /**
   * Get all provider events
   */
  getProviderEvents(limit?: number): ProviderEvent[] {
    const events = this.providerEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific provider
   */
  getProviderEventHistory(providerId: string): ProviderEvent[] {
    return this.providerEvents.filter(e => e.providerId === providerId);
  }

  /**
   * Build the base URL for the Cloudflare AI Gateway
   */
  getProviderBaseUrl(provider: Provider): string {
    const { accountId, gatewayId } = provider.config;
    return `https://gateway.ai.cloudflare.com/v1/${accountId}/${gatewayId}`;
  }
}

export default ProviderManager;
