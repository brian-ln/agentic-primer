#!/usr/bin/env bun
/**
 * Information Entity for Universal Graph System
 *
 * Information nodes store structured knowledge that can be validated and queried.
 * State machine: draft -> validated -> active -> archived
 *                        ^                    |
 *                        |-----(reactivate)---|
 */

import GraphStore, { Node } from '../graph.ts';

// Information lifecycle states
export type InformationLifecycle = 'draft' | 'validated' | 'active' | 'archived';

// Information types
export type InformationType = 'fact' | 'schema' | 'workflow' | 'pattern';

// Information event types
export type InformationEventType =
  | 'INFORMATION_CREATED'
  | 'INFORMATION_UPDATED'
  | 'INFORMATION_VALIDATED'
  | 'INFORMATION_ACTIVATED'
  | 'INFORMATION_ARCHIVED';

// Information config
export interface InformationConfig {
  infoType: InformationType;
  content: any;  // The actual information content
  schema?: string;  // "@(schema-id)" for validation
  sources?: string[];  // "@(task-id)", "@(info-id)", URLs
  tags?: string[];
  description?: string;
}

// Information interface
export interface Information {
  id: string;
  type: 'program';
  programType: 'information';
  lifecycle: InformationLifecycle;
  config: InformationConfig;
  version: number;
  created: number;
  modified: number;
}

// Information event structure
export interface InformationEvent {
  id: string;
  timestamp: number;
  type: InformationEventType;
  informationId: string;
  data: any;
}

/**
 * InformationManager - Manages information lifecycle using GraphStore
 */
export class InformationManager {
  private store: GraphStore;
  private informationEvents: InformationEvent[] = [];
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
    return `info_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit an information event
   */
  private async emitEvent(type: InformationEventType, informationId: string, data: any): Promise<void> {
    const event: InformationEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      informationId,
      data
    };

    this.informationEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'information_event', {
      eventType: type,
      informationId,
      ...data
    });
  }

  /**
   * Create new information in draft lifecycle
   */
  async createInformation(
    id: string,
    infoType: InformationType,
    content: any,
    options: {
      schema?: string;
      sources?: string[];
      tags?: string[];
      description?: string;
    } = {}
  ): Promise<Information> {
    // Check if information already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Information already exists: ${id}`);
    }

    const config: InformationConfig = {
      infoType,
      content,
      schema: options.schema,
      sources: options.sources,
      tags: options.tags,
      description: options.description
    };

    const information: Information = {
      id,
      type: 'program',
      programType: 'information',
      lifecycle: 'draft',
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'information', {
      programType: 'information',
      infoType,
      content: JSON.stringify(content),
      schema: options.schema,
      sources: options.sources ? JSON.stringify(options.sources) : undefined,
      tags: options.tags ? JSON.stringify(options.tags) : undefined,
      description: options.description,
      lifecycle: information.lifecycle,
      version: information.version
    });

    await this.emitEvent('INFORMATION_CREATED', id, {
      infoType,
      lifecycle: information.lifecycle,
      version: information.version,
      tags: options.tags
    });

    return information;
  }

  /**
   * Get information by ID
   */
  getInformation(id: string): Information | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'information') {
      return null;
    }

    return this.nodeToInformation(node);
  }

  /**
   * Convert a Node to an Information
   */
  private nodeToInformation(node: Node): Information {
    const contentStr = node.properties.get('content');
    const sourcesStr = node.properties.get('sources');
    const tagsStr = node.properties.get('tags');
    // Handle legacy 'state' property for backwards compatibility
    const lifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as InformationLifecycle;

    return {
      id: node.id,
      type: 'program',
      programType: 'information',
      lifecycle,
      config: {
        infoType: (node.properties.get('infoType') || 'fact') as InformationType,
        content: contentStr ? JSON.parse(contentStr) : null,
        schema: node.properties.get('schema'),
        sources: sourcesStr ? JSON.parse(sourcesStr) : undefined,
        tags: tagsStr ? JSON.parse(tagsStr) : undefined,
        description: node.properties.get('description')
      },
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Update information (only allowed in draft lifecycle)
   */
  async updateInformation(
    id: string,
    updates: {
      content?: any;
      schema?: string;
      sources?: string[];
      tags?: string[];
      description?: string;
    }
  ): Promise<Information> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'information') {
      throw new Error(`Information not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as InformationLifecycle;
    if (currentLifecycle !== 'draft') {
      throw new Error(`Cannot update information in ${currentLifecycle} lifecycle. Only draft information can be updated.`);
    }

    // Build properties to update
    const propsToUpdate: Record<string, any> = {};

    if (updates.content !== undefined) {
      propsToUpdate.content = JSON.stringify(updates.content);
    }
    if (updates.schema !== undefined) {
      propsToUpdate.schema = updates.schema;
    }
    if (updates.sources !== undefined) {
      propsToUpdate.sources = JSON.stringify(updates.sources);
    }
    if (updates.tags !== undefined) {
      propsToUpdate.tags = JSON.stringify(updates.tags);
    }
    if (updates.description !== undefined) {
      propsToUpdate.description = updates.description;
    }

    // Increment version
    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    // Persist the update
    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('INFORMATION_UPDATED', id, {
      updates: Object.keys(updates),
      newVersion: currentVersion + 1
    });

    return this.getInformation(id)!;
  }

  /**
   * Validate information (transition from draft to validated)
   */
  async validateInformation(id: string): Promise<Information> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'information') {
      throw new Error(`Information not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as InformationLifecycle;
    if (currentLifecycle !== 'draft') {
      throw new Error(`Cannot validate information in ${currentLifecycle} lifecycle. Only draft information can be validated.`);
    }

    // TODO: If schema is set, validate content against schema
    // For now, just transition the lifecycle

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'validated' });

    await this.emitEvent('INFORMATION_VALIDATED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'validated'
    });

    return this.getInformation(id)!;
  }

  /**
   * Activate information (transition from validated to active)
   */
  async activateInformation(id: string): Promise<Information> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'information') {
      throw new Error(`Information not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as InformationLifecycle;
    if (currentLifecycle !== 'validated' && currentLifecycle !== 'archived') {
      throw new Error(`Cannot activate information in ${currentLifecycle} lifecycle. Only validated or archived information can be activated.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'active' });

    await this.emitEvent('INFORMATION_ACTIVATED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'active'
    });

    return this.getInformation(id)!;
  }

  /**
   * Archive information (transition from active to archived)
   */
  async archiveInformation(id: string): Promise<Information> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'information') {
      throw new Error(`Information not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'draft') as InformationLifecycle;
    if (currentLifecycle !== 'active') {
      throw new Error(`Cannot archive information in ${currentLifecycle} lifecycle. Only active information can be archived.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'archived' });

    await this.emitEvent('INFORMATION_ARCHIVED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'archived'
    });

    return this.getInformation(id)!;
  }

  /**
   * List all information, optionally filtered by lifecycle
   */
  listInformation(lifecycle?: InformationLifecycle): Information[] {
    const infoNodes = this.store.getByType('information');
    let informations = infoNodes.map(node => this.nodeToInformation(node));

    if (lifecycle) {
      informations = informations.filter(i => i.lifecycle === lifecycle);
    }

    return informations;
  }

  /**
   * Query information by type
   */
  queryByType(infoType: InformationType): Information[] {
    const allInfo = this.listInformation();
    return allInfo.filter(i => i.config.infoType === infoType);
  }

  /**
   * Query information by tag
   */
  queryByTag(tag: string): Information[] {
    const allInfo = this.listInformation();
    return allInfo.filter(i => i.config.tags && i.config.tags.includes(tag));
  }

  /**
   * Get all information events
   */
  getInformationEvents(limit?: number): InformationEvent[] {
    const events = this.informationEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific information
   */
  getInformationEventHistory(informationId: string): InformationEvent[] {
    return this.informationEvents.filter(e => e.informationId === informationId);
  }
}

export default InformationManager;
