#!/usr/bin/env bun
/**
 * Program Entity for Universal Graph System
 *
 * Programs are executable units that can be invoked with inputs and produce outputs.
 * State machine: draft -> published -> deprecated
 */

import GraphStore, { Node } from '../graph.ts';

// Program states
export type ProgramState = 'draft' | 'published' | 'deprecated';

// Program event types
export type ProgramEventType =
  | 'PROGRAM_CREATED'
  | 'PROGRAM_UPDATED'
  | 'PROGRAM_PUBLISHED'
  | 'PROGRAM_INVOKED'
  | 'PROGRAM_DEPRECATED';

// Program interface
export interface Program {
  id: string;
  name: string;
  impl: string;  // The implementation code (JavaScript/TypeScript)
  state: ProgramState;
  inputSchema?: Record<string, any>;  // JSON Schema for input validation
  outputSchema?: Record<string, any>; // JSON Schema for output validation
  description?: string;
  version: number;
  created: number;
  modified: number;
}

// Program event structure
export interface ProgramEvent {
  id: string;
  timestamp: number;
  type: ProgramEventType;
  programId: string;
  data: any;
}

// Invocation result
export interface InvocationResult {
  success: boolean;
  output?: any;
  error?: string;
  duration: number;
  timestamp: number;
}

/**
 * ProgramManager - Manages program lifecycle using GraphStore
 */
export class ProgramManager {
  private store: GraphStore;
  private programEvents: ProgramEvent[] = [];
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
    return `prog_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit a program event
   */
  private async emitEvent(type: ProgramEventType, programId: string, data: any): Promise<void> {
    const event: ProgramEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      programId,
      data
    };

    this.programEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'program_event', {
      eventType: type,
      programId,
      ...data
    });
  }

  /**
   * Create a new program in draft state
   */
  async createProgram(
    id: string,
    impl: string,
    options: {
      name?: string;
      inputSchema?: Record<string, any>;
      outputSchema?: Record<string, any>;
      description?: string;
    } = {}
  ): Promise<Program> {
    // Check if program already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Program already exists: ${id}`);
    }

    const program: Program = {
      id,
      name: options.name || id,
      impl,
      state: 'draft',
      inputSchema: options.inputSchema,
      outputSchema: options.outputSchema,
      description: options.description,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'program', {
      name: program.name,
      impl: program.impl,
      state: program.state,
      inputSchema: program.inputSchema ? JSON.stringify(program.inputSchema) : undefined,
      outputSchema: program.outputSchema ? JSON.stringify(program.outputSchema) : undefined,
      description: program.description,
      version: program.version
    });

    await this.emitEvent('PROGRAM_CREATED', id, {
      name: program.name,
      state: program.state,
      version: program.version
    });

    return program;
  }

  /**
   * Get a program by ID
   */
  getProgram(id: string): Program | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'program') {
      return null;
    }

    return this.nodeToProgram(node);
  }

  /**
   * Convert a Node to a Program
   */
  private nodeToProgram(node: Node): Program {
    const inputSchemaStr = node.properties.get('inputSchema');
    const outputSchemaStr = node.properties.get('outputSchema');

    return {
      id: node.id,
      name: node.properties.get('name') || node.id,
      impl: node.properties.get('impl') || '',
      state: (node.properties.get('state') || 'draft') as ProgramState,
      inputSchema: inputSchemaStr ? JSON.parse(inputSchemaStr) : undefined,
      outputSchema: outputSchemaStr ? JSON.parse(outputSchemaStr) : undefined,
      description: node.properties.get('description'),
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Update a program (only allowed in draft state)
   */
  async updateProgram(
    id: string,
    updates: {
      impl?: string;
      name?: string;
      inputSchema?: Record<string, any>;
      outputSchema?: Record<string, any>;
      description?: string;
    }
  ): Promise<Program> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'program') {
      throw new Error(`Program not found: ${id}`);
    }

    const currentState = node.properties.get('state') as ProgramState;
    if (currentState !== 'draft') {
      throw new Error(`Cannot update program in ${currentState} state. Only draft programs can be updated.`);
    }

    // Build properties to update
    const propsToUpdate: Record<string, any> = {};

    if (updates.impl !== undefined) {
      propsToUpdate.impl = updates.impl;
    }
    if (updates.name !== undefined) {
      propsToUpdate.name = updates.name;
    }
    if (updates.inputSchema !== undefined) {
      propsToUpdate.inputSchema = JSON.stringify(updates.inputSchema);
    }
    if (updates.outputSchema !== undefined) {
      propsToUpdate.outputSchema = JSON.stringify(updates.outputSchema);
    }
    if (updates.description !== undefined) {
      propsToUpdate.description = updates.description;
    }

    // Increment version
    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    // Persist the update
    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('PROGRAM_UPDATED', id, {
      updates: Object.keys(updates),
      newVersion: currentVersion + 1
    });

    return this.getProgram(id)!;
  }

  /**
   * Publish a program (transition from draft to published)
   */
  async publishProgram(id: string): Promise<Program> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'program') {
      throw new Error(`Program not found: ${id}`);
    }

    const currentState = node.properties.get('state') as ProgramState;
    if (currentState !== 'draft') {
      throw new Error(`Cannot publish program in ${currentState} state. Only draft programs can be published.`);
    }

    // Persist the state change
    await this.store.updateNode(id, { state: 'published' });

    await this.emitEvent('PROGRAM_PUBLISHED', id, {
      previousState: currentState,
      newState: 'published'
    });

    return this.getProgram(id)!;
  }

  /**
   * Deprecate a program (transition from published to deprecated)
   */
  async deprecateProgram(id: string): Promise<Program> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'program') {
      throw new Error(`Program not found: ${id}`);
    }

    const currentState = node.properties.get('state') as ProgramState;
    if (currentState !== 'published') {
      throw new Error(`Cannot deprecate program in ${currentState} state. Only published programs can be deprecated.`);
    }

    // Persist the state change
    await this.store.updateNode(id, { state: 'deprecated' });

    await this.emitEvent('PROGRAM_DEPRECATED', id, {
      previousState: currentState,
      newState: 'deprecated'
    });

    return this.getProgram(id)!;
  }

  /**
   * Invoke a program (only published programs can be invoked)
   */
  async invokeProgram(id: string, input?: any): Promise<InvocationResult> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'program') {
      throw new Error(`Program not found: ${id}`);
    }

    const currentState = node.properties.get('state') as ProgramState;
    if (currentState !== 'published') {
      throw new Error(`Cannot invoke program in ${currentState} state. Only published programs can be invoked.`);
    }

    const impl = node.properties.get('impl') as string;
    const startTime = Date.now();
    let result: InvocationResult;

    try {
      // Create a sandboxed execution context
      const fn = new Function('input', impl);
      const output = fn(input);

      result = {
        success: true,
        output,
        duration: Date.now() - startTime,
        timestamp: Date.now()
      };
    } catch (error: any) {
      result = {
        success: false,
        error: error.message,
        duration: Date.now() - startTime,
        timestamp: Date.now()
      };
    }

    await this.emitEvent('PROGRAM_INVOKED', id, {
      input: input !== undefined ? JSON.stringify(input) : undefined,
      success: result.success,
      duration: result.duration,
      error: result.error
    });

    return result;
  }

  /**
   * List all programs, optionally filtered by state
   */
  listPrograms(state?: ProgramState): Program[] {
    const programNodes = this.store.getByType('program');
    let programs = programNodes.map(node => this.nodeToProgram(node));

    if (state) {
      programs = programs.filter(p => p.state === state);
    }

    return programs;
  }

  /**
   * Get all program events
   */
  getProgramEvents(limit?: number): ProgramEvent[] {
    const events = this.programEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific program
   */
  getProgramEventHistory(programId: string): ProgramEvent[] {
    return this.programEvents.filter(e => e.programId === programId);
  }
}

export default ProgramManager;
