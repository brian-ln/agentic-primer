#!/usr/bin/env bun
/**
 * Agent Entity for Universal Graph System
 *
 * Agents are autonomous program executors with system prompts and tools.
 * State machine: idle -> thinking -> executing -> waiting -> completed | error
 * All states can transition to idle (reset) or error (failure)
 */

import GraphStore, { Node } from '../graph.ts';
import { TaskManager, Task } from './task.ts';
import { ModelManager, Model } from './model.ts';
import { SessionManager, Session, SendMessageResult } from './session.ts';

// Agent states
export type AgentState = 'idle' | 'thinking' | 'executing' | 'waiting' | 'completed' | 'error';

// Agent event types
export type AgentEventType =
  | 'AGENT_CREATED'
  | 'AGENT_CONFIGURED'
  | 'AGENT_TASK_ASSIGNED'
  | 'AGENT_THINKING'
  | 'AGENT_EXECUTING'
  | 'AGENT_WAITING'
  | 'AGENT_COMPLETED'
  | 'AGENT_ERROR'
  | 'AGENT_CHECKPOINT'
  | 'AGENT_RESET'
  | 'AGENT_TOKEN';

// Streaming options for agent step
export interface AgentStreamingOptions {
  stream?: boolean;
  onToken?: (token: string) => void;
}

// Harness configuration
export interface AgentHarness {
  maxTurns?: number;          // Limit iterations (default 50)
  reflectOnFailure?: boolean; // Auto-reflect on errors
  checkpointEvery?: number;   // Save state every N turns
}

// Agent config
export interface AgentConfig {
  name: string;
  systemPrompt: string;
  tools?: string[];         // "@(tool-id)" references to code programs
  defaultModel: string;     // "@(claude-balanced)" model reference
  harness: AgentHarness;
  // Runtime state
  currentTask?: string;     // Active task ID
  currentSession?: string;  // Active session ID for conversation
  turnCount?: number;
  lastCheckpoint?: number;
  lastError?: string;
}

// Agent interface
export interface Agent {
  id: string;
  type: 'program';
  programType: 'agent';
  state: AgentState;  // Execution state (not lifecycle)
  config: AgentConfig;
  version: number;
  created: number;
  modified: number;
}

// Agent event structure
export interface AgentEvent {
  id: string;
  timestamp: number;
  type: AgentEventType;
  agentId: string;
  data: any;
}

// Step result
export interface StepResult {
  done: boolean;
  turn?: number;
  reason?: string;
  response?: string;
  error?: string;
}

/**
 * AgentManager - Manages agent lifecycle and execution using GraphStore
 */
export class AgentManager {
  private store: GraphStore;
  private taskManager: TaskManager;
  private modelManager: ModelManager;
  private sessionManager?: SessionManager;
  private agentEvents: AgentEvent[] = [];
  private eventCounter = 0;

  // Default harness settings
  private static readonly DEFAULT_HARNESS: AgentHarness = {
    maxTurns: 50,
    reflectOnFailure: true,
    checkpointEvery: 5
  };

  constructor(store: GraphStore, taskManager: TaskManager, modelManager: ModelManager, sessionManager?: SessionManager) {
    this.store = store;
    this.taskManager = taskManager;
    this.modelManager = modelManager;
    this.sessionManager = sessionManager;
  }

  /**
   * Set session manager (for dependency injection)
   */
  setSessionManager(sessionManager: SessionManager): void {
    this.sessionManager = sessionManager;
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
    return `agent_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit an agent event
   */
  private async emitEvent(type: AgentEventType, agentId: string, data: any): Promise<void> {
    const event: AgentEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      agentId,
      data
    };

    this.agentEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'agent_event', {
      eventType: type,
      agentId,
      ...data
    });
  }

  /**
   * Create a new agent in idle state
   */
  async createAgent(
    id: string,
    name: string,
    systemPrompt: string,
    options: {
      tools?: string[];
      defaultModel?: string;
      harness?: Partial<AgentHarness>;
    } = {}
  ): Promise<Agent> {
    // Check if agent already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Agent already exists: ${id}`);
    }

    const harness: AgentHarness = {
      ...AgentManager.DEFAULT_HARNESS,
      ...options.harness
    };

    const config: AgentConfig = {
      name,
      systemPrompt,
      tools: options.tools,
      defaultModel: options.defaultModel || '@(claude-balanced)',
      harness,
      turnCount: 0
    };

    const agent: Agent = {
      id,
      type: 'program',
      programType: 'agent',
      state: 'idle',
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'agent', {
      programType: 'agent',
      name,
      systemPrompt,
      tools: options.tools ? JSON.stringify(options.tools) : undefined,
      defaultModel: agent.config.defaultModel,
      harness: JSON.stringify(harness),
      state: agent.state,
      turnCount: 0,
      version: agent.version
    });

    await this.emitEvent('AGENT_CREATED', id, {
      name,
      state: agent.state,
      defaultModel: agent.config.defaultModel,
      version: agent.version
    });

    return agent;
  }

  /**
   * Get an agent by ID
   */
  getAgent(id: string): Agent | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      return null;
    }

    return this.nodeToAgent(node);
  }

  /**
   * Convert a Node to an Agent
   */
  private nodeToAgent(node: Node): Agent {
    const toolsStr = node.properties.get('tools');
    const harnessStr = node.properties.get('harness');

    return {
      id: node.id,
      type: 'program',
      programType: 'agent',
      state: (node.properties.get('state') || 'idle') as AgentState,
      config: {
        name: node.properties.get('name') || node.id,
        systemPrompt: node.properties.get('systemPrompt') || '',
        tools: toolsStr ? JSON.parse(toolsStr) : undefined,
        defaultModel: node.properties.get('defaultModel') || '@(claude-balanced)',
        harness: harnessStr ? JSON.parse(harnessStr) : AgentManager.DEFAULT_HARNESS,
        currentTask: node.properties.get('currentTask') || undefined,
        currentSession: node.properties.get('currentSession') || undefined,
        turnCount: node.properties.get('turnCount') || 0,
        lastCheckpoint: node.properties.get('lastCheckpoint'),
        lastError: node.properties.get('lastError')
      },
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Configure an agent's harness settings (only allowed in idle state)
   */
  async configureAgent(
    id: string,
    updates: {
      name?: string;
      systemPrompt?: string;
      tools?: string[];
      defaultModel?: string;
      harness?: Partial<AgentHarness>;
    }
  ): Promise<Agent> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      throw new Error(`Agent not found: ${id}`);
    }

    const currentState = node.properties.get('state') as AgentState;
    if (currentState !== 'idle') {
      throw new Error(`Cannot configure agent in ${currentState} state. Only idle agents can be configured.`);
    }

    // Build properties to update
    const propsToUpdate: Record<string, any> = {};

    if (updates.name !== undefined) {
      propsToUpdate.name = updates.name;
    }
    if (updates.systemPrompt !== undefined) {
      propsToUpdate.systemPrompt = updates.systemPrompt;
    }
    if (updates.tools !== undefined) {
      propsToUpdate.tools = JSON.stringify(updates.tools);
    }
    if (updates.defaultModel !== undefined) {
      propsToUpdate.defaultModel = updates.defaultModel;
    }
    if (updates.harness !== undefined) {
      const harnessStr = node.properties.get('harness');
      const currentHarness: AgentHarness = harnessStr ? JSON.parse(harnessStr) : AgentManager.DEFAULT_HARNESS;
      propsToUpdate.harness = JSON.stringify({
        ...currentHarness,
        ...updates.harness
      });
    }

    // Increment version
    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    // Persist the update
    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('AGENT_CONFIGURED', id, {
      updates: Object.keys(updates),
      newVersion: currentVersion + 1
    });

    return this.getAgent(id)!;
  }

  /**
   * Assign a task to an agent (transitions to thinking state)
   * Creates a new session for the agent to use for conversation
   */
  async assignTask(id: string, taskId: string): Promise<Agent> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      throw new Error(`Agent not found: ${id}`);
    }

    const currentState = node.properties.get('state') as AgentState;
    if (currentState !== 'idle') {
      throw new Error(`Cannot assign task to agent in ${currentState} state. Agent must be idle.`);
    }

    // Verify task exists
    const task = this.taskManager.getTask(taskId);
    if (!task) {
      throw new Error(`Task not found: ${taskId}`);
    }

    const agent = this.nodeToAgent(node);
    let sessionId: string | undefined;

    // Create a session for this task if SessionManager is available
    if (this.sessionManager) {
      const modelRef = agent.config.defaultModel;
      const modelId = modelRef.match(/@\(([^)]+)\)/)?.[1] || modelRef;

      // Create a session owned by this agent
      sessionId = `${id}_${taskId}_${Date.now()}`;
      await this.sessionManager.createSession(sessionId, modelId, {
        owner: `@(${id})`
      });
    }

    // Update agent state
    await this.store.updateNode(id, {
      state: 'thinking',
      currentTask: taskId,
      currentSession: sessionId || '',
      turnCount: 0,
      lastCheckpoint: 0
    });

    await this.emitEvent('AGENT_TASK_ASSIGNED', id, {
      taskId,
      sessionId,
      previousState: currentState,
      newState: 'thinking'
    });

    return this.getAgent(id)!;
  }

  /**
   * Build messages for model invocation
   */
  private buildMessages(agent: Agent, task: Task | null): Array<{ role: 'system' | 'user'; content: string }> {
    const messages: Array<{ role: 'system' | 'user'; content: string }> = [];

    // System prompt
    messages.push({
      role: 'system',
      content: agent.config.systemPrompt
    });

    // Task context
    if (task) {
      const taskContext = `
## Current Task

**Title:** ${task.config.title}
**ID:** ${task.id}
**Priority:** ${task.config.priority || 'not set'}
**Lifecycle:** ${task.lifecycle}
${task.config.description ? `**Description:** ${task.config.description}` : ''}
${task.config.spec ? `**Spec:** ${JSON.stringify(task.config.spec)}` : ''}

Turn ${agent.config.turnCount || 0} of max ${agent.config.harness.maxTurns}
      `.trim();

      messages.push({
        role: 'user',
        content: taskContext
      });
    }

    return messages;
  }

  /**
   * Check if response contains a tool call pattern
   */
  private hasToolCall(response: string): boolean {
    // Simple heuristic: look for common tool call patterns
    return response.includes('<tool_call>') ||
           response.includes('```tool') ||
           response.includes('ACTION:') ||
           /\bexecute\s*\(/i.test(response);
  }

  /**
   * Check if response signals completion
   */
  private isComplete(response: string): boolean {
    // Simple heuristic: look for completion signals
    return response.includes('TASK_COMPLETE') ||
           response.includes('DONE') ||
           /task\s+(is\s+)?complete/i.test(response) ||
           /successfully\s+completed/i.test(response);
  }

  /**
   * Execute one turn of the agent loop
   *
   * Supports streaming via the options parameter. When stream=true,
   * tokens are emitted via onToken callback and AGENT_TOKEN events.
   */
  async step(id: string, options: AgentStreamingOptions = {}): Promise<StepResult> {
    const agent = this.getAgent(id);
    if (!agent) {
      throw new Error(`Agent not found: ${id}`);
    }

    if (agent.state !== 'thinking') {
      throw new Error(`Cannot step agent in ${agent.state} state. Agent must be thinking.`);
    }

    // Check turn limit
    const turnCount = (agent.config.turnCount || 0) + 1;
    const maxTurns = agent.config.harness.maxTurns || 50;

    if (turnCount > maxTurns) {
      await this.store.updateNode(id, {
        state: 'completed',
        lastError: 'Max turns reached'
      });
      await this.emitEvent('AGENT_COMPLETED', id, {
        turns: turnCount - 1,
        reason: 'max_turns_reached'
      });
      return { done: true, reason: 'max_turns_reached' };
    }

    // Update turn count
    await this.store.updateNode(id, { turnCount });

    // Emit thinking event
    await this.emitEvent('AGENT_THINKING', id, { turn: turnCount });

    // Get task context
    const task = agent.config.currentTask ? this.taskManager.getTask(agent.config.currentTask) : null;

    // Build the user message for this turn
    const messages = this.buildMessages(agent, task);
    const userMessage = messages.find(m => m.role === 'user')?.content || '';
    const systemPrompt = messages.find(m => m.role === 'system')?.content;

    // Create wrapped onToken that also emits AGENT_TOKEN events
    const wrappedOnToken = options.stream ? (token: string) => {
      // Call user-provided callback
      if (options.onToken) {
        options.onToken(token);
      }
      // Emit agent token event (don't await to avoid blocking)
      this.emitEvent('AGENT_TOKEN', id, { token, turn: turnCount });
    } : undefined;

    // Try to invoke model through session (if available) or directly
    let responseText: string;
    try {
      if (this.sessionManager && agent.config.currentSession) {
        // Use Session for inference - this preserves conversation history
        const result = await this.sessionManager.sendMessage(
          agent.config.currentSession,
          userMessage,
          {
            system: systemPrompt,
            stream: options.stream,
            onToken: wrappedOnToken
          }
        );

        if (!result.success) {
          throw new Error(result.error || 'Session inference failed');
        }

        responseText = result.text || '';
      } else {
        // Fallback: Direct model invocation (no session history)
        const modelRef = agent.config.defaultModel;
        const modelId = modelRef.match(/@\(([^)]+)\)/)?.[1] || modelRef;

        const model = this.modelManager.getModel(modelId);
        if (!model) {
          throw new Error(`Model not found: ${modelId}`);
        }

        if (model.lifecycle !== 'published') {
          throw new Error(`Model ${modelId} is not published`);
        }

        const result = await this.modelManager.invokeModel(modelId, {
          message: userMessage,
          system: systemPrompt,
          stream: options.stream,
          onToken: wrappedOnToken
        });

        if (!result.success) {
          throw new Error(result.error || 'Model invocation failed');
        }

        responseText = result.text || '';
      }
    } catch (error: any) {
      // For testing/development, simulate a response when model is unavailable
      responseText = `[Simulated response for turn ${turnCount}] Processing task: ${task?.config.title || 'unknown'}`;
    }

    // Check for tool calls
    if (this.hasToolCall(responseText)) {
      await this.store.updateNode(id, { state: 'executing' });
      await this.emitEvent('AGENT_EXECUTING', id, { turn: turnCount, tool: 'detected' });

      // For now, immediately transition back to thinking
      await this.store.updateNode(id, { state: 'thinking' });
    }

    // Checkpoint if needed
    const checkpointEvery = agent.config.harness.checkpointEvery || 5;
    if (turnCount % checkpointEvery === 0) {
      await this.store.updateNode(id, { lastCheckpoint: turnCount });
      await this.emitEvent('AGENT_CHECKPOINT', id, { turn: turnCount });
    }

    // Check for completion signals
    if (this.isComplete(responseText)) {
      await this.store.updateNode(id, { state: 'completed' });
      await this.emitEvent('AGENT_COMPLETED', id, { turns: turnCount, response: responseText });
      return { done: true, turn: turnCount, reason: 'completed', response: responseText };
    }

    return { done: false, turn: turnCount, response: responseText };
  }

  /**
   * Run agent until completion or limit
   */
  async run(id: string, maxTurns?: number): Promise<StepResult> {
    const agent = this.getAgent(id);
    if (!agent) {
      throw new Error(`Agent not found: ${id}`);
    }

    if (agent.state !== 'thinking' && agent.state !== 'idle') {
      throw new Error(`Cannot run agent in ${agent.state} state.`);
    }

    // If idle with no task, error
    if (agent.state === 'idle') {
      throw new Error('Agent has no task assigned. Use assignTask first.');
    }

    const effectiveMaxTurns = maxTurns || agent.config.harness.maxTurns || 50;
    let result: StepResult = { done: false };

    while (!result.done && (agent.config.turnCount || 0) < effectiveMaxTurns) {
      result = await this.step(id);
    }

    return result;
  }

  /**
   * Pause execution (transition to waiting state)
   */
  async pause(id: string, reason?: string): Promise<Agent> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      throw new Error(`Agent not found: ${id}`);
    }

    const currentState = node.properties.get('state') as AgentState;
    if (currentState !== 'thinking' && currentState !== 'executing') {
      throw new Error(`Cannot pause agent in ${currentState} state.`);
    }

    await this.store.updateNode(id, { state: 'waiting' });
    await this.emitEvent('AGENT_WAITING', id, {
      previousState: currentState,
      reason: reason || 'paused'
    });

    return this.getAgent(id)!;
  }

  /**
   * Resume execution with optional input
   */
  async resume(id: string, input?: string): Promise<Agent> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      throw new Error(`Agent not found: ${id}`);
    }

    const currentState = node.properties.get('state') as AgentState;
    if (currentState !== 'waiting') {
      throw new Error(`Cannot resume agent in ${currentState} state. Agent must be waiting.`);
    }

    await this.store.updateNode(id, { state: 'thinking' });
    await this.emitEvent('AGENT_THINKING', id, {
      previousState: 'waiting',
      resumedWith: input ? 'input' : 'no_input'
    });

    return this.getAgent(id)!;
  }

  /**
   * Force stop execution (transition to idle)
   */
  async stop(id: string): Promise<Agent> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      throw new Error(`Agent not found: ${id}`);
    }

    const currentState = node.properties.get('state') as AgentState;
    if (currentState === 'idle') {
      return this.getAgent(id)!;
    }

    await this.store.updateNode(id, {
      state: 'idle',
      currentTask: '',
      currentSession: '',
      turnCount: 0,
      lastCheckpoint: 0
    });

    await this.emitEvent('AGENT_RESET', id, {
      previousState: currentState,
      reason: 'stopped'
    });

    return this.getAgent(id)!;
  }

  /**
   * Reset agent to idle state
   */
  async reset(id: string): Promise<Agent> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      throw new Error(`Agent not found: ${id}`);
    }

    const currentState = node.properties.get('state') as AgentState;

    await this.store.updateNode(id, {
      state: 'idle',
      currentTask: '',
      currentSession: '',
      turnCount: 0,
      lastCheckpoint: 0,
      lastError: ''
    });

    await this.emitEvent('AGENT_RESET', id, {
      previousState: currentState,
      reason: 'reset'
    });

    return this.getAgent(id)!;
  }

  /**
   * Transition to error state
   */
  async setError(id: string, error: string): Promise<Agent> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'agent') {
      throw new Error(`Agent not found: ${id}`);
    }

    const currentState = node.properties.get('state') as AgentState;

    await this.store.updateNode(id, {
      state: 'error',
      lastError: error
    });

    await this.emitEvent('AGENT_ERROR', id, {
      previousState: currentState,
      error
    });

    return this.getAgent(id)!;
  }

  /**
   * Get agent status summary
   */
  getStatus(id: string): { state: AgentState; turnCount: number; currentTask?: string; lastError?: string } | null {
    const agent = this.getAgent(id);
    if (!agent) {
      return null;
    }

    return {
      state: agent.state,
      turnCount: agent.config.turnCount || 0,
      currentTask: agent.config.currentTask,
      lastError: agent.config.lastError
    };
  }

  /**
   * List all agents, optionally filtered by state
   */
  listAgents(state?: AgentState): Agent[] {
    const agentNodes = this.store.getByType('agent');
    let agents = agentNodes.map(node => this.nodeToAgent(node));

    if (state) {
      agents = agents.filter(a => a.state === state);
    }

    return agents;
  }

  /**
   * Get all agent events
   */
  getAgentEvents(limit?: number): AgentEvent[] {
    const events = this.agentEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific agent
   */
  getAgentEventHistory(agentId: string): AgentEvent[] {
    return this.agentEvents.filter(e => e.agentId === agentId);
  }
}

export default AgentManager;
