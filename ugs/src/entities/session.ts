#!/usr/bin/env bun
/**
 * Session Entity for Universal Graph System
 *
 * Sessions are conversation containers that log messages to JSONL files.
 * They integrate with the Model entity for inference.
 * State machine: created -> active -> paused | completed
 *                                 paused -> active
 */

import GraphStore, { Node } from '../graph.ts';
import { ModelManager, InferenceResult } from './model.ts';
import { writeFile, readFile, mkdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';

// Session lifecycle states
export type SessionLifecycle = 'created' | 'active' | 'paused' | 'completed';

// Session event types
export type SessionEventType =
  | 'SESSION_CREATED'
  | 'SESSION_MESSAGE'
  | 'SESSION_PAUSED'
  | 'SESSION_RESUMED'
  | 'SESSION_COMPLETED';

// Session config
export interface SessionConfig {
  owner?: string;        // "@(agent-id)" or "@(human-id)"
  defaultModel: string;  // "@(claude-balanced)"
  logFile: string;       // "sessions/abc123.jsonl"
}

// Session interface
export interface Session {
  id: string;
  type: 'program';
  programType: 'session';
  lifecycle: SessionLifecycle;
  config: SessionConfig;
  version: number;
  created: number;
  modified: number;
}

// Session event structure
export interface SessionEvent {
  id: string;
  timestamp: number;
  type: SessionEventType;
  sessionId: string;
  data: any;
}

// JSONL log entry format (per Claude Code pattern)
export interface SessionLogEntry {
  uuid: string;
  parentUuid?: string;
  sessionId: string;
  timestamp: string;
  message: {
    role: 'user' | 'assistant';
    content: string;
    model?: string;           // Which model generated (assistant only)
    usage?: {
      inputTokens: number;
      outputTokens: number;
    };
  };
  requestId?: string;
  provider?: string;
  latencyMs?: number;
  situation?: string;
}

// Message send options
export interface SendMessageOptions {
  model?: string;      // Override default model for this message
  situation?: string;  // Situational params
  system?: string;     // System prompt
  stream?: boolean;    // Enable streaming mode
  onToken?: (token: string) => void;  // Callback for streaming tokens
}

// Message send result
export interface SendMessageResult {
  success: boolean;
  text?: string;
  usage?: {
    inputTokens: number;
    outputTokens: number;
  };
  error?: string;
  duration: number;
  model: string;
  situation?: string;
  userEntry: SessionLogEntry;
  assistantEntry?: SessionLogEntry;
}

/**
 * SessionManager - Manages session lifecycle and message logging
 */
export class SessionManager {
  private store: GraphStore;
  private modelManager: ModelManager;
  private sessionEvents: SessionEvent[] = [];
  private eventCounter = 0;
  private sessionsDir: string;

  constructor(store: GraphStore, modelManager: ModelManager, dataDir: string = './data') {
    this.store = store;
    this.modelManager = modelManager;
    this.sessionsDir = join(dataDir, 'sessions');
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
    return `sess_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit a session event
   */
  private async emitEvent(type: SessionEventType, sessionId: string, data: any): Promise<void> {
    const event: SessionEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      sessionId,
      data
    };

    this.sessionEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'session_event', {
      eventType: type,
      sessionId,
      ...data
    });
  }

  /**
   * Ensure sessions directory exists
   */
  private async ensureSessionsDir(): Promise<void> {
    if (!existsSync(this.sessionsDir)) {
      await mkdir(this.sessionsDir, { recursive: true });
    }
  }

  /**
   * Get log file path for a session
   */
  private getLogFilePath(sessionId: string): string {
    return join(this.sessionsDir, `${sessionId}.jsonl`);
  }

  /**
   * Append entry to JSONL log
   */
  private async appendToLog(sessionId: string, entry: SessionLogEntry): Promise<void> {
    await this.ensureSessionsDir();
    const logPath = this.getLogFilePath(sessionId);
    const line = JSON.stringify(entry) + '\n';
    await writeFile(logPath, line, { flag: 'a' });
  }

  /**
   * Read log entries from JSONL file
   */
  private async readLog(sessionId: string, limit?: number): Promise<SessionLogEntry[]> {
    const logPath = this.getLogFilePath(sessionId);

    if (!existsSync(logPath)) {
      return [];
    }

    const content = await readFile(logPath, 'utf-8');
    const lines = content.trim().split('\n').filter(line => line.trim());

    let entries: SessionLogEntry[] = lines.map(line => JSON.parse(line));

    if (limit && limit > 0) {
      entries = entries.slice(-limit);
    }

    return entries;
  }

  /**
   * Extract model ID from reference string
   */
  private extractModelId(modelRef: string): string {
    // Handle @(model-id) format
    const match = modelRef.match(/@\(([^)]+)\)/);
    return match ? match[1] : modelRef;
  }

  /**
   * Create a new session in created lifecycle
   */
  async createSession(
    id: string,
    defaultModel: string,
    options: {
      owner?: string;
    } = {}
  ): Promise<Session> {
    // Check if session already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Session already exists: ${id}`);
    }

    // Verify model exists
    const modelId = this.extractModelId(defaultModel);
    const model = this.modelManager.getModel(modelId);
    if (!model) {
      throw new Error(`Model not found: ${modelId}`);
    }

    const logFile = `sessions/${id}.jsonl`;

    const config: SessionConfig = {
      owner: options.owner,
      defaultModel: `@(${modelId})`,
      logFile
    };

    const session: Session = {
      id,
      type: 'program',
      programType: 'session',
      lifecycle: 'created',
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'session', {
      programType: 'session',
      owner: options.owner,
      defaultModel: config.defaultModel,
      logFile,
      lifecycle: session.lifecycle,
      version: session.version
    });

    // Create edge to default model
    await this.store.addEdge(`${id}_uses_${modelId}`, id, modelId, 'uses_model');

    await this.emitEvent('SESSION_CREATED', id, {
      owner: options.owner,
      defaultModel: config.defaultModel,
      lifecycle: session.lifecycle,
      version: session.version
    });

    return session;
  }

  /**
   * Get a session by ID
   */
  getSession(id: string): Session | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'session') {
      return null;
    }

    return this.nodeToSession(node);
  }

  /**
   * Convert a Node to a Session
   */
  private nodeToSession(node: Node): Session {
    // Handle legacy 'state' property for backwards compatibility
    const lifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'created') as SessionLifecycle;

    return {
      id: node.id,
      type: 'program',
      programType: 'session',
      lifecycle,
      config: {
        owner: node.properties.get('owner'),
        defaultModel: node.properties.get('defaultModel') || '',
        logFile: node.properties.get('logFile') || ''
      },
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Build conversation history for model invocation
   */
  private async buildConversationHistory(sessionId: string): Promise<Array<{ role: 'user' | 'assistant'; content: string }>> {
    const entries = await this.readLog(sessionId);
    return entries.map(entry => ({
      role: entry.message.role,
      content: entry.message.content
    }));
  }

  /**
   * Send a message in a session
   */
  async sendMessage(
    sessionId: string,
    message: string,
    options: SendMessageOptions = {}
  ): Promise<SendMessageResult> {
    const node = this.store.get(sessionId);
    if (!node || !(node instanceof Node) || node.type !== 'session') {
      throw new Error(`Session not found: ${sessionId}`);
    }

    const session = this.nodeToSession(node);
    const currentLifecycle = session.lifecycle;

    // Only created, active, or paused sessions can receive messages
    if (currentLifecycle === 'completed') {
      throw new Error(`Cannot send message to completed session`);
    }

    // Transition created -> active on first message
    if (currentLifecycle === 'created') {
      await this.store.updateNode(sessionId, { lifecycle: 'active' });
    }

    // Transition paused -> active when sending message
    if (currentLifecycle === 'paused') {
      await this.store.updateNode(sessionId, { lifecycle: 'active' });
      await this.emitEvent('SESSION_RESUMED', sessionId, {
        previousLifecycle: 'paused',
        newLifecycle: 'active'
      });
    }

    // Determine which model to use
    const modelId = options.model
      ? this.extractModelId(options.model)
      : this.extractModelId(session.config.defaultModel);

    // Create user message entry
    const userEntryUuid = randomUUID();
    const userEntry: SessionLogEntry = {
      uuid: userEntryUuid,
      sessionId,
      timestamp: new Date().toISOString(),
      message: {
        role: 'user',
        content: message
      },
      situation: options.situation
    };

    // Log user message
    await this.appendToLog(sessionId, userEntry);

    // Get conversation history for context
    const history = await this.buildConversationHistory(sessionId);

    // Invoke model
    const startTime = Date.now();
    let result: InferenceResult;

    try {
      result = await this.modelManager.invokeModel(modelId, {
        message: history.map(h => `${h.role}: ${h.content}`).join('\n'),
        system: options.system,
        situation: options.situation,
        stream: options.stream,
        onToken: options.onToken
      });
    } catch (error: any) {
      // Return error result
      return {
        success: false,
        error: error.message,
        duration: Date.now() - startTime,
        model: modelId,
        situation: options.situation,
        userEntry
      };
    }

    if (!result.success) {
      return {
        success: false,
        error: result.error,
        duration: result.duration,
        model: result.model,
        situation: result.situation,
        userEntry
      };
    }

    // Create assistant message entry
    const assistantEntry: SessionLogEntry = {
      uuid: randomUUID(),
      parentUuid: userEntryUuid,
      sessionId,
      timestamp: new Date().toISOString(),
      message: {
        role: 'assistant',
        content: result.text || '',
        model: result.model,
        usage: result.usage ? {
          inputTokens: result.usage.promptTokens,
          outputTokens: result.usage.completionTokens
        } : undefined
      },
      requestId: randomUUID(),
      latencyMs: result.duration,
      situation: result.situation
    };

    // Log assistant response
    await this.appendToLog(sessionId, assistantEntry);

    // Update version
    const currentVersion = session.version;
    await this.store.updateNode(sessionId, { version: currentVersion + 1 });

    // Emit message event
    await this.emitEvent('SESSION_MESSAGE', sessionId, {
      model: result.model,
      usage: assistantEntry.message.usage,
      duration: result.duration,
      situation: result.situation
    });

    return {
      success: true,
      text: result.text,
      usage: assistantEntry.message.usage,
      duration: result.duration,
      model: result.model,
      situation: result.situation,
      userEntry,
      assistantEntry
    };
  }

  /**
   * Get session conversation history
   */
  async getHistory(sessionId: string, limit?: number): Promise<SessionLogEntry[]> {
    const session = this.getSession(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    return this.readLog(sessionId, limit);
  }

  /**
   * Pause a session
   */
  async pauseSession(sessionId: string): Promise<Session> {
    const node = this.store.get(sessionId);
    if (!node || !(node instanceof Node) || node.type !== 'session') {
      throw new Error(`Session not found: ${sessionId}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'created') as SessionLifecycle;
    if (currentLifecycle !== 'active') {
      throw new Error(`Cannot pause session in ${currentLifecycle} lifecycle. Only active sessions can be paused.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(sessionId, { lifecycle: 'paused' });

    await this.emitEvent('SESSION_PAUSED', sessionId, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'paused'
    });

    return this.getSession(sessionId)!;
  }

  /**
   * Resume a paused session
   */
  async resumeSession(sessionId: string): Promise<Session> {
    const node = this.store.get(sessionId);
    if (!node || !(node instanceof Node) || node.type !== 'session') {
      throw new Error(`Session not found: ${sessionId}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'created') as SessionLifecycle;
    if (currentLifecycle !== 'paused') {
      throw new Error(`Cannot resume session in ${currentLifecycle} lifecycle. Only paused sessions can be resumed.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(sessionId, { lifecycle: 'active' });

    await this.emitEvent('SESSION_RESUMED', sessionId, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'active'
    });

    return this.getSession(sessionId)!;
  }

  /**
   * Complete a session
   */
  async completeSession(sessionId: string): Promise<Session> {
    const node = this.store.get(sessionId);
    if (!node || !(node instanceof Node) || node.type !== 'session') {
      throw new Error(`Session not found: ${sessionId}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'created') as SessionLifecycle;
    if (currentLifecycle !== 'active' && currentLifecycle !== 'paused') {
      throw new Error(`Cannot complete session in ${currentLifecycle} lifecycle. Only active or paused sessions can be completed.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(sessionId, { lifecycle: 'completed' });

    await this.emitEvent('SESSION_COMPLETED', sessionId, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'completed'
    });

    return this.getSession(sessionId)!;
  }

  /**
   * List all sessions, optionally filtered by lifecycle
   */
  listSessions(lifecycle?: SessionLifecycle): Session[] {
    const sessionNodes = this.store.getByType('session');
    let sessions = sessionNodes.map(node => this.nodeToSession(node));

    if (lifecycle) {
      sessions = sessions.filter(s => s.lifecycle === lifecycle);
    }

    return sessions;
  }

  /**
   * Get all session events
   */
  getSessionEvents(limit?: number): SessionEvent[] {
    const events = this.sessionEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific session
   */
  getSessionEventHistory(sessionId: string): SessionEvent[] {
    return this.sessionEvents.filter(e => e.sessionId === sessionId);
  }
}

export default SessionManager;
