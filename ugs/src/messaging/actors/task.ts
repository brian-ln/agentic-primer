#!/usr/bin/env bun
/**
 * TaskActor - Graph-addressable task management
 *
 * Wraps TaskManager with message-based interface for graph-addressable tasks.
 * Enables tasks to interact with knowledge actors and other parts of the system.
 *
 * Examples:
 *   @(tasks/task-123)
 *   @(tasks/validate-hypothesis)
 */

import { Actor } from '../actor.ts';
import type { MessageRouter } from '../router.ts';
import type { Message, MessageResponse, Address } from '@agentic-primer/actors';
import { createResponse, createErrorResponse, address } from '@agentic-primer/actors';
import { TaskManager, type Task, type TaskSpec, type TaskPriority } from '../../entities/task.ts';
import GraphStore from '../../graph.ts';

/**
 * TaskActor - Manages tasks through message interface
 */
export class TaskActor extends Actor {
  private taskManager: TaskManager;

  constructor(id: string, router: MessageRouter, store: GraphStore) {
    super(id, router);
    this.taskManager = new TaskManager(store);
  }

  /**
   * Handle incoming messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'create':
          return await this.handleCreate(message, payload);

        case 'get':
          return await this.handleGet(message, payload);

        case 'query':
          return await this.handleQuery(message, payload);

        case 'start':
          return await this.handleStart(message, payload);

        case 'complete':
          return await this.handleComplete(message, payload);

        case 'fail':
          return await this.handleFail(message, payload);

        case 'update':
          return await this.handleUpdate(message, payload);

        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message);
    }
  }

  /**
   * Create new task
   */
  private async handleCreate(message: Message, payload: any): Promise<MessageResponse> {
    const { id, title, spec, assignee, priority, description } = payload;

    if (!id || !title) {
      return createErrorResponse(message, 'Missing required fields: id, title');
    }

    const task = await this.taskManager.createTask(id, title, {
      spec,
      assignee,
      priority: priority as TaskPriority,
      description
    });

    const taskAddress = `@(tasks/${id})` as Address;

    return createResponse(message, {
      address: taskAddress,
      task
    });
  }

  /**
   * Get task by ID
   */
  private async handleGet(message: Message, payload: any): Promise<MessageResponse> {
    const { id } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    const task = this.taskManager.getTask(id);

    if (!task) {
      return createErrorResponse(message, `Task not found: ${id}`);
    }

    return createResponse(message, { task });
  }

  /**
   * Query tasks
   */
  private async handleQuery(message: Message, payload: any): Promise<MessageResponse> {
    const { filter = {}, limit = 100 } = payload;

    // Use TaskManager's listTasks with filter
    const results = this.taskManager.listTasks(filter);

    // Apply limit
    const limited = results.slice(0, limit);

    return createResponse(message, {
      count: limited.length,
      tasks: limited
    });
  }

  /**
   * Start task (transition to in_progress)
   */
  private async handleStart(message: Message, payload: any): Promise<MessageResponse> {
    const { id } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    await this.taskManager.startTask(id);
    const task = this.taskManager.getTask(id);

    return createResponse(message, { task });
  }

  /**
   * Complete task
   */
  private async handleComplete(message: Message, payload: any): Promise<MessageResponse> {
    const { id, result, createKnowledge } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    await this.taskManager.completeTask(id, result);
    const task = this.taskManager.getTask(id);

    // If createKnowledge specified, send message to knowledge actor
    let knowledgeAddress: Address | undefined;
    if (createKnowledge) {
      const { category, content, reasoning, epistemic_level, confidence, evidence, session_id } = createKnowledge;

      const knowledgeMsg = await this.ask(
        address('services/knowledge'),
        'create',
        {
          category,
          content,
          reasoning,
          epistemic_level,
          confidence,
          evidence,
          session_id
        }
      );

      knowledgeAddress = knowledgeMsg.payload?.address;
    }

    return createResponse(message, {
      task,
      knowledgeCreated: knowledgeAddress
    });
  }

  /**
   * Fail task
   */
  private async handleFail(message: Message, payload: any): Promise<MessageResponse> {
    const { id, reason } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    await this.taskManager.failTask(id, reason);
    const task = this.taskManager.getTask(id);

    return createResponse(message, { task });
  }

  /**
   * Update task
   */
  private async handleUpdate(message: Message, payload: any): Promise<MessageResponse> {
    const { id, updates } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    // Update task through TaskManager
    const task = this.taskManager.getTask(id);
    if (!task) {
      return createErrorResponse(message, `Task not found: ${id}`);
    }

    // Apply updates
    if (updates.assignee !== undefined) {
      await this.taskManager.assignTask(id, updates.assignee);
    }

    if (updates.priority !== undefined) {
      // Note: TaskManager doesn't have updatePriority method, would need to add
      // For now, just return the task
    }

    const updatedTask = this.taskManager.getTask(id);

    return createResponse(message, { task: updatedTask });
  }
}

export default TaskActor;
