#!/usr/bin/env bun
/**
 * WorkflowOrchestrator - Event-driven workflow execution on actor fabric
 *
 * Monitors TaskActor completions and auto-executes dependent tasks when ready.
 * Uses RelationshipActor for dependency graphs (requires relationships).
 *
 * Architecture:
 * - Subscribe to TaskActor port('events') for completions
 * - Query RelationshipActor.traverse() to find dependents
 * - Check if all dependencies satisfied (isReady)
 * - Auto-start ready tasks via TaskActor.ask('start', ...)
 *
 * Examples:
 *   Build pipeline: compile → link → test → deploy
 *   ETL workflow: extract → transform → load
 *   ML pipeline: preprocess → train → evaluate
 */

import { Actor } from '../actor.ts';
import type { MessageRouter } from '../router.ts';
import type { Message, MessageResponse, Address } from '@agentic-primer/actors';
import { createResponse, createErrorResponse, address } from '@agentic-primer/actors';
import type GraphStore from '../../graph.ts';
import type { Task, TaskEventType } from '../../entities/task.ts';
import type { Relationship } from './relationship.ts';

/**
 * Workflow definition for declarative workflow building
 */
export interface WorkflowDefinition {
  id: string;
  name: string;
  description?: string;
  tasks: Array<{
    id: string;
    title: string;
    description?: string;
    dependsOn?: string[];  // IDs of tasks that must complete first
    priority?: 'P0' | 'P1' | 'P2' | 'P3' | 'P4';
  }>;
}

/**
 * Workflow execution state
 */
export interface WorkflowExecution {
  id: string;
  workflowId: string;
  status: 'pending' | 'running' | 'completed' | 'failed';
  startedAt?: number;
  completedAt?: number;
  taskStates: Map<string, 'pending' | 'running' | 'completed' | 'failed'>;
}

/**
 * WorkflowOrchestrator - Auto-executes dependent tasks on completion
 */
export class WorkflowOrchestrator extends Actor {
  private store: GraphStore;
  private taskActorAddress: Address;
  private relationshipActorAddress: Address;
  private workflows = new Map<string, WorkflowDefinition>();
  private executions = new Map<string, WorkflowExecution>();
  private monitoring = false;

  constructor(
    id: string,
    router: MessageRouter,
    store: GraphStore,
    taskActorAddress = '@(tasks)' as Address,
    relationshipActorAddress = '@(relationships)' as Address
  ) {
    super(id, router);
    this.store = store;
    this.taskActorAddress = taskActorAddress;
    this.relationshipActorAddress = relationshipActorAddress;
  }

  /**
   * Handle incoming messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'define-workflow':
          return await this.handleDefineWorkflow(message, payload);

        case 'execute-workflow':
          return await this.handleExecuteWorkflow(message, payload);

        case 'get-workflow':
          return await this.handleGetWorkflow(message, payload);

        case 'get-execution':
          return await this.handleGetExecution(message, payload);

        case 'list-workflows':
          return await this.handleListWorkflows(message);

        case 'list-executions':
          return await this.handleListExecutions(message);

        case 'task-completed':
          return await this.handleTaskCompleted(message, payload);

        case 'task-failed':
          return await this.handleTaskFailed(message, payload);

        case 'start-monitoring':
          return await this.handleStartMonitoring(message);

        case 'stop-monitoring':
          return await this.handleStopMonitoring(message);

        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message);
    }
  }

  /**
   * Define a workflow (create task graph)
   */
  private async handleDefineWorkflow(message: Message, payload: any): Promise<MessageResponse> {
    const { id, name, description, tasks } = payload;

    if (!id || !name || !tasks || !Array.isArray(tasks)) {
      return createErrorResponse(message, 'Missing required fields: id, name, tasks');
    }

    const workflow: WorkflowDefinition = {
      id,
      name,
      description,
      tasks
    };

    this.workflows.set(id, workflow);

    return createResponse(message, {
      workflow,
      message: 'Workflow defined. Call execute-workflow to create task instances.'
    });
  }

  /**
   * Execute a workflow (create task instances + dependency edges)
   */
  private async handleExecuteWorkflow(message: Message, payload: any): Promise<MessageResponse> {
    const { workflowId, executionId } = payload;

    if (!workflowId) {
      return createErrorResponse(message, 'Missing required field: workflowId');
    }

    const workflow = this.workflows.get(workflowId);
    if (!workflow) {
      return createErrorResponse(message, `Workflow not found: ${workflowId}`);
    }

    // Generate execution ID if not provided
    const execId = executionId || this.generateExecutionId(workflowId);

    // Create execution state
    const execution: WorkflowExecution = {
      id: execId,
      workflowId,
      status: 'pending',
      taskStates: new Map()
    };

    // Create task instances
    const taskIds = new Map<string, string>(); // workflow task ID -> actual task ID
    for (const taskDef of workflow.tasks) {
      const taskId = `${execId}-${taskDef.id}`;
      taskIds.set(taskDef.id, taskId);

      // Create task via TaskActor
      const createResp = await this.ask(this.taskActorAddress, 'create', {
        id: taskId,
        title: taskDef.title,
        description: taskDef.description,
        priority: taskDef.priority || 'P2'
      });

      if (createResp.error) {
        return createErrorResponse(message, `Failed to create task ${taskId}: ${createResp.error}`);
      }

      execution.taskStates.set(taskId, 'pending');
    }

    // Create dependency relationships
    for (const taskDef of workflow.tasks) {
      if (taskDef.dependsOn && taskDef.dependsOn.length > 0) {
        const taskId = taskIds.get(taskDef.id)!;
        const taskAddr = `@(tasks/${taskId})` as Address;

        for (const depId of taskDef.dependsOn) {
          const depTaskId = taskIds.get(depId);
          if (!depTaskId) {
            return createErrorResponse(message, `Dependency not found: ${depId}`);
          }

          const depAddr = `@(tasks/${depTaskId})` as Address;

          // Create "requires" relationship (task requires dependency)
          const relResp = await this.ask(this.relationshipActorAddress, 'create', {
            type: 'requires',
            from: taskAddr,
            to: depAddr,
            strength: 1.0,
            evidence: `Workflow dependency: ${taskDef.id} requires ${depId}`
          });

          if (relResp.error) {
            return createErrorResponse(message, `Failed to create relationship: ${relResp.error}`);
          }
        }
      }
    }

    // Find root tasks (no dependencies) and auto-start them
    const rootTasks = workflow.tasks.filter(t => !t.dependsOn || t.dependsOn.length === 0);
    for (const rootTask of rootTasks) {
      const taskId = taskIds.get(rootTask.id)!;

      // Assign to system (for auto-start)
      await this.ask(this.taskActorAddress, 'update', {
        id: taskId,
        updates: { assignee: '@(system)' }
      });

      // Start immediately
      const startResp = await this.ask(this.taskActorAddress, 'start', { id: taskId });
      if (!startResp.error) {
        execution.taskStates.set(taskId, 'running');
      }
    }

    execution.status = 'running';
    execution.startedAt = Date.now();
    this.executions.set(execId, execution);

    return createResponse(message, {
      executionId: execId,
      execution,
      taskIds: Object.fromEntries(taskIds)
    });
  }

  /**
   * Handle task completion - propagate to dependents
   */
  private async handleTaskCompleted(message: Message, payload: any): Promise<MessageResponse> {
    const { taskId } = payload;

    if (!taskId) {
      return createErrorResponse(message, 'Missing required field: taskId');
    }

    await this.propagateCompletion(taskId);

    return createResponse(message, {
      message: 'Task completion propagated',
      taskId
    });
  }

  /**
   * Handle task failure
   */
  private async handleTaskFailed(message: Message, payload: any): Promise<MessageResponse> {
    const { taskId } = payload;

    if (!taskId) {
      return createErrorResponse(message, 'Missing required field: taskId');
    }

    // Mark execution as failed
    for (const execution of this.executions.values()) {
      if (execution.taskStates.has(taskId)) {
        execution.taskStates.set(taskId, 'failed');
        execution.status = 'failed';
        execution.completedAt = Date.now();
      }
    }

    return createResponse(message, {
      message: 'Task failure recorded',
      taskId
    });
  }

  /**
   * Core: Propagate task completion to dependents
   */
  async propagateCompletion(taskId: string): Promise<void> {
    const taskAddr = `@(tasks/${taskId})` as Address;

    // Find tasks that depend on this one (inbound "requires" relationships)
    const depsResp = await this.ask<{ relationships: Relationship[] }>(
      this.relationshipActorAddress,
      'query',
      {
        filter: { to: taskAddr, type: 'requires' }
      }
    );

    if (depsResp.error || !depsResp.payload?.relationships) {
      return;
    }

    const dependents = depsResp.payload.relationships;

    // Check each dependent - is it now ready?
    for (const rel of dependents) {
      const dependentTaskAddr = rel.from;
      const dependentTaskId = this.extractTaskId(dependentTaskAddr);

      if (await this.isTaskReady(dependentTaskAddr)) {
        // Auto-start the task!
        const task = await this.getTask(dependentTaskId);
        if (task && task.lifecycle === 'pending') {
          // Assign to system
          await this.ask(this.taskActorAddress, 'update', {
            id: dependentTaskId,
            updates: { assignee: '@(system)' }
          });

          // Start task
          await this.ask(this.taskActorAddress, 'start', { id: dependentTaskId });

          // Update execution state
          for (const execution of this.executions.values()) {
            if (execution.taskStates.has(dependentTaskId)) {
              execution.taskStates.set(dependentTaskId, 'running');
            }
          }
        }
      }
    }

    // Check if any executions are now complete
    for (const execution of this.executions.values()) {
      if (execution.status === 'running') {
        const allCompleted = Array.from(execution.taskStates.values()).every(
          state => state === 'completed' || state === 'failed'
        );

        if (allCompleted) {
          execution.status = 'completed';
          execution.completedAt = Date.now();
        }
      }
    }
  }

  /**
   * Check if a task is ready to execute (all dependencies completed)
   */
  async isTaskReady(taskAddr: Address): Promise<boolean> {
    // Find all tasks this one requires (outbound "requires" relationships)
    const blockersResp = await this.ask<{ paths: Array<{ node: Address; relationship: Relationship }> }>(
      this.relationshipActorAddress,
      'traverse',
      {
        start: taskAddr,
        direction: 'outbound',
        relationshipType: 'requires',
        depth: 1
      }
    );

    if (blockersResp.error || !blockersResp.payload?.paths) {
      return false;
    }

    const blockers = blockersResp.payload.paths;

    // Ready if all blockers are completed
    for (const blocker of blockers) {
      const blockerId = this.extractTaskId(blocker.node);
      const task = await this.getTask(blockerId);

      if (!task || task.lifecycle !== 'completed') {
        return false;
      }
    }

    return true;
  }

  /**
   * Get task by ID
   */
  private async getTask(taskId: string): Promise<Task | null> {
    const resp = await this.ask<{ task: Task }>(this.taskActorAddress, 'get', { id: taskId });
    return resp.payload?.task || null;
  }

  /**
   * Extract task ID from address (@(tasks/task-123) → task-123)
   */
  private extractTaskId(address: Address): string {
    const match = address.match(/@\(tasks\/(.+)\)/);
    return match ? match[1] : address;
  }

  /**
   * Generate execution ID
   */
  private generateExecutionId(workflowId: string): string {
    return `${workflowId}-exec-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`;
  }

  /**
   * Get workflow by ID
   */
  private async handleGetWorkflow(message: Message, payload: any): Promise<MessageResponse> {
    const { id } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    const workflow = this.workflows.get(id);

    if (!workflow) {
      return createErrorResponse(message, `Workflow not found: ${id}`);
    }

    return createResponse(message, { workflow });
  }

  /**
   * Get execution by ID
   */
  private async handleGetExecution(message: Message, payload: any): Promise<MessageResponse> {
    const { id } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    const execution = this.executions.get(id);

    if (!execution) {
      return createErrorResponse(message, `Execution not found: ${id}`);
    }

    // Convert Map to object for serialization
    const executionData = {
      ...execution,
      taskStates: Object.fromEntries(execution.taskStates)
    };

    return createResponse(message, { execution: executionData });
  }

  /**
   * List all workflows
   */
  private async handleListWorkflows(message: Message): Promise<MessageResponse> {
    const workflows = Array.from(this.workflows.values());
    return createResponse(message, { workflows, count: workflows.length });
  }

  /**
   * List all executions
   */
  private async handleListExecutions(message: Message): Promise<MessageResponse> {
    const executions = Array.from(this.executions.values()).map(exec => ({
      ...exec,
      taskStates: Object.fromEntries(exec.taskStates)
    }));

    return createResponse(message, { executions, count: executions.length });
  }

  /**
   * Start monitoring TaskActor events (future: subscribe to port)
   */
  private async handleStartMonitoring(message: Message): Promise<MessageResponse> {
    this.monitoring = true;
    return createResponse(message, { monitoring: true });
  }

  /**
   * Stop monitoring
   */
  private async handleStopMonitoring(message: Message): Promise<MessageResponse> {
    this.monitoring = false;
    return createResponse(message, { monitoring: false });
  }
}

export default WorkflowOrchestrator;
