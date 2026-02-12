#!/usr/bin/env bun
/**
 * Query DSL Examples
 *
 * Demonstrates the declarative query interface for actor workflows.
 */

import { query, pattern, send, update, filter, logic, create, deleteEntity, createRelationship, deleteRelationship } from './index.ts';
import { knowledge, decisions, learnings, errors, knowledgeTraversal, knowledgeAddress } from './knowledge-integration.ts';
import type { QueryDefinition } from './types.ts';

/**
 * Example 1: Find ready tasks
 *
 * Cypher equivalent:
 * MATCH (task:Task {status: 'open'})
 * WHERE NOT EXISTS {
 *   (task)<-[:REQUIRES]-(blocker:Task {status: 'open'})
 * }
 * RETURN task
 */
export const findReadyTasks = (): QueryDefinition => {
  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({ status: 'open' })
        .notExists(
          pattern('blocker')
            .label('Task')
            .where({ status: 'open' })
            .relatedTo('task', { type: 'requires', direction: 'inbound' })
        )
    )
    .return(['task'])
    .build();
};

/**
 * Example 2: Auto-start ready tasks
 *
 * Cypher equivalent:
 * MATCH (task:Task {status: 'open'})
 * WHERE NOT EXISTS {
 *   (task)<-[:REQUIRES]-(blocker:Task {status: 'open'})
 * }
 * SEND task MESSAGE { type: 'start' }
 */
export const autoStartReadyTasks = (): QueryDefinition => {
  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({ status: 'open' })
        .notExists(
          pattern('blocker')
            .label('Task')
            .where({ status: 'open' })
            .relatedTo('task', { type: 'requires', direction: 'inbound' })
        )
    )
    .forEach(send('task').tell('start'))
    .build();
};

/**
 * Example 3: Build dependency tree
 *
 * Traverse from root task through requires relationships.
 */
export const buildDependencyTree = (rootTaskId: string): QueryDefinition => {
  return query()
    .match(pattern('root').label('Task').where({ id: rootTaskId }))
    .traverse({
      from: 'root',
      relationship: 'requires',
      direction: 'outbound',
      depth: { max: 5 },
      as: 'dependencies',
    })
    .aggregate({
      operation: 'collect',
      variable: 'dependencies',
      by: 'root',
      as: 'tree',
    })
    .return(['root', 'tree'])
    .build();
};

/**
 * Example 4: Find high-priority tasks assigned to user
 *
 * Demonstrates filtering with multiple conditions.
 */
export const findMyHighPriorityTasks = (userId: string): QueryDefinition => {
  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({ assignee: userId, status: 'open' })
    )
    .where(
      logic.or(
        filter('task', 'priority').eq('high'),
        filter('task', 'priority').eq('urgent')
      )
    )
    .return(['task'])
    .build();
};

/**
 * Example 5: Conditional workflow - test and deploy
 *
 * When test completes successfully, trigger deploy.
 * When test fails, retry compile.
 */
export const testDeployWorkflow = (): QueryDefinition => {
  return query()
    .match(
      pattern('compile').where({ id: 'compile' }),
      pattern('test').where({ id: 'test' }),
      pattern('deploy').where({ id: 'deploy' })
    )
    .when(
      pattern('test').where({ lifecycle: 'completed', result: { passed: true } })
    )
    .then(send('deploy').tell('start'))
    .build();
};

/**
 * Example 6: Find all tasks related to a knowledge item
 *
 * Demonstrates bidirectional relationship traversal.
 */
export const findTasksForKnowledge = (knowledgeId: string): QueryDefinition => {
  return query()
    .match(pattern('knowledge').label('Knowledge').where({ id: knowledgeId }))
    .traverse({
      from: 'knowledge',
      relationship: 'supports',
      direction: 'both', // Both inbound and outbound
      depth: { max: 2 },
      as: 'tasks',
    })
    .return(['tasks'])
    .build();
};

/**
 * Example 7: Count tasks by status
 *
 * Demonstrates aggregation operations.
 */
export const countTasksByStatus = (): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task'))
    .aggregate({
      operation: 'group',
      variable: 'task',
      by: 'status',
      as: 'statusCounts',
    })
    .return(['statusCounts'])
    .build();
};

/**
 * Example 8: Find blocked tasks
 *
 * Tasks with open dependencies.
 */
export const findBlockedTasks = (): QueryDefinition => {
  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({ status: 'open' })
        .relatedTo('blocker', {
          type: 'requires',
          direction: 'outbound',
        })
    )
    .match(pattern('blocker').label('Task').where({ status: 'open' }))
    .return(['task', 'blocker'])
    .build();
};

/**
 * Example 9: Update task properties in bulk
 *
 * Set all open tasks to in_progress.
 */
export const bulkUpdateTaskStatus = (
  fromStatus: string,
  toStatus: string
): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ status: fromStatus }))
    .forEach(send('task').tell('update', { status: toStatus }))
    .build();
};

/**
 * Example 10: Query with complex filtering
 *
 * Find tasks created in the last 7 days with specific criteria.
 */
export const findRecentHighValueTasks = (): QueryDefinition => {
  const sevenDaysAgo = Date.now() - 7 * 24 * 60 * 60 * 1000;

  return query()
    .match(pattern('task').label('Task'))
    .where(
      logic.and(
        filter('task', 'createdAt').gte(sevenDaysAgo),
        logic.or(
          filter('task', 'priority').eq('high'),
          filter('task', 'value').gte(100)
        )
      )
    )
    .return(['task'])
    .build();
};

/**
 * Example 11: CREATE a new task
 *
 * Simple task creation with properties.
 */
export const createNewTask = (title: string, assignee?: string): QueryDefinition => {
  return query()
    .forEach(
      create('task').as({
        title,
        assignee,
        status: 'open',
        priority: 'normal',
        createdAt: Date.now(),
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 12: CREATE multiple entities in batch
 *
 * Demonstrates batch creation of multiple tasks.
 */
export const createBatchTasks = (tasks: Array<{ title: string; priority: string }>): QueryDefinition => {
  let q = query();

  for (const task of tasks) {
    q = q.forEach(
      create('task').as({
        title: task.title,
        priority: task.priority,
        status: 'open',
        createdAt: Date.now(),
      })
    );
  }

  return q.build();
};

/**
 * Example 13: CREATE task from template
 *
 * Find a template and create a task based on its properties.
 */
export const createTaskFromTemplate = (templateId: string): QueryDefinition => {
  return query()
    .match(pattern('template').label('Template').where({ id: templateId }))
    .forEach(
      create('task').as({
        title: '${template.title}',
        description: '${template.description}',
        priority: '${template.defaultPriority}',
        status: 'open',
        createdAt: Date.now(),
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 14: CREATE knowledge entity
 *
 * Create a new knowledge item with relationships.
 */
export const createKnowledgeItem = (
  title: string,
  content: string,
  tags: string[]
): QueryDefinition => {
  return query()
    .forEach(
      create('knowledge').as({
        title,
        content,
        tags,
        type: 'insight',
        createdAt: Date.now(),
        metadata: {
          source: 'manual',
          version: 1,
        },
      })
    )
    .return(['knowledge'])
    .build();
};

/**
 * Example 15: CREATE with nested properties
 *
 * Create entity with complex nested structure.
 */
export const createComplexTask = (): QueryDefinition => {
  return query()
    .forEach(
      create('task').as({
        title: 'Complex Task',
        status: 'open',
        metadata: {
          project: 'alpha',
          phase: 'development',
          estimate: {
            hours: 8,
            confidence: 0.8,
          },
        },
        configuration: {
          retryPolicy: {
            maxAttempts: 3,
            backoffMs: 1000,
          },
          timeout: 30000,
        },
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 16: Conditional CREATE
 *
 * Create task only if certain conditions are met.
 */
export const createConditionalTask = (userId: string): QueryDefinition => {
  return query()
    .match(
      pattern('user')
        .label('User')
        .where({ id: userId, hasCapacity: true })
    )
    .forEach(
      create('task').as({
        title: 'Auto-assigned Task',
        assignee: userId,
        status: 'open',
        priority: 'normal',
        autoAssigned: true,
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 17: CREATE multiple entity types
 *
 * Create both a task and a knowledge item in one workflow.
 */
export const createTaskWithKnowledge = (
  taskTitle: string,
  knowledgeContent: string
): QueryDefinition => {
  return query()
    .forEach(
      create('task').as({
        title: taskTitle,
        status: 'open',
        priority: 'high',
      })
    )
    .forEach(
      create('knowledge').as({
        title: `Knowledge for: ${taskTitle}`,
        content: knowledgeContent,
        type: 'documentation',
      })
    )
    .return(['task', 'knowledge'])
    .build();
};

/**
 * Example 18: CREATE with dynamic properties
 *
 * Create entity with properties computed at runtime.
 */
export const createDynamicTask = (params: {
  title: string;
  urgency: 'low' | 'medium' | 'high' | 'critical';
}): QueryDefinition => {
  const priorityMap = {
    low: 'low',
    medium: 'normal',
    high: 'high',
    critical: 'urgent',
  };

  const dueDateMs = {
    low: 7 * 24 * 60 * 60 * 1000, // 7 days
    medium: 3 * 24 * 60 * 60 * 1000, // 3 days
    high: 24 * 60 * 60 * 1000, // 1 day
    critical: 4 * 60 * 60 * 1000, // 4 hours
  };

  return query()
    .forEach(
      create('task').as({
        title: params.title,
        priority: priorityMap[params.urgency],
        status: 'open',
        dueDate: Date.now() + dueDateMs[params.urgency],
        urgency: params.urgency,
        createdAt: Date.now(),
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 13: DELETE a single task by ID
 *
 * Safely delete a specific task with explicit confirmation.
 *
 * WARNING: DELETE operations require explicit confirmation to prevent
 * accidental data loss. Use .confirm() for single entity deletes.
 */
export const deleteSingleTask = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(deleteEntity('task').confirm())
    .build();
};

/**
 * Example 14: SOFT DELETE completed tasks
 *
 * Mark completed tasks as deleted without actually removing them.
 * Useful for maintaining audit trails and allowing data recovery.
 *
 * Soft delete sets a 'deletedAt' timestamp rather than removing entities.
 */
export const softDeleteCompletedTasks = (olderThanDays: number): QueryDefinition => {
  const cutoffDate = Date.now() - olderThanDays * 24 * 60 * 60 * 1000;

  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({
          status: 'completed',
          completedAt: { lt: cutoffDate },
        })
    )
    .forEach(deleteEntity('task').soft())
    .return(['task'])
    .build();
};

/**
 * Example 15: CASCADE DELETE project with dependencies
 *
 * Delete a project and all related entities (tasks, documents, etc.).
 *
 * WARNING: Cascade deletes are destructive and will remove all related
 * entities through specified relationships. Use with caution.
 */
export const cascadeDeleteProject = (
  projectId: string,
  includeRelationships: string[] = ['tasks', 'milestones', 'documents']
): QueryDefinition => {
  return query()
    .match(pattern('project').label('Project').where({ id: projectId }))
    .forEach(deleteEntity('project').cascade(includeRelationships))
    .build();
};

/**
 * Example 16: DELETE obsolete tasks with safety check
 *
 * Delete old cancelled tasks that haven't been modified in a year.
 * This demonstrates safe bulk deletion patterns.
 *
 * Note: If more than 1 entity matches, execution will fail unless
 * .confirmBulk(N) is used instead of .confirm().
 */
export const deleteObsoleteTasks = (): QueryDefinition => {
  const oneYearAgo = Date.now() - 365 * 24 * 60 * 60 * 1000;

  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({
          status: 'cancelled',
          lastModified: { lt: oneYearAgo },
        })
    )
    .forEach(deleteEntity('task').confirm())
    .return(['task'])
    .build();
};

/**
 * Example 17: Conditional DELETE based on lifecycle
 *
 * Delete tasks that have been in a failed state for too long.
 * Combines pattern matching with deletion.
 */
export const deleteFailedTasks = (retentionDays: number = 30): QueryDefinition => {
  const cutoffDate = Date.now() - retentionDays * 24 * 60 * 60 * 1000;

  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({
          lifecycle: 'failed',
          failedAt: { lt: cutoffDate },
        })
    )
    .forEach(deleteEntity('task').soft())
    .build();
};

/**
 * Example 18: DELETE with traversal
 *
 * Find all orphaned subtasks (tasks without parent) and delete them.
 * Demonstrates deletion after graph traversal.
 */
export const deleteOrphanedSubtasks = (): QueryDefinition => {
  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({ type: 'subtask' })
        .notExists(
          pattern('parent')
            .label('Task')
            .relatedTo('task', { type: 'parent', direction: 'inbound' })
        )
    )
    .forEach(deleteEntity('task').confirm())
    .return(['task'])
    .build();
};

/**
 * Example 26: UPDATE task status
 *
 * Simple property update on a single task.
 * Demonstrates basic UPDATE operation.
 */
export const updateTaskStatus = (taskId: string, newStatus: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(update('task').set({ status: newStatus }))
    .return(['task'])
    .build();
};

/**
 * Example 27: UPDATE with timestamp
 *
 * Update task and record when it was modified.
 * Demonstrates UPDATE with computed values.
 */
export const markTaskInProgress = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(
      update('task').set({
        status: 'in_progress',
        startedAt: Date.now(),
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 28: Bulk UPDATE by criteria
 *
 * Update all tasks matching specific criteria.
 * Demonstrates batch UPDATE operations.
 */
export const bulkUpdatePriority = (fromPriority: string, toPriority: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ priority: fromPriority, status: 'open' }))
    .forEach(
      update('task').set({
        priority: toPriority,
        priorityChangedAt: Date.now(),
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 29: UPDATE with partial properties
 *
 * Update only specific fields, leaving others unchanged.
 * Demonstrates partial UPDATE semantics.
 */
export const assignTaskToUser = (taskId: string, userId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(
      update('task').set({
        assignee: userId,
        assignedAt: Date.now(),
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 30: UPDATE with nested properties
 *
 * Update nested object properties (metadata).
 * Demonstrates UPDATE with complex data structures.
 */
export const updateTaskMetadata = (taskId: string, metadata: Record<string, any>): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(
      update('task').set({
        metadata: {
          ...metadata,
          lastModified: Date.now(),
        },
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 31: Conditional UPDATE
 *
 * Update task only when specific conditions are met.
 * Demonstrates WHEN...THEN with UPDATE.
 */
export const autoProgressTask = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .when(pattern('task').where({ status: 'ready', allDependenciesComplete: true }))
    .then(
      update('task').set({
        status: 'in_progress',
        autoStartedAt: Date.now(),
      })
    )
    .build();
};

/**
 * Example 32: UPDATE after traversal
 *
 * Find related entities and update them.
 * Demonstrates UPDATE on traversal results.
 */
export const markDependenciesReady = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .traverse({
      from: 'task',
      relationship: 'requires',
      direction: 'outbound',
      as: 'dependencies',
    })
    .forEach(
      update('dependencies').set({
        status: 'ready',
        readyAt: Date.now(),
      })
    )
    .return(['dependencies'])
    .build();
};

/**
 * Example 33: Cascade UPDATE
 *
 * Update a task and all its dependencies.
 * Demonstrates multi-level UPDATE operations.
 */
export const cascadeStatusUpdate = (rootTaskId: string, newStatus: string): QueryDefinition => {
  return query()
    .match(pattern('root').label('Task').where({ id: rootTaskId }))
    .traverse({
      from: 'root',
      relationship: 'requires',
      direction: 'outbound',
      depth: { max: 5 },
      as: 'allDeps',
    })
    .forEach(
      update('allDeps').set({
        status: newStatus,
        cascadeUpdatedAt: Date.now(),
      })
    )
    .return(['root', 'allDeps'])
    .build();
};

/**
 * Example 34: UPDATE with null (property removal)
 *
 * Remove properties by setting them to null.
 * Demonstrates property deletion via UPDATE.
 */
export const unassignTask = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(
      update('task').set({
        assignee: null,
        assignedAt: null,
        unassignedAt: Date.now(),
      })
    )
    .return(['task'])
    .build();
};

/**
 * Example 35: Multiple sequential UPDATEs
 *
 * Apply multiple updates to the same entity.
 * Demonstrates chained UPDATE operations.
 */
export const progressTaskThroughStages = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(update('task').set({ status: 'in_progress', startedAt: Date.now() }))
    .forEach(update('task').set({ stage: 'testing', testingStartedAt: Date.now() }))
    .return(['task'])
    .build();
};

/**
 * Example 36: UPDATE with aggregation
 *
 * Update based on aggregate calculations.
 * Demonstrates UPDATE with computed values from aggregations.
 */
export const updateProjectProgress = (projectId: string): QueryDefinition => {
  return query()
    .match(pattern('project').label('Project').where({ id: projectId }))
    .traverse({
      from: 'project',
      relationship: 'contains',
      direction: 'outbound',
      as: 'tasks',
    })
    .aggregate({
      operation: 'count',
      variable: 'tasks',
      as: 'totalTasks',
    })
    .forEach(
      update('project').set({
        taskCount: 0, // Would be replaced with actual aggregate value at runtime
        lastUpdated: Date.now(),
      })
    )
    .return(['project'])
    .build();
};

/**
 * Example 37: DELETE specific relationship
 *
 * Remove a specific dependency between two tasks.
 * Demonstrates precise relationship deletion.
 */
export const removeTaskDependency = (
  taskId: string,
  dependencyId: string
): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ id: taskId }),
      pattern('dep').label('Task').where({ id: dependencyId })
    )
    .forEach(
      deleteRelationship('task', 'dep', { type: 'requires' }).confirm()
    )
    .return(['task'])
    .build();
};

/**
 * Example 38: DELETE all relationships of a type
 *
 * Remove all assignments from a task.
 * Demonstrates bulk relationship deletion.
 */
export const unassignAllUsersFromTask = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(
      deleteRelationship('task', undefined, {
        type: 'assignedTo',
        direction: 'outbound'
      }).confirmAll()
    )
    .return(['task'])
    .build();
};

/**
 * Example 39: DELETE relationships and detach node
 *
 * Remove all relationships from cancelled tasks.
 * Demonstrates complete node detachment.
 */
export const detachCancelledTasks = (): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ status: 'cancelled' }))
    .forEach(
      deleteRelationship('task', undefined, {
        direction: 'both'
      }).confirmAll()
    )
    .build();
};

/**
 * Example 40: DELETE relationships with cascade orphans
 *
 * Remove dependency and clean up orphaned nodes.
 * Demonstrates cascade deletion of orphaned entities.
 *
 * WARNING: cascadeOrphans will delete nodes that have no remaining
 * relationships after the deletion. Use with caution.
 */
export const removeDependencyWithCleanup = (
  taskId: string
): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ id: taskId }),
      pattern('dep').label('Task').relatedTo('task', {
        type: 'requires',
        direction: 'outbound'
      })
    )
    .forEach(
      deleteRelationship('task', 'dep', { type: 'requires' })
        .cascadeOrphans()
        .confirm()
    )
    .build();
};

/**
 * Example 41: Conditional relationship deletion
 *
 * Remove completed dependencies from open tasks.
 * Demonstrates selective relationship deletion based on node properties.
 */
export const cleanupCompletedDependencies = (): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ status: 'open' }),
      pattern('dep').label('Task')
        .where({ status: 'completed' })
        .relatedTo('task', { type: 'requires', direction: 'inbound' })
    )
    .forEach(
      deleteRelationship('task', 'dep', { type: 'requires' }).confirm()
    )
    .build();
};

/**
 * Example 42: DELETE inbound relationships
 *
 * Remove all tasks that depend on this one.
 * Demonstrates deletion by direction.
 */
export const removeInboundDependencies = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(
      deleteRelationship('task', undefined, {
        type: 'requires',
        direction: 'inbound'
      }).confirmAll()
    )
    .build();
};

/**
 * Example 43: DELETE bidirectional relationships
 *
 * Remove all relationships in both directions for a given type.
 * Useful for cleaning up symmetric relationships.
 */
export const removeAllCollaborations = (userId: string): QueryDefinition => {
  return query()
    .match(pattern('user').label('User').where({ id: userId }))
    .forEach(
      deleteRelationship('user', undefined, {
        type: 'collaborates',
        direction: 'both'
      }).confirmAll()
    )
    .build();
};

/**
 * Example 44: DELETE multiple relationship types
 *
 * Remove multiple types of relationships from a node.
 * Demonstrates chained relationship deletions.
 */
export const detachTaskCompletely = (taskId: string): QueryDefinition => {
  return query()
    .match(pattern('task').label('Task').where({ id: taskId }))
    .forEach(deleteRelationship('task', undefined, { type: 'requires' }).confirmAll())
    .forEach(deleteRelationship('task', undefined, { type: 'assignedTo' }).confirmAll())
    .forEach(deleteRelationship('task', undefined, { type: 'blockedBy' }).confirmAll())
    .build();
};

/**
 * Example 45: CREATE simple dependency relationship
 *
 * Create a dependency relationship between two tasks.
 * Demonstrates basic relationship creation using link shorthand.
 */
export const createTaskDependency = (
  taskId: string,
  dependencyId: string
): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ id: taskId }),
      pattern('dep').label('Task').where({ id: dependencyId })
    )
    .link('task', 'dep', 'requires')
    .return(['task', 'dep'])
    .build();
};

/**
 * Example 46: CREATE relationship with properties
 *
 * Create a relationship with additional metadata.
 * Demonstrates full createRelationship syntax with properties.
 */
export const createTaskBlocker = (
  taskId: string,
  blockerId: string,
  priority: string
): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ id: taskId }),
      pattern('blocker').label('Task').where({ id: blockerId })
    )
    .createRelationship('task', 'blocker', {
      type: 'requires',
      properties: {
        priority,
        createdAt: Date.now(),
        createdBy: 'system',
      },
    })
    .return(['task', 'blocker'])
    .build();
};

/**
 * Example 47: CREATE relationship with strength and evidence
 *
 * Create a knowledge graph relationship with confidence metrics.
 * Demonstrates using relationship builder methods.
 */
export const linkKnowledgeWithEvidence = (
  learningId: string,
  decisionId: string,
  evidence: string
): QueryDefinition => {
  return query()
    .match(
      pattern('learning').label('Knowledge').where({ id: learningId }),
      pattern('decision').label('Knowledge').where({ id: decisionId })
    )
    .forEach(
      createRelationship('learning', 'decision', 'supports')
        .strength(0.9)
        .evidence(evidence)
        .withProperties({
          source: 'analysis',
          verified: true,
          timestamp: Date.now(),
        })
    )
    .return(['learning', 'decision'])
    .build();
};

/**
 * Example 48: CREATE assignment relationship
 *
 * Assign a task to a user with assignment metadata.
 * Demonstrates typical user-task relationship pattern.
 */
export const assignTaskWithMetadata = (
  taskId: string,
  userId: string
): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ id: taskId }),
      pattern('user').label('User').where({ id: userId })
    )
    .link('task', 'user', 'assignedTo', {
      assignedAt: Date.now(),
      assignedBy: 'workflow-engine',
      priority: 'normal',
    })
    .return(['task', 'user'])
    .build();
};

/**
 * Example 49: CREATE multiple relationships in dependency chain
 *
 * Build a chain of dependencies: deploy → test → build → compile.
 * Demonstrates creating multiple relationships in a single query.
 */
export const createDependencyChain = (): QueryDefinition => {
  return query()
    .match(
      pattern('compile').label('Task').where({ id: 'compile' }),
      pattern('build').label('Task').where({ id: 'build' }),
      pattern('test').label('Task').where({ id: 'test' }),
      pattern('deploy').label('Task').where({ id: 'deploy' })
    )
    .link('build', 'compile', 'requires')
    .link('test', 'build', 'requires')
    .link('deploy', 'test', 'requires')
    .return(['compile', 'build', 'test', 'deploy'])
    .build();
};

/**
 * Example 50: CREATE bidirectional relationships
 *
 * Create symmetric relationships between collaborating users.
 * Demonstrates bidirectional relationship patterns.
 */
export const createCollaboration = (
  user1Id: string,
  user2Id: string
): QueryDefinition => {
  return query()
    .match(
      pattern('user1').label('User').where({ id: user1Id }),
      pattern('user2').label('User').where({ id: user2Id })
    )
    .link('user1', 'user2', 'collaborates', {
      since: Date.now(),
      active: true,
    })
    .link('user2', 'user1', 'collaborates', {
      since: Date.now(),
      active: true,
    })
    .return(['user1', 'user2'])
    .build();
};

/**
 * Example 51: CREATE relationship based on pattern match
 *
 * Find tasks without owners and assign them to available users.
 * Demonstrates conditional relationship creation.
 */
export const autoAssignUnownedTasks = (userId: string): QueryDefinition => {
  return query()
    .match(
      pattern('task')
        .label('Task')
        .where({ status: 'open' })
        .notExists(
          pattern('owner')
            .label('User')
            .relatedTo('task', { type: 'assignedTo', direction: 'inbound' })
        )
    )
    .match(pattern('user').label('User').where({ id: userId }))
    .link('task', 'user', 'assignedTo', {
      autoAssigned: true,
      assignedAt: Date.now(),
    })
    .return(['task', 'user'])
    .build();
};

/**
 * Example 52: CREATE knowledge relationships from analysis
 *
 * Link decisions with supporting learnings discovered in session.
 * Demonstrates graph-based knowledge management patterns.
 */
export const linkDecisionToLearnings = (
  decisionId: string,
  learningIds: string[]
): QueryDefinition => {
  let q = query().match(
    pattern('decision').label('Knowledge').where({ id: decisionId })
  );

  // Add patterns for each learning
  learningIds.forEach((learningId, idx) => {
    q = q.match(
      pattern(`learning${idx}`).label('Knowledge').where({ id: learningId })
    );
  });

  // Create relationships from each learning to the decision
  learningIds.forEach((_, idx) => {
    q = q.link(`learning${idx}`, 'decision', 'supports', {
      discoveredAt: Date.now(),
      source: 'session-analysis',
    });
  });

  return q.return(['decision']).build();
};

/**
 * Example 53: CREATE relationship with contraints validation
 *
 * Create a dependency only if the blocker is not complete.
 * Demonstrates validation through pattern matching.
 */
export const createValidatedDependency = (
  taskId: string,
  blockerId: string
): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ id: taskId, status: 'open' }),
      pattern('blocker').label('Task').where({
        id: blockerId,
        status: { ne: 'completed' }, // Not completed
      })
    )
    .link('task', 'blocker', 'requires', {
      validatedAt: Date.now(),
      reason: 'task-blocked-until-completion',
    })
    .return(['task', 'blocker'])
    .build();
};

/**
 * Example 54: CREATE relationship for graph traversal
 *
 * Build a hierarchical task structure with parent-child relationships.
 * Demonstrates tree-building patterns in graphs.
 */
export const createTaskHierarchy = (
  parentId: string,
  childIds: string[]
): QueryDefinition => {
  let q = query().match(
    pattern('parent').label('Task').where({ id: parentId })
  );

  // Add child patterns
  childIds.forEach((childId, idx) => {
    q = q.match(pattern(`child${idx}`).label('Task').where({ id: childId }));
  });

  // Create parent-child relationships
  childIds.forEach((_, idx) => {
    q = q.link('parent', `child${idx}`, 'contains', {
      order: idx,
      createdAt: Date.now(),
    });
  });

  return q.return(['parent']).build();
};

/**
 * Example 55: CREATE cross-type relationships
 *
 * Link tasks to knowledge items for documentation.
 * Demonstrates relationships across different entity types.
 */
export const linkTaskToKnowledge = (
  taskId: string,
  knowledgeId: string,
  relationshipType: 'documents' | 'implements' | 'validates'
): QueryDefinition => {
  return query()
    .match(
      pattern('task').label('Task').where({ id: taskId }),
      pattern('knowledge').label('Knowledge').where({ id: knowledgeId })
    )
    .link('task', 'knowledge', relationshipType, {
      linkedAt: Date.now(),
      category: 'task-knowledge-link',
    })
    .return(['task', 'knowledge'])
    .build();
};

/**
 * KNOWLEDGE MANAGEMENT EXAMPLES
 */

/**
 * Example 56: Find high-confidence decisions
 *
 * Query decisions with 'know' epistemic level and high confidence.
 * Demonstrates knowledge pattern matching.
 */
export const findHighConfidenceDecisions = (): QueryDefinition => {
  return query()
    .match(
      decisions('dec')
        .epistemicLevel('know')
        .minConfidence(0.95)
    )
    .return(['dec'])
    .build();
};

/**
 * Example 57: Find learnings from session
 *
 * Query all learnings extracted from a specific session.
 * Demonstrates session-based knowledge filtering.
 */
export const findSessionLearnings = (sessionId: string): QueryDefinition => {
  return query()
    .match(learnings('learn').sessionId(sessionId))
    .return(['learn'])
    .build();
};

/**
 * Example 58: Find errors by category
 *
 * Query errors from session knowledge system.
 * Demonstrates error knowledge queries.
 */
export const findKnownErrors = (): QueryDefinition => {
  return query()
    .match(errors('err').minConfidence(0.8))
    .return(['err'])
    .build();
};

/**
 * Example 59: Traverse decision support network
 *
 * Find all evidence that supports a decision.
 * Demonstrates graph traversal over knowledge relationships.
 */
export const findDecisionEvidence = (decisionId: string): QueryDefinition => {
  return query()
    .match(decisions('dec').id(decisionId))
    .traverse(
      knowledgeTraversal('dec', {
        relationshipType: 'supports',
        direction: 'inbound',
        maxDepth: 3,
        as: 'evidence',
      })
    )
    .return(['dec', 'evidence'])
    .build();
};

/**
 * Example 60: Find contradicting knowledge
 *
 * Find knowledge items that contradict each other.
 * Demonstrates relationship-based pattern matching.
 */
export const findContradictions = (): QueryDefinition => {
  return query()
    .match(
      knowledge('k1'),
      knowledge('k2').relatedTo('k1', {
        type: 'contradicts',
        direction: 'outbound',
      })
    )
    .return(['k1', 'k2'])
    .build();
};

/**
 * Example 61: Validate uncertain knowledge
 *
 * Find knowledge with low epistemic levels and trigger validation.
 * Demonstrates conditional knowledge processing.
 */
export const validateUncertainKnowledge = (): QueryDefinition => {
  return query()
    .match(
      knowledge('k')
        .epistemicLevel('suspect')
        .minConfidence(0.6)
    )
    .forEach(send('@(knowledge)').ask('validate', { variable: 'k' }))
    .build();
};

/**
 * Example 62: Link decision to learnings
 *
 * Create relationships between decisions and supporting learnings.
 * Demonstrates knowledge graph building.
 */
export const createDecisionSupportNetwork = (
  decisionId: string,
  sessionId: string
): QueryDefinition => {
  return query()
    .match(
      decisions('dec').id(decisionId),
      learnings('learn').sessionId(sessionId)
    )
    .createRelationship('learn', 'dec', {
      type: 'supports',
      properties: {
        strength: 0.9,
        linkedAt: Date.now(),
        source: 'session-analysis',
      },
    })
    .return(['dec', 'learn'])
    .build();
};

/**
 * Example 63: Find knowledge requiring additional evidence
 *
 * Find knowledge items that need more evidence to increase confidence.
 * Demonstrates multi-level pattern matching.
 */
export const findKnowledgeNeedingEvidence = (): QueryDefinition => {
  return query()
    .match(
      knowledge('k')
        .epistemicLevel('wonder')
        .relatedTo('evidence', {
          type: 'requires',
          direction: 'outbound',
        })
    )
    .return(['k', 'evidence'])
    .build();
};

/**
 * Example 64: Aggregate knowledge by epistemic level
 *
 * Count knowledge items at each epistemic level.
 * Demonstrates knowledge aggregation.
 */
export const aggregateByEpistemicLevel = (): QueryDefinition => {
  return query()
    .match(knowledge('k'))
    .aggregate({
      operation: 'group',
      variable: 'k',
      by: 'epistemic_level',
      as: 'levelCounts',
    })
    .return(['levelCounts'])
    .build();
};

/**
 * Example 65: Find strongly supported knowledge
 *
 * Find knowledge with multiple strong supporting relationships.
 * Demonstrates relationship strength filtering.
 */
export const findStronglySupportedKnowledge = (): QueryDefinition => {
  return query()
    .match(
      knowledge('k')
        .minConfidence(0.9)
        .relatedTo('evidence', {
          type: 'supports',
          direction: 'inbound',
          minStrength: 0.8,
        })
    )
    .return(['k', 'evidence'])
    .build();
};

/**
 * Example 66: Update epistemic level based on evidence
 *
 * Promote knowledge to higher epistemic level when evidence accumulates.
 * Demonstrates conditional knowledge updates.
 */
export const promoteKnowledgeWithEvidence = (knowledgeId: string): QueryDefinition => {
  return query()
    .match(knowledge('k').id(knowledgeId))
    .forEach(
      send('@(knowledge)').tell('update-confidence', {
        newConfidence: 0.95,
        reason: 'Strong supporting evidence accumulated',
      })
    )
    .return(['k'])
    .build();
};

/**
 * Example 67: Find related knowledge by session
 *
 * Find all knowledge from a session and their relationships.
 * Demonstrates session-wide knowledge analysis.
 */
export const findSessionKnowledgeNetwork = (sessionId: string): QueryDefinition => {
  return query()
    .match(knowledge('k').sessionId(sessionId))
    .traverse(
      knowledgeTraversal('k', {
        direction: 'both',
        maxDepth: 2,
        as: 'related',
      })
    )
    .return(['k', 'related'])
    .build();
};

/**
 * Example 68: Find knowledge gaps
 *
 * Find decisions that lack supporting evidence.
 * Demonstrates NOT EXISTS pattern with knowledge.
 */
export const findDecisionsLackingEvidence = (): QueryDefinition => {
  return query()
    .match(
      decisions('dec')
        .epistemicLevel('believe')
        .notExists(
          knowledge('evidence')
            .relatedTo('dec', {
              type: 'supports',
              direction: 'inbound',
            })
        )
    )
    .return(['dec'])
    .build();
};

/**
 * Example 69: Query knowledge by multiple criteria
 *
 * Complex knowledge query with multiple filters.
 * Demonstrates advanced knowledge pattern matching.
 */
export const findRecentHighValueKnowledge = (sessionId: string): QueryDefinition => {
  const sevenDaysAgo = Date.now() - 7 * 24 * 60 * 60 * 1000;

  return query()
    .match(
      knowledge('k')
        .sessionId(sessionId)
        .epistemicLevel('know')
        .minConfidence(0.9)
    )
    .where(filter('k', 'created').gte(sevenDaysAgo))
    .return(['k'])
    .build();
};

/**
 * Example 70: Build knowledge dependency chain
 *
 * Create a chain of knowledge dependencies.
 * Demonstrates building knowledge relationships.
 */
export const buildKnowledgeDependencyChain = (
  learningId: string,
  decisionId: string,
  errorId: string
): QueryDefinition => {
  return query()
    .match(
      learnings('learn').id(learningId),
      decisions('dec').id(decisionId),
      errors('err').id(errorId)
    )
    .createRelationship('learn', 'dec', {
      type: 'supports',
      properties: { strength: 0.9 },
    })
    .createRelationship('err', 'learn', {
      type: 'requires',
      properties: { strength: 0.8 },
    })
    .return(['learn', 'dec', 'err'])
    .build();
};
