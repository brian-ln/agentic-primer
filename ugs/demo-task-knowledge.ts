#!/usr/bin/env bun
/**
 * Demo: Task-Knowledge Integration
 *
 * Shows how tasks interact with knowledge actors to create and update knowledge
 * as part of their workflow.
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { KnowledgeActor } from './src/messaging/actors/knowledge.ts';
import { RelationshipActor } from './src/messaging/actors/relationship.ts';
import { TaskActor } from './src/messaging/actors/task.ts';
import { address, createMessage, generateCorrelationId } from './src/messaging/message.ts';
import { createClient } from '@libsql/client';

console.log('🔄 Task-Knowledge Integration Demo\n');

// Initialize infrastructure
const store = new GraphStore();
const programManager = new ProgramManager(store);
const router = new MessageRouter(store, programManager);

// Create actors
const knowledgeActor = new KnowledgeActor('knowledge', router);
const relationshipActor = new RelationshipActor('relationships', router);
const taskActor = new TaskActor('tasks', router, store);

// Register actors with router
router.registerActor('services/knowledge', knowledgeActor);
router.registerActor('domain/relationships', relationshipActor);
router.registerActor('domain/tasks', taskActor);

// Get an existing session_id from the database for demo purposes
const dbClient = createClient({
  url: `file:${process.env.HOME}/.claude/index/sessions-libsql.db`
});
const sessionResult = await dbClient.execute('SELECT id FROM sessions LIMIT 1');
const demoSessionId = sessionResult.rows[0]?.id as string || 'task-demo';
await dbClient.close();

console.log('✓ All actors initialized\n');

// Demo 1: Create a task to validate a hypothesis
console.log('📋 Demo 1: Creating Task\n');

const createTaskMsg = createMessage(
  address('domain/tasks'),
  'create',
  {
    id: 'validate-hypothesis-001',
    title: 'Validate: Graph-addressable actors improve maintainability',
    description: 'Investigate whether graph-addressable actor architecture actually improves code maintainability',
    priority: 'P1',
    assignee: '@(agents/validator)'  // Assign to an agent so it can be started
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const taskResult = await taskActor.receive(createTaskMsg);
console.log('Created task:', taskResult.payload?.address);
console.log('Task details:', {
  title: taskResult.payload?.task.config.title,
  lifecycle: taskResult.payload?.task.lifecycle
});

// Demo 2: Task creates initial knowledge item (wonder level)
console.log('\n\n💭 Demo 2: Task Creates Initial Hypothesis\n');

const createHypothesisMsg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'decision',
    content: 'Graph-addressable actors improve code maintainability',
    reasoning: 'Initial hypothesis before investigation',
    epistemic_level: 'wonder',
    confidence: 0.45,
    session_id: demoSessionId,
    evidence: [{
      type: 'HYPOTHESIS',
      description: 'Based on architectural patterns from other systems',
      confidence: 0.45
    }]
  },
  { pattern: 'ask', from: address('tasks/validate-hypothesis-001'), correlationId: generateCorrelationId() }
);

const hypothesisResult = await knowledgeActor.receive(createHypothesisMsg);
const hypothesisAddr = hypothesisResult.payload?.address;
console.log('Hypothesis created:', hypothesisAddr);
console.log('Initial state:', {
  content: hypothesisResult.payload?.item.content,
  epistemic_level: hypothesisResult.payload?.item.epistemic_level,
  confidence: hypothesisResult.payload?.item.confidence
});

// Demo 3: Task starts work
console.log('\n\n🚀 Demo 3: Task Starts Investigation\n');

const startTaskMsg = createMessage(
  address('domain/tasks'),
  'start',
  { id: 'validate-hypothesis-001' },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const startResult = await taskActor.receive(startTaskMsg);
console.log('Task started - lifecycle:', startResult.payload?.task.lifecycle);

// Demo 4: Task finds supporting evidence
console.log('\n\n🔬 Demo 4: Task Discovers Evidence\n');

// Simulate task finding evidence during its work
const evidence1Msg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'learning',
    content: 'Message-based communication reduces coupling between components',
    reasoning: 'Observed in existing graph-message-layer branch',
    epistemic_level: 'believe',
    confidence: 0.92,
    session_id: demoSessionId,
    evidence: [{
      type: 'MEASURED',
      description: 'Code review showed clear component boundaries',
      confidence: 0.92
    }]
  },
  { pattern: 'ask', from: address('tasks/validate-hypothesis-001'), correlationId: generateCorrelationId() }
);

const evidence1Result = await knowledgeActor.receive(evidence1Msg);
const evidence1Addr = evidence1Result.payload?.address;
console.log('Evidence found:', evidence1Addr);

// Create relationship: evidence supports hypothesis
const supportsMsg = createMessage(
  address('domain/relationships'),
  'create',
  {
    type: 'supports',
    from: evidence1Addr,
    to: hypothesisAddr,
    strength: 0.85,
    evidence: 'Decoupling is key to maintainability'
  },
  { pattern: 'ask', from: address('tasks/validate-hypothesis-001'), correlationId: generateCorrelationId() }
);

const supportsResult = await relationshipActor.receive(supportsMsg);
console.log('Relationship created:', supportsResult.payload?.address);

// Demo 5: Task updates hypothesis confidence based on findings
console.log('\n\n📈 Demo 5: Task Updates Hypothesis Confidence\n');

const updateConfidenceMsg = createMessage(
  address('services/knowledge'),
  'update-confidence',
  {
    id: hypothesisResult.payload?.item.id,
    newConfidence: 0.78,
    reason: 'Strong evidence found supporting the hypothesis'
  },
  { pattern: 'ask', from: address('tasks/validate-hypothesis-001'), correlationId: generateCorrelationId() }
);

const confidenceResult = await knowledgeActor.receive(updateConfidenceMsg);
console.log('Confidence updated:', {
  old: confidenceResult.payload?.oldConfidence,
  new: confidenceResult.payload?.newConfidence,
  promoted: confidenceResult.payload?.promoted,
  new_level: confidenceResult.payload?.item.epistemic_level
});

// Demo 6: Task completes and creates learning
console.log('\n\n✅ Demo 6: Task Completion Creates Learning\n');

const completeTaskMsg = createMessage(
  address('domain/tasks'),
  'complete',
  {
    id: 'validate-hypothesis-001',
    result: 'Investigation confirms that graph-addressable actors improve maintainability through better decoupling',
    createKnowledge: {
      category: 'learning',
      content: 'Graph-addressable architecture enables better component isolation and testability',
      reasoning: 'Validated through code review and comparison with previous approaches',
      epistemic_level: 'believe',
      confidence: 0.85,
      session_id: demoSessionId,
      evidence: [{
        type: 'VALIDATED',
        description: 'Task validate-hypothesis-001 completed successfully',
        confidence: 0.85
      }]
    }
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const completeResult = await taskActor.receive(completeTaskMsg);
console.log('Task completed - lifecycle:', completeResult.payload?.task.lifecycle);
console.log('Learning created:', completeResult.payload?.knowledgeCreated);

// Demo 7: Query final knowledge state
console.log('\n\n📊 Demo 7: Final Knowledge Graph State\n');

const queryMsg = createMessage(
  address('services/knowledge'),
  'query',
  {
    filter: { session_id: demoSessionId, min_confidence: 0.7 }
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const queryResult = await knowledgeActor.receive(queryMsg);
console.log(`Found ${queryResult.payload?.count} high-confidence knowledge items:`);
queryResult.payload?.items.forEach((item: any) => {
  console.log(`  - [${item.epistemic_level}] ${item.content} (${Math.round(item.confidence * 100)}%)`);
});

// Query relationships
const relQueryMsg = createMessage(
  address('domain/relationships'),
  'query',
  {},
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const relQueryResult = await relationshipActor.receive(relQueryMsg);
console.log(`\nRelationships: ${relQueryResult.payload?.count} total`);

console.log('\n✓ Demo complete!\n');
console.log('Key Insights:');
console.log('  • Tasks can create knowledge items during their lifecycle');
console.log('  • Tasks send messages to @(knowledge) and @(relationships)');
console.log('  • Task completion can automatically create learning knowledge');
console.log('  • Knowledge confidence evolves as task progresses');
console.log('  • Full traceability: knowledge → task that created it');
console.log('\nNext Steps:');
console.log('  • Auto-detect knowledge gaps and create tasks');
console.log('  • Tasks spawn agents that update knowledge');
console.log('  • Confidence propagation triggers task creation');
