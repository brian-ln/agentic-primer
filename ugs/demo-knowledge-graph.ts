#!/usr/bin/env bun
/**
 * Demo: Knowledge Graph with Relationships
 *
 * Shows how knowledge items connect through typed relationships,
 * forming a queryable knowledge graph.
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { KnowledgeActor } from './src/messaging/actors/knowledge.ts';
import { RelationshipActor } from './src/messaging/actors/relationship.ts';
import { address, createMessage, generateCorrelationId } from './src/messaging/message.ts';

console.log('🕸️  Knowledge Graph Demo\n');

// Initialize infrastructure
const store = new GraphStore();
const programManager = new ProgramManager(store);
const router = new MessageRouter(store, programManager);

// Create actors
const knowledgeActor = new KnowledgeActor('knowledge', router);
const relationshipActor = new RelationshipActor('relationships', router);

// Get an existing session_id from the database for demo purposes
import { createClient } from '@libsql/client';
const dbClient = createClient({
  url: `file:${process.env.HOME}/.claude/index/sessions-libsql.db`
});
const sessionResult = await dbClient.execute('SELECT id FROM sessions LIMIT 1');
const demoSessionId = sessionResult.rows[0]?.id as string || 'graph-demo';
await dbClient.close();

console.log('✓ Actors initialized\n');

// Demo 1: Build a knowledge graph
console.log('📊 Demo 1: Building Knowledge Graph\n');

// Create hypothesis
const hypothesisMsg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'decision',
    content: 'Graph-addressable actors improve knowledge management',
    epistemic_level: 'wonder',
    confidence: 0.50,
    session_id: demoSessionId
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const hypothesis = await knowledgeActor.receive(hypothesisMsg);
const hypothesisAddr = hypothesis.payload?.address;
console.log('Created hypothesis:', hypothesisAddr);

// Create supporting evidence
const evidence1Msg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'learning',
    content: 'Actor model enables distributed systems',
    epistemic_level: 'know',
    confidence: 0.98,
    evidence: [{ type: 'CITED', description: 'Erlang/Elixir systems', confidence: 0.98 }],
    session_id: demoSessionId
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const evidence1 = await knowledgeActor.receive(evidence1Msg);
const evidence1Addr = evidence1.payload?.address;
console.log('Created evidence 1:', evidence1Addr);

// Create second evidence
const evidence2Msg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'learning',
    content: 'Message-based communication scales well',
    epistemic_level: 'know',
    confidence: 0.96,
    evidence: [{ type: 'MEASURED', description: 'Benchmark results', confidence: 0.96 }],
    session_id: demoSessionId
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const evidence2 = await knowledgeActor.receive(evidence2Msg);
const evidence2Addr = evidence2.payload?.address;
console.log('Created evidence 2:', evidence2Addr);

// Create contradicting knowledge
const contradictionMsg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'error',
    content: 'Message overhead can slow down simple operations',
    epistemic_level: 'suspect',
    confidence: 0.70,
    session_id: demoSessionId
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const contradiction = await knowledgeActor.receive(contradictionMsg);
const contradictionAddr = contradiction.payload?.address;
console.log('Created contradiction:', contradictionAddr);

// Demo 2: Create relationships
console.log('\n\n🔗 Demo 2: Creating Relationships\n');

// Evidence supports hypothesis
const supports1Msg = createMessage(
  address('domain/relationships'),
  'create',
  {
    type: 'supports',
    from: evidence1Addr,
    to: hypothesisAddr,
    strength: 0.8,
    evidence: 'Distributed actor systems prove scalability of actor model'
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const supports1 = await relationshipActor.receive(supports1Msg);
console.log('Created "supports" relationship:', supports1.payload?.address);

// Second evidence also supports
const supports2Msg = createMessage(
  address('domain/relationships'),
  'create',
  {
    type: 'supports',
    from: evidence2Addr,
    to: hypothesisAddr,
    strength: 0.75,
    evidence: 'Messaging performance validates approach'
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const supports2 = await relationshipActor.receive(supports2Msg);
console.log('Created second "supports" relationship:', supports2.payload?.address);

// Contradiction questions hypothesis
const questionsMsg = createMessage(
  address('domain/relationships'),
  'create',
  {
    type: 'questions',
    from: contradictionAddr,
    to: hypothesisAddr,
    strength: 0.6,
    evidence: 'Performance overhead raises concerns'
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const questions = await relationshipActor.receive(questionsMsg);
console.log('Created "questions" relationship:', questions.payload?.address);

// Demo 3: Traverse the graph
console.log('\n\n🚶 Demo 3: Graph Traversal\n');

// Find all evidence supporting the hypothesis
const traverseMsg = createMessage(
  address('domain/relationships'),
  'traverse',
  {
    start: hypothesisAddr,
    direction: 'inbound',
    depth: 1,
    maxResults: 10
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const traverseResult = await relationshipActor.receive(traverseMsg);
console.log(`Found ${traverseResult.payload?.count} relationships to hypothesis:`);
traverseResult.payload?.paths.forEach((path: any) => {
  console.log(`  [${path.relationship.type}] ${path.node}`);
  console.log(`    Strength: ${path.relationship.strength}`);
  console.log(`    Evidence: ${path.relationship.evidence}`);
});

// Demo 4: Confidence propagation through relationships
console.log('\n\n📈 Demo 4: Confidence Propagation\n');

console.log('Initial hypothesis confidence:', hypothesis.payload?.item.confidence);
console.log('Supporting evidence:');
console.log(`  - Evidence 1: ${evidence1.payload?.item.confidence} (strength: 0.8)`);
console.log(`  - Evidence 2: ${evidence2.payload?.item.confidence} (strength: 0.75)`);
console.log('Questioning evidence:');
console.log(`  - Contradiction: ${contradiction.payload?.item.confidence} (strength: 0.6)`);

// Calculate weighted confidence based on relationships
const supportsQuery = createMessage(
  address('domain/relationships'),
  'query',
  {
    filter: {
      to: hypothesisAddr,
      type: 'supports'
    }
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const supportsRels = await relationshipActor.receive(supportsQuery);

const questionsQuery = createMessage(
  address('domain/relationships'),
  'query',
  {
    filter: {
      to: hypothesisAddr,
      type: 'questions'
    }
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const questionsRels = await relationshipActor.receive(questionsQuery);

// Simple confidence propagation algorithm
let positiveEvidence = 0;
let negativeEvidence = 0;

supportsRels.payload?.relationships.forEach((rel: any) => {
  positiveEvidence += rel.strength || 0.5;
});

questionsRels.payload?.relationships.forEach((rel: any) => {
  negativeEvidence += rel.strength || 0.5;
});

const propagatedConfidence = Math.min(0.95, Math.max(0.05,
  0.5 + (positiveEvidence * 0.2) - (negativeEvidence * 0.15)
));

console.log(`\nProposed confidence after propagation: ${Math.round(propagatedConfidence * 100)}%`);
console.log(`  Positive evidence weight: ${positiveEvidence.toFixed(2)}`);
console.log(`  Negative evidence weight: ${negativeEvidence.toFixed(2)}`);

// Update hypothesis confidence
const updateMsg = createMessage(
  address('services/knowledge'),
  'update-confidence',
  {
    id: hypothesis.payload?.item.id,
    newConfidence: propagatedConfidence,
    reason: 'Confidence propagated through graph relationships'
  },
  { pattern: 'ask', from: address('domain/demo'), correlationId: generateCorrelationId() }
);

const updated = await knowledgeActor.receive(updateMsg);
console.log(`\nHypothesis updated: ${hypothesis.payload?.item.epistemic_level} → ${updated.payload?.item.epistemic_level}`);
console.log(`Confidence: ${hypothesis.payload?.item.confidence} → ${updated.payload?.item.confidence}`);
console.log(`Promoted: ${updated.payload?.promoted}`);

// Demo 5: Statistics
console.log('\n\n📊 Demo 5: Graph Statistics\n');

const knowledgeStats = await knowledgeActor.getStats();
const relationshipStats = await relationshipActor.getStats();

console.log('Knowledge Stats:', JSON.stringify(knowledgeStats, null, 2));
console.log('\nRelationship Stats:', JSON.stringify(relationshipStats, null, 2));

console.log('\n✓ Demo complete!\n');
console.log('Key Insights:');
console.log('  • Knowledge items form graph nodes');
console.log('  • Relationships are typed edges (supports, questions, etc.)');
console.log('  • Graph traversal finds connected knowledge');
console.log('  • Confidence propagates through relationships');
console.log('  • Relationship strength weights evidence');
console.log('\nNext Steps:');
console.log('  • Connect to graph storage (replace Maps)');
console.log('  • Implement more propagation algorithms');
console.log('  • Add conflict detection (contradictory evidence)');
console.log('  • Build query DSL for complex graph patterns');
