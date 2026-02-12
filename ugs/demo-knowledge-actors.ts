#!/usr/bin/env bun
/**
 * Demo: Graph-Addressable Knowledge System
 *
 * Demonstrates epistemic knowledge management through actor messages.
 * All knowledge items are graph-addressable and interact via messages.
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { KnowledgeActor } from './src/messaging/actors/knowledge.ts';
import { address, createMessage, generateCorrelationId } from './src/messaging/message.ts';

console.log('🧠 Graph-Addressable Knowledge System Demo\n');

// Initialize infrastructure
const store = new GraphStore();
const programManager = new ProgramManager(store);
const router = new MessageRouter(store, programManager);

// Create knowledge actor
const knowledgeActor = new KnowledgeActor('knowledge', router);

// Get an existing session_id from the database for demo purposes
import { createClient } from '@libsql/client';
const dbClient = createClient({
  url: `file:${process.env.HOME}/.claude/index/sessions-libsql.db`
});
const sessionResult = await dbClient.execute('SELECT id FROM sessions LIMIT 1');
const demoSessionId = sessionResult.rows[0]?.id as string || 'demo-session';
await dbClient.close();

console.log('✓ Infrastructure initialized\n');

// Demo 1: Create knowledge with different epistemic levels
console.log('📝 Demo 1: Creating Knowledge Items\n');

const wonderMsg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'decision',
    content: 'Should we implement graph-addressable knowledge?',
    reasoning: 'Seems promising but unvalidated',
    epistemic_level: 'wonder',
    confidence: 0.50,
    session_id: demoSessionId
  },
  {
    pattern: 'ask',
    from: address('domain/demo-user'),
    correlationId: generateCorrelationId()
  }
);

const wonderResult = await knowledgeActor.receive(wonderMsg);
console.log('Wonder (40-60%):', {
  address: wonderResult.payload?.address,
  content: wonderResult.payload?.item.content,
  confidence: wonderResult.payload?.item.confidence
});

const suspectMsg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'decision',
    content: 'Actor model works well for knowledge management',
    reasoning: 'Built foundation, early testing shows promise',
    epistemic_level: 'suspect',
    confidence: 0.75,
    evidence: [{
      type: 'HYPOTHESIS',
      description: 'Successfully created knowledge actor',
      confidence: 0.75
    }],
    session_id: demoSessionId
  },
  {
    pattern: 'ask',
    from: address('domain/demo-user'),
    correlationId: generateCorrelationId()
  }
);

const suspectResult = await knowledgeActor.receive(suspectMsg);
console.log('\nSuspect (60-80%):', {
  address: suspectResult.payload?.address,
  content: suspectResult.payload?.item.content,
  confidence: suspectResult.payload?.item.confidence,
  evidence: suspectResult.payload?.item.evidence.length + ' items'
});

const knowMsg = createMessage(
  address('services/knowledge'),
  'create',
  {
    category: 'learning',
    content: 'Message-based actors enable distributed knowledge systems',
    reasoning: 'Proven pattern from actor model theory',
    epistemic_level: 'know',
    confidence: 0.98,
    evidence: [{
      type: 'CITED',
      description: 'Erlang/Elixir actor systems demonstrate scalability',
      source: 'Actor Model literature',
      confidence: 0.98
    }],
    session_id: demoSessionId
  },
  {
    pattern: 'ask',
    from: address('domain/demo-user'),
    correlationId: generateCorrelationId()
  }
);

const knowResult = await knowledgeActor.receive(knowMsg);
console.log('\nKnow (95-100%):', {
  address: knowResult.payload?.address,
  content: knowResult.payload?.item.content,
  confidence: knowResult.payload?.item.confidence,
  evidence: knowResult.payload?.item.evidence
});

// Demo 2: Query knowledge by epistemic level
console.log('\n\n🔍 Demo 2: Querying Knowledge\n');

const queryMsg = createMessage(
  address('services/knowledge'),
  'query',
  {
    filter: {
      min_confidence: 0.6,
      session_id: demoSessionId
    },
    limit: 10
  },
  {
    pattern: 'ask',
    from: address('domain/demo-user'),
    correlationId: generateCorrelationId()
  }
);

const queryResult = await knowledgeActor.receive(queryMsg);
console.log(`Found ${queryResult.payload?.count} items with confidence >= 0.6:`);
queryResult.payload?.items.forEach((item: any) => {
  console.log(`  - [${item.epistemic_level}] ${item.content} (${Math.round(item.confidence * 100)}%)`);
});

// Demo 3: Add evidence and watch confidence evolve
console.log('\n\n🔬 Demo 3: Evidence-Driven Confidence Evolution\n');

// Get the suspect decision we created earlier
const suspectId = suspectResult.payload?.item.id;

console.log('Initial state:', {
  epistemic_level: suspectResult.payload?.item.epistemic_level,
  confidence: suspectResult.payload?.item.confidence
});

// Add new evidence
const evidenceMsg = createMessage(
  address('services/knowledge'),
  'add-evidence',
  {
    id: suspectId,
    evidence: {
      type: 'VALIDATED',
      description: 'Demo successfully ran end-to-end',
      confidence: 0.85
    }
  },
  {
    pattern: 'ask',
    from: address('domain/demo-user'),
    correlationId: generateCorrelationId()
  }
);

const evidenceResult = await knowledgeActor.receive(evidenceMsg);
console.log('\nAfter adding evidence:', {
  evidence_count: evidenceResult.payload?.item.evidence.length,
  latest_evidence: evidenceResult.payload?.item.evidence[evidenceResult.payload?.item.evidence.length - 1]
});

// Update confidence based on new evidence
const confidenceMsg = createMessage(
  address('services/knowledge'),
  'update-confidence',
  {
    id: suspectId,
    newConfidence: 0.88,
    reason: 'Validation successful, evidence accumulating'
  },
  {
    pattern: 'ask',
    from: address('domain/demo-user'),
    correlationId: generateCorrelationId()
  }
);

const confidenceResult = await knowledgeActor.receive(confidenceMsg);
console.log('\nConfidence updated:', {
  old: confidenceResult.payload?.oldConfidence,
  new: confidenceResult.payload?.newConfidence,
  promoted: confidenceResult.payload?.promoted,
  new_level: confidenceResult.payload?.item.epistemic_level
});

// Demo 4: Actor messaging patterns
console.log('\n\n📨 Demo 4: Actor Messaging Patterns\n');

// Using actor.ask() pattern
const directQuery = await knowledgeActor.ask(
  address('services/knowledge'),
  'query',
  {
    filter: { epistemic_level: 'suspect' }
  }
);

console.log(`Direct ask() pattern - Found ${directQuery.payload?.count} suspect items`);

// Using actor.tell() pattern (fire-and-forget)
await knowledgeActor.tell(
  address('services/knowledge'),
  'create',
  {
    category: 'learning',
    content: 'Fire-and-forget messaging works',
    epistemic_level: 'know',
    confidence: 0.96,
    session_id: demoSessionId
  }
);

console.log('Tell pattern (fire-and-forget) - Message sent without waiting for response');

// Demo 5: Statistics
console.log('\n\n📊 Demo 5: Knowledge Statistics\n');

const stats = await knowledgeActor.getStats();
console.log('Knowledge Actor Stats:', JSON.stringify(stats, null, 2));

// Demo 6: Show addresses
console.log('\n\n🏷️  Demo 6: Graph Addressing\n');

console.log('All knowledge items are addressable:');
console.log(`  Wonder decision:  ${wonderResult.payload?.address}`);
console.log(`  Suspect decision: ${suspectResult.payload?.address}`);
console.log(`  Know learning:    ${knowResult.payload?.address}`);

console.log('\n✓ Demo complete!\n');
console.log('Key Insights:');
console.log('  • Knowledge items are graph-addressable via @(knowledge/type/id)');
console.log('  • All interactions happen through messages (create, query, update)');
console.log('  • Epistemic levels (wonder → suspect → believe → know) with auto-promotion');
console.log('  • Evidence accumulation tracked with confidence evolution');
console.log('  • Consistent interface: same message protocol for all operations');
console.log('\nNext Steps:');
console.log('  • Connect to libSQL storage (replace Map)');
console.log('  • Add relationship actors for knowledge graph edges');
console.log('  • Implement task → knowledge integration');
console.log('  • Add streaming queries for real-time updates');
