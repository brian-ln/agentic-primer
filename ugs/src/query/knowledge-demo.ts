#!/usr/bin/env bun
/**
 * Knowledge Integration Demo
 *
 * Demonstrates how to use knowledge patterns, semantic search, and graph
 * traversal with the query/DSL system.
 */

import {
  query,
  knowledge,
  decisions,
  learnings,
  errors,
  knowledgeTraversal,
  send,
} from './index.ts';

/**
 * Example 1: Find high-confidence decisions
 */
console.log('Example 1: Find high-confidence decisions\n');

const highConfidenceDecisions = query()
  .match(
    decisions('dec')
      .epistemicLevel('know')
      .minConfidence(0.95)
  )
  .return(['dec'])
  .build();

console.log('Query:', JSON.stringify(highConfidenceDecisions, null, 2));

/**
 * Example 2: Find learnings from specific session
 */
console.log('\n\nExample 2: Find learnings from session\n');

const sessionLearnings = query()
  .match(learnings('learn').sessionId('f03b3b54...'))
  .return(['learn'])
  .build();

console.log('Query:', JSON.stringify(sessionLearnings, null, 2));

/**
 * Example 3: Traverse decision support network
 */
console.log('\n\nExample 3: Traverse decision support network\n');

const decisionEvidence = query()
  .match(decisions('dec').id('dec-123'))
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

console.log('Query:', JSON.stringify(decisionEvidence, null, 2));

/**
 * Example 4: Find contradicting knowledge
 */
console.log('\n\nExample 4: Find contradicting knowledge\n');

const contradictions = query()
  .match(
    knowledge('k1').epistemicLevel('believe'),
    knowledge('k2').relatedTo('k1', {
      type: 'contradicts',
      direction: 'outbound',
    })
  )
  .return(['k1', 'k2'])
  .build();

console.log('Query:', JSON.stringify(contradictions, null, 2));

/**
 * Example 5: Validate uncertain knowledge
 */
console.log('\n\nExample 5: Validate uncertain knowledge\n');

const validateUncertain = query()
  .match(
    knowledge('k')
      .epistemicLevel('suspect')
      .minConfidence(0.6)
  )
  .forEach(send('@(knowledge)').ask('validate', { variable: 'k' }))
  .build();

console.log('Query:', JSON.stringify(validateUncertain, null, 2));

/**
 * Example 6: Link decision to supporting learnings
 */
console.log('\n\nExample 6: Link decision to supporting learnings\n');

const linkKnowledge = query()
  .match(
    decisions('dec').id('dec-123'),
    learnings('learn').sessionId('session-456')
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

console.log('Query:', JSON.stringify(linkKnowledge, null, 2));

/**
 * Example 7: Find knowledge gaps (decisions lacking evidence)
 */
console.log('\n\nExample 7: Find knowledge gaps\n');

const knowledgeGaps = query()
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

console.log('Query:', JSON.stringify(knowledgeGaps, null, 2));

/**
 * Example 8: Aggregate knowledge by epistemic level
 */
console.log('\n\nExample 8: Aggregate by epistemic level\n');

const epistemicStats = query()
  .match(knowledge('k'))
  .aggregate({
    operation: 'group',
    variable: 'k',
    by: 'epistemic_level',
    as: 'levelCounts',
  })
  .return(['levelCounts'])
  .build();

console.log('Query:', JSON.stringify(epistemicStats, null, 2));

/**
 * Example 9: Multi-hop knowledge traversal
 */
console.log('\n\nExample 9: Multi-hop knowledge traversal\n');

const knowledgeNetwork = query()
  .match(decisions('dec').id('dec-123'))
  .traverse(
    knowledgeTraversal('dec', {
      direction: 'both',
      maxDepth: 3,
      as: 'network',
    })
  )
  .return(['dec', 'network'])
  .build();

console.log('Query:', JSON.stringify(knowledgeNetwork, null, 2));

/**
 * Example 10: Complex filtering with multiple criteria
 */
console.log('\n\nExample 10: Complex knowledge query\n');

const complexQuery = query()
  .match(
    knowledge('k')
      .category('decision')
      .epistemicLevel('know')
      .minConfidence(0.9)
      .sessionId('session-789')
  )
  .return(['k'])
  .build();

console.log('Query:', JSON.stringify(complexQuery, null, 2));

console.log('\n\n✅ Knowledge integration demo complete!\n');
console.log('Key Features Demonstrated:');
console.log('  - Knowledge pattern matching (decisions, learnings, errors)');
console.log('  - Epistemic level filtering');
console.log('  - Confidence-based queries');
console.log('  - Graph traversal over relationships');
console.log('  - Knowledge gap detection');
console.log('  - Relationship creation');
console.log('  - Aggregation and analytics');
console.log('  - Multi-hop traversal');
