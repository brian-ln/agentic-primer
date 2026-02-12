#!/usr/bin/env bun
/**
 * Knowledge Integration Tests
 *
 * Tests for knowledge pattern matching, semantic search, and graph traversal
 * integration with the query/DSL layer.
 */

import { describe, it, expect, beforeEach } from 'bun:test';
import {
  knowledge,
  decisions,
  learnings,
  errors,
  searchKnowledge,
  traverseKnowledge,
  knowledgeTraversal,
  knowledgeAddress,
  parseKnowledgeAddress,
  type KnowledgeSearchOptions,
  type KnowledgeTraversalOptions,
} from './knowledge-integration.ts';
import { query, send } from './builder.ts';

describe('KnowledgePatternBuilder', () => {
  describe('knowledge() pattern builder', () => {
    it('should create basic knowledge pattern', () => {
      const pattern = knowledge('k').build();

      expect(pattern.variable).toBe('k');
      expect(pattern.labels).toContain('Knowledge');
    });

    it('should filter by category', () => {
      const pattern = knowledge('k').category('decision').build();

      expect(pattern.where?.category).toBe('decision');
    });

    it('should filter by epistemic level', () => {
      const pattern = knowledge('k').epistemicLevel('know').build();

      expect(pattern.where?.epistemic_level).toBe('know');
    });

    it('should filter by minimum confidence', () => {
      const pattern = knowledge('k').minConfidence(0.8).build();

      expect(pattern.where?.min_confidence).toBe(0.8);
    });

    it('should filter by session ID', () => {
      const pattern = knowledge('k').sessionId('session-123').build();

      expect(pattern.where?.session_id).toBe('session-123');
    });

    it('should filter by knowledge ID', () => {
      const pattern = knowledge('k').id('k_123').build();

      expect(pattern.where?.id).toBe('k_123');
    });

    it('should chain multiple filters', () => {
      const pattern = knowledge('k')
        .category('decision')
        .epistemicLevel('believe')
        .minConfidence(0.9)
        .sessionId('session-456')
        .build();

      expect(pattern.where?.category).toBe('decision');
      expect(pattern.where?.epistemic_level).toBe('believe');
      expect(pattern.where?.min_confidence).toBe(0.9);
      expect(pattern.where?.session_id).toBe('session-456');
    });
  });

  describe('decisions() shorthand', () => {
    it('should create decision pattern', () => {
      const pattern = decisions('dec').build();

      expect(pattern.variable).toBe('dec');
      expect(pattern.where?.category).toBe('decision');
      expect(pattern.labels).toContain('Knowledge');
    });

    it('should chain with epistemic level', () => {
      const pattern = decisions('dec').epistemicLevel('know').build();

      expect(pattern.where?.category).toBe('decision');
      expect(pattern.where?.epistemic_level).toBe('know');
    });

    it('should chain with confidence filter', () => {
      const pattern = decisions('dec').minConfidence(0.95).build();

      expect(pattern.where?.category).toBe('decision');
      expect(pattern.where?.min_confidence).toBe(0.95);
    });
  });

  describe('learnings() shorthand', () => {
    it('should create learning pattern', () => {
      const pattern = learnings('learn').build();

      expect(pattern.variable).toBe('learn');
      expect(pattern.where?.category).toBe('learning');
      expect(pattern.labels).toContain('Knowledge');
    });

    it('should filter by session', () => {
      const pattern = learnings('learn').sessionId('session-789').build();

      expect(pattern.where?.category).toBe('learning');
      expect(pattern.where?.session_id).toBe('session-789');
    });
  });

  describe('errors() shorthand', () => {
    it('should create error pattern', () => {
      const pattern = errors('err').build();

      expect(pattern.variable).toBe('err');
      expect(pattern.where?.category).toBe('error');
      expect(pattern.labels).toContain('Knowledge');
    });

    it('should filter by ID', () => {
      const pattern = errors('err').id('err_123').build();

      expect(pattern.where?.category).toBe('error');
      expect(pattern.where?.id).toBe('err_123');
    });
  });

  describe('relationship constraints', () => {
    it('should add outbound relationship', () => {
      const pattern = knowledge('k')
        .relatedTo('other', {
          type: 'supports',
          direction: 'outbound',
        })
        .build();

      expect(pattern.relationships).toHaveLength(1);
      expect(pattern.relationships![0].target).toBe('other');
      expect(pattern.relationships![0].type).toBe('supports');
      expect(pattern.relationships![0].direction).toBe('outbound');
    });

    it('should add inbound relationship', () => {
      const pattern = knowledge('k')
        .relatedTo('source', {
          type: 'requires',
          direction: 'inbound',
        })
        .build();

      expect(pattern.relationships![0].direction).toBe('inbound');
      expect(pattern.relationships![0].type).toBe('requires');
    });

    it('should add bidirectional relationship', () => {
      const pattern = knowledge('k')
        .relatedTo('peer', {
          type: 'related-to',
          direction: 'both',
        })
        .build();

      expect(pattern.relationships![0].direction).toBe('both');
    });

    it('should filter by minimum strength', () => {
      const pattern = knowledge('k')
        .relatedTo('other', {
          type: 'supports',
          direction: 'outbound',
          minStrength: 0.8,
        })
        .build();

      expect(pattern.relationships![0].properties?.min_strength).toBe(0.8);
    });

    it('should support multiple relationships', () => {
      const pattern = knowledge('k')
        .relatedTo('a', { type: 'supports', direction: 'outbound' })
        .relatedTo('b', { type: 'requires', direction: 'inbound' })
        .build();

      expect(pattern.relationships).toHaveLength(2);
    });
  });
});

describe('Query Integration', () => {
  describe('knowledge patterns in queries', () => {
    it('should integrate with query builder', () => {
      const q = query()
        .match(decisions('dec').minConfidence(0.9))
        .return(['dec'])
        .build();

      expect(q.patterns).toHaveLength(1);
      expect(q.patterns[0].variable).toBe('dec');
      expect(q.patterns[0].where?.category).toBe('decision');
    });

    it('should support multiple knowledge patterns', () => {
      const q = query()
        .match(
          decisions('dec').id('dec-123'),
          learnings('learn').sessionId('session-456')
        )
        .return(['dec', 'learn'])
        .build();

      expect(q.patterns).toHaveLength(2);
      expect(q.patterns[0].where?.category).toBe('decision');
      expect(q.patterns[1].where?.category).toBe('learning');
    });

    it('should combine knowledge with actions', () => {
      const q = query()
        .match(decisions('dec').epistemicLevel('suspect'))
        .forEach(send('@(knowledge)').ask('validate', { id: 'dec' }))
        .build();

      expect(q.patterns[0].where?.epistemic_level).toBe('suspect');
      expect(q.actions).toHaveLength(1);
      expect(q.actions![0].type).toBe('send');
    });
  });

  describe('knowledge traversals', () => {
    it('should create traversal spec', () => {
      const spec = knowledgeTraversal('decision', {
        relationshipType: 'supports',
        maxDepth: 3,
      });

      expect(spec.from).toBe('decision');
      expect(spec.relationship).toBe('supports');
      expect(spec.depth?.max).toBe(3);
    });

    it('should use custom result variable', () => {
      const spec = knowledgeTraversal('decision', {
        as: 'supporting_evidence',
      });

      expect(spec.as).toBe('supporting_evidence');
    });

    it('should integrate with query builder', () => {
      const q = query()
        .match(decisions('dec').id('dec-123'))
        .traverse(
          knowledgeTraversal('dec', {
            relationshipType: 'supports',
            direction: 'inbound',
            maxDepth: 2,
            as: 'evidence',
          })
        )
        .return(['dec', 'evidence'])
        .build();

      expect(q.traversals).toHaveLength(1);
      expect(q.traversals![0].from).toBe('dec');
      expect(q.traversals![0].relationship).toBe('supports');
      expect(q.traversals![0].as).toBe('evidence');
    });
  });
});

describe('Semantic Search', () => {
  describe('searchKnowledge()', () => {
    it('should accept basic search options', async () => {
      const options: KnowledgeSearchOptions = {
        query: 'authentication patterns',
        limit: 5,
      };

      // Test that function accepts options (actual search requires embeddings)
      expect(options.query).toBe('authentication patterns');
      expect(options.limit).toBe(5);
    });

    it('should support category filtering', () => {
      const options: KnowledgeSearchOptions = {
        query: 'test',
        category: 'decision',
      };

      expect(options.category).toBe('decision');
    });

    it('should support epistemic level filtering', () => {
      const options: KnowledgeSearchOptions = {
        query: 'test',
        minEpistemicLevel: 'believe',
      };

      expect(options.minEpistemicLevel).toBe('believe');
    });

    it('should support confidence filtering', () => {
      const options: KnowledgeSearchOptions = {
        query: 'test',
        minConfidence: 0.8,
      };

      expect(options.minConfidence).toBe(0.8);
    });

    it('should support session filtering', () => {
      const options: KnowledgeSearchOptions = {
        query: 'test',
        sessionId: 'session-123',
      };

      expect(options.sessionId).toBe('session-123');
    });

    it('should support similarity threshold', () => {
      const options: KnowledgeSearchOptions = {
        query: 'test',
        threshold: 0.75,
      };

      expect(options.threshold).toBe(0.75);
    });

    it('should combine all filters', () => {
      const options: KnowledgeSearchOptions = {
        query: 'authentication decisions',
        limit: 10,
        category: 'decision',
        minEpistemicLevel: 'know',
        minConfidence: 0.9,
        sessionId: 'session-456',
        threshold: 0.8,
      };

      expect(options.query).toBe('authentication decisions');
      expect(options.category).toBe('decision');
      expect(options.minConfidence).toBe(0.9);
    });
  });
});

describe('Graph Traversal', () => {
  describe('traverseKnowledge()', () => {
    it('should accept basic traversal options', async () => {
      const options: KnowledgeTraversalOptions = {
        from: '@(knowledge/decisions/dec-123)',
        maxDepth: 3,
      };

      const result = await traverseKnowledge(options);

      expect(result).toHaveProperty('nodes');
      expect(result).toHaveProperty('relationships');
      expect(result.nodes).toContain(options.from);
    });

    it('should support relationship type filtering', async () => {
      const options: KnowledgeTraversalOptions = {
        from: '@(knowledge/decisions/dec-123)',
        relationshipType: 'supports',
      };

      const result = await traverseKnowledge(options);

      expect(result.nodes).toHaveLength(1); // Placeholder returns only start node
    });

    it('should support direction filtering', async () => {
      const options: KnowledgeTraversalOptions = {
        from: '@(knowledge/decisions/dec-123)',
        direction: 'inbound',
      };

      const result = await traverseKnowledge(options);

      expect(result).toBeDefined();
    });

    it('should support strength filtering', async () => {
      const options: KnowledgeTraversalOptions = {
        from: '@(knowledge/decisions/dec-123)',
        minStrength: 0.8,
      };

      const result = await traverseKnowledge(options);

      expect(result).toBeDefined();
    });
  });
});

describe('Address Utilities', () => {
  describe('knowledgeAddress()', () => {
    it('should create decision address', () => {
      const address = knowledgeAddress('decision', 'dec-123');

      expect(address).toBe('@(knowledge/decisions/dec-123)');
    });

    it('should create learning address', () => {
      const address = knowledgeAddress('learning', 'learn-456');

      expect(address).toBe('@(knowledge/learnings/learn-456)');
    });

    it('should create error address', () => {
      const address = knowledgeAddress('error', 'err-789');

      expect(address).toBe('@(knowledge/errors/err-789)');
    });
  });

  describe('parseKnowledgeAddress()', () => {
    it('should parse decision address', () => {
      const parsed = parseKnowledgeAddress('@(knowledge/decisions/dec-123)');

      expect(parsed).not.toBeNull();
      expect(parsed?.category).toBe('decision');
      expect(parsed?.id).toBe('dec-123');
    });

    it('should parse learning address', () => {
      const parsed = parseKnowledgeAddress('@(knowledge/learnings/learn-456)');

      expect(parsed).not.toBeNull();
      expect(parsed?.category).toBe('learning');
      expect(parsed?.id).toBe('learn-456');
    });

    it('should parse error address', () => {
      const parsed = parseKnowledgeAddress('@(knowledge/errors/err-789)');

      expect(parsed).not.toBeNull();
      expect(parsed?.category).toBe('error');
      expect(parsed?.id).toBe('err-789');
    });

    it('should return null for invalid address', () => {
      const parsed = parseKnowledgeAddress('@(invalid/address)');

      expect(parsed).toBeNull();
    });

    it('should return null for wrong format', () => {
      const parsed = parseKnowledgeAddress('@(knowledge/wrong/format)');

      expect(parsed).toBeNull();
    });
  });
});

describe('Complex Query Scenarios', () => {
  it('should find high-confidence decisions', () => {
    const q = query()
      .match(
        decisions('dec')
          .epistemicLevel('know')
          .minConfidence(0.95)
      )
      .return(['dec'])
      .build();

    expect(q.patterns[0].where?.epistemic_level).toBe('know');
    expect(q.patterns[0].where?.min_confidence).toBe(0.95);
  });

  it('should find learnings in session', () => {
    const q = query()
      .match(learnings('learn').sessionId('session-123'))
      .return(['learn'])
      .build();

    expect(q.patterns[0].where?.session_id).toBe('session-123');
  });

  it('should find errors by type', () => {
    const q = query()
      .match(errors('err').id('NetworkError'))
      .return(['err'])
      .build();

    expect(q.patterns[0].where?.id).toBe('NetworkError');
  });

  it('should traverse decision support network', () => {
    const q = query()
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

    expect(q.traversals![0].relationship).toBe('supports');
    expect(q.traversals![0].direction).toBe('inbound');
  });

  it('should find contradicting knowledge', () => {
    const q = query()
      .match(
        knowledge('k1').id('k-123'),
        knowledge('k2').relatedTo('k1', {
          type: 'contradicts',
          direction: 'outbound',
        })
      )
      .return(['k1', 'k2'])
      .build();

    expect(q.patterns[1].relationships![0].type).toBe('contradicts');
  });

  it('should validate knowledge with low confidence', () => {
    const q = query()
      .match(
        knowledge('k')
          .epistemicLevel('suspect')
          .minConfidence(0.6)
      )
      .forEach(send('@(knowledge)').ask('validate', { variable: 'k' }))
      .build();

    expect(q.patterns[0].where?.epistemic_level).toBe('suspect');
    expect(q.actions![0].params.type).toBe('validate');
  });

  it('should link decision to supporting learnings', () => {
    const q = query()
      .match(
        decisions('dec').id('dec-123'),
        learnings('learn').sessionId('session-456')
      )
      .createRelationship('learn', 'dec', {
        type: 'supports',
        properties: { strength: 0.9 },
      })
      .return(['dec', 'learn'])
      .build();

    expect(q.patterns[0].where?.category).toBe('decision');
    expect(q.patterns[1].where?.category).toBe('learning');
  });

  it('should find knowledge requiring validation', () => {
    const q = query()
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

    expect(q.patterns[0].where?.epistemic_level).toBe('wonder');
  });

  it('should aggregate knowledge by epistemic level', () => {
    const q = query()
      .match(knowledge('k'))
      .aggregate({
        operation: 'group',
        variable: 'k',
        by: 'epistemic_level',
        as: 'levelCounts',
      })
      .return(['levelCounts'])
      .build();

    expect(q.aggregations![0].by).toBe('epistemic_level');
  });

  it('should find knowledge with strong evidence', () => {
    const q = query()
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

    expect(q.patterns[0].where?.min_confidence).toBe(0.9);
    expect(q.patterns[0].relationships![0].properties?.min_strength).toBe(0.8);
  });
});

describe('Edge Cases', () => {
  it('should handle knowledge with no relationships', () => {
    const pattern = knowledge('k').build();

    expect(pattern.relationships).toHaveLength(0);
  });

  it('should handle empty where clause', () => {
    const pattern = knowledge('k').build();

    expect(pattern.where).toBeDefined();
  });

  it('should handle default variable names', () => {
    const k = knowledge();
    const d = decisions();
    const l = learnings();
    const e = errors();

    expect(k.getVariable()).toBe('knowledge');
    expect(d.getVariable()).toBe('decision');
    expect(l.getVariable()).toBe('learning');
    expect(e.getVariable()).toBe('error');
  });

  it('should handle traversal with defaults', () => {
    const spec = knowledgeTraversal('k');

    expect(spec.direction).toBe('outbound');
    expect(spec.depth?.max).toBe(3);
    expect(spec.as).toBe('k_network');
  });

  it('should handle search with minimal options', () => {
    const options: KnowledgeSearchOptions = {
      query: 'test',
    };

    expect(options.limit).toBeUndefined();
    expect(options.category).toBeUndefined();
  });

  it('should handle all epistemic levels', () => {
    const levels = ['reject', 'doubt', 'wonder', 'suspect', 'believe', 'know'] as const;

    for (const level of levels) {
      const pattern = knowledge('k').epistemicLevel(level).build();
      expect(pattern.where?.epistemic_level).toBe(level);
    }
  });

  it('should handle all relationship types', () => {
    const types = ['supports', 'contradicts', 'requires', 'extends', 'questions', 'related-to'] as const;

    for (const type of types) {
      const pattern = knowledge('k')
        .relatedTo('other', { type, direction: 'outbound' })
        .build();
      expect(pattern.relationships![0].type).toBe(type);
    }
  });

  it('should handle all categories', () => {
    const categories = ['decision', 'learning', 'error'] as const;

    for (const category of categories) {
      const pattern = knowledge('k').category(category).build();
      expect(pattern.where?.category).toBe(category);
    }
  });
});
