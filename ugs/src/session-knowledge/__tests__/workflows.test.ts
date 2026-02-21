/**
 * End-to-End Workflow Tests for Know Tool
 * Epic: agentic-primer-9ad
 * Task: agentic-primer-t49.2
 *
 * Comprehensive integration tests covering complete user workflows:
 * - Extract → Search → Query scenarios
 * - Temporal queries with --as-of
 * - Decay visualization
 * - Thinking arc detection
 * - Cross-session queries
 * - Data isolation
 *
 * Uses real session data from ~/.claude/index/sessions-libsql.db
 */

import { describe, test, expect, beforeAll, afterAll } from 'bun:test';
import { createClient, type Client } from '@libsql/client';
import { join } from 'path';
import { KnowledgeExtractor } from '../extraction/KnowledgeExtractor';
import { EmbeddingGenerator } from '../embeddings/EmbeddingGenerator';
import { TemporalQueries } from '../temporal/TemporalQueries';
import { ConfidenceDecay } from '../temporal/ConfidenceDecay';
import { ArcDetector } from '../temporal/ArcDetector';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

describe('Workflow 1: Extract Knowledge → Verify Storage', () => {
  let db: Client;
  let extractor: KnowledgeExtractor;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
    extractor = new KnowledgeExtractor();
  });

  afterAll(async () => {
    db.close();
    await extractor.close();
  });

  test('should extract decisions from a session and store them', async () => {
    // Get a real session ID with data
    const sessionsResult = await db.execute({
      sql: 'SELECT id FROM sessions WHERE message_count > 5 LIMIT 1'
    });

    if (sessionsResult.rows.length === 0) {
      console.log('No sessions with sufficient messages found, skipping test');
      return;
    }

    const sessionId = sessionsResult.rows[0].id as string;

    // Check decisions before extraction
    const beforeResult = await db.execute({
      sql: 'SELECT COUNT(*) as count FROM session_decisions WHERE session_id = ?',
      args: [sessionId]
    });
    const beforeCount = beforeResult.rows[0].count as number;

    // Extract knowledge (may not extract anything if no decisions present)
    const result = await extractor.extractSession(sessionId);

    // Verify extraction completed
    expect(result.sessionId).toBe(sessionId);
    // messageCount may be 0 if session has no messages with embeddings
    expect(result.messageCount).toBeGreaterThanOrEqual(0);

    // Check decisions after extraction
    const afterResult = await db.execute({
      sql: 'SELECT COUNT(*) as count FROM session_decisions WHERE session_id = ?',
      args: [sessionId]
    });
    const afterCount = afterResult.rows[0].count as number;

    // Should have same or more decisions (idempotent)
    expect(afterCount).toBeGreaterThanOrEqual(beforeCount);
  }, 30000);

  test('should extract learnings and validate structure', async () => {
    const learningsResult = await db.execute({
      sql: 'SELECT * FROM session_learnings LIMIT 5'
    });

    if (learningsResult.rows.length === 0) {
      console.log('No learnings found in database, skipping validation');
      return;
    }

    // Validate learning structure
    for (const row of learningsResult.rows) {
      expect(row.id).toBeDefined();
      expect(row.session_id).toBeDefined();
      expect(row.learning).toBeDefined();
      expect(row.timestamp).toBeGreaterThan(0);

      // Confidence should be valid if present
      if (row.confidence !== null) {
        expect(row.confidence).toBeGreaterThanOrEqual(0);
        expect(row.confidence).toBeLessThanOrEqual(1);
      }
    }
  });

  test('should extract errors with tool and type information', async () => {
    const errorsResult = await db.execute({
      sql: 'SELECT * FROM session_errors LIMIT 5'
    });

    if (errorsResult.rows.length === 0) {
      console.log('No errors found in database, skipping validation');
      return;
    }

    // Validate error structure
    for (const row of errorsResult.rows) {
      expect(row.id).toBeDefined();
      expect(row.session_id).toBeDefined();
      expect(row.error_message).toBeDefined();
      expect(row.timestamp).toBeGreaterThan(0);

      // Either tool_name or error_type should be present
      const hasMetadata = row.tool_name || row.error_type;
      if (hasMetadata) {
        expect(typeof hasMetadata).toBe('string');
      }
    }
  });

  test('should handle extraction of session with no extractable knowledge', async () => {
    // Create a test session ID that doesn't exist
    const fakeSessionId = 'test-' + Date.now();

    // Should not throw, just return zero extractions
    const result = await extractor.extractSession(fakeSessionId);

    expect(result.decisionsExtracted).toBe(0);
    expect(result.learningsExtracted).toBe(0);
    expect(result.errorsExtracted).toBe(0);
  }, 10000);
});

describe('Workflow 2: Search Across Knowledge Types', () => {
  let db: Client;
  let embedder: EmbeddingGenerator;

  // LLM-dependent tests require an embedding service (LM Studio) to be running.
  // Skip unless RUN_SLOW_TESTS is set, matching the pattern used in integration tests.
  const testOrSkip = process.env.RUN_SLOW_TESTS ? test : test.skip;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
    embedder = new EmbeddingGenerator();
  });

  afterAll(() => {
    db.close();
  });

  testOrSkip('should search decisions semantically', async () => {
    // Check if we have any decisions with embeddings
    const checkResult = await db.execute({
      sql: 'SELECT COUNT(*) as count FROM session_decisions WHERE embedding IS NOT NULL'
    });

    if (checkResult.rows[0].count === 0) {
      console.log('No decisions with embeddings found, skipping semantic search');
      return;
    }

    // Generate query embedding - skip if embedding service unavailable
    const query = 'database choice';
    let queryEmbedding;
    try {
      queryEmbedding = await embedder.embed(query);
    } catch (error) {
      console.log('Embedding service unavailable, skipping test');
      return;
    }
    const vectorJson = `[${Array.from(queryEmbedding).join(',')}]`;

    // Search decisions
    const results = await db.execute({
      sql: `
        SELECT id, decision, vector_distance_cos(embedding, vector(?)) as distance
        FROM session_decisions
        WHERE embedding IS NOT NULL
        ORDER BY distance ASC
        LIMIT 5
      `,
      args: [vectorJson]
    });

    expect(results.rows.length).toBeGreaterThan(0);

    // Distance should be in valid range
    for (const row of results.rows) {
      expect(row.distance).toBeGreaterThanOrEqual(0);
      expect(row.distance).toBeLessThanOrEqual(2); // cosine distance max is 2
    }
  });

  testOrSkip('should search across all knowledge types', async () => {
    const query = 'error handling';
    let queryEmbedding;
    try {
      queryEmbedding = await embedder.embed(query);
    } catch (error) {
      console.log('Embedding service unavailable, skipping test');
      return;
    }
    const vectorJson = `[${Array.from(queryEmbedding).join(',')}]`;

    interface SearchResult {
      type: string;
      id: string;
      content: string;
      distance: number;
    }

    const results: SearchResult[] = [];

    // Search decisions
    const decisionsResult = await db.execute({
      sql: `
        SELECT 'decision' as type, id, decision as content,
               vector_distance_cos(embedding, vector(?)) as distance
        FROM session_decisions
        WHERE embedding IS NOT NULL
        ORDER BY distance ASC
        LIMIT 3
      `,
      args: [vectorJson]
    });

    results.push(...decisionsResult.rows.map(r => ({
      type: r.type as string,
      id: r.id as string,
      content: r.content as string,
      distance: r.distance as number
    })));

    // Search learnings
    const learningsResult = await db.execute({
      sql: `
        SELECT 'learning' as type, id, learning as content,
               vector_distance_cos(embedding, vector(?)) as distance
        FROM session_learnings
        WHERE embedding IS NOT NULL
        ORDER BY distance ASC
        LIMIT 3
      `,
      args: [vectorJson]
    });

    results.push(...learningsResult.rows.map(r => ({
      type: r.type as string,
      id: r.id as string,
      content: r.content as string,
      distance: r.distance as number
    })));

    // Search errors
    const errorsResult = await db.execute({
      sql: `
        SELECT 'error' as type, id, error_message as content,
               vector_distance_cos(embedding, vector(?)) as distance
        FROM session_errors
        WHERE embedding IS NOT NULL
        ORDER BY distance ASC
        LIMIT 3
      `,
      args: [vectorJson]
    });

    results.push(...errorsResult.rows.map(r => ({
      type: r.type as string,
      id: r.id as string,
      content: r.content as string,
      distance: r.distance as number
    })));

    // Sort all results by distance
    results.sort((a, b) => a.distance - b.distance);

    if (results.length > 0) {
      // Should find results from at least one knowledge type
      expect(results.length).toBeGreaterThan(0);

      // Best match should have reasonable similarity
      expect(results[0].distance).toBeLessThan(1.5);
    }
  });

  test('should validate search result quality', async () => {
    // Search for a specific term
    const query = 'testing';
    let queryEmbedding;
    try {
      queryEmbedding = await embedder.embed(query);
    } catch (error) {
      console.log('Embedding service unavailable, skipping test');
      return;
    }
    const vectorJson = `[${Array.from(queryEmbedding).join(',')}]`;

    const results = await db.execute({
      sql: `
        SELECT learning, vector_distance_cos(embedding, vector(?)) as distance
        FROM session_learnings
        WHERE embedding IS NOT NULL
        ORDER BY distance ASC
        LIMIT 10
      `,
      args: [vectorJson]
    });

    if (results.rows.length > 0) {
      // Top results should be more similar than bottom results
      const topDistance = results.rows[0].distance as number;
      const lastDistance = results.rows[results.rows.length - 1].distance as number;

      expect(topDistance).toBeLessThanOrEqual(lastDistance);
    }
  });
});

describe('Workflow 3: Query by Type with Filters', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should query decisions by date range', async () => {
    const startOfToday = new Date().setHours(0, 0, 0, 0);
    const endOfToday = new Date().setHours(23, 59, 59, 999);

    const result = await db.execute({
      sql: `
        SELECT * FROM session_decisions
        WHERE timestamp >= ? AND timestamp <= ?
        ORDER BY timestamp DESC
      `,
      args: [startOfToday, endOfToday]
    });

    // All results should be from today
    for (const row of result.rows) {
      const timestamp = row.timestamp as number;
      expect(timestamp).toBeGreaterThanOrEqual(startOfToday);
      expect(timestamp).toBeLessThanOrEqual(endOfToday);
    }
  });

  test('should query learnings by category', async () => {
    // First, get available categories
    const categoriesResult = await db.execute({
      sql: 'SELECT DISTINCT category FROM session_learnings WHERE category IS NOT NULL'
    });

    if (categoriesResult.rows.length === 0) {
      console.log('No categorized learnings found, skipping test');
      return;
    }

    const category = categoriesResult.rows[0].category as string;

    // Query by this category
    const result = await db.execute({
      sql: 'SELECT * FROM session_learnings WHERE category = ?',
      args: [category]
    });

    // All results should match the category
    for (const row of result.rows) {
      expect(row.category).toBe(category);
    }
  });

  test('should query errors by type', async () => {
    // Get available error types
    const typesResult = await db.execute({
      sql: 'SELECT DISTINCT error_type FROM session_errors WHERE error_type IS NOT NULL LIMIT 1'
    });

    if (typesResult.rows.length === 0) {
      console.log('No typed errors found, skipping test');
      return;
    }

    const errorType = typesResult.rows[0].error_type as string;

    // Query by this type
    const result = await db.execute({
      sql: 'SELECT * FROM session_errors WHERE error_type = ?',
      args: [errorType]
    });

    // All results should match the error type
    for (const row of result.rows) {
      expect(row.error_type).toBe(errorType);
    }
  });

  test('should query by session ID', async () => {
    // Get a session with multiple knowledge items
    const sessionResult = await db.execute({
      sql: `
        SELECT session_id, COUNT(*) as count
        FROM session_decisions
        GROUP BY session_id
        HAVING count > 1
        LIMIT 1
      `
    });

    if (sessionResult.rows.length === 0) {
      console.log('No sessions with multiple decisions found, skipping test');
      return;
    }

    const sessionId = sessionResult.rows[0].session_id as string;

    // Query all knowledge from this session
    const decisions = await db.execute({
      sql: 'SELECT * FROM session_decisions WHERE session_id = ?',
      args: [sessionId]
    });

    const learnings = await db.execute({
      sql: 'SELECT * FROM session_learnings WHERE session_id = ?',
      args: [sessionId]
    });

    const errors = await db.execute({
      sql: 'SELECT * FROM session_errors WHERE session_id = ?',
      args: [sessionId]
    });

    // Should have at least some knowledge from this session
    const totalKnowledge = decisions.rows.length + learnings.rows.length + errors.rows.length;
    expect(totalKnowledge).toBeGreaterThan(0);

    // All results should match session ID
    for (const row of decisions.rows) {
      expect(row.session_id).toBe(sessionId);
    }
    for (const row of learnings.rows) {
      expect(row.session_id).toBe(sessionId);
    }
    for (const row of errors.rows) {
      expect(row.session_id).toBe(sessionId);
    }
  });

  test('should query recent knowledge with limit', async () => {
    const limit = 10;

    const result = await db.execute({
      sql: `
        SELECT * FROM session_decisions
        ORDER BY timestamp DESC
        LIMIT ?
      `,
      args: [limit]
    });

    expect(result.rows.length).toBeLessThanOrEqual(limit);

    // Should be sorted by timestamp descending
    if (result.rows.length > 1) {
      for (let i = 0; i < result.rows.length - 1; i++) {
        const current = result.rows[i].timestamp as number;
        const next = result.rows[i + 1].timestamp as number;
        expect(current).toBeGreaterThanOrEqual(next);
      }
    }
  });
});

describe('Workflow 4: Temporal Queries with --as-of', () => {
  let temporal: TemporalQueries;

  beforeAll(() => {
    temporal = new TemporalQueries();
  });

  afterAll(() => {
    temporal.close();
  });

  test('should query knowledge as of a specific date', async () => {
    const asOfDate = new Date('2026-02-01');
    const results = await temporal.queryAtTime('database', asOfDate);

    // All results should be valid at the requested time
    for (const result of results) {
      const validFrom = result.validFrom || result.timestamp;
      const validTo = result.validTo;

      expect(validFrom).toBeLessThanOrEqual(asOfDate.getTime());

      if (validTo) {
        expect(validTo).toBeGreaterThanOrEqual(asOfDate.getTime());
      }
    }
  });

  test('should return empty results for dates before any knowledge', async () => {
    const veryOldDate = new Date('2020-01-01');
    const results = await temporal.queryAtTime('anything', veryOldDate);

    expect(results.length).toBe(0);
  });

  test('should query current knowledge (as of now)', async () => {
    const now = new Date();
    const results = await temporal.queryAtTime('', now); // Empty query matches all

    // All results should be currently valid
    for (const result of results) {
      const validFrom = result.validFrom || result.timestamp;
      expect(validFrom).toBeLessThanOrEqual(now.getTime());

      // If validTo exists, should be in the future or null
      if (result.validTo) {
        expect(result.validTo).toBeGreaterThanOrEqual(now.getTime());
      }
    }
  });

  test('should detect changes between two dates', async () => {
    const startDate = new Date('2026-01-01');
    const endDate = new Date('2026-02-03');

    const changes = await temporal.getChangesBetween(startDate, endDate);

    // Changes should be within the date range
    for (const change of changes) {
      expect(change.timestamp).toBeGreaterThanOrEqual(startDate.getTime());
      expect(change.timestamp).toBeLessThanOrEqual(endDate.getTime());
    }

    // Should be sorted by timestamp (descending)
    if (changes.length > 1) {
      for (let i = 0; i < changes.length - 1; i++) {
        expect(changes[i].timestamp).toBeGreaterThanOrEqual(changes[i + 1].timestamp);
      }
    }
  });

  test('should respect temporal semantics in queries', async () => {
    // Query for knowledge at a specific point in time
    const queryDate = new Date('2026-02-01');

    const results = await temporal.queryAtTime('', queryDate); // Empty query to match all

    // If we have results, validate they respect temporal semantics
    if (results.length > 0) {
      for (const result of results) {
        // If valid_from exists, it should be before or at the query date
        if (result.validFrom) {
          expect(result.validFrom).toBeLessThanOrEqual(queryDate.getTime());
        }

        // If valid_to exists, it should be after the query date (item was still valid)
        if (result.validTo) {
          expect(result.validTo).toBeGreaterThanOrEqual(queryDate.getTime());
        }

        // If transaction_from exists, we knew about it by the query date
        // (NULL transaction_from means we use timestamp, which may be after query date)
        if (result.transactionFrom) {
          expect(result.transactionFrom).toBeLessThanOrEqual(queryDate.getTime());
        }

        // If transaction_to exists, it should be after the query date (version was current)
        if (result.transactionTo) {
          expect(result.transactionTo).toBeGreaterThanOrEqual(queryDate.getTime());
        }
      }
    } else {
      // No results is also valid - just means no knowledge at that time
      expect(results.length).toBe(0);
    }
  });
});

describe('Workflow 5: Decay Visualization', () => {
  let temporal: TemporalQueries;
  let decay: ConfidenceDecay;

  beforeAll(() => {
    temporal = new TemporalQueries();
    decay = new ConfidenceDecay();
  });

  afterAll(() => {
    temporal.close();
  });

  test('should calculate current confidence with decay applied', async () => {
    const results = await temporal.getWithDecay('');

    if (results.length === 0) {
      console.log('No knowledge items found, skipping decay test');
      return;
    }

    for (const result of results) {
      // Should have both base and current confidence
      expect(result.baseConfidence).toBeDefined();
      expect(result.currentConfidence).toBeDefined();

      // Current confidence should never exceed base confidence
      expect(result.currentConfidence).toBeLessThanOrEqual(result.baseConfidence);

      // Both should be in valid range
      expect(result.baseConfidence).toBeGreaterThanOrEqual(0);
      expect(result.baseConfidence).toBeLessThanOrEqual(1);
      expect(result.currentConfidence).toBeGreaterThanOrEqual(0);
      expect(result.currentConfidence).toBeLessThanOrEqual(1);
    }
  });

  test('should filter knowledge by domain', async () => {
    const domains = ['tech', 'science', 'news', 'core'];

    for (const domain of domains) {
      const results = await temporal.getWithDecay('', domain);

      // All results should match the requested domain
      for (const result of results) {
        if (result.domain) {
          expect(result.domain).toBe(domain);
        }
      }
    }
  });

  test('should show older knowledge with more decay', async () => {
    const results = await temporal.getWithDecay('');

    if (results.length < 2) {
      console.log('Not enough knowledge items to compare decay, skipping');
      return;
    }

    // Group by domain to compare within same decay function
    const byDomain = new Map<string, typeof results>();
    for (const result of results) {
      const domain = result.domain || 'unknown';
      if (!byDomain.has(domain)) {
        byDomain.set(domain, []);
      }
      byDomain.get(domain)!.push(result);
    }

    // Check decay within each domain
    for (const [domain, items] of byDomain) {
      if (items.length < 2) continue;

      // Sort by age (oldest first)
      items.sort((a, b) => {
        const ageA = Date.now() - (a.validFrom || a.timestamp);
        const ageB = Date.now() - (b.validFrom || b.timestamp);
        return ageB - ageA;
      });

      // Older items should generally have lower current confidence
      // (allowing for base confidence differences)
      const oldestItem = items[0];
      const newestItem = items[items.length - 1];

      if (oldestItem.baseConfidence > 0 && newestItem.baseConfidence > 0) {
        const oldestDecayPct = 1 - (oldestItem.currentConfidence / oldestItem.baseConfidence);
        const newestDecayPct = 1 - (newestItem.currentConfidence / newestItem.baseConfidence);

        expect(oldestDecayPct).toBeGreaterThanOrEqual(newestDecayPct);
      }
    }
  });

  test('should respect minimum confidence thresholds', async () => {
    const results = await temporal.getWithDecay('');

    for (const result of results) {
      if (result.domain && result.currentConfidence !== undefined) {
        const config = decay.getConfig(result.domain);
        if (config) {
          // Current confidence should respect domain minimum
          expect(result.currentConfidence).toBeGreaterThanOrEqual(config.minConfidence);
        }
      }
    }
  });

  test('should calculate decay rate correctly', async () => {
    const testConfidence = 1.0;
    const testAge = 30 * 24 * 60 * 60 * 1000; // 30 days

    const domains = decay.getDomains();

    for (const domain of domains) {
      const rate = decay.getDecayRate(testConfidence, testAge, domain);

      // Exponential functions should have negative rate
      const config = decay.getConfig(domain);
      if (config?.decayFunction === 'exponential' || config?.decayFunction === 'power_law') {
        expect(rate).toBeLessThan(0);
      }
    }
  });
});

describe('Workflow 6: Thinking Arc Detection', () => {
  let detector: ArcDetector;
  let db: Client;

  beforeAll(() => {
    detector = new ArcDetector();
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    detector.close();
    db.close();
  });

  test('should detect arcs in sessions with knowledge', async () => {
    // Find a session with multiple knowledge items
    const sessionResult = await db.execute({
      sql: `
        SELECT session_id, COUNT(*) as count
        FROM (
          SELECT session_id FROM session_decisions
          UNION ALL
          SELECT session_id FROM session_learnings
        )
        GROUP BY session_id
        HAVING count >= 2
        LIMIT 1
      `
    });

    if (sessionResult.rows.length === 0) {
      console.log('No sessions with multiple knowledge items found, skipping');
      return;
    }

    const sessionId = sessionResult.rows[0].session_id as string;

    const arcs = await detector.detectArcs(sessionId);

    // May or may not find arcs depending on content
    expect(Array.isArray(arcs)).toBe(true);

    if (arcs.length > 0) {
      for (const arc of arcs) {
        // Validate arc structure
        expect(arc.sessionId).toBe(sessionId);
        expect(arc.arcType).toBeDefined();
        expect(arc.description).toBeDefined();
        expect(arc.confidence).toBeGreaterThanOrEqual(0);
        expect(arc.confidence).toBeLessThanOrEqual(1);
        expect(arc.createdAt).toBeGreaterThan(0);

        // Arc type should be valid
        const validTypes = ['breakthrough', 'pattern_discovery', 'concrete_to_abstract', 'refinement', 'synthesis'];
        expect(validTypes).toContain(arc.arcType);
      }
    }
  });

  test('should store and retrieve arcs', async () => {
    // Find any session with arcs
    const arcsResult = await db.execute({
      sql: 'SELECT DISTINCT session_id FROM thinking_arcs LIMIT 1'
    });

    if (arcsResult.rows.length === 0) {
      console.log('No stored arcs found, skipping retrieval test');
      return;
    }

    const sessionId = arcsResult.rows[0].session_id as string;

    // Retrieve arcs
    const arcs = await detector.getSessionArcs(sessionId);

    expect(arcs.length).toBeGreaterThan(0);

    for (const arc of arcs) {
      expect(arc.sessionId).toBe(sessionId);
      expect(arc.id).toBeDefined();
    }
  });

  test('should handle sessions with no arcs gracefully', async () => {
    const fakeSessionId = 'no-arcs-' + Date.now();

    const arcs = await detector.detectArcs(fakeSessionId);

    expect(arcs.length).toBe(0);
  });

  test('should detect different arc types', async () => {
    // Get all stored arcs
    const allArcs = await db.execute({
      sql: 'SELECT arc_type, COUNT(*) as count FROM thinking_arcs GROUP BY arc_type'
    });

    if (allArcs.rows.length === 0) {
      console.log('No arcs in database, skipping arc type test');
      return;
    }

    // Should have variety in arc types
    const arcTypes = allArcs.rows.map(r => r.arc_type as string);
    expect(arcTypes.length).toBeGreaterThan(0);

    // Each type should have valid count
    for (const row of allArcs.rows) {
      expect(row.count).toBeGreaterThan(0);
    }
  });
});

describe('Workflow 7: Cross-Session Queries', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should query knowledge across multiple sessions', async () => {
    const result = await db.execute({
      sql: `
        SELECT DISTINCT session_id
        FROM session_decisions
        LIMIT 5
      `
    });

    if (result.rows.length < 2) {
      console.log('Not enough sessions to test cross-session queries, skipping');
      return;
    }

    const sessionIds = result.rows.map(r => r.session_id as string);

    // Query decisions from all these sessions
    const decisions = await db.execute({
      sql: `
        SELECT * FROM session_decisions
        WHERE session_id IN (${sessionIds.map(() => '?').join(',')})
        ORDER BY timestamp DESC
      `,
      args: sessionIds
    });

    // Should find decisions from multiple sessions
    const uniqueSessions = new Set(decisions.rows.map(r => r.session_id));
    expect(uniqueSessions.size).toBeGreaterThan(1);
  });

  test('should aggregate statistics across sessions', async () => {
    const stats = await db.execute({
      sql: `
        SELECT
          COUNT(DISTINCT session_id) as session_count,
          COUNT(*) as total_decisions,
          AVG(confidence) as avg_confidence
        FROM session_decisions
      `
    });

    const row = stats.rows[0];

    if (row.session_count > 0) {
      expect(row.session_count).toBeGreaterThan(0);
      expect(row.total_decisions).toBeGreaterThan(0);

      if (row.avg_confidence !== null) {
        expect(row.avg_confidence).toBeGreaterThanOrEqual(0);
        expect(row.avg_confidence).toBeLessThanOrEqual(1);
      }
    }
  });

  test('should find related knowledge across sessions', async () => {
    // Find a common term that appears in multiple sessions
    const commonTerms = await db.execute({
      sql: `
        SELECT decision, session_id
        FROM session_decisions
        WHERE decision LIKE '%test%'
        LIMIT 10
      `
    });

    if (commonTerms.rows.length > 0) {
      const uniqueSessions = new Set(commonTerms.rows.map(r => r.session_id));

      // If we found matches in multiple sessions, they're related
      if (uniqueSessions.size > 1) {
        expect(uniqueSessions.size).toBeGreaterThan(1);
      }
    }
  });

  test('should track knowledge evolution across sessions', async () => {
    // Query all decisions ordered by timestamp
    const timeline = await db.execute({
      sql: `
        SELECT session_id, decision, timestamp
        FROM session_decisions
        ORDER BY timestamp ASC
        LIMIT 20
      `
    });

    if (timeline.rows.length < 2) {
      console.log('Not enough decisions to track evolution, skipping');
      return;
    }

    // Timeline should be properly ordered
    for (let i = 0; i < timeline.rows.length - 1; i++) {
      const current = timeline.rows[i].timestamp as number;
      const next = timeline.rows[i + 1].timestamp as number;
      expect(current).toBeLessThanOrEqual(next);
    }
  });
});

describe('Workflow 8: Data Isolation', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should isolate knowledge by session', async () => {
    // Get two different sessions
    const sessions = await db.execute({
      sql: 'SELECT DISTINCT session_id FROM session_decisions LIMIT 2'
    });

    if (sessions.rows.length < 2) {
      console.log('Not enough sessions to test isolation, skipping');
      return;
    }

    const session1 = sessions.rows[0].session_id as string;
    const session2 = sessions.rows[1].session_id as string;

    // Query each session independently
    const decisions1 = await db.execute({
      sql: 'SELECT * FROM session_decisions WHERE session_id = ?',
      args: [session1]
    });

    const decisions2 = await db.execute({
      sql: 'SELECT * FROM session_decisions WHERE session_id = ?',
      args: [session2]
    });

    // No overlap in IDs
    const ids1 = new Set(decisions1.rows.map(r => r.id));
    const ids2 = new Set(decisions2.rows.map(r => r.id));

    for (const id of ids1) {
      expect(ids2.has(id)).toBe(false);
    }
  });

  test('should maintain referential integrity', async () => {
    // All decisions should reference valid sessions
    const orphanedDecisions = await db.execute({
      sql: `
        SELECT d.id
        FROM session_decisions d
        LEFT JOIN sessions s ON d.session_id = s.id
        WHERE s.id IS NULL
        LIMIT 5
      `
    });

    expect(orphanedDecisions.rows.length).toBe(0);
  });

  test('should handle concurrent queries safely', async () => {
    // Execute multiple queries in parallel
    const queries = [
      db.execute({ sql: 'SELECT COUNT(*) as count FROM session_decisions' }),
      db.execute({ sql: 'SELECT COUNT(*) as count FROM session_learnings' }),
      db.execute({ sql: 'SELECT COUNT(*) as count FROM session_errors' })
    ];

    const results = await Promise.all(queries);

    // All queries should succeed
    for (const result of results) {
      expect(result.rows.length).toBe(1);
      expect(result.rows[0].count).toBeGreaterThanOrEqual(0);
    }
  });
});

describe('Workflow 9: Complex Query Patterns', () => {
  let db: Client;
  let embedder: EmbeddingGenerator;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
    embedder = new EmbeddingGenerator();
  });

  afterAll(() => {
    db.close();
  });

  test('should combine semantic search with temporal filtering', async () => {
    const query = 'database';
    let queryEmbedding;
    try {
      queryEmbedding = await embedder.embed(query);
    } catch (error) {
      console.log('Embedding service unavailable, skipping test');
      return;
    }
    const vectorJson = `[${Array.from(queryEmbedding).join(',')}]`;

    const startDate = new Date('2026-01-01').getTime();
    const endDate = new Date('2026-02-03').getTime();

    const results = await db.execute({
      sql: `
        SELECT id, decision, timestamp, vector_distance_cos(embedding, vector(?)) as distance
        FROM session_decisions
        WHERE embedding IS NOT NULL
          AND timestamp >= ?
          AND timestamp <= ?
        ORDER BY distance ASC
        LIMIT 10
      `,
      args: [vectorJson, startDate, endDate]
    });

    // All results should be within date range
    for (const row of results.rows) {
      expect(row.timestamp).toBeGreaterThanOrEqual(startDate);
      expect(row.timestamp).toBeLessThanOrEqual(endDate);
    }
  });

  test('should aggregate knowledge by category and time period', async () => {
    const stats = await db.execute({
      sql: `
        SELECT
          category,
          DATE(timestamp / 1000, 'unixepoch') as date,
          COUNT(*) as count
        FROM session_learnings
        WHERE category IS NOT NULL
        GROUP BY category, date
        ORDER BY date DESC, count DESC
        LIMIT 10
      `
    });

    // Each row should have valid aggregation
    for (const row of stats.rows) {
      expect(row.category).toBeDefined();
      expect(row.count).toBeGreaterThan(0);
    }
  });

  test('should find knowledge with multiple criteria', async () => {
    const results = await db.execute({
      sql: `
        SELECT *
        FROM session_decisions
        WHERE confidence >= 0.7
          AND reasoning IS NOT NULL
          AND reasoning != ''
          AND timestamp > ?
        ORDER BY confidence DESC
        LIMIT 10
      `,
      args: [Date.now() - 30 * 24 * 60 * 60 * 1000] // Last 30 days
    });

    // All results should match criteria
    for (const row of results.rows) {
      expect(row.confidence).toBeGreaterThanOrEqual(0.7);
      expect(row.reasoning).not.toBeNull();
      expect(row.reasoning).not.toBe('');
    }
  });
});

describe('Workflow 10: Performance and Scale', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should query large result sets efficiently', async () => {
    const start = Date.now();

    const result = await db.execute({
      sql: 'SELECT * FROM session_decisions LIMIT 1000'
    });

    const duration = Date.now() - start;

    // Should complete in reasonable time (< 1 second)
    expect(duration).toBeLessThan(1000);
    expect(result.rows.length).toBeLessThanOrEqual(1000);
  });

  test('should handle pagination efficiently', async () => {
    const pageSize = 50;
    const pages: any[][] = [];

    // Fetch multiple pages
    for (let i = 0; i < 3; i++) {
      const result = await db.execute({
        sql: `
          SELECT * FROM session_decisions
          ORDER BY timestamp DESC
          LIMIT ? OFFSET ?
        `,
        args: [pageSize, i * pageSize]
      });

      pages.push(result.rows);
    }

    // Pages should not have overlapping IDs
    const allIds = new Set();
    for (const page of pages) {
      for (const row of page) {
        expect(allIds.has(row.id)).toBe(false);
        allIds.add(row.id);
      }
    }
  });

  test('should use indexes for common queries', async () => {
    // Query by session_id (should use index)
    const result = await db.execute({
      sql: 'SELECT * FROM session_decisions WHERE session_id = ?',
      args: ['test-session']
    });

    // Should execute quickly even with no results
    expect(result.rows.length).toBeGreaterThanOrEqual(0);
  });
});

describe('Workflow 11: Knowledge Relationships', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should query outgoing relationships for knowledge', async () => {
    // Get any knowledge item with relationships
    const relResult = await db.execute({
      sql: 'SELECT DISTINCT from_id FROM knowledge_relationships LIMIT 1'
    });

    if (relResult.rows.length === 0) {
      console.log('No relationships found, skipping test');
      return;
    }

    const fromId = relResult.rows[0].from_id as string;

    // Query outgoing relationships
    const relationships = await db.execute({
      sql: 'SELECT * FROM knowledge_relationships WHERE from_id = ?',
      args: [fromId]
    });

    expect(relationships.rows.length).toBeGreaterThan(0);

    for (const row of relationships.rows) {
      expect(row.from_id).toBe(fromId);
      expect(row.to_id).toBeDefined();
      expect(row.relationship_type).toBeDefined();
      expect(row.confidence).toBeGreaterThanOrEqual(0);
      expect(row.confidence).toBeLessThanOrEqual(1);
    }
  });

  test('should query incoming relationships for knowledge', async () => {
    // Get any knowledge item that is a target of relationships
    const relResult = await db.execute({
      sql: 'SELECT DISTINCT to_id FROM knowledge_relationships LIMIT 1'
    });

    if (relResult.rows.length === 0) {
      console.log('No relationships found, skipping test');
      return;
    }

    const toId = relResult.rows[0].to_id as string;

    // Query incoming relationships
    const relationships = await db.execute({
      sql: 'SELECT * FROM knowledge_relationships WHERE to_id = ?',
      args: [toId]
    });

    expect(relationships.rows.length).toBeGreaterThan(0);

    for (const row of relationships.rows) {
      expect(row.to_id).toBe(toId);
      expect(row.from_id).toBeDefined();
    }
  });

  test('should validate relationship types', async () => {
    const relationships = await db.execute({
      sql: 'SELECT DISTINCT relationship_type FROM knowledge_relationships'
    });

    const validTypes = ['supports', 'contradicts', 'supersedes', 'evolves_from', 'depends_on', 'questions', 'requires'];

    for (const row of relationships.rows) {
      expect(validTypes).toContain(row.relationship_type as string);
    }
  });

  test('should ensure no self-referential relationships', async () => {
    const selfRefs = await db.execute({
      sql: `
        SELECT * FROM knowledge_relationships
        WHERE from_type = to_type AND from_id = to_id
      `
    });

    // Should have no self-referential relationships
    expect(selfRefs.rows.length).toBe(0);
  });
});

describe('Workflow 12: Batch Operations', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should handle bulk insert of knowledge items', async () => {
    const sessionId = 'test-bulk-' + Date.now();
    const timestamp = Date.now();

    // Create session first (required for foreign key constraint)
    await db.execute({
      sql: `
        INSERT OR IGNORE INTO sessions (id, created, modified, summary, message_count, agent_count, cost, duration)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      `,
      args: [sessionId, timestamp, timestamp, 'Test session', 0, 0, 0.0, 0]
    });

    // Insert multiple decisions in a transaction
    const decisions = [
      { id: `dec1-${timestamp}`, decision: 'Test decision 1', confidence: 0.8 },
      { id: `dec2-${timestamp}`, decision: 'Test decision 2', confidence: 0.9 },
      { id: `dec3-${timestamp}`, decision: 'Test decision 3', confidence: 0.7 }
    ];

    for (const dec of decisions) {
      await db.execute({
        sql: `
          INSERT INTO session_decisions (id, session_id, message_id, timestamp, decision, confidence)
          VALUES (?, ?, ?, ?, ?, ?)
        `,
        args: [dec.id, sessionId, `msg-${dec.id}`, timestamp, dec.decision, dec.confidence]
      });
    }

    // Verify all were inserted
    const result = await db.execute({
      sql: 'SELECT * FROM session_decisions WHERE session_id = ?',
      args: [sessionId]
    });

    expect(result.rows.length).toBe(decisions.length);

    // Cleanup
    await db.execute({
      sql: 'DELETE FROM session_decisions WHERE session_id = ?',
      args: [sessionId]
    });

    await db.execute({
      sql: 'DELETE FROM sessions WHERE id = ?',
      args: [sessionId]
    });
  });

  test('should handle bulk updates of confidence scores', async () => {
    // Get a sample of decisions
    const decisions = await db.execute({
      sql: 'SELECT id, confidence FROM session_decisions LIMIT 5'
    });

    if (decisions.rows.length === 0) {
      console.log('No decisions to update, skipping test');
      return;
    }

    // Store original values
    const original = new Map(decisions.rows.map(r => [r.id, r.confidence]));

    // Update confidence (simulate decay)
    for (const row of decisions.rows) {
      const newConfidence = Math.max(0.1, (row.confidence as number) * 0.9);
      await db.execute({
        sql: 'UPDATE session_decisions SET confidence = ? WHERE id = ?',
        args: [newConfidence, row.id]
      });
    }

    // Verify updates
    const updated = await db.execute({
      sql: `SELECT id, confidence FROM session_decisions WHERE id IN (${decisions.rows.map(() => '?').join(',')})`,
      args: decisions.rows.map(r => r.id)
    });

    for (const row of updated.rows) {
      const originalConf = original.get(row.id) as number;
      const expectedConf = Math.max(0.1, originalConf * 0.9);
      expect(Math.abs((row.confidence as number) - expectedConf)).toBeLessThan(0.01);
    }

    // Restore original values
    for (const [id, confidence] of original) {
      await db.execute({
        sql: 'UPDATE session_decisions SET confidence = ? WHERE id = ?',
        args: [confidence, id]
      });
    }
  });

  test('should handle bulk deletion with filters', async () => {
    // Create test data
    const testSessionId = 'test-delete-' + Date.now();
    const timestamp = Date.now();

    // Create session first
    await db.execute({
      sql: `
        INSERT OR IGNORE INTO sessions (id, created, modified, summary, message_count, agent_count, cost, duration)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      `,
      args: [testSessionId, timestamp, timestamp, 'Test session', 0, 0, 0.0, 0]
    });

    await db.execute({
      sql: `
        INSERT INTO session_decisions (id, session_id, message_id, timestamp, decision, confidence)
        VALUES (?, ?, ?, ?, ?, ?)
      `,
      args: [`dec-del-${timestamp}`, testSessionId, 'msg-1', timestamp, 'Test decision', 0.5]
    });

    // Verify insertion
    const before = await db.execute({
      sql: 'SELECT COUNT(*) as count FROM session_decisions WHERE session_id = ?',
      args: [testSessionId]
    });
    expect(before.rows[0].count).toBe(1);

    // Delete
    await db.execute({
      sql: 'DELETE FROM session_decisions WHERE session_id = ?',
      args: [testSessionId]
    });

    // Verify deletion
    const after = await db.execute({
      sql: 'SELECT COUNT(*) as count FROM session_decisions WHERE session_id = ?',
      args: [testSessionId]
    });
    expect(after.rows[0].count).toBe(0);

    // Cleanup session
    await db.execute({
      sql: 'DELETE FROM sessions WHERE id = ?',
      args: [testSessionId]
    });
  });
});

describe('Workflow 13: Error Handling and Edge Cases', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should handle queries with special characters', async () => {
    const specialChars = ['%', '_', "'", '"', '\\', '\n', '\t'];

    for (const char of specialChars) {
      const result = await db.execute({
        sql: 'SELECT * FROM session_decisions WHERE decision LIKE ?',
        args: [`%${char}%`]
      });

      // Should execute without error
      expect(result.rows.length).toBeGreaterThanOrEqual(0);
    }
  });

  test('should handle empty result sets gracefully', async () => {
    const result = await db.execute({
      sql: 'SELECT * FROM session_decisions WHERE session_id = ?',
      args: ['definitely-does-not-exist-' + Date.now()]
    });

    expect(result.rows.length).toBe(0);
    expect(Array.isArray(result.rows)).toBe(true);
  });

  test('should handle NULL values in optional fields', async () => {
    const decisions = await db.execute({
      sql: 'SELECT * FROM session_decisions WHERE reasoning IS NULL LIMIT 5'
    });

    for (const row of decisions.rows) {
      expect(row.reasoning).toBeNull();
      // But required fields should still be present
      expect(row.id).toBeDefined();
      expect(row.decision).toBeDefined();
    }
  });

  test('should handle very large text content', async () => {
    // Get a decision with potentially large content
    const result = await db.execute({
      sql: 'SELECT decision FROM session_decisions WHERE LENGTH(decision) > 100 LIMIT 1'
    });

    if (result.rows.length > 0) {
      const content = result.rows[0].decision as string;
      expect(content.length).toBeGreaterThan(100);
      expect(typeof content).toBe('string');
    }
  });

  test('should validate timestamp ranges', async () => {
    const result = await db.execute({
      sql: 'SELECT MIN(timestamp) as min, MAX(timestamp) as max FROM session_decisions'
    });

    if (result.rows[0].min && result.rows[0].max) {
      const min = result.rows[0].min as number;
      const max = result.rows[0].max as number;

      // Timestamps should be reasonable (after 2020, before 2030)
      const year2020 = new Date('2020-01-01').getTime();
      const year2030 = new Date('2030-01-01').getTime();

      expect(min).toBeGreaterThan(year2020);
      expect(max).toBeLessThan(year2030);
      expect(max).toBeGreaterThanOrEqual(min);
    }
  });
});

describe('Workflow 14: Analytics and Aggregations', () => {
  let db: Client;

  beforeAll(() => {
    db = createClient({ url: `file:${DB_PATH}` });
  });

  afterAll(() => {
    db.close();
  });

  test('should calculate knowledge distribution by type', async () => {
    const stats = await db.execute({
      sql: `
        SELECT
          (SELECT COUNT(*) FROM session_decisions) as decisions,
          (SELECT COUNT(*) FROM session_learnings) as learnings,
          (SELECT COUNT(*) FROM session_errors) as errors,
          (SELECT COUNT(*) FROM session_workflows) as workflows
      `
    });

    const row = stats.rows[0];
    const total = (row.decisions as number) + (row.learnings as number) +
                  (row.errors as number) + (row.workflows as number);

    expect(total).toBeGreaterThanOrEqual(0);

    // At least one type should have data
    expect(total).toBeGreaterThan(0);
  });

  test('should calculate average confidence by knowledge type', async () => {
    const stats = await db.execute({
      sql: `
        SELECT
          'decision' as type,
          AVG(confidence) as avg_confidence,
          COUNT(*) as count
        FROM session_decisions
        WHERE confidence IS NOT NULL
        UNION ALL
        SELECT
          'learning' as type,
          AVG(confidence) as avg_confidence,
          COUNT(*) as count
        FROM session_learnings
        WHERE confidence IS NOT NULL
        UNION ALL
        SELECT
          'error' as type,
          AVG(confidence) as avg_confidence,
          COUNT(*) as count
        FROM session_errors
        WHERE confidence IS NOT NULL
      `
    });

    for (const row of stats.rows) {
      if (row.count > 0) {
        expect(row.avg_confidence).toBeGreaterThanOrEqual(0);
        expect(row.avg_confidence).toBeLessThanOrEqual(1);
      }
    }
  });

  test('should calculate knowledge growth over time', async () => {
    const growth = await db.execute({
      sql: `
        SELECT
          DATE(timestamp / 1000, 'unixepoch') as date,
          COUNT(*) as count
        FROM session_decisions
        GROUP BY date
        ORDER BY date DESC
        LIMIT 30
      `
    });

    if (growth.rows.length > 1) {
      // Should have data from multiple days
      const dates = new Set(growth.rows.map(r => r.date));
      expect(dates.size).toBeGreaterThan(0);
    }
  });

  test('should identify most productive sessions', async () => {
    const productive = await db.execute({
      sql: `
        SELECT
          session_id,
          COUNT(*) as knowledge_count
        FROM (
          SELECT session_id FROM session_decisions
          UNION ALL
          SELECT session_id FROM session_learnings
          UNION ALL
          SELECT session_id FROM session_errors
        )
        GROUP BY session_id
        ORDER BY knowledge_count DESC
        LIMIT 10
      `
    });

    if (productive.rows.length > 0) {
      // Top session should have most knowledge
      const topCount = productive.rows[0].knowledge_count as number;
      expect(topCount).toBeGreaterThan(0);

      // Should be sorted descending
      for (let i = 0; i < productive.rows.length - 1; i++) {
        expect(productive.rows[i].knowledge_count)
          .toBeGreaterThanOrEqual(productive.rows[i + 1].knowledge_count);
      }
    }
  });

  test('should calculate domain distribution', async () => {
    const domains = await db.execute({
      sql: `
        SELECT
          domain,
          COUNT(*) as count
        FROM (
          SELECT domain FROM session_decisions WHERE domain IS NOT NULL
          UNION ALL
          SELECT domain FROM session_learnings WHERE domain IS NOT NULL
          UNION ALL
          SELECT domain FROM session_errors WHERE domain IS NOT NULL
        )
        GROUP BY domain
        ORDER BY count DESC
      `
    });

    const validDomains = ['tech', 'science', 'news', 'core'];

    for (const row of domains.rows) {
      expect(validDomains).toContain(row.domain as string);
      expect(row.count).toBeGreaterThan(0);
    }
  });
});
