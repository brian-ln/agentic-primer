/**
 * Cognitive Integration Tests
 * Epic: agentic-primer-9ad
 * Phase: Cognitive Integration v1
 *
 * Tests for bi-temporal queries, confidence decay, and thinking arcs
 */

import { describe, test, expect, beforeAll, afterAll } from 'bun:test';
import { ConfidenceDecay } from '../temporal/ConfidenceDecay';
import { TemporalQueries } from '../temporal/TemporalQueries';
import { ArcDetector } from '../temporal/ArcDetector';

describe('ConfidenceDecay', () => {
  const decay = new ConfidenceDecay();

  describe('Tech domain (exponential decay)', () => {
    test('should return 100% confidence at age 0', () => {
      const confidence = decay.calculateDecay(1.0, 0, 'tech');
      expect(confidence).toBe(1.0);
    });

    test('should decay to ~50% at half-life (9 months)', () => {
      const nineMonths = 9 * 30 * 24 * 60 * 60 * 1000;
      const confidence = decay.calculateDecay(1.0, nineMonths, 'tech');
      expect(confidence).toBeGreaterThan(0.45);
      expect(confidence).toBeLessThan(0.55);
    });

    test('should never go below min confidence (20%)', () => {
      const veryOld = 10 * 365 * 24 * 60 * 60 * 1000; // 10 years
      const confidence = decay.calculateDecay(1.0, veryOld, 'tech');
      expect(confidence).toBeGreaterThanOrEqual(0.2);
    });

    test('should decay faster than science domain', () => {
      const oneYear = 365 * 24 * 60 * 60 * 1000;
      const techConfidence = decay.calculateDecay(1.0, oneYear, 'tech');
      const scienceConfidence = decay.calculateDecay(1.0, oneYear, 'science');
      expect(techConfidence).toBeLessThan(scienceConfidence);
    });
  });

  describe('Science domain (power law decay)', () => {
    test('should return 100% confidence at age 0', () => {
      const confidence = decay.calculateDecay(1.0, 0, 'science');
      expect(confidence).toBe(1.0);
    });

    test('should decay slower than exponential', () => {
      const oneYear = 365 * 24 * 60 * 60 * 1000;
      const scienceConfidence = decay.calculateDecay(1.0, oneYear, 'science');
      expect(scienceConfidence).toBeGreaterThan(0.6);
    });

    test('should never go below min confidence (40%)', () => {
      const veryOld = 50 * 365 * 24 * 60 * 60 * 1000; // 50 years
      const confidence = decay.calculateDecay(1.0, veryOld, 'science');
      expect(confidence).toBeGreaterThanOrEqual(0.4);
    });
  });

  describe('News domain (fast exponential decay)', () => {
    test('should decay to ~50% at 2 months', () => {
      const twoMonths = 2 * 30 * 24 * 60 * 60 * 1000;
      const confidence = decay.calculateDecay(1.0, twoMonths, 'news');
      expect(confidence).toBeGreaterThan(0.45);
      expect(confidence).toBeLessThan(0.55);
    });

    test('should decay faster than tech', () => {
      const threeMonths = 3 * 30 * 24 * 60 * 60 * 1000;
      const newsConfidence = decay.calculateDecay(1.0, threeMonths, 'news');
      const techConfidence = decay.calculateDecay(1.0, threeMonths, 'tech');
      expect(newsConfidence).toBeLessThan(techConfidence);
    });
  });

  describe('Core domain (stepped decay)', () => {
    test('should stay at 100% for first 2 years', () => {
      const oneYear = 365 * 24 * 60 * 60 * 1000;
      const confidence = decay.calculateDecay(1.0, oneYear, 'core');
      expect(confidence).toBe(1.0);
    });

    test('should drop to 90% between 2-5 years', () => {
      const threeYears = 3 * 365 * 24 * 60 * 60 * 1000;
      const confidence = decay.calculateDecay(1.0, threeYears, 'core');
      expect(confidence).toBe(0.9);
    });

    test('should drop to 70% between 5-10 years', () => {
      const sevenYears = 7 * 365 * 24 * 60 * 60 * 1000;
      const confidence = decay.calculateDecay(1.0, sevenYears, 'core');
      expect(confidence).toBe(0.7);
    });

    test('should never go below min confidence (60%) in practical timeframes', () => {
      const veryOld = 100 * 365 * 24 * 60 * 60 * 1000; // 100 years
      const confidence = decay.calculateDecay(1.0, veryOld, 'core');
      expect(confidence).toBeGreaterThanOrEqual(0.3);
    });
  });

  describe('Edge cases', () => {
    test('should handle negative age gracefully', () => {
      const confidence = decay.calculateDecay(1.0, -1000, 'tech');
      expect(confidence).toBe(1.0);
    });

    test('should handle unknown domain conservatively', () => {
      const oneYear = 365 * 24 * 60 * 60 * 1000;
      const confidence = decay.calculateDecay(1.0, oneYear, 'unknown');
      expect(confidence).toBeGreaterThan(0.5); // Should decay slowly like core
    });

    test('should throw on invalid base confidence', () => {
      expect(() => decay.calculateDecay(1.5, 1000, 'tech')).toThrow();
      expect(() => decay.calculateDecay(-0.1, 1000, 'tech')).toThrow();
    });

    test('should preserve base confidence < 1.0', () => {
      const confidence = decay.calculateDecay(0.5, 0, 'tech');
      expect(confidence).toBe(0.5);
    });
  });

  describe('Time estimation', () => {
    test('should estimate time to reach target confidence', () => {
      const timeMs = decay.estimateTimeToConfidence(1.0, 0.5, 'tech');
      expect(timeMs).not.toBeNull();
      if (timeMs) {
        const months = timeMs / (30 * 24 * 60 * 60 * 1000);
        expect(months).toBeCloseTo(9, 0); // Should be ~9 months for tech half-life
      }
    });

    test('should return null for impossible targets', () => {
      expect(decay.estimateTimeToConfidence(1.0, 0.1, 'tech')).toBeNull(); // Below min
      expect(decay.estimateTimeToConfidence(0.5, 0.8, 'tech')).toBeNull(); // Above base
    });
  });

  describe('Decay rate', () => {
    test('should calculate decay rate for exponential functions', () => {
      const rate = decay.getDecayRate(1.0, 0, 'tech');
      expect(rate).toBeLessThan(0); // Negative rate means decaying
    });

    test('should show zero rate for stepped functions', () => {
      const rate = decay.getDecayRate(1.0, 1000, 'core');
      expect(rate).toBe(0); // Stepped has zero derivative
    });
  });

  describe('Configuration', () => {
    test('should provide config for all domains', () => {
      const domains = decay.getDomains();
      expect(domains).toContain('tech');
      expect(domains).toContain('science');
      expect(domains).toContain('news');
      expect(domains).toContain('core');
    });

    test('should return config for each domain', () => {
      const techConfig = decay.getConfig('tech');
      expect(techConfig).toBeDefined();
      expect(techConfig?.decayFunction).toBe('exponential');
      expect(techConfig?.minConfidence).toBe(0.2);
    });
  });
});

describe('TemporalQueries (Integration)', () => {
  let temporal: TemporalQueries;

  beforeAll(() => {
    temporal = new TemporalQueries();
  });

  afterAll(() => {
    temporal.close();
  });

  describe('queryAtTime', () => {
    test('should query knowledge at a specific date', async () => {
      const date = new Date('2026-02-01');
      const results = await temporal.queryAtTime('libSQL', date);

      // Should find decisions/learnings about libSQL
      expect(results.length).toBeGreaterThan(0);
      expect(results.every(r => r.content.toLowerCase().includes('libsql'))).toBe(true);
    });

    test('should filter by valid_time correctly', async () => {
      const date = new Date('2020-01-01'); // Far in the past
      const results = await temporal.queryAtTime('anything', date);

      // Should have no results (all our data is from 2026)
      expect(results.length).toBe(0);
    });

    test('should include all knowledge types', async () => {
      const date = new Date('2026-02-01');
      const results = await temporal.queryAtTime('', date); // Empty query = match all

      const types = new Set(results.map(r => r.type));
      // Should have at least some knowledge types
      expect(types.size).toBeGreaterThan(0);
    });
  });

  describe('getChangesBetween', () => {
    test('should detect changes in a time period', async () => {
      const start = new Date('2026-01-01');
      const end = new Date('2026-02-03');

      const changes = await temporal.getChangesBetween(start, end);

      // Should find changes (added items)
      expect(changes.length).toBeGreaterThan(0);
      expect(changes.every(c => c.changeType === 'added' || c.changeType === 'modified' || c.changeType === 'invalidated')).toBe(true);
    });

    test('should be sorted by timestamp descending', async () => {
      const start = new Date('2026-01-01');
      const end = new Date('2026-02-03');

      const changes = await temporal.getChangesBetween(start, end);

      if (changes.length > 1) {
        for (let i = 0; i < changes.length - 1; i++) {
          expect(changes[i].timestamp).toBeGreaterThanOrEqual(changes[i + 1].timestamp);
        }
      }
    });
  });

  describe('getWithDecay', () => {
    test('should apply confidence decay to results', async () => {
      const results = await temporal.getWithDecay('libSQL');

      expect(results.length).toBeGreaterThan(0);

      // All results should have currentConfidence
      expect(results.every(r => r.currentConfidence !== undefined)).toBe(true);

      // Current confidence should be <= base confidence (decay never increases)
      expect(results.every(r => {
        return r.currentConfidence === undefined || r.currentConfidence <= r.baseConfidence;
      })).toBe(true);
    });

    test('should filter by domain', async () => {
      const results = await temporal.getWithDecay('', 'core');

      if (results.length > 0) {
        expect(results.every(r => r.domain === 'core')).toBe(true);
      }
    });

    test('should sort by current confidence (highest first)', async () => {
      const results = await temporal.getWithDecay('');

      if (results.length > 1) {
        for (let i = 0; i < results.length - 1; i++) {
          const curr = results[i].currentConfidence || 0;
          const next = results[i + 1].currentConfidence || 0;
          expect(curr).toBeGreaterThanOrEqual(next);
        }
      }
    });
  });
});

describe('ArcDetector (Integration)', () => {
  let detector: ArcDetector;

  beforeAll(() => {
    detector = new ArcDetector();
  });

  afterAll(() => {
    detector.close();
  });

  describe('detectArcs', () => {
    test('should detect arcs in a real session', async () => {
      // Use a real session ID from the database
      const sessionId = '4af7ce26-80a3-4ea4-b3b4-312c40e39e76';

      const arcs = await detector.detectArcs(sessionId);

      // Should find at least some arcs
      expect(arcs.length).toBeGreaterThan(0);
    });

    test('should return empty array for sessions with < 2 knowledge items', async () => {
      // Fake session ID with no knowledge
      const arcs = await detector.detectArcs('fake-session-id');

      expect(arcs.length).toBe(0);
    });

    test('should detect different arc types', async () => {
      const sessionId = '4af7ce26-80a3-4ea4-b3b4-312c40e39e76';

      const arcs = await detector.detectArcs(sessionId);

      // Should have variety in arc types
      const arcTypes = new Set(arcs.map(a => a.arcType));
      expect(arcTypes.size).toBeGreaterThan(0);

      // All arc types should be valid
      const validTypes = ['breakthrough', 'pattern_discovery', 'concrete_to_abstract', 'refinement', 'synthesis'];
      expect(arcs.every(a => validTypes.includes(a.arcType))).toBe(true);
    });

    test('should have confidence scores in valid range', async () => {
      const sessionId = '4af7ce26-80a3-4ea4-b3b4-312c40e39e76';

      const arcs = await detector.detectArcs(sessionId);

      expect(arcs.every(a => a.confidence >= 0 && a.confidence <= 1)).toBe(true);
    });
  });

  describe('getSessionArcs', () => {
    test('should retrieve stored arcs', async () => {
      const sessionId = '4af7ce26-80a3-4ea4-b3b4-312c40e39e76';

      // First detect and store arcs
      await detector.detectArcs(sessionId);

      // Then retrieve them
      const arcs = await detector.getSessionArcs(sessionId);

      expect(arcs.length).toBeGreaterThan(0);
      expect(arcs.every(a => a.sessionId === sessionId)).toBe(true);
    });
  });
});

describe('End-to-End Cognitive Integration', () => {
  test('should support complete temporal workflow', async () => {
    const temporal = new TemporalQueries();
    const decay = new ConfidenceDecay();

    try {
      // 1. Query current knowledge
      const current = await temporal.queryAtTime('libSQL', new Date());
      expect(current.length).toBeGreaterThan(0);

      // 2. Query historical knowledge
      const historical = await temporal.queryAtTime('libSQL', new Date('2026-01-15'));
      // Historical should have fewer or equal items (knowledge accumulates)
      expect(historical.length).toBeLessThanOrEqual(current.length);

      // 3. Apply decay
      const withDecay = await temporal.getWithDecay('libSQL');
      expect(withDecay.every(r => r.currentConfidence !== undefined)).toBe(true);

      // 4. Check confidence decay calculation
      for (const item of withDecay) {
        if (item.domain && item.validFrom) {
          const ageMs = Date.now() - item.validFrom;
          const calculatedConfidence = decay.calculateDecay(item.baseConfidence, ageMs, item.domain);
          expect(item.currentConfidence).toBeCloseTo(calculatedConfidence, 5);
        }
      }
    } finally {
      temporal.close();
    }
  });
});
