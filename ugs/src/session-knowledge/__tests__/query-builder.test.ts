/**
 * Query Builder Tests
 * Epic: agentic-primer-0lg.2
 */

import { describe, test, expect } from 'bun:test';
import {
  queryBuilder,
  buildLikePattern,
  buildLikeQuery,
  buildSearchQuery,
  type LikeMode,
} from '../security/query-builder';

describe('buildLikePattern', () => {
  test('contains mode adds wildcards on both sides', () => {
    const { pattern, escape } = buildLikePattern('test', 'contains');
    expect(pattern).toBe('%test%');
    expect(escape).toBe(true);
  });

  test('starts mode adds wildcard at end', () => {
    const { pattern, escape } = buildLikePattern('prefix', 'starts');
    expect(pattern).toBe('prefix%');
    expect(escape).toBe(true);
  });

  test('ends mode adds wildcard at start', () => {
    const { pattern, escape } = buildLikePattern('suffix', 'ends');
    expect(pattern).toBe('%suffix');
    expect(escape).toBe(true);
  });

  test('exact mode has no wildcards', () => {
    const { pattern, escape } = buildLikePattern('exact', 'exact');
    expect(pattern).toBe('exact');
    expect(escape).toBe(true);
  });

  test('sanitizes percent wildcards', () => {
    const { pattern } = buildLikePattern('100%', 'contains');
    expect(pattern).toBe('%100\\%%');
  });

  test('sanitizes underscore wildcards', () => {
    const { pattern } = buildLikePattern('file_name', 'contains');
    expect(pattern).toBe('%file\\_name%');
  });

  test('sanitizes backslashes', () => {
    const { pattern } = buildLikePattern('path\\to\\file', 'contains');
    expect(pattern).toBe('%path\\\\to\\\\file%');
  });

  test('sanitizes complex patterns', () => {
    const { pattern } = buildLikePattern('Use % and _ wildcards', 'contains');
    expect(pattern).toBe('%Use \\% and \\_ wildcards%');
  });
});

describe('QueryBuilder - Basic Construction', () => {
  test('builds simple SELECT query', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .build();

    expect(query.sql).toBe('SELECT * FROM session_decisions');
    expect(query.params).toEqual([]);
  });

  test('builds query with specific columns', () => {
    const query = queryBuilder()
      .select('id', 'decision', 'timestamp')
      .from('session_decisions')
      .build();

    expect(query.sql).toBe('SELECT id, decision, timestamp FROM session_decisions');
    expect(query.params).toEqual([]);
  });

  test('throws error when no columns specified', () => {
    expect(() => {
      queryBuilder().from('table').build();
    }).toThrow('No columns specified');
  });

  test('throws error when no table specified', () => {
    expect(() => {
      queryBuilder().select('*').build();
    }).toThrow('No table specified');
  });

  test('validates table name', () => {
    expect(() => {
      queryBuilder().select('*').from('invalid-table').build();
    }).toThrow('Invalid table name');
  });
});

describe('QueryBuilder - WHERE Conditions', () => {
  test('adds simple WHERE condition', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .where('id = ?', 'abc123')
      .build();

    expect(query.sql).toBe('SELECT * FROM session_decisions WHERE id = ?');
    expect(query.params).toEqual(['abc123']);
  });

  test('adds multiple WHERE conditions with AND', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .where('timestamp >= ?', 1000)
      .where('timestamp < ?', 2000)
      .build();

    expect(query.sql).toBe('SELECT * FROM session_decisions WHERE timestamp >= ? AND timestamp < ?');
    expect(query.params).toEqual([1000, 2000]);
  });

  test('handles multiple parameters in single condition', () => {
    const query = queryBuilder()
      .select('*')
      .from('table')
      .where('col1 = ? AND col2 = ?', 'value1', 'value2')
      .build();

    expect(query.sql).toBe('SELECT * FROM table WHERE col1 = ? AND col2 = ?');
    expect(query.params).toEqual(['value1', 'value2']);
  });
});

describe('QueryBuilder - LIKE Conditions', () => {
  test('adds LIKE condition with proper sanitization', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('decision', 'auth', 'contains')
      .build();

    expect(query.sql).toBe("SELECT * FROM session_decisions WHERE decision LIKE ? ESCAPE '\\\\'");
    expect(query.params).toEqual(['%auth%']);
  });

  test('sanitizes wildcards in LIKE pattern', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('decision', 'Use % wildcard', 'contains')
      .build();

    expect(query.sql).toBe("SELECT * FROM session_decisions WHERE decision LIKE ? ESCAPE '\\\\'");
    expect(query.params).toEqual(['%Use \\% wildcard%']);
  });

  test('validates column name in LIKE', () => {
    expect(() => {
      queryBuilder()
        .select('*')
        .from('table')
        .whereLike('invalid-column', 'test', 'contains')
        .build();
    }).toThrow('Invalid column name');
  });

  test('supports all LIKE modes', () => {
    const modes: LikeMode[] = ['contains', 'starts', 'ends', 'exact'];
    const expectedPatterns = ['%test%', 'test%', '%test', 'test'];

    modes.forEach((mode, i) => {
      const query = queryBuilder()
        .select('*')
        .from('table')
        .whereLike('col', 'test', mode)
        .build();

      expect(query.params).toEqual([expectedPatterns[i]]);
    });
  });
});

describe('QueryBuilder - Multi-Column LIKE', () => {
  test('builds OR-based LIKE query', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereOrLike(['decision', 'reasoning'], 'auth', 'contains')
      .build();

    expect(query.sql).toBe(
      "SELECT * FROM session_decisions WHERE (decision LIKE ? ESCAPE '\\\\' OR reasoning LIKE ? ESCAPE '\\\\')"
    );
    expect(query.params).toEqual(['%auth%', '%auth%']);
  });

  test('supports three columns', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereOrLike(['decision', 'reasoning', 'alternatives'], 'test', 'contains')
      .build();

    expect(query.sql).toContain('decision LIKE ?');
    expect(query.sql).toContain('OR reasoning LIKE ?');
    expect(query.sql).toContain('OR alternatives LIKE ?');
    expect(query.params).toEqual(['%test%', '%test%', '%test%']);
  });

  test('throws error with empty column array', () => {
    expect(() => {
      queryBuilder()
        .select('*')
        .from('table')
        .whereOrLike([], 'test', 'contains')
        .build();
    }).toThrow('At least one column required');
  });

  test('validates all column names', () => {
    expect(() => {
      queryBuilder()
        .select('*')
        .from('table')
        .whereOrLike(['valid_col', 'invalid-col'], 'test', 'contains')
        .build();
    }).toThrow('Invalid column name');
  });

  test('sanitizes patterns in multi-column search', () => {
    const query = queryBuilder()
      .select('*')
      .from('table')
      .whereOrLike(['col1', 'col2'], 'test%_value', 'contains')
      .build();

    expect(query.params).toEqual(['%test\\%\\_value%', '%test\\%\\_value%']);
  });
});

describe('QueryBuilder - ORDER BY', () => {
  test('adds ORDER BY clause', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .orderBy('timestamp DESC')
      .build();

    expect(query.sql).toBe('SELECT * FROM session_decisions ORDER BY timestamp DESC');
  });

  test('supports multiple sort columns', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .orderBy('decision ASC, timestamp DESC')
      .build();

    expect(query.sql).toContain('ORDER BY decision ASC, timestamp DESC');
  });

  test('validates ORDER BY clause', () => {
    expect(() => {
      queryBuilder()
        .select('*')
        .from('table')
        .orderBy('col; DROP TABLE users;')
        .build();
    }).toThrow('Invalid ORDER BY clause');
  });
});

describe('QueryBuilder - LIMIT', () => {
  test('adds LIMIT clause', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .limit(100)
      .build();

    expect(query.sql).toBe('SELECT * FROM session_decisions LIMIT 100');
  });

  test('validates LIMIT is positive integer', () => {
    expect(() => {
      queryBuilder().select('*').from('table').limit(0).build();
    }).toThrow('Invalid LIMIT value');

    expect(() => {
      queryBuilder().select('*').from('table').limit(-5).build();
    }).toThrow('Invalid LIMIT value');

    expect(() => {
      queryBuilder().select('*').from('table').limit(3.14).build();
    }).toThrow('Invalid LIMIT value');
  });
});

describe('QueryBuilder - Complex Queries', () => {
  test('builds complete query with all clauses', () => {
    const query = queryBuilder()
      .select('id', 'decision', 'timestamp')
      .from('session_decisions')
      .where('timestamp >= ?', 1000)
      .where('timestamp < ?', 2000)
      .whereLike('decision', 'auth', 'contains')
      .orderBy('timestamp DESC')
      .limit(50)
      .build();

    expect(query.sql).toBe(
      "SELECT id, decision, timestamp FROM session_decisions WHERE timestamp >= ? AND timestamp < ? AND decision LIKE ? ESCAPE '\\\\' ORDER BY timestamp DESC LIMIT 50"
    );
    expect(query.params).toEqual([1000, 2000, '%auth%']);
  });

  test('mixes WHERE and LIKE conditions', () => {
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .where('session_id = ?', 'abc123')
      .whereLike('decision', 'security', 'contains')
      .whereLike('reasoning', 'injection', 'contains')
      .build();

    expect(query.sql).toContain('WHERE session_id = ?');
    expect(query.sql).toContain("AND decision LIKE ? ESCAPE '\\\\'");
    expect(query.sql).toContain("AND reasoning LIKE ? ESCAPE '\\\\'");
    expect(query.params).toEqual(['abc123', '%security%', '%injection%']);
  });
});

describe('Convenience Functions', () => {
  test('buildLikeQuery creates simple LIKE query', () => {
    const query = buildLikeQuery(
      'session_decisions',
      'decision',
      'auth',
      'contains',
      100
    );

    expect(query.sql).toBe(
      "SELECT * FROM session_decisions WHERE decision LIKE ? ESCAPE '\\\\' LIMIT 100"
    );
    expect(query.params).toEqual(['%auth%']);
  });

  test('buildLikeQuery without limit', () => {
    const query = buildLikeQuery(
      'session_decisions',
      'decision',
      'auth',
      'contains'
    );

    expect(query.sql).not.toContain('LIMIT');
  });

  test('buildSearchQuery creates multi-column search', () => {
    const query = buildSearchQuery(
      'session_decisions',
      ['decision', 'reasoning', 'alternatives'],
      'security',
      'contains',
      50
    );

    expect(query.sql).toContain('decision LIKE ?');
    expect(query.sql).toContain('OR reasoning LIKE ?');
    expect(query.sql).toContain('OR alternatives LIKE ?');
    expect(query.sql).toContain('LIMIT 50');
    expect(query.params).toEqual(['%security%', '%security%', '%security%']);
  });

  test('buildSearchQuery without limit', () => {
    const query = buildSearchQuery(
      'table',
      ['col1', 'col2'],
      'test'
    );

    expect(query.sql).not.toContain('LIMIT');
  });
});

describe('SQL Injection Prevention', () => {
  test('prevents wildcard data exfiltration via %', () => {
    const maliciousInput = '%';
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('decision', maliciousInput, 'contains')
      .build();

    expect(query.params).toEqual(['%\\%%']);
    // Pattern %\%% matches only literal "%", not all records
  });

  test('prevents wildcard data exfiltration via multiple %', () => {
    const maliciousInput = '%%%';
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('decision', maliciousInput, 'contains')
      .build();

    expect(query.params).toEqual(['%\\%\\%\\%%']);
  });

  test('prevents underscore wildcard attacks', () => {
    const maliciousInput = '_______________';
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('decision', maliciousInput, 'contains')
      .build();

    expect(query.params[0]).toContain('\\_\\_\\_');
  });

  test('prevents backslash escape bypass', () => {
    const maliciousInput = '\\%test';
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('decision', maliciousInput, 'contains')
      .build();

    expect(query.params).toEqual(['%\\\\\\%test%']);
  });

  test('prevents SQL injection via table name', () => {
    expect(() => {
      queryBuilder()
        .select('*')
        .from('table; DROP TABLE users; --')
        .build();
    }).toThrow('Invalid table name');
  });

  test('prevents SQL injection via column name', () => {
    expect(() => {
      queryBuilder()
        .select('*')
        .from('table')
        .whereLike('col; DROP TABLE users; --', 'test', 'contains')
        .build();
    }).toThrow('Invalid column name');
  });

  test('handles SQL injection attempts in pattern', () => {
    const maliciousInput = "'; DROP TABLE session_decisions; --";
    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('decision', maliciousInput, 'contains')
      .build();

    // Pattern is safely escaped, SQL injection is neutralized
    expect(query.sql).toContain('WHERE decision LIKE ?');
    expect(query.params[0]).toContain("'; DROP TABLE");
    // The malicious SQL is treated as literal search text
  });
});

describe('Edge Cases', () => {
  test('handles empty pattern', () => {
    const query = queryBuilder()
      .select('*')
      .from('table')
      .whereLike('col', '', 'contains')
      .build();

    expect(query.params).toEqual(['%%']);
  });

  test('handles pattern with only wildcards', () => {
    const query = queryBuilder()
      .select('*')
      .from('table')
      .whereLike('col', '%%%___', 'contains')
      .build();

    expect(query.params).toEqual(['%\\%\\%\\%\\_\\_\\_%']);
  });

  test('handles unicode in pattern', () => {
    const query = queryBuilder()
      .select('*')
      .from('table')
      .whereLike('col', 'emoji 🔒 test', 'contains')
      .build();

    expect(query.params).toEqual(['%emoji 🔒 test%']);
  });

  test('handles very long pattern', () => {
    const longPattern = 'a'.repeat(1000);
    const query = queryBuilder()
      .select('*')
      .from('table')
      .whereLike('col', longPattern, 'contains')
      .build();

    expect(query.params[0]).toHaveLength(1002); // +2 for % wildcards
  });
});
