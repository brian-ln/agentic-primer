/**
 * Tests for Actor Session UUID Generation
 *
 * Validates:
 * - Deterministic UUID generation
 * - Address normalization across formats
 * - Cache management
 * - Integration with session management
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import {
  actorAddressToSessionUUID,
  normalizeActorAddress,
  isValidUUID,
  registerActorSession,
  findSessionByAddress,
  findAddressByUUID,
  touchSession,
  clearSessionCache,
  getAllSessions,
  generateReadableSessionId,
  PRIMER_NAMESPACE,
  TEST_NAMESPACE,
} from './actor-session-uuid';

describe('actorAddressToSessionUUID', () => {
  test('generates valid RFC 4122 UUID', () => {
    const uuid = actorAddressToSessionUUID('primer/tasks/task_55');
    expect(isValidUUID(uuid)).toBe(true);
  });

  test('is deterministic (same input → same output)', () => {
    const address = 'primer/tasks/task_55';
    const uuid1 = actorAddressToSessionUUID(address);
    const uuid2 = actorAddressToSessionUUID(address);
    expect(uuid1).toBe(uuid2);
  });

  test('different addresses produce different UUIDs', () => {
    const uuid1 = actorAddressToSessionUUID('primer/tasks/task_55');
    const uuid2 = actorAddressToSessionUUID('primer/tasks/task_56');
    expect(uuid1).not.toBe(uuid2);
  });

  test('uses custom namespace when provided', () => {
    const address = 'primer/tasks/task_55';
    const uuid1 = actorAddressToSessionUUID(address, PRIMER_NAMESPACE);
    const uuid2 = actorAddressToSessionUUID(address, TEST_NAMESPACE);
    expect(uuid1).not.toBe(uuid2);
  });

  test('throws error for empty address', () => {
    expect(() => actorAddressToSessionUUID('')).toThrow('Actor address cannot be empty');
    expect(() => actorAddressToSessionUUID('   ')).toThrow('Actor address cannot be empty');
  });

  test('handles hierarchical paths', () => {
    const uuid = actorAddressToSessionUUID('primer/tasks/task_55/agent_a');
    expect(isValidUUID(uuid)).toBe(true);
  });

  test('handles Docker-style names', () => {
    const uuid = actorAddressToSessionUUID('happy-elephant-42');
    expect(isValidUUID(uuid)).toBe(true);
  });

  test('handles dot notation', () => {
    const uuid = actorAddressToSessionUUID('primer.tasks.task_55');
    expect(isValidUUID(uuid)).toBe(true);
  });
});

describe('normalizeActorAddress', () => {
  test('converts dots to slashes', () => {
    expect(normalizeActorAddress('primer.tasks.task_55')).toBe('primer/tasks/task_55');
  });

  test('collapses multiple slashes', () => {
    expect(normalizeActorAddress('primer//tasks///task_55')).toBe('primer/tasks/task_55');
  });

  test('removes leading/trailing slashes', () => {
    expect(normalizeActorAddress('/primer/tasks/task_55/')).toBe('primer/tasks/task_55');
  });

  test('converts to lowercase', () => {
    expect(normalizeActorAddress('Primer/Tasks/Task_55')).toBe('primer/tasks/task_55');
    expect(normalizeActorAddress('PRIMER.TASKS.TASK_55')).toBe('primer/tasks/task_55');
  });

  test('preserves hyphens', () => {
    expect(normalizeActorAddress('Happy-Elephant-42')).toBe('happy-elephant-42');
  });

  test('handles mixed formats', () => {
    expect(normalizeActorAddress('Primer.Tasks/Task_55')).toBe('primer/tasks/task_55');
  });

  test('trims whitespace', () => {
    expect(normalizeActorAddress('  primer/tasks/task_55  ')).toBe('primer/tasks/task_55');
  });
});

describe('normalizeActorAddress determinism', () => {
  test('equivalent addresses produce same UUID', () => {
    const addresses = [
      'primer/tasks/task_55',
      'Primer/Tasks/Task_55',
      'primer.tasks.task_55',
      '/primer/tasks/task_55/',
      'primer//tasks///task_55',
      '  primer/tasks/task_55  ',
    ];

    const uuids = addresses.map((addr) => actorAddressToSessionUUID(addr));
    const unique = new Set(uuids);
    expect(unique.size).toBe(1); // All should produce same UUID
  });
});

describe('isValidUUID', () => {
  test('validates correct UUIDs', () => {
    expect(isValidUUID('550e8400-e29b-41d4-a716-446655440000')).toBe(true);
    expect(isValidUUID('6ba7b810-9dad-11d1-80b4-00c04fd430c8')).toBe(true);
  });

  test('rejects invalid UUIDs', () => {
    expect(isValidUUID('not-a-uuid')).toBe(false);
    expect(isValidUUID('550e8400-e29b-41d4-a716')).toBe(false); // Too short
    expect(isValidUUID('550e8400-e29b-41d4-a716-446655440000-extra')).toBe(false); // Too long
    expect(isValidUUID('')).toBe(false);
  });

  test('is case-insensitive', () => {
    expect(isValidUUID('550E8400-E29B-41D4-A716-446655440000')).toBe(true);
  });
});

describe('Session Registry (Cache Management)', () => {
  beforeEach(() => {
    clearSessionCache();
  });

  describe('registerActorSession', () => {
    test('creates new session mapping', () => {
      const mapping = registerActorSession('primer/tasks/task_55');
      expect(mapping.address).toBe('primer/tasks/task_55');
      expect(mapping.canonicalAddress).toBe('primer/tasks/task_55');
      expect(isValidUUID(mapping.sessionUUID)).toBe(true);
      expect(mapping.createdAt).toBeInstanceOf(Date);
      expect(mapping.lastAccessedAt).toBeInstanceOf(Date);
    });

    test('accepts pre-generated UUID', () => {
      const customUUID = '550e8400-e29b-41d4-a716-446655440000';
      const mapping = registerActorSession('primer/tasks/task_55', customUUID);
      expect(mapping.sessionUUID).toBe(customUUID);
    });

    test('normalizes address in mapping', () => {
      const mapping = registerActorSession('Primer.Tasks.Task_55');
      expect(mapping.canonicalAddress).toBe('primer/tasks/task_55');
    });

    test('throws error for invalid custom UUID', () => {
      expect(() => registerActorSession('test', 'not-a-uuid')).toThrow('Invalid UUID');
    });
  });

  describe('findSessionByAddress', () => {
    test('finds registered session', () => {
      const original = registerActorSession('primer/tasks/task_55');
      const found = findSessionByAddress('primer/tasks/task_55');
      expect(found).toEqual(original);
    });

    test('finds session by normalized address', () => {
      const original = registerActorSession('primer/tasks/task_55');
      const found = findSessionByAddress('Primer.Tasks.Task_55');
      expect(found).toEqual(original);
    });

    test('returns undefined for unregistered address', () => {
      expect(findSessionByAddress('nonexistent')).toBeUndefined();
    });
  });

  describe('findAddressByUUID', () => {
    test('finds registered session by UUID', () => {
      const original = registerActorSession('primer/tasks/task_55');
      const found = findAddressByUUID(original.sessionUUID);
      expect(found).toEqual(original);
    });

    test('returns undefined for unknown UUID', () => {
      expect(findAddressByUUID('550e8400-e29b-41d4-a716-446655440000')).toBeUndefined();
    });
  });

  describe('touchSession', () => {
    test('updates lastAccessedAt timestamp', async () => {
      const mapping = registerActorSession('primer/tasks/task_55');
      const originalTime = mapping.lastAccessedAt.getTime();

      // Wait 10ms to ensure timestamp difference
      await new Promise((resolve) => setTimeout(resolve, 10));

      touchSession(mapping.sessionUUID);

      const updated = findAddressByUUID(mapping.sessionUUID);
      expect(updated?.lastAccessedAt.getTime()).toBeGreaterThan(originalTime);
    });

    test('does nothing for unknown UUID', () => {
      // Should not throw
      touchSession('550e8400-e29b-41d4-a716-446655440000');
    });
  });

  describe('getAllSessions', () => {
    test('returns all registered sessions', () => {
      registerActorSession('primer/tasks/task_55');
      registerActorSession('primer/tasks/task_56');
      registerActorSession('primer/knowledge/research');

      const all = getAllSessions();
      expect(all.length).toBe(3);
    });

    test('returns empty array when no sessions', () => {
      expect(getAllSessions()).toEqual([]);
    });
  });

  describe('clearSessionCache', () => {
    test('removes all sessions', () => {
      registerActorSession('primer/tasks/task_55');
      registerActorSession('primer/tasks/task_56');
      expect(getAllSessions().length).toBe(2);

      clearSessionCache();
      expect(getAllSessions().length).toBe(0);
    });
  });
});

describe('generateReadableSessionId', () => {
  test('includes short UUID prefix', () => {
    const readable = generateReadableSessionId('primer/tasks/task_55');
    const uuid = actorAddressToSessionUUID('primer/tasks/task_55');
    expect(readable.startsWith(uuid.slice(0, 8))).toBe(true);
  });

  test('includes address hint from path', () => {
    const readable = generateReadableSessionId('primer/tasks/task_55');
    expect(readable).toContain('task_55');
  });

  test('handles dot notation', () => {
    const readable = generateReadableSessionId('primer.tasks.task_55');
    expect(readable).toContain('task_55');
  });

  test('handles simple names', () => {
    const readable = generateReadableSessionId('happy-elephant-42');
    expect(readable).toContain('happy-elephant-42');
  });

  test('format is uuid-addresshint', () => {
    const readable = generateReadableSessionId('primer/tasks/task_55');
    expect(readable).toMatch(/^[0-9a-f]{8}-task_55$/);
  });
});

describe('Real-world scenarios', () => {
  beforeEach(() => {
    clearSessionCache();
  });

  test('scenario: hierarchical task agent addressing', () => {
    const addresses = [
      'primer/tasks/task_55/agent_a',
      'primer/tasks/task_55/agent_b',
      'primer/tasks/task_56/agent_a',
    ];

    const mappings = addresses.map((addr) => registerActorSession(addr));

    // All produce unique UUIDs
    const uuids = mappings.map((m) => m.sessionUUID);
    expect(new Set(uuids).size).toBe(3);

    // All are findable
    expect(findSessionByAddress('primer/tasks/task_55/agent_a')).toBeDefined();
    expect(findAddressByUUID(mappings[0].sessionUUID)).toBeDefined();
  });

  test('scenario: knowledge graph actor addressing', () => {
    const address = 'primer/knowledge/signal_detectors/research_agent';
    const mapping = registerActorSession(address);

    // Readable ID for logging
    const readable = generateReadableSessionId(address);
    expect(readable).toContain('research_agent');

    // Can find by various normalized forms
    expect(findSessionByAddress('Primer.Knowledge.Signal_Detectors.Research_Agent')).toEqual(
      mapping
    );
  });

  test('scenario: Docker-style container names', () => {
    const names = ['happy-elephant-42', 'clever-dolphin-7', 'brave-tiger-99'];
    const mappings = names.map((name) => registerActorSession(name));

    // Each gets unique UUID
    const uuids = mappings.map((m) => m.sessionUUID);
    expect(new Set(uuids).size).toBe(3);

    // Readable IDs preserve container name
    names.forEach((name) => {
      const readable = generateReadableSessionId(name);
      expect(readable).toContain(name);
    });
  });

  test('scenario: multi-format address equivalence', () => {
    // Register using one format
    const original = registerActorSession('primer/tasks/task_55');

    // Find using different formats
    const variants = [
      'Primer/Tasks/Task_55',
      'primer.tasks.task_55',
      '/primer/tasks/task_55/',
      'PRIMER.TASKS.TASK_55',
    ];

    variants.forEach((variant) => {
      const found = findSessionByAddress(variant);
      expect(found).toEqual(original);
    });
  });
});

describe('Namespace separation', () => {
  test('different namespaces produce different UUIDs for same address', () => {
    const address = 'primer/tasks/task_55';
    const primerUUID = actorAddressToSessionUUID(address, PRIMER_NAMESPACE);
    const testUUID = actorAddressToSessionUUID(address, TEST_NAMESPACE);

    expect(primerUUID).not.toBe(testUUID);
    expect(isValidUUID(primerUUID)).toBe(true);
    expect(isValidUUID(testUUID)).toBe(true);
  });

  test('namespaces are valid UUIDs', () => {
    expect(isValidUUID(PRIMER_NAMESPACE)).toBe(true);
    expect(isValidUUID(TEST_NAMESPACE)).toBe(true);
  });
});
