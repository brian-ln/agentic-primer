#!/usr/bin/env bun
import { describe, expect, test } from 'bun:test';
import {
  toProtocolAddress,
  fromProtocolAddress,
  addressToString,
  parseProtocolAddress,
  validateProtocolAddress,
  createProtocolAddress,
} from '../index.ts';
import { address } from '@agentic-primer/actors';

describe('Protocol Adapter', () => {
  describe('toProtocolAddress', () => {
    test('converts flat ID to protocol address', () => {
      const seagAddr = address('actor-123');
      const protocolAddr = toProtocolAddress(seagAddr);

      expect(protocolAddr).toEqual({
        id: 'actor-123',
        scope: 'node',
      });
    });

    test('converts hierarchical path to protocol address', () => {
      const seagAddr = address('domain/inference');
      const protocolAddr = toProtocolAddress(seagAddr);

      expect(protocolAddr).toEqual({
        id: 'inference',
        namespace: 'domain',
        scope: 'node',
      });
    });

    test('converts deep path to protocol address', () => {
      const seagAddr = address('workflows/build/tasks/compile');
      const protocolAddr = toProtocolAddress(seagAddr);

      expect(protocolAddr).toEqual({
        id: 'compile',
        namespace: 'workflows/build/tasks',
        scope: 'node',
      });
    });

    test('handles single segment namespace', () => {
      const seagAddr = address('services/llm');
      const protocolAddr = toProtocolAddress(seagAddr);

      expect(protocolAddr).toEqual({
        id: 'llm',
        namespace: 'services',
        scope: 'node',
      });
    });
  });

  describe('fromProtocolAddress', () => {
    test('converts flat protocol address to SEAG address', () => {
      const protocolAddr = {
        id: 'actor-123',
        scope: 'node' as const,
      };

      const seagAddr = fromProtocolAddress(protocolAddr);
      expect(seagAddr).toBe('@(actor-123)');
    });

    test('converts namespaced protocol address to SEAG address', () => {
      const protocolAddr = {
        id: 'inference',
        namespace: 'domain',
        scope: 'node' as const,
      };

      const seagAddr = fromProtocolAddress(protocolAddr);
      expect(seagAddr).toBe('@(domain/inference)');
    });

    test('converts deep namespace to SEAG address', () => {
      const protocolAddr = {
        id: 'compile',
        namespace: 'workflows/build/tasks',
        scope: 'node' as const,
      };

      const seagAddr = fromProtocolAddress(protocolAddr);
      expect(seagAddr).toBe('@(workflows/build/tasks/compile)');
    });

    test('ignores version in conversion to SEAG', () => {
      const protocolAddr = {
        id: 'inference',
        namespace: 'domain',
        scope: 'node' as const,
        version: 'v1',
      };

      const seagAddr = fromProtocolAddress(protocolAddr);
      expect(seagAddr).toBe('@(domain/inference)');
    });
  });

  describe('round-trip conversion', () => {
    test('flat ID round-trips correctly', () => {
      const original = address('actor-123');
      const protocol = toProtocolAddress(original);
      const back = fromProtocolAddress(protocol);

      expect(back).toBe(original);
    });

    test('hierarchical path round-trips correctly', () => {
      const original = address('domain/inference');
      const protocol = toProtocolAddress(original);
      const back = fromProtocolAddress(protocol);

      expect(back).toBe(original);
    });

    test('deep path round-trips correctly', () => {
      const original = address('workflows/build/tasks/compile');
      const protocol = toProtocolAddress(original);
      const back = fromProtocolAddress(protocol);

      expect(back).toBe(original);
    });
  });

  describe('addressToString', () => {
    test('serializes flat ID', () => {
      const addr = { id: 'actor-123', scope: 'node' as const };
      expect(addressToString(addr)).toBe('actor-123');
    });

    test('serializes namespaced address', () => {
      const addr = {
        id: 'inference',
        namespace: 'domain',
        scope: 'node' as const,
      };
      expect(addressToString(addr)).toBe('domain/inference');
    });

    test('serializes versioned address', () => {
      const addr = {
        id: 'inference',
        namespace: 'domain',
        scope: 'node' as const,
        version: 'v1',
      };
      expect(addressToString(addr)).toBe('domain/inference@v1');
    });

    test('serializes deep namespace', () => {
      const addr = {
        id: 'compile',
        namespace: 'workflows/build/tasks',
        scope: 'node' as const,
      };
      expect(addressToString(addr)).toBe('workflows/build/tasks/compile');
    });
  });

  describe('parseProtocolAddress', () => {
    test('parses flat ID', () => {
      const addr = parseProtocolAddress('actor-123');
      expect(addr).toEqual({
        id: 'actor-123',
        namespace: null,
        scope: 'node',
      });
    });

    test('parses namespaced address', () => {
      const addr = parseProtocolAddress('domain/inference');
      expect(addr).toEqual({
        id: 'inference',
        namespace: 'domain',
        scope: 'node',
      });
    });

    test('parses versioned address', () => {
      const addr = parseProtocolAddress('domain/inference@v1');
      expect(addr).toEqual({
        id: 'inference',
        namespace: 'domain',
        scope: 'node',
        version: 'v1',
      });
    });

    test('parses deep namespace', () => {
      const addr = parseProtocolAddress('workflows/build/tasks/compile');
      expect(addr).toEqual({
        id: 'compile',
        namespace: 'workflows/build/tasks',
        scope: 'node',
      });
    });

    test('throws on empty string', () => {
      expect(() => parseProtocolAddress('')).toThrow();
    });

    test('throws on only slashes', () => {
      expect(() => parseProtocolAddress('///')).toThrow();
    });
  });

  describe('string round-trip', () => {
    test('flat ID round-trips', () => {
      const original = 'actor-123';
      const addr = parseProtocolAddress(original);
      const back = addressToString(addr);

      expect(back).toBe(original);
    });

    test('namespaced address round-trips', () => {
      const original = 'domain/inference';
      const addr = parseProtocolAddress(original);
      const back = addressToString(addr);

      expect(back).toBe(original);
    });

    test('versioned address round-trips', () => {
      const original = 'domain/inference@v1';
      const addr = parseProtocolAddress(original);
      const back = addressToString(addr);

      expect(back).toBe(original);
    });
  });

  describe('validateProtocolAddress', () => {
    test('validates correct flat address', () => {
      const addr = { id: 'actor-123', scope: 'node' as const };
      expect(validateProtocolAddress(addr)).toBe(true);
    });

    test('validates correct namespaced address', () => {
      const addr = {
        id: 'inference',
        namespace: 'domain',
        scope: 'node' as const,
      };
      expect(validateProtocolAddress(addr)).toBe(true);
    });

    test('rejects address without id', () => {
      const addr = { scope: 'node' as const } as any;
      expect(validateProtocolAddress(addr)).toBe(false);
    });

    test('rejects address without scope', () => {
      const addr = { id: 'test' } as any;
      expect(validateProtocolAddress(addr)).toBe(false);
    });
  });

  describe('createProtocolAddress', () => {
    test('creates flat address', () => {
      const addr = createProtocolAddress('actor-123');
      expect(addr).toEqual({
        id: 'actor-123',
        namespace: null,
        scope: 'node',
      });
    });

    test('creates namespaced address', () => {
      const addr = createProtocolAddress('inference', 'domain');
      expect(addr).toEqual({
        id: 'inference',
        namespace: 'domain',
        scope: 'node',
      });
    });

    test('creates versioned address', () => {
      const addr = createProtocolAddress('inference', 'domain', 'v1');
      expect(addr).toEqual({
        id: 'inference',
        namespace: 'domain',
        scope: 'node',
        version: 'v1',
      });
    });

    test('validates during creation', () => {
      expect(() => createProtocolAddress('')).toThrow();
    });
  });

  describe('integration with path-based addressing', () => {
    test('hierarchical routing paths work with protocol addresses', () => {
      // SEAG hierarchical path
      const seagPath = address('workflows/build-pipeline/tasks/compile');

      // Convert to protocol
      const protocol = toProtocolAddress(seagPath);
      expect(protocol.namespace).toBe('workflows/build-pipeline/tasks');
      expect(protocol.id).toBe('compile');

      // Convert back
      const back = fromProtocolAddress(protocol);
      expect(back).toBe(seagPath);
    });

    test('namespace can be used for hierarchical routing', () => {
      const addr = createProtocolAddress('compile', 'workflows/build-pipeline/tasks');

      // Namespace represents the supervision tree path
      expect(addr.namespace).toBe('workflows/build-pipeline/tasks');

      // Convert to SEAG for routing
      const seagAddr = fromProtocolAddress(addr);
      expect(seagAddr).toBe('@(workflows/build-pipeline/tasks/compile)');
    });
  });
});
