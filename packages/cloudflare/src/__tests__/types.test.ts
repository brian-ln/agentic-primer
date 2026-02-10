/**
 * Tests for address parsing utilities.
 * Pure logic tests -- no miniflare or Cloudflare bindings needed.
 */
import { describe, it, expect } from 'vitest';
import { parseDOAddress, parseWorkerAddress } from '../types.ts';

describe('parseDOAddress', () => {
  it('parses a valid DO address', () => {
    const result = parseDOAddress('do://brain/triage');
    expect(result).toEqual({ doName: 'brain', actorPath: 'triage' });
  });

  it('parses a DO address with multi-segment actor path', () => {
    const result = parseDOAddress('do://brain/agents/summarizer');
    expect(result).toEqual({
      doName: 'brain',
      actorPath: 'agents/summarizer',
    });
  });

  it('parses a DO address with hyphens and underscores', () => {
    const result = parseDOAddress('do://my-do_name/actor-path_v2');
    expect(result).toEqual({
      doName: 'my-do_name',
      actorPath: 'actor-path_v2',
    });
  });

  it('throws on missing protocol', () => {
    expect(() => parseDOAddress('brain/triage')).toThrow(
      'Invalid DO address format'
    );
  });

  it('throws on wrong protocol', () => {
    expect(() => parseDOAddress('worker://brain/triage')).toThrow(
      'Invalid DO address format'
    );
  });

  it('throws on missing actor path', () => {
    expect(() => parseDOAddress('do://brain')).toThrow(
      'Invalid DO address format'
    );
  });

  it('throws on missing actor path with trailing slash', () => {
    expect(() => parseDOAddress('do://brain/')).toThrow(
      'Invalid DO address format'
    );
  });

  it('throws on empty string', () => {
    expect(() => parseDOAddress('')).toThrow('Invalid DO address format');
  });

  it('throws on missing DO name', () => {
    expect(() => parseDOAddress('do:///triage')).toThrow(
      'Invalid DO address format'
    );
  });
});

describe('parseWorkerAddress', () => {
  it('parses a valid Worker address', () => {
    const result = parseWorkerAddress('worker://relay/ingest');
    expect(result).toEqual({ serviceName: 'relay', actorPath: 'ingest' });
  });

  it('parses a Worker address with multi-segment actor path', () => {
    const result = parseWorkerAddress('worker://api-gateway/v2/auth');
    expect(result).toEqual({
      serviceName: 'api-gateway',
      actorPath: 'v2/auth',
    });
  });

  it('parses a Worker address with special characters in name', () => {
    const result = parseWorkerAddress('worker://my_service-v2/handler');
    expect(result).toEqual({
      serviceName: 'my_service-v2',
      actorPath: 'handler',
    });
  });

  it('throws on missing protocol', () => {
    expect(() => parseWorkerAddress('relay/ingest')).toThrow(
      'Invalid Worker address format'
    );
  });

  it('throws on wrong protocol', () => {
    expect(() => parseWorkerAddress('do://relay/ingest')).toThrow(
      'Invalid Worker address format'
    );
  });

  it('throws on missing actor path', () => {
    expect(() => parseWorkerAddress('worker://relay')).toThrow(
      'Invalid Worker address format'
    );
  });

  it('throws on missing actor path with trailing slash', () => {
    expect(() => parseWorkerAddress('worker://relay/')).toThrow(
      'Invalid Worker address format'
    );
  });

  it('throws on empty string', () => {
    expect(() => parseWorkerAddress('')).toThrow(
      'Invalid Worker address format'
    );
  });

  it('throws on missing service name', () => {
    expect(() => parseWorkerAddress('worker:///ingest')).toThrow(
      'Invalid Worker address format'
    );
  });
});
