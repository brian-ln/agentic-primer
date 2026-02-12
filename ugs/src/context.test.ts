import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import {
  ExecutionContext,
  getCurrentContext,
  runWithContext,
  runWithContextAsync,
  createContextFromEnv,
  withContext,
} from './context';

describe('ExecutionContext', () => {
  describe('constructor', () => {
    test('creates with defaults', () => {
      const ctx = new ExecutionContext();
      expect(ctx.principal.id).toBe('system');
      expect(ctx.principal.type).toBe('system');
      expect(ctx.credentials.size).toBe(0);
      expect(ctx.config.size).toBe(0);
    });

    test('creates with provided values', () => {
      const ctx = new ExecutionContext({
        principal: { id: 'user-1', type: 'human' },
        credentials: { API_KEY: 'secret' },
        config: { timeout: 5000 },
        metadata: { requestId: 'req-123' },
      });

      expect(ctx.principal.id).toBe('user-1');
      expect(ctx.credentials.get('API_KEY')).toBe('secret');
      expect(ctx.config.get('timeout')).toBe(5000);
      expect(ctx.metadata.get('requestId')).toBe('req-123');
    });
  });

  describe('getCredential', () => {
    const originalEnv = process.env.TEST_CRED;

    afterEach(() => {
      if (originalEnv !== undefined) {
        process.env.TEST_CRED = originalEnv;
      } else {
        delete process.env.TEST_CRED;
      }
    });

    test('returns credential from context', () => {
      const ctx = new ExecutionContext({
        credentials: { TEST_CRED: 'from-context' },
      });
      expect(ctx.getCredential('TEST_CRED')).toBe('from-context');
    });

    test('falls back to env var', () => {
      process.env.TEST_CRED = 'from-env';
      const ctx = new ExecutionContext();
      expect(ctx.getCredential('TEST_CRED')).toBe('from-env');
    });

    test('context takes precedence over env', () => {
      process.env.TEST_CRED = 'from-env';
      const ctx = new ExecutionContext({
        credentials: { TEST_CRED: 'from-context' },
      });
      expect(ctx.getCredential('TEST_CRED')).toBe('from-context');
    });

    test('returns undefined if not found', () => {
      const ctx = new ExecutionContext();
      expect(ctx.getCredential('NONEXISTENT')).toBeUndefined();
    });
  });

  describe('getConfig', () => {
    test('returns config value', () => {
      const ctx = new ExecutionContext({
        config: { timeout: 5000 },
      });
      expect(ctx.getConfig('timeout')).toBe(5000);
    });

    test('returns default if not found', () => {
      const ctx = new ExecutionContext();
      expect(ctx.getConfig('timeout', 3000)).toBe(3000);
    });

    test('returns undefined if not found and no default', () => {
      const ctx = new ExecutionContext();
      expect(ctx.getConfig('timeout')).toBeUndefined();
    });
  });

  describe('child', () => {
    test('inherits parent values', () => {
      const parent = new ExecutionContext({
        principal: { id: 'parent', type: 'human' },
        credentials: { KEY1: 'value1' },
        config: { setting1: 'a' },
      });

      const child = parent.child();

      expect(child.principal.id).toBe('parent');
      expect(child.credentials.get('KEY1')).toBe('value1');
      expect(child.config.get('setting1')).toBe('a');
    });

    test('overrides with new values', () => {
      const parent = new ExecutionContext({
        principal: { id: 'parent', type: 'human' },
        credentials: { KEY1: 'value1' },
        config: { setting1: 'a' },
      });

      const child = parent.child({
        principal: { id: 'child', type: 'agent' },
        credentials: { KEY2: 'value2' },
        config: { setting1: 'b' },
      });

      expect(child.principal.id).toBe('child');
      expect(child.credentials.get('KEY1')).toBe('value1');  // Inherited
      expect(child.credentials.get('KEY2')).toBe('value2');  // New
      expect(child.config.get('setting1')).toBe('b');  // Overridden
    });
  });

  describe('delegate', () => {
    test('creates context with onBehalfOf', () => {
      const parent = new ExecutionContext({
        principal: { id: 'human-1', type: 'human' },
        credentials: { KEY1: 'value1' },
      });

      const delegated = parent.delegate({ id: 'agent-1', type: 'agent' });

      expect(delegated.principal.id).toBe('agent-1');
      expect(delegated.principal.type).toBe('agent');
      expect(delegated.principal.onBehalfOf).toBe('human-1');
      expect(delegated.credentials.get('KEY1')).toBe('value1');  // Inherited
    });
  });

  describe('toJSON', () => {
    test('redacts credentials', () => {
      const ctx = new ExecutionContext({
        principal: { id: 'user-1', type: 'human' },
        credentials: { SECRET: 'super-secret' },
        config: { timeout: 5000 },
      });

      const json = ctx.toJSON();

      expect(json.principal.id).toBe('user-1');
      expect(json.credentials).toBe('[REDACTED]');
      expect(json.config.timeout).toBe(5000);
    });
  });
});

describe('Context storage', () => {
  describe('getCurrentContext', () => {
    test('returns default context when none set', () => {
      const ctx = getCurrentContext();
      expect(ctx.principal.id).toBe('system');
    });
  });

  describe('runWithContext', () => {
    test('provides context to sync function', () => {
      const ctx = new ExecutionContext({
        principal: { id: 'test-user', type: 'human' },
      });

      const result = runWithContext(ctx, () => {
        const current = getCurrentContext();
        return current.principal.id;
      });

      expect(result).toBe('test-user');
    });

    test('context is isolated', () => {
      const ctx1 = new ExecutionContext({
        principal: { id: 'user-1', type: 'human' },
      });
      const ctx2 = new ExecutionContext({
        principal: { id: 'user-2', type: 'human' },
      });

      let result1: string | undefined;
      let result2: string | undefined;

      runWithContext(ctx1, () => {
        result1 = getCurrentContext().principal.id;
        runWithContext(ctx2, () => {
          result2 = getCurrentContext().principal.id;
        });
      });

      expect(result1).toBe('user-1');
      expect(result2).toBe('user-2');
    });
  });

  describe('runWithContextAsync', () => {
    test('provides context to async function', async () => {
      const ctx = new ExecutionContext({
        principal: { id: 'async-user', type: 'human' },
      });

      const result = await runWithContextAsync(ctx, async () => {
        await Promise.resolve();  // Simulate async operation
        const current = getCurrentContext();
        return current.principal.id;
      });

      expect(result).toBe('async-user');
    });

    test('context flows through async calls', async () => {
      const ctx = new ExecutionContext({
        principal: { id: 'flow-user', type: 'human' },
        credentials: { TOKEN: 'abc123' },
      });

      const innerFn = async () => {
        await Promise.resolve();
        return getCurrentContext().getCredential('TOKEN');
      };

      const outerFn = async () => {
        await Promise.resolve();
        return innerFn();
      };

      const result = await runWithContextAsync(ctx, outerFn);

      expect(result).toBe('abc123');
    });
  });

  describe('withContext wrapper', () => {
    test('wraps function to receive context', () => {
      const fn = withContext((ctx, x: number, y: number) => {
        return `${ctx.principal.id}: ${x + y}`;
      });

      const ctx = new ExecutionContext({
        principal: { id: 'wrapper-user', type: 'human' },
      });

      const result = runWithContext(ctx, () => fn(1, 2));

      expect(result).toBe('wrapper-user: 3');
    });
  });
});

describe('createContextFromEnv', () => {
  const originalCloudflare = process.env.CLOUDFLARE_API_TOKEN;

  afterEach(() => {
    if (originalCloudflare !== undefined) {
      process.env.CLOUDFLARE_API_TOKEN = originalCloudflare;
    } else {
      delete process.env.CLOUDFLARE_API_TOKEN;
    }
  });

  test('loads credentials from env', () => {
    process.env.CLOUDFLARE_API_TOKEN = 'test-token';
    const ctx = createContextFromEnv();

    expect(ctx.credentials.get('CLOUDFLARE_API_TOKEN')).toBe('test-token');
  });

  test('uses provided principal', () => {
    const ctx = createContextFromEnv({ id: 'custom', type: 'agent' });
    expect(ctx.principal.id).toBe('custom');
    expect(ctx.principal.type).toBe('agent');
  });

  test('defaults to cli human principal', () => {
    const ctx = createContextFromEnv();
    expect(ctx.principal.id).toBe('cli');
    expect(ctx.principal.type).toBe('human');
  });
});
