#!/usr/bin/env bun
/**
 * ExecutionContext - Simple context passing for credentials and config
 *
 * Provides a way to pass credentials, config, and principal info through
 * the call chain without explicit parameter passing.
 *
 * Uses AsyncLocalStorage for automatic propagation through async calls.
 */

import { AsyncLocalStorage } from 'node:async_hooks';

/**
 * Principal information - who is making the request
 */
export interface Principal {
  id: string;
  type: 'human' | 'agent' | 'session' | 'system';
  // For compound principals (agent acting on behalf of human)
  onBehalfOf?: string;
}

/**
 * ExecutionContext - holds credentials, config, and principal for a request
 */
export class ExecutionContext {
  readonly principal: Principal;
  readonly credentials: Map<string, string>;
  readonly config: Map<string, any>;
  readonly metadata: Map<string, any>;

  constructor(options: {
    principal?: Principal;
    credentials?: Record<string, string>;
    config?: Record<string, any>;
    metadata?: Record<string, any>;
  } = {}) {
    this.principal = options.principal || { id: 'system', type: 'system' };
    this.credentials = new Map(Object.entries(options.credentials || {}));
    this.config = new Map(Object.entries(options.config || {}));
    this.metadata = new Map(Object.entries(options.metadata || {}));
  }

  /**
   * Get a credential, falling back to environment variable
   */
  getCredential(key: string): string | undefined {
    return this.credentials.get(key) ?? process.env[key];
  }

  /**
   * Get a config value with optional default
   */
  getConfig<T = any>(key: string, defaultValue?: T): T | undefined {
    return (this.config.get(key) as T) ?? defaultValue;
  }

  /**
   * Create a child context with additional/overridden values
   */
  child(options: {
    principal?: Principal;
    credentials?: Record<string, string>;
    config?: Record<string, any>;
    metadata?: Record<string, any>;
  } = {}): ExecutionContext {
    return new ExecutionContext({
      principal: options.principal || this.principal,
      credentials: {
        ...Object.fromEntries(this.credentials),
        ...options.credentials,
      },
      config: {
        ...Object.fromEntries(this.config),
        ...options.config,
      },
      metadata: {
        ...Object.fromEntries(this.metadata),
        ...options.metadata,
      },
    });
  }

  /**
   * Create context for a delegated call (e.g., agent invoking program)
   */
  delegate(newPrincipal: Principal): ExecutionContext {
    return this.child({
      principal: {
        ...newPrincipal,
        onBehalfOf: this.principal.id,
      },
    });
  }

  toJSON() {
    return {
      principal: this.principal,
      credentials: '[REDACTED]',  // Never serialize credentials
      config: Object.fromEntries(this.config),
      metadata: Object.fromEntries(this.metadata),
    };
  }
}

// AsyncLocalStorage for automatic context propagation
const contextStorage = new AsyncLocalStorage<ExecutionContext>();

/**
 * Get the current execution context
 * Returns a default system context if none is set
 */
export function getCurrentContext(): ExecutionContext {
  return contextStorage.getStore() ?? new ExecutionContext();
}

/**
 * Run a function with a specific execution context
 */
export function runWithContext<T>(context: ExecutionContext, fn: () => T): T {
  return contextStorage.run(context, fn);
}

/**
 * Run an async function with a specific execution context
 */
export async function runWithContextAsync<T>(
  context: ExecutionContext,
  fn: () => Promise<T>
): Promise<T> {
  return contextStorage.run(context, fn);
}

/**
 * Create a context from environment variables (for CLI usage)
 */
export function createContextFromEnv(principal?: Principal): ExecutionContext {
  // Common credential env vars (support both /ai config names and standard names)
  const credentialKeys = [
    'CLOUDFLARE_API_TOKEN',
    'CLOUDFLARE_ACCOUNT_ID',
    'CLOUDFLARE_GATEWAY_ID',
    'CF_AIG_TOKEN',
    'CF_ACCOUNT_ID',
    'CF_GATEWAY_NAME',
    'OPENAI_API_KEY',
    'ANTHROPIC_API_KEY',
  ];

  const credentials: Record<string, string> = {};
  for (const key of credentialKeys) {
    const value = process.env[key];
    if (value) {
      credentials[key] = value;
    }
  }

  return new ExecutionContext({
    principal: principal || { id: 'cli', type: 'human' },
    credentials,
  });
}

/**
 * Context-aware wrapper for functions that need credentials
 * Usage: const apiCall = withContext(async (ctx) => { ... });
 */
export function withContext<T, Args extends any[]>(
  fn: (ctx: ExecutionContext, ...args: Args) => T
): (...args: Args) => T {
  return (...args: Args) => {
    const ctx = getCurrentContext();
    return fn(ctx, ...args);
  };
}

export default ExecutionContext;
