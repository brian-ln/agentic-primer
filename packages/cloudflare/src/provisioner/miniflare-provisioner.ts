/**
 * MiniflareProvisioner — local development provisioner using Miniflare.
 *
 * Wraps the Miniflare programmatic API to provision Workers runtime
 * resources (D1, KV, R2, Durable Objects) for local development and testing.
 *
 * Key behaviors:
 * - setOptions() is a full replace, so we track merged config internally
 * - Each provision() call merges new resources into the running instance
 * - Persistence options control whether state survives restarts
 * - dispose() tears down the Miniflare instance
 */

import { Miniflare, type MiniflareOptions } from 'miniflare';
import type {
  IProvisioner,
  SystemManifest,
  ProvisionedResource,
  PersistenceConfig,
  DOResourceConfig,
  ResourceDeclaration,
} from './types.ts';

/** Configuration for the MiniflareProvisioner */
export interface MiniflareProvisionerConfig {
  /** Persistence options. Omit for in-memory only. */
  persistence?: PersistenceConfig;
  /** Default compatibility date for workers */
  compatibilityDate?: string;
  /** Default compatibility flags */
  compatibilityFlags?: string[];
  /** Additional Miniflare options to merge */
  miniflareOptions?: Partial<MiniflareOptions>;
}

/** Internal state for a provisioned worker */
interface WorkerState {
  name: string;
  namespace?: string;
  resources: ProvisionedResource[];
  modules: Array<{ type: string; path: string; contents?: string | Uint8Array }>;
  compatibilityDate: string;
  compatibilityFlags: string[];
  d1Databases: Record<string, string>;
  kvNamespaces: Record<string, string>;
  r2Buckets: Record<string, string>;
  durableObjects: Record<string, { className: string; useSQLite?: boolean }>;
}

/**
 * Generate a stable ID for a resource based on namespace and binding.
 * This ensures the same manifest produces the same IDs across restarts.
 */
function resourceId(namespace: string | undefined, binding: string, type: string): string {
  const prefix = namespace ? `${namespace}-` : '';
  return `${prefix}${type}-${binding}`.toLowerCase();
}

export class MiniflareProvisioner implements IProvisioner {
  private mf: Miniflare | null = null;
  private readonly config: MiniflareProvisionerConfig;
  private readonly workers = new Map<string, WorkerState>();

  constructor(config: MiniflareProvisionerConfig = {}) {
    this.config = {
      compatibilityDate: '2024-12-01',
      ...config,
    };
  }

  async provision(manifest: SystemManifest): Promise<ProvisionedResource[]> {
    const workerName = manifest.name;
    const ns = manifest.namespace;

    // Build resource bindings from declarations
    const d1Databases: Record<string, string> = {};
    const kvNamespaces: Record<string, string> = {};
    const r2Buckets: Record<string, string> = {};
    const durableObjects: Record<string, { className: string; useSQLite?: boolean }> = {};
    const provisioned: ProvisionedResource[] = [];

    for (const decl of manifest.resources) {
      const id = resourceId(ns, decl.binding, decl.type);

      switch (decl.type) {
        case 'd1': {
          d1Databases[decl.binding] = id;
          provisioned.push({ type: 'd1', binding: decl.binding, id, namespace: ns });
          break;
        }
        case 'kv': {
          kvNamespaces[decl.binding] = id;
          provisioned.push({ type: 'kv', binding: decl.binding, id, namespace: ns });
          break;
        }
        case 'r2': {
          r2Buckets[decl.binding] = id;
          provisioned.push({ type: 'r2', binding: decl.binding, id, namespace: ns });
          break;
        }
        case 'do': {
          const doConfig = decl.config as DOResourceConfig | undefined;
          if (!doConfig?.className) {
            throw new Error(`DO resource '${decl.binding}' requires config.className`);
          }
          durableObjects[decl.binding] = {
            className: doConfig.className,
            useSQLite: doConfig.useSQLite,
          };
          provisioned.push({ type: 'do', binding: decl.binding, id: doConfig.className, namespace: ns });
          break;
        }
        default:
          // queue, ai, vectorize — not yet supported in miniflare provisioner
          provisioned.push({ type: decl.type, binding: decl.binding, id, namespace: ns });
      }
    }

    // Store worker state
    const workerState: WorkerState = {
      name: workerName,
      namespace: ns,
      resources: provisioned,
      modules: (manifest.worker.modules ?? []).map((m) => ({
        type: m.type,
        path: m.path,
        contents: m.contents,
      })),
      compatibilityDate: manifest.worker.compatibilityDate ?? this.config.compatibilityDate ?? '2024-12-01',
      compatibilityFlags: manifest.worker.compatibilityFlags ?? this.config.compatibilityFlags ?? [],
      d1Databases,
      kvNamespaces,
      r2Buckets,
      durableObjects,
    };
    this.workers.set(workerName, workerState);

    // Build and apply Miniflare options
    const opts = this.buildMiniflareOptions();

    if (this.mf === null) {
      this.mf = new Miniflare(opts);
      await this.mf.ready;
    } else {
      await this.mf.setOptions(opts);
    }

    return provisioned;
  }

  async deprovision(namespace: string): Promise<void> {
    // Remove all workers with the given namespace
    for (const [name, state] of this.workers) {
      if (state.namespace === namespace) {
        this.workers.delete(name);
      }
    }

    if (this.workers.size === 0) {
      // No workers left — dispose miniflare
      await this.dispose();
    } else {
      // Rebuild with remaining workers
      const opts = this.buildMiniflareOptions();
      if (this.mf) {
        await this.mf.setOptions(opts);
      }
    }
  }

  resources(namespace?: string): ProvisionedResource[] {
    const all: ProvisionedResource[] = [];
    for (const state of this.workers.values()) {
      if (namespace === undefined || state.namespace === namespace) {
        all.push(...state.resources);
      }
    }
    return all;
  }

  async getBinding<T = unknown>(binding: string, workerName?: string): Promise<T> {
    if (!this.mf) {
      throw new Error('No Miniflare instance — call provision() first');
    }

    // Find which worker has this binding
    const targetWorker = workerName ?? this.findWorkerWithBinding(binding);
    if (!targetWorker) {
      throw new Error(`No worker found with binding '${binding}'`);
    }

    const state = this.workers.get(targetWorker);
    if (!state) {
      throw new Error(`Worker '${targetWorker}' not found`);
    }

    // Route to the correct miniflare getter
    if (binding in state.d1Databases) {
      return await this.mf.getD1Database(binding, targetWorker) as T;
    }
    if (binding in state.kvNamespaces) {
      return await this.mf.getKVNamespace(binding, targetWorker) as T;
    }
    if (binding in state.r2Buckets) {
      return await this.mf.getR2Bucket(binding, targetWorker) as T;
    }
    if (binding in state.durableObjects) {
      return await this.mf.getDurableObjectNamespace(binding, targetWorker) as T;
    }

    // Fall back to generic bindings
    const bindings = await this.mf.getBindings(targetWorker);
    const value = (bindings as Record<string, unknown>)[binding];
    if (value === undefined) {
      throw new Error(`Binding '${binding}' not found in worker '${targetWorker}'`);
    }
    return value as T;
  }

  /** Get the underlying Miniflare instance (for advanced use). */
  getMiniflare(): Miniflare | null {
    return this.mf;
  }

  /** Get a Fetcher for dispatching fetch requests to a worker. */
  async getWorker(workerName?: string) {
    if (!this.mf) {
      throw new Error('No Miniflare instance — call provision() first');
    }
    return this.mf.getWorker(workerName);
  }

  async dispose(): Promise<void> {
    if (this.mf) {
      await this.mf.dispose();
      this.mf = null;
    }
    this.workers.clear();
  }

  // --- Private ---

  private findWorkerWithBinding(binding: string): string | undefined {
    for (const [name, state] of this.workers) {
      if (
        binding in state.d1Databases ||
        binding in state.kvNamespaces ||
        binding in state.r2Buckets ||
        binding in state.durableObjects
      ) {
        return name;
      }
    }
    return undefined;
  }

  private buildMiniflareOptions(): MiniflareOptions {
    const persistence = this.config.persistence;

    // Shared options (persistence, etc.)
    const shared: Record<string, unknown> = {};
    if (persistence) {
      if (persistence.persistRoot !== undefined) {
        shared.defaultPersistRoot = persistence.persistRoot === false ? undefined : persistence.persistRoot;
      }
      if (persistence.d1Persist !== undefined) shared.d1Persist = persistence.d1Persist;
      if (persistence.kvPersist !== undefined) shared.kvPersist = persistence.kvPersist;
      if (persistence.r2Persist !== undefined) shared.r2Persist = persistence.r2Persist;
      if (persistence.durableObjectsPersist !== undefined) {
        shared.durableObjectsPersist = persistence.durableObjectsPersist;
      }
    }

    // Build worker options array
    const workerConfigs = [...this.workers.values()].map((state) =>
      this.buildWorkerOptions(state)
    );

    if (workerConfigs.length === 0) {
      throw new Error('Cannot build Miniflare options with no workers');
    }

    if (workerConfigs.length === 1) {
      // Single worker — flat config
      return {
        ...shared,
        ...workerConfigs[0],
        ...(this.config.miniflareOptions ?? {}),
      } as MiniflareOptions;
    }

    // Multi-worker
    return {
      ...shared,
      workers: workerConfigs,
      ...(this.config.miniflareOptions ?? {}),
    } as MiniflareOptions;
  }

  private buildWorkerOptions(state: WorkerState): Record<string, unknown> {
    const opts: Record<string, unknown> = {
      name: state.name,
      compatibilityDate: state.compatibilityDate,
      compatibilityFlags: state.compatibilityFlags,
    };

    // Worker code
    if (state.modules.length > 0) {
      opts.modules = state.modules;
    }

    // D1
    if (Object.keys(state.d1Databases).length > 0) {
      opts.d1Databases = state.d1Databases;
    }

    // KV
    if (Object.keys(state.kvNamespaces).length > 0) {
      opts.kvNamespaces = state.kvNamespaces;
    }

    // R2
    if (Object.keys(state.r2Buckets).length > 0) {
      opts.r2Buckets = state.r2Buckets;
    }

    // Durable Objects
    if (Object.keys(state.durableObjects).length > 0) {
      opts.durableObjects = Object.fromEntries(
        Object.entries(state.durableObjects).map(([binding, config]) => [
          binding,
          {
            className: config.className,
            ...(config.useSQLite ? { useSQLite: true } : {}),
          },
        ])
      );
    }

    return opts;
  }
}
