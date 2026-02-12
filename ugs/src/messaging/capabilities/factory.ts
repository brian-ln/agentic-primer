#!/usr/bin/env bun
/**
 * ActorFactory - Creates actors with scoped capabilities
 *
 * Provides a factory pattern for constructing actors with
 * explicit capability configuration.
 *
 * @example
 * ```typescript
 * const factory = new ActorFactory(router);
 *
 * const taskActor = factory.createActor(TaskActor, {
 *   id: 'task-manager',
 *   namespace: '/workflows',
 *   capabilities: {
 *     namespace: '/workflows',
 *     storage: {
 *       allowedTables: ['tasks', 'workflows'],
 *       operations: ['read', 'write']
 *     },
 *     fs: {
 *       allowedPaths: ['/workflows/tasks'],
 *       operations: ['read', 'write']
 *     }
 *   }
 * });
 * ```
 */

import type { MessageRouter } from '../router.ts';
import type { Actor } from '../actor.ts';
import { StorageCapability, type StorageCapabilityConfig } from './storage.ts';
import { FileSystemCapability, type FileSystemCapabilityConfig } from './filesystem.ts';

/**
 * ActorFactory - Creates actors with capabilities
 */
export class ActorFactory {
  private router: MessageRouter;

  constructor(router: MessageRouter) {
    this.router = router;
  }

  /**
   * Create actor with capabilities
   *
   * The ActorClass constructor should accept:
   * - id: string
   * - router: MessageRouter
   * - ...additionalArgs: any[]
   *
   * Capabilities are attached after construction.
   */
  createActor<T extends Actor>(
    ActorClass: new (id: string, router: MessageRouter, ...args: any[]) => T,
    config: ActorFactoryConfig
  ): T {
    // Create actor instance
    const actor = new ActorClass(
      config.id,
      this.router,
      ...(config.constructorArgs || [])
    );

    // Attach capabilities if specified
    if (config.capabilities) {
      this.attachCapabilities(actor, config.capabilities);
    }

    return actor;
  }

  /**
   * Attach capabilities to an actor
   *
   * Note: This uses duck typing - the actor instance must have
   * 'storage' and 'fs' properties that can be assigned.
   */
  private attachCapabilities(actor: any, capabilities: CapabilityConfig): void {
    if (capabilities.storage) {
      actor.storage = new StorageCapability(this.router, {
        namespace: capabilities.namespace,
        allowedTables: capabilities.storage.allowedTables,
        operations: capabilities.storage.operations,
      });
    }

    if (capabilities.fs) {
      actor.fs = new FileSystemCapability(this.router, {
        namespace: capabilities.namespace,
        allowedPaths: capabilities.fs.allowedPaths,
        operations: capabilities.fs.operations,
      });
    }
  }
}

/**
 * ActorFactory configuration
 */
export interface ActorFactoryConfig {
  /** Actor ID (address) */
  id: string;

  /** Actor namespace (e.g., '/workflows') */
  namespace: string;

  /** Additional constructor arguments */
  constructorArgs?: any[];

  /** Capability configuration */
  capabilities?: CapabilityConfig;
}

/**
 * Capability configuration
 */
export interface CapabilityConfig {
  /** Actor namespace (for capability routing) */
  namespace: string;

  /** Storage capability config (optional) */
  storage?: {
    allowedTables: string[];
    operations: ('read' | 'write' | 'delete' | 'admin' | 'subscribe')[];
  };

  /** Filesystem capability config (optional) */
  fs?: {
    allowedPaths: string[];
    operations: ('read' | 'write' | 'delete' | 'watch')[];
  };
}

/**
 * Pre-configured capability profiles
 */
export const CapabilityProfiles = {
  /**
   * Read-only storage and filesystem
   */
  readOnly: (namespace: string, tables: string[], paths: string[]): CapabilityConfig => ({
    namespace,
    storage: {
      allowedTables: tables,
      operations: ['read'],
    },
    fs: {
      allowedPaths: paths,
      operations: ['read'],
    },
  }),

  /**
   * Full storage and filesystem access (scoped to tables/paths)
   */
  fullAccess: (namespace: string, tables: string[], paths: string[]): CapabilityConfig => ({
    namespace,
    storage: {
      allowedTables: tables,
      operations: ['read', 'write', 'delete', 'subscribe'],
    },
    fs: {
      allowedPaths: paths,
      operations: ['read', 'write', 'delete', 'watch'],
    },
  }),

  /**
   * Storage only (no filesystem access)
   */
  storageOnly: (
    namespace: string,
    tables: string[],
    operations: ('read' | 'write' | 'delete' | 'admin' | 'subscribe')[]
  ): CapabilityConfig => ({
    namespace,
    storage: {
      allowedTables: tables,
      operations,
    },
  }),

  /**
   * Filesystem only (no storage access)
   */
  filesystemOnly: (
    namespace: string,
    paths: string[],
    operations: ('read' | 'write' | 'delete' | 'watch')[]
  ): CapabilityConfig => ({
    namespace,
    fs: {
      allowedPaths: paths,
      operations,
    },
  }),

  /**
   * No capabilities (pure computation actor)
   */
  noCapabilities: (namespace: string): CapabilityConfig => ({
    namespace,
  }),
};
