/**
 * Provisioner types — declares what resources a system needs
 * and how they get materialized in a target runtime.
 *
 * The IProvisioner interface is the Layer 5 projection surface:
 * abstract resource declarations get bound to concrete providers.
 */

/** Resource types that can be provisioned */
export type ResourceType = 'd1' | 'kv' | 'r2' | 'do' | 'queue' | 'ai' | 'vectorize';

/** Durable Object configuration */
export interface DOResourceConfig {
  className: string;
  useSQLite?: boolean;
}

/** Queue configuration */
export interface QueueResourceConfig {
  queueName: string;
}

/** Type-specific config union */
export type ResourceConfig = DOResourceConfig | QueueResourceConfig | Record<string, unknown>;

/** A single resource declaration in a manifest */
export interface ResourceDeclaration {
  type: ResourceType;
  /** Binding name exposed to the worker (e.g., 'DB', 'KV', 'MY_DO') */
  binding: string;
  /** Type-specific configuration */
  config?: ResourceConfig;
}

/** Worker module definition (for provisioners that need inline code) */
export interface WorkerModule {
  type: 'ESModule' | 'CommonJS' | 'Text' | 'Data' | 'CompiledWasm';
  path: string;
  contents?: string | Uint8Array;
}

/** Worker code definition */
export interface WorkerDefinition {
  modules?: WorkerModule[];
  scriptPath?: string;
  compatibilityDate?: string;
  compatibilityFlags?: string[];
}

/**
 * System manifest — declares what a system needs to run.
 * Capabilities in code (compile-time), concrete fulfillment at projection (deploy-time).
 */
export interface SystemManifest {
  /** System name (used as worker name in miniflare) */
  name: string;
  /** Namespace for resource isolation (scopes binding IDs) */
  namespace?: string;
  /** Worker code definition */
  worker: WorkerDefinition;
  /** Resources this system needs */
  resources: ResourceDeclaration[];
}

/** Information about a provisioned resource */
export interface ProvisionedResource {
  type: ResourceType;
  binding: string;
  /** Runtime identifier (miniflare-assigned ID, database name, etc.) */
  id: string;
  /** Namespace this resource belongs to */
  namespace?: string;
}

/** Persistence configuration for local provisioners */
export interface PersistenceConfig {
  /** Root directory for all persistence. False disables persistence. */
  persistRoot?: string | false;
  /** Per-service overrides */
  d1Persist?: string | boolean;
  kvPersist?: string | boolean;
  r2Persist?: string | boolean;
  durableObjectsPersist?: string | boolean;
}

/**
 * IProvisioner — abstract interface for materializing system manifests
 * into running infrastructure.
 *
 * Implementations: MiniflareProvisioner (local), CloudflareProvisioner (API), etc.
 */
export interface IProvisioner {
  /** Provision resources declared in a manifest. Idempotent. */
  provision(manifest: SystemManifest): Promise<ProvisionedResource[]>;

  /** Remove all resources for a namespace. */
  deprovision(namespace: string): Promise<void>;

  /** List provisioned resources, optionally filtered by namespace. */
  resources(namespace?: string): ProvisionedResource[];

  /** Get a typed binding by name. */
  getBinding<T = unknown>(binding: string, workerName?: string): Promise<T>;

  /** Clean up all provisioned resources and connections. */
  dispose(): Promise<void>;
}
