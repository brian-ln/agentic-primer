/**
 * @agentic-primer/cloudflare - Cloudflare runtime adapter
 *
 * Adapts the portable actor core (@agentic-primer/actors) to run on
 * Cloudflare's runtime: Durable Objects, Workers, D1, KV, and R2.
 *
 * Provides:
 * - DOActorSystem: Abstract DO base class wrapping ActorSystem
 * - DOPersistence: IPersistence via DO SQLite
 * - Transports: Inter-DO (stub.fetch), Worker-to-Worker (service bindings), WebSocket bridge
 * - Storage adapters: D1 (SQL), KV (key-value), R2 (blob)
 * - Types: Address schemes, alarm scheduling, environment helpers
 */

// DO Actor System
export { DOActorSystem, type DOActorSystemConfig } from './do-actor-system.ts';

// Persistence
export { DOPersistence } from './do-persistence.ts';

// Transports
export { DOTransport, type DOTransportConfig } from './transports/do-transport.ts';
export { WorkerTransport, type WorkerTransportConfig } from './transports/worker-transport.ts';
export { WebSocketBridge } from './transports/websocket-bridge.ts';

// Storage adapters (implementations are local; interfaces live in @agentic-primer/actors)
export { D1Storage } from './storage/d1-storage.ts';
export { KVStorage } from './storage/kv-storage.ts';
export { R2Storage } from './storage/r2-storage.ts';
// Re-export storage interfaces from actors for backward-compatible imports
export type {
  ISqlStorage,
  SqlValue,
  SqlRow,
  SqlResult,
  SqlError,
  IKeyValueStorage,
  IBlobStorage,
} from '@agentic-primer/actors';

// Provisioner
export {
  MiniflareProvisioner,
  type MiniflareProvisionerConfig,
} from './provisioner/index.ts';
export type {
  IProvisioner,
  SystemManifest,
  WorkerDefinition,
  WorkerModule,
  ResourceDeclaration,
  ResourceType,
  ResourceConfig,
  DOResourceConfig,
  QueueResourceConfig,
  ProvisionedResource,
  PersistenceConfig,
} from './provisioner/index.ts';

// Types
export type {
  DOAddress,
  WorkerAddress,
  CloudflareEnv,
  AlarmSchedule,
  ParsedDOAddress,
  ParsedWorkerAddress,
} from './types.ts';
export { parseDOAddress, parseWorkerAddress } from './types.ts';
