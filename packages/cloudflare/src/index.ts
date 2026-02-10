/**
 * @agentic-primer/cloudflare - Cloudflare runtime adapter
 *
 * Adapts the portable actor core (@agentic-primer/actors) to run on
 * Cloudflare's runtime: Durable Objects, Workers, D1, KV, and R2.
 *
 * Provides:
 * - DOActorSystem: Abstract DO base class wrapping ActorSystem
 * - DOActorCheckpoint: IActorCheckpoint via DO SQLite
 * - Transports: Inter-DO (stub.fetch), Worker-to-Worker (service bindings), WebSocket bridge
 * - Storage adapters: D1 (SQL), KV (key-value), R2 (blob), Vectorize (vector)
 * - AI adapters: WorkersAIEmbedder (embeddings)
 * - Types: Address schemes, alarm scheduling, environment helpers
 */

// DO Actor System
export { DOActorSystem, type DOActorSystemConfig } from './do-actor-system.ts';

// Actor Checkpoint
export { DOActorCheckpoint } from './do-actor-checkpoint.ts';

// Transports
export { DOTransport, type DOTransportConfig } from './transports/do-transport.ts';
export { WorkerTransport, type WorkerTransportConfig } from './transports/worker-transport.ts';
export { WebSocketBridge } from './transports/websocket-bridge.ts';

// Storage adapters (implementations are local; interfaces live in @agentic-primer/actors)
export { D1Storage } from './storage/d1-storage.ts';
export { KVStorage } from './storage/kv-storage.ts';
export { R2Storage } from './storage/r2-storage.ts';
export { VectorizeStore } from './storage/vectorize-store.ts';
// Re-export storage interfaces from actors for backward-compatible imports
export type {
  ISqlStorage,
  SqlValue,
  SqlRow,
  SqlResult,
  SqlError,
  IKeyValueStorage,
  IBlobStorage,
  IVectorStore,
  IEmbedder,
} from '@agentic-primer/actors';

// AI adapters
export { WorkersAIEmbedder, type WorkersAIEmbedderConfig } from './workers-ai-embedder.ts';

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
