/**
 * @agentic-primer/bun - Bun runtime adapter
 *
 * Provides Bun-specific actor implementations:
 * - FileSystemActor: Sandboxed filesystem access (node:fs)
 * - StorageActor: SQLite database access (bun:sqlite)
 * - SubprocessCodeComputeActor: Isolated code execution (Bun.spawn)
 * - FsActorCheckpoint: File-based WAL + snapshot checkpoint (IActorCheckpoint)
 */

export {
  FileSystemActor,
  type FileSystemActorConfig,
  type FileSystemOperation,
} from './filesystem-actor.ts';

export {
  StorageActor,
  type StorageActorConfig,
  type StorageOperation,
} from './storage-actor.ts';

export { FsActorCheckpoint } from './checkpoint/fs-actor-checkpoint.ts';

export { SubprocessCodeComputeActor } from './compute/subprocess-code.ts';

export { LibSqlStorage } from './libsql-storage.ts';

export { LibSqlVectorStore } from './libsql-vector-store.ts';
export type { LibSqlVectorStoreConfig } from './libsql-vector-store.ts';
