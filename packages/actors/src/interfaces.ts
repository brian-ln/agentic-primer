/**
 * Core interfaces for the actor platform.
 * These decouple the actor core from runtime-specific implementations.
 */

import type { Message, MessageResponse, MessageHandler } from './message.ts';

/** Actor registry - decouples MessageRouter from GraphStore */
export interface IActorRegistry {
  lookup(address: string): MessageHandler | undefined;
  register(address: string, actor: MessageHandler): void;
  unregister(address: string): boolean;
  list(): string[];
}

/** Simple Map-based actor registry */
export class MapActorRegistry implements IActorRegistry {
  private readonly actors = new Map<string, MessageHandler>();

  lookup(address: string): MessageHandler | undefined {
    return this.actors.get(address);
  }

  register(address: string, actor: MessageHandler): void {
    this.actors.set(address, actor);
  }

  unregister(address: string): boolean {
    return this.actors.delete(address);
  }

  list(): string[] {
    return [...this.actors.keys()];
  }
}

/** Actor checkpoint interface - decouples actor state snapshots/WAL from storage */
export interface IActorCheckpoint {
  initialize(): Promise<void>;
  loadSnapshot(key: string): Promise<Uint8Array | null>;
  saveSnapshot(key: string, data: Uint8Array): Promise<void>;
  appendWAL(key: string, entry: Uint8Array): Promise<void>;
  replayWAL(key: string): Promise<Uint8Array[]>;
}

/** Transport interface for cross-runtime communication */
export interface ITransport {
  connect(remoteAddress: string): Promise<void>;
  disconnect(): Promise<void>;
  send(recipient: string, message: unknown): Promise<void>;
  onReceive(handler: (sender: string, message: unknown) => void): void;
  readonly state: ConnectionState;
}

export type ConnectionState = 'disconnected' | 'connecting' | 'connected' | 'disconnecting';

/** Serde interface for message serialization */
export interface ISerde {
  serialize(value: unknown): Uint8Array;
  deserialize(data: Uint8Array): unknown;
  readonly contentType: string;
}

/**
 * Message router interface - decouples Actor from concrete MessageRouter.
 * Actors communicate through this interface.
 */
export interface IMessageRouter {
  tell(message: Message): Promise<void>;
  ask<T = unknown>(message: Message, timeout?: number): Promise<MessageResponse<T>>;
  registerActor(id: string, actor: MessageHandler): void;
  unregisterActor(id: string): void;
  getActor(path: string): MessageHandler | undefined;
  listActors(): string[];
  cacheActor(path: string, actor: MessageHandler): void;
  invalidatePath(path: string): void;
}

// ============================================================================
// Storage interfaces — aligned with WIT domain.wit sql-storage / key-value-storage
// ============================================================================

/**
 * SQL value types — matches WIT sql-value variant.
 *
 * WIT: null-val | integer(s64) | real(f64) | text(string) | blob(list<u8>)
 * In TypeScript, number covers both integer and real.
 */
export type SqlValue = null | number | string | Uint8Array;

/** A single result row as positional values. */
export type SqlRow = SqlValue[];

/**
 * SQL query result — matches WIT sql-result record.
 *
 * Note: The auto-generated SqlResult in @agentic-primer/protocols uses
 * kebab-case property names ('rows-affected') and omits `columns`.
 * This version is the canonical TypeScript interface for runtime use.
 */
export interface SqlResult {
  /** Column names in result order. */
  columns: string[];
  /** Rows as positional value arrays (column-aligned). */
  rows: SqlRow[];
  /** Number of rows affected by a mutation (0 for SELECT). */
  rowsAffected: number;
  /** ROWID of last inserted row, if applicable. */
  lastInsertRowid?: number;
}

/**
 * SQL error categories — matches WIT sql-error variant.
 */
export type SqlError =
  | { type: 'query-failed'; message: string }
  | { type: 'constraint-violation'; message: string }
  | { type: 'connection-error'; message: string };

/**
 * Portable SQL storage interface — matches WIT sql-storage.
 *
 * Implementations: D1Storage (Cloudflare), bun:sqlite, libsql, etc.
 */
export interface ISqlStorage {
  execute(sql: string, params?: SqlValue[]): Promise<SqlResult>;
  batch(
    statements: Array<{ sql: string; params?: SqlValue[] }>
  ): Promise<SqlResult[]>;
}

/**
 * Portable key-value storage interface.
 *
 * Aligned with WIT key-value-storage but uses TypeScript-idiomatic
 * types (generics, typed options) rather than raw byte arrays.
 *
 * Implementations: KVStorage (Cloudflare), Map-based (test), etc.
 */
export interface IKeyValueStorage {
  get<T = string>(
    key: string,
    options?: { type?: 'text' | 'json' | 'arrayBuffer' }
  ): Promise<T | null>;
  put(
    key: string,
    value: string | ArrayBuffer | ReadableStream,
    options?: {
      expirationTtl?: number;
      metadata?: Record<string, unknown>;
    }
  ): Promise<void>;
  delete(key: string): Promise<void>;
  list(options?: {
    prefix?: string;
    limit?: number;
    cursor?: string;
  }): Promise<{
    keys: Array<{ name: string; metadata?: unknown }>;
    cursor?: string;
  }>;
}

/**
 * Portable blob/object storage interface.
 *
 * Abstracts S3-compatible object stores (Cloudflare R2, S3, GCS, etc.).
 *
 * Implementations: R2Storage (Cloudflare), FS-based (test), etc.
 */
export interface IBlobStorage {
  get(
    key: string
  ): Promise<{
    body: ReadableStream;
    metadata?: Record<string, string>;
  } | null>;
  put(
    key: string,
    body: ReadableStream | ArrayBuffer | string,
    options?: {
      httpMetadata?: Record<string, string>;
      customMetadata?: Record<string, string>;
    }
  ): Promise<void>;
  delete(key: string): Promise<void>;
  list(options?: {
    prefix?: string;
    limit?: number;
    cursor?: string;
  }): Promise<{
    objects: Array<{ key: string; size: number; uploaded: Date }>;
    cursor?: string;
  }>;
  head(
    key: string
  ): Promise<{
    size: number;
    uploaded: Date;
    httpMetadata?: Record<string, string>;
  } | null>;
}

/** Vector store interface for similarity search */
export interface IVectorStore {
  index(id: string, vector: number[], metadata?: Record<string, string>): Promise<void>;
  query(vector: number[], options?: { limit?: number; minSimilarity?: number; filter?: Record<string, string> }): Promise<Array<{ id: string; similarity: number; metadata?: Record<string, string> }>>;
  delete(id: string): Promise<void>;
}

/** Embedding model interface */
export interface IEmbedder {
  embed(text: string): Promise<number[]>;
  embedBatch(texts: string[]): Promise<number[][]>;
  readonly dimensions: number;
}
