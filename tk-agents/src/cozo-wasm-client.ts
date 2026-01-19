// CozoDB WASM Client
//
// Embedded CozoDB using WASM - works with Bun, no separate process needed.
// This is the recommended integration method for Bun.

import init, { CozoDb } from "cozo-lib-wasm";

export interface CozoResult {
  ok: boolean;
  rows: unknown[][];
  headers: string[];
  took?: number;
  next?: string | null;
}

export interface CozoError {
  ok: false;
  message: string;
  display: string;
}

/**
 * CozoDB WASM Client
 *
 * Embedded CozoDB using WebAssembly. Works with Bun runtime.
 * Requires calling `initialize()` before use.
 *
 * @example
 * ```typescript
 * const client = new CozoWasmClient();
 * await client.initialize();
 * const result = await client.run("?[] <- [[1, 2, 3]]");
 * console.log(result.rows); // [[1, 2, 3]]
 * ```
 */
export class CozoWasmClient {
  private db: CozoDb | null = null;
  private initialized: boolean = false;

  /**
   * Initialize the WASM module and create database instance.
   * Must be called before any queries.
   *
   * @throws Error if initialization fails
   */
  async initialize(): Promise<void> {
    if (this.initialized) {
      return;
    }

    try {
      // Initialize WASM module (loads .wasm file)
      await init();

      // Create in-memory database instance
      this.db = CozoDb.new();
      this.initialized = true;
    } catch (error) {
      throw new Error(
        `Failed to initialize CozoDB WASM: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  /**
   * Execute a CozoScript query
   *
   * @param script - CozoScript query string
   * @param params - Optional parameters as a Record<string, unknown>
   * @returns Query result or throws on error
   * @throws Error if not initialized or query fails
   */
  async run(script: string, params: Record<string, unknown> = {}): Promise<CozoResult> {
    if (!this.initialized || !this.db) {
      throw new Error("CozoWasmClient not initialized. Call initialize() first.");
    }

    try {
      // CozoDb.run() returns JSON string
      const resultJson = this.db.run(script, JSON.stringify(params));
      const result = JSON.parse(resultJson);

      if (!result.ok) {
        const error = result as CozoError;
        throw new Error(`CozoDB query failed: ${error.display || error.message}`);
      }

      return result as CozoResult;
    } catch (error) {
      // Re-throw our own errors
      if (error instanceof Error && error.message.startsWith("CozoDB")) {
        throw error;
      }

      // Wrap other errors
      throw new Error(
        `Query execution failed: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  /**
   * Execute a query synchronously (WASM is synchronous)
   *
   * @param script - CozoScript query string
   * @param params - Optional parameters
   * @returns Query result or throws on error
   */
  runSync(script: string, params: Record<string, unknown> = {}): CozoResult {
    if (!this.initialized || !this.db) {
      throw new Error("CozoWasmClient not initialized. Call initialize() first.");
    }

    const resultJson = this.db.run(script, JSON.stringify(params));
    const result = JSON.parse(resultJson);

    if (!result.ok) {
      const error = result as CozoError;
      throw new Error(`CozoDB query failed: ${error.display || error.message}`);
    }

    return result as CozoResult;
  }

  /**
   * Export all data as JSON
   *
   * @param relations - Optional list of relation names to export (exports all if omitted)
   * @returns JSON string containing exported data
   */
  exportRelations(relations?: string[]): string {
    if (!this.initialized || !this.db) {
      throw new Error("CozoWasmClient not initialized. Call initialize() first.");
    }

    return this.db.export_relations(relations ? JSON.stringify(relations) : "[]");
  }

  /**
   * Import data from JSON
   *
   * @param data - JSON string or object containing data to import
   */
  importRelations(data: string | object): void {
    if (!this.initialized || !this.db) {
      throw new Error("CozoWasmClient not initialized. Call initialize() first.");
    }

    const dataStr = typeof data === "string" ? data : JSON.stringify(data);
    this.db.import_relations(dataStr);
  }

  /**
   * Import data from a backup
   *
   * @param data - Backup data (JSON string or object)
   */
  importFromBackup(data: string | object): void {
    if (!this.initialized || !this.db) {
      throw new Error("CozoWasmClient not initialized. Call initialize() first.");
    }

    const dataStr = typeof data === "string" ? data : JSON.stringify(data);
    this.db.import_from_backup(dataStr);
  }

  /**
   * Close the database (cleanup WASM resources)
   */
  close(): void {
    if (this.db) {
      // CozoDb has a free() method for WASM cleanup
      (this.db as any).free?.();
      this.db = null;
      this.initialized = false;
    }
  }

  /**
   * Check if the client is initialized and ready
   */
  isReady(): boolean {
    return this.initialized && this.db !== null;
  }

  /**
   * Ping the database with a simple query
   *
   * @returns true if database responds correctly
   */
  async ping(): Promise<boolean> {
    if (!this.isReady()) {
      return false;
    }

    try {
      const result = await this.run("?[] <- [[1]]");
      return result.ok && result.rows.length === 1;
    } catch {
      return false;
    }
  }
}

/**
 * Create and initialize a CozoWasmClient in one call
 *
 * @example
 * ```typescript
 * const db = await createCozoClient();
 * const result = await db.run("?[] <- [[1, 2, 3]]");
 * ```
 */
export async function createCozoClient(): Promise<CozoWasmClient> {
  const client = new CozoWasmClient();
  await client.initialize();
  return client;
}
