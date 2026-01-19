/**
 * CozoDB FFI Client for Bun
 *
 * Uses Bun's FFI to interface with libcozo_c.dylib (CozoDB C bindings)
 *
 * Architecture:
 * - Rust (cozo-core) → C FFI (cozo-lib-c) → Bun FFI → TypeScript
 *
 * Comparison with WASM:
 * - FFI: Native performance, platform-specific (.dylib)
 * - WASM: Portable, slightly slower, cross-platform
 */

import { dlopen, CString, ptr } from "bun:ffi";
import path from "path";

// Path to libcozo_c.dylib
const LIBCOZO_PATH = path.resolve(process.cwd(), "bin/libcozo_c.dylib");

/**
 * CozoDB FFI Symbols
 *
 * Based on cozo_c.h:
 * - cozo_open_db(engine, path, options, db_id) -> error_string
 * - cozo_close_db(db_id) -> bool
 * - cozo_run_query(db_id, script, params, immutable) -> json_string
 * - cozo_free_str(string) -> void
 */
const lib = dlopen(LIBCOZO_PATH, {
  cozo_open_db: {
    args: ["cstring", "cstring", "cstring", "ptr"], // engine, path, options, db_id
    returns: "ptr", // Returns error string or null
  },
  cozo_close_db: {
    args: ["i32"], // db_id
    returns: "bool",
  },
  cozo_run_query: {
    args: ["i32", "cstring", "cstring", "bool"], // db_id, script, params, immutable
    returns: "ptr", // Returns JSON string
  },
  cozo_import_relations: {
    args: ["i32", "cstring"], // db_id, json_payload
    returns: "ptr",
  },
  cozo_export_relations: {
    args: ["i32", "cstring"], // db_id, json_payload
    returns: "ptr",
  },
  cozo_backup: {
    args: ["i32", "cstring"], // db_id, out_path
    returns: "ptr",
  },
  cozo_restore: {
    args: ["i32", "cstring"], // db_id, in_path
    returns: "ptr",
  },
  cozo_import_from_backup: {
    args: ["i32", "cstring"], // db_id, json_payload
    returns: "ptr",
  },
  cozo_free_str: {
    args: ["ptr"],
    returns: "void",
  },
});

/**
 * CozoDB Query Result
 */
export interface CozoResult {
  ok: boolean;
  rows?: any[][];
  headers?: string[];
  next?: string;
  took?: number;
  [key: string]: any;
}

/**
 * CozoDB FFI Client
 *
 * Wrapper around CozoDB C API using Bun FFI
 */
export class CozoFfiClient {
  private dbId: number;
  private closed = false;

  /**
   * Private constructor - use static create() method
   */
  private constructor(dbId: number) {
    this.dbId = dbId;
  }

  /**
   * Create a new CozoDB instance
   *
   * @param engine - Storage engine: "mem", "sqlite", "rocksdb"
   * @param path - Database path (empty string for in-memory)
   * @param options - Engine-specific options (JSON string, default "{}")
   */
  static create(
    engine: "mem" | "sqlite" | "rocksdb" = "mem",
    path: string = "",
    options: string = "{}"
  ): CozoFfiClient {
    // Allocate space for db_id (int32)
    const dbIdBuffer = new Int32Array(1);
    const dbIdPtr = ptr(dbIdBuffer);

    // Convert strings to null-terminated C strings
    const engineCStr = Buffer.from(engine + "\0");
    const pathCStr = Buffer.from(path + "\0");
    const optionsCStr = Buffer.from(options + "\0");

    // Call cozo_open_db
    const errorPtr = lib.symbols.cozo_open_db(engineCStr, pathCStr, optionsCStr, dbIdPtr);

    // Check for error
    if (errorPtr !== null && errorPtr !== 0) {
      const errorMsg = new CString(errorPtr);
      lib.symbols.cozo_free_str(errorPtr);
      throw new Error(`Failed to open CozoDB: ${errorMsg}`);
    }

    // Get db_id
    const dbId = dbIdBuffer[0];

    return new CozoFfiClient(dbId);
  }

  /**
   * Run a CozoScript query
   *
   * @param script - CozoScript query
   * @param params - Query parameters (object, will be JSON stringified)
   * @param immutable - Whether this is a read-only query
   */
  run(script: string, params: Record<string, any> = {}, immutable = false): CozoResult {
    if (this.closed) {
      throw new Error("Database is closed");
    }

    // Convert to C strings
    const scriptCStr = Buffer.from(script + "\0");
    const paramsCStr = Buffer.from(JSON.stringify(params) + "\0");

    // Call cozo_run_query
    const resultPtr = lib.symbols.cozo_run_query(this.dbId, scriptCStr, paramsCStr, immutable);

    if (resultPtr === null || resultPtr === 0) {
      throw new Error("cozo_run_query returned null");
    }

    // Convert result to string
    const resultStr = new CString(resultPtr);
    lib.symbols.cozo_free_str(resultPtr);

    // Parse JSON
    const result: CozoResult = JSON.parse(resultStr.toString());

    // Check for errors
    if (!result.ok) {
      throw new Error(`CozoScript error: ${JSON.stringify(result)}`);
    }

    return result;
  }

  /**
   * Run query and return rows directly
   */
  async query(script: string, params: Record<string, any> = {}): Promise<any[][]> {
    const result = this.run(script, params, true);
    return result.rows || [];
  }

  /**
   * Import relations from JSON
   */
  importRelations(jsonPayload: string | object): CozoResult {
    if (this.closed) {
      throw new Error("Database is closed");
    }

    const payload = typeof jsonPayload === "string" ? jsonPayload : JSON.stringify(jsonPayload);
    const payloadCStr = Buffer.from(payload + "\0");

    const resultPtr = lib.symbols.cozo_import_relations(this.dbId, payloadCStr);

    if (resultPtr === null || resultPtr === 0) {
      throw new Error("cozo_import_relations returned null");
    }

    const resultStr = new CString(resultPtr);
    lib.symbols.cozo_free_str(resultPtr);

    return JSON.parse(resultStr.toString());
  }

  /**
   * Export relations to JSON
   */
  exportRelations(jsonPayload: string | object): CozoResult {
    if (this.closed) {
      throw new Error("Database is closed");
    }

    const payload = typeof jsonPayload === "string" ? jsonPayload : JSON.stringify(jsonPayload);
    const payloadCStr = Buffer.from(payload + "\0");

    const resultPtr = lib.symbols.cozo_export_relations(this.dbId, payloadCStr);

    if (resultPtr === null || resultPtr === 0) {
      throw new Error("cozo_export_relations returned null");
    }

    const resultStr = new CString(resultPtr);
    lib.symbols.cozo_free_str(resultPtr);

    return JSON.parse(resultStr.toString());
  }

  /**
   * Backup database
   */
  backup(outPath: string): CozoResult {
    if (this.closed) {
      throw new Error("Database is closed");
    }

    const pathCStr = Buffer.from(outPath + "\0");
    const resultPtr = lib.symbols.cozo_backup(this.dbId, pathCStr);

    if (resultPtr === null || resultPtr === 0) {
      throw new Error("cozo_backup returned null");
    }

    const resultStr = new CString(resultPtr);
    lib.symbols.cozo_free_str(resultPtr);

    return JSON.parse(resultStr.toString());
  }

  /**
   * Restore database from backup
   */
  restore(inPath: string): CozoResult {
    if (this.closed) {
      throw new Error("Database is closed");
    }

    const pathCStr = Buffer.from(inPath + "\0");
    const resultPtr = lib.symbols.cozo_restore(this.dbId, pathCStr);

    if (resultPtr === null || resultPtr === 0) {
      throw new Error("cozo_restore returned null");
    }

    const resultStr = new CString(resultPtr);
    lib.symbols.cozo_free_str(resultPtr);

    return JSON.parse(resultStr.toString());
  }

  /**
   * Import from backup
   */
  importFromBackup(jsonPayload: string | object): CozoResult {
    if (this.closed) {
      throw new Error("Database is closed");
    }

    const payload = typeof jsonPayload === "string" ? jsonPayload : JSON.stringify(jsonPayload);
    const payloadCStr = Buffer.from(payload + "\0");

    const resultPtr = lib.symbols.cozo_import_from_backup(this.dbId, payloadCStr);

    if (resultPtr === null || resultPtr === 0) {
      throw new Error("cozo_import_from_backup returned null");
    }

    const resultStr = new CString(resultPtr);
    lib.symbols.cozo_free_str(resultPtr);

    return JSON.parse(resultStr.toString());
  }

  /**
   * Close the database
   */
  close(): void {
    if (this.closed) {
      return;
    }

    const success = lib.symbols.cozo_close_db(this.dbId);
    this.closed = true;

    if (!success) {
      console.warn(`Database ${this.dbId} was already closed or does not exist`);
    }
  }

  /**
   * Check if database is closed
   */
  isClosed(): boolean {
    return this.closed;
  }
}

/**
 * Helper function to create CozoDB client
 */
export function createCozoFfiClient(
  engine: "mem" | "sqlite" | "rocksdb" = "mem",
  path: string = "",
  options: string = "{}"
): CozoFfiClient {
  return CozoFfiClient.create(engine, path, options);
}
