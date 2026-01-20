// CozoDB REPL Client
//
// Uses the CozoDB REPL mode via stdin/stdout for communication.
// Simpler than HTTP API and works directly with the binary.

import { spawn, type ChildProcess } from "node:child_process";
import { EventEmitter } from "node:events";

export interface CozoResult {
  ok: boolean;
  rows: unknown[][];
  headers: string[];
}

/**
 * CozoDB REPL Client
 *
 * Communicates with CozoDB via REPL mode using stdin/stdout.
 */
export class CozoReplClient extends EventEmitter {
  private process: ChildProcess | null = null;
  private buffer: string = "";
  private pendingQuery: { resolve: (value: CozoResult) => void; reject: (error: Error) => void } | null = null;

  constructor(
    private binaryPath: string,
    private engine: "mem" | "sqlite" = "mem",
    private dbPath?: string
  ) {
    super();
  }

  /**
   * Start the REPL process
   */
  async start(): Promise<void> {
    if (this.process) {
      throw new Error("REPL already started");
    }

    const args = ["repl"];
    if (this.engine === "sqlite" && this.dbPath) {
      args.push("--engine", "sqlite", "--path", this.dbPath);
    }

    this.process = spawn(this.binaryPath, args, {
      stdio: ["pipe", "pipe", "pipe"],
    });

    this.process.stdout?.on("data", (data: Buffer) => {
      this.buffer += data.toString();
      this.processBuffer();
    });

    this.process.stderr?.on("data", (data: Buffer) => {
      console.error("CozoDB REPL stderr:", data.toString());
    });

    this.process.on("error", (error) => {
      this.emit("error", error);
      if (this.pendingQuery) {
        this.pendingQuery.reject(error);
        this.pendingQuery = null;
      }
    });

    this.process.on("exit", (code) => {
      this.emit("exit", code);
      if (this.pendingQuery) {
        this.pendingQuery.reject(new Error(`REPL exited with code ${code}`));
        this.pendingQuery = null;
      }
    });

    // Wait for welcome message
    await new Promise((resolve) => setTimeout(resolve, 100));
  }

  /**
   * Process output buffer and parse results
   */
  private processBuffer(): void {
    // Look for table output (headers + rows)
    // CozoDB REPL outputs tables in ASCII format
    if (!this.pendingQuery) return;

    // Simple heuristic: if buffer contains a line with "----", it's probably a table
    if (this.buffer.includes("----") || this.buffer.includes("Error:")) {
      const result = this.parseOutput(this.buffer);
      this.buffer = "";
      this.pendingQuery.resolve(result);
      this.pendingQuery = null;
    }
  }

  /**
   * Parse REPL output into CozoResult
   */
  private parseOutput(output: string): CozoResult {
    const lines = output.split("\n").filter((line) => line.trim() !== "");

    // Check for errors
    if (output.includes("Error:")) {
      return {
        ok: false,
        rows: [],
        headers: [],
      };
    }

    // Find header line (contains "|")
    const headerIndex = lines.findIndex((line) => line.includes("|") && !line.includes("---"));
    if (headerIndex === -1) {
      return {
        ok: true,
        rows: [],
        headers: [],
      };
    }

    const headerLine = lines[headerIndex];
    const headers = headerLine
      .split("|")
      .map((h) => h.trim())
      .filter((h) => h !== "");

    // Find data rows (after separator "----")
    const separatorIndex = lines.findIndex((line) => line.includes("---"));
    if (separatorIndex === -1) {
      return {
        ok: true,
        rows: [],
        headers,
      };
    }

    const dataLines = lines.slice(separatorIndex + 1);
    const rows = dataLines
      .filter((line) => line.includes("|"))
      .map((line) => {
        return line
          .split("|")
          .map((cell) => cell.trim())
          .filter((cell) => cell !== "")
          .map((cell) => {
            // Try to parse as number
            const num = parseFloat(cell);
            if (!isNaN(num) && cell === num.toString()) {
              return num;
            }
            return cell;
          });
      });

    return {
      ok: true,
      rows,
      headers,
    };
  }

  /**
   * Execute a query
   */
  async run(script: string): Promise<CozoResult> {
    if (!this.process || !this.process.stdin) {
      throw new Error("REPL not started");
    }

    if (this.pendingQuery) {
      throw new Error("Another query is pending");
    }

    return new Promise((resolve, reject) => {
      this.pendingQuery = { resolve, reject };
      this.buffer = "";

      // Send query
      this.process!.stdin!.write(script + "\n");

      // Set timeout for query
      setTimeout(() => {
        if (this.pendingQuery) {
          this.pendingQuery.reject(new Error("Query timeout"));
          this.pendingQuery = null;
        }
      }, 5000);
    });
  }

  /**
   * Close the REPL
   */
  close(): void {
    if (this.process) {
      this.process.stdin?.end();
      this.process.kill();
      this.process = null;
    }
  }

  /**
   * Check if REPL is running
   */
  isRunning(): boolean {
    return this.process !== null && !this.process.killed;
  }
}
