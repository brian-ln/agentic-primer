// CozoDB HTTP Client
//
// Simple HTTP client for CozoDB standalone server.
// Uses fetch API (works with Bun out of the box).

export interface CozoResult {
  ok: boolean;
  rows: unknown[][];
  headers: string[];
  took: number;
  next?: string | null;
}

export interface CozoError {
  ok: false;
  message: string;
  display: string;
}

/**
 * CozoDB HTTP Client
 *
 * Communicates with a CozoDB standalone server via HTTP API.
 */
export class CozoClient {
  private baseUrl: string;

  constructor(baseUrl: string = "http://127.0.0.1:9070") {
    this.baseUrl = baseUrl;
  }

  /**
   * Execute a CozoScript query
   *
   * @param script - CozoScript query string
   * @param params - Optional parameters as a Record<string, unknown>
   * @returns Query result or throws on error
   */
  async run(script: string, params: Record<string, unknown> = {}): Promise<CozoResult> {
    const response = await fetch(`${this.baseUrl}/text-query`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        script,
        params,
      }),
    });

    if (!response.ok) {
      throw new Error(`CozoDB HTTP error: ${response.status} ${response.statusText}`);
    }

    const result = await response.json();

    if (!result.ok) {
      const error = result as CozoError;
      throw new Error(`CozoDB query failed: ${error.display}`);
    }

    return result as CozoResult;
  }

  /**
   * Close the client (no-op for HTTP client)
   */
  close(): void {
    // HTTP client doesn't need explicit cleanup
  }

  /**
   * Check if the server is reachable
   */
  async ping(): Promise<boolean> {
    try {
      const result = await this.run("?[] <- [[1]]");
      return result.ok && result.rows.length === 1;
    } catch {
      return false;
    }
  }
}
