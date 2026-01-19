// Network setup - hybrid unix socket + port fallback

import { existsSync, unlinkSync } from "node:fs";
import type { DaemonConfig } from "./config.ts";
import { saveNetworkInfo } from "./config.ts";

export interface NetworkSetup {
  type: "socket" | "port";
  value: string | number;
  unix?: string;
  port?: number;
  hostname?: string;
}

/**
 * Setup network connection (try unix socket first, fallback to random port)
 */
export async function setupNetwork(config: DaemonConfig): Promise<NetworkSetup> {
  // Try unix socket first
  if (config.network.type === "socket" && config.network.socketPath) {
    try {
      const socketPath = config.network.socketPath;

      // Remove stale socket file if exists
      if (existsSync(socketPath)) {
        unlinkSync(socketPath);
      }

      // Save network info for CLI discovery
      saveNetworkInfo("socket", socketPath);

      return {
        type: "socket",
        value: socketPath,
        unix: socketPath,
      };
    } catch (error) {
      console.warn("Failed to setup unix socket, falling back to random port:", error);
    }
  }

  // Fallback to random port (OS-assigned)
  const port = config.network.port || 0; // 0 = random port
  const hostname = "127.0.0.1";

  // Save network info for CLI discovery
  saveNetworkInfo("port", port === 0 ? "random" : port);

  return {
    type: "port",
    value: port,
    port,
    hostname,
  };
}

/**
 * Get URL for HTTP access (if using port)
 */
export function getHttpUrl(network: NetworkSetup): string | null {
  if (network.type === "port" && network.port !== undefined && network.hostname) {
    return `http://${network.hostname}:${network.port}`;
  }
  return null;
}

/**
 * Cleanup network resources
 */
export function cleanupNetwork(network: NetworkSetup): void {
  if (network.type === "socket" && network.unix) {
    try {
      if (existsSync(network.unix)) {
        unlinkSync(network.unix);
      }
    } catch (error) {
      console.error("Failed to cleanup socket:", error);
    }
  }
}
