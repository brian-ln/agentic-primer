import { Transport } from "./interface.ts";
import { loadNetworkInfo } from "../../../daemon/config.ts";
import { startDaemon, isDaemonRunning } from "../../../daemon/process.ts";
import type { Message, Response } from "../base.ts";

export class HttpTransport implements Transport {
  private baseUrl: string = "";
  private daemonEnsured: boolean = false;

  private async getBaseUrl(): Promise<string> {
    if (this.baseUrl) return this.baseUrl;

    const info = loadNetworkInfo();
    if (!info) {
      // If we're ensuring daemon, we expect this to appear soon
      // For now fallback to default
      return "http://127.0.0.1:3000";
    }

    if (info.type === "socket") {
      throw new Error("Unix socket not supported by HttpTransport yet");
    }

    this.baseUrl = `http://127.0.0.1:${info.value}`;
    return this.baseUrl;
  }

  private async ensureDaemon(): Promise<void> {
    if (this.daemonEnsured) return;

    const status = isDaemonRunning();
    if (!status.running) {
      console.error("Primer daemon not running. Starting up...");
      const startResult = startDaemon();
      if (!startResult.success) {
        throw new Error(`Failed to auto-start daemon: ${startResult.error}`);
      }

      // Wait for health check to pass
      let ready = false;
      const timeout = 5000;
      const start = Date.now();

      while (Date.now() - start < timeout) {
        try {
          // Force reload network info as port might have changed or been assigned
          this.baseUrl = ""; 
          const baseUrl = await this.getBaseUrl();
          const response = await fetch(`${baseUrl}/api/health`);
          if (response.ok) {
            ready = true;
            break;
          }
        } catch (e) {
          // Daemon still starting
        }
        await new Promise(resolve => setTimeout(resolve, 200));
      }

      if (!ready) {
        throw new Error("Daemon auto-start timed out (5s). Check daemon logs.");
      }
      console.error("Daemon ready.");
    }

    this.daemonEnsured = true;
  }

  async send(target: string, message: Message): Promise<Response> {
    try {
      await this.ensureDaemon();
      
      const baseUrl = await this.getBaseUrl();
      const url = `${baseUrl}/api/actor/message`;

      const response = await fetch(url, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          target,
          message,
        }),
      });

      if (!response.ok) {
        const text = await response.text();
        return {
          success: false,
          error: `HTTP error ${response.status}: ${text}`,
        };
      }

      const result = await response.json();
      return result as Response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }
}