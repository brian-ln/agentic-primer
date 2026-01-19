#!/usr/bin/env bun

/**
 * Daemon CLI - Manage primer daemon
 *
 * Commands:
 *   primer daemon start    Start daemon
 *   primer daemon stop     Stop daemon
 *   primer daemon restart  Restart daemon
 *   primer daemon status   Check daemon status
 *   primer daemon open     Open browser to workbench
 */

import {
  startDaemon,
  stopDaemon,
  restartDaemon,
  getStatus,
} from "../../daemon/process.ts";
import { loadNetworkInfo } from "../../daemon/config.ts";
import { spawn } from "node:child_process";

async function cmdStart() {
  console.log("Starting daemon...");

  const result = startDaemon();

  if (result.success) {
    console.log(`Daemon started with PID ${result.pid}`);

    // Wait briefly for server to start
    await Bun.sleep(500);

    // Show network info
    const networkInfo = loadNetworkInfo();
    if (networkInfo) {
      if (networkInfo.type === "socket") {
        console.log(`Listening on unix socket: ${networkInfo.value}`);
      } else {
        console.log(`Listening on http://127.0.0.1:${networkInfo.value}`);
      }
    }

    console.log("\nRun 'primer daemon open' to open the browser workbench");
  } else {
    console.error(`Failed to start daemon: ${result.error}`);
    process.exit(1);
  }
}

async function cmdStop() {
  console.log("Stopping daemon...");

  const result = stopDaemon();

  if (result.success) {
    console.log("Daemon stopped");
  } else {
    console.error(`Failed to stop daemon: ${result.error}`);
    process.exit(1);
  }
}

async function cmdRestart() {
  console.log("Restarting daemon...");

  const result = restartDaemon();

  if (result.success) {
    console.log(`Daemon restarted with PID ${result.pid}`);

    // Wait briefly for server to start
    await Bun.sleep(500);

    // Show network info
    const networkInfo = loadNetworkInfo();
    if (networkInfo) {
      if (networkInfo.type === "socket") {
        console.log(`Listening on unix socket: ${networkInfo.value}`);
      } else {
        console.log(`Listening on http://127.0.0.1:${networkInfo.value}`);
      }
    }
  } else {
    console.error(`Failed to restart daemon: ${result.error}`);
    process.exit(1);
  }
}

async function cmdStatus() {
  const status = getStatus();

  if (status.running) {
    console.log(`Daemon is running`);
    console.log(`PID: ${status.pid}`);

    if (status.network) {
      if (status.network.type === "socket") {
        console.log(`Socket: ${status.network.value}`);
      } else {
        console.log(`Port: ${status.network.value}`);
        console.log(`URL: http://127.0.0.1:${status.network.value}`);
      }
    }
  } else {
    console.log("Daemon is not running");
  }
}

async function cmdOpen() {
  const status = getStatus();

  if (!status.running) {
    console.error("Daemon is not running. Start it with 'primer daemon start'");
    process.exit(1);
  }

  // Get network info
  const networkInfo = loadNetworkInfo();

  if (!networkInfo) {
    console.error("Could not determine daemon network info");
    process.exit(1);
  }

  if (networkInfo.type === "socket") {
    console.error("Daemon is using unix socket. Browser workbench requires port.");
    console.error("Restart daemon with port configuration.");
    process.exit(1);
  }

  // Open browser
  const url = `http://127.0.0.1:${networkInfo.value}`;
  console.log(`Opening browser to ${url}`);

  // Platform-specific open command
  const platform = process.platform;
  let openCmd: string;

  if (platform === "darwin") {
    openCmd = "open";
  } else if (platform === "linux") {
    openCmd = "xdg-open";
  } else if (platform === "win32") {
    openCmd = "start";
  } else {
    console.log(`Please open manually: ${url}`);
    return;
  }

  try {
    spawn(openCmd, [url], { detached: true, stdio: "ignore" }).unref();
  } catch (error) {
    console.error(`Failed to open browser: ${error}`);
    console.log(`Please open manually: ${url}`);
  }
}

// Main CLI
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  switch (command) {
    case "start":
      await cmdStart();
      break;

    case "stop":
      await cmdStop();
      break;

    case "restart":
      await cmdRestart();
      break;

    case "status":
      await cmdStatus();
      break;

    case "open":
      await cmdOpen();
      break;

    default:
      console.log("Daemon CLI - Manage primer daemon\n");
      console.log("Commands:");
      console.log("  primer daemon start    Start daemon");
      console.log("  primer daemon stop     Stop daemon");
      console.log("  primer daemon restart  Restart daemon");
      console.log("  primer daemon status   Check daemon status");
      console.log("  primer daemon open     Open browser to workbench");
      process.exit(command ? 1 : 0);
  }
}

main().catch((err) => {
  console.error("Error:", err.message);
  process.exit(1);
});
