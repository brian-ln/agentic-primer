// Process management for primer daemon

import { existsSync, readFileSync, writeFileSync, unlinkSync, openSync } from "node:fs";
import { spawn } from "node:child_process";
import { loadConfig } from "./config.ts";

/**
 * Get PID from PID file
 */
export function getPid(): number | null {
  const config = loadConfig();
  const pidFile = config.paths.pidFile;

  if (!existsSync(pidFile)) {
    return null;
  }

  try {
    const content = readFileSync(pidFile, "utf-8");
    const pid = parseInt(content.trim());
    return isNaN(pid) ? null : pid;
  } catch (error) {
    console.error(`Failed to read PID file:`, error);
    return null;
  }
}

/**
 * Write PID to PID file
 */
export function writePid(pid: number): void {
  const config = loadConfig();
  const pidFile = config.paths.pidFile;

  try {
    writeFileSync(pidFile, pid.toString(), "utf-8");
  } catch (error) {
    console.error(`Failed to write PID file:`, error);
    throw error;
  }
}

/**
 * Remove PID file
 */
export function removePid(): void {
  const config = loadConfig();
  const pidFile = config.paths.pidFile;

  if (existsSync(pidFile)) {
    try {
      unlinkSync(pidFile);
    } catch (error) {
      console.error(`Failed to remove PID file:`, error);
    }
  }
}

/**
 * Check if process with given PID is running
 */
export function isProcessRunning(pid: number): boolean {
  try {
    // Send signal 0 to check if process exists (doesn't actually send a signal)
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return false;
  }
}

/**
 * Check if daemon is running
 */
export function isDaemonRunning(): { running: boolean; pid: number | null } {
  const pid = getPid();

  if (pid === null) {
    return { running: false, pid: null };
  }

  const running = isProcessRunning(pid);

  // Clean up stale PID file
  if (!running) {
    removePid();
    return { running: false, pid: null };
  }

  return { running: true, pid };
}

/**
 * Start daemon as background process
 */
export function startDaemon(): { success: boolean; pid?: number; error?: string } {
  // Check if already running
  const status = isDaemonRunning();
  if (status.running) {
    return {
      success: false,
      error: `Daemon already running with PID ${status.pid}`,
    };
  }

  try {
    // Get config for log file path
    const config = loadConfig();
    const logFile = config.paths.logFile;

    // Open log file for append
    const logFd = openSync(logFile, "a");

    // Spawn daemon process in detached mode
    const daemonPath = new URL("./server.ts", import.meta.url).pathname;
    const child = spawn("bun", [daemonPath], {
      detached: true,
      stdio: ["ignore", logFd, logFd], // Redirect stdout and stderr to log file
      env: {
        ...process.env,
        DAEMON_MODE: "true", // Signal to server.ts that it's running as daemon
      },
    });

    // Unref so parent can exit
    child.unref();

    // Write PID file
    if (child.pid) {
      writePid(child.pid);
      return { success: true, pid: child.pid };
    } else {
      return { success: false, error: "Failed to get daemon PID" };
    }
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Stop daemon
 */
export function stopDaemon(): { success: boolean; error?: string } {
  const status = isDaemonRunning();

  if (!status.running || status.pid === null) {
    return { success: false, error: "Daemon not running" };
  }

  try {
    // Send SIGTERM for graceful shutdown
    process.kill(status.pid, "SIGTERM");

    // Wait briefly for process to exit
    let attempts = 0;
    const maxAttempts = 10;

    while (attempts < maxAttempts) {
      if (!isProcessRunning(status.pid)) {
        removePid();
        return { success: true };
      }

      // Sleep 100ms between checks
      Bun.sleepSync(100);
      attempts++;
    }

    // If still running, force kill
    if (isProcessRunning(status.pid)) {
      process.kill(status.pid, "SIGKILL");
      removePid();
      return { success: true };
    }

    removePid();
    return { success: true };
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Restart daemon
 */
export function restartDaemon(): { success: boolean; pid?: number; error?: string } {
  // Stop first
  const stopResult = stopDaemon();

  // Even if stop fails (not running), try to start
  return startDaemon();
}

/**
 * Get daemon status
 */
export function getStatus(): {
  running: boolean;
  pid: number | null;
  uptime?: number;
  network?: { type: string; value: string | number };
} {
  const status = isDaemonRunning();

  if (!status.running) {
    return { running: false, pid: null };
  }

  // Try to get network info
  const { loadNetworkInfo } = require("./config.ts");
  const networkInfo = loadNetworkInfo();

  return {
    running: true,
    pid: status.pid,
    network: networkInfo
      ? { type: networkInfo.type, value: networkInfo.value }
      : undefined,
  };
}
