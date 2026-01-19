// Configuration management for primer daemon

import { existsSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname } from "node:path";

/**
 * Daemon configuration
 */
export interface DaemonConfig {
  /** Network connection type */
  network: {
    type: "socket" | "port";
    /** Unix domain socket path (if type=socket) */
    socketPath?: string;
    /** Port number (if type=port) */
    port?: number;
  };
  /** Notification settings */
  notifications: {
    mode: "auto-open" | "notify-only" | "silent";
    macOS: boolean;
    browser: boolean;
    minPriority: 0 | 1 | 2 | 3 | 4;
    events: string[];
  };
  /** Paths */
  paths: {
    snapshotFile: string;
    eventLog: string;
    pidFile: string;
    configFile: string;
    logFile: string;
  };
  /** Agent scheduling configuration */
  agentScheduling: {
    /** Enable/disable auto-launch after compaction */
    autoMode: boolean;
    /** Target concurrent agent capacity */
    targetCapacity: number;
    /** Minimum priority for auto-launched agents */
    minPriority: 0 | 1 | 2 | 3 | 4;
    /** Last compaction timestamp */
    lastCompaction?: string;
    /** Auto-mode change history (for debugging) */
    history?: Array<{
      timestamp: string;
      action: "enabled" | "disabled";
      triggeredBy: "user" | "system";
    }>;
  };
}

/**
 * Default configuration
 */
export const DEFAULT_CONFIG: DaemonConfig = {
  network: {
    type: "port",
    port: 0, // 0 = random port assigned by OS
  },
  notifications: {
    mode: "notify-only",
    macOS: true,
    browser: true,
    minPriority: 1,
    events: ["review_ready", "agent_complete", "task_blocked"],
  },
  paths: {
    snapshotFile: "graph-snapshot.json",
    eventLog: "events.jsonl",
    pidFile: join(homedir(), ".primer-daemon.pid"),
    configFile: join(homedir(), ".primer-config.json"),
    logFile: join(homedir(), ".primer-daemon.log"),
  },
  agentScheduling: {
    autoMode: true, // Default ENABLED (maintains current behavior)
    targetCapacity: 10,
    minPriority: 1, // P0-P1 tasks only
    history: [],
  },
};

/**
 * Load configuration from file or return defaults
 * Supports environment variable overrides for testing
 */
export function loadConfig(): DaemonConfig {
  const configPath = process.env.DAEMON_CONFIG_FILE || DEFAULT_CONFIG.paths.configFile;

  let config: DaemonConfig;

  if (!existsSync(configPath)) {
    // Create default config file (skip if test mode)
    if (!process.env.DAEMON_MODE) {
      saveConfig(DEFAULT_CONFIG);
    }
    config = DEFAULT_CONFIG;
  } else {
    try {
      const content = readFileSync(configPath, "utf-8");
      const userConfig = JSON.parse(content);

      // Merge with defaults (deep merge for nested objects)
      config = {
        ...DEFAULT_CONFIG,
        ...userConfig,
        network: { ...DEFAULT_CONFIG.network, ...userConfig.network },
        notifications: { ...DEFAULT_CONFIG.notifications, ...userConfig.notifications },
        paths: { ...DEFAULT_CONFIG.paths, ...userConfig.paths },
        agentScheduling: { ...DEFAULT_CONFIG.agentScheduling, ...userConfig.agentScheduling },
      };
    } catch (error) {
      console.error(`Failed to load config from ${configPath}:`, error);
      config = DEFAULT_CONFIG;
    }
  }

  // Apply environment variable overrides (for testing)
  if (process.env.DAEMON_SNAPSHOT_FILE) {
    config.paths.snapshotFile = process.env.DAEMON_SNAPSHOT_FILE;
  }
  if (process.env.DAEMON_EVENT_LOG) {
    config.paths.eventLog = process.env.DAEMON_EVENT_LOG;
  }
  if (process.env.DAEMON_PID_FILE) {
    config.paths.pidFile = process.env.DAEMON_PID_FILE;
  }
  if (process.env.DAEMON_NETWORK_TYPE) {
    config.network.type = process.env.DAEMON_NETWORK_TYPE as "socket" | "port";
  }
  if (process.env.DAEMON_PORT) {
    config.network.port = parseInt(process.env.DAEMON_PORT);
  }
  if (process.env.DAEMON_SOCKET_PATH) {
    config.network.socketPath = process.env.DAEMON_SOCKET_PATH;
  }

  return config;
}

/**
 * Save configuration to file
 */
export function saveConfig(config: DaemonConfig): void {
  const configPath = config.paths.configFile;

  // Ensure directory exists
  const dir = dirname(configPath);
  if (!existsSync(dir)) {
    mkdirSync(dir, { recursive: true });
  }

  try {
    const content = JSON.stringify(config, null, 2);
    writeFileSync(configPath, content, "utf-8");
  } catch (error) {
    console.error(`Failed to save config to ${configPath}:`, error);
  }
}

/**
 * Save network connection info (for CLI to discover daemon)
 */
export function saveNetworkInfo(type: "socket" | "port", value: string | number): void {
  const portFilePath = process.env.DAEMON_NETWORK_FILE || join(homedir(), ".primer-daemon-port");

  const info = {
    type,
    value,
    timestamp: new Date().toISOString(),
  };

  try {
    writeFileSync(portFilePath, JSON.stringify(info, null, 2), "utf-8");
  } catch (error) {
    console.error(`Failed to save network info to ${portFilePath}:`, error);
  }
}

/**
 * Load network connection info
 */
export function loadNetworkInfo(): { type: "socket" | "port"; value: string | number } | null {
  const portFilePath = join(homedir(), ".primer-daemon-port");

  if (!existsSync(portFilePath)) {
    return null;
  }

  try {
    const content = readFileSync(portFilePath, "utf-8");
    const info = JSON.parse(content);
    return { type: info.type, value: info.value };
  } catch (error) {
    console.error(`Failed to load network info from ${portFilePath}:`, error);
    return null;
  }
}
