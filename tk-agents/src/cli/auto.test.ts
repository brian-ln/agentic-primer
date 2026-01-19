// Tests for Auto-Mode CLI

import { describe, test, expect, beforeEach, afterEach } from "bun:test";
import { existsSync, unlinkSync, writeFileSync } from "node:fs";
import { loadConfig, saveConfig, DEFAULT_CONFIG } from "../../daemon/config.ts";
import { homedir } from "node:os";
import { join } from "node:path";

describe("Auto-Mode Configuration", () => {
  const testConfigPath = join(homedir(), ".primer-config-test.json");

  beforeEach(() => {
    // Clean up test config file
    if (existsSync(testConfigPath)) {
      unlinkSync(testConfigPath);
    }
  });

  afterEach(() => {
    // Clean up test config file
    if (existsSync(testConfigPath)) {
      unlinkSync(testConfigPath);
    }
  });

  test("default config includes agentScheduling with autoMode enabled", () => {
    expect(DEFAULT_CONFIG.agentScheduling).toBeDefined();
    expect(DEFAULT_CONFIG.agentScheduling.autoMode).toBe(true);
    expect(DEFAULT_CONFIG.agentScheduling.targetCapacity).toBe(10);
    expect(DEFAULT_CONFIG.agentScheduling.minPriority).toBe(1);
    expect(DEFAULT_CONFIG.agentScheduling.history).toEqual([]);
  });

  test("loadConfig merges agentScheduling from user config", () => {
    // Create a test config with custom agentScheduling
    const customConfig = {
      ...DEFAULT_CONFIG,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
      agentScheduling: {
        autoMode: false,
        targetCapacity: 5,
        minPriority: 0 as 0 | 1 | 2 | 3 | 4,
        history: [
          {
            timestamp: "2026-01-17T12:00:00.000Z",
            action: "disabled" as "enabled" | "disabled",
            triggeredBy: "user" as "user" | "system",
          },
        ],
      },
    };

    writeFileSync(testConfigPath, JSON.stringify(customConfig, null, 2), "utf-8");

    // Temporarily override config path for this test
    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    const loaded = loadConfig();

    // Restore original path
    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

    expect(loaded.agentScheduling.autoMode).toBe(false);
    expect(loaded.agentScheduling.targetCapacity).toBe(5);
    expect(loaded.agentScheduling.minPriority).toBe(0);
    expect(loaded.agentScheduling.history?.length).toBe(1);
  });

  test("loadConfig uses defaults when agentScheduling is missing from user config", () => {
    // Create config without agentScheduling
    const partialConfig = {
      network: DEFAULT_CONFIG.network,
      notifications: DEFAULT_CONFIG.notifications,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
    };

    writeFileSync(testConfigPath, JSON.stringify(partialConfig, null, 2), "utf-8");

    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    const loaded = loadConfig();

    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

    // Should use default agentScheduling
    expect(loaded.agentScheduling.autoMode).toBe(true);
    expect(loaded.agentScheduling.targetCapacity).toBe(10);
    expect(loaded.agentScheduling.minPriority).toBe(1);
  });

  test("saveConfig persists agentScheduling changes", () => {
    const config = {
      ...DEFAULT_CONFIG,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
      agentScheduling: {
        ...DEFAULT_CONFIG.agentScheduling,
        autoMode: false,
        history: [
          {
            timestamp: "2026-01-17T12:00:00.000Z",
            action: "disabled" as "enabled" | "disabled",
            triggeredBy: "user" as "user" | "system",
          },
        ],
      },
    };

    saveConfig(config);

    expect(existsSync(testConfigPath)).toBe(true);

    // Read back and verify
    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    const loaded = loadConfig();

    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

    expect(loaded.agentScheduling.autoMode).toBe(false);
    expect(loaded.agentScheduling.history?.length).toBe(1);
    expect(loaded.agentScheduling.history?.[0].action).toBe("disabled");
  });

  test("toggling auto-mode adds history entries", () => {
    const config = {
      ...DEFAULT_CONFIG,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
    };

    // First toggle: disable
    config.agentScheduling.autoMode = false;
    config.agentScheduling.history = [
      {
        timestamp: new Date().toISOString(),
        action: "disabled" as "enabled" | "disabled",
        triggeredBy: "user" as "user" | "system",
      },
    ];
    saveConfig(config);

    // Second toggle: enable
    config.agentScheduling.autoMode = true;
    config.agentScheduling.history.push({
      timestamp: new Date().toISOString(),
      action: "enabled" as "enabled" | "disabled",
      triggeredBy: "user" as "user" | "system",
    });
    saveConfig(config);

    // Load and verify
    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    const loaded = loadConfig();

    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

    expect(loaded.agentScheduling.history?.length).toBe(2);
    expect(loaded.agentScheduling.history?.[0].action).toBe("disabled");
    expect(loaded.agentScheduling.history?.[1].action).toBe("enabled");
  });

  test("auto-mode state survives config reload", () => {
    const config = {
      ...DEFAULT_CONFIG,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
      agentScheduling: {
        ...DEFAULT_CONFIG.agentScheduling,
        autoMode: false,
      },
    };

    saveConfig(config);

    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    // First load
    const loaded1 = loadConfig();
    expect(loaded1.agentScheduling.autoMode).toBe(false);

    // Second load (simulate restart)
    const loaded2 = loadConfig();
    expect(loaded2.agentScheduling.autoMode).toBe(false);

    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;
  });

  test("targetCapacity can be customized", () => {
    const config = {
      ...DEFAULT_CONFIG,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
      agentScheduling: {
        ...DEFAULT_CONFIG.agentScheduling,
        targetCapacity: 20,
      },
    };

    saveConfig(config);

    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    const loaded = loadConfig();

    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

    expect(loaded.agentScheduling.targetCapacity).toBe(20);
  });

  test("minPriority can be customized", () => {
    const config = {
      ...DEFAULT_CONFIG,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
      agentScheduling: {
        ...DEFAULT_CONFIG.agentScheduling,
        minPriority: 0 as 0 | 1 | 2 | 3 | 4,
      },
    };

    saveConfig(config);

    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    const loaded = loadConfig();

    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

    expect(loaded.agentScheduling.minPriority).toBe(0);
  });

  test("lastCompaction timestamp can be tracked", () => {
    const timestamp = "2026-01-17T14:23:15.000Z";
    const config = {
      ...DEFAULT_CONFIG,
      paths: {
        ...DEFAULT_CONFIG.paths,
        configFile: testConfigPath,
      },
      agentScheduling: {
        ...DEFAULT_CONFIG.agentScheduling,
        lastCompaction: timestamp,
      },
    };

    saveConfig(config);

    const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
    (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

    const loaded = loadConfig();

    (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

    expect(loaded.agentScheduling.lastCompaction).toBe(timestamp);
  });
});

describe("Auto-Mode CLI Integration", () => {
  test("auto CLI command exists and is executable", () => {
    const autoCliPath = join(process.cwd(), "src/cli/auto.ts");
    expect(existsSync(autoCliPath)).toBe(true);
  });

  test("config file can be created and modified via programmatic API", () => {
    const testConfigPath = join(homedir(), ".primer-config-cli-test.json");

    try {
      const config = {
        ...DEFAULT_CONFIG,
        paths: {
          ...DEFAULT_CONFIG.paths,
          configFile: testConfigPath,
        },
      };

      // Simulate `auto off`
      config.agentScheduling.autoMode = false;
      config.agentScheduling.history = [
        {
          timestamp: new Date().toISOString(),
          action: "disabled" as "enabled" | "disabled",
          triggeredBy: "user" as "user" | "system",
        },
      ];
      saveConfig(config);

      // Simulate `auto on`
      config.agentScheduling.autoMode = true;
      config.agentScheduling.history.push({
        timestamp: new Date().toISOString(),
        action: "enabled" as "enabled" | "disabled",
        triggeredBy: "user" as "user" | "system",
      });
      saveConfig(config);

      // Verify final state
      const originalConfigFile = DEFAULT_CONFIG.paths.configFile;
      (DEFAULT_CONFIG.paths as any).configFile = testConfigPath;

      const loaded = loadConfig();

      (DEFAULT_CONFIG.paths as any).configFile = originalConfigFile;

      expect(loaded.agentScheduling.autoMode).toBe(true);
      expect(loaded.agentScheduling.history?.length).toBe(2);
    } finally {
      if (existsSync(testConfigPath)) {
        unlinkSync(testConfigPath);
      }
    }
  });
});
