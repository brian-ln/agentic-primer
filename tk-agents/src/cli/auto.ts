#!/usr/bin/env bun

/**
 * Auto-Mode CLI - Toggle automatic post-compaction agent scheduling
 *
 * Commands:
 *   auto on              Enable auto-mode
 *   auto off             Disable auto-mode
 *   auto status          Show current auto-mode status
 *   auto toggle          Flip auto-mode state
 *   auto config          Show full scheduling config
 */

import { loadConfig, saveConfig } from "../../daemon/config.ts";

const args = process.argv.slice(2);
const command = args[0] || "status";

async function main() {
  const config = loadConfig();

  switch (command) {
    case "on":
      config.agentScheduling.autoMode = true;
      config.agentScheduling.history = config.agentScheduling.history || [];
      config.agentScheduling.history.push({
        timestamp: new Date().toISOString(),
        action: "enabled",
        triggeredBy: "user",
      });
      saveConfig(config);

      console.log("Auto-mode ENABLED");
      console.log("Post-compaction: Will automatically launch agents to fill capacity");
      console.log(`Target: ${config.agentScheduling.targetCapacity} concurrent agents`);
      break;

    case "off":
      config.agentScheduling.autoMode = false;
      config.agentScheduling.history = config.agentScheduling.history || [];
      config.agentScheduling.history.push({
        timestamp: new Date().toISOString(),
        action: "disabled",
        triggeredBy: "user",
      });
      saveConfig(config);

      console.log("Auto-mode DISABLED");
      console.log("Post-compaction: Will NOT automatically launch agents");
      console.log("Use 'task list --status created' to see available work");
      break;

    case "toggle":
      const newState = !config.agentScheduling.autoMode;
      config.agentScheduling.autoMode = newState;
      config.agentScheduling.history = config.agentScheduling.history || [];
      config.agentScheduling.history.push({
        timestamp: new Date().toISOString(),
        action: newState ? "enabled" : "disabled",
        triggeredBy: "user",
      });
      saveConfig(config);

      console.log(newState ? "Auto-mode ENABLED" : "Auto-mode DISABLED");
      break;

    case "status":
    case "":
      const status = config.agentScheduling.autoMode ? "ENABLED" : "DISABLED";
      console.log(`Auto-mode: ${status}`);
      console.log(`Target capacity: ${config.agentScheduling.targetCapacity} concurrent agents`);
      console.log(`Min priority: P${config.agentScheduling.minPriority}`);

      if (config.agentScheduling.lastCompaction) {
        console.log(`Last compaction: ${config.agentScheduling.lastCompaction}`);
      }

      // Show recent history
      const history = config.agentScheduling.history || [];
      if (history.length > 0) {
        console.log("\nRecent changes:");
        history.slice(-3).forEach((entry) => {
          const action = entry.action === "enabled" ? "ON" : "OFF";
          console.log(`  ${entry.timestamp}: ${action} (${entry.triggeredBy})`);
        });
      }
      break;

    case "config":
      console.log("Agent Scheduling Configuration:");
      console.log(JSON.stringify(config.agentScheduling, null, 2));
      break;

    default:
      console.error("Unknown command. Usage:");
      console.error("  auto on|off|status|toggle|config");
      process.exit(1);
  }
}

main();
