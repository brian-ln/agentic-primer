#!/usr/bin/env bun

/**
 * Primer CLI - Unified interface for tk-agents subsystems
 *
 * Usage:
 *   primer <subsystem> <command> [args...] [options]
 *   ap <subsystem> <command> [args...] [options]  (short alias)
 *
 * Subsystems:
 *   task        Manage tasks with dependencies and priorities
 *   graph       Low-level graph manipulation
 *   knowledge   Manage versioned knowledge with querying (alias: k)
 *   daemon      Manage primer daemon (browser workbench + API)
 *
 * Global Options:
 *   --json      Output in JSON format (where supported)
 *   --yes       Auto-confirm all prompts
 *
 * Examples:
 *   primer task add "Implement feature" --priority P0
 *   primer graph list-nodes --type task
 *   primer knowledge search "API design"
 *   primer k query knowledge_1 "What is REST?"
 *   ap task ready
 */

import { resolve } from "path";
import { fileURLToPath } from "url";
import { dirname } from "path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Subsystem configuration
const SUBSYSTEMS = {
  task: {
    name: "task",
    description: "Manage tasks with dependencies and priorities",
    file: resolve(__dirname, "task.ts"),
    commands: ["init", "add", "update", "delete", "list", "ready", "show", "status", "eval", "graph", "search"],
  },
  graph: {
    name: "graph",
    description: "Low-level graph manipulation",
    file: resolve(__dirname, "graph.ts"),
    commands: [
      "init",
      "create-node",
      "delete-node",
      "create-edge",
      "delete-edge",
      "list-nodes",
      "list-edges",
      "show",
      "show-graph",
      "export",
      "import",
    ],
  },
  knowledge: {
    name: "knowledge",
    description: "Manage versioned knowledge with querying",
    file: resolve(__dirname, "knowledge.ts"),
    alias: "k",
    commands: ["init", "add", "get", "list", "append", "update", "delete", "query", "search", "link", "unlink", "synthesize"],
  },
  daemon: {
    name: "daemon",
    description: "Manage primer daemon (browser workbench + API)",
    file: resolve(__dirname, "daemon.ts"),
    commands: ["start", "stop", "restart", "status", "open"],
  },
};

// Alias mapping
const ALIASES: Record<string, string> = {
  k: "knowledge",
};

// Version information
const VERSION = "1.0.0";

// Show general help
function showHelp() {
  console.log("Primer CLI - Unified interface for tk-agents\n");
  console.log("Usage: primer <subsystem> <command> [args...] [options]\n");
  console.log("Subsystems:");
  for (const [key, subsystem] of Object.entries(SUBSYSTEMS)) {
    const aliasText = subsystem.alias ? ` (alias: ${subsystem.alias})` : "";
    console.log(`  ${key.padEnd(12)}${subsystem.description}${aliasText}`);
  }
  console.log("\nGlobal Options:");
  console.log("  --json      Output in JSON format (where supported)");
  console.log("  --yes       Auto-confirm all prompts");
  console.log("\nExamples:");
  console.log('  primer task add "My task" --priority P0');
  console.log("  primer graph list-nodes --type task");
  console.log('  primer knowledge search "API design"');
  console.log('  primer k query knowledge_1 "What?"');
  console.log("\nUse 'primer help <subsystem>' for subsystem-specific commands.");
  console.log("Aliases: ap (short form)");
}

// Show subsystem-specific help
function showSubsystemHelp(subsystemName: string) {
  const subsystem = SUBSYSTEMS[subsystemName as keyof typeof SUBSYSTEMS];
  if (!subsystem) {
    console.error(`Error: Unknown subsystem "${subsystemName}"`);
    process.exit(1);
  }

  console.log(`Primer - ${capitalize(subsystem.name)} Subsystem\n`);
  console.log(`Usage: primer ${subsystem.name} <command> [args...] [options]\n`);
  console.log("Commands:");
  for (const cmd of subsystem.commands) {
    console.log(`  ${cmd}`);
  }
  console.log("\nGlobal Options:");
  console.log("  --json      Output in JSON format");
  console.log("  --yes       Auto-confirm all prompts");
  console.log(`\nSee full documentation: ${subsystem.name.toUpperCase()}_CLI.spec.md`);
}

// Show version information
function showVersion() {
  console.log(`Primer CLI v${VERSION}`);
  console.log(`├─ Task CLI v${VERSION}`);
  console.log(`├─ Graph CLI v${VERSION}`);
  console.log(`└─ Knowledge CLI v${VERSION}`);
  console.log(`\nSystem: tk-agents`);
  console.log(`Bun: ${Bun.version}`);
}

// Capitalize first letter
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Resolve subsystem alias
function resolveSubsystem(subsystem: string): string {
  return ALIASES[subsystem.toLowerCase()] || subsystem.toLowerCase();
}

// Execute subsystem CLI command
async function executeSubsystemCommand(subsystem: string, args: string[]): Promise<number> {
  const subsystemConfig = SUBSYSTEMS[subsystem as keyof typeof SUBSYSTEMS];
  if (!subsystemConfig) {
    console.error(`Error: Unknown subsystem "${subsystem}"`);
    console.error('Valid subsystems: task, graph, knowledge (k)\n');
    console.error("Use 'primer help' for usage information.");
    return 1;
  }

  // Check if command is specified
  if (args.length === 0) {
    console.error(`Error: No command specified for ${subsystem} subsystem\n`);
    console.error(`Usage: primer ${subsystem} <command> [args...]\n`);
    console.error(`Use 'primer help ${subsystem}' for available commands.`);
    return 1;
  }

  // Execute the subsystem CLI using Bun's spawn
  const proc = Bun.spawn(["bun", subsystemConfig.file, ...args], {
    stdin: "inherit",
    stdout: "inherit",
    stderr: "inherit",
    env: process.env,
  });

  // Wait for completion
  const exitCode = await proc.exited;
  return exitCode;
}

// Main CLI logic
async function main() {
  const args = process.argv.slice(2);

  // Handle no arguments
  if (args.length === 0) {
    console.error("Error: No subsystem specified\n");
    console.error("Usage: primer <subsystem> <command> [args...]\n");
    console.error("Use 'primer help' for more information.");
    process.exit(1);
  }

  const firstArg = args[0].toLowerCase();

  // Handle meta commands
  if (firstArg === "help") {
    if (args.length > 1) {
      const subsystem = resolveSubsystem(args[1]);
      showSubsystemHelp(subsystem);
    } else {
      showHelp();
    }
    process.exit(0);
  }

  if (firstArg === "version" || firstArg === "--version" || firstArg === "-v") {
    showVersion();
    process.exit(0);
  }

  // Handle subsystem routing
  const subsystem = resolveSubsystem(firstArg);
  const subsystemArgs = args.slice(1);

  // Execute subsystem command
  const exitCode = await executeSubsystemCommand(subsystem, subsystemArgs);
  process.exit(exitCode);
}

// Run CLI
main().catch((err) => {
  console.error("Error:", err.message);
  process.exit(1);
});
