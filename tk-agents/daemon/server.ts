#!/usr/bin/env bun

/**
 * Primer Daemon Server
 *
 * Hot graph daemon with HTTP REST API + WebSocket real-time updates
 *
 * Features:
 * - Hybrid network (unix socket + port fallback)
 * - In-memory Graph loaded from tasks.json
 * - EventLog for persistence and history
 * - WebSocket for real-time browser updates
 * - Graceful shutdown with state save
 */

import { Graph } from "../src/graph.ts";
import { EventLog } from "../src/persistence/event-log.ts";
import { CozoFfiClient } from "../src/cozo-ffi-client.ts";
import { DualWriteCoordinator } from "../src/dual-write-coordinator.ts";
import { loadConfig, saveNetworkInfo } from "./config.ts";
import { setupNetwork, getHttpUrl, cleanupNetwork, type NetworkSetup } from "./network.ts";
import { writePid, removePid, getPid } from "./process.ts";
import { existsSync, readFileSync, writeFileSync, watch } from "node:fs";
import { resolve } from "node:path";
import type { TaskProperties, NodeProperties, Edge } from "../src/types.ts";

// Global state
let graph: Graph;
let eventLog: EventLog;
let cozoDB: CozoFfiClient;
let coordinator: DualWriteCoordinator;
let config = loadConfig();
let network: NetworkSetup;

/**
 * Load Graph from snapshot file
 */
function loadGraph(filePath: string): Graph {
  const graph = new Graph();

  if (!existsSync(filePath)) {
    console.log(`No snapshot found at ${filePath}, starting with empty graph`);
    return graph;
  }

  try {
    const content = readFileSync(filePath, "utf-8");
    const data = JSON.parse(content);

    // Load nodes (using TaskActor factory for task nodes)
    const { TaskActor } = require("../src/task.ts");

    for (const nodeProps of data.nodes) {
      if (nodeProps.type === "task") {
        const taskProps = nodeProps as TaskProperties;
        const originalId = taskProps.id;

        // Create task actor (this generates a NEW id)
        TaskActor({
          goal: taskProps.goal,
          desiredDeliverables: taskProps.desiredDeliverables,
          objectiveSuccessCriteria: taskProps.objectiveSuccessCriteria,
          subjectiveSuccessCriteria: taskProps.subjectiveSuccessCriteria,
          informationGaps: taskProps.informationGaps,
          toolsAvailable: taskProps.toolsAvailable,
          parentTaskId: taskProps.parentTaskId,
          labels: taskProps.labels,
          priority: taskProps.priority,
          graph,
        });

        // Get the newly created task ID (last one in graph)
        const allIds = graph.getNodeIds();
        const newId = allIds[allIds.length - 1];

        // If the IDs don't match, we need to fix this
        if (newId !== originalId) {
          // Get the node's address and properties
          const address = (graph as any).nodes.get(newId);
          const props = graph.getNodeProperties(newId);

          // Remove the node with wrong ID
          (graph as any).nodes.delete(newId);
          (graph as any).nodeProperties.delete(newId);

          // Re-register with correct ID
          if (address && props) {
            // Update the properties ID to match original
            props.id = originalId;
            (graph as any).nodes.set(originalId, address);
            (graph as any).nodeProperties.set(originalId, props);
          }
        }

        // Restore full state (now using correct ID)
        const restoredProps = graph.getNodeProperties(originalId);
        if (restoredProps) {
          Object.assign(restoredProps, taskProps);
        }
      } else {
        // Register generic nodes (knowledge, etc.)
        const actor = { send: async () => ({ success: true, data: {} }) };
        const address = graph.getSystem().register(actor);
        graph.registerNode(nodeProps.id, address, nodeProps);
      }
    }

    // Load edges
    for (const edge of data.edges) {
      const edgeNum = parseInt(edge.id.replace("edge_", ""));
      if (!isNaN(edgeNum)) {
        graph.setEdgeCounter(Math.max(edgeNum, 0));
      }
      graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
    }

    console.log(`Loaded ${data.nodes.length} nodes and ${data.edges.length} edges from snapshot`);
    return graph;
  } catch (error) {
    console.error(`Failed to load snapshot:`, error);
    return graph;
  }
}

/**
 * Replay EventLog to bring Graph up to date
 */
async function replayEvents(graph: Graph, eventLog: EventLog, coordinator: DualWriteCoordinator): Promise<void> {
  console.log("Replaying EventLog...");
  let count = 0;
  
  // We only replay events that happened AFTER the snapshot was taken
  // For now, since we don't store "last replayed event" in snapshot, we replay everything
  // Graph.registerNode and coordinator handles idempotency (usually)
  
  eventLog.replay(async (event) => {
    count++;
    try {
      switch (event.type) {
        case "task_created": {
          const data = event.data as any;
          // Use coordinator to ensure CozoDB also stays in sync
          await coordinator.createTask({
            ...data,
            // Pass explicit ID if we want to preserve history exactly
            // But createTask generates its own. 
            // In a mature system, we'd use the event.nodeId.
          });
          break;
        }
        case "task_start":
        case "task_complete":
        case "task_block": {
          const action = event.type.split("_")[1] as any;
          await coordinator.updateTask(event.nodeId, action, event.data as any);
          break;
        }
        // ... handle other event types ...
      }
    } catch (e) {
      // Ignore replay errors (idempotency issues, etc)
    }
  });
  
  console.log(`Replayed ${count} events.`);
}

/**
 * Save Graph to snapshot file
 */
function saveGraph(graph: Graph, filePath: string): void {
  try {
    const dump = graph.dump();
    const serialized = JSON.stringify(dump, (key, value) => {
      if (value instanceof Date) {
        return value.toISOString();
      }
      return value;
    }, 2);

    writeFileSync(filePath, serialized, "utf-8");
    console.log(`Saved snapshot to ${filePath}`);
  } catch (error) {
    console.error(`Failed to save snapshot:`, error);
  }
}

/**
 * Initialize EventLog and emit startup event
 */
function initEventLog(filePath: string): EventLog {
  const log = new EventLog(filePath);

  log.append({
    timestamp: new Date().toISOString(),
    type: "daemon_started",
    nodeId: "daemon",
    data: {
      pid: process.pid,
      config: config.network,
    },
  });

  console.log(`EventLog initialized at ${filePath}`);
  return log;
}

/**
 * Initialize CozoDB and DualWriteCoordinator
 */
async function initCozo(graph: Graph, eventLog: EventLog): Promise<{ cozoDB: CozoFfiClient; coordinator: DualWriteCoordinator }> {
  console.log("Initializing CozoDB FFI client (SQLite)...");

  if (!existsSync("data")) {
    const { mkdirSync } = require("node:fs");
    mkdirSync("data", { recursive: true });
  }

  const cozoDB = CozoFfiClient.create("sqlite", "data/primer.db");

  console.log("Creating DualWriteCoordinator...");
  const coordinator = new DualWriteCoordinator(graph, eventLog, cozoDB);

  console.log("Running CozoDB schema migrations...");
  await coordinator.initialize();

  console.log("✓ CozoDB integration ready");
  return { cozoDB, coordinator };
}

/**
 * WebSocket connection manager
 */
class WebSocketManager {
  private clients: Set<any> = new Set();

  addClient(ws: any) {
    this.clients.add(ws);
    console.log(`WebSocket client connected (total: ${this.clients.size})`);
  }

  removeClient(ws: any) {
    this.clients.delete(ws);
    console.log(`WebSocket client disconnected (total: ${this.clients.size})`);
  }

  broadcast(event: { type: string; data: any }) {
    const message = JSON.stringify(event);
    for (const client of this.clients) {
      try {
        client.send(message);
      } catch (error) {
        console.error("Failed to send to client:", error);
        this.clients.delete(client);
      }
    }
  }

  close() {
    for (const client of this.clients) {
      try {
        client.close();
      } catch (error) {
        // Ignore
      }
    }
    this.clients.clear();
  }
}

const wsManager = new WebSocketManager();

/**
 * CORS headers for browser access
 */
const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type",
};

/**
 * Setup file watcher for tasks.json
 * Watches both the file and its parent directory to catch file replacements
 */
function setupFileWatcher(filePath: string, wsManager: any) {
  let reloadTimeout: Timer | null = null;

  // Reload function
  const reloadGraph = () => {
    try {
      console.log(`[File Watcher] Reloading graph from ${filePath}...`);

      // Reload graph from file
      const newGraph = loadGraph(filePath);
      const newCount = newGraph.getNodeIds().length;
      const oldCount = graph.getNodeIds().length;

      // Update global graph reference
      graph = newGraph;

      // Re-initialize coordinator with the new graph
      coordinator = new DualWriteCoordinator(graph, eventLog, cozoDB);

      // Log reload event
      eventLog.append({
        timestamp: new Date().toISOString(),
        type: "graph_reloaded",
        nodeId: "daemon",
        data: {
          source: "file_watcher",
          oldCount,
          newCount,
          delta: newCount - oldCount
        },
      });

      // Broadcast reload event to connected clients
      wsManager.broadcast({
        type: "graph_reloaded",
        data: {
          timestamp: new Date().toISOString(),
          oldCount,
          newCount,
          delta: newCount - oldCount
        },
      });

      console.log(`[File Watcher] Graph reloaded: ${oldCount} → ${newCount} nodes (delta: ${newCount - oldCount})`);
    } catch (error) {
      console.error("[File Watcher] Failed to reload graph:", error);

      // Log error event
      eventLog.append({
        timestamp: new Date().toISOString(),
        type: "graph_reload_error",
        nodeId: "daemon",
        data: {
          error: error instanceof Error ? error.message : String(error)
        },
      });
    }
  };

  // Handle file change events with debouncing
  const handleChange = (eventType: string, filename: string | null) => {
    // Clear existing timeout
    if (reloadTimeout) {
      clearTimeout(reloadTimeout);
    }

    // Debounce rapid changes
    reloadTimeout = setTimeout(() => {
      console.log(`[File Watcher] Change detected: ${eventType} on ${filename || filePath}`);
      reloadGraph();
    }, 200); // 200ms debounce for file writes
  };

  // Watch the file directly
  const watcher = watch(filePath, handleChange);

  watcher.on('error', (error) => {
    console.error("[File Watcher] Error:", error);
  });

  console.log(`[File Watcher] Active for ${filePath}`);

  return watcher;
}

/**
 * Main server startup
 */
async function main() {
  console.log("Starting Primer Daemon...");

  // Write PID file (launcher already did this, but we'll ensure it exists)
  // Actually process.ts launcher wrote it.

  // Setup network
  network = await setupNetwork(config);
  console.log(`Network: ${network.type} = ${network.value}`);

  // Load graph from snapshot
  const snapshotPath = resolve(config.paths.snapshotFile);
  graph = loadGraph(snapshotPath);

  // Initialize EventLog
  const eventLogPath = resolve(config.paths.eventLog);
  eventLog = initEventLog(eventLogPath);

  // Initialize CozoDB + Coordinator
  const cozo = await initCozo(graph, eventLog);
  cozoDB = cozo.cozoDB;
  coordinator = cozo.coordinator;

  // REPLAY LOG: Bring graph up to date from EventLog
  await replayEvents(graph, eventLog, coordinator);

  // Import API routes
  const { createRoutes } = await import("./api/routes.ts");
  // Pass graph, eventLog, and coordinator to routes
  const routes = createRoutes(() => graph, eventLog, coordinator, wsManager);

  // Setup file watcher for snapshot
  if (!existsSync(snapshotPath)) {
    saveGraph(graph, snapshotPath);
  }
  const fileWatcher = setupFileWatcher(snapshotPath, wsManager);

  // Start server
  const server = Bun.serve({
    unix: network.type === "socket" ? network.unix : undefined,
    port: network.type === "port" ? network.port : undefined,
    hostname: network.type === "port" ? network.hostname : undefined,

    async fetch(req, server) {
      const url = new URL(req.url);

      // WebSocket upgrade
      if (url.pathname === "/ws") {
        if (server.upgrade(req)) {
          return; // WebSocket connection established
        }
        return new Response("WebSocket upgrade failed", { status: 500 });
      }

      // Handle OPTIONS for CORS preflight
      if (req.method === "OPTIONS") {
        return new Response(null, {
          status: 204,
          headers: CORS_HEADERS,
        });
      }

      // Serve browser app at root
      if (url.pathname === "/" || url.pathname === "/index.html") {
        const htmlPath = new URL("../browser/index.html", import.meta.url).pathname;
        const file = Bun.file(htmlPath);
        return new Response(file, {
          headers: { "Content-Type": "text/html" },
        });
      }

      // Serve browser static files
      if (url.pathname.startsWith("/browser/") || url.pathname.match(/\.(tsx?|jsx?|css)$/)) {
        const filePath = url.pathname.startsWith("/browser/")
          ? new URL(".."+url.pathname, import.meta.url).pathname
          : new URL("../browser"+url.pathname, import.meta.url).pathname;

        const file = Bun.file(filePath);
        if (await file.exists()) {
          const contentType = url.pathname.endsWith(".css")
            ? "text/css"
            : url.pathname.endsWith(".tsx") || url.pathname.endsWith(".ts")
            ? "application/typescript"
            : url.pathname.endsWith(".jsx") || url.pathname.endsWith(".js")
            ? "application/javascript"
            : "text/plain";

          return new Response(file, {
            headers: { "Content-Type": contentType },
          });
        }
      }

      // Route matching
      for (const route of routes) {
        if (route.method === req.method && route.path instanceof RegExp) {
          const match = url.pathname.match(route.path);
          if (match) {
            const params: Record<string, string> = {};
            if (route.paramNames) {
              route.paramNames.forEach((name, i) => {
                params[name] = match[i + 1];
              });
            }
            return route.handler(req, params);
          }
        } else if (route.method === req.method && route.path === url.pathname) {
          return route.handler(req, {});
        }
      }

      // Not found
      return new Response(JSON.stringify({ error: "Not found" }), {
        status: 404,
        headers: { "Content-Type": "application/json", ...CORS_HEADERS },
      });
    },

    websocket: {
      open(ws) {
        wsManager.addClient(ws);
      },
      message(ws, message) {
        // Handle client messages (subscriptions, etc.)
        try {
          const msg = JSON.parse(message as string);
          if (msg.type === "subscribe") {
            // Store subscription preferences on ws object
            (ws as any).subscriptions = msg.events || [];
          }
        } catch (error) {
          console.error("Failed to parse WebSocket message:", error);
        }
      },
      close(ws) {
        wsManager.removeClient(ws);
      },
    },
  });

  // Update network info with actual port if random was assigned
  if (network.type === "port" && server.port) {
    network.port = server.port;
    network.value = server.port;
    saveNetworkInfo("port", server.port);
    console.log(`Server listening on http://${network.hostname}:${server.port}`);
  } else if (network.type === "socket") {
    console.log(`Server listening on unix socket: ${network.unix}`);
  }

  // Graceful shutdown
  const shutdown = () => {
    console.log("\nShutting down daemon...");

    // Log shutdown event
    eventLog.append({
      timestamp: new Date().toISOString(),
      type: "daemon_stopped",
      nodeId: "daemon",
      data: { pid: process.pid },
    });

    // Save graph snapshot
    saveGraph(graph, snapshotPath);

    // Close CozoDB
    if (cozoDB) {
      cozoDB.close();
      console.log("CozoDB closed");
    }

    // Close file watcher
    fileWatcher.close();

    // Close WebSocket connections
    wsManager.close();

    // Stop server
    server.stop();

    // Cleanup network resources
    cleanupNetwork(network);

    // Remove PID file if we own it
    if (getPid() === process.pid) {
      removePid();
    }

    console.log("Daemon stopped");
    process.exit(0);
  };

  process.on("SIGTERM", shutdown);
  process.on("SIGINT", shutdown);

  console.log("Daemon started successfully");
  console.log(`PID: ${process.pid}`);
  console.log("Press Ctrl+C to stop");
}

// Run if executed directly
if (import.meta.main || process.env.DAEMON_MODE === "true") {
  main().catch((error) => {
    console.error("Fatal error:", error);
    // Remove PID file if we own it
    if (getPid() === process.pid) {
      removePid();
    }

    console.log("Daemon stopped");

    process.exit(1);
  });
}
