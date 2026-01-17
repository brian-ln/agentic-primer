#!/usr/bin/env bun

/**
 * Graph CLI - Low-level graph manipulation
 *
 * Commands:
 *   graph init                                    Create empty graph.json
 *   graph create-node <id> --type TYPE [--data JSON]
 *                                                 Create a node
 *   graph delete-node <id> [--force]              Delete node and edges
 *   graph create-edge <from> <to> --type TYPE [--data JSON]
 *                                                 Create edge
 *   graph delete-edge <edgeId>                    Delete edge
 *   graph list-nodes [--type TYPE]                List nodes
 *   graph list-edges [--from ID] [--to ID] [--type TYPE]
 *                                                 List edges
 *   graph show <id>                               Show node details
 *   graph show-graph [--format ascii] [--root ID]
 *                                                 Visualize graph
 *   graph export [--output FILE]                  Export to JSON
 *   graph import <file>                           Import from JSON
 */

import { Graph } from "../graph.ts";
import type { Edge, NodeProperties, NodeType, EdgeType } from "../types.ts";
import { readFileSync, writeFileSync, existsSync } from "fs";
import { resolve } from "path";

const GRAPH_FILE = "graph.json";

// File format - matches Graph.dump() structure
interface GraphFile {
  nodes: NodeProperties[];
  edges: Edge[];
}

// Load graph from file (minimal loading - no actors created)
function loadGraph(filePath: string): { graph: Graph; data: GraphFile } {
  if (!existsSync(filePath)) {
    throw new Error(`Graph file not found: ${filePath}`);
  }

  const content = readFileSync(filePath, "utf-8");
  const data: GraphFile = JSON.parse(content);

  const graph = new Graph();

  // For graph CLI, we just store properties without creating actors
  // This is low-level manipulation
  for (const nodeProps of data.nodes) {
    // Create minimal "dummy" actor for registration
    const actor = {
      send: async () => ({ success: true, data: {} }),
    };
    const address = graph.getSystem().register(actor);
    graph.registerNode(nodeProps.id, address, nodeProps);
  }

  // Recreate edges
  for (const edge of data.edges) {
    const edgeNum = parseInt(edge.id.replace("edge_", ""));
    if (!isNaN(edgeNum)) {
      graph.setEdgeCounter(Math.max(edgeNum, 0));
    }
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
  }

  return { graph, data };
}

// Save graph to file
function saveGraph(graph: Graph, filePath: string): void {
  const dump = graph.dump();

  // Serialize dates as ISO strings
  const serialized = JSON.stringify(dump, (key, value) => {
    if (value instanceof Date) {
      return value.toISOString();
    }
    return value;
  }, 2);

  writeFileSync(filePath, serialized, "utf-8");
}

// Commands

function cmdInit() {
  const filePath = resolve(GRAPH_FILE);

  if (existsSync(filePath)) {
    console.error(`Error: ${GRAPH_FILE} already exists`);
    process.exit(1);
  }

  // Create empty graph
  const graph = new Graph();
  saveGraph(graph, filePath);

  console.log(`Created ${GRAPH_FILE}`);
}

function cmdCreateNode(id: string, options: { type?: string; data?: string }) {
  const filePath = resolve(GRAPH_FILE);
  if (!existsSync(filePath)) {
    console.error(`Error: ${GRAPH_FILE} not found. Run 'graph init' first.`);
    process.exit(1);
  }

  const { graph } = loadGraph(filePath);

  // Validate type
  if (!options.type) {
    console.error("Error: --type is required");
    process.exit(1);
  }

  const validTypes: NodeType[] = ["task", "knowledge", "artifact", "pattern"];
  if (!validTypes.includes(options.type as NodeType)) {
    console.error(`Error: Invalid type "${options.type}". Valid types: ${validTypes.join(", ")}`);
    process.exit(1);
  }

  // Check if node already exists
  if (graph.getNodeProperties(id)) {
    console.error(`Error: Node "${id}" already exists`);
    process.exit(1);
  }

  // Parse data if provided
  let additionalData: Record<string, unknown> = {};
  if (options.data) {
    try {
      additionalData = JSON.parse(options.data);
    } catch (error) {
      console.error(`Error: Invalid JSON in --data: ${error instanceof Error ? error.message : String(error)}`);
      process.exit(1);
    }
  }

  // Create minimal node properties
  const nodeProps: NodeProperties = {
    id,
    type: options.type as NodeType,
    createdAt: new Date(),
    ...additionalData,
  };

  // Create dummy actor and register
  const actor = {
    send: async () => ({ success: true, data: {} }),
  };
  const address = graph.getSystem().register(actor);
  graph.registerNode(id, address, nodeProps);

  saveGraph(graph, filePath);

  console.log(`Created node: ${id}`);
  console.log(`Type: ${options.type}`);
}

function cmdDeleteNode(id: string, force: boolean = false) {
  const filePath = resolve(GRAPH_FILE);
  const { graph } = loadGraph(filePath);

  // Check if node exists
  const props = graph.getNodeProperties(id);
  if (!props) {
    console.error(`Error: Node not found: ${id}`);
    process.exit(1);
  }

  // Get connected edges
  const edges = graph.getAllEdges(id);

  // Show what will be deleted (unless forced)
  if (!force) {
    console.log(`\nNode to delete: ${id}`);
    console.log("─".repeat(80));
    console.log(`Type: ${props.type}`);
    console.log(`Created: ${props.createdAt}`);

    if (edges.length > 0) {
      console.log(`\nConnected edges (${edges.length}):`);
      edges.forEach((e) => {
        console.log(`  ${e.id}: ${e.fromId} -> ${e.toId} (${e.type})`);
      });
    }

    // Confirmation prompt
    console.log();
    const answer = prompt("Delete this node? (yes/no): ");
    if (answer?.toLowerCase() !== "yes") {
      console.log("Deletion cancelled.");
      process.exit(0);
    }
  }

  // Delete the node
  const removed = graph.removeNode(id);

  if (removed) {
    saveGraph(graph, filePath);
    console.log(`\nDeleted node ${id}`);
    if (edges.length > 0) {
      console.log(`Removed ${edges.length} connected edge(s)`);
    }
  } else {
    console.error(`Error: Failed to delete node ${id}`);
    process.exit(1);
  }
}

function cmdCreateEdge(from: string, to: string, options: { type?: string; data?: string }) {
  const filePath = resolve(GRAPH_FILE);
  const { graph } = loadGraph(filePath);

  // Validate nodes exist
  if (!graph.getNodeProperties(from)) {
    console.error(`Error: Source node "${from}" not found`);
    process.exit(1);
  }

  if (!graph.getNodeProperties(to)) {
    console.error(`Error: Target node "${to}" not found`);
    process.exit(1);
  }

  // Validate type
  if (!options.type) {
    console.error("Error: --type is required");
    process.exit(1);
  }

  const validTypes: EdgeType[] = ["depends_on", "requires_knowledge", "produces", "spawned_by", "blocks", "references"];
  if (!validTypes.includes(options.type as EdgeType)) {
    console.error(`Error: Invalid edge type "${options.type}". Valid types: ${validTypes.join(", ")}`);
    process.exit(1);
  }

  // Parse data if provided
  let edgeProps: Record<string, unknown> = {};
  if (options.data) {
    try {
      edgeProps = JSON.parse(options.data);
    } catch (error) {
      console.error(`Error: Invalid JSON in --data: ${error instanceof Error ? error.message : String(error)}`);
      process.exit(1);
    }
  }

  // Create edge
  const edge = graph.addEdge(from, to, options.type as EdgeType, edgeProps);

  saveGraph(graph, filePath);

  console.log(`Created edge: ${edge.id}`);
  console.log(`From: ${from}`);
  console.log(`To: ${to}`);
  console.log(`Type: ${options.type}`);
}

function cmdDeleteEdge(edgeId: string) {
  const filePath = resolve(GRAPH_FILE);
  const { graph, data } = loadGraph(filePath);

  // Find the edge to show details
  const edge = data.edges.find((e) => e.id === edgeId);
  if (!edge) {
    console.error(`Error: Edge not found: ${edgeId}`);
    process.exit(1);
  }

  // Delete the edge
  const success = graph.removeEdge(edgeId);

  if (success) {
    saveGraph(graph, filePath);
    console.log(`Deleted edge: ${edgeId} (${edge.fromId} -> ${edge.toId}, ${edge.type})`);
  } else {
    console.error(`Error: Failed to delete edge ${edgeId}`);
    process.exit(1);
  }
}

function cmdListNodes(options: { type?: string }) {
  const filePath = resolve(GRAPH_FILE);
  const { graph } = loadGraph(filePath);

  let nodeIds = graph.getNodeIds();

  // Filter by type if specified
  if (options.type) {
    nodeIds = nodeIds.filter((id) => {
      const props = graph.getNodeProperties(id);
      return props?.type === options.type;
    });
  }

  console.log("\nNodes:");
  console.log("─".repeat(80));
  console.log("ID".padEnd(20) + "Type".padEnd(15) + "Created");
  console.log("─".repeat(80));

  if (nodeIds.length === 0) {
    console.log("No nodes found.");
  } else {
    for (const id of nodeIds) {
      const props = graph.getNodeProperties(id);
      if (props) {
        const created = props.createdAt instanceof Date
          ? props.createdAt.toISOString()
          : String(props.createdAt);
        console.log(`${id.padEnd(20)}${props.type.padEnd(15)}${created}`);
      }
    }
  }

  console.log("─".repeat(80));
  console.log(`Total: ${nodeIds.length} node(s)`);
  console.log();
}

function cmdListEdges(options: { from?: string; to?: string; type?: string }) {
  const filePath = resolve(GRAPH_FILE);
  const { data } = loadGraph(filePath);

  let edges = data.edges;

  // Apply filters
  if (options.from) {
    edges = edges.filter((e) => e.fromId === options.from);
  }
  if (options.to) {
    edges = edges.filter((e) => e.toId === options.to);
  }
  if (options.type) {
    edges = edges.filter((e) => e.type === options.type);
  }

  console.log("\nEdges:");
  console.log("─".repeat(80));
  console.log("ID".padEnd(12) + "From".padEnd(20) + "To".padEnd(20) + "Type");
  console.log("─".repeat(80));

  if (edges.length === 0) {
    console.log("No edges found.");
  } else {
    for (const edge of edges) {
      console.log(
        `${edge.id.padEnd(12)}${edge.fromId.padEnd(20)}${edge.toId.padEnd(20)}${edge.type}`
      );
    }
  }

  console.log("─".repeat(80));
  console.log(`Total: ${edges.length} edge(s)`);
  console.log();
}

function cmdShow(id: string) {
  const filePath = resolve(GRAPH_FILE);
  const { graph } = loadGraph(filePath);

  const props = graph.getNodeProperties(id);
  if (!props) {
    console.error(`Error: Node not found: ${id}`);
    process.exit(1);
  }

  // Get edges
  const outgoing = graph.getEdgesFrom(id);
  const incoming = graph.getEdgesTo(id);

  console.log(`\nNode: ${id}`);
  console.log("─".repeat(80));
  console.log(`ID:             ${props.id}`);
  console.log(`Type:           ${props.type}`);
  console.log(`Created:        ${props.createdAt}`);

  // Show additional properties
  const standardKeys = ["id", "type", "createdAt"];
  const additionalKeys = Object.keys(props).filter((k) => !standardKeys.includes(k));
  if (additionalKeys.length > 0) {
    console.log(`\nProperties:`);
    for (const key of additionalKeys) {
      const value = (props as Record<string, unknown>)[key];
      const valueStr = typeof value === "object" ? JSON.stringify(value) : String(value);
      const truncated = valueStr.length > 60 ? valueStr.slice(0, 57) + "..." : valueStr;
      console.log(`  ${key}: ${truncated}`);
    }
  }

  if (outgoing.length > 0) {
    console.log(`\nOutgoing Edges (${outgoing.length}):`);
    outgoing.forEach((e) => {
      console.log(`  ${e.id} -> ${e.toId} (${e.type})`);
    });
  }

  if (incoming.length > 0) {
    console.log(`\nIncoming Edges (${incoming.length}):`);
    incoming.forEach((e) => {
      console.log(`  ${e.id} <- ${e.fromId} (${e.type})`);
    });
  }

  console.log();
}

function cmdShowGraph(options: { format?: string; root?: string }) {
  const filePath = resolve(GRAPH_FILE);
  const { graph } = loadGraph(filePath);

  const format = options.format || "ascii";

  if (format === "ascii") {
    showGraphASCII(graph, options.root);
  } else if (format === "mermaid") {
    showGraphMermaid(graph, options.root);
  } else {
    console.error(`Error: Unknown format "${format}". Use ascii or mermaid.`);
    process.exit(1);
  }
}

function showGraphASCII(graph: Graph, rootId?: string) {
  console.log("\nGraph:");
  console.log("─".repeat(80));

  const nodeIds = rootId ? [rootId] : graph.getNodeIds();
  const visited = new Set<string>();

  function printNode(nodeId: string, indent: number = 0, prefix: string = "") {
    if (visited.has(nodeId)) {
      console.log(`${prefix}(cycle: ${nodeId})`);
      return;
    }
    visited.add(nodeId);

    const props = graph.getNodeProperties(nodeId);
    if (!props) {
      console.log(`${prefix}${nodeId} [not found]`);
      return;
    }

    console.log(`${prefix}${nodeId} [${props.type}]`);

    // Show outgoing edges
    const outgoing = graph.getEdgesFrom(nodeId);
    outgoing.forEach((edge, index) => {
      const isLast = index === outgoing.length - 1;
      const connector = isLast ? "└──" : "├──";
      const childPrefix = prefix + (isLast ? "    " : "│   ");

      console.log(`${prefix}${connector} ${edge.type}: ${edge.toId}`);
    });

    visited.delete(nodeId);
  }

  if (nodeIds.length === 0) {
    console.log("No nodes in graph.");
  } else {
    nodeIds.forEach((id) => {
      if (!visited.has(id)) {
        printNode(id);
        console.log();
      }
    });
  }
}

function showGraphMermaid(graph: Graph, rootId?: string) {
  console.log("\nMermaid Diagram:");
  console.log("─".repeat(80));
  console.log("graph TD");

  const nodeIds = rootId ? [rootId] : graph.getNodeIds();
  const processedEdges = new Set<string>();

  // Declare all nodes
  for (const id of nodeIds) {
    const props = graph.getNodeProperties(id);
    if (props) {
      const safeId = id.replace(/[^a-zA-Z0-9_]/g, "_");
      console.log(`  ${safeId}["${id}<br/>${props.type}"]`);
    }
  }

  // Declare all edges
  for (const id of nodeIds) {
    const edges = graph.getEdgesFrom(id);
    for (const edge of edges) {
      const edgeKey = `${edge.fromId}->${edge.toId}`;
      if (!processedEdges.has(edgeKey)) {
        processedEdges.add(edgeKey);
        const safeFrom = edge.fromId.replace(/[^a-zA-Z0-9_]/g, "_");
        const safeTo = edge.toId.replace(/[^a-zA-Z0-9_]/g, "_");
        console.log(`  ${safeFrom} -->|${edge.type}| ${safeTo}`);
      }
    }
  }

  console.log("─".repeat(80));
  console.log("Copy the above to mermaid.live or GitHub markdown");
  console.log();
}

function cmdExport(options: { output?: string }) {
  const filePath = resolve(GRAPH_FILE);
  const content = readFileSync(filePath, "utf-8");

  if (options.output) {
    const outputPath = resolve(options.output);
    writeFileSync(outputPath, content, "utf-8");

    const data: GraphFile = JSON.parse(content);
    console.log(`Exported graph to ${options.output}`);
    console.log(`Nodes: ${data.nodes.length}`);
    console.log(`Edges: ${data.edges.length}`);
  } else {
    // Output to stdout
    console.log(content);
  }
}

function cmdImport(file: string) {
  const filePath = resolve(GRAPH_FILE);
  const importPath = resolve(file);

  if (!existsSync(importPath)) {
    console.error(`Error: Import file not found: ${file}`);
    process.exit(1);
  }

  // Read and validate import file
  const content = readFileSync(importPath, "utf-8");
  let importData: GraphFile;
  try {
    importData = JSON.parse(content);
  } catch (error) {
    console.error(`Error: Invalid JSON in import file: ${error instanceof Error ? error.message : String(error)}`);
    process.exit(1);
  }

  // Validate structure
  if (!Array.isArray(importData.nodes) || !Array.isArray(importData.edges)) {
    console.error("Error: Import file must have 'nodes' and 'edges' arrays");
    process.exit(1);
  }

  // Load existing graph (or create new if doesn't exist)
  let graph: Graph;
  if (existsSync(filePath)) {
    const result = loadGraph(filePath);
    graph = result.graph;
  } else {
    graph = new Graph();
  }

  // Import nodes
  let nodesAdded = 0;
  for (const nodeProps of importData.nodes) {
    if (!graph.getNodeProperties(nodeProps.id)) {
      const actor = {
        send: async () => ({ success: true, data: {} }),
      };
      const address = graph.getSystem().register(actor);
      graph.registerNode(nodeProps.id, address, nodeProps);
      nodesAdded++;
    }
  }

  // Import edges (recalculate IDs to avoid conflicts)
  let edgesAdded = 0;
  for (const edge of importData.edges) {
    // Check if both nodes exist
    if (graph.getNodeProperties(edge.fromId) && graph.getNodeProperties(edge.toId)) {
      graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
      edgesAdded++;
    }
  }

  saveGraph(graph, filePath);

  console.log(`Imported graph from ${file}`);
  console.log(`Added ${nodesAdded} node(s)`);
  console.log(`Added ${edgesAdded} edge(s)`);
}

// Help message
function showHelp() {
  console.log("Graph CLI - Low-level graph manipulation\n");
  console.log("Commands:");
  console.log("  graph init                                    Create empty graph.json");
  console.log("  graph create-node <id> --type TYPE [--data JSON]");
  console.log("                                                Create a node");
  console.log("  graph delete-node <id> [--force]              Delete node and edges");
  console.log("  graph create-edge <from> <to> --type TYPE [--data JSON]");
  console.log("                                                Create edge");
  console.log("  graph delete-edge <edgeId>                    Delete edge");
  console.log("  graph list-nodes [--type TYPE]                List nodes");
  console.log("  graph list-edges [--from ID] [--to ID] [--type TYPE]");
  console.log("                                                List edges");
  console.log("  graph show <id>                               Show node details");
  console.log("  graph show-graph [--format ascii|mermaid] [--root ID]");
  console.log("                                                Visualize graph");
  console.log("  graph export [--output FILE]                  Export to JSON");
  console.log("  graph import <file>                           Import from JSON");
}

// Main CLI
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  switch (command) {
    case "init":
      cmdInit();
      break;

    case "create-node": {
      if (args.length < 2) {
        console.error("Usage: graph create-node <id> --type TYPE [--data JSON]");
        process.exit(1);
      }
      const id = args[1];
      const options: { type?: string; data?: string } = {};
      for (let i = 2; i < args.length; i++) {
        if (args[i] === "--type" && args[i + 1]) {
          options.type = args[i + 1];
          i++;
        } else if (args[i] === "--data" && args[i + 1]) {
          options.data = args[i + 1];
          i++;
        }
      }
      cmdCreateNode(id, options);
      break;
    }

    case "delete-node": {
      if (args.length < 2) {
        console.error("Usage: graph delete-node <id> [--force]");
        process.exit(1);
      }
      const id = args[1];
      const force = args.includes("--force");
      cmdDeleteNode(id, force);
      break;
    }

    case "create-edge": {
      if (args.length < 3) {
        console.error("Usage: graph create-edge <from> <to> --type TYPE [--data JSON]");
        process.exit(1);
      }
      const from = args[1];
      const to = args[2];
      const options: { type?: string; data?: string } = {};
      for (let i = 3; i < args.length; i++) {
        if (args[i] === "--type" && args[i + 1]) {
          options.type = args[i + 1];
          i++;
        } else if (args[i] === "--data" && args[i + 1]) {
          options.data = args[i + 1];
          i++;
        }
      }
      cmdCreateEdge(from, to, options);
      break;
    }

    case "delete-edge": {
      if (args.length < 2) {
        console.error("Usage: graph delete-edge <edgeId>");
        process.exit(1);
      }
      cmdDeleteEdge(args[1]);
      break;
    }

    case "list-nodes": {
      const options: { type?: string } = {};
      for (let i = 1; i < args.length; i++) {
        if (args[i] === "--type" && args[i + 1]) {
          options.type = args[i + 1];
          i++;
        }
      }
      cmdListNodes(options);
      break;
    }

    case "list-edges": {
      const options: { from?: string; to?: string; type?: string } = {};
      for (let i = 1; i < args.length; i++) {
        if (args[i] === "--from" && args[i + 1]) {
          options.from = args[i + 1];
          i++;
        } else if (args[i] === "--to" && args[i + 1]) {
          options.to = args[i + 1];
          i++;
        } else if (args[i] === "--type" && args[i + 1]) {
          options.type = args[i + 1];
          i++;
        }
      }
      cmdListEdges(options);
      break;
    }

    case "show": {
      if (args.length < 2) {
        console.error("Usage: graph show <id>");
        process.exit(1);
      }
      cmdShow(args[1]);
      break;
    }

    case "show-graph": {
      const options: { format?: string; root?: string } = {};
      for (let i = 1; i < args.length; i++) {
        if (args[i] === "--format" && args[i + 1]) {
          options.format = args[i + 1];
          i++;
        } else if (args[i] === "--root" && args[i + 1]) {
          options.root = args[i + 1];
          i++;
        }
      }
      cmdShowGraph(options);
      break;
    }

    case "export": {
      const options: { output?: string } = {};
      for (let i = 1; i < args.length; i++) {
        if (args[i] === "--output" && args[i + 1]) {
          options.output = args[i + 1];
          i++;
        }
      }
      cmdExport(options);
      break;
    }

    case "import": {
      if (args.length < 2) {
        console.error("Usage: graph import <file>");
        process.exit(1);
      }
      cmdImport(args[1]);
      break;
    }

    default:
      showHelp();
      process.exit(command ? 1 : 0);
  }
}

main().catch((err) => {
  console.error("Error:", err.message);
  process.exit(1);
});
