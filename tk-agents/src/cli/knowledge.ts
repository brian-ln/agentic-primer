#!/usr/bin/env bun

/**
 * Knowledge CLI - Manage knowledge nodes
 *
 * Commands:
 *   knowledge init                            Create knowledge.json
 *   knowledge add <title> [--content TEXT] [--sources S1,S2]
 *                                             Create knowledge node
 *   knowledge get <id>                        Show knowledge details
 *   knowledge list [--limit N]                List knowledge nodes
 *   knowledge append <id> <content> [--source SRC]
 *                                             Append to knowledge
 *   knowledge update <id> [--title TEXT] [--content TEXT]
 *                                             Update knowledge properties
 *   knowledge delete <id> [--force]           Delete knowledge node
 *   knowledge query <id> <question>           Query knowledge
 *   knowledge search <query>                  Search all knowledge
 *   knowledge link <kid> <nid> --type TYPE    Link to node
 *   knowledge unlink <kid> <nid>              Unlink from node
 *   knowledge synthesize <id> --from ID1,ID2  Synthesize from sources
 */

import { Graph } from "../graph.ts";
import { KnowledgeActor } from "../knowledge.ts";
import type { Edge, NodeProperties, KnowledgeProperties } from "../types.ts";
import { readFileSync, writeFileSync, existsSync } from "fs";
import { resolve } from "path";

const KNOWLEDGE_FILE = "knowledge.json";

// File format
interface KnowledgeFile {
  nodes: NodeProperties[];
  edges: Edge[];
}

// Load knowledge graph from file
async function loadGraph(filePath: string): Promise<Graph> {
  if (!existsSync(filePath)) {
    throw new Error(`Knowledge file not found: ${filePath}`);
  }

  const content = readFileSync(filePath, "utf-8");
  const data: KnowledgeFile = JSON.parse(content);

  const graph = new Graph();

  // Recreate knowledge nodes using KnowledgeActor factory
  for (const nodeProps of data.nodes) {
    if (nodeProps.type === "knowledge") {
      const kProps = nodeProps as KnowledgeProperties;

      // Create knowledge actor using factory
      KnowledgeActor({
        title: kProps.title,
        content: kProps.content,
        sources: kProps.sources,
        graph,
      });

      // Restore state by updating properties in graph
      const restoredProps = graph.getNodeProperties(kProps.id);
      if (restoredProps) {
        Object.assign(restoredProps, kProps);
      }
    }
  }

  // Recreate edges
  for (const edge of data.edges) {
    const edgeNum = parseInt(edge.id.replace("edge_", ""));
    if (!isNaN(edgeNum)) {
      graph.setEdgeCounter(Math.max(edgeNum, 0));
    }
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
  }

  return graph;
}

// Save graph to file
async function saveGraph(graph: Graph, filePath: string): Promise<void> {
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

async function cmdInit() {
  const filePath = resolve(KNOWLEDGE_FILE);

  if (existsSync(filePath)) {
    console.error(`Error: ${KNOWLEDGE_FILE} already exists`);
    process.exit(1);
  }

  const graph = new Graph();

  // Create example knowledge node
  KnowledgeActor({
    title: "Example Knowledge - Getting Started",
    content: "This is an example knowledge node. Use 'knowledge add' to create more.",
    sources: ["System"],
    graph,
  });

  const knowledgeId = graph.getNodeIds()[0];

  await saveGraph(graph, filePath);
  console.log(`Created ${KNOWLEDGE_FILE} with example knowledge`);
  console.log(`Knowledge ID: ${knowledgeId}`);
}

async function cmdAdd(title: string, options: { content?: string; sources?: string }) {
  const filePath = resolve(KNOWLEDGE_FILE);

  if (!existsSync(filePath)) {
    console.error(`Error: ${KNOWLEDGE_FILE} not found. Run 'knowledge init' first.`);
    process.exit(1);
  }

  const graph = await loadGraph(filePath);

  // Parse sources
  const sources = options.sources ? options.sources.split(",").map((s) => s.trim()) : [];

  // Create knowledge node
  KnowledgeActor({
    title,
    content: options.content || "",
    sources,
    graph,
  });

  // Get the new knowledge ID (last one added)
  const allIds = graph.getNodeIds();
  const knowledgeId = allIds[allIds.length - 1];

  await saveGraph(graph, filePath);

  console.log(`Added knowledge: ${knowledgeId}`);
  console.log(`Title: ${title}`);
  console.log(`Version: 1`);
}

async function cmdGet(id: string) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  const result = await graph.send(id, "get", {});
  const getResponse = result as { id: string; properties: KnowledgeProperties; edges: Edge[] };

  const props = getResponse.properties;

  console.log(`\nKnowledge: ${id}`);
  console.log("─".repeat(80));
  console.log(`Title:          ${props.title}`);
  console.log(`Version:        ${props.version}`);
  console.log(`Created:        ${props.createdAt}`);
  console.log(`Content Length: ${props.content.length} characters`);
  console.log(`Sources:        ${props.sources.length}`);

  console.log(`\nContent:`);
  console.log(props.content);

  if (props.sources.length > 0) {
    console.log(`\nSources:`);
    props.sources.forEach((source) => console.log(`  - ${source}`));
  }

  if (getResponse.edges.length > 0) {
    console.log(`\nConnected Nodes (${getResponse.edges.length}):`);
    getResponse.edges.forEach((e) => {
      const direction = e.fromId === id ? "->" : "<-";
      const otherNode = e.fromId === id ? e.toId : e.fromId;
      console.log(`  ${otherNode} ${direction} (${e.type})`);
    });
  }

  console.log();
}

async function cmdList(options: { limit?: number }) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  let nodeIds = graph.getNodeIds().filter((id) => {
    const props = graph.getNodeProperties(id);
    return props?.type === "knowledge";
  });

  // Sort by creation date (newest first)
  nodeIds.sort((a, b) => {
    const propsA = graph.getNodeProperties(a) as KnowledgeProperties;
    const propsB = graph.getNodeProperties(b) as KnowledgeProperties;
    const dateA = new Date(propsA.createdAt).getTime();
    const dateB = new Date(propsB.createdAt).getTime();
    return dateB - dateA; // Descending
  });

  // Apply limit
  if (options.limit) {
    nodeIds = nodeIds.slice(0, options.limit);
  }

  console.log("\nKnowledge Nodes:");
  console.log("─".repeat(80));
  console.log("ID".padEnd(20) + "Title".padEnd(35) + "Ver".padEnd(5) + "Created");
  console.log("─".repeat(80));

  if (nodeIds.length === 0) {
    console.log("No knowledge nodes found.");
  } else {
    for (const id of nodeIds) {
      const props = graph.getNodeProperties(id) as KnowledgeProperties;
      const titleTrunc = props.title.length > 32 ? props.title.slice(0, 29) + "..." : props.title;
      const created = new Date(props.createdAt).toISOString().split("T")[0]; // Just date
      console.log(
        `${id.padEnd(20)}${titleTrunc.padEnd(35)}${String(props.version).padEnd(5)}${created}`
      );
    }
  }

  console.log("─".repeat(80));
  console.log(`Total: ${nodeIds.length} knowledge node(s)`);
  console.log();
}

async function cmdAppend(id: string, content: string, options: { source?: string }) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  // Send append message
  const result = await graph.send(id, "append", {
    data: content,
    source: options.source,
  });

  const appendResponse = result as { success: boolean; version: number };

  await saveGraph(graph, filePath);

  console.log(`Appended to ${id}`);
  console.log(`New version: ${appendResponse.version}`);
  if (options.source) {
    console.log(`Source added: ${options.source}`);
  }
}

async function cmdUpdate(id: string, options: { title?: string; content?: string }) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  if (!options.title && !options.content) {
    console.error("Error: Provide at least --title or --content");
    process.exit(1);
  }

  // Build update properties
  const updateProps: Partial<KnowledgeProperties> = {};
  if (options.title) updateProps.title = options.title;
  if (options.content) updateProps.content = options.content;

  // Send update message
  await graph.send(id, "update", { properties: updateProps });

  await saveGraph(graph, filePath);

  console.log(`Updated ${id}`);
  if (options.title) console.log(`Title: ${options.title}`);
  if (options.content) console.log(`Content replaced`);
}

async function cmdDelete(id: string, force: boolean = false) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  // Check if knowledge exists
  const props = graph.getNodeProperties(id) as KnowledgeProperties;
  if (!props || props.type !== "knowledge") {
    console.error(`Error: Knowledge not found: ${id}`);
    process.exit(1);
  }

  // Get connected edges
  const edges = graph.getAllEdges(id);

  // Show what will be deleted (unless forced)
  if (!force) {
    console.log(`\nKnowledge to delete: ${id}`);
    console.log("─".repeat(80));
    console.log(`Title: ${props.title}`);
    console.log(`Version: ${props.version}`);
    console.log(`Content length: ${props.content.length} characters`);

    if (edges.length > 0) {
      console.log(`\nConnected to ${edges.length} node(s):`);
      edges.forEach((e) => {
        const otherNode = e.fromId === id ? e.toId : e.fromId;
        console.log(`  ${otherNode} (${e.type})`);
      });
    }

    // Confirmation prompt
    console.log();
    const answer = prompt("Delete this knowledge? (yes/no): ");
    if (answer?.toLowerCase() !== "yes") {
      console.log("Deletion cancelled.");
      process.exit(0);
    }
  }

  // Delete the knowledge
  const removed = graph.removeNode(id);

  if (removed) {
    await saveGraph(graph, filePath);
    console.log(`\nDeleted ${id}`);
    if (edges.length > 0) {
      console.log(`Removed ${edges.length} connected edge(s)`);
    }
  } else {
    console.error(`Error: Failed to delete knowledge ${id}`);
    process.exit(1);
  }
}

async function cmdQuery(id: string, question: string) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  // Send query message
  const result = await graph.send(id, "query", { question });
  const queryResponse = result as { answer: string; confidence: number; sources: string[] };

  console.log(`\nQuery: "${question}"`);
  console.log(`Knowledge: ${id}`);
  console.log("─".repeat(80));
  console.log(`\nAnswer:`);
  console.log(queryResponse.answer);
  console.log(`\nConfidence: ${(queryResponse.confidence * 100).toFixed(0)}%`);

  if (queryResponse.sources.length > 0) {
    console.log(`\nSources:`);
    queryResponse.sources.forEach((source) => console.log(`  - ${source}`));
  }
  console.log();
}

async function cmdSearch(query: string) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  const nodeIds = graph.getNodeIds().filter((id) => {
    const props = graph.getNodeProperties(id);
    return props?.type === "knowledge";
  });

  // Query each knowledge node
  const results: Array<{
    id: string;
    title: string;
    answer: string;
    confidence: number;
  }> = [];

  for (const id of nodeIds) {
    const result = await graph.send(id, "query", { question: query });
    const queryResponse = result as { answer: string; confidence: number; sources: string[] };

    if (queryResponse.confidence > 0) {
      const props = graph.getNodeProperties(id) as KnowledgeProperties;
      results.push({
        id,
        title: props.title,
        answer: queryResponse.answer,
        confidence: queryResponse.confidence,
      });
    }
  }

  // Sort by confidence (descending)
  results.sort((a, b) => b.confidence - a.confidence);

  console.log(`\nSearch: "${query}"`);
  console.log("─".repeat(80));

  if (results.length === 0) {
    console.log("No matching knowledge found.");
  } else {
    console.log(`\nResults:`);
    for (const result of results) {
      const snippetPreview = result.answer.length > 60
        ? result.answer.slice(0, 57) + "..."
        : result.answer;
      const confidence = `${(result.confidence * 100).toFixed(0)}%`;
      console.log(`\n[${confidence}] ${result.id}: ${result.title}`);
      console.log(`  ${snippetPreview}`);
    }
  }

  console.log("\n─".repeat(80));
  console.log(`Found ${results.length} matching knowledge node(s)`);
  console.log();
}

async function cmdLink(knowledgeId: string, nodeId: string, options: { type?: string }) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  if (!options.type) {
    console.error("Error: --type is required");
    process.exit(1);
  }

  // Validate knowledge exists
  const kProps = graph.getNodeProperties(knowledgeId);
  if (!kProps || kProps.type !== "knowledge") {
    console.error(`Error: Knowledge not found: ${knowledgeId}`);
    process.exit(1);
  }

  // For linking to external nodes, we just create the edge directly
  // (target node might be in different file like tasks.json)
  const edge = graph.addEdge(knowledgeId, nodeId, options.type as any);

  await saveGraph(graph, filePath);

  console.log(`Linked ${knowledgeId} to ${nodeId}`);
  console.log(`Edge type: ${options.type}`);
  console.log(`Edge ID: ${edge.id}`);
}

async function cmdUnlink(knowledgeId: string, nodeId: string) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  // Find edges between nodes (in either direction)
  const edges = graph.getAllEdges(knowledgeId).filter((e) =>
    (e.fromId === knowledgeId && e.toId === nodeId) ||
    (e.fromId === nodeId && e.toId === knowledgeId)
  );

  if (edges.length === 0) {
    console.error(`Error: No edges found between ${knowledgeId} and ${nodeId}`);
    process.exit(1);
  }

  // Remove all matching edges
  let removedCount = 0;
  for (const edge of edges) {
    if (graph.removeEdge(edge.id)) {
      removedCount++;
    }
  }

  await saveGraph(graph, filePath);

  console.log(`Unlinked ${knowledgeId} from ${nodeId}`);
  console.log(`Removed ${removedCount} edge(s)`);
}

async function cmdSynthesize(id: string, options: { from?: string }) {
  const filePath = resolve(KNOWLEDGE_FILE);
  const graph = await loadGraph(filePath);

  if (!options.from) {
    console.error("Error: --from is required (comma-separated node IDs)");
    process.exit(1);
  }

  // Parse source node IDs
  const fromNodes = options.from.split(",").map((s) => s.trim());

  // Send synthesize message
  const result = await graph.send(id, "synthesize", { fromNodes });
  const synthResponse = result as { synthesis: string; sources: string[] };

  const props = graph.getNodeProperties(id) as KnowledgeProperties;

  console.log(`\nSynthesized from ${fromNodes.length} source(s)`);
  console.log(`Target: ${id}`);
  console.log(`Sources: ${fromNodes.join(", ")}`);
  console.log("─".repeat(80));
  console.log(`\nSynthesis (${synthResponse.synthesis.length} characters):`);
  console.log(synthResponse.synthesis);

  console.log("\n─".repeat(80));
  console.log(`Combined sources:`);
  synthResponse.sources.forEach((source) => console.log(`  - ${source}`));
  console.log();
}

// Help message
function showHelp() {
  console.log("Knowledge CLI - Manage knowledge nodes\n");
  console.log("Commands:");
  console.log("  knowledge init                            Create knowledge.json");
  console.log("  knowledge add <title> [--content TEXT] [--sources S1,S2]");
  console.log("                                            Create knowledge node");
  console.log("  knowledge get <id>                        Show knowledge details");
  console.log("  knowledge list [--limit N]                List knowledge nodes");
  console.log("  knowledge append <id> <content> [--source SRC]");
  console.log("                                            Append to knowledge");
  console.log("  knowledge update <id> [--title TEXT] [--content TEXT]");
  console.log("                                            Update knowledge properties");
  console.log("  knowledge delete <id> [--force]           Delete knowledge node");
  console.log("  knowledge query <id> <question>           Query knowledge");
  console.log("  knowledge search <query>                  Search all knowledge");
  console.log("  knowledge link <kid> <nid> --type TYPE    Link to node");
  console.log("  knowledge unlink <kid> <nid>              Unlink from node");
  console.log("  knowledge synthesize <id> --from ID1,ID2  Synthesize from sources");
}

// Main CLI
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  switch (command) {
    case "init":
      await cmdInit();
      break;

    case "add": {
      if (args.length < 2) {
        console.error("Usage: knowledge add <title> [--content TEXT] [--sources S1,S2]");
        process.exit(1);
      }
      const title = args[1];
      const options: { content?: string; sources?: string } = {};
      for (let i = 2; i < args.length; i++) {
        if (args[i] === "--content" && args[i + 1]) {
          options.content = args[i + 1];
          i++;
        } else if (args[i] === "--sources" && args[i + 1]) {
          options.sources = args[i + 1];
          i++;
        }
      }
      await cmdAdd(title, options);
      break;
    }

    case "get": {
      if (args.length < 2) {
        console.error("Usage: knowledge get <id>");
        process.exit(1);
      }
      await cmdGet(args[1]);
      break;
    }

    case "list": {
      const options: { limit?: number } = {};
      for (let i = 1; i < args.length; i++) {
        if (args[i] === "--limit" && args[i + 1]) {
          options.limit = parseInt(args[i + 1]);
          i++;
        }
      }
      await cmdList(options);
      break;
    }

    case "append": {
      if (args.length < 3) {
        console.error("Usage: knowledge append <id> <content> [--source SRC]");
        process.exit(1);
      }
      const id = args[1];
      const content = args[2];
      const options: { source?: string } = {};
      for (let i = 3; i < args.length; i++) {
        if (args[i] === "--source" && args[i + 1]) {
          options.source = args[i + 1];
          i++;
        }
      }
      await cmdAppend(id, content, options);
      break;
    }

    case "update": {
      if (args.length < 2) {
        console.error("Usage: knowledge update <id> [--title TEXT] [--content TEXT]");
        process.exit(1);
      }
      const id = args[1];
      const options: { title?: string; content?: string } = {};
      for (let i = 2; i < args.length; i++) {
        if (args[i] === "--title" && args[i + 1]) {
          options.title = args[i + 1];
          i++;
        } else if (args[i] === "--content" && args[i + 1]) {
          options.content = args[i + 1];
          i++;
        }
      }
      await cmdUpdate(id, options);
      break;
    }

    case "delete": {
      if (args.length < 2) {
        console.error("Usage: knowledge delete <id> [--force]");
        process.exit(1);
      }
      const id = args[1];
      const force = args.includes("--force");
      await cmdDelete(id, force);
      break;
    }

    case "query": {
      if (args.length < 3) {
        console.error("Usage: knowledge query <id> <question>");
        process.exit(1);
      }
      await cmdQuery(args[1], args[2]);
      break;
    }

    case "search": {
      if (args.length < 2) {
        console.error("Usage: knowledge search <query>");
        process.exit(1);
      }
      await cmdSearch(args[1]);
      break;
    }

    case "link": {
      if (args.length < 3) {
        console.error("Usage: knowledge link <kid> <nid> --type TYPE");
        process.exit(1);
      }
      const knowledgeId = args[1];
      const nodeId = args[2];
      const options: { type?: string } = {};
      for (let i = 3; i < args.length; i++) {
        if (args[i] === "--type" && args[i + 1]) {
          options.type = args[i + 1];
          i++;
        }
      }
      await cmdLink(knowledgeId, nodeId, options);
      break;
    }

    case "unlink": {
      if (args.length < 3) {
        console.error("Usage: knowledge unlink <kid> <nid>");
        process.exit(1);
      }
      await cmdUnlink(args[1], args[2]);
      break;
    }

    case "synthesize": {
      if (args.length < 2) {
        console.error("Usage: knowledge synthesize <id> --from ID1,ID2");
        process.exit(1);
      }
      const id = args[1];
      const options: { from?: string } = {};
      for (let i = 2; i < args.length; i++) {
        if (args[i] === "--from" && args[i + 1]) {
          options.from = args[i + 1];
          i++;
        }
      }
      await cmdSynthesize(id, options);
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
