#!/usr/bin/env bun
// Interactive Concept Graph Explorer

import {
  search,
  getConcept,
  getAllDomains,
  getAllTags,
  getRelatedConcepts,
  traverse,
  findPath,
  formatConcept,
  formatRelationships,
  getStats,
  type SearchOptions,
} from "./search";

// CLI utilities
function printSection(title: string) {
  console.log("\n" + "=".repeat(60));
  console.log(title);
  console.log("=".repeat(60) + "\n");
}

function printList(items: string[], prefix: string = "  - ") {
  for (const item of items) {
    console.log(prefix + item);
  }
}

// Commands

function showStats() {
  printSection("Knowledge Graph Statistics");
  const stats = getStats();
  console.log(`Total Concepts: ${stats.totalConcepts}`);
  console.log(`Total Relationships: ${stats.totalRelationships}`);
  console.log(`Domains: ${stats.domains}`);
  console.log(`Tags: ${stats.tags}`);
  console.log(`Relationship Types: ${stats.relationshipTypes}`);
}

function listDomains() {
  printSection("Available Domains");
  const domains = getAllDomains();
  printList(domains);
}

function listTags() {
  printSection("Available Tags");
  const tags = getAllTags();
  console.log(`(${tags.length} tags)`);
  // Print in columns
  const columns = 4;
  for (let i = 0; i < tags.length; i += columns) {
    const row = tags.slice(i, i + columns);
    console.log("  " + row.join(", "));
  }
}

function searchConcepts(options: SearchOptions) {
  const results = search(options);

  if (results.length === 0) {
    console.log("No concepts found matching criteria.");
    return;
  }

  printSection(
    `Search Results (${results.length} concept${results.length > 1 ? "s" : ""})`
  );

  for (const concept of results) {
    console.log(`\n${concept.label} (${concept.id})`);
    console.log(`  Domains: ${concept.domains.join(", ")}`);
    console.log(`  Tags: ${concept.tags.join(", ")}`);
    console.log(`  ${concept.description.substring(0, 100)}...`);
  }
}

function showConcept(id: string) {
  const concept = getConcept(id);
  if (!concept) {
    console.log(`Concept not found: ${id}`);
    return;
  }

  printSection(concept.label);
  console.log(formatConcept(concept));
  console.log("\n" + formatRelationships(id));
}

function exploreConcept(id: string, depth: number = 1) {
  const concept = getConcept(id);
  if (!concept) {
    console.log(`Concept not found: ${id}`);
    return;
  }

  printSection(`Exploring: ${concept.label}`);

  const reachable = traverse(id, { maxDepth: depth, direction: "both" });

  console.log(`Found ${reachable.size} concepts within ${depth} hop(s):\n`);

  // Group by depth
  const byDepth = new Map<number, string[]>();
  for (const [conceptId, { depth }] of reachable) {
    if (!byDepth.has(depth)) {
      byDepth.set(depth, []);
    }
    byDepth.get(depth)!.push(conceptId);
  }

  for (let d = 0; d <= depth; d++) {
    const conceptIds = byDepth.get(d) ?? [];
    if (conceptIds.length > 0) {
      console.log(`Depth ${d}: (${conceptIds.length})`);
      for (const cid of conceptIds) {
        const c = getConcept(cid)!;
        console.log(`  - ${c.label} (${cid})`);
      }
      console.log();
    }
  }
}

function showPath(fromId: string, toId: string) {
  const from = getConcept(fromId);
  const to = getConcept(toId);

  if (!from) {
    console.log(`Concept not found: ${fromId}`);
    return;
  }

  if (!to) {
    console.log(`Concept not found: ${toId}`);
    return;
  }

  printSection(`Path from "${from.label}" to "${to.label}"`);

  const path = findPath(fromId, toId);

  if (!path) {
    console.log("No path found between these concepts.");
    return;
  }

  console.log(`Path length: ${path.length - 1} hop(s)\n`);

  for (let i = 0; i < path.length; i++) {
    const c = getConcept(path[i])!;
    console.log(`${i}. ${c.label} (${path[i]})`);

    if (i < path.length - 1) {
      // Show relationship
      const related = getRelatedConcepts(path[i]);
      const nextId = path[i + 1];

      const outgoing = related.outgoing.find(
        (r) => r.concept.id === nextId
      );
      const incoming = related.incoming.find(
        (r) => r.concept.id === nextId
      );

      if (outgoing) {
        console.log(
          `   → ${outgoing.relationship.type}: ${outgoing.relationship.description}`
        );
      } else if (incoming) {
        console.log(
          `   ← ${incoming.relationship.type}: ${incoming.relationship.description}`
        );
      }
    }
  }
}

function showRelated(id: string) {
  const concept = getConcept(id);
  if (!concept) {
    console.log(`Concept not found: ${id}`);
    return;
  }

  printSection(`Concepts related to: ${concept.label}`);

  const related = getRelatedConcepts(id);

  if (related.outgoing.length > 0) {
    console.log("\n### Outgoing Relationships\n");
    for (const { relationship, concept: relConcept } of related.outgoing) {
      console.log(`• ${relConcept.label}`);
      console.log(`  ${relationship.type}: ${relationship.description}`);
    }
  }

  if (related.incoming.length > 0) {
    console.log("\n### Incoming Relationships\n");
    for (const { relationship, concept: relConcept } of related.incoming) {
      console.log(`• ${relConcept.label}`);
      console.log(`  ${relationship.type}: ${relationship.description}`);
    }
  }
}

// CLI Interface

function printHelp() {
  console.log(`
Concept Graph Explorer - Interactive navigation of interdisciplinary concepts

USAGE:
  bun explore.ts <command> [arguments]

COMMANDS:
  stats                         Show graph statistics
  domains                       List all domains
  tags                          List all tags

  search <query>                Search by keyword
  search --domain <domain>      Search by domain
  search --tag <tag>            Search by tag

  show <concept-id>             Show concept details
  related <concept-id>          Show related concepts
  explore <concept-id> [depth]  Explore neighborhood (default depth: 1)
  path <from-id> <to-id>        Find shortest path between concepts

  help                          Show this help message

EXAMPLES:
  bun explore.ts stats
  bun explore.ts search actor
  bun explore.ts search --domain neuroscience
  bun explore.ts search --tag erlang
  bun explore.ts show actor-model
  bun explore.ts related erlang-otp
  bun explore.ts explore message-passing 2
  bun explore.ts path actor-model memory-reconsolidation
`);
}

// Main
function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    printHelp();
    return;
  }

  const command = args[0];

  switch (command) {
    case "stats":
      showStats();
      break;

    case "domains":
      listDomains();
      break;

    case "tags":
      listTags();
      break;

    case "search": {
      const options: SearchOptions = {};

      if (args[1] === "--domain") {
        options.domain = args[2];
      } else if (args[1] === "--tag") {
        options.tag = args[2];
      } else if (args[1]) {
        options.keyword = args.slice(1).join(" ");
      }

      searchConcepts(options);
      break;
    }

    case "show": {
      if (!args[1]) {
        console.log("Usage: show <concept-id>");
        return;
      }
      showConcept(args[1]);
      break;
    }

    case "related": {
      if (!args[1]) {
        console.log("Usage: related <concept-id>");
        return;
      }
      showRelated(args[1]);
      break;
    }

    case "explore": {
      if (!args[1]) {
        console.log("Usage: explore <concept-id> [depth]");
        return;
      }
      const depth = args[2] ? parseInt(args[2]) : 1;
      exploreConcept(args[1], depth);
      break;
    }

    case "path": {
      if (!args[1] || !args[2]) {
        console.log("Usage: path <from-id> <to-id>");
        return;
      }
      showPath(args[1], args[2]);
      break;
    }

    case "help":
    default:
      printHelp();
      break;
  }
}

main();
