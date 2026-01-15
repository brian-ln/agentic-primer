#!/usr/bin/env bun
// Example explorations of the concept graph
// Demonstrates various ways to navigate and discover connections

import {
  search,
  getConcept,
  getRelatedConcepts,
  traverse,
  findPath,
  getAllDomains,
  getAllTags,
  formatConcept,
} from "./search";

function section(title: string) {
  console.log("\n" + "=".repeat(70));
  console.log(`  ${title}`);
  console.log("=".repeat(70) + "\n");
}

// Example 1: Discovering actor model concepts
function example1() {
  section("Example 1: Actor Model Ecosystem");

  console.log("Starting from 'actor-model', exploring 2 hops...\n");

  const neighborhood = traverse("actor-model", { maxDepth: 2, direction: "both" });

  console.log(`Found ${neighborhood.size} related concepts:\n`);

  for (const [id, { concept, depth }] of neighborhood) {
    const indent = "  ".repeat(depth);
    console.log(`${indent}[${depth}] ${concept.label}`);
  }

  console.log("\nKey insight: Actor model connects to message-passing, isolation,");
  console.log("and is implemented in Erlang/OTP with supervision patterns.");
}

// Example 2: Cross-disciplinary connections
function example2() {
  section("Example 2: Bridging Computer Science and Neuroscience");

  console.log("Finding path from 'graph-protocol' to 'memory-reconsolidation'...\n");

  const path = findPath("graph-protocol", "memory-reconsolidation");

  if (path) {
    console.log(`Path found (${path.length - 1} hops):\n`);

    for (let i = 0; i < path.length; i++) {
      const concept = getConcept(path[i])!;
      console.log(`${i + 1}. ${concept.label}`);
      console.log(`   Domains: ${concept.domains.join(", ")}`);

      if (i < path.length - 1) {
        const related = getRelatedConcepts(path[i]);
        const nextId = path[i + 1];
        const rel = related.outgoing.find((r) => r.concept.id === nextId);
        if (rel) {
          console.log(`   → ${rel.relationship.type}: ${rel.relationship.description}`);
        }
      }
      console.log();
    }

    console.log("Key insight: Access patterns bridge computational and cognitive systems.");
    console.log("Task/knowledge graphs create access patterns, which influence memory reconsolidation.");
  }
}

// Example 3: Exploring by domain
function example3() {
  section("Example 3: Mathematical Foundations");

  console.log("Concepts in mathematics domain:\n");

  const mathConcepts = search({ domain: "mathematics" });

  for (const concept of mathConcepts) {
    console.log(`• ${concept.label}`);
    console.log(`  ${concept.description.substring(0, 80)}...`);

    const related = getRelatedConcepts(concept.id);
    if (related.outgoing.length > 0) {
      const targets = related.outgoing.map((r) => r.concept.label).join(", ");
      console.log(`  Connected to: ${targets}`);
    }
    console.log();
  }

  console.log("Key insight: Mathematics provides theoretical foundations");
  console.log("for functional programming, type systems, and formal methods.");
}

// Example 4: Tag-based discovery
function example4() {
  section("Example 4: Fault Tolerance Strategies");

  console.log("Concepts tagged with 'fault-tolerance':\n");

  const ftConcepts = search({ tag: "fault-tolerance" });

  for (const concept of ftConcepts) {
    console.log(`• ${concept.label} (${concept.domains.join(", ")})`);

    // Show what it enables or achieves
    const related = getRelatedConcepts(concept.id);
    const achieves = related.outgoing.filter(
      (r) => r.relationship.type === "achieves" || r.relationship.type === "uses"
    );

    if (achieves.length > 0) {
      for (const { relationship, concept: target } of achieves) {
        console.log(`  ${relationship.type} → ${target.label}`);
      }
    }
    console.log();
  }

  console.log("Key insight: Multiple approaches to fault tolerance:");
  console.log("  - Erlang: supervision + let-it-crash");
  console.log("  - Patterns: circuit-breaker");
  console.log("  - Distributed: replication + consensus");
}

// Example 5: Concept comparison
function example5() {
  section("Example 5: Comparing Message Passing Implementations");

  const concepts = [
    "actor-model",
    "erlang-otp",
    "smalltalk",
    "distributed-systems",
  ];

  for (const id of concepts) {
    const concept = getConcept(id);
    if (!concept) continue;

    console.log(`${concept.label}:`);
    console.log(`  ${concept.description.substring(0, 100)}...`);

    const related = getRelatedConcepts(id);
    const mpRelated = related.outgoing.find(
      (r) => r.concept.id === "message-passing"
    );

    if (mpRelated) {
      console.log(`  Uses message-passing: ${mpRelated.relationship.description}`);
    } else {
      // Check incoming
      const mpIncoming = related.incoming.find(
        (r) => r.concept.id === "message-passing"
      );
      if (mpIncoming) {
        console.log(`  Related to message-passing: ${mpIncoming.relationship.description}`);
      }
    }
    console.log();
  }

  console.log("Key insight: Message passing is a universal pattern across");
  console.log("actor systems, OOP (Smalltalk), and distributed computing.");
}

// Example 6: Finding conceptual gaps
function example6() {
  section("Example 6: Exploring Unconnected Areas");

  const domains = getAllDomains();

  console.log("Concepts per domain:\n");

  for (const domain of domains) {
    const concepts = search({ domain });
    console.log(`  ${domain}: ${concepts.length} concept(s)`);
  }

  console.log("\nPotential areas for expansion:");
  console.log("  - More cognitive science concepts");
  console.log("  - Biology/neuroscience mechanisms");
  console.log("  - Programming language theory");
  console.log("  - More system design patterns");
  console.log("  - Historical context and influences");
}

// Run all examples
function main() {
  console.log("\n" + "█".repeat(70));
  console.log("  CONCEPT GRAPH: Example Explorations");
  console.log("█".repeat(70));

  example1();
  example2();
  example3();
  example4();
  example5();
  example6();

  section("Summary");

  console.log("This concept graph enables multiple exploration strategies:");
  console.log("  1. Neighborhood exploration (traverse from a node)");
  console.log("  2. Path finding (discover cross-disciplinary connections)");
  console.log("  3. Domain-based filtering (focus on one discipline)");
  console.log("  4. Tag-based search (find concepts by topic)");
  console.log("  5. Comparative analysis (examine related concepts)");
  console.log("  6. Gap analysis (identify areas for expansion)");

  console.log("\nTry running these explorations yourself with explore.ts!");
  console.log("Example: bun explore.ts explore actor-model 2\n");
}

main();
