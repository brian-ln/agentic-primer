#!/usr/bin/env bun

import { MarkdownGraph } from './MarkdownGraph';

const sampleMarkdown = `# Graph System Specification

## Overview

The Graph System is a central store and message router.

### Core Responsibilities

## Core Concepts

See [ACTOR_SYSTEM.spec.md](./actors/ACTOR_SYSTEM.spec.md) for details.

\`\`\`typescript
const graph = new Graph();
graph.registerNode("task-1", address, props);
\`\`\`

### Dual Identity Model

## API Reference

### Key Methods

\`\`\`typescript
graph.send("task-1", "get", {});
\`\`\`
`;

const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

console.log('=== H2 Sections ===');
const h2Sections = graph.findNodes({ type: 'heading', depth: 2 });
for (const section of h2Sections) {
  console.log(`\nSection: ${section.metadata.text}`);

  // Get descendants (AST tree children)
  const descendants = graph.getDescendants(section.id);
  const codeInDescendants = descendants.filter(n => n.type === 'code');
  console.log(`  Descendants: ${descendants.length}, Code in descendants: ${codeInDescendants.length}`);

  // Get code via contains-code edges
  const codeEdges = graph.getEdges(section.id, 'contains-code', 'from');
  console.log(`  Contains-code edges: ${codeEdges.length}`);

  // Get all code blocks that belong to this section
  const allCode = graph.findNodes({ type: 'code' });
  const codeInSection = allCode.filter(c => c.metadata.section === section.metadata.text);
  console.log(`  Code blocks with matching section: ${codeInSection.length}`);
}
