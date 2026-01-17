#!/usr/bin/env bun

/**
 * Exploration script to understand markdown AST structure
 * Run: bun src/markdown-graph/explore-ast.ts
 */

import { unified } from 'unified';
import remarkParse from 'remark-parse';
import { visit } from 'unist-util-visit';

const sampleMarkdown = `# Graph System Specification

## Overview

The Graph System is a central store and message router.

### Core Responsibilities

1. **Node Management** - Register nodes
2. **Edge Management** - Create relationships
3. **Message Routing** - Route messages

## Core Concepts

See [ACTOR_SYSTEM.spec.md](./actors/ACTOR_SYSTEM.spec.md) for details.

\`\`\`typescript
const graph = new Graph();
graph.registerNode("task-1", address, props);
\`\`\`

### Dual Identity Model

Every node has two identities:
- String ID (external)
- Address (internal)
`;

async function exploreAST() {
  // Parse markdown to AST
  const processor = unified().use(remarkParse);
  const ast = processor.parse(sampleMarkdown);

  console.log('=== Full AST Structure ===');
  console.log(JSON.stringify(ast, null, 2));

  console.log('\n=== Node Types Present ===');
  const nodeTypes = new Set<string>();
  visit(ast, (node) => {
    nodeTypes.add(node.type);
  });
  console.log(Array.from(nodeTypes).sort());

  console.log('\n=== Headings Hierarchy ===');
  visit(ast, 'heading', (node: any) => {
    const indent = '  '.repeat(node.depth - 1);
    const text = node.children.map((c: any) => c.value || '').join('');
    console.log(`${indent}H${node.depth}: ${text}`);
  });

  console.log('\n=== Links Found ===');
  visit(ast, 'link', (node: any) => {
    console.log(`Link: ${node.url} (text: ${node.children.map((c: any) => c.value).join('')})`);
  });

  console.log('\n=== Code Blocks ===');
  visit(ast, 'code', (node: any) => {
    console.log(`Code block (${node.lang}): ${node.value.substring(0, 50)}...`);
  });
}

exploreAST().catch(console.error);
