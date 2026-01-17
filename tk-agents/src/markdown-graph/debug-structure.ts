#!/usr/bin/env bun

import { MarkdownGraph } from './MarkdownGraph';

const sampleMarkdown = `## Core Concepts

See [ACTOR_SYSTEM.spec.md](./actors/ACTOR_SYSTEM.spec.md) for details.

\`\`\`typescript
const graph = new Graph();
graph.registerNode("task-1", address, props);
\`\`\`

### Dual Identity Model
`;

const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

console.log('=== Node Tree ===');
const root = graph.getAllNodes().find(n => n.type === 'root');

function printTree(nodeId: string, indent: number = 0) {
  const node = graph.getNode(nodeId);
  if (!node) return;

  const prefix = '  '.repeat(indent);
  console.log(`${prefix}${node.type} (${node.id})${node.metadata.text ? ': ' + node.metadata.text : ''}`);

  for (const childId of node.children) {
    printTree(childId, indent + 1);
  }
}

if (root) {
  printTree(root.id);
}

console.log('\n=== Code Blocks and Their Parents ===');
const codeBlocks = graph.findNodes({ type: 'code' });
for (const code of codeBlocks) {
  console.log(`Code: ${code.id}`);
  const ancestors = graph.getAncestors(code.id);
  console.log(`  Ancestors: ${ancestors.map(a => `${a.type}(${a.metadata.text || ''})`).join(' <- ')}`);

  // Find heading ancestor
  const headingAncestor = ancestors.find(a => a.type === 'heading');
  console.log(`  Heading ancestor: ${headingAncestor?.metadata.text || 'NONE'}`);
}

console.log('\n=== Contains-code Edges ===');
const allEdges = graph.dump().edges;
const codeEdges = allEdges.filter(e => e.type === 'contains-code');
console.log(`Found ${codeEdges.length} contains-code edges`);
for (const edge of codeEdges) {
  const from = graph.getNode(edge.fromId);
  console.log(`  ${from?.metadata.text} (${edge.fromId}) -> ${edge.toId}`);
}
