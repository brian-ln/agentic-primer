#!/usr/bin/env bun

import { MarkdownGraph } from './MarkdownGraph';

const sampleMarkdown = `# Test

See [ACTOR_SYSTEM.spec.md](./actors/ACTOR_SYSTEM.spec.md) for details.

\`\`\`typescript
const graph = new Graph();
\`\`\`
`;

const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

console.log('=== All Edges ===');
const dump = graph.dump();
for (const edge of dump.edges) {
  console.log(`${edge.type}: ${edge.fromId} -> ${edge.toId}`);
  console.log(`  Properties:`, edge.properties);
}

console.log('\n=== Links-to Edges ===');
const links = graph.findLinks();
console.log(`Found ${links.length} links-to edges`);
for (const link of links) {
  console.log(`  ${link.fromId} -> ${link.toId} (${link.properties.url})`);
}

console.log('\n=== Contains-code Edges ===');
const headings = graph.findNodes({ type: 'heading' });
for (const heading of headings) {
  const codeEdges = graph.getEdges(heading.id, 'contains-code', 'from');
  if (codeEdges.length > 0) {
    console.log(`${heading.metadata.text}: ${codeEdges.length} code blocks`);
  }
}

console.log('\n=== Link Nodes ===');
const linkNodes = graph.findNodes({ type: 'link' });
for (const node of linkNodes) {
  console.log(`Link node: ${node.id}, url: ${node.properties.url}, parent: ${node.parent}`);
}

console.log('\n=== Code Nodes ===');
const codeNodes = graph.findNodes({ type: 'code' });
for (const node of codeNodes) {
  console.log(`Code node: ${node.id}, parent: ${node.parent}`);
  const parent = node.parent ? graph.getNode(node.parent) : null;
  console.log(`  Parent type: ${parent?.type}`);
}
