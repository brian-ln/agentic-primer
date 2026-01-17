#!/usr/bin/env bun

/**
 * Demonstration of Markdown Graph capabilities
 *
 * Run: bun src/markdown-graph/demo.ts
 */

import { MarkdownGraph } from './MarkdownGraph';
import * as path from 'path';

const DEMO_SECTIONS = {
  PARSING: '1. Parsing and Structure',
  NAVIGATION: '2. Structural Navigation',
  QUERIES: '3. Semantic Queries',
  ANALYSIS: '4. Document Analysis',
  CROSS_REF: '5. Cross-Reference Analysis'
};

function section(title: string) {
  console.log(`\n${'='.repeat(70)}`);
  console.log(`  ${title}`);
  console.log('='.repeat(70));
}

function subsection(title: string) {
  console.log(`\n--- ${title} ---`);
}

async function demo() {
  const specPath = path.join(process.cwd(), 'src/GRAPH_SYSTEM.spec.md');

  section(DEMO_SECTIONS.PARSING);

  console.log(`Loading: ${specPath}`);
  const graph = await MarkdownGraph.fromFile(specPath);

  const nodes = graph.getAllNodes();
  console.log(`Total nodes: ${nodes.length}`);

  const nodeTypeCounts = new Map<string, number>();
  for (const node of nodes) {
    nodeTypeCounts.set(node.type, (nodeTypeCounts.get(node.type) || 0) + 1);
  }

  console.log('\nNode type distribution:');
  for (const [type, count] of Array.from(nodeTypeCounts.entries()).sort((a, b) => b[1] - a[1])) {
    console.log(`  ${type.padEnd(15)} ${count.toString().padStart(4)}`);
  }

  section(DEMO_SECTIONS.NAVIGATION);

  subsection('Heading Hierarchy');
  const hierarchy = graph.getHeadingHierarchy();

  function printHierarchy(node: any, indent = 0) {
    if (node.depth > 0) {
      const prefix = '  '.repeat(indent);
      console.log(`${prefix}${'H'.repeat(node.depth)}: ${node.text}`);
    }
    for (const child of node.children.slice(0, 3)) { // Limit to first 3 for brevity
      printHierarchy(child, indent + 1);
    }
    if (node.children.length > 3) {
      const prefix = '  '.repeat(indent + 1);
      console.log(`${prefix}... (${node.children.length - 3} more)`);
    }
  }

  printHierarchy(hierarchy);

  subsection('Table of Contents');
  const toc = graph.extractTableOfContents();
  console.log(`\nFirst 10 entries:`);
  for (const entry of toc.slice(0, 10)) {
    const indent = '  '.repeat(entry.depth - 1);
    console.log(`${indent}${entry.depth}. ${entry.text}`);
  }
  console.log(`... (${toc.length} total entries)`);

  section(DEMO_SECTIONS.QUERIES);

  subsection('Find H2 Sections');
  const h2Sections = graph.findNodes({ type: 'heading', depth: 2 });
  console.log(`Found ${h2Sections.length} H2 sections:`);
  for (const section of h2Sections.slice(0, 5)) {
    console.log(`  - ${section.metadata.text}`);
  }
  if (h2Sections.length > 5) {
    console.log(`  ... (${h2Sections.length - 5} more)`);
  }

  subsection('Find TypeScript Code Blocks');
  const tsCode = graph.findNodes({
    type: 'code',
    lang: 'typescript'
  });
  console.log(`Found ${tsCode.length} TypeScript code blocks`);

  if (tsCode.length > 0) {
    const firstCode = tsCode[0];
    console.log(`\nFirst example (section: ${firstCode.metadata.section}):`);
    const codePreview = (firstCode.properties.value as string)
      .split('\n')
      .slice(0, 3)
      .join('\n');
    console.log(codePreview);
    console.log('...');
  }

  subsection('Find Code in Specific Section');
  const targetSection = 'Core Features';
  const sectionNode = graph.findSection(targetSection);

  if (sectionNode) {
    // Use contains-code edges
    const codeEdges = graph.getEdges(sectionNode.id, 'contains-code', 'from');
    console.log(`\nSection "${targetSection}" contains ${codeEdges.length} code blocks`);

    // Also find via section metadata
    const allCode = graph.findNodes({ type: 'code' });
    const codeInSection = allCode.filter(c =>
      c.metadata.section === targetSection ||
      c.metadata.section?.startsWith(targetSection)
    );
    console.log(`Via section metadata: ${codeInSection.length} code blocks`);
  }

  subsection('Find Strong Emphasis (Bold Text)');
  const strongNodes = graph.findNodes({ type: 'strong' });
  console.log(`Found ${strongNodes.length} bold text nodes`);
  const samples = strongNodes.slice(0, 5).map(n => n.metadata.text);
  console.log(`Samples: ${samples.join(', ')}...`);

  section(DEMO_SECTIONS.ANALYSIS);

  subsection('Document Metrics');
  const metrics = graph.computeDocumentMetrics();
  console.log(`Nodes:          ${metrics.nodeCount}`);
  console.log(`Edges:          ${metrics.edgeCount}`);
  console.log(`Headings:       ${metrics.headingCount}`);
  console.log(`Code blocks:    ${metrics.codeBlockCount}`);
  console.log(`Links:          ${metrics.linkCount}`);
  console.log(`Word count:     ${metrics.wordCount}`);
  console.log(`Max tree depth: ${metrics.maxDepth}`);

  subsection('Link Analysis');
  const links = graph.findLinks();
  console.log(`\nTotal links: ${links.length}`);

  const linkUrls = links.map(l => l.properties.url as string);
  const internalLinks = linkUrls.filter(u => u.startsWith('./') || u.startsWith('../') || u.startsWith('#'));
  const externalLinks = linkUrls.filter(u => u.startsWith('http'));
  const otherLinks = linkUrls.filter(u => !internalLinks.includes(u) && !externalLinks.includes(u));

  console.log(`  Internal: ${internalLinks.length}`);
  console.log(`  External: ${externalLinks.length}`);
  console.log(`  Other:    ${otherLinks.length}`);

  if (internalLinks.length > 0) {
    console.log(`\nInternal links (first 5):`);
    for (const url of internalLinks.slice(0, 5)) {
      console.log(`  - ${url}`);
    }
  }

  subsection('Code Block Distribution by Section');
  const sectionsWithCode = new Map<string, number>();

  for (const code of graph.findNodes({ type: 'code' })) {
    const section = code.metadata.section || 'No section';
    sectionsWithCode.set(section, (sectionsWithCode.get(section) || 0) + 1);
  }

  console.log(`\nSections with most code examples:`);
  const sortedSections = Array.from(sectionsWithCode.entries())
    .sort((a, b) => b[1] - a[1])
    .slice(0, 5);

  for (const [section, count] of sortedSections) {
    console.log(`  ${section.padEnd(40)} ${count} blocks`);
  }

  section(DEMO_SECTIONS.CROSS_REF);

  subsection('Sections-of Edges');
  const sectionEdges = graph.dump().edges.filter(e => e.type === 'section-of');
  console.log(`Found ${sectionEdges.length} section-of edges`);

  subsection('Contains-Code Edges');
  const codeEdges = graph.dump().edges.filter(e => e.type === 'contains-code');
  console.log(`Found ${codeEdges.length} contains-code edges`);

  const codeBySection = new Map<string, number>();
  for (const edge of codeEdges) {
    const heading = graph.getNode(edge.fromId);
    if (heading) {
      const section = heading.metadata.text || 'Unknown';
      codeBySection.set(section, (codeBySection.get(section) || 0) + 1);
    }
  }

  console.log(`\nSections linked to code:`);
  for (const [section, count] of Array.from(codeBySection.entries()).slice(0, 5)) {
    console.log(`  ${section}: ${count} code blocks`);
  }

  subsection('Serialization Round-Trip');
  const dump = graph.dump();
  console.log(`\nDump size:`);
  console.log(`  Nodes: ${dump.nodes.length}`);
  console.log(`  Edges: ${dump.edges.length}`);
  console.log(`  Timestamp: ${dump.metadata.timestamp}`);

  const restored = MarkdownGraph.fromDump(dump);
  console.log(`\nRestored graph:`);
  console.log(`  Nodes: ${restored.getAllNodes().length}`);
  console.log(`  Match: ${restored.getAllNodes().length === graph.getAllNodes().length ? '✓' : '✗'}`);

  console.log('\n' + '='.repeat(70));
  console.log('  Demo Complete');
  console.log('='.repeat(70));
  console.log('\nKey Takeaways:');
  console.log('  1. Markdown structure is fully queryable as a graph');
  console.log('  2. Semantic relationships (sections, code, links) are explicit edges');
  console.log('  3. Navigation and analysis operations are efficient');
  console.log('  4. Round-trip serialization preserves full graph structure');
  console.log('\nNext: See MARKDOWN_GRAPH_INTEGRATION.md for tk-agents integration');
}

demo().catch(console.error);
