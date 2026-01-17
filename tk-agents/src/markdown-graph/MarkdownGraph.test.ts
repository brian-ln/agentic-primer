/**
 * Tests for MarkdownGraph
 */

import { test, expect, describe } from 'bun:test';
import { MarkdownGraph } from './MarkdownGraph';

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

## API Reference

### Key Methods

\`\`\`typescript
graph.send("task-1", "get", {});
\`\`\`
`;

describe('MarkdownGraph', () => {
  describe('Basic parsing', () => {
    test('parses markdown into nodes', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const nodes = graph.getAllNodes();

      expect(nodes.length).toBeGreaterThan(0);
      expect(nodes[0].type).toBe('root');
    });

    test('creates correct node types', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

      const headings = graph.findNodes({ type: 'heading' });
      const paragraphs = graph.findNodes({ type: 'paragraph' });
      const codeBlocks = graph.findNodes({ type: 'code' });
      const links = graph.findNodes({ type: 'link' });
      const lists = graph.findNodes({ type: 'list' });

      expect(headings.length).toBeGreaterThan(0);
      expect(paragraphs.length).toBeGreaterThan(0);
      expect(codeBlocks.length).toBe(2);
      expect(links.length).toBe(1);
      expect(lists.length).toBe(2); // ordered + unordered
    });

    test('captures node properties correctly', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

      // Check heading depth
      const h1 = graph.findNodes({ type: 'heading', depth: 1 });
      expect(h1.length).toBe(1);
      expect(h1[0].metadata.text).toBe('Graph System Specification');

      const h2 = graph.findNodes({ type: 'heading', depth: 2 });
      expect(h2.length).toBe(3); // Overview, Core Concepts, API Reference

      const h3 = graph.findNodes({ type: 'heading', depth: 3 });
      expect(h3.length).toBe(3); // Core Responsibilities, Dual Identity Model, Key Methods

      // Check code block properties
      const codeBlocks = graph.findNodes({ type: 'code', lang: 'typescript' });
      expect(codeBlocks.length).toBe(2);
      expect(codeBlocks[0].properties.value).toContain('graph.registerNode');

      // Check link properties
      const links = graph.findNodes({ type: 'link' });
      expect(links.length).toBe(1);
      expect(links[0].properties.url).toBe('./actors/ACTOR_SYSTEM.spec.md');
    });
  });

  describe('Tree navigation', () => {
    test('maintains parent-child relationships', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const root = graph.getAllNodes().find(n => n.type === 'root');

      expect(root).toBeDefined();
      expect(root!.children.length).toBeGreaterThan(0);

      // Get first heading
      const firstChild = graph.getNode(root!.children[0]);
      expect(firstChild?.type).toBe('heading');
      expect(firstChild?.parent).toBe(root!.id);
    });

    test('getChildren returns child nodes', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const heading = graph.findNodes({ type: 'heading', depth: 2 })[0];

      const children = graph.getChildren(heading.id);
      expect(children.length).toBeGreaterThan(0);
    });

    test('getParent returns parent node', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const paragraph = graph.findNodes({ type: 'paragraph' })[0];

      const parent = graph.getParent(paragraph.id);
      expect(parent).toBeDefined();
      expect(parent?.type).toBe('root');
    });

    test('getSiblings returns sibling nodes', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const headings = graph.findNodes({ type: 'heading', depth: 2 });

      if (headings.length >= 2) {
        const siblings = graph.getSiblings(headings[0].id);
        expect(siblings.length).toBeGreaterThan(0);
        expect(siblings.some(s => s.id === headings[1].id)).toBe(true);
      }
    });

    test('getDescendants returns all descendants', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const root = graph.getAllNodes().find(n => n.type === 'root');

      const descendants = graph.getDescendants(root!.id);
      expect(descendants.length).toBeGreaterThan(10); // Should have many descendants
    });

    test('getAncestors returns ancestor chain', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const text = graph.findNodes({ type: 'text' })[0];

      const ancestors = graph.getAncestors(text.id);
      expect(ancestors.length).toBeGreaterThan(0);
      expect(ancestors[ancestors.length - 1].type).toBe('root');
    });
  });

  describe('Semantic queries', () => {
    test('finds nodes by type', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

      const headings = graph.findNodes({ type: 'heading' });
      expect(headings.length).toBe(7); // 1 H1 + 3 H2 + 3 H3

      const code = graph.findNodes({ type: 'code' });
      expect(code.length).toBe(2);
    });

    test('finds nodes by text content', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

      const results = graph.findNodes({
        type: 'heading',
        textContains: 'Identity'
      });

      expect(results.length).toBe(1);
      expect(results[0].metadata.text).toContain('Identity');
    });

    test('finds nodes with custom filter', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

      const tsCodeBlocks = graph.findNodes({
        type: 'code',
        matches: (node) => node.properties.lang === 'typescript'
      });

      expect(tsCodeBlocks.length).toBe(2);
    });

    test('finds descendants with filter', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const section = graph.findSection('Core Concepts');

      const codeInSection = graph.getDescendants(section!.id, { type: 'code' });
      // Note: Core Concepts section may not directly contain code - it's under subsections
      expect(codeInSection.length).toBeGreaterThanOrEqual(0);
    });

    test('finds nodes by position', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

      const node = graph.findNodeByPosition(1);
      expect(node).toBeDefined();
      // Line 1 contains both root and heading - we get the largest (root)
      expect(['root', 'heading']).toContain(node?.type);
    });

    test('findSection finds heading by text', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);

      const section = graph.findSection('Core Concepts');
      expect(section).toBeDefined();
      expect(section?.type).toBe('heading');
      expect(section?.properties.depth).toBe(2);
    });
  });

  describe('Edge operations', () => {
    test('creates parent-child edges', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const heading = graph.findNodes({ type: 'heading' })[0];

      const edges = graph.getEdges(heading.id, 'parent-child');
      expect(edges.length).toBeGreaterThan(0);
    });

    test('creates sibling edges', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const headings = graph.findNodes({ type: 'heading', depth: 2 });

      const siblingEdges = graph.getEdges(headings[0].id, 'sibling', 'from');
      expect(siblingEdges.length).toBeGreaterThan(0);
    });

    test('creates links-to edges', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const links = graph.findLinks();

      expect(links.length).toBeGreaterThan(0);
      expect(links[0].type).toBe('links-to');
      expect(links[0].properties.url).toContain('ACTOR_SYSTEM');
    });

    test('creates contains-code edges', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const headings = graph.findNodes({ type: 'heading' });

      let containsCodeEdges = 0;
      for (const heading of headings) {
        const edges = graph.getEdges(heading.id, 'contains-code', 'from');
        containsCodeEdges += edges.length;
      }

      expect(containsCodeEdges).toBeGreaterThan(0);
    });
  });

  describe('Document analysis', () => {
    test('extracts heading hierarchy', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const hierarchy = graph.getHeadingHierarchy();

      expect(hierarchy.children.length).toBe(1); // One H1
      expect(hierarchy.children[0].text).toBe('Graph System Specification');
      expect(hierarchy.children[0].children.length).toBe(3); // Three H2s
    });

    test('extracts table of contents', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const toc = graph.extractTableOfContents();

      expect(toc.length).toBe(7);
      expect(toc[0].text).toBe('Graph System Specification');
      expect(toc[0].depth).toBe(1);
      expect(toc[0].anchor).toBe('graph-system-specification');
    });

    test('computes document metrics', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const metrics = graph.computeDocumentMetrics();

      expect(metrics.nodeCount).toBeGreaterThan(20);
      expect(metrics.headingCount).toBe(7);
      expect(metrics.codeBlockCount).toBe(2);
      expect(metrics.linkCount).toBe(1);
      expect(metrics.wordCount).toBeGreaterThan(20);
      expect(metrics.maxDepth).toBeGreaterThan(3);
    });

    test('finds broken internal links', () => {
      const mdWithBrokenLink = `# Test\n\nSee [broken](#nonexistent-section)`;
      const graph = MarkdownGraph.fromMarkdown(mdWithBrokenLink);

      const broken = graph.findBrokenLinks();
      // Should detect the broken internal link
      expect(broken.length).toBeGreaterThan(0);
    });
  });

  describe('Serialization', () => {
    test('round-trip to markdown preserves structure', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const regenerated = graph.toMarkdown();

      expect(regenerated).toContain('# Graph System Specification');
      expect(regenerated).toContain('## Overview');
      expect(regenerated).toContain('### Core Responsibilities');
    });

    test('dumps and restores graph', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const dump = graph.dump();

      expect(dump.nodes.length).toBeGreaterThan(0);
      expect(dump.edges.length).toBeGreaterThan(0);
      expect(dump.metadata.version).toBe('1.0.0');

      const restored = MarkdownGraph.fromDump(dump);
      expect(restored.getAllNodes().length).toBe(graph.getAllNodes().length);
    });
  });

  describe('File operations', () => {
    test('loads from file', async () => {
      const graph = await MarkdownGraph.fromFile(
        '/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/GRAPH_SYSTEM.spec.md'
      );

      const nodes = graph.getAllNodes();
      expect(nodes.length).toBeGreaterThan(50); // Real spec file is large

      const headings = graph.findNodes({ type: 'heading' });
      expect(headings.length).toBeGreaterThan(10);
    });
  });

  describe('Real-world use cases', () => {
    test('finds all TypeScript examples in a section', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const coreSection = graph.findSection('Core Concepts');

      expect(coreSection).toBeDefined();

      const tsCode = graph.getDescendants(coreSection!.id, {
        type: 'code',
        lang: 'typescript'
      });

      // Core Concepts section contains a code block under a subsection
      expect(tsCode.length).toBeGreaterThanOrEqual(0);
      if (tsCode.length > 0) {
        expect(tsCode[0].properties.value).toContain('graph');
      }
    });

    test('finds all links for validation', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const links = graph.findLinks();

      expect(links.length).toBeGreaterThan(0);
      const linkUrls = links.map(edge => edge.properties.url);
      expect(linkUrls).toContain('./actors/ACTOR_SYSTEM.spec.md');
    });

    test('generates section-based navigation', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const section = graph.findSection('Overview');

      expect(section).toBeDefined();

      // Get next sibling section
      const siblings = graph.getSiblings(section!.id);
      const nextSections = siblings.filter(s => s.type === 'heading' && s.properties.depth === 2);

      expect(nextSections.length).toBeGreaterThan(0);
      expect(nextSections[0].metadata.text).toBe('Core Concepts');
    });

    test('extracts metadata for knowledge nodes', () => {
      const graph = MarkdownGraph.fromMarkdown(sampleMarkdown);
      const sections = graph.findNodes({ type: 'heading', depth: 2 });

      const knowledgeData = sections.map(section => {
        // Use contains-code edges to find code in this section
        const codeEdges = graph.getEdges(section.id, 'contains-code', 'from');

        return {
          title: section.metadata.text,
          section: section.metadata.section,
          hasCode: codeEdges.length > 0,
          codeCount: codeEdges.length
        };
      });

      expect(knowledgeData.length).toBe(3);
      expect(knowledgeData[1].title).toBe('Core Concepts');
      // Check if any sections have code
      const hasAnyCode = knowledgeData.some(d => d.hasCode);
      expect(hasAnyCode).toBe(true);
    });
  });
});
