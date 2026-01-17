/**
 * MarkdownGraph - Graph representation of markdown documents
 *
 * Parses markdown into AST and provides graph operations for:
 * - Structural navigation
 * - Semantic queries
 * - Cross-document linking
 * - Round-trip serialization
 */

import { unified } from 'unified';
import remarkParse from 'remark-parse';
import remarkStringify from 'remark-stringify';
import { visit } from 'unist-util-visit';
import { toString } from 'mdast-util-to-string';
import type { Root, Node as MdastNode } from 'mdast';
import type { Position } from 'unist';

import type {
  MarkdownNode,
  MarkdownEdge,
  NodeFilter,
  HeadingNode,
  TOCEntry,
  DocumentMetrics,
  GraphDump,
  EdgeType,
  MarkdownNodeType
} from './types';

export class MarkdownGraph {
  private nodes: Map<string, MarkdownNode> = new Map();
  private edges: Map<string, MarkdownEdge> = new Map();
  private edgeCounter: number = 0;
  private document?: string;
  private rootId?: string;

  /**
   * Parse markdown text into graph
   */
  static fromMarkdown(markdown: string, documentPath?: string): MarkdownGraph {
    const graph = new MarkdownGraph();
    graph.document = documentPath;

    const processor = unified().use(remarkParse);
    const ast = processor.parse(markdown) as Root;

    // Convert AST to graph
    graph.buildFromAST(ast);

    return graph;
  }

  /**
   * Parse markdown file into graph
   */
  static async fromFile(filePath: string): Promise<MarkdownGraph> {
    const content = await Bun.file(filePath).text();
    return MarkdownGraph.fromMarkdown(content, filePath);
  }

  /**
   * Build graph from AST
   */
  private buildFromAST(ast: Root): void {
    let nodeIdCounter = 0;
    const parentStack: string[] = [];
    let currentSectionStack: Array<{ heading: string; depth: number }> = [];

    // Convert AST node to graph node
    const convertNode = (astNode: MdastNode, depth: number): string => {
      const nodeId = `node_${nodeIdCounter++}`;

      // Extract properties based on node type
      const properties: any = {};
      const type = astNode.type as MarkdownNodeType;

      // Type-specific properties
      if (type === 'heading') {
        properties.depth = (astNode as any).depth;
      } else if (type === 'code') {
        properties.lang = (astNode as any).lang || undefined;
        properties.value = (astNode as any).value;
      } else if (type === 'inlineCode') {
        properties.value = (astNode as any).value;
      } else if (type === 'text') {
        properties.value = (astNode as any).value;
      } else if (type === 'link' || type === 'image') {
        properties.url = (astNode as any).url;
        properties.title = (astNode as any).title || undefined;
        if (type === 'image') {
          properties.alt = (astNode as any).alt || undefined;
        }
      } else if (type === 'list') {
        properties.ordered = (astNode as any).ordered || false;
        properties.start = (astNode as any).start || undefined;
      } else if (type === 'listItem') {
        properties.checked = (astNode as any).checked ?? null;
      } else if (type === 'html') {
        properties.value = (astNode as any).value;
      }

      // Get text content
      const text = toString(astNode);

      // Update section stack for headings
      if (type === 'heading') {
        const headingDepth = properties.depth;
        // Pop sections with equal or greater depth
        while (currentSectionStack.length > 0 &&
               currentSectionStack[currentSectionStack.length - 1].depth >= headingDepth) {
          currentSectionStack.pop();
        }
        // Push this heading
        currentSectionStack.push({ heading: text, depth: headingDepth });
      }

      // Create graph node
      const node: MarkdownNode = {
        id: nodeId,
        type,
        properties,
        position: astNode.position,
        metadata: {
          document: this.document,
          depth,
          text: text || undefined,
          // Set section to most recent heading
          section: currentSectionStack.length > 0
            ? currentSectionStack[currentSectionStack.length - 1].heading
            : undefined,
        },
        children: [],
        parent: parentStack.length > 0 ? parentStack[parentStack.length - 1] : undefined,
      };

      this.nodes.set(nodeId, node);

      // Add parent-child edge
      if (node.parent) {
        const parentNode = this.nodes.get(node.parent);
        if (parentNode) {
          parentNode.children.push(nodeId);
          this.addEdge({
            fromId: node.parent,
            toId: nodeId,
            type: 'parent-child',
            properties: {}
          });
        }
      }

      // Process children
      if ('children' in astNode && Array.isArray(astNode.children)) {
        parentStack.push(nodeId);
        for (const child of astNode.children) {
          convertNode(child, depth + 1);
        }
        parentStack.pop();
      }

      return nodeId;
    };

    // Start conversion
    this.rootId = convertNode(ast, 0);

    // Add sibling edges
    this.addSiblingEdges();

    // Add semantic edges (links, sections, etc.)
    this.addSemanticEdges();
  }

  /**
   * Add sibling edges between sequential nodes
   */
  private addSiblingEdges(): void {
    for (const node of this.nodes.values()) {
      for (let i = 0; i < node.children.length - 1; i++) {
        const fromId = node.children[i];
        const toId = node.children[i + 1];
        this.addEdge({
          fromId,
          toId,
          type: 'sibling',
          properties: { bidirectional: false }
        });
      }
    }
  }

  /**
   * Add semantic edges (links, sections, etc.)
   */
  private addSemanticEdges(): void {
    // Add section-of edges for headings
    for (const node of this.nodes.values()) {
      if (node.type === 'heading' && this.document) {
        this.addEdge({
          fromId: node.id,
          toId: 'document',
          type: 'section-of',
          properties: { document: this.document }
        });
      }

      // Add links-to edges for links
      // Note: link nodes exist in the tree, so we track them as edges pointing to external targets
      if (node.type === 'link' && node.properties.url) {
        // Find the link's parent to establish context
        const parentId = node.parent || node.id;
        this.addEdge({
          fromId: parentId,
          toId: node.properties.url,
          type: 'links-to',
          properties: { url: node.properties.url, linkNodeId: node.id }
        });
      }

      // Add contains-code edges for sections with code
      // Note: In markdown, blocks are flat siblings, not nested under headings in AST
      // So we need to find the *preceding* heading to establish section containment
      if (node.type === 'code') {
        const currentSection = node.metadata.section;
        if (currentSection) {
          // Find the heading node for this section
          const headingNode = Array.from(this.nodes.values()).find(
            n => n.type === 'heading' && n.metadata.text === currentSection
          );
          if (headingNode) {
            this.addEdge({
              fromId: headingNode.id,
              toId: node.id,
              type: 'contains-code',
              properties: { lang: node.properties.lang }
            });
          }
        }
      }
    }
  }

  /**
   * Add an edge to the graph
   */
  addEdge(edge: Omit<MarkdownEdge, 'id'>): MarkdownEdge {
    const id = `edge_${this.edgeCounter++}`;
    const fullEdge = { id, ...edge };
    this.edges.set(id, fullEdge);
    return fullEdge;
  }

  /**
   * Get node by ID
   */
  getNode(id: string): MarkdownNode | null {
    return this.nodes.get(id) || null;
  }

  /**
   * Get all nodes
   */
  getAllNodes(): MarkdownNode[] {
    return Array.from(this.nodes.values());
  }

  /**
   * Get edges for a node
   */
  getEdges(nodeId: string, type?: EdgeType, direction: 'from' | 'to' | 'both' = 'both'): MarkdownEdge[] {
    const edges: MarkdownEdge[] = [];

    for (const edge of this.edges.values()) {
      const matchesType = !type || edge.type === type;
      const matchesFrom = direction === 'from' || direction === 'both' ? edge.fromId === nodeId : false;
      const matchesTo = direction === 'to' || direction === 'both' ? edge.toId === nodeId : false;

      if (matchesType && (matchesFrom || matchesTo)) {
        edges.push(edge);
      }
    }

    return edges;
  }

  /**
   * Get children of a node
   */
  getChildren(nodeId: string): MarkdownNode[] {
    const node = this.nodes.get(nodeId);
    if (!node) return [];

    return node.children
      .map(id => this.nodes.get(id))
      .filter((n): n is MarkdownNode => n !== undefined);
  }

  /**
   * Get parent of a node
   */
  getParent(nodeId: string): MarkdownNode | null {
    const node = this.nodes.get(nodeId);
    if (!node?.parent) return null;
    return this.nodes.get(node.parent) || null;
  }

  /**
   * Get siblings of a node
   */
  getSiblings(nodeId: string): MarkdownNode[] {
    const node = this.nodes.get(nodeId);
    if (!node?.parent) return [];

    const parent = this.nodes.get(node.parent);
    if (!parent) return [];

    return parent.children
      .filter(id => id !== nodeId)
      .map(id => this.nodes.get(id))
      .filter((n): n is MarkdownNode => n !== undefined);
  }

  /**
   * Get all descendants of a node
   */
  getDescendants(nodeId: string, filter?: NodeFilter): MarkdownNode[] {
    const node = this.nodes.get(nodeId);
    if (!node) return [];

    const descendants: MarkdownNode[] = [];
    const queue = [...node.children];

    while (queue.length > 0) {
      const childId = queue.shift()!;
      const child = this.nodes.get(childId);
      if (!child) continue;

      if (this.matchesFilter(child, filter)) {
        descendants.push(child);
      }

      queue.push(...child.children);
    }

    return descendants;
  }

  /**
   * Get all ancestors of a node
   */
  getAncestors(nodeId: string): MarkdownNode[] {
    const ancestors: MarkdownNode[] = [];
    let current = this.nodes.get(nodeId);

    while (current?.parent) {
      const parent = this.nodes.get(current.parent);
      if (!parent) break;
      ancestors.push(parent);
      current = parent;
    }

    return ancestors;
  }

  /**
   * Find nodes matching filter
   */
  findNodes(filter: NodeFilter): MarkdownNode[] {
    const results: MarkdownNode[] = [];

    for (const node of this.nodes.values()) {
      if (this.matchesFilter(node, filter)) {
        results.push(node);
      }
    }

    return results;
  }

  /**
   * Find node by position
   */
  findNodeByPosition(line: number, column?: number): MarkdownNode | null {
    for (const node of this.nodes.values()) {
      if (!node.position) continue;

      const { start, end } = node.position;
      if (line >= start.line && line <= end.line) {
        if (column !== undefined) {
          if (line === start.line && column < start.column) continue;
          if (line === end.line && column > end.column) continue;
        }
        return node;
      }
    }

    return null;
  }

  /**
   * Find section by heading text
   */
  findSection(heading: string): MarkdownNode | null {
    for (const node of this.nodes.values()) {
      if (node.type === 'heading' && node.metadata.text === heading) {
        return node;
      }
    }
    return null;
  }

  /**
   * Find all links in the document
   */
  findLinks(fromNodeId?: string): MarkdownEdge[] {
    if (fromNodeId) {
      return this.getEdges(fromNodeId, 'links-to', 'from');
    }

    // Get all links-to edges
    const allLinks: MarkdownEdge[] = [];
    for (const edge of this.edges.values()) {
      if (edge.type === 'links-to') {
        allLinks.push(edge);
      }
    }
    return allLinks;
  }

  /**
   * Get heading hierarchy
   */
  getHeadingHierarchy(): HeadingNode {
    const rootHeading: HeadingNode = {
      id: 'root',
      text: this.document || 'Document',
      depth: 0,
      children: []
    };

    const headings = this.findNodes({ type: 'heading' });
    const stack: HeadingNode[] = [rootHeading];

    for (const node of headings) {
      const depth = node.properties.depth || 1;
      const heading: HeadingNode = {
        id: node.id,
        text: node.metadata.text || '',
        depth,
        children: []
      };

      // Pop stack until we find the parent level
      while (stack.length > 1 && stack[stack.length - 1].depth >= depth) {
        stack.pop();
      }

      // Add to parent
      stack[stack.length - 1].children.push(heading);
      stack.push(heading);
    }

    return rootHeading;
  }

  /**
   * Extract table of contents
   */
  extractTableOfContents(): TOCEntry[] {
    const headings = this.findNodes({ type: 'heading' });
    return headings.map(node => ({
      text: node.metadata.text || '',
      depth: node.properties.depth || 1,
      anchor: this.slugify(node.metadata.text || ''),
      position: node.position
    }));
  }

  /**
   * Find broken links (links to non-existent nodes/files)
   */
  findBrokenLinks(): MarkdownEdge[] {
    const linkEdges = this.findLinks();
    const broken: MarkdownEdge[] = [];

    for (const edge of linkEdges) {
      // Check if link is internal (starts with #)
      const url = edge.properties.url as string;
      if (url?.startsWith('#')) {
        const anchor = url.substring(1);
        const target = this.findNodes({
          type: 'heading',
          matches: (node) => this.slugify(node.metadata.text || '') === anchor
        });
        if (target.length === 0) {
          broken.push(edge);
        }
      }
      // Note: External file validation requires file system access
    }

    return broken;
  }

  /**
   * Compute document metrics
   */
  computeDocumentMetrics(): DocumentMetrics {
    const nodes = Array.from(this.nodes.values());

    return {
      nodeCount: nodes.length,
      edgeCount: this.edges.size,
      headingCount: nodes.filter(n => n.type === 'heading').length,
      codeBlockCount: nodes.filter(n => n.type === 'code').length,
      linkCount: nodes.filter(n => n.type === 'link').length,
      wordCount: this.countWords(),
      maxDepth: Math.max(...nodes.map(n => n.metadata.depth))
    };
  }

  /**
   * Serialize graph back to markdown
   */
  toMarkdown(): string {
    if (!this.rootId) return '';

    const root = this.nodes.get(this.rootId);
    if (!root) return '';

    // Reconstruct AST from graph
    const ast = this.reconstructAST(root);

    // Serialize to markdown
    const processor = unified().use(remarkStringify);
    return processor.stringify(ast as Root);
  }

  /**
   * Dump graph to JSON
   */
  dump(): GraphDump {
    return {
      nodes: Array.from(this.nodes.values()),
      edges: Array.from(this.edges.values()),
      metadata: {
        document: this.document,
        timestamp: new Date().toISOString(),
        version: '1.0.0'
      }
    };
  }

  /**
   * Load graph from dump
   */
  static fromDump(dump: GraphDump): MarkdownGraph {
    const graph = new MarkdownGraph();
    graph.document = dump.metadata.document;

    for (const node of dump.nodes) {
      graph.nodes.set(node.id, node);
      if (node.type === 'root') {
        graph.rootId = node.id;
      }
    }

    for (const edge of dump.edges) {
      graph.edges.set(edge.id, edge);
      const edgeNum = parseInt(edge.id.replace('edge_', ''));
      graph.edgeCounter = Math.max(graph.edgeCounter, edgeNum + 1);
    }

    return graph;
  }

  // Helper methods

  private matchesFilter(node: MarkdownNode, filter?: NodeFilter): boolean {
    if (!filter) return true;

    if (filter.type && node.type !== filter.type) return false;
    if (filter.depth !== undefined && node.properties.depth !== filter.depth) return false;
    if (filter.lang && node.properties.lang !== filter.lang) return false;
    if (filter.textContains && !node.metadata.text?.includes(filter.textContains)) return false;
    if (filter.matches && !filter.matches(node)) return false;

    return true;
  }

  private countWords(): number {
    let count = 0;
    for (const node of this.nodes.values()) {
      if (node.type === 'text' && node.properties.value) {
        count += node.properties.value.split(/\s+/).filter(w => w.length > 0).length;
      }
    }
    return count;
  }

  private slugify(text: string): string {
    return text
      .toLowerCase()
      .replace(/[^\w\s-]/g, '')
      .replace(/\s+/g, '-')
      .replace(/-+/g, '-')
      .trim();
  }

  private reconstructAST(node: MarkdownNode): MdastNode {
    const astNode: any = {
      type: node.type,
      ...node.properties
    };

    if (node.position) {
      astNode.position = node.position;
    }

    // Reconstruct children
    if (node.children.length > 0) {
      astNode.children = node.children
        .map(id => this.nodes.get(id))
        .filter((n): n is MarkdownNode => n !== undefined)
        .map(n => this.reconstructAST(n));
    }

    return astNode;
  }
}
