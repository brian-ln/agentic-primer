# Markdown Graph

Graph-based representation of markdown documents for structural analysis and semantic queries.

## Overview

The Markdown Graph module parses markdown documents into Abstract Syntax Trees (AST) and represents them as queryable graph structures. This enables:

- **Structural navigation** - Traverse heading hierarchies, find siblings, get ancestors
- **Semantic queries** - Find code blocks by language, search for content, locate links
- **Cross-document analysis** - Track references, validate links, build knowledge graphs
- **Round-trip serialization** - Parse to graph, modify, serialize back to markdown

## Quick Start

```typescript
import { MarkdownGraph } from './MarkdownGraph';

// Parse markdown
const graph = MarkdownGraph.fromMarkdown('# Hello\n\nWorld');

// Or load from file
const specGraph = await MarkdownGraph.fromFile('spec.md');

// Query nodes
const headings = graph.findNodes({ type: 'heading' });
const codeBlocks = graph.findNodes({ type: 'code', lang: 'typescript' });

// Navigate structure
const children = graph.getChildren(headingId);
const parent = graph.getParent(nodeId);

// Analyze document
const toc = graph.extractTableOfContents();
const metrics = graph.computeDocumentMetrics();

// Serialize back to markdown
const markdown = graph.toMarkdown();
```

## Key Concepts

### 1. Nodes

Every markdown element becomes a graph node:

```typescript
interface MarkdownNode {
  id: string;                    // Unique node ID
  type: MarkdownNodeType;        // Node type (heading, paragraph, code, etc.)
  properties: {...};             // Type-specific properties
  position?: Position;           // Source position (line, column)
  metadata: {
    document?: string;           // Source file
    section?: string;            // Containing section name
    depth: number;               // Tree depth
    text?: string;               // Text content (cached)
  };
  children: string[];            // Child node IDs
  parent?: string;               // Parent node ID
}
```

### 2. Edges

Relationships between nodes:

```typescript
type EdgeType =
  | 'parent-child'      // AST containment
  | 'sibling'           // Sequential order
  | 'links-to'          // Link references
  | 'section-of'        // Heading to document
  | 'contains-code'     // Section has code
  | 'defines'           // Section defines concept
  | 'implements'        // Code implements spec
  | 'references';       // General reference
```

### 3. Section Tracking

Nodes track which section they belong to:

```markdown
## Core Concepts         <- Section heading

Some text here          <- section = "Core Concepts"

\`\`\`typescript
code                    <- section = "Core Concepts"
\`\`\`

### Subsection          <- section = "Core Concepts"
```

This enables finding all content in a section, even though markdown has a flat AST structure.

## API Reference

### Construction

```typescript
// From markdown string
MarkdownGraph.fromMarkdown(markdown: string, documentPath?: string): MarkdownGraph

// From file
MarkdownGraph.fromFile(filePath: string): Promise<MarkdownGraph>

// From dump
MarkdownGraph.fromDump(dump: GraphDump): MarkdownGraph
```

### Node Operations

```typescript
// Get node by ID
getNode(id: string): MarkdownNode | null

// Get all nodes
getAllNodes(): MarkdownNode[]

// Find nodes by filter
findNodes(filter: NodeFilter): MarkdownNode[]

// Find by position
findNodeByPosition(line: number, column?: number): MarkdownNode | null

// Find section by heading text
findSection(heading: string): MarkdownNode | null
```

### Navigation

```typescript
// Get children
getChildren(nodeId: string): MarkdownNode[]

// Get parent
getParent(nodeId: string): MarkdownNode | null

// Get siblings
getSiblings(nodeId: string): MarkdownNode[]

// Get all descendants
getDescendants(nodeId: string, filter?: NodeFilter): MarkdownNode[]

// Get all ancestors
getAncestors(nodeId: string): MarkdownNode[]
```

### Edge Operations

```typescript
// Add edge
addEdge(edge: Omit<MarkdownEdge, 'id'>): MarkdownEdge

// Get edges
getEdges(nodeId: string, type?: EdgeType, direction?: 'from' | 'to' | 'both'): MarkdownEdge[]

// Find all links
findLinks(fromNodeId?: string): MarkdownEdge[]
```

### Analysis

```typescript
// Get heading hierarchy
getHeadingHierarchy(): HeadingNode

// Extract table of contents
extractTableOfContents(): TOCEntry[]

// Find broken internal links
findBrokenLinks(): MarkdownEdge[]

// Compute metrics
computeDocumentMetrics(): DocumentMetrics
```

### Serialization

```typescript
// Convert to markdown
toMarkdown(): string

// Dump to JSON
dump(): GraphDump
```

## Node Filter

```typescript
interface NodeFilter {
  type?: MarkdownNodeType;           // Match node type
  depth?: number;                     // Match heading depth
  lang?: string;                      // Match code language
  textContains?: string;              // Text includes string
  matches?: (node: MarkdownNode) => boolean;  // Custom filter
}
```

Examples:

```typescript
// Find all H2 headings
graph.findNodes({ type: 'heading', depth: 2 })

// Find TypeScript code
graph.findNodes({ type: 'code', lang: 'typescript' })

// Find headings containing "API"
graph.findNodes({
  type: 'heading',
  textContains: 'API'
})

// Custom filter
graph.findNodes({
  type: 'code',
  matches: (node) => node.properties.value.includes('async')
})
```

## Examples

### Extract Code Examples

```typescript
const graph = await MarkdownGraph.fromFile('API.md');

// Find all TypeScript examples
const examples = graph.findNodes({
  type: 'code',
  lang: 'typescript'
});

for (const example of examples) {
  console.log(`Section: ${example.metadata.section}`);
  console.log(example.properties.value);
}
```

### Build Table of Contents

```typescript
const toc = graph.extractTableOfContents();

for (const entry of toc) {
  const indent = '  '.repeat(entry.depth - 1);
  console.log(`${indent}- [${entry.text}](#${entry.anchor})`);
}
```

### Find Broken Links

```typescript
const broken = graph.findBrokenLinks();

for (const link of broken) {
  const sourceNode = graph.getNode(link.fromId);
  console.log(`Broken link in section: ${sourceNode?.metadata.section}`);
  console.log(`  URL: ${link.properties.url}`);
}
```

### Extract Section Content

```typescript
const section = graph.findSection('Installation');

// Find all code in this section
const allCode = graph.findNodes({ type: 'code' });
const codeInSection = allCode.filter(
  c => c.metadata.section === 'Installation'
);

// Or use contains-code edges
const codeEdges = graph.getEdges(section.id, 'contains-code', 'from');
```

### Validate Cross-References

```typescript
const links = graph.findLinks();

for (const link of links) {
  const url = link.properties.url;

  if (url.startsWith('./')) {
    // Check if target file exists
    const targetPath = resolvePath(url);
    if (!await fileExists(targetPath)) {
      console.log(`Missing file: ${url}`);
    }
  }
}
```

## Architecture

```
┌─────────────────────────────────────────┐
│         Application Layer               │
│    (Analysis, Validation, Export)       │
├─────────────────────────────────────────┤
│       MarkdownGraph API                 │
│  - Node operations                      │
│  - Navigation methods                   │
│  - Query interface                      │
│  - Analysis functions                   │
├─────────────────────────────────────────┤
│      Graph Representation               │
│  - Map<id, MarkdownNode>                │
│  - Map<id, MarkdownEdge>                │
│  - Section tracking                     │
│  - Position indexing                    │
├─────────────────────────────────────────┤
│        Remark/Unified                   │
│  - Parse markdown → AST                 │
│  - Serialize AST → markdown             │
│  - Position tracking                    │
└─────────────────────────────────────────┘
```

## Performance

- **Node lookup**: O(1) via Map
- **Edge queries**: O(e) where e = total edges
- **Tree traversal**: O(n) where n = descendants
- **Find by position**: O(n) linear scan

For large documents (1000+ nodes), consider:
- Caching query results
- Building position indexes
- Limiting traversal depth

## Testing

```bash
# Run tests
bun test src/markdown-graph/MarkdownGraph.test.ts

# Run demo
bun src/markdown-graph/demo.ts
```

## Integration

See [MARKDOWN_GRAPH_INTEGRATION.md](../../MARKDOWN_GRAPH_INTEGRATION.md) for integration with tk-agents.

## References

- [MD_AS_GRAPH_ANALYSIS.md](../../MD_AS_GRAPH_ANALYSIS.md) - Full analysis and concepts
- [MARKDOWN_GRAPH_INTEGRATION.md](../../MARKDOWN_GRAPH_INTEGRATION.md) - Integration proposal
- [remark](https://github.com/remarkjs/remark) - Markdown processor
- [unified](https://github.com/unifiedjs/unified) - Content processing
