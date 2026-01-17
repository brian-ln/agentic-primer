# Markdown as Graph/Tree: Analysis and Concepts

**Author:** Background Research Agent
**Date:** 2026-01-16
**Project:** tk-agents

## Executive Summary

Markdown documents are inherently hierarchical structures that map naturally to tree/graph representations. By treating markdown as an Abstract Syntax Tree (AST) rather than linear text, we unlock powerful capabilities for:

- **Structural navigation** - Jump between sections, traverse heading hierarchies
- **Semantic queries** - Find all code blocks, extract all links, locate definitions
- **Cross-referencing** - Track document relationships, build knowledge graphs
- **Intelligent editing** - Modify structure without string manipulation
- **Version control aware** - Diff at semantic level, not line level

This analysis explores how markdown maps to graphs, the benefits of this approach, and how it integrates with the tk-agents system.

## 1. Markdown Structure as Tree/Graph

### 1.1 The Markdown AST

When parsed, markdown documents become Abstract Syntax Trees (AST) with the following structure:

```
Root Node
  ├── Heading (depth: 1)
  │   └── Text ("Title")
  ├── Paragraph
  │   ├── Text ("Some content")
  │   └── Link (url: "...")
  │       └── Text ("link text")
  ├── Heading (depth: 2)
  │   └── Text ("Subsection")
  ├── List (ordered: false)
  │   ├── ListItem
  │   │   └── Paragraph
  │   │       └── Text ("Item 1")
  │   └── ListItem
  │       └── Paragraph
  │           └── Text ("Item 2")
  └── Code (lang: "typescript")
      └── Text ("const x = 1;")
```

**Key Insight:** Every markdown element is a node in a tree structure with parent-child relationships, position metadata, and semantic type information.

### 1.2 Node Types in Markdown AST

Based on analysis of remark/unified parser, markdown ASTs contain these node types:

| Node Type | Purpose | Children | Key Properties |
|-----------|---------|----------|----------------|
| `root` | Document root | Multiple | - |
| `heading` | Section headers | Inline content | `depth` (1-6) |
| `paragraph` | Text block | Inline content | - |
| `text` | Leaf text | None | `value` (string) |
| `list` | Ordered/unordered list | `listItem` nodes | `ordered`, `start` |
| `listItem` | List entry | Block content | `checked` (for tasks) |
| `code` | Code block | None | `lang`, `value` |
| `inlineCode` | Inline code | None | `value` |
| `link` | Hyperlink | Inline content | `url`, `title` |
| `strong` | Bold text | Inline content | - |
| `emphasis` | Italic text | Inline content | - |
| `blockquote` | Quote block | Block content | - |
| `table` | Table structure | `tableRow` nodes | `align` |
| `image` | Image reference | None | `url`, `alt`, `title` |

**Position Metadata:** Every node includes `position` with:
- `start` - `{ line, column, offset }`
- `end` - `{ line, column, offset }`

This enables precise source mapping for editing and error reporting.

### 1.3 Tree to Graph Transformation

While markdown is naturally a tree (parent-child relationships), we can enhance it to a graph by adding cross-reference edges:

**Tree Edges (implicit in AST):**
- `parent-child` - Structural containment
- `sibling` - Sequential relationship (derived from order)

**Graph Edges (semantic relationships):**
- `links-to` - Link references another document/section
- `section-of` - Heading belongs to document
- `contains-code` - Section contains code examples
- `defines` - Section defines a term/concept
- `implements` - Code block implements a specification
- `references` - General cross-reference

**Example transformation:**

```
# Task System Spec              (node: heading-1)
                                     ↓ section-of
See [GRAPH_SYSTEM](...)         (node: link-1)
                                     ↓ links-to
                                (external: GRAPH_SYSTEM.spec.md)

## Core Concepts                (node: heading-2)
                                     ↓ parent-of
### State Machine               (node: heading-3)
```

## 2. Benefits of Graph-Based Markdown

### 2.1 Structural Navigation

**Problem with text-based:** Finding "the next H2 heading after line 50" requires regex or manual scanning.

**With graph:**
```typescript
// Find current section
const section = graph.findNodeByPosition(line: 50, type: "heading");

// Navigate to next sibling heading
const nextSection = graph.getNextSibling(section, depth: 2);

// Get parent section
const parentSection = graph.getParent(section, type: "heading", maxDepth: 2);

// Get all subsections
const subsections = graph.getChildren(section, type: "heading");
```

**Use cases:**
- Table of contents generation
- Section folding/expansion in editors
- "Jump to next section" navigation
- Breadcrumb trails

### 2.2 Semantic Queries

**Problem with text-based:** "Find all TypeScript code blocks in the 'Core Concepts' section" is complex regex + line counting.

**With graph:**
```typescript
// Find section
const coreSection = graph.findNode({
  type: "heading",
  text: "Core Concepts"
});

// Find all code blocks in that section
const codeBlocks = graph.findDescendants(coreSection, {
  type: "code",
  lang: "typescript"
});

// Extract code snippets
const snippets = codeBlocks.map(node => node.value);
```

**Use cases:**
- Extract all links for validation
- Find all TODOs/FIXMEs in comments
- Collect all examples of a pattern
- Generate code index from spec files
- Find all tables with specific headers

### 2.3 Cross-Document References

**Problem with text-based:** Link URLs are just strings; no way to track what links to what.

**With graph:**
```typescript
// Build cross-document graph
const docGraph = new DocumentGraph();
docGraph.addDocument("GRAPH_SYSTEM.spec.md", graphAST);
docGraph.addDocument("TASK_SYSTEM.spec.md", taskAST);

// Find all documents that reference GRAPH_SYSTEM
const inboundLinks = docGraph.getInboundLinks("GRAPH_SYSTEM.spec.md");

// Find orphaned documents (no inbound links)
const orphans = docGraph.findOrphans();

// Build dependency graph
const deps = docGraph.buildDependencyGraph();
```

**Use cases:**
- Documentation dependency analysis
- "What links here" for documentation
- Detect broken internal links
- Generate documentation site maps
- Find circular references in specs

### 2.4 Intelligent Editing

**Problem with text-based:** Renaming a section requires updating heading text AND all internal links to that section.

**With graph:**
```typescript
// Rename section
graph.updateNode(sectionNode, { text: "New Section Name" });

// Automatically update all links pointing to this section
graph.updateInboundLinks(sectionNode, {
  text: "New Section Name",
  anchor: "new-section-name"
});

// Serialize back to markdown
const updatedMarkdown = graph.toMarkdown();
```

**Use cases:**
- Refactor section names consistently
- Move sections between documents
- Split documents while maintaining links
- Merge duplicate sections
- Reorder content structurally

### 2.5 Version Control Aware

**Problem with text-based:** Git diffs show line changes, not semantic changes.

**With graph:**
```typescript
// Parse both versions
const beforeAST = parseMarkdown(beforeText);
const afterAST = parseMarkdown(afterText);

// Semantic diff
const diff = computeSemanticDiff(beforeAST, afterAST);
// Returns: [
//   { type: "section-added", heading: "New Feature" },
//   { type: "code-modified", section: "API Reference", lang: "typescript" },
//   { type: "link-removed", url: "deprecated.md" }
// ]
```

**Use cases:**
- Human-readable change summaries
- Track documentation completeness
- Detect API changes in examples
- Identify removed/added sections
- Semantic merge conflict resolution

## 3. Graph Representation Design

### 3.1 Node Schema

Every markdown element becomes a graph node with:

```typescript
interface MarkdownNode {
  id: string;              // Unique node ID
  type: MarkdownNodeType;  // Node type (heading, paragraph, etc.)
  properties: {
    // Type-specific properties
    depth?: number;        // For headings
    lang?: string;         // For code blocks
    url?: string;          // For links/images
    value?: string;        // For text/code
    ordered?: boolean;     // For lists
    // ... more type-specific
  };
  position: {
    start: { line: number; column: number; offset: number };
    end: { line: number; column: number; offset: number };
  };
  metadata: {
    document: string;      // Source document path
    section?: string;      // Parent section name
    depth: number;         // Tree depth
  };
}
```

### 3.2 Edge Schema

Relationships between nodes:

```typescript
interface MarkdownEdge {
  id: string;
  fromId: string;          // Source node
  toId: string;            // Target node
  type: EdgeType;          // Relationship type
  properties: {
    // Edge-specific properties
    label?: string;        // For display
    weight?: number;       // For ranking
    bidirectional?: boolean;
  };
}

type EdgeType =
  | "parent-child"         // Structural containment (tree)
  | "sibling"              // Sequential order
  | "links-to"             // Link references
  | "section-of"           // Heading to document
  | "contains-code"        // Section has code
  | "defines"              // Section defines concept
  | "implements"           // Code implements spec
  | "references"           // General reference
  | "depends-on";          // Semantic dependency
```

### 3.3 Graph Operations

Core operations for markdown graphs:

```typescript
interface MarkdownGraph {
  // Node operations
  addNode(node: MarkdownNode): void;
  getNode(id: string): MarkdownNode | null;
  removeNode(id: string): void;
  updateNode(id: string, updates: Partial<MarkdownNode>): void;

  // Edge operations
  addEdge(edge: MarkdownEdge): void;
  getEdges(nodeId: string, type?: EdgeType): MarkdownEdge[];
  removeEdge(edgeId: string): void;

  // Traversal
  getChildren(nodeId: string): MarkdownNode[];
  getParent(nodeId: string): MarkdownNode | null;
  getSiblings(nodeId: string): MarkdownNode[];
  getDescendants(nodeId: string, filter?: NodeFilter): MarkdownNode[];
  getAncestors(nodeId: string): MarkdownNode[];

  // Queries
  findNodes(filter: NodeFilter): MarkdownNode[];
  findNodeByPosition(position: Position): MarkdownNode | null;
  findSection(heading: string): MarkdownNode | null;
  findLinks(fromNodeId?: string): MarkdownEdge[];

  // Cross-document
  linkDocument(docPath: string, graph: MarkdownGraph): void;
  resolveLink(link: MarkdownEdge): MarkdownNode | null;
  getDocumentReferences(docPath: string): MarkdownEdge[];

  // Serialization
  toMarkdown(): string;
  dump(): { nodes: MarkdownNode[]; edges: MarkdownEdge[] };

  // Analysis
  getHeadingHierarchy(): HeadingTree;
  extractTableOfContents(): TOCEntry[];
  findBrokenLinks(): MarkdownEdge[];
  computeDocumentMetrics(): DocumentMetrics;
}
```

## 4. Use Cases in tk-agents

### 4.1 Specification Cross-Referencing

**Current state:** We have multiple `.spec.md` files that reference each other.

**With markdown graph:**

```typescript
// Load all specs into graph
const specGraph = new MultiDocumentGraph();
specGraph.addDocument("GRAPH_SYSTEM.spec.md", ...);
specGraph.addDocument("TASK_SYSTEM.spec.md", ...);
specGraph.addDocument("KNOWLEDGE_SYSTEM.spec.md", ...);

// Find all specs that reference Graph System
const refs = specGraph.findReferences("GRAPH_SYSTEM");

// Build dependency visualization
const deps = specGraph.buildDependencyGraph();
// Returns graph showing which specs depend on which

// Validate all links work
const brokenLinks = specGraph.validateAllLinks();
```

### 4.2 Knowledge Node Extraction

**Goal:** Extract structured knowledge from markdown documents into knowledge nodes.

```typescript
// Parse spec document
const specGraph = MarkdownGraph.fromFile("GRAPH_SYSTEM.spec.md");

// Extract sections as knowledge nodes
const sections = specGraph.findNodes({ type: "heading", depth: 2 });

for (const section of sections) {
  const sectionContent = specGraph.getDescendantText(section);
  const codeExamples = specGraph.getDescendants(section, { type: "code" });

  // Create knowledge node
  const knowledgeNode = KnowledgeActor({
    id: `knowledge-${slugify(section.text)}`,
    title: section.text,
    content: sectionContent,
    sources: [{ file: "GRAPH_SYSTEM.spec.md", section: section.text }],
    examples: codeExamples.map(c => c.value),
    system: graph.getSystem()
  });

  // Register in graph
  graph.registerNode(knowledgeNode.id, knowledgeNode, ...);
}
```

### 4.3 Task Generation from Specifications

**Goal:** Generate tasks from TODO items or specification sections.

```typescript
// Find all TODO comments in code blocks
const todos = specGraph.findDescendants(null, {
  type: "code",
  matches: (node) => node.value.includes("// TODO:")
});

// Create task for each TODO
for (const todo of todos) {
  const section = specGraph.findAncestor(todo, { type: "heading" });
  const todoText = extractTodoText(todo.value);

  // Create task node
  const taskNode = TaskActor({
    id: `task-${generateId()}`,
    goal: todoText,
    context: { specSection: section.text },
    system: graph.getSystem()
  });

  // Link task to knowledge
  graph.addEdge(taskNode.id, `knowledge-${slugify(section.text)}`, "requires_knowledge");
}
```

### 4.4 Documentation-Driven Development

**Goal:** Keep code and specs in sync by validating examples.

```typescript
// Extract all TypeScript examples from specs
const examples = specGraph.findNodes({
  type: "code",
  lang: "typescript"
});

// Validate each example compiles
for (const example of examples) {
  const section = specGraph.findAncestor(example, { type: "heading" });

  try {
    await validateTypeScript(example.value);
  } catch (error) {
    // Create task to fix invalid example
    createTask({
      goal: `Fix invalid TypeScript in ${section.text}`,
      priority: "high",
      blockedBy: error
    });
  }
}
```

### 4.5 Semantic Search Across Documents

**Goal:** Query across all documentation using structural queries.

```typescript
// Find all mentions of "Actor System" in any heading
const actorSections = docGraph.query({
  nodeType: "heading",
  textContains: "Actor System"
});

// Find all code examples showing message sending
const messagingExamples = docGraph.query({
  nodeType: "code",
  inSection: (section) => section.text.includes("Message"),
  codeContains: ".send("
});

// Build concept map
const conceptMap = docGraph.buildConceptMap([
  "Actor System",
  "Graph System",
  "Task System"
]);
// Returns graph showing how concepts relate across docs
```

## 5. Implementation Approach

### 5.1 Library Selection

**Recommended: unified + remark ecosystem**

- `unified` - Core processing framework
- `remark-parse` - Markdown → AST
- `remark-stringify` - AST → Markdown
- `unist-util-visit` - AST traversal
- `mdast-util-to-string` - Extract text content

**Why remark:**
- Industry standard for markdown processing
- Rich plugin ecosystem
- Well-typed (TypeScript native)
- Handles edge cases (tables, GFM, frontmatter, etc.)
- Roundtrip fidelity (parse → modify → serialize)

### 5.2 Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
│        (Knowledge System, Task System, CLI)                 │
├─────────────────────────────────────────────────────────────┤
│                  Markdown Graph Layer                       │
│  - Node/Edge abstraction                                    │
│  - Query interface                                          │
│  - Cross-document linking                                   │
├─────────────────────────────────────────────────────────────┤
│                  Remark/Unified Layer                       │
│  - Parse markdown → AST                                     │
│  - Serialize AST → markdown                                 │
│  - Plugin ecosystem                                         │
├─────────────────────────────────────────────────────────────┤
│                   Graph System                              │
│  - Node/Edge storage                                        │
│  - Actor registration                                       │
│  - Message routing                                          │
└─────────────────────────────────────────────────────────────┘
```

### 5.3 Integration with Graph System

Markdown nodes can become first-class citizens in our Graph System:

```typescript
// Parse markdown file
const mdGraph = MarkdownGraph.fromFile("spec.md");

// Register markdown structure as graph nodes
for (const node of mdGraph.getAllNodes()) {
  // Create actor for this markdown node (if needed)
  const actor = MarkdownNodeActor({
    id: node.id,
    type: node.type,
    content: node,
    system: graph.getSystem()
  });

  // Register in graph
  graph.registerNode(node.id, actor, {
    id: node.id,
    type: "markdown-node",
    mdType: node.type,
    source: "spec.md",
    ...node.properties
  });
}

// Add structural edges
for (const edge of mdGraph.getAllEdges()) {
  graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
}
```

**Benefits:**
- Markdown structure becomes queryable via Graph System
- Can send messages to markdown nodes (e.g., "update", "validate")
- Unified graph contains both runtime state AND documentation
- Changes to graph can update source markdown files

## 6. Tradeoffs and Challenges

### 6.1 Complexity vs. Value

**Tradeoff:** Graph representation adds complexity overhead.

**When to use:**
- ✅ Large documentation sets with cross-references
- ✅ Documentation-driven workflows
- ✅ Automated doc generation/validation
- ✅ Knowledge extraction pipelines
- ❌ Small, single-file documents
- ❌ Static documentation that rarely changes

### 6.2 Memory Overhead

**Challenge:** Full AST in memory uses more RAM than plain text.

**Mitigation:**
- Lazy loading - parse on demand
- Streaming processing for large files
- Cache only frequently accessed documents
- Serialize to disk for large document sets

**Metrics:**
- Plain text: ~1KB per page
- AST representation: ~5-10KB per page
- With full graph edges: ~15-20KB per page

### 6.3 Serialization Fidelity

**Challenge:** AST → markdown may not preserve exact formatting.

**Mitigation:**
- Use remark-stringify with format preservation options
- Store original source mapping
- Use prettier-style formatting for consistency
- Accept that minor whitespace changes are okay

**Known limitations:**
- Comment positions may shift
- Blank line counts may change
- Indentation style might normalize
- Markdown flavor differences (CommonMark vs. GFM)

### 6.4 Update Synchronization

**Challenge:** Keeping graph in sync with file system changes.

**Mitigation:**
- File watchers to detect changes
- Incremental re-parsing
- Version tracking with timestamps
- Conflict detection for concurrent edits

**Strategy:**
```typescript
// Watch for file changes
fileWatcher.on("change", (path) => {
  const oldGraph = docGraph.getDocument(path);
  const newGraph = MarkdownGraph.fromFile(path);

  // Compute diff
  const diff = computeDiff(oldGraph, newGraph);

  // Update graph incrementally
  docGraph.applyDiff(path, diff);

  // Notify listeners
  emit("document-updated", { path, diff });
});
```

### 6.5 Scaling to Large Document Sets

**Challenge:** 100+ documents with thousands of nodes.

**Considerations:**
- **Indexing:** Build indexes for common queries (heading text, code lang)
- **Partitioning:** Load document subsets based on workspace
- **Caching:** Cache query results, heading hierarchies
- **Background processing:** Parse and index asynchronously

**Example scaling strategy:**
```typescript
// Lazy-loading document graph
class LazyDocumentGraph {
  private loaded = new Map<string, MarkdownGraph>();

  async getDocument(path: string): Promise<MarkdownGraph> {
    if (!this.loaded.has(path)) {
      const graph = await MarkdownGraph.fromFile(path);
      this.loaded.set(path, graph);
    }
    return this.loaded.get(path)!;
  }

  // Evict least recently used
  evictLRU(keepCount: number) {
    // ... LRU eviction logic
  }
}
```

## 7. Comparison to Alternatives

### 7.1 vs. Full-Text Search (grep/ripgrep)

| Feature | Graph-Based | Full-Text Search |
|---------|-------------|------------------|
| Find text | ✅ Yes | ✅ Yes (faster) |
| Structural queries | ✅ Yes | ❌ Regex only |
| Respect sections | ✅ Yes | ❌ Line-based |
| Cross-references | ✅ Yes | ❌ No |
| Performance | Moderate | Fast |
| Setup cost | High | Low |

**Verdict:** Use both - grep for fast text search, graph for structure.

### 7.2 vs. Custom Markdown Parser

| Feature | AST-Based (remark) | Custom Parser |
|---------|-------------------|---------------|
| Correctness | ✅ Highly tested | ⚠️ Bug-prone |
| Features | ✅ Full spec | ⚠️ Subset |
| Maintenance | ✅ Community | ❌ You |
| Performance | ✅ Optimized | ⚠️ Varies |
| Plugin ecosystem | ✅ Rich | ❌ None |

**Verdict:** Use remark - battle-tested and feature-complete.

### 7.3 vs. LSP/Tree-sitter

| Feature | Markdown Graph | LSP/Tree-sitter |
|---------|---------------|-----------------|
| Markdown support | ✅ Native | ✅ Via grammar |
| Cross-document | ✅ Yes | ⚠️ Limited |
| Graph operations | ✅ Yes | ❌ Tree only |
| Editor integration | ⚠️ Custom | ✅ Native |
| Query language | ⚠️ Custom | ✅ Standard |

**Verdict:** Complementary - LSP for editing, graph for analysis.

## 8. Future Enhancements

### 8.1 Semantic Link Resolution

Automatically resolve semantic links:

```typescript
// Detect implicit references
// "See the Actor System section" → finds heading
// "As shown in the example above" → finds previous code block

const semanticLinks = graph.findSemanticReferences(node);
```

### 8.2 Schema Validation

Validate document structure against schemas:

```typescript
// Define spec schema
const specSchema = {
  requiredSections: ["Overview", "Core Concepts", "API"],
  requiredSubsections: {
    "API": ["Usage Examples", "Error Handling"]
  },
  codeBlocksRequired: true
};

// Validate
const errors = graph.validateSchema(specSchema);
```

### 8.3 Diff and Merge

Structural diff and merge:

```typescript
// Three-way merge
const merged = MarkdownGraph.merge(base, local, remote, {
  conflictResolution: "structural"
});
```

### 8.4 AI-Powered Analysis

Use LLM to enhance graph:

```typescript
// Auto-generate summaries for sections
for (const section of graph.findNodes({ type: "heading" })) {
  const content = graph.getDescendantText(section);
  const summary = await llm.summarize(content);

  // Add summary as metadata
  graph.updateNode(section.id, {
    metadata: { ...section.metadata, summary }
  });
}
```

### 8.5 Visual Graph Editor

Interactive graph-based markdown editor:

- Drag-and-drop section reordering
- Visual link creation between documents
- Collapsible section trees
- Real-time validation
- Collaborative editing with CRDT

## 9. Conclusion

**Key Takeaways:**

1. **Markdown is inherently structured** - Already a tree, easy to enhance to graph
2. **AST enables semantic operations** - Query by structure, not regex
3. **Cross-document graphs enable analysis** - Dependencies, references, knowledge networks
4. **Practical for tk-agents** - Specs, knowledge extraction, task generation
5. **Use existing tools** - remark/unified ecosystem is mature and complete

**Recommendation:** Implement a lightweight markdown graph layer for tk-agents that:
- Parses specs to AST
- Enables structural queries
- Extracts knowledge nodes
- Validates cross-references
- Integrates with Graph System

**Next Steps:**
1. Build working prototype demonstrating core concepts
2. Create integration plan with existing Graph System
3. Identify killer use cases for immediate value
4. Implement incrementally, starting with spec validation
