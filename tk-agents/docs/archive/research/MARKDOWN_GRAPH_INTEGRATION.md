# Markdown Graph Integration Proposal for tk-agents

**Author:** Background Research Agent
**Date:** 2026-01-16
**Status:** Proposal

## Executive Summary

This document proposes integrating the markdown graph system with tk-agents to enable:
- **Specification as knowledge nodes** - Extract structured knowledge from .spec.md files
- **Documentation-driven task generation** - Create tasks from TODOs and requirements in specs
- **Cross-reference validation** - Ensure all internal links are valid
- **Knowledge graph visualization** - Build semantic networks of concepts across documentation

## Integration Architecture

```
┌───────────────────────────────────────────────────────────────┐
│                      Application Layer                        │
│            (CLI, Task Planning, Knowledge Queries)            │
├───────────────────────────────────────────────────────────────┤
│                   Markdown Graph Layer                        │
│   - Parse specs to AST                                        │
│   - Extract sections as knowledge candidates                  │
│   - Track cross-document references                           │
│   - Validate code examples                                    │
├───────────────────────────────────────────────────────────────┤
│                     Graph System                              │
│   - Register knowledge nodes                                  │
│   - Create dependency edges                                   │
│   - Route messages                                            │
├───────────────────────────────────────────────────────────────┤
│                   Actor System                                │
│   - KnowledgeActor, TaskActor instances                       │
│   - Message passing                                           │
└───────────────────────────────────────────────────────────────┘
```

## Use Case 1: Spec-to-Knowledge Pipeline

### Goal
Automatically extract knowledge nodes from specification documents.

### Implementation

```typescript
// src/markdown-graph/spec-loader.ts
import { MarkdownGraph } from './MarkdownGraph';
import { Graph } from '../graph/Graph';
import { KnowledgeActor } from '../actors/KnowledgeActor';

export class SpecLoader {
  constructor(private graph: Graph) {}

  /**
   * Load a specification file and extract knowledge nodes
   */
  async loadSpec(filePath: string): Promise<string[]> {
    // Parse markdown to graph
    const mdGraph = await MarkdownGraph.fromFile(filePath);

    // Extract H2 sections as knowledge nodes
    const sections = mdGraph.findNodes({ type: 'heading', depth: 2 });
    const knowledgeIds: string[] = [];

    for (const section of sections) {
      const knowledgeId = this.createKnowledgeNode(mdGraph, section, filePath);
      knowledgeIds.push(knowledgeId);
    }

    // Create cross-reference edges
    this.linkKnowledgeNodes(mdGraph, knowledgeIds);

    return knowledgeIds;
  }

  private createKnowledgeNode(
    mdGraph: MarkdownGraph,
    section: MarkdownNode,
    filePath: string
  ): string {
    const sectionTitle = section.metadata.text || 'Untitled';
    const knowledgeId = `knowledge-${slugify(sectionTitle)}`;

    // Get all content in this section (via metadata.section matching)
    const allNodes = mdGraph.getAllNodes();
    const sectionContent = allNodes.filter(
      n => n.metadata.section === sectionTitle
    );

    // Extract different types of content
    const paragraphs = sectionContent
      .filter(n => n.type === 'paragraph')
      .map(n => n.metadata.text)
      .join('\n\n');

    const codeExamples = sectionContent
      .filter(n => n.type === 'code')
      .map(n => ({
        lang: n.properties.lang,
        code: n.properties.value
      }));

    const links = sectionContent
      .filter(n => n.type === 'link')
      .map(n => n.properties.url);

    // Create knowledge actor
    const knowledgeActor = KnowledgeActor({
      id: knowledgeId,
      title: sectionTitle,
      content: paragraphs,
      sources: [{
        type: 'spec',
        file: filePath,
        section: sectionTitle,
        position: section.position
      }],
      metadata: {
        codeExamples,
        references: links
      },
      system: this.graph.getSystem()
    });

    // Register in graph
    this.graph.registerNode(knowledgeId, knowledgeActor, {
      id: knowledgeId,
      type: 'knowledge',
      title: sectionTitle,
      source: filePath,
      createdAt: new Date()
    });

    return knowledgeId;
  }

  private linkKnowledgeNodes(mdGraph: MarkdownGraph, knowledgeIds: string[]): void {
    // Find links between sections
    const links = mdGraph.findLinks();

    for (const link of links) {
      const url = link.properties.url as string;

      // Check if link references another spec file
      if (url?.includes('.spec.md')) {
        const targetFile = url.split('#')[0];
        const targetSection = url.split('#')[1];

        // Create requires_knowledge edge
        // (Implementation would need to resolve target knowledge node)
        // this.graph.addEdge(fromKnowledge, toKnowledge, 'references');
      }
    }
  }
}
```

### Usage

```typescript
// Load all specs
const specLoader = new SpecLoader(graph);
const graphKnowledge = await specLoader.loadSpec('src/GRAPH_SYSTEM.spec.md');
const taskKnowledge = await specLoader.loadSpec('src/TASK_SYSTEM.spec.md');

// Now knowledge nodes are available for querying
const conceptNodes = graph.findNodes({ type: 'knowledge' });
```

## Use Case 2: Documentation-Driven Task Generation

### Goal
Extract TODO comments and specification sections as task nodes.

### Implementation

```typescript
// src/markdown-graph/task-extractor.ts
export class TaskExtractor {
  constructor(private graph: Graph) {}

  /**
   * Extract tasks from spec TODOs and requirements
   */
  async extractTasks(filePath: string): Promise<string[]> {
    const mdGraph = await MarkdownGraph.fromFile(filePath);
    const tasks: string[] = [];

    // Method 1: Find TODO comments in code blocks
    const codeBlocks = mdGraph.findNodes({ type: 'code' });
    for (const code of codeBlocks) {
      const todos = this.extractTODOs(code.properties.value as string);
      for (const todo of todos) {
        const taskId = await this.createTask({
          goal: todo.text,
          context: {
            source: filePath,
            section: code.metadata.section,
            type: 'code-todo'
          }
        });
        tasks.push(taskId);
      }
    }

    // Method 2: Find "Implementation Required" sections
    const implSections = mdGraph.findNodes({
      type: 'heading',
      matches: (node) =>
        node.metadata.text?.includes('TODO') ||
        node.metadata.text?.includes('Implementation')
    });

    for (const section of implSections) {
      const taskId = await this.createTask({
        goal: `Implement: ${section.metadata.text}`,
        context: {
          source: filePath,
          section: section.metadata.text,
          type: 'spec-section'
        }
      });
      tasks.push(taskId);
    }

    return tasks;
  }

  private extractTODOs(code: string): Array<{ text: string; line: number }> {
    const todos: Array<{ text: string; line: number }> = [];
    const lines = code.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const match = lines[i].match(/\/\/\s*TODO:\s*(.+)/);
      if (match) {
        todos.push({ text: match[1].trim(), line: i + 1 });
      }
    }

    return todos;
  }

  private async createTask(params: {
    goal: string;
    context: any;
  }): Promise<string> {
    const taskId = `task-${generateId()}`;

    const taskActor = TaskActor({
      id: taskId,
      goal: params.goal,
      state: 'created',
      context: params.context,
      system: this.graph.getSystem()
    });

    this.graph.registerNode(taskId, taskActor, {
      id: taskId,
      type: 'task',
      goal: params.goal,
      state: 'created',
      createdAt: new Date()
    });

    return taskId;
  }
}
```

## Use Case 3: Cross-Reference Validation

### Goal
Validate that all internal documentation links are correct.

### Implementation

```typescript
// src/markdown-graph/link-validator.ts
export class LinkValidator {
  /**
   * Validate links across all spec files
   */
  async validateAllSpecs(specDir: string): Promise<ValidationReport> {
    const specFiles = await this.findSpecFiles(specDir);
    const report: ValidationReport = {
      totalLinks: 0,
      brokenLinks: [],
      externalLinks: [],
      validLinks: 0
    };

    for (const file of specFiles) {
      const mdGraph = await MarkdownGraph.fromFile(file);

      // Check internal anchor links
      const brokenInternal = mdGraph.findBrokenLinks();
      report.brokenLinks.push(
        ...brokenInternal.map(edge => ({
          file,
          url: edge.properties.url,
          type: 'broken-anchor'
        }))
      );

      // Check cross-file links
      const allLinks = mdGraph.findLinks();
      for (const link of allLinks) {
        const url = link.properties.url as string;
        report.totalLinks++;

        if (url.startsWith('http')) {
          report.externalLinks.push({ file, url });
        } else if (url.includes('.md')) {
          const targetPath = this.resolvePath(file, url);
          const exists = await this.fileExists(targetPath);
          if (!exists) {
            report.brokenLinks.push({
              file,
              url,
              type: 'missing-file',
              target: targetPath
            });
          } else {
            report.validLinks++;
          }
        }
      }
    }

    return report;
  }

  async generateReport(report: ValidationReport): Promise<string> {
    let md = '# Documentation Link Validation Report\n\n';
    md += `**Generated:** ${new Date().toISOString()}\n\n`;
    md += `## Summary\n\n`;
    md += `- Total links: ${report.totalLinks}\n`;
    md += `- Valid links: ${report.validLinks}\n`;
    md += `- Broken links: ${report.brokenLinks.length}\n`;
    md += `- External links: ${report.externalLinks.length}\n\n`;

    if (report.brokenLinks.length > 0) {
      md += `## Broken Links\n\n`;
      for (const broken of report.brokenLinks) {
        md += `- **${broken.file}**: \`${broken.url}\` (${broken.type})\n`;
        if (broken.target) {
          md += `  - Target: ${broken.target}\n`;
        }
      }
    }

    return md;
  }

  private async findSpecFiles(dir: string): Promise<string[]> {
    // Use glob to find all .spec.md files
    return glob('**/*.spec.md', { cwd: dir });
  }

  private resolvePath(from: string, to: string): string {
    // Resolve relative path
    const fromDir = path.dirname(from);
    return path.resolve(fromDir, to);
  }

  private async fileExists(path: string): Promise<boolean> {
    return Bun.file(path).exists();
  }
}
```

### CLI Integration

```typescript
// src/cli/commands/validate-docs.ts
export async function validateDocs() {
  const validator = new LinkValidator();
  const report = await validator.validateAllSpecs('src');

  console.log(await validator.generateReport(report));

  if (report.brokenLinks.length > 0) {
    process.exit(1);
  }
}
```

## Use Case 4: Knowledge Graph Visualization

### Goal
Generate a visual representation of how concepts relate across documentation.

### Implementation

```typescript
// src/markdown-graph/knowledge-graph-builder.ts
export class KnowledgeGraphBuilder {
  async buildConceptGraph(specDir: string): Promise<ConceptGraph> {
    const graph = new ConceptGraph();

    // Load all specs
    const specFiles = await glob('**/*.spec.md', { cwd: specDir });

    for (const file of specFiles) {
      const mdGraph = await MarkdownGraph.fromFile(file);

      // Extract concepts (H2 headings)
      const concepts = mdGraph.findNodes({ type: 'heading', depth: 2 });

      for (const concept of concepts) {
        const conceptName = concept.metadata.text || '';

        // Add concept node
        graph.addConcept({
          name: conceptName,
          source: file,
          summary: this.extractSummary(mdGraph, concept)
        });

        // Find references to other concepts
        const links = mdGraph.findLinks();
        for (const link of links) {
          const url = link.properties.url as string;
          if (url?.includes('.spec.md')) {
            const targetFile = url.split('#')[0];
            const targetSection = url.split('#')[1];

            if (targetSection) {
              graph.addRelationship({
                from: conceptName,
                to: targetSection,
                type: 'references',
                context: file
              });
            }
          }
        }

        // Find code examples that mention other concepts
        const codeEdges = mdGraph.getEdges(concept.id, 'contains-code', 'from');
        for (const edge of codeEdges) {
          const codeNode = mdGraph.getNode(edge.toId);
          const codeText = codeNode?.properties.value as string;

          // Check if code mentions other known concepts
          for (const otherConcept of concepts) {
            const otherName = otherConcept.metadata.text || '';
            if (otherName !== conceptName && codeText?.includes(otherName)) {
              graph.addRelationship({
                from: conceptName,
                to: otherName,
                type: 'implements',
                context: 'code-example'
              });
            }
          }
        }
      }
    }

    return graph;
  }

  private extractSummary(mdGraph: MarkdownGraph, concept: MarkdownNode): string {
    // Get first paragraph after heading
    const allNodes = mdGraph.getAllNodes();
    const conceptSection = allNodes.filter(
      n => n.metadata.section === concept.metadata.text
    );

    const firstPara = conceptSection.find(n => n.type === 'paragraph');
    return firstPara?.metadata.text?.substring(0, 200) || '';
  }

  /**
   * Export to Mermaid diagram
   */
  exportMermaid(graph: ConceptGraph): string {
    let mermaid = 'graph LR\n';

    for (const concept of graph.concepts) {
      const id = this.sanitizeId(concept.name);
      mermaid += `  ${id}["${concept.name}"]\n`;
    }

    for (const rel of graph.relationships) {
      const fromId = this.sanitizeId(rel.from);
      const toId = this.sanitizeId(rel.to);
      const label = rel.type;
      mermaid += `  ${fromId} -->|${label}| ${toId}\n`;
    }

    return mermaid;
  }

  private sanitizeId(name: string): string {
    return name.replace(/[^a-zA-Z0-9]/g, '_');
  }
}
```

## Use Case 5: Code Example Validation

### Goal
Extract and validate all code examples from specifications.

### Implementation

```typescript
// src/markdown-graph/code-validator.ts
export class CodeValidator {
  async validateSpecExamples(filePath: string): Promise<ValidationResult[]> {
    const mdGraph = await MarkdownGraph.fromFile(filePath);
    const results: ValidationResult[] = [];

    // Get all TypeScript code blocks
    const tsCode = mdGraph.findNodes({
      type: 'code',
      lang: 'typescript'
    });

    for (const code of tsCode) {
      const section = code.metadata.section || 'Unknown';
      const codeText = code.properties.value as string;

      try {
        // Validate TypeScript (would use actual TS compiler)
        await this.validateTypeScript(codeText);

        results.push({
          section,
          status: 'valid',
          code: codeText.substring(0, 100)
        });
      } catch (error) {
        results.push({
          section,
          status: 'invalid',
          code: codeText.substring(0, 100),
          error: error.message
        });
      }
    }

    return results;
  }

  private async validateTypeScript(code: string): Promise<void> {
    // Wrap in a temporary file and use tsc to validate
    // Or use TypeScript compiler API
    // For now, just check syntax
    try {
      new Function(code);
    } catch (e) {
      throw new Error(`Syntax error: ${e.message}`);
    }
  }
}
```

## Implementation Roadmap

### Phase 1: Core Integration (Week 1)
- [ ] Implement `SpecLoader` class
- [ ] Create CLI command: `task-cli load-specs`
- [ ] Test with GRAPH_SYSTEM.spec.md
- [ ] Verify knowledge nodes are created correctly

### Phase 2: Validation Tools (Week 2)
- [ ] Implement `LinkValidator`
- [ ] Create CLI command: `task-cli validate-docs`
- [ ] Run validation on all existing specs
- [ ] Fix any broken links found

### Phase 3: Task Generation (Week 3)
- [ ] Implement `TaskExtractor`
- [ ] Create CLI command: `task-cli extract-tasks <spec>`
- [ ] Test with specs containing TODOs
- [ ] Integrate with task planning workflow

### Phase 4: Visualization (Week 4)
- [ ] Implement `KnowledgeGraphBuilder`
- [ ] Export concept graph to Mermaid
- [ ] Generate visualization for existing specs
- [ ] Add to documentation site

### Phase 5: Code Validation (Week 5)
- [ ] Implement `CodeValidator`
- [ ] Integrate with TypeScript compiler API
- [ ] Add to CI/CD pipeline
- [ ] Create fixing workflow for invalid examples

## Benefits

1. **Automated Knowledge Extraction**
   - Specs become source of truth for knowledge nodes
   - No manual knowledge entry required
   - Always in sync with latest specs

2. **Documentation Quality**
   - Broken links detected automatically
   - Code examples validated
   - Consistency across docs enforced

3. **Task Generation**
   - TODOs automatically become tasks
   - Spec sections drive implementation work
   - Clear traceability from spec to code

4. **Knowledge Discovery**
   - Concept relationships visualized
   - Cross-references mapped
   - Understanding system architecture easier

5. **Living Documentation**
   - Docs stay current with code
   - Changes tracked semantically
   - History of concepts maintained

## Success Metrics

- **Coverage**: % of spec sections extracted as knowledge nodes
- **Validation**: # of broken links found and fixed
- **Task Generation**: # of tasks created from specs
- **Code Quality**: % of code examples that validate
- **Usage**: # of knowledge queries against spec nodes

## Next Steps

1. Get feedback on integration approach
2. Prioritize use cases
3. Implement Phase 1 (SpecLoader)
4. Validate with existing specs
5. Iterate based on results

## Conclusion

Integrating markdown graph with tk-agents transforms static documentation into an active knowledge and task management system. By treating specs as structured data rather than plain text, we unlock powerful automation capabilities while maintaining human-readable documentation.

The proposed architecture is modular, testable, and builds on the existing Graph System foundation. Each use case delivers immediate value while contributing to a comprehensive documentation-driven development workflow.
