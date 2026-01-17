# Graph Query Language Research

**Date:** 2026-01-16
**Context:** tk-agents graph query enhancement
**Current State:** Basic list/filter operations, Address-based actors with typed edges

## Executive Summary

After comprehensive research into graph query languages and result formats, I recommend a **three-phase approach**:

1. **Phase 1 (Immediate):** Fluent API - TypeScript-native builder pattern (2-3 days)
2. **Phase 2 (Medium-term):** CSS-style selectors - familiar syntax for power users (1-2 weeks)
3. **Phase 3 (Long-term):** Datalog - logic programming for complex recursive queries (3-4 weeks)

**Key Findings:**
- **Fluent APIs** offer the lowest implementation barrier with full TypeScript type safety
- **CSS selectors** provide familiar syntax but require custom parser implementation
- **Datalog** offers the most power for recursive/transitive queries but has steep learning curve
- **SPARQL/Cypher/Gremlin** are powerful but overkill for in-memory graph, high implementation cost
- **Result formats** should support both tuples (for tabular display) and subgraphs (for graph operations)

---

## 1. Query Language Options

### 1.1 Fluent API (Method Chaining)

**Description:** TypeScript-native builder pattern using method chaining for query composition.

**Complexity:** 2/10 (Very Low)

**Expressiveness:** 7/10 (Good for common patterns, limited by API design)

**Learning Curve:** 1/10 (Minimal - autocomplete guides users)

**Example Queries:**
```typescript
// Find all active tasks that depend on knowledge nodes
graph.query()
  .nodes({ type: 'task', state: 'active' })
  .followEdges({ type: 'depends_on' })
  .toNodes({ type: 'knowledge' })
  .select(['id', 'title', 'content'])
  .execute();

// Find blocked tasks and their blockers
graph.query()
  .nodes({ type: 'task', state: 'blocked' })
  .withEdges({ type: 'blocks', direction: 'incoming' })
  .selectSubgraph()
  .execute();

// Get task hierarchy (parent -> children)
graph.query()
  .node('task_123')
  .traverseEdges({ type: 'spawned_by', direction: 'incoming' })
  .maxDepth(3)
  .selectTree()
  .execute();
```

**Pros:**
- Zero parser needed - pure TypeScript
- Full IDE autocomplete and type checking
- Easy to extend with new operations
- Natural for imperative programmers
- Can expose as both API and CLI (via command builder)
- Excellent error messages

**Cons:**
- API surface grows with features
- Less concise than declarative syntax
- Method names must be carefully chosen
- Not as portable to other languages

**Implementation Notes:**
- Use TypeScript generics for type-safe query building
- Return new query object from each method (immutable)
- Lazy evaluation - build AST, execute at `.execute()`
- Can generate SQL-like query plan for debugging

---

### 1.2 CSS-Style Selectors

**Description:** Adapt CSS selector syntax for graph traversal - familiar to web developers.

**Complexity:** 5/10 (Moderate - requires parser)

**Expressiveness:** 8/10 (Powerful pattern matching)

**Learning Curve:** 3/10 (Low for web developers)

**Example Queries:**
```typescript
// Attribute selectors
graph.query('node[type=task][state=active]');

// Direct child (outgoing edge)
graph.query('node#task_123 > edge[type=spawned_by] > node');

// Descendant (transitive edges)
graph.query('node#task_root edge[type=depends_on] node[type=knowledge]');

// Multiple selectors
graph.query('node[type=task][state=blocked], node[state=failed]');

// Pseudo-classes
graph.query('node[type=task]:has(edge[type=blocks])');

// Edge filters
graph.query('edge[type=depends_on][properties.weight>0.5]');
```

**Pros:**
- Familiar syntax from CSS/jQuery
- Concise and readable
- Pattern matching is intuitive
- Can support pseudo-selectors for complex filters
- Good for CLI usage (one-liner queries)

**Cons:**
- Requires parser implementation
- CSS semantics don't map perfectly to graphs
- Edge direction handling is awkward
- Limited support for graph algorithms (shortest path, etc.)
- Error messages can be cryptic with complex selectors

**Implementation Notes:**
- Use parser generator (e.g., PEG.js, nearley)
- Map to internal query AST
- Support subset of CSS selectors initially
- Add graph-specific extensions (edge direction, traversal depth)

**Parser Libraries:**
- `css-what` - CSS selector parser
- `css-select` - CSS selector engine (adaptable)
- Custom PEG grammar with `peggy`

---

### 1.3 SPARQL (RDF Query Language)

**Description:** W3C standard query language for RDF triple stores. Declarative SQL-like syntax.

**Complexity:** 8/10 (High - requires RDF conversion, complex spec)

**Expressiveness:** 10/10 (Extremely powerful, recursive, federated queries)

**Learning Curve:** 8/10 (Steep - unfamiliar to most developers)

**Example Queries:**
```sparql
# Find active tasks with their knowledge dependencies
PREFIX task: <http://example.org/task#>
PREFIX edge: <http://example.org/edge#>

SELECT ?taskId ?knowledgeId ?knowledgeTitle
WHERE {
  ?taskId a task:Task ;
          task:state "active" ;
          edge:depends_on ?knowledgeId .
  ?knowledgeId a task:Knowledge ;
               task:title ?knowledgeTitle .
}

# Find blocked tasks and count their blockers
SELECT ?taskId (COUNT(?blocker) AS ?blockerCount)
WHERE {
  ?taskId a task:Task ;
          task:state "blocked" .
  ?blocker edge:blocks ?taskId .
}
GROUP BY ?taskId
HAVING (COUNT(?blocker) > 0)
```

**Pros:**
- Industry standard (W3C)
- Extremely expressive (UNION, OPTIONAL, FILTER, aggregates)
- Mature tooling ecosystem
- Supports federated queries
- Well-defined semantics
- Great for semantic web integration

**Cons:**
- Massive implementation effort
- Requires RDF triple store model
- Verbose syntax
- Poor fit for in-memory graphs
- Overkill for most use cases
- Performance overhead of RDF layer

**Implementation Notes:**
- Would need to convert graph to RDF triples
- Use existing SPARQL engine: `rdflib.js`, `comunica`
- Map node properties to RDF predicates
- Edge types become RDF properties
- Consider SPARQL subset (no federation, no inference)

**Libraries:**
- `rdflib.js` - RDF library with SPARQL support
- `@comunica/query-sparql` - Modular SPARQL engine
- `sparqljs` - SPARQL parser/generator

---

### 1.4 Cypher (Neo4j Query Language)

**Description:** Declarative pattern-matching language from Neo4j. Uses ASCII art for patterns.

**Complexity:** 6/10 (Moderate-High)

**Expressiveness:** 9/10 (Very powerful, designed for graphs)

**Learning Curve:** 5/10 (Moderate - visual but new concepts)

**Example Queries:**
```cypher
// Find active tasks and their knowledge dependencies
MATCH (t:Task {state: 'active'})-[:DEPENDS_ON]->(k:Knowledge)
RETURN t.id, k.id, k.title

// Find blocked tasks with blockers
MATCH (t:Task {state: 'blocked'})<-[:BLOCKS]-(blocker)
RETURN t.id, COLLECT(blocker.id) AS blockers

// Variable-length paths (transitive dependencies)
MATCH (t:Task {id: 'task_123'})-[:DEPENDS_ON*1..3]->(deps)
RETURN deps

// Create new edge
MATCH (t1:Task {id: 'task_1'}), (t2:Task {id: 'task_2'})
CREATE (t1)-[:BLOCKS]->(t2)
```

**Pros:**
- Designed specifically for graphs
- Visual ASCII art patterns are intuitive
- Powerful pattern matching
- Supports mutations (CREATE, DELETE, SET)
- Variable-length path patterns
- Good for graph algorithms

**Cons:**
- Requires substantial parser
- Neo4j-specific (less portable)
- Need to implement execution engine
- Complex optimization needed for performance
- Not widely known outside Neo4j community

**Implementation Notes:**
- Parser required (no good standalone parsers)
- Could use `cypher-query-builder` as inspiration
- Map to graph traversal operations
- Pattern matching is key feature to implement
- Consider subset: MATCH, WHERE, RETURN only

**Libraries:**
- `cypher-query-builder` - Fluent API for Cypher (not parser)
- Would need custom parser with PEG
- Can reference Neo4j's OpenCypher spec

---

### 1.5 Gremlin (Apache TinkerPop)

**Description:** Traversal-based query language using fluent API. Step-by-step graph walking.

**Complexity:** 5/10 (Moderate)

**Expressiveness:** 9/10 (Very powerful, composable)

**Learning Curve:** 6/10 (Moderate-High - functional thinking required)

**Example Queries:**
```javascript
// Find active tasks depending on knowledge
g.V().hasLabel('task')
     .has('state', 'active')
     .out('depends_on')
     .hasLabel('knowledge')
     .values('id', 'title')

// Find blocked tasks and count blockers
g.V().hasLabel('task')
     .has('state', 'blocked')
     .as('task')
     .in('blocks')
     .count()
     .as('blockerCount')
     .select('task', 'blockerCount')

// Recursive traversal with repeat
g.V('task_123')
     .repeat(out('spawned_by'))
     .times(3)
     .path()
```

**Pros:**
- Powerful traversal primitives
- Good for graph algorithms
- Composable steps
- Apache TinkerPop ecosystem
- Can optimize traversals
- Supports mutations

**Cons:**
- Functional/pipeline thinking required
- JavaScript API is verbose
- Need graph database backend or adapter
- Performance tuning complex
- Less intuitive than pattern matching

**Implementation Notes:**
- Could adapt Gremlin JavaScript API
- Build on top of our Address-based system
- Implement subset of steps: V(), E(), has(), out(), in(), both()
- Lazy evaluation with streams
- Map to graph traversal operations

**Libraries:**
- `gremlin` - Official Gremlin JavaScript driver
- `ts-tinkerpop` - TypeScript Gremlin client
- Could implement custom traversal engine

---

### 1.6 GraphQL

**Description:** API query language with strongly-typed schema. Primarily for client-server APIs.

**Complexity:** 6/10 (Moderate - requires schema, resolver infrastructure)

**Expressiveness:** 7/10 (Good for hierarchical queries, limited for graph traversal)

**Learning Curve:** 4/10 (Moderate - widely known)

**Example Queries:**
```graphql
# Find task with dependencies
query {
  task(id: "task_123") {
    id
    state
    goal
    dependencies {
      type
      targetNode {
        ... on Knowledge {
          id
          title
          content
        }
      }
    }
  }
}

# Find all active tasks
query {
  tasks(filter: { state: ACTIVE }) {
    id
    goal
    blockers {
      id
      reason
    }
  }
}
```

**Pros:**
- Strongly typed with schema
- Widely adopted
- Great tooling (GraphiQL, Apollo)
- Natural for hierarchical queries
- Type-safe client generation
- Good for API exposure

**Cons:**
- Not designed for graph traversal
- Requires schema definition
- Resolver boilerplate
- N+1 query problem
- Limited for recursive queries
- More suited for API than CLI

**Implementation Notes:**
- Use `graphql-js` or `type-graphql`
- Define schema from our types
- Implement resolvers for each node type
- Use DataLoader for batching
- Consider GraphQL subscriptions for live updates

**Libraries:**
- `graphql` - Reference implementation
- `type-graphql` - TypeScript decorators
- `apollo-server` - Full GraphQL server

---

### 1.7 Datalog

**Description:** Logic programming language with declarative rules. Based on Prolog, designed for queries.

**Complexity:** 6/10 (Moderate - requires engine, unfamiliar syntax)

**Expressiveness:** 10/10 (Most powerful - recursive, transitive closure, negation)

**Learning Curve:** 7/10 (High - logic programming paradigm)

**Example Queries:**
```datalog
% Find active tasks depending on knowledge
?- task(TaskId, active),
   edge(TaskId, depends_on, KnowId),
   knowledge(KnowId, Title, _).

% Transitive dependencies (recursive rule)
depends_transitively(X, Y) :- edge(X, depends_on, Y).
depends_transitively(X, Z) :-
   edge(X, depends_on, Y),
   depends_transitively(Y, Z).

?- depends_transitively(task_123, Dep).

% Find tasks with no blockers
?- task(TaskId, _),
   \+ edge(_, blocks, TaskId).

% Count blockers per task
?- task(TaskId, blocked),
   findall(Blocker, edge(Blocker, blocks, TaskId), Blockers),
   length(Blockers, Count).
```

**Pros:**
- Extremely powerful (recursive queries native)
- Declarative - describe what, not how
- Excellent for transitive closure
- Negation as failure
- Can express complex constraints
- Academic backing (decades of research)

**Cons:**
- Unfamiliar to most developers
- Requires Datalog engine
- Performance tuning complex
- Limited JavaScript implementations
- Syntax is alien to imperative programmers

**Implementation Notes:**
- Use existing Datalog engine: `datalog-js`, `datascript`
- Define facts from graph nodes/edges
- Query with rules
- Consider Datalog subset (no recursion initially)
- Can compile to SQL or direct graph traversal

**Libraries:**
- `datalog-js` - Datalog interpreter
- `datascript` - Immutable database with Datalog
- `logicjs` - Logic programming in JS
- Could implement minimal Datalog subset

---

### 1.8 Custom DSL (Domain-Specific Language)

**Description:** Purpose-built query language tailored to our graph model and use cases.

**Complexity:** 7/10 (High - design + parser + execution)

**Expressiveness:** 8/10 (Can be very powerful, limited by our design)

**Learning Curve:** Varies (depends on design - aim for 3-4/10)

**Example Queries:**
```
// Simple syntax inspired by our domain
FIND tasks WHERE state = active
  FOLLOW depends_on
  TO knowledge
  SELECT id, title

FIND task task_123
  TRAVERSE spawned_by (depth: 3)
  AS TREE

FIND tasks WHERE state = blocked
  WITH EDGES blocks (direction: in)
  AS SUBGRAPH
```

**Pros:**
- Perfect fit for our domain
- Can optimize for our use cases
- Full control over features
- Can evolve with our needs
- No unnecessary features
- Can make very intuitive for users

**Cons:**
- Significant design effort
- Parser implementation required
- Documentation needed
- Users must learn new syntax
- No external tooling
- Maintenance burden

**Implementation Notes:**
- Design syntax with user feedback
- Use parser generator (PEG, ANTLR)
- Keep syntax simple and regular
- Borrow familiar patterns (SQL, CSS)
- Provide clear error messages
- Consider REPL for experimentation

---

## 2. Result Format Options

### 2.1 Tuples/Tables (SQL-style)

**Use Cases:**
- CLI display (human-readable tables)
- CSV export
- Reporting and analytics
- Integration with data tools
- Simple projections (SELECT-like)

**Implementation Complexity:** 2/10 (Very simple)

**Memory/Performance:**
- Memory: Low (flat data)
- Performance: Fast for simple queries
- Streaming: Easy (row-by-row)

**Format:**
```typescript
interface TupleResult {
  columns: string[];
  rows: Array<Record<string, unknown>>;
  meta?: {
    executionTime: number;
    totalMatches: number;
  };
}

// Example
{
  columns: ['id', 'type', 'state'],
  rows: [
    { id: 'task_1', type: 'task', state: 'active' },
    { id: 'task_2', type: 'task', state: 'blocked' }
  ]
}
```

**Pros:**
- Simple to understand
- Easy to display (ASCII table)
- Natural for aggregations
- Portable format (JSON, CSV)

**Cons:**
- Loses graph structure
- Can't represent complex relationships
- Duplicates data for multi-valued properties

---

### 2.2 Subgraph (Nodes + Edges)

**Use Cases:**
- Graph visualization
- Further graph operations
- Exporting graph slices
- Graph algorithms (on result)
- Preserving full structure

**Implementation Complexity:** 3/10 (Simple)

**Memory/Performance:**
- Memory: Medium (nodes + edges)
- Performance: Good for moderate result sets
- Streaming: Moderate (need to collect graph)

**Format:**
```typescript
interface SubgraphResult {
  nodes: Map<string, NodeProperties>;
  edges: Edge[];
  rootNodes?: string[]; // Entry points
  meta?: {
    nodeCount: number;
    edgeCount: number;
    components: number; // Connected components
  };
}

// Example
{
  nodes: new Map([
    ['task_1', { id: 'task_1', type: 'task', state: 'active', ... }],
    ['know_1', { id: 'know_1', type: 'knowledge', title: '...', ... }]
  ]),
  edges: [
    { id: 'edge_1', fromId: 'task_1', toId: 'know_1', type: 'depends_on', properties: {} }
  ],
  rootNodes: ['task_1']
}
```

**Pros:**
- Preserves graph structure
- Can visualize results
- Composable (query results)
- Natural for graph operations
- Supports further traversal

**Cons:**
- More complex than tuples
- Redundant for simple projections
- Requires graph-aware tools to use

---

### 2.3 Paths (Sequences)

**Use Cases:**
- Shortest path queries
- Dependency chains
- Traversal results
- Flow analysis
- Lineage tracking

**Implementation Complexity:** 4/10 (Moderate)

**Memory/Performance:**
- Memory: Low-Medium (depends on path length)
- Performance: Good for path-finding algorithms
- Streaming: Easy (path-by-path)

**Format:**
```typescript
interface PathResult {
  paths: Path[];
  meta?: {
    algorithm: string; // e.g., 'dijkstra', 'bfs'
    totalPaths: number;
  };
}

interface Path {
  nodes: string[];
  edges: string[];
  length: number;
  weight?: number; // For weighted paths
  metadata?: Record<string, unknown>;
}

// Example - dependency chain
{
  paths: [
    {
      nodes: ['task_1', 'know_1', 'know_2'],
      edges: ['edge_1', 'edge_2'],
      length: 2
    }
  ]
}
```

**Pros:**
- Natural for path queries
- Compact representation
- Easy to understand linearly
- Good for dependency analysis

**Cons:**
- Limited to sequential relationships
- Can't represent DAGs compactly
- Redundant data if many overlapping paths

---

### 2.4 Tree (Hierarchical)

**Use Cases:**
- Task hierarchies (parent-child)
- Dependency trees
- CLI tree display
- Recursive structures
- Drill-down interfaces

**Implementation Complexity:** 4/10 (Moderate)

**Memory/Performance:**
- Memory: Medium (nested structure)
- Performance: Good for hierarchical queries
- Streaming: Hard (need full tree)

**Format:**
```typescript
interface TreeResult {
  root: TreeNode;
  meta?: {
    maxDepth: number;
    totalNodes: number;
  };
}

interface TreeNode {
  id: string;
  properties: NodeProperties;
  children: TreeNode[];
  edge?: Edge; // Edge from parent
}

// Example
{
  root: {
    id: 'task_root',
    properties: { ... },
    children: [
      {
        id: 'task_child_1',
        properties: { ... },
        edge: { id: 'edge_1', type: 'spawned_by', ... },
        children: []
      }
    ]
  }
}
```

**Pros:**
- Natural for hierarchies
- Easy to visualize (ASCII tree)
- Matches task decomposition
- Recursive structure

**Cons:**
- Can't represent DAGs (cycles, multiple parents)
- Arbitrary choice for root
- Redundant for non-hierarchical graphs

---

### 2.5 JSON Objects (Flexible)

**Use Cases:**
- API responses
- JavaScript consumption
- Mixed result types
- Custom projections
- Integration with web apps

**Implementation Complexity:** 2/10 (Very simple)

**Memory/Performance:**
- Memory: Varies (depends on nesting)
- Performance: Good
- Streaming: Easy (JSON streaming)

**Format:**
```typescript
// Arbitrary structure based on query
type JSONResult =
  | Record<string, unknown>
  | Array<Record<string, unknown>>;

// Example - nested object
{
  task: {
    id: 'task_1',
    state: 'active',
    dependencies: [
      { id: 'know_1', title: 'Knowledge 1' }
    ],
    blockers: []
  }
}
```

**Pros:**
- Maximum flexibility
- Natural for JavaScript
- Easy to consume
- Can nest arbitrarily

**Cons:**
- No standard structure
- Hard to document
- Can be inconsistent
- May duplicate data

---

### 2.6 Streaming (Generator/AsyncIterator)

**Use Cases:**
- Large result sets
- Real-time updates
- Memory-constrained environments
- Progressive rendering
- Pagination

**Implementation Complexity:** 5/10 (Moderate)

**Memory/Performance:**
- Memory: Very low (constant)
- Performance: Excellent for large results
- Streaming: Native

**Format:**
```typescript
interface StreamingResult<T> {
  stream: AsyncIterable<T>;
  meta?: {
    estimatedTotal?: number;
  };
}

// Usage
for await (const node of result.stream) {
  console.log(node.id);
}

// With generators
async function* queryStream() {
  for (const nodeId of matchingNodes) {
    yield graph.getNodeProperties(nodeId);
  }
}
```

**Pros:**
- Constant memory usage
- Can start processing immediately
- Natural for large datasets
- Composable (pipeline)

**Cons:**
- Can't know total count upfront
- Hard to implement some operations (sorting)
- Requires async handling
- Can't random-access results

---

## 3. Implementation Ease Ranking

| Rank | Option | Effort | Fit | Library Support | User Familiarity | Overall Score |
|------|--------|--------|-----|-----------------|------------------|---------------|
| 1 | Fluent API | 2/10 | 9/10 | 10/10 (native) | 8/10 | 9.0 |
| 2 | CSS Selectors | 5/10 | 7/10 | 5/10 | 9/10 | 7.5 |
| 3 | Datalog | 6/10 | 9/10 | 6/10 | 3/10 | 7.0 |
| 4 | Custom DSL | 7/10 | 10/10 | 0/10 | 5/10 | 6.5 |
| 5 | Gremlin | 5/10 | 8/10 | 7/10 | 4/10 | 6.0 |
| 6 | Cypher | 6/10 | 9/10 | 3/10 | 5/10 | 5.5 |
| 7 | GraphQL | 6/10 | 5/10 | 10/10 | 8/10 | 5.0 |
| 8 | SPARQL | 8/10 | 6/10 | 7/10 | 2/10 | 4.0 |

**Evaluation Criteria:**
- **Effort:** Implementation complexity (lower is better)
- **Fit:** How well it matches our graph model and use cases
- **Library Support:** Availability of TypeScript/JavaScript libraries
- **User Familiarity:** How many developers already know it
- **Overall Score:** Weighted average (Fit 35%, Effort 25%, Support 20%, Familiarity 20%)

**Analysis:**

**Top 3 Winners:**

1. **Fluent API (9.0)** - Clear winner for Phase 1
   - Native TypeScript, zero dependencies
   - Immediate productivity
   - Can build CLI commands on top

2. **CSS Selectors (7.5)** - Strong second for Phase 2
   - Familiar syntax for power users
   - Concise one-liner queries
   - Good for CLI

3. **Datalog (7.0)** - Long-term power option for Phase 3
   - Most expressive for complex queries
   - Handles recursion naturally
   - Growing interest in logic programming

**Why Others Rank Lower:**

- **SPARQL (4.0):** Massive overkill, RDF mismatch, verbose
- **GraphQL (5.0):** Wrong tool (API-first, not graph traversal)
- **Cypher (5.5):** Great language, but Neo4j-specific, high implementation cost
- **Gremlin (6.0):** Powerful but verbose, unfamiliar paradigm
- **Custom DSL (6.5):** High effort, no ecosystem benefits

---

## 4. Specifications for Top 3

### 4.1 Fluent API Specification

**Syntax Examples:**
```typescript
// Simple node filter
graph.query()
  .nodes({ type: 'task', state: 'active' })
  .execute();

// Follow edges
graph.query()
  .nodes({ type: 'task' })
  .followEdges({ type: 'depends_on', direction: 'outgoing' })
  .toNodes({ type: 'knowledge' })
  .execute();

// Multiple hops with depth
graph.query()
  .node('task_123')
  .traverse({ type: 'spawned_by', direction: 'incoming', maxDepth: 3 })
  .execute();

// With projection
graph.query()
  .nodes({ type: 'task' })
  .select(['id', 'state', 'goal'])
  .execute();

// Subgraph result
graph.query()
  .nodes({ type: 'task', state: 'blocked' })
  .withEdges({ type: 'blocks', direction: 'both' })
  .asSubgraph()
  .execute();

// Aggregation
graph.query()
  .nodes({ type: 'task' })
  .groupBy('state')
  .count()
  .execute();
```

**TypeScript Interfaces:**
```typescript
// Query builder interface
interface Query<T = NodeProperties[]> {
  // Starting points
  node(id: string): Query<NodeProperties>;
  nodes(filter?: NodeFilter): Query<NodeProperties[]>;
  edges(filter?: EdgeFilter): Query<Edge[]>;

  // Traversal
  followEdges(opts: EdgeTraversalOptions): Query;
  toNodes(filter?: NodeFilter): Query;
  traverse(opts: TraversalOptions): Query;

  // Filtering
  where(predicate: PredicateFn): Query<T>;
  filter(fn: (item: T) => boolean): Query<T>;

  // Projection
  select(fields: string[]): Query<Array<Record<string, unknown>>>;
  selectSubgraph(): Query<SubgraphResult>;
  selectTree(): Query<TreeResult>;
  selectPaths(): Query<PathResult>;

  // Aggregation
  count(): Query<number>;
  groupBy(field: string): Query<Map<string, T[]>>;

  // Execution
  execute(): Promise<T>;
  stream(): AsyncIterable<T>;

  // Query introspection
  explain(): QueryPlan;
  toString(): string; // Debug representation
}

// Filter types
interface NodeFilter {
  type?: NodeType | NodeType[];
  id?: string | string[];
  [property: string]: unknown; // Property filters
}

interface EdgeFilter {
  type?: EdgeType | EdgeType[];
  fromId?: string | string[];
  toId?: string | string[];
  [property: string]: unknown;
}

interface EdgeTraversalOptions {
  type?: EdgeType | EdgeType[];
  direction: 'incoming' | 'outgoing' | 'both';
  filter?: (edge: Edge) => boolean;
}

interface TraversalOptions extends EdgeTraversalOptions {
  maxDepth?: number;
  minDepth?: number;
  breadthFirst?: boolean; // vs depth-first
}

type PredicateFn = (item: NodeProperties | Edge) => boolean;

// Result types
interface QueryResult<T> {
  data: T;
  meta: {
    executionTime: number;
    nodesScanned: number;
    edgesScanned: number;
  };
}

interface SubgraphResult {
  nodes: Map<string, NodeProperties>;
  edges: Edge[];
  rootNodes: string[];
}

interface TreeResult {
  root: TreeNode;
  maxDepth: number;
}

interface TreeNode {
  id: string;
  properties: NodeProperties;
  children: TreeNode[];
  edge?: Edge;
}

interface PathResult {
  paths: Path[];
  algorithm: string;
}

interface Path {
  nodes: string[];
  edges: string[];
  length: number;
}

interface QueryPlan {
  steps: QueryStep[];
  estimatedCost: number;
}

interface QueryStep {
  operation: string;
  estimatedResults: number;
  indexes?: string[];
}
```

**Execution Model:**
1. Build AST during method chaining (immutable)
2. Optimize query plan at `.execute()`
3. Execute steps sequentially (or optimize to parallel where possible)
4. Return typed results

**Implementation Strategy:**
```typescript
class QueryBuilder<T> implements Query<T> {
  private ast: QueryAST;

  constructor(private graph: Graph, ast: QueryAST = { steps: [] }) {
    this.ast = ast;
  }

  nodes(filter?: NodeFilter): Query<NodeProperties[]> {
    const newAST = {
      ...this.ast,
      steps: [...this.ast.steps, { type: 'nodes', filter }]
    };
    return new QueryBuilder(this.graph, newAST);
  }

  followEdges(opts: EdgeTraversalOptions): Query {
    const newAST = {
      ...this.ast,
      steps: [...this.ast.steps, { type: 'followEdges', opts }]
    };
    return new QueryBuilder(this.graph, newAST);
  }

  async execute(): Promise<T> {
    const plan = this.optimize(this.ast);
    return await this.executeAST(plan);
  }

  private optimize(ast: QueryAST): QueryPlan {
    // TODO: Query optimization
    // - Push filters down
    // - Identify index usage
    // - Reorder joins
    return { steps: ast.steps, estimatedCost: 0 };
  }

  private async executeAST(plan: QueryPlan): Promise<T> {
    let result: any = null;

    for (const step of plan.steps) {
      switch (step.type) {
        case 'nodes':
          result = this.executeNodes(step.filter);
          break;
        case 'followEdges':
          result = this.executeFollowEdges(result, step.opts);
          break;
        // ... other steps
      }
    }

    return result;
  }
}

// Usage in Graph class
class Graph {
  // ... existing methods

  query(): Query {
    return new QueryBuilder(this);
  }
}
```

**CLI Integration:**
```bash
# Simple queries via flags
graph query --nodes type=task state=active

# Fluent syntax as command
graph query "nodes({type:'task'}).followEdges({type:'depends_on'}).toNodes()"

# Or interactive REPL
graph query-repl
> nodes({type: 'task', state: 'active'})
> .followEdges({type: 'depends_on'})
> .toNodes()
> .select(['id', 'title'])
> .execute()
```

---

### 4.2 CSS Selector Specification

**Syntax Examples:**
```
// Basic attribute selectors
node[type=task]
node[type=task][state=active]
node#task_123

// Combinator: direct child (outgoing edge)
node[type=task] > edge[type=spawned_by] > node

// Combinator: descendant (transitive outgoing edges)
node#task_root node[type=knowledge]

// Combinator: with specific edge type
node[type=task] >depends_on> node[type=knowledge]

// Multiple selectors
node[type=task][state=blocked], node[state=failed]

// Pseudo-classes
node[type=task]:has(edge[type=blocks])
node:not([state=completed])

// Property value operators
node[priority>2]
node[labels*=urgent]     // Array contains
node[goal^="Implement"]  // Starts with
node[goal$="API"]        // Ends with
node[goal*="test"]       // Contains

// Edge selectors
edge[type=depends_on][from=task_1]
edge[properties.weight>0.5]
```

**Grammar (PEG.js style):**
```peg
Query
  = Selector (Comma Selector)*

Selector
  = Element Combinator? Selector
  / Element

Element
  = NodeSelector
  / EdgeSelector

NodeSelector
  = "node" ID? AttributeList? PseudoClass*

EdgeSelector
  = "edge" AttributeList? PseudoClass*

ID
  = "#" identifier

AttributeList
  = "[" Attribute "]"+

Attribute
  = identifier Operator value

Operator
  = "=" / "!=" / ">" / "<" / ">=" / "<=" / "*=" / "^=" / "$="

Combinator
  = ">" EdgeType? ">"        // Direct child with optional edge type
  / " "                       // Descendant

EdgeType
  = identifier

PseudoClass
  = ":" identifier "(" ArgumentList? ")"

identifier
  = [a-zA-Z_][a-zA-Z0-9_]*

value
  = quoted_string / number / identifier
```

**TypeScript Interfaces:**
```typescript
// Parsed selector AST
interface SelectorAST {
  type: 'query';
  selectors: Selector[];
}

interface Selector {
  elements: Element[];
  combinators: Combinator[];
}

interface Element {
  type: 'node' | 'edge';
  id?: string;
  attributes: Attribute[];
  pseudoClasses: PseudoClass[];
}

interface Attribute {
  name: string;
  operator: '=' | '!=' | '>' | '<' | '>=' | '<=' | '*=' | '^=' | '$=';
  value: string | number | boolean;
}

interface PseudoClass {
  name: string;
  arguments?: string[];
}

interface Combinator {
  type: 'child' | 'descendant';
  edgeType?: EdgeType;
}

// Query executor
interface SelectorEngine {
  parse(selector: string): SelectorAST;
  execute(ast: SelectorAST, graph: Graph): NodeProperties[] | Edge[];
}
```

**Execution Model:**
1. Parse selector string to AST
2. Evaluate left-to-right (or optimize)
3. For each combinator:
   - `>` : Follow direct edges (1 hop)
   - ` ` : Follow transitive edges (N hops)
4. Apply filters at each step
5. Return matching nodes/edges

**Implementation Strategy:**
```typescript
class SelectorEngine {
  parse(selector: string): SelectorAST {
    // Use parser (PEG.js, custom)
    return parser.parse(selector);
  }

  execute(ast: SelectorAST, graph: Graph): NodeProperties[] {
    const results: Set<string> = new Set();

    for (const selector of ast.selectors) {
      const matches = this.evaluateSelector(selector, graph);
      matches.forEach(id => results.add(id));
    }

    return Array.from(results).map(id =>
      graph.getNodeProperties(id)!
    );
  }

  private evaluateSelector(selector: Selector, graph: Graph): Set<string> {
    let current: Set<string> = this.matchElement(selector.elements[0], graph);

    for (let i = 1; i < selector.elements.length; i++) {
      const combinator = selector.combinators[i - 1];
      const element = selector.elements[i];

      current = this.applyCombinator(current, combinator, element, graph);
    }

    return current;
  }

  private matchElement(element: Element, graph: Graph): Set<string> {
    let candidates: Set<string>;

    if (element.id) {
      candidates = new Set([element.id]);
    } else if (element.type === 'node') {
      candidates = new Set(graph.getNodeIds());
    } else {
      // Edge selector - return edge IDs
      candidates = new Set(/* all edge IDs */);
    }

    // Apply attribute filters
    for (const attr of element.attributes) {
      candidates = this.filterByAttribute(candidates, attr, graph);
    }

    // Apply pseudo-classes
    for (const pseudo of element.pseudoClasses) {
      candidates = this.filterByPseudoClass(candidates, pseudo, graph);
    }

    return candidates;
  }

  private applyCombinator(
    current: Set<string>,
    combinator: Combinator,
    next: Element,
    graph: Graph
  ): Set<string> {
    const results = new Set<string>();

    for (const nodeId of current) {
      if (combinator.type === 'child') {
        // Direct children only
        const edges = graph.getEdgesFrom(nodeId);
        for (const edge of edges) {
          if (!combinator.edgeType || edge.type === combinator.edgeType) {
            if (this.elementMatches(edge.toId, next, graph)) {
              results.add(edge.toId);
            }
          }
        }
      } else if (combinator.type === 'descendant') {
        // Transitive closure
        const descendants = this.getDescendants(nodeId, graph, combinator.edgeType);
        for (const desc of descendants) {
          if (this.elementMatches(desc, next, graph)) {
            results.add(desc);
          }
        }
      }
    }

    return results;
  }
}
```

**CLI Integration:**
```bash
# Direct selector queries
graph query 'node[type=task][state=active]'

# With output format
graph query 'node#task_123 > node' --format table

# Save to file
graph query 'node[type=task]' --output tasks.json
```

---

### 4.3 Datalog Specification

**Syntax Examples:**
```datalog
% Facts (derived from graph)
node(task_1, task, active).
node(know_1, knowledge).
edge(task_1, depends_on, know_1).

% Simple query: find active tasks
?- node(TaskId, task, active).

% Query with join: find knowledge dependencies
?- node(TaskId, task, active),
   edge(TaskId, depends_on, KnowId),
   node(KnowId, knowledge).

% Recursive rule: transitive dependencies
depends_transitively(X, Y) :- edge(X, depends_on, Y).
depends_transitively(X, Z) :-
   edge(X, depends_on, Y),
   depends_transitively(Y, Z).

?- depends_transitively(task_123, Dep).

% Rule: blocked tasks
blocked_task(TaskId, BlockerCount) :-
   node(TaskId, task, blocked),
   findall(Blocker, edge(Blocker, blocks, TaskId), Blockers),
   length(Blockers, BlockerCount).

% Negation: tasks with no dependencies
independent_task(TaskId) :-
   node(TaskId, task, _),
   \+ edge(TaskId, depends_on, _).

% Aggregation: count tasks by state
count_by_state(State, Count) :-
   findall(T, node(T, task, State), Tasks),
   length(Tasks, Count).
```

**TypeScript Interfaces:**
```typescript
// Datalog database
interface DatalogDB {
  // Assert facts
  assertFact(predicate: string, ...args: (string | number)[]): void;

  // Define rules
  defineRule(head: Term, body: Term[]): void;

  // Query
  query(goal: Term): Iterator<Substitution>;
  queryAll(goal: Term): Substitution[];

  // Retract
  retractFact(predicate: string, ...args: (string | number)[]): void;
}

interface Term {
  type: 'atom' | 'variable' | 'compound';
  functor?: string;
  args?: Term[];
  value?: string | number;
}

type Substitution = Map<string, string | number>;

// Graph to Datalog adapter
interface GraphDatalogAdapter {
  // Load graph into Datalog
  loadGraph(graph: Graph): DatalogDB;

  // Query with Datalog, return graph results
  query(db: DatalogDB, query: string): NodeProperties[];

  // Define custom rules
  defineRule(db: DatalogDB, rule: string): void;
}
```

**Execution Model:**
1. Convert graph to Datalog facts on load
2. User provides rules and queries
3. Datalog engine executes (backward chaining or forward chaining)
4. Results (substitutions) are mapped back to graph nodes
5. Support both queries and rules

**Implementation Strategy:**
```typescript
import datalog from 'datalog-js'; // or datascript

class GraphDatalogAdapter {
  loadGraph(graph: Graph): DatalogDB {
    const db = new DatalogDB();

    // Assert node facts: node(id, type, state, ...)
    for (const nodeId of graph.getNodeIds()) {
      const props = graph.getNodeProperties(nodeId)!;

      // Simple fact: node(id, type)
      db.assertFact('node', nodeId, props.type);

      // Properties as separate facts
      if (props.type === 'task') {
        const task = props as TaskProperties;
        db.assertFact('task_state', nodeId, task.state);
        db.assertFact('task_goal', nodeId, task.goal);
        // ... other properties
      }
    }

    // Assert edge facts: edge(from, type, to)
    const { edges } = graph.dump();
    for (const edge of edges) {
      db.assertFact('edge', edge.fromId, edge.type, edge.toId);
    }

    // Define common rules
    this.defineCommonRules(db);

    return db;
  }

  private defineCommonRules(db: DatalogDB) {
    // Transitive dependencies
    db.defineRule(
      { type: 'compound', functor: 'depends_transitively', args: [
        { type: 'variable', value: 'X' },
        { type: 'variable', value: 'Y' }
      ]},
      [
        { type: 'compound', functor: 'edge', args: [
          { type: 'variable', value: 'X' },
          { type: 'atom', value: 'depends_on' },
          { type: 'variable', value: 'Y' }
        ]}
      ]
    );

    // Recursive case
    db.defineRule(
      { type: 'compound', functor: 'depends_transitively', args: [
        { type: 'variable', value: 'X' },
        { type: 'variable', value: 'Z' }
      ]},
      [
        { type: 'compound', functor: 'edge', args: [
          { type: 'variable', value: 'X' },
          { type: 'atom', value: 'depends_on' },
          { type: 'variable', value: 'Y' }
        ]},
        { type: 'compound', functor: 'depends_transitively', args: [
          { type: 'variable', value: 'Y' },
          { type: 'variable', value: 'Z' }
        ]}
      ]
    );

    // Add more common rules...
  }

  query(db: DatalogDB, queryStr: string): NodeProperties[] {
    // Parse query string (simplified)
    const goal = this.parseQuery(queryStr);

    // Execute query
    const results = db.queryAll(goal);

    // Map substitutions back to nodes
    const nodeIds = new Set<string>();
    for (const subst of results) {
      // Extract node IDs from substitution
      for (const [variable, value] of subst) {
        if (typeof value === 'string' && value.startsWith('task_') || value.startsWith('know_')) {
          nodeIds.add(value);
        }
      }
    }

    return Array.from(nodeIds).map(id =>
      graph.getNodeProperties(id)!
    ).filter(Boolean);
  }
}
```

**CLI Integration:**
```bash
# Load graph and enter Datalog REPL
graph datalog

# Query from command line
graph datalog query "node(TaskId, task, active)"

# Define rule and query
graph datalog --rule "depends_transitively(X,Y) :- edge(X,depends_on,Y)"
graph datalog query "depends_transitively(task_123, Dep)"

# Load rules from file
graph datalog --rules rules.dl --query "blocked_task(T, Count)"
```

**Example Rules File:**
```datalog
% rules.dl

% Transitive dependencies
depends_transitively(X, Y) :- edge(X, depends_on, Y).
depends_transitively(X, Z) :-
   edge(X, depends_on, Y),
   depends_transitively(Y, Z).

% Critical path (tasks with no dependents)
critical_task(TaskId) :-
   node(TaskId, task, _),
   \+ edge(_, depends_on, TaskId).

% Ready to work (no blockers)
ready_task(TaskId) :-
   node(TaskId, task, _),
   \+ edge(_, blocks, TaskId).
```

---

## 5. Library Inspiration

### 5.1 graphology

**Link:** https://graphology.github.io/
**Type:** In-memory graph library
**Language:** JavaScript/TypeScript

**Features:**
- Directed, undirected, mixed graphs
- Node/edge attributes
- Graph traversal methods
- Serialization (JSON, GEXF)
- Built-in algorithms (BFS, DFS, shortest path)

**Example:**
```typescript
import Graph from 'graphology';

const graph = new Graph();

graph.addNode('task_1', { type: 'task', state: 'active' });
graph.addNode('know_1', { type: 'knowledge' });
graph.addEdge('task_1', 'know_1', { type: 'depends_on' });

// Traversal
graph.forEachNode((node, attrs) => {
  console.log(node, attrs);
});

// Filter edges
const edges = graph.filterEdges((edge, attrs) =>
  attrs.type === 'depends_on'
);

// BFS traversal
import { bfs } from 'graphology-traversal';
bfs(graph, 'task_1', (node, attr) => {
  console.log(node);
});
```

**Pros:**
- Mature, well-tested
- Good TypeScript support
- Many algorithms available
- Active community

**Cons:**
- No query language
- Simple attribute model (not node types)
- No actor model integration

**Relevance:** Could use as underlying graph storage, build query layer on top.

---

### 5.2 cytoscape.js

**Link:** https://js.cytoscape.org/
**Type:** Graph visualization + query library
**Language:** JavaScript

**Features:**
- Graph visualization (web-based)
- Selector-based queries (jQuery-like)
- Graph algorithms
- Layouts (force-directed, hierarchical)
- Interactive events

**Example:**
```javascript
const cy = cytoscape({
  elements: [
    { data: { id: 'task_1', type: 'task' } },
    { data: { id: 'know_1', type: 'knowledge' } },
    { data: { id: 'e1', source: 'task_1', target: 'know_1' } }
  ]
});

// jQuery-style selectors
cy.nodes('[type = "task"]').forEach(node => {
  console.log(node.data());
});

// Complex selectors
cy.nodes('#task_1').outgoers('edge[type = "depends_on"]');

// Algorithms
const shortestPath = cy.elements().aStar({
  root: cy.$('#task_1'),
  goal: cy.$('#know_1')
});
```

**Pros:**
- Excellent selector syntax (proven)
- Visualization capabilities
- Rich API
- Good documentation

**Cons:**
- Heavy (visualization overhead)
- Selector syntax is jQuery-like, not CSS exactly
- No TypeScript types (community types available)

**Relevance:** Could borrow selector syntax, not visualization parts.

---

### 5.3 DataScript (Clojure/JavaScript)

**Link:** https://github.com/tonsky/datascript
**Type:** Immutable in-memory database with Datalog
**Language:** ClojureScript (JavaScript port)

**Features:**
- Datalog query language
- Immutable database
- Time-travel (historical queries)
- Reactive subscriptions
- Schema-less (flexible attributes)

**Example:**
```javascript
import { datascript as d } from 'datascript';

// Create database
const conn = d.create_conn();

// Add data
d.transact(conn, [
  { ':db/id': -1, name: 'task_1', type: 'task', state: 'active' },
  { ':db/id': -2, name: 'know_1', type: 'knowledge' },
  { ':db/id': -3, edge_from: -1, edge_to: -2, edge_type: 'depends_on' }
]);

// Datalog query
const results = d.q(
  `[:find ?task
    :where
    [?task :type "task"]
    [?task :state "active"]]`,
  d.db(conn)
);
```

**Pros:**
- Proven Datalog implementation
- Immutable (great for state management)
- Time-travel queries
- Reactive updates

**Cons:**
- ClojureScript-centric (syntax)
- Learning curve (Datalog + EDN format)
- Heavyweight for simple use cases

**Relevance:** Excellent reference for Datalog implementation, consider JS port.

---

### 5.4 datalog-js

**Link:** https://github.com/borgar/datalog-js
**Type:** Pure JavaScript Datalog engine
**Language:** JavaScript

**Features:**
- Lightweight Datalog interpreter
- Facts, rules, queries
- Unification and backtracking
- No dependencies

**Example:**
```javascript
import { Datalog } from 'datalog-js';

const db = new Datalog();

// Assert facts
db.assert('node', 'task_1', 'task');
db.assert('node', 'know_1', 'knowledge');
db.assert('edge', 'task_1', 'depends_on', 'know_1');

// Define rules
db.rule('depends_transitively', ['X', 'Y'], [
  ['edge', 'X', 'depends_on', 'Y']
]);

db.rule('depends_transitively', ['X', 'Z'], [
  ['edge', 'X', 'depends_on', 'Y'],
  ['depends_transitively', 'Y', 'Z']
]);

// Query
const results = db.query('depends_transitively', 'task_1', '_');
```

**Pros:**
- Pure JS, no dependencies
- Simple API
- Lightweight

**Cons:**
- Less mature than DataScript
- Limited documentation
- No TypeScript types

**Relevance:** Good starting point for Datalog Phase 3, easy to integrate.

---

### 5.5 rdflib.js

**Link:** https://github.com/linkeddata/rdflib.js
**Type:** RDF library with SPARQL support
**Language:** JavaScript

**Features:**
- RDF triple store
- SPARQL 1.1 queries
- Linked data support
- Turtle, N3, RDF/XML parsers

**Example:**
```javascript
import { graph, parse, query } from 'rdflib';

const store = graph();

// Add triples
const doc = 'http://example.org#';
store.add(
  sym(`${doc}task_1`),
  sym(`${doc}type`),
  literal('task')
);

// SPARQL query
const sparql = `
  SELECT ?task WHERE {
    ?task <${doc}type> "task" .
    ?task <${doc}state> "active" .
  }
`;

const results = query(store, sparql);
```

**Pros:**
- Full SPARQL support
- Standards-compliant
- Semantic web ecosystem

**Cons:**
- RDF overhead (triples)
- Complex for simple graphs
- Verbose queries

**Relevance:** Overkill for our use case, but reference for SPARQL if needed.

---

### 5.6 gremlin-javascript

**Link:** https://www.npmjs.com/package/gremlin
**Type:** Official Gremlin JavaScript driver
**Language:** JavaScript

**Features:**
- Gremlin query language
- Connect to TinkerPop-enabled databases
- Fluent traversal API
- GraphSON serialization

**Example:**
```javascript
import { driver } from 'gremlin';

const traversal = driver.process.AnonymousTraversalSource.traversal;
const g = traversal().withRemote(connection);

// Traversal query
const results = await g.V()
  .hasLabel('task')
  .has('state', 'active')
  .out('depends_on')
  .hasLabel('knowledge')
  .values('id', 'title')
  .toList();
```

**Pros:**
- Industry standard (Apache)
- Powerful traversal API
- Well-documented

**Cons:**
- Requires TinkerPop server
- Heavyweight
- Not designed for in-memory graphs

**Relevance:** Good reference for traversal API design, but server dependency is blocker.

---

### 5.7 sql.js / alasql

**Link:** https://github.com/sql-js/sql.js, https://github.com/alasql/alasql
**Type:** SQL database in JavaScript
**Language:** JavaScript (SQLite WASM)

**Features:**
- Full SQL queries
- In-memory or persistent
- Transactions
- Mature query optimizer

**Example:**
```javascript
import initSqlJs from 'sql.js';

const SQL = await initSqlJs();
const db = new SQL.Database();

// Create tables
db.run(`
  CREATE TABLE nodes (id TEXT, type TEXT, state TEXT);
  CREATE TABLE edges (fromId TEXT, toId TEXT, type TEXT);
`);

// Insert data
db.run(`INSERT INTO nodes VALUES ('task_1', 'task', 'active')`);

// Query
const results = db.exec(`
  SELECT n.id, n.state, k.id as knowId
  FROM nodes n
  JOIN edges e ON e.fromId = n.id
  JOIN nodes k ON k.id = e.toId
  WHERE n.type = 'task' AND n.state = 'active'
    AND e.type = 'depends_on'
`);
```

**Pros:**
- SQL is widely known
- Excellent query optimizer
- Mature, stable
- Full transaction support

**Cons:**
- Relational model awkward for graphs
- Recursive queries limited (CTEs)
- Need to flatten graph into tables
- Not graph-native

**Relevance:** Could use for storage layer, but query model doesn't fit graphs well.

---

### 5.8 JOINs on JSON (AlaSQL)

**Link:** https://github.com/AlaSQL/alasql
**Type:** SQL on JavaScript arrays/objects
**Language:** JavaScript

**Features:**
- SQL syntax on JavaScript data
- JOINs, aggregations, GROUP BY
- No database needed
- Export to CSV, Excel

**Example:**
```javascript
import alasql from 'alasql';

const nodes = [
  { id: 'task_1', type: 'task', state: 'active' },
  { id: 'know_1', type: 'knowledge', title: 'Knowledge 1' }
];

const edges = [
  { fromId: 'task_1', toId: 'know_1', type: 'depends_on' }
];

// SQL query on arrays
const results = alasql(`
  SELECT n.id, n.state, k.title
  FROM ? n
  JOIN ? e ON e.fromId = n.id
  JOIN ? k ON k.id = e.toId
  WHERE n.type = 'task' AND n.state = 'active'
`, [nodes, edges, nodes]);
```

**Pros:**
- No database setup
- SQL familiarity
- Works on plain arrays
- Good for reports

**Cons:**
- Not graph-optimized
- Performance limited
- Relational thinking required

**Relevance:** Could use for report generation, but not core query engine.

---

### 5.9 JSONPath / JMESPath

**Link:** https://goessner.net/articles/JsonPath/, https://jmespath.org/
**Type:** Query languages for JSON
**Language:** Language-agnostic (many implementations)

**Features:**
- XPath-like for JSON
- Filter, slice, project
- Recursive descent

**Example (JSONPath):**
```javascript
import jsonpath from 'jsonpath';

const data = {
  nodes: [
    { id: 'task_1', type: 'task', state: 'active' },
    { id: 'task_2', type: 'task', state: 'blocked' }
  ]
};

// Find all active tasks
const tasks = jsonpath.query(data, '$.nodes[?(@.state=="active")]');

// Recursive search
const ids = jsonpath.query(data, '$..id');
```

**Pros:**
- Simple, lightweight
- Good for nested JSON
- Standard (JSONPath)

**Cons:**
- Not graph-aware
- Limited traversal
- No JOIN operations

**Relevance:** Could use for property extraction, but not graph traversal.

---

### 5.10 TypeORM / Prisma (Query Builders)

**Link:** https://typeorm.io/, https://www.prisma.io/
**Type:** TypeScript ORM with query builder
**Language:** TypeScript

**Features:**
- Type-safe query building
- Fluent API
- Migrations
- Multiple database support

**Example (TypeORM):**
```typescript
import { getRepository } from 'typeorm';

const tasks = await getRepository(Task)
  .createQueryBuilder('task')
  .where('task.state = :state', { state: 'active' })
  .leftJoinAndSelect('task.dependencies', 'knowledge')
  .getMany();
```

**Pros:**
- Excellent TypeScript integration
- Type-safe
- Proven patterns
- Good DX

**Cons:**
- Designed for relational DBs
- Requires entities/models
- Not graph-native

**Relevance:** Great inspiration for Fluent API design (Phase 1).

---

## 6. Recommendations

### Phase 1 (Immediate): Fluent API

**Why:**
- Lowest implementation effort (2-3 days)
- Native TypeScript - full type safety and IDE support
- No dependencies, no parser
- Can iterate quickly based on feedback
- Natural for both programmatic use and CLI

**Timeline:** 2-3 days

**Deliverables:**
1. `QueryBuilder` class with fluent methods
2. TypeScript interfaces for query API
3. Basic execution engine (no optimization yet)
4. CLI integration: `graph query --builder` or REPL
5. Documentation and examples

**Example Queries:**
```typescript
// Find blocked tasks with blockers
const result = await graph.query()
  .nodes({ type: 'task', state: 'blocked' })
  .withEdges({ type: 'blocks', direction: 'incoming' })
  .asSubgraph()
  .execute();

// Task dependency chain
const chain = await graph.query()
  .node('task_123')
  .traverse({ type: 'depends_on', maxDepth: 5 })
  .selectPaths()
  .execute();
```

**CLI Usage:**
```bash
# Interactive REPL
graph query-repl
> nodes({ type: 'task', state: 'active' })
> .followEdges({ type: 'depends_on' })
> .toNodes()
> .execute()

# Programmatic
bun run query.ts
```

**Success Metrics:**
- Users can express common queries easily
- Type errors caught at compile time
- CLI provides instant feedback
- Query performance acceptable (<100ms for typical graphs)

---

### Phase 2 (Medium-term): CSS Selectors

**Why:**
- Familiar syntax for web developers
- Very concise (one-liner queries)
- Good for CLI power users
- Pattern matching is intuitive
- Can coexist with Fluent API

**Timeline:** 1-2 weeks

**Deliverables:**
1. PEG grammar for selector syntax
2. Parser (using `peggy` or `nearley`)
3. AST to query executor
4. CLI integration: `graph query 'selector'`
5. Documentation with examples

**Example Queries:**
```bash
# CLI usage
graph query 'node[type=task][state=active]'
graph query 'node#task_123 > edge[type=spawned_by] > node'
graph query 'node[type=task]:has(edge[type=blocks])'

# Output formats
graph query 'node[type=task]' --format table
graph query 'node[type=task]' --format json
graph query 'node[type=task]' --format tree
```

**Success Metrics:**
- 90% of common queries expressible in one line
- Parser errors are clear and actionable
- Performance comparable to Fluent API
- Users report high satisfaction (conciseness)

---

### Phase 3 (Long-term): Datalog

**Why:**
- Most powerful for recursive/transitive queries
- Logic programming is elegant for graphs
- Can express complex constraints
- Growing developer interest
- Future-proof for advanced use cases

**Timeline:** 3-4 weeks

**Deliverables:**
1. Integration with `datalog-js` or custom engine
2. Graph-to-Datalog adapter
3. Common rule library (transitive deps, etc.)
4. CLI: `graph datalog` subcommand
5. Rule file format (.dl)
6. Documentation and tutorials

**Example Usage:**
```bash
# Interactive Datalog REPL
graph datalog
?- node(TaskId, task, active).

# Load rules from file
graph datalog --rules rules.dl

# Query with rule
graph datalog query "depends_transitively(task_123, Dep)"
```

**Example Rules:**
```datalog
% rules.dl

% Transitive dependencies
depends_transitively(X, Y) :- edge(X, depends_on, Y).
depends_transitively(X, Z) :-
   edge(X, depends_on, Y),
   depends_transitively(Y, Z).

% Critical path analysis
critical_path_task(TaskId) :-
   node(TaskId, task, _),
   depends_transitively(_, TaskId),
   \+ depends_transitively(TaskId, _).
```

**Success Metrics:**
- Recursive queries work correctly
- Performance acceptable for typical graphs (<500ms)
- Rule library covers common patterns
- Users can define custom rules easily

---

## 7. Next Steps

### Immediate Actions (This Week)

1. **Implement Fluent API (Phase 1)**
   - [ ] Create `src/query/QueryBuilder.ts`
   - [ ] Define TypeScript interfaces
   - [ ] Implement basic methods: `nodes()`, `edges()`, `followEdges()`, `select()`
   - [ ] Add result formatters (tuple, subgraph, tree, path)
   - [ ] Write unit tests
   - [ ] Update Graph class with `.query()` method
   - [ ] Create CLI command: `graph query-repl`
   - [ ] Write documentation and examples

2. **Validate with Real Use Cases**
   - [ ] Test with task CLI queries
   - [ ] Get user feedback on API
   - [ ] Identify missing features
   - [ ] Refine interfaces

3. **Plan Phase 2**
   - [ ] Design CSS selector syntax (adapt from cytoscape.js)
   - [ ] Choose parser generator (recommend `peggy`)
   - [ ] Create grammar spec
   - [ ] Prototype parser

### Medium-term (Next 2-4 Weeks)

4. **Implement CSS Selectors (Phase 2)**
   - [ ] Build parser with `peggy`
   - [ ] Create AST types
   - [ ] Implement executor
   - [ ] CLI integration
   - [ ] Performance testing
   - [ ] Documentation

5. **Query Optimization**
   - [ ] Add query planning
   - [ ] Implement indexes (by type, state, etc.)
   - [ ] Measure and optimize hotspots
   - [ ] Consider caching

### Long-term (1-3 Months)

6. **Datalog Integration (Phase 3)**
   - [ ] Evaluate `datalog-js` vs custom engine
   - [ ] Build graph adapter
   - [ ] Define common rules
   - [ ] CLI Datalog REPL
   - [ ] Rule file format
   - [ ] Advanced tutorials

7. **Advanced Features**
   - [ ] Query composition (combine results)
   - [ ] Materialized views (cached query results)
   - [ ] Subscriptions (reactive queries)
   - [ ] Query analytics (which queries are slow)
   - [ ] Query suggestions (autocomplete)

---

## Appendix A: Performance Considerations

### Indexing Strategies

For in-memory graphs, maintain indexes:
- **By type:** `Map<NodeType, Set<string>>`
- **By state:** `Map<TaskState, Set<string>>`
- **By edge type:** `Map<EdgeType, Edge[]>`

Update indexes on mutations.

### Query Optimization Techniques

1. **Filter pushdown:** Apply filters early to reduce intermediate sets
2. **Index selection:** Use indexes when available
3. **Join ordering:** Start with most selective filter
4. **Short-circuit evaluation:** Stop when result set is sufficient
5. **Lazy evaluation:** Don't compute until `.execute()`

### Memory Management

- Use generators for large result sets
- Implement streaming for subgraph results
- Consider pagination for CLI display

### Benchmarking

Target performance:
- **Simple filter (by type):** <1ms
- **Edge traversal (1-hop):** <5ms
- **Transitive traversal (N-hops):** <50ms
- **Complex query (multiple joins):** <100ms

For graphs up to 10,000 nodes, 50,000 edges.

---

## Appendix B: Alternative Result Format Details

### CSV Export
```typescript
interface CSVFormatter {
  format(result: TupleResult): string;
}

// Usage
const csv = new CSVFormatter().format(result);
console.log(csv);
// Output:
// id,type,state
// task_1,task,active
// task_2,task,blocked
```

### ASCII Table
```typescript
interface TableFormatter {
  format(result: TupleResult): string;
}

// Usage
const table = new TableFormatter().format(result);
console.log(table);
// Output:
// ┌─────────┬──────┬─────────┐
// │ id      │ type │ state   │
// ├─────────┼──────┼─────────┤
// │ task_1  │ task │ active  │
// │ task_2  │ task │ blocked │
// └─────────┴──────┴─────────┘
```

### Mermaid Diagram
```typescript
interface MermaidFormatter {
  format(result: SubgraphResult): string;
}

// Usage
const mermaid = new MermaidFormatter().format(result);
console.log(mermaid);
// Output:
// graph TD
//   task_1["task_1<br/>task"]
//   know_1["know_1<br/>knowledge"]
//   task_1 -->|depends_on| know_1
```

---

## Conclusion

This research provides a comprehensive foundation for enhancing the tk-agents graph query capabilities. The three-phase approach balances:

1. **Immediate value** (Fluent API) - ships fast, TypeScript-native
2. **Power user productivity** (CSS Selectors) - concise, familiar
3. **Future-proof expressiveness** (Datalog) - handles complex recursive queries

Start with Phase 1, validate with users, then incrementally add Phase 2 and 3 as needs arise.

**Key Takeaway:** Don't build all three at once. Ship Fluent API first, gather feedback, then decide if Phase 2/3 are needed.
