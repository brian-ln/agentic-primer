# BeadActor Architecture: Tasks as Computational Actors

## Overview

**Vision:** Unify task management (beads) with actor computation (simplify) by making tasks first-class actors in the computation graph. Tasks become executable, message-driven nodes that auto-execute when dependencies complete.

**Core Insight:** "Beads doesn't think about what simplify is. Beads is task management, not an information graph that is message-driven with actor-based nodes that can do computation, manage knowledge, or interface with other systems. We could define and execute processes if we could express bead task graphs and formulas on the simplify fabric."

**Current State:**
- Beads: Passive JSONL metadata + bd CLI queries
- Simplify: Active actor system (message-driven, reactive, supervision)
- Gap: Tasks are metadata, not executable computational nodes

**Target State:**
- BeadActors: Tasks as actors in the simplify computation graph
- Auto-execution: When dependencies complete, dependent beads execute
- Knowledge integration: BeadActors manage domain knowledge
- External sync: BeadActors interface with GitHub, Slack, etc.

---

## Architecture Design

### 1. BeadActor Core Interface

BeadActors extend the Actor base class with task-specific metadata and lifecycle.

```typescript
/**
 * BeadActor - Task as executable actor
 *
 * Unifies task metadata (status, priority, dependencies) with actor
 * execution model (messages, supervision, ports).
 */
interface BeadActor extends SupervisedActor {
  // Core Actor Protocol
  readonly address: Address;
  receive(msg: Message): Promise<MessageResponse>;
  port?(name: string): Channel<any>;

  // Supervision Lifecycle
  preRestart?(error: Error, message?: Message): Promise<BeadCheckpoint>;
  postRestart?(checkpoint?: BeadCheckpoint): Promise<void>;
  healthCheck?(): Promise<boolean>;

  // Task Metadata (from JSONL schema)
  readonly metadata: BeadMetadata;

  // Task Lifecycle
  readonly status: BeadStatus;
  readonly dependencies: Address[];  // Other BeadActors this depends on

  // Execution Model
  readonly executor: BeadExecutor;   // Pluggable work function

  // Observable Progress
  port(name: 'progress' | 'status' | 'results'): Channel<BeadEvent>;
}

/**
 * Bead Metadata - Preserves existing bead schema
 */
interface BeadMetadata {
  id: string;                       // simplify-xyz
  title: string;
  description: string;
  status: BeadStatus;
  priority: number;                 // 0=P0, 1=P1, 2=P2, etc.
  issue_type: 'feature' | 'bug' | 'task' | 'epic';
  owner: string;
  estimated_minutes?: number;
  created_at: string;
  created_by: string;
  updated_at: string;
  closed_at?: string;
  close_reason?: string;
  tags?: string[];
  notes?: string;
}

/**
 * Bead Status - Task lifecycle states
 */
type BeadStatus =
  | 'backlog'      // Not started, dependencies may not be ready
  | 'ready'        // All dependencies complete, can execute
  | 'in_progress'  // Currently executing
  | 'blocked'      // Dependencies not complete
  | 'suspended'    // Paused (manual intervention)
  | 'completed'    // Execution successful
  | 'failed'       // Execution failed (retryable)
  | 'closed';      // Permanently closed (not retryable)

/**
 * Bead Executor - Pluggable work function
 *
 * Defines what "execution" means for this bead.
 */
interface BeadExecutor {
  /**
   * Execute the bead's work.
   *
   * @param bead - The bead actor instance
   * @param context - Execution context (dependencies, config)
   * @returns Execution result
   */
  execute(
    bead: BeadActor,
    context: ExecutionContext
  ): Promise<ExecutionResult>;

  /**
   * Estimate execution time in milliseconds.
   * Used for progress tracking and timeout calculation.
   */
  estimateTime?(): number;

  /**
   * Validate bead can execute (check preconditions).
   * Returns error message if invalid, undefined if valid.
   */
  validate?(bead: BeadActor): Promise<string | undefined>;
}

/**
 * Execution Context - Data available during execution
 */
interface ExecutionContext {
  /**
   * Results from dependency beads.
   * Map of dependency bead ID to its execution result.
   */
  dependencies: Map<string, ExecutionResult>;

  /**
   * Graph storage for reading/writing knowledge.
   */
  graph: GraphStore;

  /**
   * Message router for actor communication.
   */
  router: MessageRouter;

  /**
   * Configuration from bead metadata or environment.
   */
  config: Record<string, any>;

  /**
   * Cancellation signal for long-running work.
   */
  signal: AbortSignal;
}

/**
 * Execution Result - Output from bead execution
 */
interface ExecutionResult {
  /**
   * Execution success/failure.
   */
  success: boolean;

  /**
   * Result data (available to dependent beads).
   */
  data?: any;

  /**
   * Error if execution failed.
   */
  error?: Error;

  /**
   * Execution duration in milliseconds.
   */
  duration: number;

  /**
   * Execution timestamp.
   */
  timestamp: number;

  /**
   * Optional artifacts (files, URLs, etc.).
   */
  artifacts?: Artifact[];
}

/**
 * Bead Checkpoint - State saved during restart
 */
interface BeadCheckpoint {
  metadata: BeadMetadata;
  status: BeadStatus;
  partialResults?: any;
  queuedMessages: Message[];
  timestamp: number;
}

/**
 * Bead Event - Progress/status updates via ports
 */
type BeadEvent =
  | { type: 'status-changed'; from: BeadStatus; to: BeadStatus }
  | { type: 'progress'; percent: number; message: string }
  | { type: 'dependency-completed'; dependencyId: string }
  | { type: 'execution-started'; timestamp: number }
  | { type: 'execution-completed'; result: ExecutionResult }
  | { type: 'execution-failed'; error: Error };
```

---

### 2. Message Protocol for Dependency Completion

BeadActors communicate via messages to coordinate dependency-driven execution.

#### Message Types

```typescript
/**
 * BeadMessage - Bead-specific message types
 */
type BeadMessage =
  // Execution control
  | { type: 'bead:execute'; force?: boolean }           // Trigger execution
  | { type: 'bead:cancel' }                             // Cancel execution
  | { type: 'bead:suspend'; reason: string }            // Suspend bead
  | { type: 'bead:resume' }                             // Resume suspended bead

  // Dependency tracking
  | { type: 'bead:dependency-completed'; dependencyId: string; result: ExecutionResult }
  | { type: 'bead:dependency-failed'; dependencyId: string; error: Error }

  // Status queries
  | { type: 'bead:get-status' }                         // Get current status
  | { type: 'bead:get-dependencies' }                   // Get dependency graph
  | { type: 'bead:get-result' }                         // Get execution result

  // Formula evaluation (for formula beads)
  | { type: 'bead:evaluate-formula'; inputs: Map<string, any> };
```

#### Dependency Completion Flow

When a BeadActor completes execution:

```
BeadActor A completes
       │
       ▼
A.postExecution()
       │
       ├─► Store result in graph
       ├─► Emit 'execution-completed' port event
       └─► For each dependent B:
             │
             ▼
           B.receive({
             type: 'bead:dependency-completed',
             dependencyId: A.id,
             result: A.result
           })
             │
             ▼
           B.checkReady()
             │
             ├─► If all dependencies complete:
             │     └─► B.execute()
             │
             └─► Else: update internal dependency map
```

---

### 3. Execution Model

BeadActors support multiple execution patterns via pluggable executors.

#### Execution Patterns

**1. Manual Execution (Default)**

Bead executes only when explicitly triggered (via message or UI).

```typescript
class ManualExecutor implements BeadExecutor {
  async execute(bead: BeadActor, context: ExecutionContext): Promise<ExecutionResult> {
    // Wait for manual trigger (user sends 'bead:execute' message)
    return { success: true, data: { status: 'manual-trigger-required' }, duration: 0, timestamp: Date.now() };
  }
}
```

**2. Auto Execution**

Bead auto-executes when all dependencies complete.

```typescript
class AutoExecutor implements BeadExecutor {
  constructor(private workFn: (context: ExecutionContext) => Promise<any>) {}

  async execute(bead: BeadActor, context: ExecutionContext): Promise<ExecutionResult> {
    const start = Date.now();
    try {
      const data = await this.workFn(context);
      return {
        success: true,
        data,
        duration: Date.now() - start,
        timestamp: start
      };
    } catch (error: any) {
      return {
        success: false,
        error,
        duration: Date.now() - start,
        timestamp: start
      };
    }
  }
}

// Example: Run tests when code is ready
const testBead = new BeadActor({
  metadata: { id: 'simplify-test-123', title: 'Run unit tests', ... },
  executor: new AutoExecutor(async (ctx) => {
    const result = await runCommand('bun test');
    return { exitCode: result.exitCode, output: result.stdout };
  }),
  dependencies: [address('simplify-code-456')]  // Depends on code bead
});
```

**3. Formula Execution**

Bead computes a value from dependency results (pure function).

```typescript
class FormulaExecutor implements BeadExecutor {
  constructor(private formula: (inputs: Map<string, any>) => any) {}

  async execute(bead: BeadActor, context: ExecutionContext): Promise<ExecutionResult> {
    const start = Date.now();
    const inputs = new Map();

    // Collect dependency results
    for (const [depId, result] of context.dependencies) {
      inputs.set(depId, result.data);
    }

    const data = this.formula(inputs);
    return {
      success: true,
      data,
      duration: Date.now() - start,
      timestamp: start
    };
  }
}

// Example: Aggregate test results from multiple suites
const aggregateBead = new BeadActor({
  metadata: { id: 'simplify-agg-789', title: 'Aggregate test results', ... },
  executor: new FormulaExecutor((inputs) => {
    const totalTests = Array.from(inputs.values()).reduce((sum, r) => sum + r.count, 0);
    const totalFailures = Array.from(inputs.values()).reduce((sum, r) => sum + r.failures, 0);
    return { totalTests, totalFailures, pass: totalFailures === 0 };
  }),
  dependencies: [address('unit-tests'), address('integration-tests')]
});
```

**4. Knowledge Management**

Bead manages domain knowledge, responds to semantic queries.

```typescript
class KnowledgeExecutor implements BeadExecutor {
  constructor(private knowledge: KnowledgeGraph) {}

  async execute(bead: BeadActor, context: ExecutionContext): Promise<ExecutionResult> {
    // Execution = extract patterns from dependencies
    const patterns = await this.extractPatterns(context.dependencies);
    await this.knowledge.index(patterns);

    return {
      success: true,
      data: { patternsExtracted: patterns.length },
      duration: 0,
      timestamp: Date.now()
    };
  }

  // Knowledge beads also respond to query messages
  async query(queryText: string): Promise<any[]> {
    return this.knowledge.search(queryText);
  }
}
```

**5. External Integration**

Bead synchronizes with external systems (GitHub, Slack, etc.).

```typescript
class GitHubSyncExecutor implements BeadExecutor {
  constructor(private github: GitHubClient) {}

  async execute(bead: BeadActor, context: ExecutionContext): Promise<ExecutionResult> {
    // Sync bead metadata to GitHub issue
    const issue = await this.github.updateIssue(bead.metadata.id, {
      title: bead.metadata.title,
      body: bead.metadata.description,
      state: bead.metadata.status === 'closed' ? 'closed' : 'open',
      labels: [bead.metadata.issue_type, `P${bead.metadata.priority}`]
    });

    return {
      success: true,
      data: { issueUrl: issue.html_url },
      duration: 0,
      timestamp: Date.now()
    };
  }
}
```

---

### 4. Dependency Resolution Algorithm

BeadActors track dependency completion to determine readiness.

#### BeadActor Internal State

```typescript
class BaseBeadActor extends Actor implements BeadActor {
  private dependencyResults = new Map<string, ExecutionResult>();
  private _status: BeadStatus = 'backlog';

  /**
   * Receive message - handle dependency completion
   */
  async receive(msg: Message): Promise<MessageResponse> {
    switch (msg.type) {
      case 'bead:dependency-completed': {
        const { dependencyId, result } = msg.payload;
        this.dependencyResults.set(dependencyId, result);

        // Check if all dependencies complete
        if (this.allDependenciesComplete()) {
          await this.transitionToReady();

          // Auto-execute if executor supports it
          if (this.executor.autoExecute !== false) {
            await this.execute();
          }
        }

        return createResponse(msg, { status: this._status });
      }

      case 'bead:execute': {
        if (this._status !== 'ready' && !msg.payload.force) {
          return createErrorResponse(msg, 'Bead not ready (dependencies incomplete)');
        }

        const result = await this.execute();
        return createResponse(msg, result);
      }

      case 'bead:get-status': {
        return createResponse(msg, {
          status: this._status,
          dependencies: this.getDependencyStatus(),
          result: this.lastResult
        });
      }

      default:
        return createErrorResponse(msg, `Unknown message type: ${msg.type}`);
    }
  }

  /**
   * Check if all dependencies have completed
   */
  private allDependenciesComplete(): boolean {
    return this.dependencies.every(depAddr => {
      const depId = parseAddress(depAddr);
      const result = this.dependencyResults.get(depId);
      return result && result.success;
    });
  }

  /**
   * Transition to ready state and notify dependents
   */
  private async transitionToReady(): Promise<void> {
    const oldStatus = this._status;
    this._status = 'ready';

    // Emit status change event
    await this.getPort('status').send({
      type: 'status-changed',
      from: oldStatus,
      to: 'ready'
    });
  }

  /**
   * Execute the bead's work
   */
  private async execute(): Promise<ExecutionResult> {
    this._status = 'in_progress';

    // Emit execution started
    await this.getPort('progress').send({
      type: 'execution-started',
      timestamp: Date.now()
    });

    // Build execution context
    const context: ExecutionContext = {
      dependencies: this.dependencyResults,
      graph: this.graph,
      router: this.router,
      config: this.metadata,
      signal: new AbortController().signal
    };

    // Execute via pluggable executor
    const result = await this.executor.execute(this, context);

    // Store result
    this.lastResult = result;
    this._status = result.success ? 'completed' : 'failed';

    // Emit completion
    await this.getPort('progress').send({
      type: 'execution-completed',
      result
    });

    // Notify dependents
    await this.notifyDependents(result);

    return result;
  }

  /**
   * Notify dependent beads of completion
   */
  private async notifyDependents(result: ExecutionResult): Promise<void> {
    // Query graph for beads that depend on this one
    const dependents = await this.graph.query(`
      SELECT id FROM beads WHERE dependencies LIKE '%${this.metadata.id}%'
    `);

    // Send completion message to each dependent
    for (const dep of dependents) {
      await this.tell(address(dep.id), 'bead:dependency-completed', {
        dependencyId: this.metadata.id,
        result
      });
    }
  }
}
```

---

### 5. Graph Storage Schema

BeadActors are stored in SQLite with graph-friendly schema (from GRAPH_QUERY_RESEARCH.md).

#### SQLite Schema

```sql
-- Beads table (nodes)
CREATE TABLE beads (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT,
  status TEXT NOT NULL,  -- BeadStatus enum
  priority INTEGER NOT NULL,
  issue_type TEXT NOT NULL,
  owner TEXT,
  estimated_minutes INTEGER,
  created_at TEXT NOT NULL,
  created_by TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  closed_at TEXT,
  close_reason TEXT,
  tags TEXT,  -- JSON array
  notes TEXT,

  -- Executor configuration (JSON)
  executor_type TEXT,  -- 'manual' | 'auto' | 'formula' | 'knowledge' | 'external'
  executor_config TEXT,  -- JSON config for executor

  -- Execution state
  last_result TEXT,  -- JSON ExecutionResult
  last_execution_at TEXT
);

CREATE INDEX idx_beads_status ON beads(status);
CREATE INDEX idx_beads_priority ON beads(priority);
CREATE INDEX idx_beads_type ON beads(issue_type);

-- Dependencies table (edges)
CREATE TABLE bead_deps (
  id TEXT NOT NULL,
  depends_on TEXT NOT NULL,
  type TEXT DEFAULT 'blocks',  -- 'blocks' | 'relates-to' | 'duplicates'
  created_at TEXT NOT NULL,
  created_by TEXT NOT NULL,

  FOREIGN KEY (id) REFERENCES beads(id) ON DELETE CASCADE,
  FOREIGN KEY (depends_on) REFERENCES beads(id) ON DELETE CASCADE,

  PRIMARY KEY (id, depends_on)
);

CREATE INDEX idx_deps_id ON bead_deps(id);
CREATE INDEX idx_deps_depends ON bead_deps(depends_on);

-- Execution history (for monitoring)
CREATE TABLE bead_executions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  bead_id TEXT NOT NULL,
  timestamp TEXT NOT NULL,
  duration INTEGER NOT NULL,  -- milliseconds
  success INTEGER NOT NULL,   -- 0 or 1
  error TEXT,                 -- Error message if failed
  result TEXT,                -- JSON result data

  FOREIGN KEY (bead_id) REFERENCES beads(id) ON DELETE CASCADE
);

CREATE INDEX idx_executions_bead ON bead_executions(bead_id);
CREATE INDEX idx_executions_timestamp ON bead_executions(timestamp);
```

#### Graph Queries

**Find Ready Beads (No Open Dependencies)**

```sql
SELECT b.id, b.title, b.priority
FROM beads b
WHERE b.status = 'backlog'
  AND NOT EXISTS (
    SELECT 1
    FROM bead_deps bd
    JOIN beads blocker ON bd.depends_on = blocker.id
    WHERE bd.id = b.id
      AND blocker.status NOT IN ('completed', 'closed')
  )
ORDER BY b.priority ASC, b.created_at ASC;
```

**Find Blocked Beads with Blockers**

```sql
SELECT
  b.id AS blocked_id,
  b.title AS blocked_title,
  GROUP_CONCAT(blocker.id) AS blockers
FROM beads b
JOIN bead_deps bd ON b.id = bd.id
JOIN beads blocker ON bd.depends_on = blocker.id
WHERE b.status IN ('backlog', 'blocked')
  AND blocker.status NOT IN ('completed', 'closed')
GROUP BY b.id, b.title;
```

**Find Dependency Tree (Transitive Closure)**

```sql
WITH RECURSIVE dep_tree(root, dep, level, path) AS (
  -- Base: root bead
  SELECT id, id, 0, id
  FROM beads
  WHERE id = ?

  UNION ALL

  -- Recursive: follow dependencies
  SELECT dt.root, bd.depends_on, dt.level + 1, dt.path || ' > ' || bd.depends_on
  FROM dep_tree dt
  JOIN bead_deps bd ON dt.dep = bd.id
  WHERE bd.depends_on IS NOT NULL
    AND dt.path NOT LIKE '%' || bd.depends_on || '%'  -- Prevent cycles
)
SELECT
  dep AS dependency_id,
  b.title,
  b.status,
  level,
  path
FROM dep_tree dt
JOIN beads b ON dt.dep = b.id
WHERE level > 0
ORDER BY level, dep;
```

---

### 6. Supervision Integration

BeadActors participate in the supervision hierarchy (from SUPERVISION_ARCHITECTURE.md).

#### Supervision Tree

```
                    ┌─────────────────┐
                    │  Root Supervisor │
                    │  (ActorSystem)   │
                    └────────┬─────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
      ┌───────▼────┐  ┌─────▼─────┐  ┌────▼─────┐
      │   Bead     │  │  Channel   │  │  Session │
      │ Supervisor │  │ Supervisor │  │Supervisor│
      └──────┬─────┘  └────────────┘  └──────────┘
             │
      ┌──────┼──────┐
      │      │      │
   ┌──▼─┐ ┌─▼──┐ ┌─▼──┐
   │Exec│ │Form│ │Know│
   │Bead│ │Bead│ │Bead│
   └────┘ └────┘ └────┘
```

#### BeadSupervisor

```typescript
/**
 * BeadSupervisor - Supervises task execution actors
 *
 * Monitors bead executions, restarts failed beads, tracks progress.
 */
class BeadSupervisor extends BaseSupervisorActor {
  constructor(router: MessageRouter, graph: GraphStore) {
    super('bead-supervisor', router, {
      strategy: {
        type: 'one-for-one',  // Independent bead failures
        maxRestarts: 3,
        withinSeconds: 60
      },
      healthCheck: {
        interval: 60000,  // Check every minute
        timeout: 5000,
        threshold: 3
      }
    });

    this.graph = graph;
  }

  /**
   * Load all beads from graph and supervise them
   */
  async start(): Promise<void> {
    const beads = await this.loadBeadsFromGraph();

    for (const beadData of beads) {
      const bead = this.createBeadActor(beadData);
      this.supervise(bead, {
        strategy: this.getStrategyForBead(bead),
        errorClassifier: this.beadErrorClassifier
      });
    }
  }

  /**
   * Create BeadActor from database row
   */
  private createBeadActor(data: any): BeadActor {
    const executor = this.createExecutor(data.executor_type, data.executor_config);
    const dependencies = this.loadDependencies(data.id);

    return new BaseBeadActor({
      metadata: data,
      executor,
      dependencies,
      graph: this.graph,
      router: this.router
    });
  }

  /**
   * Classify bead execution errors
   */
  private beadErrorClassifier: ErrorClassifier = (error, actor, message) => {
    // Dependency errors: transient (might resolve when dependency retries)
    if (error.message.includes('dependency')) {
      return {
        severity: 'transient',
        shouldRestart: true,
        escalate: false,
        reason: 'Dependency failure (may resolve on retry)'
      };
    }

    // Executor errors: permanent (likely code bug)
    if (error.message.includes('executor')) {
      return {
        severity: 'permanent',
        shouldRestart: false,
        escalate: true,
        reason: 'Executor failure (code bug)'
      };
    }

    // Default: treat as transient
    return defaultErrorClassifier(error, actor, message);
  };
}
```

#### Lifecycle Hooks

BeadActors implement supervision lifecycle hooks:

```typescript
class BaseBeadActor extends Actor implements BeadActor, SupervisedActor {
  /**
   * preRestart - Save execution state before restart
   */
  async preRestart(error: Error, message?: Message): Promise<BeadCheckpoint> {
    this.logger.warn('BeadActor restarting', {
      bead: this.metadata.id,
      error
    });

    // Save checkpoint
    const checkpoint: BeadCheckpoint = {
      metadata: this.metadata,
      status: this._status,
      partialResults: this.partialResults,
      queuedMessages: this.messageQueue,
      timestamp: Date.now()
    };

    // Persist to graph
    await this.graph.execute(`
      UPDATE beads
      SET status = 'suspended',
          notes = 'Actor restarted due to: ${error.message}'
      WHERE id = ?
    `, [this.metadata.id]);

    return checkpoint;
  }

  /**
   * postRestart - Restore execution state after restart
   */
  async postRestart(checkpoint?: BeadCheckpoint): Promise<void> {
    if (checkpoint) {
      this.metadata = checkpoint.metadata;
      this._status = checkpoint.status;
      this.partialResults = checkpoint.partialResults;
      this.messageQueue = checkpoint.queuedMessages;

      this.logger.info('BeadActor restored from checkpoint', {
        bead: this.metadata.id,
        checkpointAge: Date.now() - checkpoint.timestamp
      });
    }

    // Resume dependency tracking
    await this.loadDependencyResults();

    // Check if ready to execute
    if (this.allDependenciesComplete()) {
      await this.transitionToReady();
    }
  }

  /**
   * healthCheck - Verify actor is responsive
   */
  async healthCheck(): Promise<boolean> {
    // Check if actor is stuck in in_progress state for too long
    if (this._status === 'in_progress') {
      const elapsed = Date.now() - (this.lastExecutionStart || 0);
      const timeout = this.executor.estimateTime?.() || 300000; // 5 min default

      if (elapsed > timeout * 2) {
        this.logger.warn('BeadActor appears stuck', {
          bead: this.metadata.id,
          elapsed,
          timeout
        });
        return false;
      }
    }

    return true;
  }
}
```

---

### 7. Knowledge Management Pattern

Knowledge BeadActors manage domain knowledge and respond to semantic queries.

```typescript
/**
 * KnowledgeBeadActor - Manages domain knowledge
 *
 * Extracts patterns from dependencies, indexes knowledge,
 * responds to semantic queries.
 */
class KnowledgeBeadActor extends BaseBeadActor {
  private knowledge: KnowledgeGraph;

  constructor(config: BeadActorConfig) {
    super({
      ...config,
      executor: new KnowledgeExecutor(new KnowledgeGraph(config.domain))
    });

    this.knowledge = (this.executor as KnowledgeExecutor).knowledge;
  }

  /**
   * Receive message - handle queries in addition to standard bead messages
   */
  async receive(msg: Message): Promise<MessageResponse> {
    // Handle semantic queries
    if (msg.type === 'bead:query') {
      const results = await this.knowledge.search(msg.payload.query);
      return createResponse(msg, { results });
    }

    // Handle pattern extraction requests
    if (msg.type === 'bead:extract-patterns') {
      const patterns = await this.extractPatterns(msg.payload.source);
      await this.knowledge.index(patterns);
      return createResponse(msg, { patternsExtracted: patterns.length });
    }

    // Delegate to base class for standard bead messages
    return super.receive(msg);
  }

  /**
   * Execute - Extract knowledge from dependencies
   */
  async execute(context: ExecutionContext): Promise<ExecutionResult> {
    const patterns = [];

    // Extract patterns from each dependency result
    for (const [depId, result] of context.dependencies) {
      if (result.success && result.data) {
        const extracted = await this.extractPatterns(result.data);
        patterns.push(...extracted);
      }
    }

    // Index patterns into knowledge graph
    await this.knowledge.index(patterns);

    return {
      success: true,
      data: { patternsExtracted: patterns.length },
      duration: 0,
      timestamp: Date.now()
    };
  }

  private async extractPatterns(data: any): Promise<Pattern[]> {
    // Pattern extraction logic (LLM-based, rule-based, etc.)
    // ...
  }
}

// Example: Session knowledge bead
const sessionKnowledgeBead = new KnowledgeBeadActor({
  metadata: {
    id: 'simplify-session-knowledge',
    title: 'Session Knowledge Management',
    description: 'Extracts and indexes architectural decisions, learnings, and errors from sessions',
    status: 'backlog',
    priority: 2,
    issue_type: 'feature'
  },
  dependencies: [
    address('simplify-session-embedder'),  // Dependency: embed session messages
    address('simplify-llm-classifier')     // Dependency: classify message types
  ],
  domain: 'session-knowledge'
});
```

---

### 8. External Integration Pattern

External BeadActors sync task state with external systems (GitHub, Slack, Jira, etc.).

```typescript
/**
 * ExternalSyncBeadActor - Syncs bead state with external system
 *
 * Bidirectional sync: bead changes → external, external changes → bead.
 */
class ExternalSyncBeadActor extends BaseBeadActor {
  private externalClient: ExternalClient;
  private syncConfig: SyncConfig;

  constructor(config: BeadActorConfig & { externalClient: ExternalClient; syncConfig: SyncConfig }) {
    super({
      ...config,
      executor: new ExternalSyncExecutor(config.externalClient, config.syncConfig)
    });

    this.externalClient = config.externalClient;
    this.syncConfig = config.syncConfig;

    // Start polling for external changes
    this.startExternalPolling();
  }

  /**
   * Execute - Sync bead to external system
   */
  async execute(context: ExecutionContext): Promise<ExecutionResult> {
    const external = await this.externalClient.sync(this.metadata, {
      title: this.metadata.title,
      description: this.metadata.description,
      status: this.mapStatus(this.metadata.status),
      priority: this.mapPriority(this.metadata.priority),
      labels: [this.metadata.issue_type, `P${this.metadata.priority}`]
    });

    return {
      success: true,
      data: { externalId: external.id, url: external.url },
      duration: 0,
      timestamp: Date.now()
    };
  }

  /**
   * Poll external system for changes
   */
  private async startExternalPolling(): Promise<void> {
    setInterval(async () => {
      const external = await this.externalClient.get(this.metadata.id);

      if (external && this.hasChanged(external)) {
        await this.applyExternalChanges(external);
      }
    }, this.syncConfig.pollInterval || 60000);  // Default: 1 minute
  }

  /**
   * Apply external changes to bead
   */
  private async applyExternalChanges(external: ExternalItem): Promise<void> {
    // Update metadata
    this.metadata.title = external.title;
    this.metadata.description = external.body;
    this.metadata.status = this.reverseMapStatus(external.status);

    // Persist to graph
    await this.graph.execute(`
      UPDATE beads
      SET title = ?, description = ?, status = ?, updated_at = ?
      WHERE id = ?
    `, [
      this.metadata.title,
      this.metadata.description,
      this.metadata.status,
      new Date().toISOString(),
      this.metadata.id
    ]);

    // Emit status change event
    await this.getPort('status').send({
      type: 'status-changed',
      from: this._status,
      to: this.metadata.status as BeadStatus
    });
  }
}

// Example: GitHub sync bead
const githubSyncBead = new ExternalSyncBeadActor({
  metadata: {
    id: 'simplify-github-sync',
    title: 'Sync Beads to GitHub Issues',
    description: 'Bidirectional sync between beads and GitHub issues',
    status: 'backlog',
    priority: 2,
    issue_type: 'feature'
  },
  externalClient: new GitHubClient({ token: process.env.GITHUB_TOKEN }),
  syncConfig: {
    pollInterval: 60000,  // 1 minute
    repo: 'bln/simplify',
    bidirectional: true
  },
  dependencies: []
});
```

---

### 9. Migration Strategy

Phased migration from current JSONL-based beads to BeadActors.

#### Phase 1: Read-Only BeadActors (Query Existing Beads)

**Goal:** BeadActors can read existing JSONL beads, no execution yet.

```typescript
// 1. Create SQLite schema and migrate JSONL data
await migrateBeadsToSQLite({
  jsonlPath: '.beads/issues.jsonl',
  dbPath: '.beads/beads.db'
});

// 2. Create read-only BeadActors
const beadSystem = new BeadActorSystem(graph, router);
await beadSystem.loadBeadsFromDB();

// 3. Query beads via actors (instead of bd CLI)
const readyBeads = await beadSystem.findReady();
const blockedBeads = await beadSystem.findBlocked();

// 4. Backward compatibility: bd CLI queries SQLite instead of JSONL
// (No API changes to bd CLI)
```

**Deliverables:**
- SQLite schema creation script
- JSONL → SQLite migration script
- BeadActorSystem class (loads beads as read-only actors)
- Update bd CLI to query SQLite (transparent to users)

**Success Criteria:**
- All JSONL beads migrated to SQLite
- bd CLI queries work unchanged
- BeadActors can be queried via messages

#### Phase 2: Executable BeadActors (Auto-Run on Ready)

**Goal:** BeadActors auto-execute when dependencies complete.

```typescript
// 1. Add executor support to BeadActor
const testBead = new BeadActor({
  metadata: { id: 'simplify-test-123', ... },
  executor: new AutoExecutor(async () => {
    return await runCommand('bun test');
  }),
  dependencies: [address('simplify-code-456')]
});

// 2. BeadSupervisor monitors and restarts failed beads
const supervisor = new BeadSupervisor(router, graph);
supervisor.supervise(testBead, { strategy: { type: 'one-for-one', maxRestarts: 3, withinSeconds: 60 } });

// 3. When code bead completes, test bead auto-executes
codeBead.complete() → testBead.receive('dependency-completed') → testBead.execute()
```

**Deliverables:**
- BeadExecutor interface + implementations (Manual, Auto, Formula)
- Dependency tracking + auto-execution logic
- BeadSupervisor class
- Execution history storage

**Success Criteria:**
- BeadActors execute when dependencies complete
- Failed executions restart automatically (via supervision)
- Execution history persisted to graph

#### Phase 3: Knowledge BeadActors (Semantic Queries)

**Goal:** BeadActors manage domain knowledge, respond to semantic queries.

```typescript
// 1. Create knowledge beads
const sessionKnowledgeBead = new KnowledgeBeadActor({
  metadata: { id: 'simplify-session-knowledge', ... },
  dependencies: [address('simplify-embedder')],
  domain: 'session-knowledge'
});

// 2. Query knowledge via messages
const results = await sessionKnowledgeBead.ask('bead:query', {
  query: 'What architectural decisions were made about supervision?'
});
```

**Deliverables:**
- KnowledgeBeadActor class
- KnowledgeGraph integration
- Semantic query interface

**Success Criteria:**
- Knowledge beads extract patterns from dependencies
- Semantic queries return relevant results
- Integration with existing session knowledge system

#### Phase 4: Full Integration (Replace bd CLI)

**Goal:** BeadActors fully replace bd CLI, add UI visualization.

```typescript
// 1. Widget Actor UI for bead visualization
class BeadGraphWidget extends BaseWidgetActor {
  async receive(msg: Message): Promise<MessageResponse> {
    if (msg.type === 'render') {
      this.renderGraph(await this.loadBeadGraph());
    }
    return { success: true };
  }

  private async loadBeadGraph(): Promise<BeadGraph> {
    // Query BeadActors for dependency graph
    const beads = await beadSystem.getAllBeads();
    return buildGraph(beads);
  }
}

// 2. Replace bd CLI with bead command that talks to actors
// bead list --ready → BeadActorSystem.findReady()
// bead status <id> → BeadActor.ask('bead:get-status')

// 3. External sync (GitHub, Slack, etc.)
const githubSync = new ExternalSyncBeadActor({ ... });
```

**Deliverables:**
- Widget Actor UI for bead graph visualization
- bead CLI (replacement for bd)
- External sync actors (GitHub, Slack)
- Complete migration guide

**Success Criteria:**
- bd CLI deprecated (backward compatibility maintained)
- UI shows live bead graph with execution status
- External systems stay in sync with beads

---

## Implementation Files

### Type Definitions

**Location:** `src/messaging/bead-actor/types.ts`

```typescript
export interface BeadActor extends SupervisedActor { ... }
export interface BeadMetadata { ... }
export type BeadStatus = ...;
export interface BeadExecutor { ... }
export interface ExecutionContext { ... }
export interface ExecutionResult { ... }
export interface BeadCheckpoint { ... }
export type BeadEvent = ...;
export type BeadMessage = ...;
```

### Core Implementation

**Location:** `src/messaging/bead-actor/actor.ts`

```typescript
export class BaseBeadActor extends Actor implements BeadActor {
  // Dependency tracking
  // Message handling
  // Execution logic
  // Lifecycle hooks
}
```

### Executors

**Location:** `src/messaging/bead-actor/executors/`

```typescript
// executors/manual.ts
export class ManualExecutor implements BeadExecutor { ... }

// executors/auto.ts
export class AutoExecutor implements BeadExecutor { ... }

// executors/formula.ts
export class FormulaExecutor implements BeadExecutor { ... }

// executors/knowledge.ts
export class KnowledgeExecutor implements BeadExecutor { ... }

// executors/external.ts
export class ExternalSyncExecutor implements BeadExecutor { ... }
```

### Supervision

**Location:** `src/messaging/bead-actor/supervisor.ts`

```typescript
export class BeadSupervisor extends BaseSupervisorActor {
  // Load beads from graph
  // Supervise bead executions
  // Error classification
}
```

### System Orchestration

**Location:** `src/messaging/bead-actor/system.ts`

```typescript
export class BeadActorSystem {
  // Load beads from SQLite
  // Create BeadActors
  // Query ready/blocked beads
  // Manage supervision
}
```

### Migration Tools

**Location:** `src/messaging/bead-actor/migration/`

```typescript
// migration/jsonl-to-sqlite.ts
export async function migrateBeadsToSQLite(config: MigrationConfig): Promise<void> { ... }

// migration/create-schema.ts
export async function createBeadSchema(db: Database): Promise<void> { ... }
```

---

## Examples

### Example 1: Simple Executable Bead (Run Tests)

```typescript
import { BeadActor, AutoExecutor } from './src/messaging/bead-actor';

const testBead = new BaseBeadActor({
  metadata: {
    id: 'simplify-test-123',
    title: 'Run Unit Tests',
    description: 'Execute Bun test suite',
    status: 'backlog',
    priority: 1,
    issue_type: 'task',
    owner: 'newbeb@gmail.com',
    created_at: new Date().toISOString(),
    created_by: 'BLN',
    updated_at: new Date().toISOString()
  },
  executor: new AutoExecutor(async (ctx) => {
    const result = await runCommand('bun test');
    return {
      exitCode: result.exitCode,
      output: result.stdout,
      passedTests: parseTestOutput(result.stdout)
    };
  }),
  dependencies: [address('simplify-code-456')],  // Depends on code bead
  graph,
  router
});

// When code bead completes, test bead auto-executes
```

### Example 2: Formula Bead (Aggregate Results)

```typescript
const aggregateBead = new BaseBeadActor({
  metadata: {
    id: 'simplify-agg-789',
    title: 'Aggregate Test Results',
    description: 'Combine results from unit and integration tests',
    status: 'backlog',
    priority: 2,
    issue_type: 'task'
  },
  executor: new FormulaExecutor((inputs) => {
    const unitTests = inputs.get('simplify-unit-tests');
    const integrationTests = inputs.get('simplify-integration-tests');

    return {
      totalTests: unitTests.passedTests + integrationTests.passedTests,
      totalFailures: unitTests.failures + integrationTests.failures,
      allPassed: unitTests.failures === 0 && integrationTests.failures === 0
    };
  }),
  dependencies: [
    address('simplify-unit-tests'),
    address('simplify-integration-tests')
  ],
  graph,
  router
});
```

### Example 3: Knowledge Bead (Pattern Extraction)

```typescript
const knowledgeBead = new KnowledgeBeadActor({
  metadata: {
    id: 'simplify-session-knowledge',
    title: 'Session Knowledge Management',
    description: 'Extract architectural decisions, learnings, and errors from sessions',
    status: 'backlog',
    priority: 2,
    issue_type: 'feature'
  },
  dependencies: [
    address('simplify-session-embedder'),
    address('simplify-llm-classifier')
  ],
  domain: 'session-knowledge',
  graph,
  router
});

// Query knowledge
const decisions = await knowledgeBead.ask('bead:query', {
  query: 'What decisions were made about supervision architecture?'
});

console.log(decisions.payload.results);
// [{ decision: 'Chose one-for-one restart strategy for channels', ... }]
```

### Example 4: Interface Bead (GitHub Sync)

```typescript
const githubBead = new ExternalSyncBeadActor({
  metadata: {
    id: 'simplify-github-sync',
    title: 'Sync to GitHub Issues',
    description: 'Bidirectional sync between beads and GitHub',
    status: 'backlog',
    priority: 2,
    issue_type: 'feature'
  },
  externalClient: new GitHubClient({ token: process.env.GITHUB_TOKEN }),
  syncConfig: {
    pollInterval: 60000,
    repo: 'bln/simplify',
    bidirectional: true
  },
  dependencies: [],
  graph,
  router
});

// Bead changes → GitHub
await githubBead.execute();

// GitHub changes → Bead (via polling)
// Automatic via startExternalPolling()
```

### Example 5: Supervision Bead (Task Graph Orchestrator)

```typescript
const supervisor = new BeadSupervisor(router, graph);

// Load all beads from graph
await supervisor.start();

// Supervise specific bead with custom strategy
supervisor.supervise(testBead, {
  strategy: {
    type: 'one-for-one',
    maxRestarts: 5,
    withinSeconds: 120
  },
  healthCheck: {
    interval: 30000,
    timeout: 5000,
    threshold: 3
  }
});

// Monitor supervision metrics
const metrics = supervisor.getMetrics();
console.log(`Total restarts: ${metrics.totalRestarts}`);
console.log(`Active restarts: ${metrics.activeRestarts}`);
```

---

## Success Criteria

### Phase 1 (Read-Only)
- ✅ SQLite schema created with graph-friendly structure
- ✅ All JSONL beads migrated to SQLite successfully
- ✅ BeadActors can be queried via messages
- ✅ bd CLI queries SQLite (backward compatible)
- ✅ Dependency graph queries work (ready, blocked, tree)

### Phase 2 (Executable)
- ✅ BeadActors execute when dependencies complete
- ✅ Executor interface supports manual, auto, formula patterns
- ✅ BeadSupervisor restarts failed beads automatically
- ✅ Execution history persisted to graph
- ✅ Progress events emitted via ports

### Phase 3 (Knowledge)
- ✅ KnowledgeBeadActors extract patterns from dependencies
- ✅ Semantic queries return relevant results
- ✅ Integration with existing session knowledge system
- ✅ Knowledge graph stores extracted patterns

### Phase 4 (Full Integration)
- ✅ Widget Actor UI visualizes bead graph
- ✅ bead CLI replaces bd CLI
- ✅ External sync actors (GitHub, Slack) work bidirectionally
- ✅ Complete migration guide published

---

## References

- **Graph Query Research:** `/Users/bln/play/agentic-primer/simplify/GRAPH_QUERY_RESEARCH.md`
- **Supervision Architecture:** `/Users/bln/play/agentic-primer/simplify/docs/SUPERVISION_ARCHITECTURE.md`
- **Actor Base Class:** `/Users/bln/play/agentic-primer/simplify/src/messaging/actor.ts`
- **Channel Abstraction:** `/Users/bln/play/agentic-primer/simplify/src/messaging/channel.ts`
- **Widget Actors:** `/Users/bln/play/agentic-primer/simplify/src/messaging/browser/widget-actor.ts`
- **Message Protocol:** `/Users/bln/play/agentic-primer/simplify/src/messaging/message.ts`

---

## Changelog

- 2026-02-05: Initial BeadActor architecture design
