#!/usr/bin/env bun
/**
 * Core type definitions for query/DSL layer
 *
 * Defines the foundational types for building, compiling, and executing
 * graph queries over the actor fabric.
 */

import type { Address, Message } from '@agentic-primer/actors';

/**
 * Pattern specification for matching nodes in the graph
 */
export interface PatternSpec {
  /** Variable name to bind matched nodes */
  variable: string;

  /** Node labels to match (e.g., 'Task', 'Knowledge') */
  labels?: string[];

  /** Property filters */
  where?: Record<string, any>;

  /** Relationship constraints */
  relationships?: RelationshipConstraint[];

  /** Negation (NOT EXISTS) */
  notExists?: PatternSpec[];
}

/**
 * Relationship constraint for pattern matching
 */
export interface RelationshipConstraint {
  /** Target variable */
  target: string;

  /** Relationship type */
  type?: string;

  /** Direction: outbound (->) | inbound (<-) | both (-) */
  direction: 'outbound' | 'inbound' | 'both';

  /** Properties on the relationship */
  properties?: Record<string, any>;
}

/**
 * Query definition - high-level representation
 */
export interface QueryDefinition {
  /** Pattern matching clauses */
  patterns: PatternSpec[];

  /** WHERE filters (applied after pattern matching) */
  filters?: FilterExpression[];

  /** Traversal operations */
  traversals?: TraversalSpec[];

  /** Aggregation operations */
  aggregations?: AggregationSpec[];

  /** Actions to perform on results */
  actions?: ActionSpec[];

  /** Variables to return */
  returns?: string[];

  /** Metadata for optimization */
  metadata?: QueryMetadata;
}

/**
 * Filter expression for WHERE clauses
 */
export interface FilterExpression {
  type: 'comparison' | 'logical' | 'predicate';
  operator?: string; // '=', '!=', '>', '<', 'AND', 'OR', 'NOT'
  variable?: string;
  property?: string;
  value?: any;
  expressions?: FilterExpression[]; // For logical operators
}

/**
 * Traversal specification
 */
export interface TraversalSpec {
  /** Start variable */
  from: string;

  /** Relationship type */
  relationship?: string;

  /** Traversal direction */
  direction: 'outbound' | 'inbound' | 'both';

  /** Depth constraints */
  depth?: {
    min?: number;
    max?: number;
  };

  /** Result variable name */
  as: string;
}

/**
 * Aggregation specification
 */
export interface AggregationSpec {
  operation: 'count' | 'collect' | 'group' | 'sum' | 'avg';
  variable: string;
  by?: string; // For group operations
  as: string;
}

/**
 * Stream specification for continuous data streams
 */
export interface StreamSpec {
  /** Target variable to stream from */
  variable: string;

  /** Stream message type */
  type: string;

  /** Stream payload */
  payload?: any;

  /** Buffer size for backpressure */
  bufferSize?: number;

  /** Timeout in milliseconds */
  timeout?: number;

  /** Cancellation signal */
  signal?: AbortSignal;
}

/**
 * Action specification for mutations
 */
export interface ActionSpec {
  type: 'send' | 'update' | 'create' | 'delete' | 'create_relationship' | 'delete_relationship' | 'upsert_relationship';
  target: string; // Variable to apply action to
  params: any;
}

/**
 * Query metadata for optimization
 */
export interface QueryMetadata {
  /** Query hash for caching */
  hash?: string;

  /** Estimated selectivity (0-1) */
  selectivity?: number;

  /** Expected result count */
  expectedResults?: number;

  /** Priority hint */
  priority?: 'low' | 'normal' | 'high';

  /** Index hints (advisory) */
  indexHints?: IndexHint[];

  /** Path pattern used with matchPath() */
  pathPattern?: string;

  /** Whether this is a path-pattern query (set by matchPath()) */
  isPathQuery?: boolean;

  /** Event types this query reacts to (set by on()) */
  triggerEventTypes?: string[];

  /** Arbitrary user-supplied metadata (e.g. via withMetadata()) */
  [key: string]: unknown;
}

/**
 * Index hint for query optimization
 */
export interface IndexHint {
  /** Variable to apply hint to */
  variable: string;

  /** Index name or property to use */
  index: string;

  /** Hint source: manual or automatic */
  source: 'manual' | 'automatic';

  /** Confidence score (0-1, for automatic hints) */
  confidence?: number;

  /** Reason for hint selection */
  reason?: string;
}

/**
 * Query plan - compiled executable representation
 *
 * This is the output of the QueryCompiler and represents the
 * optimized execution strategy inspired by Halo paper.
 */
export interface QueryPlan {
  /** Unique plan identifier (hash of structure) */
  id: string;

  /** Executable steps (DAG nodes) */
  steps: PlanStep[];

  /** Variable bindings produced by this plan */
  variables: string[];

  /** Plan metadata for optimization */
  metadata: PlanMetadata;

  /** Original query definition */
  original: QueryDefinition;
}

/**
 * Single step in execution plan (DAG node)
 */
export interface PlanStep {
  /** Step ID within plan */
  id: string;

  /** Step type */
  type: 'query' | 'traverse' | 'filter' | 'action' | 'aggregate';

  /** Target actor address */
  actor: Address;

  /** Message to send */
  message: Omit<Message, 'id' | 'timestamp' | 'to'>;

  /** Variables this step produces */
  bindings: string[];

  /** Dependencies (step IDs that must complete first) */
  dependencies: string[];

  /** Can this step run in parallel with others? */
  parallelizable: boolean;

  /** Operation signature for deduplication */
  signature: string;

  /** Estimated cost */
  cost: StepCost;

  /** Optional metadata for step execution */
  metadata?: Record<string, any>;
}

/**
 * Cost estimation for a plan step
 */
export interface StepCost {
  /** Estimated latency (ms) */
  latencyMs: number;

  /** Estimated CPU time (ms) */
  cpuMs: number;

  /** Expected result count */
  resultCount: number;

  /** Cache hit probability (0-1) */
  cacheHitProb: number;
}

/**
 * Plan metadata for optimization
 */
export interface PlanMetadata {
  /** Estimated total cost */
  estimatedCost: PlanCost;

  /** Index hints (legacy string array for backward compatibility) */
  indexes: string[];

  /** Structured index hints */
  indexHints?: IndexHint[];

  /** Can the entire plan be parallelized? */
  parallelizable: boolean;

  /** Critical path length */
  criticalPathSteps: number;

  /** Compilation timestamp */
  compiledAt: number;
}

/**
 * Total plan cost
 */
export interface PlanCost {
  /** Critical path latency (makespan) */
  makespan: number;

  /** Total work (aggregate) */
  totalWork: number;

  /** Resource usage estimate */
  resourceUsage: ResourceProfile;
}

/**
 * Resource usage profile
 */
export interface ResourceProfile {
  /** Estimated memory (bytes) */
  memoryBytes: number;

  /** Estimated I/O operations */
  ioOps: number;

  /** Message count */
  messageCount: number;
}

/**
 * Query execution context
 *
 * Tracks the runtime state during query execution,
 * including warm actors and cached data (from Halo paper).
 */
export interface ExecutionContext {
  /** Warm actors (already initialized) */
  warmActors: Set<Address>;

  /** Cached computations (signature → result) */
  computationCache: Map<string, any>;

  /** Available resources */
  resources: {
    maxConcurrency: number;
    availableMemory: number;
  };

  /** Execution start time */
  startTime: number;
}

/**
 * Query execution result
 */
export interface QueryResult {
  /** Query plan ID */
  planId: string;

  /** Variable bindings */
  bindings: Map<string, any[]>;

  /** Execution statistics */
  stats: ExecutionStats;

  /** Success flag */
  success: boolean;

  /** Error if failed */
  error?: string;
}

/**
 * Execution statistics for profiling
 */
export interface ExecutionStats {
  /** Total execution time (ms) */
  durationMs: number;

  /** Number of steps executed */
  stepsExecuted: number;

  /** Number of messages sent */
  messagesSent: number;

  /** Cache hits */
  cacheHits: number;

  /** Cache misses */
  cacheMisses: number;

  /** Results returned */
  resultsReturned: number;

  /** Per-step statistics */
  stepStats: Map<string, StepStats>;
}

/**
 * Per-step execution statistics
 */
export interface StepStats {
  /** Step ID */
  stepId: string;

  /** Execution time (ms) */
  durationMs: number;

  /** Result count */
  resultCount: number;

  /** Cache hit? */
  cacheHit: boolean;

  /** Success flag */
  success: boolean;

  /** Error if failed */
  error?: string;
}

/**
 * Plan cache key
 *
 * Composite key for caching compiled plans based on
 * query structure and execution context (from Halo paper).
 */
export interface PlanCacheKey {
  /** Workflow DAG structure hash */
  workflowHash: string;

  /** Actor state fingerprint (warm actors, cached data) */
  actorStateFingerprint: string;

  /** Resource availability context */
  resourceContext: string;
}

/**
 * Query statistics for learning
 *
 * Tracks execution history per query signature to enable
 * adaptive optimization (from Halo paper).
 */
export interface QueryStatistics {
  /** Query signature (normalized) */
  signature: string;

  /** Execution count */
  executionCount: number;

  /** Average duration (ms) */
  avgDurationMs: number;

  /** Duration variance */
  durationVariance: number;

  /** Average result count */
  avgResultCount: number;

  /** Success rate (0-1) */
  successRate: number;

  /** Cache hit rate (0-1) */
  cacheHitRate: number;

  /** Last execution timestamp */
  lastExecutedAt: number;

  /** Latency percentiles */
  latencyPercentiles: {
    p50: number;
    p90: number;
    p99: number;
  };

  /** Index effectiveness tracking */
  indexEffectiveness?: Map<string, IndexEffectiveness>;
}

/**
 * Index effectiveness statistics
 */
export interface IndexEffectiveness {
  /** Index name */
  indexName: string;

  /** Number of times used */
  useCount: number;

  /** Average query improvement (0-1, where 1 = 100% faster) */
  avgImprovement: number;

  /** Success rate with this index (0-1) */
  successRate: number;

  /** Average result count */
  avgResultCount: number;

  /** Last used timestamp */
  lastUsedAt: number;
}

/**
 * Subscription callbacks for reactive queries
 */
export interface SubscriptionCallbacks<T = any> {
  /** Called when new results match the pattern */
  onMatch: (results: T[]) => void;

  /** Called when previously matched results no longer match */
  onUnmatch?: (results: T[]) => void;

  /** Called when an error occurs during re-evaluation */
  onError?: (error: Error) => void;
}

/**
 * Subscription handle for managing reactive queries
 */
export interface Subscription {
  /** Unsubscribe and stop receiving updates */
  unsubscribe(): void;

  /** Check if subscription is still active */
  isActive(): boolean;
}

/**
 * Event trigger specification
 */
export interface TriggerSpec {
  /** Unique trigger ID */
  id: string;

  /** Event types to listen for */
  eventTypes: string[];

  /** Pattern to filter events (optional) */
  pattern?: PatternSpec;

  /** Additional WHERE filters */
  filters?: FilterExpression[];

  /** Actions to execute on match */
  actions: ActionSpec[];

  /** Debounce delay in ms (optional) */
  debounce?: number;

  /** Throttle interval in ms (optional) */
  throttle?: number;

  /** Max retries on action failure */
  maxRetries?: number;

  /** Additional metadata */
  metadata?: Record<string, any>;
}
