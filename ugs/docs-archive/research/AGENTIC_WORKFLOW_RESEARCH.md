# Agentic Workflows as Query Planning Systems: Research Summary

**Research Date:** February 5, 2026
**Primary Paper:** Halo - Batch Query Processing and Optimization for Agentic Workflows
**arXiv ID:** [2509.02121](https://arxiv.org/abs/2509.02121)

---

## Executive Summary

The Halo system from National University of Singapore demonstrates that treating agentic LLM workflows as database-style query plans enables dramatic performance improvements through cost-based optimization, plan caching, and adaptive execution.

### Four Key Insights

1. **Workflows as Typed DAGs**: Representing workflows as structured directed acyclic graphs with explicit LLM and Tool nodes enables cross-workflow optimization opportunities that request-isolated approaches miss.

2. **Cost-Aware Planning**: A comprehensive cost model decomposing execution into preparation, model-switching, and inference costs (with cache reuse awareness) enables plan-level optimization that achieves 3.6x speedups.

3. **Signature-Based Deduplication**: Canonicalizing operation signatures (operator type + normalized arguments) allows batching identical work across concurrent workflows without exact textual matching.

4. **Adaptive Runtime Optimization**: Continuous profiling calibration with wavefront-style execution masks tail latency while maintaining dependency constraints and locality intent.

---

## Paper Details

**Title:** Batch Query Processing and Optimization for Agentic Workflows
**Authors:** Junyi Shen, Noppanat Wadlom, Yao Lu
**Institution:** National University of Singapore
**arXiv Link:** https://arxiv.org/abs/2509.02121
**Submission:** September 2, 2025 (v1), January 19, 2026 (v2)
**Classification:** cs.DB (Databases), cs.DC (Distributed Computing)
**HTML Version:** https://arxiv.org/html/2509.02121

### Abstract Summary

Halo introduces batch query processing principles to LLM-based agentic workflows that combine multi-step reasoning with heterogeneous tool use across specialized agents. By representing workflows as structured DAGs and constructing consolidated graphs for batched queries, Halo exposes shared computation opportunities. Using a cost model addressing heterogeneous resource constraints, cache reuse, and GPU placement, the system performs plan-level optimization to minimize redundant execution.

### Performance Results

- **Batch Inference:** Up to 3.6x speedup
- **Online Serving:** 2.6x throughput improvement
- **Scale:** Handles thousands of queries with complex computational graphs
- **Quality:** No degradation in output quality

---

## Technical Architecture

### Database-Style System Design

Halo follows the classic database architecture pattern:

```
Parser → Optimizer → Processor
   ↓          ↓           ↓
YAML to   Cost-Based   Runtime
GraphSpec  Plan Gen   Execution
```

**Parser:** Converts YAML workflow specifications to typed GraphSpec representations
**Optimizer:** Produces ExecutionPlan via profiling and dynamic programming solver
**Processor:** Coordinates runtime execution across heterogeneous GPU/CPU workers

### Core Innovation

The fundamental insight is treating agentic workflows like database queries with explicit DAG representations, cost-based optimization, and plan-level reasoning rather than request-isolated approaches or purely orchestration-focused frameworks.

---

## Key Technical Concepts for Query Cache Design

### 1. Workflow DAG Representation

**Node Types:**
- **LLM Nodes:** Model invocations with specific prompts, parameters, and models
- **Tool Nodes:** External API calls, database queries, helper functions

**Dependency Decoupling:**
The system "extracts embedded non-LLM computations, such as database queries, external API invocations, and local helper functions, into standalone nodes." This explicit separation enables finer-grained optimization and sharing.

**Implications for Our Design:**
- Represent actor workflows as typed DAGs with Actor and Port nodes
- Separate state management, communication, and computation nodes
- Enable dependency analysis for optimization opportunities

### 2. Cost Model Architecture

**Core Formula:**

```
T(w,v,Sₑ) = Tₚᵣₑₚ(v) + Tgₚᵤ(w,v,hᵥₑ)

Where GPU execution splits into:
Tgₚᵤ(w,v,hᵥₑ) = Tₘₒdₑₗ(v,mᵥₑ) + Tᵢₙfₑᵣ(v,uᵥₑ)

And epoch cost combines makespan and aggregate load:
Cₑₚₒcₕ(Sₑ,Aₑ) = μ·maxw∈W Tw + (1−μ)·∑w∈W Tw + λ·g(Aₑ)
```

**Parameters:**
- **Tₚᵣₑₚ:** CPU-side preparation costs
- **Tₘₒdₑₗ:** Model-switch overhead (loading weights)
- **Tᵢₙfₑᵣ:** Inference cost (prefill + decode)
- **hᵥₑ = (mᵥₑ, uᵥₑ):** Worker context (resident model, KV-cache signature)
- **μ:** Makespan vs. aggregate load tradeoff parameter
- **λ:** Overhead regularization weight

**Implications for Our Design:**
- Cost model should capture actor initialization, message passing, and computation costs
- Track persistent state (warm actors, cached data) in cost estimation
- Balance latency (critical path) vs. throughput (total work) in optimization

### 3. Signature-Based Operation Canonicalization

**Deduplication Strategy:**

The Processor "canonicalizes an operator signature (operator type plus normalized arguments) and merges pending tasks with identical signatures into a single physical execution, then fans out results to dependent nodes."

**Benefits:**
- Enables work reduction across concurrent workflows
- No requirement for exact textual matching
- Preserves per-workflow provenance while sharing computation

**Implications for Our Design:**
- Implement signature hashing for actor messages and computations
- Normalize arguments (e.g., order-independent parameter sets)
- Support result fan-out to multiple dependent actors
- Consider semantic equivalence beyond syntactic matching

### 4. Plan Caching and Reuse

**Memoized Dynamic Programming:**

The optimizer maintains a hash table **M** storing optimal sub-paths indexed by:
- Completed node sets
- Hardware context (resident model weights, KV-cache signatures)

This prevents redundant exploration of identical scheduling states across similar queries.

**Worker Context Representation:**

```
hᵥₑ = (mᵥₑ, uᵥₑ)
where:
  mᵥₑ = resident model weights on GPU
  uᵥₑ = KV-cache signature (what's already cached)
```

The scheduler recognizes when subsequent queries can exploit warm KV-cache entries based on this persistent state representation.

**Implications for Our Design:**
- Cache query plans keyed by workflow structure + actor state
- Track persistent actor state (warm actors, cached computations)
- Implement plan templates that can be instantiated with different parameters
- Use state fingerprinting to identify cache reuse opportunities

### 5. Adaptive Execution Strategies

**Wavefront-Style Execution:**

Completed LLM nodes "immediately promote successors if tool prerequisites are satisfied." This opportunistic execution strategy "masks tail latency while preserving the plan's locality intent" without violating dependency constraints.

**Online Profiling Calibration:**

"Profiling statistics are updated continuously, allowing the processor to refine cost estimates as runtime conditions drift (e.g., due to contention, network jitter, or decoding variance)."

**Per-Operator-Type Profiling:**
- **SQL:** Uses DBMS EXPLAIN interfaces
- **Black-box tools:** Maintains moving average over recent invocations, keyed by normalized signature
- **LLM inference:** Maintains calibrated throughput curves mapping tokens to latency

**Opportunistic Scheduling:**

The Coordinator "opportunistically executes other ready tasks from its buffer provided that (i) data dependencies are satisfied, and (ii) the execution does not disrupt imminent GPU state."

**Implications for Our Design:**
- Implement eager execution for ready actors while respecting dependencies
- Maintain runtime statistics per actor type and operation signature
- Use moving averages for cost estimation refinement
- Enable opportunistic scheduling when resources become available
- Balance locality (keep warm state) vs. parallelism (utilize idle resources)

### 6. Learning from Execution History

**Empirical Metrics Accumulation:**

While Halo doesn't employ ML-based prediction, it systematically accumulates empirical metrics. The profiling framework continuously updates moving averages and variance estimates during execution, feeding improved estimates back into subsequent scheduling decisions.

**State-Aware Cost Model:**

The cost model explicitly conditions estimates on accumulated hardware state (resident models, cached KV entries), implicitly encoding patterns from prior executions through these persistent context signatures.

**Implications for Our Design:**
- Maintain execution history database with per-signature statistics
- Track success/failure rates, latency distributions, resource consumption
- Use historical data to improve cost estimates and plan selection
- Consider confidence intervals and variance in optimization decisions
- Build feedback loop from runtime metrics to plan optimizer

---

## Recommendations for Query Cache Implementation

### 1. Adopt DAG-Based Query Representation

**Action:** Represent actor workflows as typed DAGs with explicit node types (Actor, Port, Channel, Computation).

**Benefits:**
- Enables dependency analysis and parallel execution
- Supports optimization transformations (fusion, reordering, caching)
- Facilitates visualization and debugging

**Implementation:**
```typescript
interface WorkflowDAG {
  nodes: Map<NodeId, WorkflowNode>;
  edges: Map<NodeId, Set<NodeId>>;  // dependency edges
  metadata: WorkflowMetadata;
}

type WorkflowNode =
  | { type: 'actor', actorClass: string, initArgs: any }
  | { type: 'port', portName: string, actorId: NodeId }
  | { type: 'channel', source: NodeId, target: NodeId }
  | { type: 'computation', operation: string, signature: string };
```

### 2. Implement Signature-Based Deduplication

**Action:** Create canonicalization functions that normalize operation signatures for deduplication.

**Benefits:**
- Reduces redundant work across similar queries
- Enables cross-workflow optimization
- Supports result sharing and caching

**Implementation:**
```typescript
interface OperationSignature {
  type: string;                    // operation type
  normalizedArgs: CanonicalArgs;   // order-independent args
  hash: string;                    // fast lookup key
}

function canonicalize(operation: Operation): OperationSignature {
  // 1. Sort object keys, normalize values
  // 2. Handle semantic equivalence (e.g., "a && b" === "b && a")
  // 3. Generate stable hash
}
```

### 3. Build Cost-Aware Query Optimizer

**Action:** Develop a cost model that estimates execution time/resources for different query plans.

**Parameters to Model:**
- Actor initialization cost
- Message passing latency
- Computation time per operation type
- Persistent state benefits (warm actors, caches)

**Benefits:**
- Choose optimal execution strategy from plan alternatives
- Balance latency vs. throughput tradeoffs
- Guide caching and prefetching decisions

**Implementation:**
```typescript
interface CostModel {
  estimateActorInit(actorClass: string): number;
  estimateMessagePassing(source: NodeId, target: NodeId): number;
  estimateComputation(signature: OperationSignature, context: ExecutionContext): number;
  estimatePlanCost(plan: ExecutionPlan, context: ExecutionContext): PlanCost;
}

interface PlanCost {
  makespan: number;      // critical path latency
  totalWork: number;     // aggregate computation
  resourceUsage: ResourceProfile;
}
```

### 4. Implement Memoized Plan Cache

**Action:** Cache optimized execution plans keyed by workflow structure and execution context.

**Cache Key Components:**
- Workflow DAG structure hash
- Actor state fingerprint (which actors are warm)
- Resource availability context

**Benefits:**
- Avoid redundant optimization for similar queries
- Fast plan lookup for common patterns
- Support plan templates with parameter substitution

**Implementation:**
```typescript
interface PlanCache {
  get(key: PlanCacheKey): ExecutionPlan | null;
  put(key: PlanCacheKey, plan: ExecutionPlan, stats: PlanStats): void;
  invalidate(predicate: (key: PlanCacheKey) => boolean): void;
}

interface PlanCacheKey {
  workflowHash: string;           // DAG structure fingerprint
  actorStateFingerprint: string;  // warm actors, cached data
  resourceContext: string;        // available resources
}
```

### 5. Enable Adaptive Execution with Online Profiling

**Action:** Track execution metrics per operation signature and use them to refine cost estimates.

**Metrics to Collect:**
- Execution latency (p50, p90, p99)
- Success/failure rates
- Resource consumption
- Cache hit rates

**Benefits:**
- Improve cost model accuracy over time
- Detect performance regressions
- Support A/B testing of plan alternatives

**Implementation:**
```typescript
interface ProfilingStore {
  recordExecution(signature: OperationSignature, metrics: ExecutionMetrics): void;
  getStatistics(signature: OperationSignature): OperationStatistics;
  updateCostEstimates(): void;  // periodic calibration
}

interface ExecutionMetrics {
  latencyMs: number;
  success: boolean;
  resourcesUsed: ResourceProfile;
  cacheHits: number;
  timestamp: number;
}

interface OperationStatistics {
  count: number;
  latencyDistribution: Percentiles;
  successRate: number;
  avgResourceUsage: ResourceProfile;
  lastUpdated: number;
}
```

### 6. Support Wavefront-Style Eager Execution

**Action:** Execute ready nodes as soon as dependencies are satisfied while respecting resource constraints.

**Benefits:**
- Masks tail latency with parallel execution
- Improves resource utilization
- Maintains dependency correctness

**Implementation:**
```typescript
interface WavefrontScheduler {
  schedule(dag: WorkflowDAG, context: ExecutionContext): AsyncIterable<ExecutionEvent>;
}

// Execution algorithm:
// 1. Maintain ready queue (dependencies satisfied)
// 2. Execute nodes from ready queue when resources available
// 3. On node completion, promote successors to ready queue
// 4. Balance locality (warm state) vs. parallelism
```

---

## Related Papers and Systems

### FlowMesh: Service Fabric for Composable LLM Workflows

**arXiv:** [2510.26913](https://arxiv.org/abs/2510.26913)
**Authors:** Junyi Shen et al.

**Key Contribution:** Multi-tenant service fabric that executes LLM workloads as one shared service instead of isolated pipelines. Decomposes workflows into fine-grained operators with lineage tracking, enabling cross-user deduplication and batching while preserving per-workflow provenance.

**Relevance:** Demonstrates value of fine-grained operator decomposition and global optimization across multiple tenants/workflows.

### Hexgen-Text2SQL: Hierarchical Scheduling for Agentic Workflows

**arXiv:** [2505.05286](https://arxiv.org/html/2505.05286v1)
**Key Contribution:** Hierarchical scheduling approach combining global workload-balanced task dispatching and local adaptive urgency-guided prioritization for heterogeneous GPU clusters.

**Relevance:** Shows how two-level scheduling (global + local) can balance different optimization objectives.

### DAAO: Difficulty-Aware Agentic Orchestration

**arXiv:** [2509.11079](https://arxiv.org/html/2509.11079)
**Key Contribution:** Dynamically generates query-specific multi-agent workflows guided by predicted query difficulty. Self-adjusting policy updates difficulty estimates based on workflow success.

**Relevance:** Demonstrates adaptive workflow generation based on task characteristics and execution feedback.

### AFlow: Automating Agentic Workflow Generation

**arXiv:** [2410.10762](https://arxiv.org/abs/2410.10762)
**Key Contribution:** Reformulates workflow optimization as search over code-represented workflows using Monte Carlo Tree Search to iteratively refine workflow structures.

**Relevance:** Shows how workflow structure itself can be optimized, not just execution of fixed workflows.

### DocETL: Agentic Query Rewriting

**Source:** [VLDB 2025 Paper](https://www.vldb.org/pvldb/vol18/p3035-shankar.pdf)
**Key Contribution:** Leverages agentic framework to evaluate pipelines and develops optimization inspired by Cascades optimizer, using top-down rule-based strategy to generate and evaluate equivalent plans.

**Relevance:** Demonstrates application of classic database optimization techniques (Cascades) to agentic workflows.

---

## Industry and Community Trends

### Cost Optimization Patterns (2025-2026)

**Key Practices:**
- Routing tasks to best model based on intent for accuracy and cost/latency optimization
- Capping per-run spend, batching where possible, trimming tokens
- OpenAI's Prompt Caching: up to 80% latency reduction, 90% input token savings
- Contract Net Protocol for tasks with agent bidding based on estimated cost and confidence

**Sources:**
- [Skywork AI: 20 Agentic Workflow Patterns](https://skywork.ai/blog/agentic-ai-examples-workflow-patterns-2025/)
- [Prompt Engineering 2026 Playbook](https://promptengineering.org/agents-at-work-the-2026-playbook-for-building-reliable-agentic-workflows/)

### Enterprise Adoption

**Microsoft Ignite 2025 Announcements:**
- Azure Copilot Optimization Agent for automated FinOps
- Foundry Control Plane for governance
- Model Router for intelligent model selection
- Agent Pre-Purchase Plans for cost management

**Source:** [FinOps Foundation: Microsoft Ignite 2025](https://www.finops.org/insights/microsoft-ignite-2025/)

### Design Principles from Practice

**Verification-Aware Design:**
- Turn checks into first-class tasks
- Make every subgoal measurable
- Design for observability from the start

**Token and Round-Trip Reduction:**
- Caching shared prompts and code-backed tools
- Batching compatible operations
- Minimizing LLM invocations for deterministic work

**Sources:**
- [Dexter Labs: AI Agentic Workflow Patterns](https://dextralabs.com/blog/ai-agentic-workflow-patterns-for-enterprises/)
- [Dynamiq: Best Practices](https://www.getdynamiq.ai/post/agentic-workflows-explained-benefits-use-cases-best-practices)

---

## Connections to Our Query Cache Design

### Current System Context

Our actor-based workflow system with query/DSL layer needs:
1. Efficient query plan generation and caching
2. Cost-based optimization for workflow execution
3. Adaptive strategies based on runtime feedback
4. Deduplication of redundant actor operations
5. Learning from execution history

### Direct Applications from Halo

| Halo Technique | Our Implementation | Expected Benefit |
|----------------|-------------------|------------------|
| Typed DAG representation | Actor workflow DAGs with Port/Channel nodes | Enable dependency analysis and optimization |
| Signature canonicalization | Normalize actor messages and operations | Deduplicate redundant work across workflows |
| Memoized DP optimizer | Cache query plans by structure + state | Avoid redundant plan generation |
| Cost model with state awareness | Model actor init, messaging, computation costs | Choose optimal execution strategies |
| Online profiling | Track per-signature execution statistics | Improve cost estimates over time |
| Wavefront execution | Eager execution of ready actors | Maximize parallelism while respecting dependencies |

### Architecture Recommendation

```
Query DSL
    ↓
Parser (DSL → Workflow DAG)
    ↓
Signature Canonicalizer (normalize operations)
    ↓
Plan Cache Lookup (structure + state → cached plan?)
    ↓
Cost-Based Optimizer (DP with memoization)
    ↓
Execution Plan
    ↓
Wavefront Scheduler (adaptive runtime)
    ↓
Profiling Store (collect metrics, update costs)
```

### Implementation Priorities

**Phase 1: Foundation (Week 1-2)**
1. Implement DAG representation for actor workflows
2. Create signature canonicalization for actor operations
3. Build basic cost model (simple latency estimates)

**Phase 2: Optimization (Week 3-4)**
4. Implement memoized DP optimizer
5. Add plan cache with structure + state keys
6. Develop wavefront scheduler

**Phase 3: Adaptation (Week 5-6)**
7. Add profiling store for execution metrics
8. Implement online cost calibration
9. Support plan quality feedback loop

---

## References and Resources

### Primary Paper
- Halo: [arXiv:2509.02121](https://arxiv.org/abs/2509.02121) - [HTML](https://arxiv.org/html/2509.02121)
- Authors: Junyi Shen, Noppanat Wadlom, Yao Lu (NUS)

### Related Academic Work
- FlowMesh: [arXiv:2510.26913](https://arxiv.org/abs/2510.26913)
- Hexgen-Text2SQL: [arXiv:2505.05286](https://arxiv.org/html/2505.05286v1)
- DAAO: [arXiv:2509.11079](https://arxiv.org/html/2509.11079)
- AFlow: [arXiv:2410.10762](https://arxiv.org/abs/2410.10762)
- DocETL: [VLDB 2025](https://www.vldb.org/pvldb/vol18/p3035-shankar.pdf)

### Code Resources
- Halo Demo: [github.com/mlsys-io/Halo_demo](https://github.com/mlsys-io/Halo_demo)

### Industry Resources
- [LlamaIndex Agentic Strategies](https://docs.llamaindex.ai/en/stable/optimizing/agentic_strategies/agentic_strategies/)
- [Skywork AI Workflow Patterns](https://skywork.ai/blog/agentic-ai-examples-workflow-patterns-2025/)
- [Prompt Engineering 2026 Playbook](https://promptengineering.org/agents-at-work-the-2026-playbook-for-building-reliable-agentic-workflows/)

### Academic Surveys
- Data Agents Survey: [github.com/HKUSTDial/awesome-data-agents](https://github.com/HKUSTDial/awesome-data-agents)
- LLM × Data Survey: [github.com/weAIDB/awesome-data-llm](https://github.com/weAIDB/awesome-data-llm)

---

## Conclusion

The Halo paper demonstrates that applying database query optimization principles to agentic workflows yields substantial performance improvements. The key insight is treating workflows as structured query plans with explicit DAG representations, enabling cost-based optimization, plan caching, and adaptive execution.

For our actor-based workflow system, adopting these techniques means:
- Representing workflows as typed DAGs for dependency analysis
- Implementing signature-based deduplication for cross-workflow optimization
- Building cost models that account for persistent state (warm actors, caches)
- Caching optimized plans keyed by structure and execution context
- Using online profiling to continuously improve cost estimates
- Supporting eager execution of ready actors while respecting dependencies

These database-inspired techniques provide a solid foundation for building an efficient query/DSL layer that can optimize actor workflow execution through plan-level reasoning rather than request-isolated approaches.

---

**Document Version:** 1.0
**Last Updated:** February 5, 2026
**Next Review:** After Phase 1 implementation (Week 2)
