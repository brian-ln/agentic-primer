# SQL Query Planner Concepts and Optimization Strategies

## Overview

This document explores how SQL query planners work, with a focus on concepts applicable to work/task planning systems. Query planners face similar challenges to work planners: decomposing problems, generating candidate plans, estimating costs, selecting optimal strategies, and adapting based on feedback.

---

## 1. Query Planner Architecture

### 1.1 Pipeline and Role

The query planner sits at the heart of the database execution pipeline, receiving a parsed and analyzed query tree and transforming it into an optimized execution plan. The planner's core responsibility is to explore the vast search space of possible execution strategies and select the plan with the lowest estimated cost.

**Key Components:**
- **Parser**: Converts SQL text into an abstract syntax tree (AST)
- **Analyzer**: Validates semantics, resolves names, checks permissions
- **Planner/Optimizer**: Generates and evaluates execution plans
- **Executor**: Executes the selected plan

### 1.2 Cost-Based Optimization

Modern database systems use **cost-based optimizers (CBO)** where every possible execution plan receives a cost estimate, and the planner chooses the plan with the lowest estimated cost. This contrasts with rule-based optimizers that apply fixed transformation rules.

**Cost Metrics** (PostgreSQL example):
- `seq_page_cost`: Cost of sequential disk page fetch (default: 1.0)
- `random_page_cost`: Cost of random disk page fetch (default: 4.0)
- `cpu_tuple_cost`: Cost of processing one row (default: 0.01)
- `cpu_index_tuple_cost`: Cost of processing one index entry (default: 0.005)
- `cpu_operator_cost`: Cost of executing an operator/function (default: 0.0025)

Costs are measured in arbitrary units, traditionally in units of disk page fetches. The optimizer combines these fundamental costs to estimate the total cost of any execution plan.

---

## 2. Problem Decomposition

### 2.1 Plan Tree Structure

A query plan is a **tree of operators** (nodes), where:
- Each node has a specific type (scan, join, aggregate, sort, etc.)
- Parent nodes pull data from child nodes row by row
- Leaf nodes are **scan operators** that read raw data from tables
- Internal nodes are **processing operators** that transform, filter, join, or aggregate data

**Example Plan Tree:**
```
Aggregate (SUM, COUNT)
  └─ Hash Join (customers.id = orders.customer_id)
      ├─ Sequential Scan (customers WHERE age > 25)
      └─ Index Scan (orders WHERE order_date > '2024-01-01')
```

### 2.2 Operator Types and Decomposition

Query planners decompose complex operations into primitive operators:

**Scan Operators:**
- Sequential Scan: Read entire table
- Index Scan: Use index to find specific rows
- Index Only Scan: Read data directly from index
- Bitmap Scan: Combine multiple indexes

**Join Operators:**
- Nested Loop Join: For small datasets or indexed lookups
- Hash Join: For medium-sized datasets with equality predicates
- Merge Join: For pre-sorted inputs or range predicates

**Processing Operators:**
- Filter: Apply WHERE conditions
- Aggregate: GROUP BY and aggregate functions
- Sort: ORDER BY operations
- Limit: Restrict result set size

The decomposition strategy allows the planner to mix and match operators to create diverse execution plans.

---

## 3. Plan Generation

### 3.1 Dynamic Programming: The Selinger Algorithm

Most database systems use a **Selinger-style optimizer** based on dynamic programming to determine join ordering. This algorithm, pioneered by Selinger et al. in the System R project, constructs optimal partial plans of increasing size.

**Algorithm:**
1. **Pass 1**: Generate optimal single-table access plans for each relation
   - Consider sequential scans, index scans, etc.
   - Keep the cheapest plan for each table
2. **Pass i**: Generate optimal plans for joining i tables
   - Combine optimal (i-1)-table plans with single-table plans
   - Consider all valid join orderings
   - Prune suboptimal plans
3. **Final Pass**: Select the globally optimal plan

**Key Insight**: Dynamic programming avoids redundant computation by storing optimal subplans and reusing them. Without this, the search space would explode exponentially.

**Complexity**: For n relations, there are O(n!) possible join orderings. Dynamic programming reduces this to O(n * 2^n), which is still exponential but much more tractable.

### 3.2 Interesting Orders

Planners don't just keep the single cheapest plan—they also keep plans that produce results in "interesting orders" (e.g., sorted by a specific column). A slightly more expensive plan might eliminate the need for an explicit sort operation later, making it the globally optimal choice.

**Example:**
- Plan A: Cost 100, unsorted output
- Plan B: Cost 120, output sorted by customer_id
- If the query requires ORDER BY customer_id, Plan B might be cheaper overall because it avoids a separate sort step (cost 50)

### 3.3 Search Space Pruning

For complex queries with many joins, exhaustive enumeration becomes computationally prohibitive. Planners use **heuristics** to prune the search space:

**Common Heuristics:**
- **Left-deep trees only**: Only consider plans where the right input of each join is a base table (not another join)
- **Avoid Cartesian products**: Don't consider cross-products until all join predicates are evaluated
- **Bushy tree restrictions**: Limit the shapes of join trees explored
- **Greedy join ordering**: Use heuristics to quickly find good (but not necessarily optimal) join orders

### 3.4 Genetic Query Optimizer (GEQO)

For very large join queries (e.g., 12+ tables), PostgreSQL switches to a **Genetic Query Optimizer** that uses genetic algorithms instead of dynamic programming. This trades optimality guarantees for reasonable planning time.

**How GEQO Works:**
1. Generate an initial population of random join orderings
2. Evaluate the fitness (cost) of each ordering
3. Select the fittest individuals
4. Apply genetic operators: crossover, mutation
5. Repeat for multiple generations
6. Return the best solution found

**Trade-off**: GEQO finds good plans quickly but may miss the globally optimal plan. For queries with 11 or fewer joins, PostgreSQL uses dynamic programming for guaranteed optimality.

---

## 4. Cost Estimation

### 4.1 Statistics and Cardinality Estimation

Accurate cost estimation depends on **statistics** about the data distribution. The optimizer uses these statistics to estimate **cardinality** (the number of rows) at each step of the plan.

**Key Statistics:**
- **Table size**: Number of rows and pages
- **Column distributions**: Histograms of value frequencies
- **Index selectivity**: How many rows match each index key
- **Correlation**: Relationships between columns
- **Null fraction**: Percentage of NULL values

**Histograms**: The most important statistical structure. A histogram divides the value range into buckets and stores the frequency of values in each bucket. The optimizer uses histograms to estimate how many rows will match a WHERE condition.

**Example Histogram:**
```
Column: age
Bucket 1: [0-20)   → 5,000 rows
Bucket 2: [20-40)  → 15,000 rows
Bucket 3: [40-60)  → 12,000 rows
Bucket 4: [60-100) → 3,000 rows
```

For a query `WHERE age > 25`, the optimizer interpolates within Bucket 2 and sums Buckets 3 and 4 to estimate ~25,000 rows.

### 4.2 Cardinality Estimation Challenges

**Key Assumptions** (often violated in practice):
- **Independence**: Column values are independent (rarely true for correlated data)
- **Uniformity**: Values are evenly distributed within each bucket (often false)
- **Up-to-date statistics**: Assumes ANALYZE has run recently (can be stale)

**Impact of Errors**: Cardinality estimation errors compound multiplicatively through joins. A 2x error at each of 5 join steps can result in a 32x error in the final estimate, potentially causing the optimizer to choose a wildly suboptimal plan.

### 4.3 Join Cardinality Estimation

Estimating the size of join results is particularly challenging. The optimizer must predict how many rows from the left table will match rows from the right table.

**Common Techniques:**
- **Histogram alignment**: Join histograms on the join columns using linear interpolation
- **Correlation statistics**: Use multi-column statistics when available
- **Sampling**: Take samples of the data to estimate join selectivity

**Modern Improvements**: SQL Server and other systems use simpler algorithms that align histograms using only minimum and maximum boundaries, avoiding complex interpolation that often introduces errors.

### 4.4 Machine Learning for Cost Estimation

Recent research (2025-2026) explores using **machine learning** to improve cardinality estimation. ML models can learn complex data distributions and correlations that traditional statistical methods miss.

**Approach**: Train models on historical query executions, using execution plan features as inputs and actual cardinalities as labels. The trained model predicts cardinalities for new queries.

**Challenges**: Requires extensive training data, may not generalize to new workloads, and introduces dependencies on external ML infrastructure.

---

## 5. Plan Selection and Execution

### 5.1 The Volcano Model

The **Volcano model** (also called the iterator model or pipeline model) is the standard execution framework used by most database systems. Each operator implements a simple interface:

```
open():  Initialize the operator
next():  Return the next tuple, or NULL if exhausted
close(): Clean up resources
```

**Characteristics:**
- **Pull-based**: Parent operators pull tuples from children on demand
- **Pipelined**: Tuples flow through the tree without materializing intermediate results
- **Composable**: Any operator can be plugged into any position in the tree
- **Lazy evaluation**: Operators produce results only when requested

**Example Execution:**
```
Aggregate.open()
  └─ Join.open()
      ├─ Scan(customers).open()
      └─ Scan(orders).open()

Aggregate.next()
  └─ Join.next()
      ├─ Scan(customers).next() → row1
      └─ Scan(orders).next() → row2
      └─ (match, return joined row)
  └─ (accumulate in aggregate state)
  ...
```

### 5.2 Plan Execution Modes

**Traditional: Plan Once, Execute Once**
- Planner generates a complete execution plan
- Executor runs the plan to completion
- No replanning or adaptation during execution

**Modern: Adaptive Execution**
- Planner generates an initial plan
- Executor collects runtime statistics
- Planner re-optimizes based on actual data
- Process repeats until query completes

---

## 6. Plan Caching and Reuse

### 6.1 Plan Cache Architecture

To avoid repeated planning overhead, database systems cache execution plans and reuse them for identical or similar queries.

**SQL Server Plan Cache:**
- **Object Plans (OBJCP)**: Plans for stored procedures, functions, triggers
- **SQL Plans (SQLCP)**: Plans for ad-hoc, dynamic, and parameterized queries

**Cache Workflow:**
1. Query arrives
2. Generate a cache key (normalized query text + environment)
3. Check cache for existing plan
4. **Cache Hit**: Reuse the cached plan (fast)
5. **Cache Miss**: Generate new plan, store in cache

### 6.2 Parameterization

**Problem**: Queries that differ only in literal values generate separate cache entries:
```sql
SELECT * FROM users WHERE id = 42;
SELECT * FROM users WHERE id = 99;
```

**Solution**: Parameterize queries to enable plan reuse:
```sql
SELECT * FROM users WHERE id = @param1;
```

SQL Server supports:
- **Forced Parameterization**: Automatically parameterize queries
- **Prepared Statements**: Explicitly parameterize with application code

### 6.3 Cache Invalidation

Plans must be invalidated when underlying assumptions change:

**Invalidation Triggers:**
- Schema changes (ALTER TABLE, DROP INDEX)
- Statistics updates (ANALYZE)
- Configuration changes (memory limits, cost parameters)
- Plan age/staleness

### 6.4 Parameter Sniffing

**The Problem**: The optimizer uses the first parameter values it sees to generate a plan. If data is skewed, the plan might be optimal for those values but terrible for others.

**Example:**
```sql
-- First call: @age = 25 (common, returns 10,000 rows)
-- Optimizer chooses: Sequential Scan
-- Plan cached

-- Second call: @age = 95 (rare, returns 50 rows)
-- Optimizer reuses: Sequential Scan (bad choice! Index would be better)
```

**Mitigations:**
- **Optimize for Ad Hoc Workloads**: Store only plan stubs initially, full plan only after reuse
- **Recompile Hints**: Force replanning for specific queries
- **Adaptive Query Processing**: Adjust plans at runtime based on actual data

### 6.5 Cross-Database Comparison

**PostgreSQL**: Takes a dynamic approach, avoiding parameter sniffing issues but requiring explicit configuration for plan reuse. Plans are cached per session, not globally.

**SQL Server**: Aggressively caches execution plans globally, reducing planning overhead but introducing parameter sniffing risks when parameter values vary widely.

---

## 7. Parallel Query Execution

### 7.1 Parallel Plan Generation

When estimated query cost exceeds a threshold, the optimizer considers **parallel execution**. This involves:

1. **Partitioning work**: Divide the dataset into chunks
2. **Assigning workers**: Allocate threads/processes to each chunk
3. **Coordinating execution**: Use a gather node to collect results from workers
4. **Merging results**: Combine partial results into final output

**PostgreSQL Parallel Query:**
```
Finalize Aggregate (Gather results from workers)
  └─ Gather (Coordinator node)
      └─ Partial Aggregate (Worker 1, 2, 3, ...)
          └─ Parallel Sequential Scan
```

### 7.2 Parallelism Strategies

**Intra-operator parallelism**: Multiple threads execute the same operator on different data partitions
- Example: Parallel sequential scan divides table into chunks, one per worker

**Inter-operator parallelism**: Different operators execute concurrently on different nodes
- Example: Build hash table for join while scanning probe side

**Pipeline parallelism**: Operators at different levels of the tree execute simultaneously
- Example: Aggregation starts as soon as first rows arrive from join

### 7.3 Parallel Execution Considerations

**When Parallelism Helps:**
- Large datasets (scans, aggregations)
- CPU-intensive operations (complex functions, sorts)
- Low concurrency (few concurrent queries)

**When Parallelism Hurts:**
- Small datasets (overhead exceeds benefit)
- I/O-bound operations (parallelism doesn't help if disk is the bottleneck)
- High concurrency (parallel queries consume more resources, slowing other queries)
- OLTP workloads (queries are fast enough serially)

**Trade-off**: Parallelism speeds up individual queries but increases resource consumption. Optimizers must balance individual query performance against system-wide throughput.

---

## 8. Adaptive Query Execution (AQE)

### 8.1 The Adaptive Paradigm Shift

Traditional query execution follows a **"plan once, execute once"** model. Adaptive Query Execution (AQE) breaks this paradigm by introducing **runtime replanning** based on actual data characteristics observed during execution.

**Key Idea**: The optimizer makes decisions with imperfect information (statistics). As the query executes, the system learns the true data distribution and can adjust the plan accordingly.

### 8.2 AQE Architecture

**Execute-Reoptimize-Execute Cycle:**
1. **Initial Planning**: Generate plan based on statistics
2. **Partial Execution**: Execute stages until materialization point
3. **Collect Runtime Stats**: Measure actual cardinalities, data skew, resource usage
4. **Reoptimization**: Replan remaining stages with accurate statistics
5. **Continue Execution**: Execute new plan
6. **Repeat**: Continue cycle until query completes

**Materialization Points**: Stages where intermediate results are fully computed (e.g., after shuffles, sorts, or aggregations). These are natural checkpoints for replanning.

### 8.3 AQE Optimizations

**1. Dynamic Join Strategy Selection**

The optimizer initially chooses a join strategy (e.g., sort-merge join) based on estimated sizes. At runtime, if the actual size is much smaller, AQE can switch to a more efficient strategy (e.g., broadcast hash join).

**Example:**
```
Initial Plan: Sort-Merge Join (estimated: 1M rows × 500K rows)
Runtime Stats: Actual right side is only 10K rows
Replanned: Broadcast Hash Join (much faster for small right side)
```

**2. Skew Join Optimization**

Data skew occurs when some partition keys are much more frequent than others, causing some workers to process far more data than others (straggler problem).

**Detection**: AQE detects skew by analyzing shuffle file statistics across partitions.

**Mitigation**: Split skewed partitions into smaller sub-partitions and assign them to multiple workers, balancing the load.

**3. Dynamic Partition Coalescing**

After a shuffle, some partitions may be very small. Processing each tiny partition separately introduces overhead (task startup, scheduling, etc.).

**Solution**: AQE combines adjacent small partitions into larger ones, reducing the number of tasks and improving efficiency.

### 8.4 Feedback Loops

AQE introduces **feedback loops** at multiple levels:

**Query-level feedback**: Adjust the current query's plan based on runtime observations.

**Workload-level feedback**: Use historical query information to improve statistics. The optimizer refines its cost model based on past prediction errors, leading to better initial plans for future queries.

**System-level feedback**: Adaptive systems resemble **reinforcement learning**, where the system continuously learns and adjusts based on feedback from data and query execution.

---

## 9. Reflection and Learning Mechanisms

### 9.1 Plan Quality Feedback

Advanced query engines compare **estimated vs. actual** metrics after execution:

- **Cardinality errors**: Actual row counts vs. estimated row counts at each operator
- **Cost errors**: Actual execution time vs. estimated cost
- **Resource usage**: Memory, CPU, I/O consumption vs. predictions

**Use Cases:**
- Identify systematic estimation biases
- Detect stale or missing statistics
- Trigger automatic statistics updates
- Log problematic queries for DBA review

### 9.2 Cardinality Estimation Feedback (SQL Server)

SQL Server's Intelligent Query Processing includes **Cardinality Estimation Feedback**:

1. Detect when CE errors cause poor plans
2. Generate alternative plans with adjusted cardinality estimates
3. Test alternative plans on subsequent executions
4. Persist the better plan in the cache

**Result**: The system automatically corrects CE errors without manual intervention.

### 9.3 Adaptive Learning Systems

Future query optimizers may incorporate **machine learning-based learning loops**:

- **Online learning**: Continuously update ML models as queries execute
- **Meta-learning**: Learn which optimization strategies work best for different query patterns
- **Transfer learning**: Apply lessons from one workload to new workloads

---

## 10. Key Insights for Work/Task Planning

### 10.1 Problem Decomposition

**SQL Insight**: Break complex queries into primitive operators (scan, join, aggregate, etc.) that can be composed flexibly.

**Task Planning Insight**: Decompose work into atomic tasks or task molecules with clear inputs, outputs, and dependencies. Use a standard interface (analogous to the Volcano iterator model) so tasks can be composed arbitrarily.

**Application**:
- Define task primitives (fetch, transform, analyze, synthesize, validate, etc.)
- Each task has clear contracts: inputs required, outputs produced, side effects
- Tasks compose into workflows/pipelines without special handling

### 10.2 Plan Generation

**SQL Insight**: Use dynamic programming to explore the space of possible plans efficiently. Keep not just the single best plan, but also plans with "interesting properties" (e.g., sorted output).

**Task Planning Insight**: Generate multiple candidate task orderings. Consider not just the fastest plan, but also plans that optimize for other properties (resource usage, risk, parallelizability, learning value).

**Application**:
- Generate task graphs with different orderings, parallelization strategies, resource allocations
- Use dynamic programming to avoid recomputing subplans
- Keep a diverse set of Pareto-optimal plans (trading off cost, time, risk, quality)

### 10.3 Cost Estimation

**SQL Insight**: Cost estimation is fundamental but inherently imperfect. Use statistics, but recognize their limitations (independence assumptions, staleness, correlations).

**Task Planning Insight**: Estimate task costs (time, resources, failure risk) based on historical data and heuristics. Accept that estimates will be wrong and design for adaptation.

**Application**:
- Collect task execution metrics (time, tokens used, success rate)
- Build cost models using histograms or ML
- Update estimates continuously as new data arrives
- Use confidence intervals, not point estimates

### 10.4 Search Space Pruning

**SQL Insight**: For complex queries, exhaustive search is infeasible. Use heuristics (left-deep trees, avoid Cartesian products) or stochastic methods (genetic algorithms) to find good plans quickly.

**Task Planning Insight**: For complex projects, exploring every possible task ordering is impossible. Apply heuristics (prefer independent tasks, batch similar operations) or use Monte Carlo tree search to guide exploration.

**Application**:
- Prioritize promising regions of the search space
- Use beam search or simulated annealing
- Balance exploration (finding novel plans) with exploitation (refining known good plans)

### 10.5 Plan Caching and Reuse

**SQL Insight**: Cache execution plans for reuse. Parameterize queries to enable cache hits across similar but not identical queries.

**Task Planning Insight**: Cache task plans for common patterns. Recognize that plans for similar projects can be adapted rather than regenerated from scratch.

**Application**:
- Identify plan templates (e.g., "data pipeline", "research → synthesis → review", "build → test → deploy")
- Parameterize plans (dataset, target quality, deadline) so they can be reused
- Invalidate cached plans when assumptions change (new tools, updated requirements)

### 10.6 Parameter Sniffing Analogy

**SQL Insight**: Plans optimized for one set of parameters may be terrible for others (parameter sniffing problem).

**Task Planning Insight**: Plans optimized for one context (e.g., high-urgency project) may be inappropriate for another (e.g., exploratory research). Avoid over-fitting to the first instance.

**Application**:
- Generate adaptive plans that check assumptions before committing to strategies
- Use conditional branching (if low-confidence, allocate more verification tasks)
- Prefer robust plans that perform reasonably across a range of scenarios

### 10.7 Adaptive Execution

**SQL Insight**: Replan at runtime based on observed data characteristics. Don't commit to the entire plan upfront.

**Task Planning Insight**: Monitor task execution and replan as information arrives. Treat plans as hypotheses to be tested, not immutable specifications.

**Application**:
- Define checkpoints (after each task or task molecule) where replanning can occur
- Collect metrics during execution (actual time, quality, new information revealed)
- Replan the remaining work based on updated state and estimates
- Implement an "execute-reoptimize-execute" cycle analogous to AQE

### 10.8 Feedback Loops

**SQL Insight**: Compare estimated vs. actual metrics. Use discrepancies to improve statistics and cost models for future queries.

**Task Planning Insight**: Track prediction errors for task costs and outcomes. Use feedback to refine future estimates and identify systematic biases.

**Application**:
- Log actual task durations, token usage, success rates
- Compare to estimates and flag large discrepancies
- Update cost models (histograms, ML models) with new data
- Detect when tools or environments change (analogous to schema changes triggering plan invalidation)

### 10.9 Parallel Execution

**SQL Insight**: Parallelize operators when the cost exceeds a threshold. Partition data, assign workers, coordinate with gather nodes.

**Task Planning Insight**: Identify independent tasks that can run in parallel. Balance parallelism (faster completion) against resource contention (higher costs, potential for errors).

**Application**:
- Build task dependency graphs to identify parallelizable tasks
- Use a gather/merge step to combine results from parallel tasks
- Dynamically adjust parallelism based on system load and task urgency
- Recognize when serial execution is better (small tasks, limited resources)

### 10.10 Interesting Properties

**SQL Insight**: Keep plans that produce results in interesting orders, even if slightly more expensive, because they may eliminate downstream costs.

**Task Planning Insight**: Consider not just the fastest plan, but plans with valuable intermediate properties (e.g., early validation, incremental deliverables, learning opportunities).

**Application**:
- Prefer plans that fail fast (validate assumptions early)
- Value plans that produce intermediate artifacts (documents, prototypes) that are useful even if the final task fails
- Consider "learning value": some tasks teach us more about the problem space, making future tasks cheaper

### 10.11 Genetic Algorithms for Complex Planning

**SQL Insight**: When the search space is too large, switch from exact algorithms to stochastic methods like genetic algorithms.

**Task Planning Insight**: For large, complex projects, use evolutionary or Monte Carlo methods to explore the space of possible plans.

**Application**:
- Generate an initial population of random or heuristic-based plans
- Evaluate fitness (estimated cost, risk, quality)
- Apply crossover (combine parts of two plans) and mutation (random changes)
- Iterate to find increasingly better plans
- Accept that the result may not be globally optimal, but is good enough given time constraints

### 10.12 Statistics and Observability

**SQL Insight**: Accurate statistics are the foundation of good planning. Stale or missing statistics lead to poor plans.

**Task Planning Insight**: Instrument task execution thoroughly. Without good data on task costs and outcomes, planning is guesswork.

**Application**:
- Log every task execution with rich metadata (start time, duration, resources, outcome)
- Periodically "ANALYZE" (update statistics) on task libraries
- Detect when statistics are stale (environment changes, new tools, different data)
- Provide visibility into cost model assumptions so users can override when needed

### 10.13 Volcano Model for Task Execution

**SQL Insight**: The Volcano model provides a uniform interface (open/next/close) for all operators, enabling flexible composition and pipelining.

**Task Planning Insight**: Define a standard task interface (initialize/execute/finalize) so tasks can be composed into workflows without special handling.

**Application**:
- Each task implements a standard protocol
- Tasks are pull-based: parent tasks pull results from child tasks
- Enable pipelining: start downstream tasks as soon as upstream tasks produce partial results
- Support lazy evaluation: don't execute tasks until their outputs are needed

### 10.14 Plan Stability vs. Adaptability

**SQL Insight**: There's a tension between plan stability (caching, reuse) and adaptability (replanning, AQE). Both are valuable.

**Task Planning Insight**: Balance between committing to a plan (reduces overhead, enables coordination) and staying flexible (adapts to new information).

**Application**:
- Use a hybrid approach: stable high-level plan with adaptive low-level execution
- Define "replanning triggers": conditions under which replanning is warranted
- Communicate plan changes to stakeholders (human users, dependent systems)
- Track plan versions to understand how plans evolve over time

---

## 11. Conclusion

SQL query planners are sophisticated systems that have evolved over decades to handle the challenges of optimizing complex queries in the face of uncertainty. The core principles—problem decomposition, cost-based planning, search space exploration, adaptive execution, and feedback loops—are broadly applicable to task and work planning systems.

**Key Takeaways:**

1. **Decompose** complex work into composable primitives with clear interfaces
2. **Generate** multiple candidate plans using dynamic programming or heuristic search
3. **Estimate** costs using historical data, but accept imperfection and design for adaptation
4. **Prune** the search space using heuristics when exhaustive search is infeasible
5. **Cache** plans for common patterns, but invalidate when assumptions change
6. **Execute** adaptively: monitor reality, compare to estimates, replan as needed
7. **Learn** from feedback: track errors, update cost models, improve future planning
8. **Parallelize** when appropriate, balancing speed against resource costs
9. **Instrument** thoroughly: good planning requires good statistics
10. **Balance** stability (efficiency, coordination) with adaptability (resilience, learning)

By applying these lessons from SQL query planners, we can build more robust, efficient, and adaptive systems for planning and executing complex workflows in agentic systems.

---

## Sources

### PostgreSQL Query Planner Architecture
- [How PostgreSQL Query Planner Really Works | by Amit Dhiman | Medium](https://medium.com/@amittdhiman91/how-postgresql-query-planner-really-works-bf3ce33c8f7a)
- [PostgreSQL: Documentation: 18: 19.7. Query Planning](https://www.postgresql.org/docs/current/runtime-config-query.html)
- [The Basics of Postgres Query Planning · pganalyze](https://pganalyze.com/docs/explain/basics-of-postgres-query-planning)
- [PostgreSQL: Documentation: 18: 51.5. Planner/Optimizer](https://www.postgresql.org/docs/current/planner-optimizer.html)
- [What Is The Cost In PostgreSQL EXPLAIN Query - ScaleGrid](https://scalegrid.io/blog/postgres-explain-cost/)

### Cost Estimation and Machine Learning
- [Redefining Cost Estimation in Database Systems: The Role of Execution Plan Features and Machine Learning](https://arxiv.org/html/2510.05612)
- [PostgreSQL: Documentation: 18: 14.1. Using EXPLAIN](https://www.postgresql.org/docs/current/using-explain.html)
- [Reading a Postgres EXPLAIN ANALYZE Query Plan](https://thoughtbot.com/blog/reading-an-explain-analyze-query-plan)

### Plan Caching and Reuse
- [SQL Server Query Plan Cache - GeeksforGeeks](https://www.geeksforgeeks.org/sql-server/sql-server-query-plan-cache/)
- [Query Processing Architecture Guide - SQL Server | Microsoft Learn](https://learn.microsoft.com/en-us/sql/relational-databases/query-processing-architecture-guide?view=sql-server-ver16)
- [SQL Plan Cache and Parameterized SQL Queries](https://www.mssqltips.com/sqlservertip/8259/sql-plan-cache-and-parameterized-sql-queries/)
- [Understanding SQL Server query plan cache](https://www.sqlshack.com/understanding-sql-server-query-plan-cache/)
- [Query plan caching in CockroachDB](https://www.cockroachlabs.com/blog/query-plan-caching-in-cockroachdb/)
- [Planning For Reuse: Plan Caches vs bind-peeking and parameter sniffing](https://use-the-index-luke.com/blog/2011-07-16/planning-for-reuse)
- [Understanding Query Plan Caching in PostgreSQL and How It Differs from SQL Server](https://www.sqlpassion.at/archive/2025/02/10/understanding-query-plan-caching-in-postgresql-and-how-it-differs-from-sql-server/)

### Parallel Query Execution
- [Parallel Query Processing and Optimization in DBMS | by Sarang S. Babu | Medium](https://medium.com/plumbersofdatascience/parallel-query-processing-and-optimization-in-dbms-7c8eeab4fc02)
- [Query Processing Architecture Guide - SQL Server | Microsoft Learn](https://learn.microsoft.com/en-us/sql/relational-databases/query-processing-architecture-guide?view=sql-server-ver16)
- [The basics of Parallel Execution Plans in SQL Server](https://www.sqlshack.com/the-basics-of-parallel-execution-plans-in-sql-server/)
- [Improve query performance with parallel queries in Amazon RDS for PostgreSQL](https://aws.amazon.com/blogs/database/improve-query-performance-with-parallel-queries-in-amazon-rds-for-postgresql-and-amazon-aurora-postgresql-compatible-edition/)
- [Optimizing SQL Server: Understanding Parallelism in Execution Plan | by Rafael Rampineli](https://rafaelrampineli.medium.com/optimizing-sql-server-understanding-parallelism-in-execution-plan-c985f278f1c4)

### Selinger Algorithm and Join Optimization
- [Assignment 4: Join Optimization](https://courses.cms.caltech.edu/cs122/assignments/lab4.html)
- [Query Optimization](https://people.eecs.berkeley.edu/~brewer/cs262/queryopt.html)
- [Part 2: Query Optimization | CS186 Projects](https://cs186.gitbook.io/project/assignments/proj3/part-2-query-optimization)
- [Adaptive Optimization of Very Large Join Queries Thomas Neumann](https://db.in.tum.de/~radke/papers/hugejoins.pdf)
- [Join Order Search - PostgreSQL wiki](https://wiki.postgresql.org/wiki/Join_Order_Search)
- [Iterative Dynamic Programming: A New Class of Query Optimization Algorithms](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.43.4235&rep=rep1&type=pdf)

### Heuristics and Genetic Algorithms
- [Query Optimization Using Genetic Algorithms](https://scialert.net/fulltext/?doi=rjit.2010.139.144)
- [(PDF) A Genetic Algorithm for Database Query Optimization](https://www.researchgate.net/publication/2673329_A_Genetic_Algorithm_for_Database_Query_Optimization)
- [Genetic Query Optimization in Database Systems](https://pirlwww.lpl.arizona.edu/resources/guide/software/postgreSQL/programmer/c21.htm)
- [A genetic algorithm for database query optimization | IEEE Conference Publication](https://ieeexplore.ieee.org/document/349926/)
- [Using Heuristics in Query Optimization](https://www.brainkart.com/article/Using-Heuristics-in-Query-Optimization_11540/)

### Adaptive Query Execution
- [How to Speed up SQL Queries with Adaptive Query Execution](https://www.databricks.com/blog/2020/05/29/adaptive-query-execution-speeding-up-spark-sql-at-runtime.html)
- [Adaptive query execution - Azure Databricks | Microsoft Learn](https://learn.microsoft.com/en-us/azure/databricks/optimizations/aqe)
- [Adaptive Query Execution (AQE)](https://celerdata.com/glossary/adaptive-query-execution)
- [Adaptive Query Execution in Streaming | Databricks Blog](https://www.databricks.com/blog/adaptive-query-execution-structured-streaming)
- [Adaptive Query Optimization — Oracle SQL & PL/SQL Optimization for Developers](https://oracle.readthedocs.io/en/latest/sql/plans/adaptive-query-optimization.html)
- [Enhancing query performance with Adaptive Query Processing in SQL Server 2017](https://www.microsoft.com/en-us/sql-server/blog/2017/09/28/enhancing-query-performance-with-adaptive-query-processing-in-sql-server-2017/)

### Volcano Model
- [Volcano-An Extensible and Parallel Query Evaluation System](https://cs-people.bu.edu/mathan/reading-groups/papers-classics/volcano.pdf)
- [Volcano Model: Research on the Scalable Architecture of Database Query Systems](https://www.oreateai.com/blog/volcano-model-research-on-the-scalable-architecture-of-database-query-systems/79e43b2337079f11e37f84d66bd900db)
- [The Volcano Optimizer Generator: Extensibility and Efficient Search](https://sfu-db.github.io/dbsystems/Lectures/VolcanoOptimizer.pdf)
- [Encapsulation of parallelism in the Volcano query processing system | the morning paper](https://blog.acolyer.org/2015/02/11/encapsulation-of-parallelism-in-the-volcano-query-processing-system/)
- [Evolution of Database Query Engines | by Peng Wang](https://medium.com/@wpleonardo0537/evolution-of-database-query-engines-1410cdb8b5a4)

### Cardinality Estimation and Statistics
- [Cardinality Estimation (SQL Server) - SQL Server | Microsoft Learn](https://learn.microsoft.com/en-us/sql/relational-databases/performance/cardinality-estimation-sql-server?view=sql-server-ver17)
- [Understanding Teradata Statistics Histograms](https://www.dwhpro.com/teradata-statistics-fail/)
- [Cardinality estimation feedback - SQL Server | Microsoft Learn](https://learn.microsoft.com/en-us/sql/relational-databases/performance/intelligent-query-processing-cardinality-estimation-feedback?view=sql-server-ver17)
- [Unrevealed tips of SQL Server Statistics](https://www.sqlshack.com/unrevealed-tips-of-sql-server-statistics/)
- [Statistics - SQL Server | Microsoft Learn](https://learn.microsoft.com/en-us/sql/relational-databases/statistics/statistics?view=sql-server-ver17)
- [Histograms](https://docs.oracle.com/database/121/TGSQL/tgsql_histo.htm)
