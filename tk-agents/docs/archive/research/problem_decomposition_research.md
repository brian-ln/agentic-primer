# Problem Decomposition and Planning Research

**Research Date:** January 16, 2026
**Focus:** Problem understanding → definition → decomposition → plan generation → execution

## Executive Summary

This document explores five major research areas that address the complete pipeline from understanding problems to generating executable plans:

1. **AI Planning (STRIPS, HTN, PDDL)** - Classical approaches to hierarchical task decomposition and automated planning
2. **Genetic Algorithms & Evolutionary Methods (GEQO)** - Optimization through evolution and population-based search
3. **Program Synthesis** - Transforming specifications into executable code through formal methods
4. **Operations Research & Constraint Programming** - Mathematical optimization and constraint satisfaction
5. **Information Foraging Theory** - Understanding how agents identify and gather needed information

These areas provide complementary perspectives on problem-solving: AI Planning offers formal decomposition structures; Genetic Algorithms handle complex search spaces through evolution; Program Synthesis addresses specification formalization; Operations Research provides mathematical rigor; and Information Foraging Theory explains information need identification.

**Key Insight:** The most effective problem-solving systems combine multiple approaches—using hierarchical decomposition (HTN) to break down problems, constraint programming to verify feasibility, evolutionary methods to explore large solution spaces, and information foraging principles to identify knowledge gaps.

---

## Area 1: AI Planning (STRIPS, HTN, PDDL)

### Overview

AI Planning is the automated reasoning about actions and their effects to generate plans that achieve specified goals. It provides formal frameworks for representing problems, decomposing tasks hierarchically, and generating executable action sequences.

**Historical Context:**
- **STRIPS** (Stanford Research Institute Problem Solver) - developed in 1971, foundational planning system
- **HTN** (Hierarchical Task Networks) - emerged in the 1970s-80s, enables hierarchical decomposition
- **PDDL** (Planning Domain Definition Language) - standardized in 1998, lingua franca of AI planning
- **HDDL** (Hierarchical Domain Definition Language) - proposed 2019, extends PDDL for HTN planning

### Core Concepts

**STRIPS Planning:**
- **State representation:** Logical predicates describing world state
- **Actions:** Defined by preconditions, add-effects, delete-effects
- **Goal:** Set of predicates to satisfy
- **Plan:** Sequence of actions transforming initial state to goal state

**HTN Planning:**
- **Primitive tasks:** Directly executable actions (like STRIPS actions)
- **Compound tasks:** High-level tasks requiring decomposition
- **Methods:** Recipes showing how to decompose compound tasks into subtasks
- **Goal tasks:** Desired outcomes (generalization of STRIPS goals)

**Key Difference:** STRIPS plans by searching state space; HTN plans by decomposing tasks according to domain knowledge encoded in methods.

### Problem Formalization

**STRIPS Formulation:**
```
State: {on(block-a, table), clear(block-a), on(block-b, block-c), ...}
Actions: pickup(X), putdown(X), stack(X,Y), unstack(X,Y)
Goal: {on(block-a, block-b), on(block-b, block-c)}
```

**HTN Formulation:**
```
Compound Task: transport-package(pkg, from, to)
Method 1: truck-delivery
  - Subtasks: [load-truck(pkg, truck), drive-truck(truck, from, to), unload-truck(pkg, truck)]
Method 2: air-delivery (for long distances)
  - Subtasks: [load-plane(pkg, plane), fly-plane(plane, from, to), unload-plane(pkg, plane)]
```

**PDDL Domain Definition:**
```pddl
(define (domain logistics)
  (:requirements :strips :typing)
  (:types vehicle package location)
  (:predicates
    (at ?obj - object ?loc - location)
    (in ?pkg - package ?veh - vehicle))
  (:action load
    :parameters (?p - package ?v - vehicle ?l - location)
    :precondition (and (at ?v ?l) (at ?p ?l))
    :effect (and (in ?p ?v) (not (at ?p ?l)))))
```

### Decomposition Strategy

**HTN Decomposition Process:**

1. **Top-Down Decomposition:** Start with high-level goals, progressively refine into primitive actions
2. **Method Selection:** For each compound task, choose applicable method based on current state
3. **Constraint Propagation:** Ensure subtasks maintain consistency with constraints
4. **Recursion:** Apply decomposition recursively until only primitive tasks remain

**Example - "Prepare Dinner" Task:**
```
Level 0 (Abstract): prepare-dinner
Level 1 (Compound): [plan-menu, shop-ingredients, cook-meal, serve-dinner]
Level 2 (Compound): cook-meal → [prep-ingredients, cook-main, cook-side, plate-food]
Level 3 (Primitive): prep-ingredients → [wash-vegetables, chop-vegetables, measure-spices]
```

**Advantages of Hierarchical Decomposition:**
- **Reduces search space:** Focus on relevant decompositions rather than all action sequences
- **Encodes domain knowledge:** Methods capture expert knowledge about how to solve problems
- **Natural problem structure:** Matches human thinking about complex tasks
- **Reusable components:** Methods can be applied across different problems

### Plan Generation

**HTN Planning Algorithm (Simplified):**

```
function HTN-Planning(task-list, state, methods)
  if task-list is empty then
    return []  // Success - empty plan

  task ← first(task-list)
  remaining ← rest(task-list)

  if task is primitive then
    if preconditions(task) satisfied in state then
      new-state ← apply(task, state)
      rest-plan ← HTN-Planning(remaining, new-state, methods)
      if rest-plan ≠ failure then
        return [task] + rest-plan

  else // task is compound
    for each method applicable to task in current state do
      subtasks ← decompose(task, method)
      extended-tasks ← subtasks + remaining
      plan ← HTN-Planning(extended-tasks, state, methods)
      if plan ≠ failure then
        return plan

  return failure
```

**Key Planning Techniques:**

- **Forward Planning:** Start from initial state, apply actions to reach goal
- **Backward Planning:** Start from goal, work backwards to find necessary preconditions
- **Partial-Order Planning:** Build plans with minimal ordering constraints
- **HTN Planning:** Decompose tasks hierarchically using domain methods

**Complexity:**
- STRIPS planning: PSPACE-complete in general
- HTN planning: Undecidable in general, but many restricted versions are decidable
- Practical HTN planning: Often more efficient than STRIPS due to reduced search space

### Resource Identification

**Planning Domain Analysis:**

1. **State Variables:** What properties of the world need to be tracked?
2. **Objects and Types:** What entities exist in the domain?
3. **Actions/Operators:** What primitive operations are available?
4. **Preconditions:** What information is needed to determine action applicability?
5. **Effects:** What changes occur when actions are executed?
6. **Resources:** What consumable or limited resources exist (time, fuel, memory)?

**Information Requirements:**

- **Domain Knowledge:** Understanding of the problem domain to write correct PDDL/methods
- **Initial State:** Complete specification of starting conditions
- **Goal Specification:** Clear definition of success criteria
- **Action Models:** Accurate models of what actions do (preconditions and effects)
- **Constraints:** Temporal constraints, resource limits, safety requirements

**Tool Identification:**

Modern AI planning requires:
- **Planners:** Fast Downward, SHOP2, PANDA, MetricFF
- **Domain Modeling:** PDDL editors, domain validators
- **Plan Verification:** Plan validators, simulation environments
- **Plan Execution:** Execution monitoring, replanning capabilities

### Applications to Task Planning

**Direct Applications:**

1. **Task Decomposition:** HTN methods directly model how complex tasks break into subtasks
2. **Dependency Management:** PDDL preconditions/effects capture task dependencies
3. **Resource Allocation:** Planning with numeric fluents handles resource constraints
4. **Contingency Planning:** Conditional planning handles uncertain outcomes

**Mapping to Agent/Task Systems:**

```javascript
// HTN-style task decomposition in agent system
class Task {
  isCompound() { /* check if needs decomposition */ }

  getDecompositions(context) {
    // Return applicable methods for decomposing this task
    // Similar to HTN methods selection
  }

  getPreconditions() {
    // What must be true to execute this task?
    // Similar to STRIPS preconditions
  }

  getEffects() {
    // What changes after task execution?
    // Similar to STRIPS effects
  }
}

// HTN-inspired planning
function decomposeTask(task, context) {
  if (task.isPrimitive()) {
    return [task];
  }

  const methods = task.getDecompositions(context);
  for (const method of methods) {
    const subtasks = method.apply(context);
    if (feasible(subtasks, context)) {
      return subtasks.flatMap(st => decomposeTask(st, context));
    }
  }

  throw new Error(`No valid decomposition for ${task}`);
}
```

**Integration with Graph/TaskNode System:**

- **Nodes:** Represent tasks at various abstraction levels
- **Edges:** Represent decomposition relationships and dependencies
- **Methods:** Stored as node metadata defining how to decompose
- **State:** Context attached to graph, updated as tasks complete
- **Planning:** Graph traversal guided by HTN decomposition logic

### Key References

**Foundational Papers:**
- Fikes & Nilsson (1971). "STRIPS: A New Approach to the Application of Theorem Proving to Problem Solving." *Artificial Intelligence* 2(3-4): 189-208.
- Erol, Hendler, Nau (1994). "HTN Planning: Complexity and Expressivity." *AAAI-94*: 1123-1128.
- Nau et al. (2003). "SHOP2: An HTN Planning System." *JAIR* 20: 379-404.

**Recent Developments:**
- Höller et al. (2020). "HDDL: A Language to Describe Hierarchical Planning Problems." *arXiv:1911.05499*
- Aiello & Georgievski (2024). "Introduction to AI Planning." *arXiv:2412.11642*

**Tools and Resources:**
- [International Planning Competition](http://www.icaps-conference.org/competitions/)
- [PDDL Reference](https://planning.wiki/)
- [HTN Planning GitHub](https://github.com/ronwalf/HTN-Translation)

**Game AI Applications:**
- Humphreys (2013). "Exploring HTN Planners through Example." *Game AI Pro*, Chapter 12.

### Example: Logistics Planning

**Problem:** Deliver packages using trucks and airplanes.

**STRIPS Formulation:**
```
Initial State:
  at(pkg1, city-a)
  at(truck1, city-a)
  at(plane1, airport-a)

Goal State:
  at(pkg1, city-b)

Actions:
  load-truck(pkg, truck, loc)
  unload-truck(pkg, truck, loc)
  drive-truck(truck, from, to)
  load-plane(pkg, plane, loc)
  unload-plane(pkg, plane, loc)
  fly-plane(plane, from, to)
```

**HTN Formulation:**
```
Task: deliver-package(pkg1, city-a, city-b)

Method: truck-delivery (if same-region(city-a, city-b))
  1. get-truck(truck1, city-a)
  2. load-package(pkg1, truck1)
  3. drive-truck(truck1, city-a, city-b)
  4. unload-package(pkg1, truck1)

Method: air-delivery (if different-region(city-a, city-b))
  1. get-truck(truck1, city-a)
  2. transport-to-airport(pkg1, truck1, airport-a)
  3. get-plane(plane1, airport-a)
  4. load-package(pkg1, plane1)
  5. fly-plane(plane1, airport-a, airport-b)
  6. unload-package(pkg1, plane1)
  7. transport-from-airport(pkg1, city-b)

Decomposition of get-truck(truck1, city-a):
  - If truck already at city-a: []
  - Otherwise: [drive-truck(truck1, current-location, city-a)]
```

**Why HTN is More Efficient:**
- STRIPS must search through all possible action sequences
- HTN uses domain knowledge (methods) to guide decomposition
- HTN search space much smaller: only considers sensible decompositions
- HTN naturally handles hierarchical abstractions

---

## Area 2: Genetic Algorithms & Evolutionary Methods (Focus on GEQO)

### Overview

Genetic Algorithms (GAs) are optimization techniques inspired by biological evolution. They work by maintaining a population of candidate solutions and iteratively improving them through selection, crossover, and mutation. GEQO (Genetic Query Optimization) is a specialized application in PostgreSQL for optimizing complex join queries.

**Biological Inspiration:**
- **Population:** Set of candidate solutions
- **Fitness:** Quality metric for solutions
- **Selection:** Prefer better solutions for reproduction
- **Crossover:** Combine parts of parent solutions
- **Mutation:** Random variations to explore new solutions
- **Generations:** Iterative improvement over time

### Core Concepts

**Genetic Algorithm Components:**

1. **Encoding (Genotype):** How solutions are represented
   - Binary strings: `[0,1,1,0,1,0,1,1]`
   - Permutations: `[3,1,4,2,5]` (e.g., join order)
   - Trees: Expression trees for program synthesis
   - Real-valued vectors: `[0.23, 0.89, 0.45]`

2. **Fitness Function:** Evaluates solution quality
   - Must be computable for all genotypes
   - Guides selection towards better solutions
   - In GEQO: estimated query execution cost

3. **Selection:** Choose parents for next generation
   - **Roulette wheel:** Probability proportional to fitness
   - **Tournament:** Compete small groups, pick winner
   - **Rank-based:** Select based on fitness ranking

4. **Crossover (Recombination):** Combine parent solutions
   - **One-point:** Split at random position, swap tails
   - **Two-point:** Swap middle segment
   - **Uniform:** Randomly pick bits from each parent
   - **Order crossover:** Preserve relative order (for permutations)

5. **Mutation:** Random changes for exploration
   - **Bit flip:** Change 0→1 or 1→0
   - **Swap:** Exchange two positions
   - **Inversion:** Reverse a subsequence
   - **Gaussian:** Add random noise (real-valued)

**Algorithm Structure:**
```
function GeneticAlgorithm(problem, pop_size, generations)
  population ← initialize_random_population(pop_size)
  evaluate_fitness(population)

  for gen = 1 to generations do
    parents ← select_parents(population)
    offspring ← []

    for each pair (p1, p2) in parents do
      (c1, c2) ← crossover(p1, p2)
      c1 ← mutate(c1)
      c2 ← mutate(c2)
      offspring ← offspring + [c1, c2]

    evaluate_fitness(offspring)
    population ← select_survivors(population, offspring)

  return best_solution(population)
```

### Problem Formalization: GEQO in PostgreSQL

**The Query Optimization Problem:**

Given a SQL query joining N tables, find the optimal join order. The search space grows as:
- **3 tables:** 12 possible join trees
- **4 tables:** 120 possible join trees
- **10 tables:** ~176 billion possible join trees
- **N tables:** Catalan number: (2N-2)! / ((N-1)! × N!)

**Why GEQO?**
- Exhaustive search infeasible for queries with >12 tables
- Dynamic programming still expensive (O(2^N))
- GEQO provides good solutions in reasonable time for large N

**GEQO Encoding:**

A join order is encoded as a permutation of table numbers:
```
Query: SELECT * FROM t1, t2, t3, t4 WHERE t1.a = t2.b AND t2.c = t3.d AND t3.e = t4.f

Chromosome: [2, 4, 1, 3]
Meaning: Join t2 and t4 first, then join t1, then join t3

Decoded to join tree:
    ⨝
   / \
  ⨝   t3
 / \
⨝   t1
/ \
t2  t4
```

**GEQO Fitness Function:**

The fitness is the **estimated query cost** from PostgreSQL's standard cost model:
```
Cost = Σ (CPU cost + I/O cost + startup cost)
```

Lower cost = higher fitness. The standard planner's cost estimation code evaluates each candidate join order.

**GEQO Process in PostgreSQL:**

1. **Random Initialization:** Generate random join order permutations
2. **Fitness Evaluation:** Use standard planner to estimate cost of each join order
3. **Selection:** Keep lower-cost candidates ("fitter" individuals)
4. **Crossover:** Combine portions of good join orders
5. **Mutation:** Random swaps in join order
6. **Iteration:** Repeat for specified number of generations
7. **Return:** Best join order found

**Implementation Details (PostgreSQL):**

Located in `src/backend/optimizer/geqo/`:
- `geqo_main.c`: Main GA loop
- `geqo_pool.c`: Population management
- `geqo_selection.c`: Parent selection (linear bias)
- `geqo_erx.c`: Edge recombination crossover (preserves adjacencies)
- `geqo_px.c`: Partially mapped crossover
- `geqo_mutation.c`: Random swaps

**Parameter Settings:**
```sql
-- Enable GEQO for queries with 12+ tables
SET geqo_threshold = 12;

-- Population size: based on number of relations
-- Default formula: gimme_pool_size()
SET geqo_pool_size = 0;  -- auto

-- Number of generations
SET geqo_generations = 0;  -- auto

-- Selection bias (1.5-2.0)
SET geqo_selection_bias = 2.0;

-- Random seed for reproducibility
SET geqo_seed = 0.0;
```

### Decomposition Strategy

Genetic algorithms don't explicitly decompose problems but implicitly decompose through **building blocks**:

**Schema Theory (Holland's Building Blocks):**

- **Schema:** Pattern describing subset of solutions
  - Example: `[*, 3, *, 1, *]` means "3 in position 2, 1 in position 4"
- **Building Block Hypothesis:** GA assembles good solutions by combining short, low-order, high-fitness schemas
- **Implicit Parallelism:** Population simultaneously explores many schemas

**For GEQO:**

- **Good subsequences:** Parts of join orders that work well
  - Example: If tables `t2` and `t5` frequently joined together in good solutions
- **Crossover preserves:** Keep good subsequences from both parents
- **Mutation explores:** New combinations of building blocks

**Multi-Objective Decomposition:**

When optimizing multiple objectives:
1. **Pareto Frontier:** Set of non-dominated solutions
2. **NSGA-II:** Non-dominated Sorting GA maintains diversity along Pareto front
3. **Objective Decomposition:** Handle each objective separately, combine via selection

### Plan Generation

Genetic algorithms generate plans through **evolutionary search**:

**Exploration vs. Exploitation:**
- **Exploration:** Mutation creates new variations, discovers new regions
- **Exploitation:** Selection and crossover refine good solutions
- **Balance:** Critical for performance
  - Too much exploration: Random search, slow convergence
  - Too much exploitation: Premature convergence to local optima

**Convergence:**
```
Generation 0:   Fitness: [1000, 980, 1200, 890, 1100, ...]
                Best: 890
Generation 10:  Fitness: [720, 680, 750, 640, 710, ...]
                Best: 640
Generation 50:  Fitness: [520, 510, 530, 505, 515, ...]
                Best: 505
Generation 100: Fitness: [490, 485, 495, 483, 488, ...]
                Best: 483 (converged)
```

**When to Stop:**
- Fixed number of generations (GEQO default)
- Fitness threshold reached
- Convergence detected (population diversity too low)
- Time budget exhausted

**GEQO Plan Quality:**

PostgreSQL documentation notes:
> "The fitness of each candidate join sequence is estimated by running the standard planner's join selection and cost estimation code from scratch."

This means:
- GEQO finds join **order**
- Standard planner determines join **methods** (hash, nested loop, merge)
- Quality depends on both GA effectiveness and cost model accuracy

**Limitations:**
- Non-deterministic (different runs may find different solutions)
- No optimality guarantee
- Repeated work (re-evaluating similar solutions)
- Parameter sensitivity

### Resource Identification

**What GAs Need:**

1. **Problem Encoding:** How to represent solutions as chromosomes
   - Must capture all solution aspects
   - Should enable meaningful crossover
   - Balance expressiveness vs. search space size

2. **Fitness Function:** How to evaluate solution quality
   - Must be computable efficiently (evaluated many times)
   - Should correlate with true quality
   - May be approximate (GEQO uses cost estimates, not actual execution)

3. **Operators:** Selection, crossover, mutation appropriate for encoding
   - Permutation encoding → order-preserving crossover
   - Tree encoding → subtree swap crossover
   - Real-valued → arithmetic crossover, Gaussian mutation

4. **Parameters:** Population size, generations, crossover/mutation rates
   - Larger populations: More exploration, slower convergence
   - More generations: Better solutions, longer runtime
   - Typical: Population 50-200, Generations 100-1000

5. **Computational Resources:**
   - Time: O(pop_size × generations × fitness_evaluation_cost)
   - Memory: O(pop_size × solution_size)
   - Parallelization: Fitness evaluation easily parallelized

**Information Requirements:**

- **Solution Representation:** Understanding problem structure to design encoding
- **Objective Function:** Clear definition of solution quality
- **Constraints:** Feasibility requirements (repair infeasible solutions or penalize)
- **Domain Knowledge:** To design effective crossover/mutation operators

### Applications to Task Planning

**When to Use Genetic Algorithms for Task Planning:**

✅ **Good fit:**
- Large search spaces where exhaustive search infeasible
- No clear decomposition structure available
- Problem has building blocks that combine well
- Multiple near-optimal solutions acceptable
- Approximate solutions sufficient

❌ **Poor fit:**
- Small search spaces (exhaustive search better)
- Strong hierarchical decomposition available (HTN better)
- Require optimality guarantees
- Real-time requirements (GAs too slow)

**Task Scheduling with GAs:**

Encode schedule as chromosome:
```
Task assignment: [A→Worker1, B→Worker2, C→Worker1, D→Worker3, ...]
Task ordering: [B, A, D, C, E, F, ...]
```

Fitness:
```
Minimize: total_time + deadline_violations + resource_conflicts
```

**Hybrid Approaches:**

Combine GA with other methods:
1. **HTN + GA:** HTN for hierarchical decomposition, GA for optimization within levels
2. **GA + Local Search:** GA for global exploration, hill-climbing for refinement
3. **GA + ML:** Learn fitness function from past evaluations (surrogate models)

**Example: Task Assignment Optimization**

```javascript
// Genetic algorithm for task-to-agent assignment
class TaskAssignmentGA {
  constructor(tasks, agents, fitnessFunction) {
    this.tasks = tasks;
    this.agents = agents;
    this.fitness = fitnessFunction;
  }

  // Chromosome: array mapping task_id -> agent_id
  encodeAssignment(assignment) {
    return this.tasks.map(task => assignment.get(task.id));
  }

  decodeAssignment(chromosome) {
    const assignment = new Map();
    this.tasks.forEach((task, i) => {
      assignment.set(task.id, this.agents[chromosome[i]]);
    });
    return assignment;
  }

  crossover(parent1, parent2) {
    // Uniform crossover: randomly pick agent from either parent
    return parent1.map((agent1, i) =>
      Math.random() < 0.5 ? agent1 : parent2[i]
    );
  }

  mutate(chromosome) {
    // Randomly reassign some tasks to different agents
    return chromosome.map(agent_id =>
      Math.random() < 0.1 ?
        Math.floor(Math.random() * this.agents.length) :
        agent_id
    );
  }

  evolve(populationSize, generations) {
    let population = this.initializePopulation(populationSize);

    for (let gen = 0; gen < generations; gen++) {
      const fitnesses = population.map(ind => this.fitness(ind));
      const parents = this.selectParents(population, fitnesses);
      const offspring = [];

      for (let i = 0; i < parents.length; i += 2) {
        const child1 = this.crossover(parents[i], parents[i+1]);
        const child2 = this.crossover(parents[i+1], parents[i]);
        offspring.push(this.mutate(child1), this.mutate(child2));
      }

      population = this.selectSurvivors(population, offspring, fitnesses);
    }

    return this.getBest(population);
  }
}
```

### Key References

**Foundational Work:**
- Holland, J.H. (1975). *Adaptation in Natural and Artificial Systems*. University of Michigan Press.
- Goldberg, D.E. (1989). *Genetic Algorithms in Search, Optimization, and Machine Learning*. Addison-Wesley.
- Mitchell, M. (1996). *An Introduction to Genetic Algorithms*. MIT Press.

**GEQO Specific:**
- [PostgreSQL Documentation: Genetic Query Optimizer](https://www.postgresql.org/docs/current/geqo.html)
- [PostgreSQL GEQO Implementation](https://www.postgresql.org/docs/current/geqo-pg-intro.html)
- PostgreSQL source: `src/backend/optimizer/geqo/README`

**Advanced Topics:**
- Deb, K. (2001). *Multi-Objective Optimization using Evolutionary Algorithms*. Wiley.
- Eiben, A.E. & Smith, J.E. (2015). *Introduction to Evolutionary Computing*. Springer.

**Applications:**
- Koza, J.R. (1992). *Genetic Programming*. MIT Press. [Tree-based GAs for program synthesis]
- Whitley, D. (1989). "The GENITOR Algorithm and Selection Pressure." *ICGA-89*: 116-121.

### Example: Query Join Optimization with GEQO

**Problem:**
Optimize join order for query with 8 tables (40,320 possible join orders).

**Query:**
```sql
SELECT * FROM customers c
  JOIN orders o ON c.id = o.customer_id
  JOIN order_items oi ON o.id = oi.order_id
  JOIN products p ON oi.product_id = p.id
  JOIN categories cat ON p.category_id = cat.id
  JOIN suppliers s ON p.supplier_id = s.id
  JOIN warehouses w ON s.warehouse_id = w.id
  JOIN regions r ON w.region_id = r.id
WHERE r.name = 'West' AND cat.type = 'Electronics';
```

**GEQO Process:**

1. **Initial Population (gen 0):**
```
[c, o, oi, p, cat, s, w, r] → cost: 15,420
[r, w, s, cat, p, oi, o, c] → cost: 12,380  ← good start (starts with filters)
[p, s, c, r, w, cat, o, oi] → cost: 23,450
[w, r, cat, p, oi, s, o, c] → cost: 18,900
... (population of 64)
```

2. **After 20 Generations:**
```
[r, cat, w, s, p, oi, o, c] → cost: 9,240  ← best so far
[cat, r, w, s, p, oi, o, c] → cost: 9,380
[r, w, cat, s, p, oi, o, c] → cost: 9,620
[r, w, s, cat, p, oi, o, c] → cost: 10,100
... (population converging)
```

3. **Final Solution (gen 50):**
```
Best: [r, cat, w, s, p, oi, o, c] → cost: 9,180
Join tree:
         ⨝ (customers)
        / \
       ⨝   c
      / \
     ⨝   o
    / \
   ⨝   oi
  / \
 ⨝   p
/ \
⨝   s
/ \
⨝   w
/ \
r  cat
```

**Why This Order is Good:**
1. Start with `r` (region = 'West'): highly selective filter
2. Join with `cat` (category = 'Electronics'): another filter
3. Continue with `w`, `s`: small intermediate results
4. Then `p`, `oi`, `o`, `c`: builds up to larger tables

**Compared to Random:**
- Random join order average cost: ~18,000
- GEQO solution cost: 9,180
- **49% improvement** in estimated execution time

**Execution Plan:**
```
HashJoin (cost=9180 .. )
  -> HashJoin (cost=7840 .. )
    -> HashJoin (cost=6200 .. )
      -> HashJoin (cost=4500 .. )
        -> HashJoin (cost=2800 .. )
          -> HashJoin (cost=1200 .. )
            -> HashJoin (cost=450 .. )
              -> Seq Scan on regions r
                 Filter: (name = 'West')
              -> Seq Scan on categories cat
                 Filter: (type = 'Electronics')
            -> Index Scan on warehouses w
          -> Index Scan on suppliers s
        -> Index Scan on products p
      -> Index Scan on order_items oi
    -> Index Scan on orders o
  -> Seq Scan on customers c
```

---

## Area 3: Program Synthesis

### Overview

Program Synthesis is the task of automatically constructing programs that provably satisfy a given high-level specification. Unlike traditional programming where developers write code, synthesis systems generate code from specifications—ranging from formal logical specifications to input-output examples.

**Key Idea:** Move from **how** to **what**—specify what the program should do, let the synthesizer figure out how.

### Core Concepts

**Synthesis Paradigms:**

1. **Deductive Synthesis:** From formal specifications (pre/post-conditions)
   - Input: Logical formula φ(x, y) relating inputs x to outputs y
   - Output: Program P such that ∀x. P(x) = y ⇔ φ(x, y)

2. **Inductive Synthesis:** From input-output examples
   - Input: Set of examples {(x₁, y₁), (x₂, y₂), ..., (xₙ, yₙ)}
   - Output: Program P such that P(xᵢ) = yᵢ for all i
   - Must generalize beyond given examples

3. **Syntax-Guided Synthesis (SyGuS):** Combines formal specs with grammar constraints
   - Input: Logical specification + context-free grammar
   - Output: Program satisfying spec, expressible in grammar

4. **Programming-by-Example (PBE):** User-friendly inductive synthesis
   - User provides examples
   - System infers program
   - Example: FlashFill in Excel

**Counter-Example Guided Inductive Synthesis (CEGIS):**

Central technique in modern synthesis:
```
function CEGIS(spec, grammar)
  examples = ∅

  loop:
    candidate = synthesize(examples, grammar)  // Inductive generalization

    if candidate = ⊥ then
      return FAIL  // No program satisfies examples

    counter_example = verify(candidate, spec)  // Deductive verification

    if counter_example = ⊥ then
      return candidate  // Verified correct

    examples = examples ∪ {counter_example}  // Add counter-example
```

**Key Insight:** Interplay between inductive synthesizer (fast, unsound) and deductive verifier (slow, sound) achieves both efficiency and correctness.

### Problem Formalization

**Deductive Synthesis Example:**

```
Specification: Find maximum of array
  Pre: array A of length n ≥ 1
  Post: result r such that:
    (∀i. 0 ≤ i < n ⇒ r ≥ A[i]) ∧  // r is upper bound
    (∃j. 0 ≤ j < n ∧ r = A[j])     // r appears in array

Synthesized Program:
  max = A[0]
  for i = 1 to n-1:
    if A[i] > max:
      max = A[i]
  return max

Proof: By induction on loop invariant:
  max = maximum of A[0..i]
```

**Inductive Synthesis Example:**

```
Examples:
  f([1, 2, 3]) = 6
  f([4, 5]) = 9
  f([10]) = 10
  f([2, 2, 2, 2]) = 8

Inferred Program:
  def f(lst):
    return sum(lst)

Alternative:
  def f(lst):
    total = 0
    for x in lst:
      total += x
    return total
```

**Syntax-Guided Synthesis (SyGuS) Example:**

```
Specification: Integer square root
  (∀x. x ≥ 0 ⇒ result*result ≤ x < (result+1)*(result+1))

Grammar:
  E ::= x | c | E + E | E - E | E * E | if B then E else E
  B ::= E ≤ E | E ≥ E | not B | B and B

Synthesized:
  isqrt(x):
    r = 0
    while (r+1)*(r+1) ≤ x:
      r = r + 1
    return r
```

### Decomposition Strategy

**Decomposition in Synthesis:**

1. **Component-Based Synthesis:**
   - Library of components (functions, APIs)
   - Synthesize by composing components
   - Search over component combinations

2. **Type-Directed Synthesis:**
   - Use types to guide search
   - Only consider well-typed compositions
   - Example: Need `String → Int`, have `String → [Char]` and `[Char] → Int`
   - Compose: `list_to_int ∘ string_to_chars`

3. **Syntax-Guided Decomposition:**
   - Grammar restricts search space
   - Non-terminal symbols define sub-problems
   - Recursively synthesize for each non-terminal

4. **Divide-and-Conquer Synthesis:**
   - Partition input-output examples by behavior
   - Synthesize separate programs for each partition
   - Combine with conditional (if-then-else)

**Example: Synthesizing Conditional Programs**

```
Examples:
  f(0) = 0, f(1) = 1, f(2) = 2, f(3) = 3,
  f(4) = 2, f(5) = 2, f(6) = 2, f(7) = 2

Observation: Different behavior for x < 4 vs x ≥ 4

Partition:
  P1: {(0,0), (1,1), (2,2), (3,3)} → synthesize g₁(x) = x
  P2: {(4,2), (5,2), (6,2), (7,2)} → synthesize g₂(x) = 2

Combine:
  f(x) = if x < 4 then g₁(x) else g₂(x)
       = if x < 4 then x else 2
```

**Multi-Modal Synthesis:**

Combine different synthesis techniques:
- Use deductive synthesis for verified core
- Use inductive synthesis for heuristic components
- Use neural synthesis for pattern recognition

### Plan Generation

**Search Strategies:**

1. **Enumerative Search:**
   - Enumerate programs in size order
   - Test each against specification
   - Guaranteed to find smallest solution
   - Exponential complexity

2. **Stochastic Search:**
   - Genetic programming, MCMC
   - Probabilistic exploration
   - No completeness guarantee
   - Can handle large spaces

3. **Constraint-Based Synthesis:**
   - Encode synthesis as SAT/SMT problem
   - Let solver find satisfying program
   - Leverages powerful solver technology

4. **Neural-Guided Synthesis:**
   - Train neural network to predict likely programs
   - Use as heuristic in search
   - Combines learning with symbolic reasoning

**Version Space Algebras:**

Represent set of consistent programs compactly:
```
Version Space = all programs consistent with examples

Operations:
  - Join: Union of version spaces
  - Intersection: Common refinement
  - Filtering: Eliminate inconsistent programs

Example (FlashFill):
  E1: "John Smith" → "Smith, J."
  E2: "Mary Jones" → "Jones, M."

  Version space contains:
    LastName(x) + ", " + FirstInitial(x) + "."
    Split(x, " ")[1] + ", " + Split(x, " ")[0][0] + "."
    ...

  All transformations matching both examples
```

**Synthesis as Search:**

```
function EnumerativeSynthesis(examples, grammar, max_size)
  for size = 1 to max_size do
    programs = enumerate(grammar, size)
    for prog in programs do
      if satisfies_examples(prog, examples) then
        return prog
  return FAIL

function ConstraintBasedSynthesis(spec, grammar)
  formula = encode_synthesis_problem(spec, grammar)
  model = SMT_solve(formula)
  if model ≠ UNSAT then
    return decode_program(model)
  else
    return FAIL
```

### Resource Identification

**What Program Synthesis Needs:**

1. **Specifications:**
   - **Formal specs:** Requires expertise, precise but difficult to write
   - **Examples:** Easy to provide, but may be ambiguous
   - **Partial programs:** Sketches with holes to fill
   - **Natural language:** Accessible but ambiguous

2. **Domain Knowledge:**
   - **Component library:** Available functions, APIs, primitives
   - **Grammar:** Syntactic constraints on program shape
   - **Type system:** Constrains composition
   - **Domain-specific abstractions:** E.g., string transformations for FlashFill

3. **Verification:**
   - **Test suite:** Check candidate programs
   - **Formal verification:** Prove correctness
   - **Runtime monitoring:** Detect violations during execution

4. **Search Strategy:**
   - **Enumerative:** Systematic but slow
   - **Stochastic:** Fast but incomplete
   - **Constraint-based:** Leverages solvers
   - **Learned heuristics:** Neural guidance

5. **Computational Resources:**
   - **Time:** Synthesis can be expensive (exponential worst-case)
   - **Memory:** Version spaces, program caches
   - **Solver capacity:** SMT solvers, SAT solvers

### Applications to Task Planning

**Task Planning as Synthesis:**

```
Specification: Goal state + constraints
Program: Sequence of actions achieving goal

Synthesis approach:
  - Actions = program statements
  - State transitions = program semantics
  - Goal condition = postcondition
  - Constraints = invariants to maintain
```

**Planning Domain as Grammar:**

```
Grammar for task planning:
  Plan ::= ε | Action ; Plan
  Action ::= PrimitiveAction | Sequence | Parallel | Choice | Loop
  Sequence ::= Action ; Action
  Parallel ::= Action ∥ Action
  Choice ::= if Condition then Action else Action
  Loop ::= while Condition do Action
```

**Example: Synthesizing Task Plans**

```javascript
// Synthesis-based task planner
class SynthesisPlanner {
  constructor(actions, preconditions, effects) {
    this.actions = actions;
    this.preconds = preconditions;
    this.effects = effects;
  }

  // Synthesize plan using CEGIS approach
  synthesizePlan(initialState, goalState) {
    const examples = [{ input: initialState, output: goalState }];
    let candidate = null;

    while (true) {
      // Inductive phase: synthesize candidate from examples
      candidate = this.inductiveSynthesis(examples);

      if (!candidate) {
        throw new Error("No plan satisfies constraints");
      }

      // Verification phase: check if candidate achieves goal in all states
      const counterExample = this.verify(candidate, goalState);

      if (!counterExample) {
        return candidate; // Verified correct
      }

      // Add counter-example and retry
      examples.push(counterExample);
    }
  }

  inductiveSynthesis(examples) {
    // Search for action sequence satisfying examples
    for (const plan of this.enumeratePlans()) {
      if (examples.every(ex => this.executes(plan, ex.input, ex.output))) {
        return plan;
      }
    }
    return null;
  }

  verify(plan, goalState) {
    // Symbolic execution to find counter-example
    const reachableStates = this.symbolicExecution(plan);
    for (const state of reachableStates) {
      if (!satisfies(state, goalState)) {
        return { input: state, output: goalState };
      }
    }
    return null; // Verified
  }
}
```

**Advantages of Synthesis Approach:**

- **Correctness:** Synthesized plans provably correct (with verification)
- **Optimality:** Can optimize for plan length, resource usage
- **Expressiveness:** Handle complex control flow (loops, conditionals)
- **Reusability:** Synthesized plans can be parameterized and reused

**Challenges:**

- **Scalability:** Synthesis can be slow for large state spaces
- **Specification:** Requires clear goal specification
- **Ambiguity:** Multiple valid plans may exist

### Key References

**Foundational Work:**
- Manna, Z. & Waldinger, R. (1980). "A Deductive Approach to Program Synthesis." *TOPLAS* 2(1): 90-121.
- Gulwani, S. (2011). "Automating String Processing in Spreadsheets Using Input-Output Examples." *POPL*: 317-330. [FlashFill]

**Synthesis Frameworks:**
- Solar-Lezama, A. (2008). "Program Synthesis by Sketching." PhD Thesis, UC Berkeley. [Sketch tool]
- Alur, R. et al. (2013). "Syntax-Guided Synthesis." *FMCAD*: 1-8. [SyGuS framework]

**CEGIS and Verification:**
- Jha, S. et al. (2010). "Oracle-Guided Component-Based Program Synthesis." *ICSE*: 215-224.
- Polozov, O. & Gulwani, S. (2015). "FlashMeta: A Framework for Inductive Program Synthesis." *OOPSLA*: 107-126.

**Recent Developments:**
- Balog, M. et al. (2017). "DeepCoder: Learning to Write Programs." *ICLR*.
- Ellis, K. et al. (2021). "DreamCoder: Growing Generalizable, Interpretable Knowledge with Wake-Sleep Bayesian Program Learning." *PLDI*.

**Surveys:**
- Gulwani, S. et al. (2017). "Program Synthesis." *Foundations and Trends in PL* 4(1-2): 1-119.
- [Program Synthesis Wikipedia](https://en.wikipedia.org/wiki/Program_synthesis)

### Example: Synthesizing String Transformations (FlashFill-style)

**Problem:** Transform names from "First Last" to "Last, F."

**Examples:**
```
"John Smith"   → "Smith, J."
"Mary Johnson" → "Johnson, M."
```

**Component Library:**
```
Substring(s, i, j)       // s[i:j]
FirstToken(s, delim)     // first part of split
LastToken(s, delim)      // last part of split
Concat(s1, s2)           // s1 + s2
FirstChar(s)             // s[0]
```

**Synthesis Process:**

1. **Generate candidate programs:**
```
P₁: Concat(LastToken(x, " "), Concat(", ", FirstChar(FirstToken(x, " "))))
P₂: LastToken(x, " ") + ", " + FirstChar(x) + "."
P₃: Substring(x, IndexOf(x, " ")+1, Length(x)) + ", " + x[0] + "."
... (many candidates)
```

2. **Filter by examples:**
```
Test "John Smith":
  P₁: "Smith" + ", " + "J" + "." = "Smith, J." ✓
  P₂: "Smith" + ", " + "J" + "." = "Smith, J." ✓
  P₃: "Smith" + ", " + "J" + "." = "Smith, J." ✓

Test "Mary Johnson":
  P₁: "Johnson" + ", " + "M" + "." = "Johnson, M." ✓
  P₂: "Johnson" + ", " + "M" + "." = "Johnson, M." ✓
  P₃: "Johnson" + ", " + "M" + "." = "Johnson, M." ✓
```

3. **Rank by simplicity:**
```
P₂ is simplest: uses fewest operations
```

4. **Return synthesized program:**
```python
def transform(name):
  parts = name.split(" ")
  return parts[1] + ", " + parts[0][0] + "."
```

**User Experience:**
- User provides 1-2 examples
- FlashFill synthesizes transformation
- Applies to entire column instantly
- If wrong, user provides correcting example

---

## Area 4: Operations Research & Constraint Programming

### Overview

Operations Research (OR) applies advanced analytical methods to help make better decisions. Constraint Programming (CP) is a paradigm for solving combinatorial problems by stating constraints that must be satisfied. Together, they provide powerful mathematical frameworks for optimization and feasibility checking.

**Historical Context:**
- OR emerged during WWII for military logistics optimization
- Linear Programming (Dantzig, 1947): Simplex algorithm
- Constraint Programming (1970s-80s): Logic programming, constraint satisfaction
- Modern integration: Hybrid CP/OR solvers combining strengths

### Core Concepts

**Mathematical Optimization:**

General form:
```
Minimize: f(x)           // objective function
Subject to:
  g_i(x) ≤ 0  ∀i        // inequality constraints
  h_j(x) = 0  ∀j        // equality constraints
  x ∈ X                 // domain constraints
```

**Optimization Types:**

1. **Linear Programming (LP):**
   - Objective and constraints are linear
   - Efficiently solvable (polynomial time)
   - Simplex, interior-point methods

2. **Mixed-Integer Programming (MIP):**
   - Some variables restricted to integers
   - NP-hard in general
   - Branch-and-bound, cutting planes

3. **Constraint Satisfaction Problem (CSP):**
   - Find assignment satisfying all constraints
   - No objective function (feasibility only)
   - Backtracking search, arc consistency

4. **Constraint Optimization Problem (COP):**
   - CSP + objective function to optimize
   - Combines satisfaction and optimization

**Constraint Programming:**

**Variables:** X = {x₁, x₂, ..., xₙ}
**Domains:** D = {D₁, D₂, ..., Dₙ} where xᵢ ∈ Dᵢ
**Constraints:** C = {c₁, c₂, ..., cₘ}

Example CSP:
```
Variables: X={A, B, C}
Domains: D_A = {1,2,3}, D_B = {1,2,3}, D_C = {1,2,3}
Constraints:
  A ≠ B
  B < C
  A + B = C

Solution: A=1, B=2, C=3
```

**Key CP Techniques:**

1. **Constraint Propagation:**
   - Deduce information from constraints
   - Reduce domains before search
   - Arc-consistency, bounds-consistency

2. **Search:**
   - Backtracking: Try values, backtrack on conflict
   - Variable ordering: Which variable to assign next
   - Value ordering: Which value to try first

3. **Global Constraints:**
   - High-level constraints with efficient propagation
   - Examples: AllDifferent, Cumulative, Circuit

**Integration Strategies (CP + OR):**

1. **Relaxation:** Use LP/MIP relaxation for CP bounds
2. **Decomposition:** Solve subproblems with CP, coordinate with OR
3. **Hybrid:** Combine CP propagation with OR optimization
4. **Column Generation:** CP for subproblem, MIP for master problem
5. **Logic-Based Benders:** OR for relaxation, CP for refinement

### Problem Formalization

**Scheduling Example (Constraint Programming):**

```
Problem: Schedule 5 tasks on 3 machines
  Tasks: T1, T2, T3, T4, T5
  Durations: d1=3, d2=2, d3=4, d4=1, d5=3
  Precedences: T1 → T3, T2 → T4, T3 → T5
  Resource: Each machine handles 1 task at a time

Variables:
  start_i: start time of task i
  machine_i: which machine for task i

Domains:
  start_i ∈ [0, 100]
  machine_i ∈ {1, 2, 3}

Constraints:
  start_3 ≥ start_1 + 3  (precedence T1 → T3)
  start_4 ≥ start_2 + 2  (precedence T2 → T4)
  start_5 ≥ start_3 + 4  (precedence T3 → T5)

  no_overlap(machine_i, start_i, duration_i)  (resource constraint)

Objective:
  Minimize: max(start_i + duration_i)  (makespan)
```

**Transportation Problem (Operations Research):**

```
Problem: Ship goods from warehouses to customers at minimum cost

Parameters:
  m warehouses with supply s_i
  n customers with demand d_j
  cost c_ij to ship from i to j

Decision Variables:
  x_ij = amount shipped from warehouse i to customer j

Formulation:
  Minimize: Σ_i Σ_j c_ij * x_ij

  Subject to:
    Σ_j x_ij ≤ s_i         ∀i  (supply limit)
    Σ_i x_ij ≥ d_j         ∀j  (demand requirement)
    x_ij ≥ 0               ∀i,j  (non-negativity)

Solution Method: Linear Programming (simplex or network flow algorithms)
```

**Integrated Example (Job Shop Scheduling):**

Combines CP (for scheduling logic) and MIP (for optimization):

```
CP Model:
  Variables: start_ij, machine_ij for each operation
  Constraints: precedence, no-overlap (cumulative)

MIP Model:
  Binary variables: y_ijk = 1 if operation i precedes j on machine k
  Objective: minimize makespan
  Constraints: linking CP and MIP variables

Hybrid Solving:
  1. CP propagation to reduce domains
  2. MIP solver for optimization with reduced variables
  3. Iterate: CP feasibility check → MIP optimization
```

### Decomposition Strategy

**Decomposition in OR/CP:**

1. **Spatial Decomposition (Dantzig-Wolfe):**
   - Partition variables into blocks
   - Solve subproblems independently
   - Coordinate via master problem

2. **Temporal Decomposition:**
   - Multi-stage problems: decompose by time periods
   - Dynamic programming: solve stages sequentially
   - Rolling horizon: optimize near term, replan later

3. **Constraint Decomposition:**
   - Partition constraints into independent sets
   - Solve each set separately
   - Combine solutions (may require coordination)

4. **Logic-Based Benders Decomposition:**
   - Master problem (MIP): High-level decisions
   - Subproblem (CP): Detailed scheduling/routing
   - Iterate: Master proposes, subproblem verifies, add cuts

**Example: Vehicle Routing with Time Windows**

```
Master Problem (MIP):
  Decide: Which customers assigned to which vehicle
  Variables: x_ijk = 1 if vehicle k goes from i to j
  Objective: Minimize total distance
  Constraints: Each customer visited once

Subproblem (CP):
  Decide: Order of visits for each vehicle
  Variables: position of customer in route, arrival times
  Constraints: time windows, precedence
  Check: Is assignment feasible given time windows?

Benders Iteration:
  1. Solve master: Get customer-to-vehicle assignment
  2. Solve subproblem: Check if routes feasible
  3. If infeasible: Generate cut eliminating this assignment
  4. Add cut to master, resolve
  5. Repeat until feasible solution found
```

**Hierarchical Decomposition:**

```
Level 1: Strategic (OR)
  - Long-term capacity planning
  - Facility location
  - Network design

Level 2: Tactical (OR/CP)
  - Production planning
  - Inventory management
  - Workforce scheduling

Level 3: Operational (CP)
  - Daily scheduling
  - Task assignment
  - Routing
```

### Plan Generation

**Branch-and-Bound (MIP):**

```
function BranchAndBound(problem)
  best_solution = ∞
  queue = [root_node]

  while queue not empty:
    node = queue.pop()

    // Bound: Solve LP relaxation
    relaxation = solve_LP(node)

    if relaxation.objective ≥ best_solution:
      continue  // Prune: Can't improve

    if relaxation.is_integer():
      best_solution = relaxation.objective
      continue  // Found integer solution

    // Branch: Pick fractional variable, create children
    var = select_branching_variable(relaxation)
    child1 = add_constraint(node, var ≤ floor(var.value))
    child2 = add_constraint(node, var ≥ ceil(var.value))
    queue.push(child1, child2)

  return best_solution
```

**Constraint Programming Search:**

```
function CP_Search(variables, domains, constraints)
  if all variables assigned:
    return current_assignment

  // Variable ordering heuristic
  var = select_variable(variables)  // e.g., smallest domain first

  // Constraint propagation
  propagate(constraints)

  // Value ordering heuristic
  for value in order_values(domains[var]):  // e.g., min-conflicts
    if consistent(var, value, constraints):
      assign(var, value)
      result = CP_Search(variables, domains, constraints)
      if result ≠ FAIL:
        return result
      unassign(var, value)

  return FAIL  // Backtrack
```

**Hybrid CP/MIP Planning:**

```
function HybridSolve(problem)
  // Phase 1: CP for feasibility
  cp_solution = CP_Search(problem.variables, problem.domains, problem.constraints)

  if cp_solution = FAIL:
    return INFEASIBLE

  // Phase 2: MIP for optimization
  mip = convert_to_MIP(problem, cp_solution)  // Use CP solution as warm start
  optimized = solve_MIP(mip)

  // Phase 3: CP to verify and refine
  final = CP_verify(optimized)

  return final
```

### Resource Identification

**What OR/CP Needs:**

1. **Problem Structure:**
   - **Decision variables:** What are we choosing?
   - **Objective function:** What are we optimizing?
   - **Constraints:** What restrictions must be satisfied?
   - **Parameters:** Given data (costs, capacities, demands)

2. **Domain Knowledge:**
   - **Valid models:** How to formulate the problem mathematically
   - **Special structure:** Network flow, assignment, knapsack
   - **Symmetries:** Equivalent solutions to break
   - **Redundant constraints:** Strengthen formulation

3. **Solver Selection:**
   - **LP:** CPLEX, Gurobi, GLPK
   - **MIP:** CPLEX, Gurobi, SCIP, CBC
   - **CP:** Gecode, OR-Tools, MiniZinc, Choco
   - **Hybrid:** OR-Tools (combines CP and MIP)

4. **Computational Resources:**
   - **Memory:** Store variables, constraints, search tree
   - **Time:** Optimization can be slow (exponential worst-case)
   - **Parallelization:** Branch-and-bound naturally parallelizable

5. **Solution Quality:**
   - **Optimality:** Proven optimal vs. best found
   - **Time limits:** Anytime algorithms provide improving bounds
   - **Feasibility:** Sometimes just finding feasible solution is hard

### Applications to Task Planning

**Task Scheduling as CSP/COP:**

```
Variables:
  start_time[task]   // when task starts
  end_time[task]     // when task ends
  assigned_agent[task]  // which agent executes

Domains:
  start_time ∈ [0, deadline]
  assigned_agent ∈ available_agents

Constraints:
  // Precedence
  ∀(t1, t2) ∈ dependencies: end_time[t1] ≤ start_time[t2]

  // Duration
  ∀task: end_time[task] = start_time[task] + duration[task]

  // Resource capacity
  cumulative(start_time, duration, resource_usage, capacity)

  // Agent capabilities
  ∀task: capable(assigned_agent[task], task)

Objective:
  Minimize: makespan = max(end_time[task])
  OR
  Minimize: total_cost = Σ cost(assigned_agent[task], task)
```

**Example Implementation:**

```javascript
// Task planning with OR-Tools (CP-SAT)
const { CpModel, CpSolver } = require('google-or-tools');

function planTasks(tasks, agents, dependencies) {
  const model = new CpModel();

  // Variables
  const startVars = {};
  const endVars = {};
  const agentVars = {};

  for (const task of tasks) {
    startVars[task.id] = model.newIntVar(0, 1000, `start_${task.id}`);
    endVars[task.id] = model.newIntVar(0, 1000, `end_${task.id}`);
    agentVars[task.id] = model.newIntVar(0, agents.length - 1, `agent_${task.id}`);

    // Duration constraint
    model.addEquality(
      endVars[task.id],
      model.newLinearExpr().add(startVars[task.id]).add(task.duration)
    );
  }

  // Precedence constraints
  for (const [t1, t2] of dependencies) {
    model.addLessOrEqual(endVars[t1], startVars[t2]);
  }

  // Resource constraints (no agent overlap)
  for (const agent of agents) {
    const intervals = tasks
      .filter(t => capable(agent, t))
      .map(t => model.newIntervalVar(
        startVars[t.id],
        task.duration,
        endVars[t.id],
        `interval_${t.id}`
      ));
    model.addNoOverlap(intervals);
  }

  // Objective: minimize makespan
  const makespan = model.newIntVar(0, 1000, 'makespan');
  for (const task of tasks) {
    model.addLessOrEqual(endVars[task.id], makespan);
  }
  model.minimize(makespan);

  // Solve
  const solver = new CpSolver();
  const status = solver.solve(model);

  if (status === 'OPTIMAL' || status === 'FEASIBLE') {
    return tasks.map(task => ({
      task: task.id,
      start: solver.value(startVars[task.id]),
      end: solver.value(endVars[task.id]),
      agent: agents[solver.value(agentVars[task.id])]
    }));
  } else {
    throw new Error('No feasible schedule found');
  }
}
```

**Integration with Task/Graph System:**

- **Nodes:** Tasks with duration, resource requirements
- **Edges:** Dependencies (precedence constraints)
- **Attributes:** Start/end times, agent assignments (solver output)
- **Optimization:** Minimize makespan, cost, or other objectives
- **Verification:** CP ensures all constraints satisfied

### Key References

**Foundational Texts:**
- Hillier, F.S. & Lieberman, G.J. (2020). *Introduction to Operations Research*. McGraw-Hill. [Classic OR textbook]
- Apt, K. (2003). *Principles of Constraint Programming*. Cambridge University Press.
- Rossi, F., Van Beek, P., Walsh, T. (2006). *Handbook of Constraint Programming*. Elsevier.

**Integration:**
- Hooker, J.N. (2000). *Logic-Based Methods for Optimization*. Wiley. [CP + OR integration]
- Milano, M. & Van Hentenryck, P. (2017). "Constraint Programming and Operations Research." *Constraints* 22(1): 1-3.

**Recent Conference (2026):**
- CPAIOR 2026: 23rd International Conference on Integration of Constraint Programming, Artificial Intelligence, and Operations Research. Rabat, Morocco, May 26-29, 2026.

**Tools:**
- [Google OR-Tools](https://developers.google.com/optimization) - Open-source CP/MIP solver
- [MiniZinc](https://www.minizinc.org/) - High-level constraint modeling language
- [Gurobi](https://www.gurobi.com/) - Commercial MIP solver
- [CPLEX](https://www.ibm.com/analytics/cplex-optimizer) - IBM's optimization suite

**Applications:**
- Baptiste, P. & Le Pape, C. (2000). "Combining Operations Research and Constraint Programming to Solve Real-Life Scheduling Problems." *ERCIM News* 44.

### Example: Project Scheduling with Resources

**Problem:**
Schedule 8 tasks with dependencies and resource constraints.

```
Tasks:
  T1: duration=2, workers=2
  T2: duration=3, workers=1
  T3: duration=4, workers=3
  T4: duration=1, workers=1
  T5: duration=3, workers=2
  T6: duration=2, workers=2
  T7: duration=3, workers=1
  T8: duration=2, workers=3

Dependencies:
  T1 → T3, T1 → T4
  T2 → T5, T2 → T6
  T3 → T7
  T4 → T8, T5 → T8
  T6 → T8

Resources:
  Available workers: 4 (at any time)
```

**CP Formulation:**

```
Variables:
  start[T1..T8]

Constraints:
  start[T3] ≥ start[T1] + 2
  start[T4] ≥ start[T1] + 2
  start[T5] ≥ start[T2] + 3
  start[T6] ≥ start[T2] + 3
  start[T7] ≥ start[T3] + 4
  start[T8] ≥ start[T4] + 1
  start[T8] ≥ start[T5] + 3
  start[T8] ≥ start[T6] + 2

  cumulative([start], [2,3,4,1,3,2,3,2], [2,1,3,1,2,2,1,3], 4)
  // At any time, total workers ≤ 4

Objective:
  minimize: start[T8] + 2  (makespan)
```

**Solution (found by CP-SAT solver):**

```
Gantt Chart:
Time:  0  1  2  3  4  5  6  7  8  9  10
T1:   [==]
T2:   [======]
T3:      [========]
T4:      [=]
T5:         [======]
T6:            [==]
T7:               [======]
T8:                     [==]

Workers:
Time:  0  1  2  3  4  5  6  7  8  9  10
       |2 |2 |4 |4 |3 |3 |3 |3 |1 |3 |  (never exceeds 4)

Makespan: 10
```

**Verification:**
- All precedence constraints satisfied
- Resource constraint satisfied (peak usage = 4)
- Optimal makespan achieved

---

## Area 5: Information Foraging Theory

### Overview

Information Foraging Theory (IFT) applies optimal foraging theory from biology to understand how humans and agents seek information. It provides a framework for identifying what information is needed, where to find it, and when to stop searching.

**Biological Roots:**
- Animals optimize foraging strategy (maximize energy/time)
- Trade-off: time spent searching vs. time spent exploiting
- Patch model: When to leave current food patch for new one

**Application to Information:**
- Information seekers optimize learning (maximize value/time)
- Trade-off: time spent searching vs. time spent reading
- Information patch: When to stop reading document and search for another

### Core Concepts

**Information Foraging Components:**

1. **Information Diet:**
   - What types of information to pursue
   - Similar to predator's prey selection
   - Based on value/cost ratio

2. **Information Patches:**
   - Clusters of related information (documents, websites, knowledge bases)
   - Rich patches: High information density
   - Poor patches: Low information density

3. **Information Scent:**
   - Cues indicating information location and value
   - Proximal cues: Immediately visible (titles, snippets, metadata)
   - Distal cues: About distant information (search results, links)

4. **Foraging Strategies:**
   - **Enrichment:** Modify environment to improve information access
   - **Navigation:** Move between patches following scent
   - **Exploitation:** Extract information from current patch

**Marginal Value Theorem:**

Central principle: Leave current patch when expected gain rate drops below average gain rate across all patches.

```
Mathematical Form:
  Leave patch when: dR/dt|patch < R̄/T̄

  Where:
    R = information gained in patch
    t = time spent in patch
    R̄ = average information gain
    T̄ = average time per patch
```

**Information Scent:**

```
Scent Strength = Σ (similarity(cue, goal) × prominence(cue))

Higher scent → Higher probability of containing relevant information
```

**Rational Analysis:**

Information foraging is **adaptive** to the structure of the information environment:
- Sparse information → Broad exploration
- Dense information → Deep exploitation
- Clear scent → Follow direct path
- Weak scent → Exploratory search

### Problem Formalization

**Information Need Specification:**

```
Information Need:
  Goal: What question to answer / problem to solve
  Context: Current knowledge state
  Constraints: Time, cost, access limitations
  Success criteria: How much information is enough

Example:
  Goal: "How to optimize PostgreSQL query performance?"
  Context: Know SQL basics, not familiar with query planner
  Constraints: Need answer within 2 hours
  Success: Understand key techniques, have actionable steps
```

**Information Environment:**

```
Environment:
  Patches: [P1, P2, ..., Pn]

  For each patch P:
    - Content: Actual information contained
    - Scent: Cues indicating content (title, abstract, keywords)
    - Access cost: Time to reach patch
    - Richness: Information density
    - Coverage: Overlap with information goal

Foraging Problem:
  Find sequence of patches maximizing information gain
  Subject to time/cost constraints
```

**Patch Model:**

```
Patch P:
  Initial richness: R₀
  Depletion rate: λ (how fast information extracted)

  Information remaining after time t:
    R(t) = R₀ × e^(-λt)

  Gain rate:
    dR/dt = -λ × R₀ × e^(-λt)

  Optimal time in patch:
    t* = (1/λ) × ln(λ × R₀ × T̄ / R̄)
```

### Decomposition Strategy

**Information Foraging Decomposition:**

1. **Goal Decomposition:**
   - Break complex information need into sub-questions
   - Each sub-question becomes foraging goal
   - Divide-and-conquer information gathering

2. **Spatial Decomposition:**
   - Partition information environment by topic
   - Academic papers, documentation, forums, code examples
   - Forage each area independently

3. **Temporal Decomposition:**
   - Exploration phase: Broadly survey information landscape
   - Exploitation phase: Deep dive into promising patches
   - Refinement phase: Fill specific knowledge gaps

4. **Social Decomposition:**
   - Leverage expertise of different sources
   - Delegate sub-questions to appropriate experts
   - Aggregate information from multiple agents

**Example: Research Problem Decomposition**

```
Main Question: "How to build a task planning system?"

Decomposed Sub-Questions:
1. What are existing task planning approaches?
   Patches: Academic papers, survey articles, textbooks

2. What are practical implementations?
   Patches: GitHub repos, technical blogs, documentation

3. What are common pitfalls?
   Patches: Forums, issue trackers, retrospectives

4. What tools/libraries exist?
   Patches: Package repositories, awesome-lists, comparisons

Foraging Strategy:
  Phase 1: Survey papers for theoretical foundations (1-2 hours)
  Phase 2: Examine implementations for practical insights (2-3 hours)
  Phase 3: Read forums for pitfalls (30 min)
  Phase 4: Evaluate tools (1 hour)
```

### Plan Generation

**Foraging Plan:**

```
function InformationForagingPlan(goal, environment, constraints)
  patches = identify_patches(environment, goal)
  scent_ratings = assess_scent(patches, goal)

  plan = []
  information_gained = 0
  time_spent = 0

  while information_gained < goal.threshold and time_spent < constraints.time:
    // Select next patch (highest scent)
    patch = max(patches, key=scent_rating)

    // Navigate to patch
    time_spent += travel_cost(patch)
    plan.append({action: 'navigate', target: patch})

    // Exploit patch until marginal value too low
    while gain_rate(patch, time_in_patch) > average_gain_rate():
      information = extract(patch, time_delta)
      information_gained += information
      time_in_patch += time_delta

      // Update scent of other patches based on new knowledge
      update_scent(patches, information)

    plan.append({action: 'exploit', patch: patch, duration: time_in_patch})

    // Leave patch, update environment model
    patches.remove(patch)

  return plan
```

**Exploration vs. Exploitation:**

```
Multi-Armed Bandit Model:

Patches = Arms
Reward = Information gained
Uncertainty = Unknown patch richness

Strategy: ε-greedy
  With probability ε: Explore (random patch)
  With probability 1-ε: Exploit (best known patch)

Or: Upper Confidence Bound (UCB)
  Select patch maximizing: R̄_i + c√(ln(n)/n_i)
  Where:
    R̄_i = average reward from patch i
    n = total visits to all patches
    n_i = visits to patch i
    c = exploration constant
```

**Adaptive Foraging:**

```
function AdaptiveForaging(goal, environment)
  knowledge = initial_knowledge()
  refined_goal = goal

  while not satisfied(refined_goal, knowledge):
    // Assess current knowledge gaps
    gaps = identify_gaps(refined_goal, knowledge)

    // Refine goal based on discoveries
    refined_goal = update_goal(refined_goal, knowledge)

    // Identify patches addressing gaps
    candidates = find_patches(gaps, environment)

    // Forage most promising patch
    patch = select_patch(candidates)
    new_info = exploit_patch(patch)

    // Integrate new information
    knowledge = integrate(knowledge, new_info)

    // Update environment model
    update_environment_model(environment, new_info)

  return knowledge
```

### Resource Identification

**Information Sources (Patches):**

1. **Structured Knowledge:**
   - Databases, knowledge graphs
   - APIs, web services
   - Documentation, reference manuals

2. **Unstructured Content:**
   - Research papers, articles
   - Web pages, blogs
   - Forums, discussions

3. **Interactive Sources:**
   - Human experts
   - AI assistants
   - Search engines

4. **Code and Artifacts:**
   - GitHub repositories
   - Package registries
   - Example implementations

**Scent Indicators:**

1. **Metadata:**
   - Title, abstract, keywords
   - Author, publication date
   - Citation count, relevance score

2. **Structural Cues:**
   - Section headings
   - Table of contents
   - Index terms

3. **Content Samples:**
   - Snippets from search results
   - First paragraphs
   - Summary statistics

4. **Social Signals:**
   - Upvotes, stars, likes
   - Expert recommendations
   - Community discussions

**Search Strategies:**

1. **Breadth-First:**
   - Survey many patches lightly
   - Good for exploration, getting overview
   - Risk: Missing depth

2. **Depth-First:**
   - Deeply exploit few patches
   - Good when scent is strong
   - Risk: Missing breadth

3. **Best-First:**
   - Follow strongest scent
   - Good for targeted search
   - Risk: Local optima (getting stuck)

4. **Iterative Deepening:**
   - Alternate breadth and depth
   - Balanced exploration/exploitation
   - Adapts to information landscape

### Applications to Task Planning

**Identifying Information Needs for Tasks:**

```javascript
// Information foraging for task planning
class TaskInformationForager {
  constructor(task, knowledgeBase, environment) {
    this.task = task;
    this.kb = knowledgeBase;
    this.env = environment;
  }

  // Identify what information is needed
  identifyInformationNeeds() {
    const needs = [];

    // What are task requirements?
    if (!this.kb.has(`requirements:${this.task.id}`)) {
      needs.push({
        type: 'requirements',
        query: `What are the requirements for ${this.task.description}?`,
        priority: 'high'
      });
    }

    // What are available tools/resources?
    if (!this.kb.has(`tools:${this.task.domain}`)) {
      needs.push({
        type: 'tools',
        query: `What tools are available for ${this.task.domain}?`,
        priority: 'medium'
      });
    }

    // What are dependencies?
    if (!this.kb.has(`dependencies:${this.task.id}`)) {
      needs.push({
        type: 'dependencies',
        query: `What other tasks must complete before ${this.task.id}?`,
        priority: 'high'
      });
    }

    // What are best practices?
    needs.push({
      type: 'best-practices',
      query: `What are best practices for ${this.task.type}?`,
      priority: 'low'
    });

    return needs.sort((a, b) => priority(b) - priority(a));
  }

  // Find information sources (patches)
  findPatches(informationNeed) {
    const patches = [];

    // Search knowledge base
    const kbResults = this.kb.search(informationNeed.query);
    if (kbResults.relevance > 0.7) {
      patches.push({
        source: 'knowledge-base',
        scent: kbResults.relevance,
        cost: 1,  // low cost, already local
        results: kbResults
      });
    }

    // Search documentation
    const docResults = this.env.searchDocs(informationNeed.query);
    patches.push({
      source: 'documentation',
      scent: docResults.relevance,
      cost: 3,
      results: docResults
    });

    // Search web
    const webResults = this.env.searchWeb(informationNeed.query);
    patches.push({
      source: 'web',
      scent: webResults.relevance,
      cost: 5,  // higher cost, more time
      results: webResults
    });

    // Ask experts
    if (informationNeed.priority === 'high') {
      patches.push({
        source: 'expert',
        scent: 0.9,  // high expected value
        cost: 20,    // expensive
        results: null
      });
    }

    return patches.sort((a, b) => (b.scent / b.cost) - (a.scent / a.cost));
  }

  // Forage for information
  async forage(informationNeed, timeLimit) {
    const patches = this.findPatches(informationNeed);
    let information = {};
    let timeSpent = 0;

    for (const patch of patches) {
      if (timeSpent >= timeLimit) break;

      // Navigate to patch
      timeSpent += patch.cost;

      // Exploit patch
      const startTime = Date.now();
      const patchInfo = await this.exploitPatch(patch, timeLimit - timeSpent);
      const duration = (Date.now() - startTime) / 1000;

      // Integrate information
      information = this.integrate(information, patchInfo);
      timeSpent += duration;

      // Check if need satisfied
      if (this.satisfied(informationNeed, information)) {
        break;
      }
    }

    return information;
  }

  // Exploit a single patch
  async exploitPatch(patch, timeLimit) {
    const information = {};
    let timeInPatch = 0;
    const avgGainRate = this.estimateAverageGainRate();

    while (timeInPatch < timeLimit) {
      // Extract information chunk
      const chunk = await patch.source.extract(patch.results, {
        time: 10,  // spend 10 seconds reading
        depth: this.currentDepth
      });

      timeInPatch += 10;
      information[chunk.key] = chunk.value;

      // Estimate gain rate in this patch
      const gainRate = this.estimateGainRate(information, timeInPatch);

      // Leave if marginal value too low (Marginal Value Theorem)
      if (gainRate < avgGainRate) {
        break;
      }
    }

    return information;
  }

  // Store information in knowledge base
  integrate(current, newInfo) {
    for (const [key, value] of Object.entries(newInfo)) {
      this.kb.store(key, value, {
        source: value.source,
        timestamp: Date.now(),
        confidence: value.confidence
      });
    }
    return { ...current, ...newInfo };
  }
}
```

**Integration with Task System:**

```javascript
// Before executing task, forage for information
async function planTaskExecution(task) {
  const forager = new TaskInformationForager(task, knowledgeBase, environment);

  // Identify what we need to know
  const informationNeeds = forager.identifyInformationNeeds();

  // Forage for each information need
  for (const need of informationNeeds) {
    const timeLimit = need.priority === 'high' ? 300 : 60;  // seconds
    const information = await forager.forage(need, timeLimit);

    // Update task metadata with findings
    task.metadata[need.type] = information;
  }

  // Now task has sufficient information to plan execution
  const executionPlan = generateExecutionPlan(task);
  return executionPlan;
}
```

### Key References

**Foundational Work:**
- Pirolli, P. & Card, S. (1999). "Information Foraging." *Psychological Review* 106(4): 643-675.
- Pirolli, P. (2007). *Information Foraging Theory: Adaptive Interaction with Information*. Oxford University Press.

**Biological Foundations:**
- Charnov, E.L. (1976). "Optimal Foraging: The Marginal Value Theorem." *Theoretical Population Biology* 9: 129-136.
- Stephens, D.W. & Krebs, J.R. (1986). *Foraging Theory*. Princeton University Press.

**Web Navigation:**
- Chi, E.H. et al. (2001). "Using Information Scent to Model User Information Needs and Actions on the Web." *CHI*: 490-497.
- Nielsen Norman Group: [Information Foraging](https://www.nngroup.com/articles/information-foraging/)

**Intelligence Analysis:**
- Pirolli, P. & Card, S. (2005). "The Sensemaking Process and Leverage Points for Analyst Technology." *Intelligence Analysis*.

**Recent Applications (2025-2026):**
- Liu, Y. et al. (2025). "Information Foraging Theory and LLMs." *CHI 2025*. [How LLMs affect information foraging behavior]

### Example: Research Literature Search

**Scenario:** Need to understand "graph-based task planning" for implementing new system.

**Information Need:**
```
Goal: Understand graph-based task planning approaches
Context: Familiar with graphs, not with task planning
Time constraint: 3 hours
Success: Can describe 2-3 approaches, identify relevant papers
```

**Foraging Plan:**

**Phase 1: Survey (45 min)**

Patches:
- Google Scholar: "graph-based task planning"
- arXiv: cs.AI recent papers
- Wikipedia: Task planning, STRIPS, HTN

Actions:
- Search Google Scholar
- Skim top 10 papers (titles, abstracts)
- Read Wikipedia articles (overview)

Scent:
- Papers with "graph" and "task" in title: High scent
- Papers with implementation details: Medium scent
- Papers with only theory: Lower scent

Information Gained:
- Key terms: HTN, STRIPS, PDDL, task graph
- Main researchers: Nau, Ghallab, Erol
- Relevant conferences: ICAPS, AIPS

**Phase 2: Deep Dive (90 min)**

Patches:
- Nau et al. (2003) "SHOP2: An HTN Planning System"
- Survey paper on task planning (if found)
- Recent ICAPS papers on graph-based approaches

Actions:
- Read SHOP2 paper (Section 3: HTN formalism)
- Read survey introduction and related work
- Skim recent papers for novel approaches

Exploitation:
- Spend 30 min on SHOP2 (good patch, rich content)
- Spend 20 min on survey (broad overview)
- Spend 10 min each on 3 recent papers (examples)

Information Gained:
- HTN decomposition methods
- Graph representation of task networks
- Comparison with other approaches

**Phase 3: Implementation Focus (45 min)**

Patches:
- GitHub: search "HTN planning", "task graph"
- Documentation: PDDL tools, planning libraries
- Stack Overflow: common issues

Actions:
- Browse 3-4 GitHub repos (README, examples)
- Read docs for one planning library (PANDA or SHOP)
- Check Stack Overflow for implementation tips

Information Gained:
- Available libraries: PANDA, pyperplan
- Common data structures: task network as DAG
- Practical considerations: grounding, search algorithms

**Total Time:** 3 hours
**Result:** Sufficient understanding to design initial system architecture

**Marginal Value Calculation:**

```
Survey phase: High gain rate (learning fundamentals quickly)
Deep dive: Medium gain rate (detailed understanding, slower)
Implementation: Low gain rate (specific details)

Decision: Stop after implementation phase
  - Additional reading unlikely to provide proportional value
  - Enough information to start prototyping
  - Can return for more details as needed
```

---

## Cross-Cutting Insights

### Common Patterns Across Areas

**1. Hierarchical Decomposition:**
- **AI Planning (HTN):** Decompose tasks from abstract to primitive
- **Program Synthesis:** Decompose specification into sub-specifications
- **OR/CP:** Decompose problem into master and subproblems
- **Information Foraging:** Decompose information needs into sub-questions

**Pattern:** Complex problems are solved by recursive decomposition into simpler sub-problems.

**2. Search + Constraints:**
- **AI Planning:** Search state/task space guided by preconditions/effects
- **Genetic Algorithms:** Search solution space guided by fitness function
- **Program Synthesis:** Search program space guided by specification
- **OR/CP:** Search assignment space guided by constraints

**Pattern:** Solution finding involves searching space constrained by requirements.

**3. Cost/Fitness Evaluation:**
- **GEQO:** Estimated query execution cost
- **OR/CP:** Objective function value
- **Program Synthesis:** Specification satisfaction
- **Information Foraging:** Information gain rate

**Pattern:** Solutions evaluated by quality metric guiding search.

**4. Exploration vs. Exploitation:**
- **Genetic Algorithms:** Mutation (explore) vs. selection (exploit)
- **Information Foraging:** Survey patches (explore) vs. read deeply (exploit)
- **OR/CP:** Branching (explore) vs. constraint propagation (exploit)

**Pattern:** Balance between discovering new possibilities and refining known good options.

**5. Iterative Refinement:**
- **CEGIS:** Synthesize candidate, verify, add counter-example, repeat
- **Benders Decomposition:** Master proposes, subproblem verifies, add cut, repeat
- **Information Foraging:** Assess need, forage, integrate, reassess, repeat

**Pattern:** Solutions improved through cycles of generation and feedback.

### Integration Opportunities

**Hybrid Planning Systems:**

Combine multiple approaches for robust task planning:

```
Level 1: Problem Understanding (Information Foraging)
  - Identify information needs
  - Gather domain knowledge
  - Build problem model

Level 2: Problem Formalization (Requirements Engineering + Synthesis)
  - Specify goals formally
  - Define constraints
  - Create task grammar

Level 3: Hierarchical Decomposition (HTN Planning)
  - Decompose high-level goals
  - Generate task network
  - Identify dependencies

Level 4: Resource Allocation (OR/CP)
  - Assign tasks to agents
  - Schedule execution
  - Optimize resources

Level 5: Plan Optimization (Genetic Algorithms)
  - Refine schedules
  - Handle large search spaces
  - Find near-optimal solutions

Level 6: Plan Verification (Synthesis Verification)
  - Prove correctness
  - Check constraints
  - Validate feasibility
```

**Example Integration:**

```javascript
// Integrated task planning system
class IntegratedTaskPlanner {
  async plan(highLevelGoal) {
    // Phase 1: Information Foraging
    const informationNeeds = this.identifyInformationNeeds(highLevelGoal);
    const domainKnowledge = await this.forage(informationNeeds);

    // Phase 2: Formal Specification
    const formalSpec = this.synthesizeSpecification(
      highLevelGoal,
      domainKnowledge
    );

    // Phase 3: HTN Decomposition
    const taskNetwork = this.decompose(formalSpec, domainKnowledge.methods);

    // Phase 4: Constraint-Based Scheduling
    const schedule = this.scheduleWithConstraints(
      taskNetwork,
      domainKnowledge.resources,
      domainKnowledge.constraints
    );

    // Phase 5: Evolutionary Optimization
    if (schedule.quality < threshold) {
      schedule = this.optimizeWithGA(schedule, taskNetwork);
    }

    // Phase 6: Verification
    const verified = this.verify(schedule, formalSpec);
    if (!verified.success) {
      throw new Error(`Plan verification failed: ${verified.errors}`);
    }

    return schedule;
  }
}
```

**When to Use Which Approach:**

| Scenario | Best Approach | Why |
|----------|---------------|-----|
| Clear hierarchical structure | HTN Planning | Leverages domain decomposition |
| Large search space, no structure | Genetic Algorithms | Handles size, finds good solutions |
| Formal correctness required | Program Synthesis + Verification | Proves properties |
| Complex constraints | OR/CP | Efficient constraint handling |
| Uncertain information needs | Information Foraging | Adaptive information gathering |
| Multi-objective optimization | OR/CP + GA | CP for constraints, GA for optimization |
| Unknown problem structure | Information Foraging + HTN | Learn structure, then decompose |

---

## Recommendations for Our System

### Prioritized Approaches

**1. HTN Planning (Hierarchical Task Networks) - HIGH PRIORITY**

**Why:**
- Natural fit for task decomposition
- Encodes domain knowledge as methods
- Reduces search space dramatically
- Matches human thinking about tasks

**Implementation:**
```javascript
// Extend TaskNode with HTN concepts
class TaskNode {
  isCompound() {
    return this.methods && this.methods.length > 0;
  }

  getDecompositionMethods(context) {
    return this.methods.filter(m => m.applicable(context));
  }

  decompose(method, context) {
    return method.expand(this, context);
  }
}

// HTN planner
function planHTN(task, context, methods) {
  if (task.isPrimitive()) {
    return [task];
  }

  const applicableMethods = task.getDecompositionMethods(context);
  for (const method of applicableMethods) {
    const subtasks = task.decompose(method, context);
    const subplans = subtasks.map(st => planHTN(st, context, methods));
    if (subplans.every(p => p !== null)) {
      return subplans.flat();
    }
  }

  return null; // No valid decomposition
}
```

**Integration:**
- Store methods as TaskNode metadata
- Graph edges represent decomposition and dependencies
- Context stored as graph attributes

**2. Information Foraging Theory - HIGH PRIORITY**

**Why:**
- Addresses information need identification
- Guides search for domain knowledge
- Adapts to available information sources
- Critical for initial planning phase

**Implementation:**
```javascript
// Information forager for task planning
class TaskInformationForager {
  identifyNeeds(task) {
    // What's needed to plan/execute this task?
    return [
      { type: 'requirements', priority: 'high' },
      { type: 'dependencies', priority: 'high' },
      { type: 'resources', priority: 'medium' },
      { type: 'methods', priority: 'medium' },
      { type: 'examples', priority: 'low' }
    ];
  }

  async forage(need, timeLimit) {
    const patches = this.rankPatches(need);
    let info = {};
    let time = 0;

    for (const patch of patches) {
      const patchInfo = await this.exploit(patch, timeLimit - time);
      info = this.integrate(info, patchInfo);
      time += patchInfo.time;

      if (this.satisfied(need, info) || time >= timeLimit) {
        break;
      }
    }

    return info;
  }
}
```

**Integration:**
- Run before planning to gather domain knowledge
- Use to identify missing task requirements
- Guide search for applicable methods and tools

**3. Constraint Programming - MEDIUM PRIORITY**

**Why:**
- Handles complex scheduling constraints
- Verifies plan feasibility
- Optimizes resource allocation
- Integrates well with HTN decomposition

**Implementation:**
```javascript
// CP-based task scheduler
function scheduleWithConstraints(taskNetwork, resources, constraints) {
  const model = new CpModel();

  // Variables: start time, assigned agent
  const startVars = {};
  const agentVars = {};

  for (const task of taskNetwork.tasks) {
    startVars[task.id] = model.newIntVar(0, deadline, `start_${task.id}`);
    agentVars[task.id] = model.newIntVar(0, resources.agents.length - 1, `agent_${task.id}`);
  }

  // Constraints: dependencies, resources, capabilities
  for (const [t1, t2] of taskNetwork.dependencies) {
    model.addLessOrEqual(
      endTime(startVars[t1], t1.duration),
      startVars[t2]
    );
  }

  // Objective: minimize makespan
  const makespan = model.newIntVar(0, deadline, 'makespan');
  model.minimize(makespan);

  return solve(model);
}
```

**Integration:**
- Use after HTN decomposition to schedule tasks
- Verify resource constraints
- Optimize execution timeline

**4. Program Synthesis (CEGIS) - LOW PRIORITY**

**Why:**
- Useful for generating task implementations
- Provides correctness guarantees
- Complex to implement
- Overkill for many tasks

**When to Use:**
- Generating code from specifications
- Safety-critical tasks requiring verification
- Repetitive tasks with clear patterns

**5. Genetic Algorithms - LOW PRIORITY**

**Why:**
- Useful for large optimization problems
- No clear decomposition structure
- Non-deterministic (different runs, different results)
- Better alternatives available (HTN + CP)

**When to Use:**
- Optimizing task assignments with >100 tasks
- No clear heuristics available
- Approximate solutions acceptable

### Recommended Architecture

```
TaskPlanningSystem:

  1. Information Gathering (Foraging)
     Input: High-level goal
     Output: Domain knowledge, requirements, constraints
     Method: Information foraging with adaptive patch selection

  2. Goal Formalization (Requirements)
     Input: High-level goal, domain knowledge
     Output: Formal goal specification, success criteria
     Method: Requirements elicitation + goal decomposition

  3. Hierarchical Decomposition (HTN)
     Input: Formal goal, domain methods
     Output: Task network (graph of tasks and dependencies)
     Method: HTN planning with method selection

  4. Constraint-Based Scheduling (CP)
     Input: Task network, resources, constraints
     Output: Scheduled plan with agent assignments and timing
     Method: CP-SAT solver for optimization

  5. Verification & Refinement
     Input: Scheduled plan, formal specification
     Output: Verified plan or counter-examples
     Method: Constraint checking, simulation

  6. Execution Monitoring
     Input: Verified plan, actual execution
     Output: Progress tracking, replanning triggers
     Method: State monitoring, deviation detection
```

### Implementation Roadmap

**Phase 1: Core HTN Planning (2-3 weeks)**
- Implement HTN task decomposition
- Create method library for common domains
- Build task network representation
- Test on simple examples

**Phase 2: Information Foraging (1-2 weeks)**
- Implement information need identification
- Create patch ranking system
- Integrate with knowledge base
- Test on documentation/API search

**Phase 3: Constraint-Based Scheduling (2-3 weeks)**
- Integrate OR-Tools CP-SAT solver
- Implement resource constraints
- Add scheduling optimization
- Test on realistic scenarios

**Phase 4: Integration & Testing (1-2 weeks)**
- Connect all components
- End-to-end testing
- Performance optimization
- Documentation

**Total: 6-10 weeks for full implementation**

### Success Metrics

**Planning Quality:**
- Plan correctness: All constraints satisfied
- Plan optimality: Within 10% of optimal makespan
- Planning time: <1 second for <50 tasks, <10 seconds for <200 tasks

**Information Gathering:**
- Need identification: 90%+ of required information identified
- Search efficiency: <5 min for common queries
- Knowledge reuse: 70%+ of queries answered from KB

**System Usability:**
- API simplicity: Express tasks in <10 LOC
- Method reusability: 80%+ of methods reusable across domains
- Error messages: Clear indication of why planning failed

### Open Questions

1. **Method Library Design:**
   - How to organize decomposition methods?
   - How to handle method conflicts/overlaps?
   - How to learn new methods from experience?

2. **Information Source Selection:**
   - Which information sources to prioritize?
   - How to balance speed vs. quality?
   - When to query expensive sources (experts, LLMs)?

3. **Adaptation and Learning:**
   - How to improve plans from execution feedback?
   - How to learn from failures?
   - How to transfer knowledge across domains?

4. **Scalability:**
   - How to handle plans with >1000 tasks?
   - How to parallelize planning?
   - How to incrementally replan as tasks complete?

---

## Conclusion

This research covers five complementary areas addressing the full problem-solving pipeline:

1. **AI Planning (HTN, STRIPS, PDDL)** provides formal frameworks for hierarchical task decomposition and dependency management.

2. **Genetic Algorithms (GEQO)** handle large search spaces through evolutionary optimization, particularly useful when no clear decomposition exists.

3. **Program Synthesis** transforms high-level specifications into executable programs with correctness guarantees through iterative refinement (CEGIS).

4. **Operations Research & Constraint Programming** offer mathematical rigor for optimization and constraint satisfaction, especially for scheduling and resource allocation.

5. **Information Foraging Theory** explains how to identify, locate, and gather the information needed to understand and solve problems.

**Key Takeaway:** Modern task planning systems should integrate multiple approaches—HTN for decomposition, information foraging for knowledge acquisition, and constraint programming for optimization—to handle real-world complexity effectively.

The recommendations prioritize HTN planning and information foraging as core capabilities, with constraint programming for advanced optimization. Program synthesis and genetic algorithms are lower priority but valuable for specific scenarios (formal verification and large-scale optimization respectively).

**Implementation should focus on:**
- Building a method library for HTN decomposition
- Creating an adaptive information foraging system
- Integrating constraint-based scheduling for optimization
- Maintaining formal specifications for verification

This combination provides both the flexibility to handle diverse tasks and the rigor to ensure correct, optimal execution.

---

## Sources

### AI Planning
- [Hierarchical Task Network - Wikipedia](https://en.wikipedia.org/wiki/Hierarchical_task_network)
- [HTN-Translation GitHub](https://github.com/ronwalf/HTN-Translation)
- [Introduction to AI Planning - arXiv:2412.11642](https://arxiv.org/html/2412.11642)
- [Hierarchical Task Network Planning - GeeksforGeeks](https://www.geeksforgeeks.org/artificial-intelligence/hierarchical-task-network-htn-planning-in-ai/)
- [HDDL Language - arXiv:1911.05499](https://ar5iv.labs.arxiv.org/html/1911.05499)
- [Game AI Pro - HTN Planners](https://www.gameaipro.com/GameAIPro/GameAIPro_Chapter12_Exploring_HTN_Planners_through_Example.pdf)

### Genetic Algorithms & GEQO
- [PostgreSQL GEQO Documentation](https://www.postgresql.org/docs/current/geqo.html)
- [PostgreSQL GEQO Implementation](https://www.postgresql.org/docs/current/geqo-pg-intro.html)

### Program Synthesis
- [Program Synthesis - Wikipedia](https://en.wikipedia.org/wiki/Program_synthesis)
- [Program Synthesis Challenges - PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC5597726/)
- [MIT Lecture: Inductive Synthesis](https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture2.htm)
- [Program Synthesis Explained - James Bornholt](https://www.cs.utexas.edu/~bornholt/post/synthesis-explained.html)
- [FlashMeta Framework - Microsoft](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/oopsla15-pbe.pdf)
- [Formal Synthesis Theory - UC Berkeley](https://people.eecs.berkeley.edu/~sseshia/pubdir/togis17.pdf)

### Operations Research & Constraint Programming
- [CPAIOR 2026 Conference](https://cpaior.org/)
- [Constraint Programming and OR - Springer](https://link.springer.com/article/10.1007/s10601-017-9280-3)
- [OR Methods in CP - CMU](https://johnhooker.tepper.cmu.edu/chapter15.pdf)
- [ERCIM: Combining OR and CP](https://www.ercim.eu/publication/Ercim_News/enw44/baptiste.html)

### Information Foraging Theory
- [Information Foraging - Interaction Design](https://www.interaction-design.org/literature/book/the-glossary-of-human-computer-interaction/information-foraging-theory)
- [Nielsen Norman Group: Information Foraging](https://www.nngroup.com/articles/information-foraging/)
- [Information Foraging in Access Environments - ACM](https://dl.acm.org/doi/fullHtml/10.1145/223904.223911)
- [Pirolli & Card: Information Foraging](https://www.peterpirolli.com/ewExternalFiles/31354_C01_UNCORRECTED_PROOF.pdf)
- [Information Foraging - Wikipedia](https://en.wikipedia.org/wiki/Information_foraging)
- [Oxford Academic: Information Foraging Theory](https://academic.oup.com/book/7603)

### Machine Learning for Cost Estimation
- [MDPI: AI in Cost Estimation](https://www.mdpi.com/2673-3951/6/2/35)
- [Nature: Software Cost Estimation with CNN](https://www.nature.com/articles/s41598-024-63025-8)
- [arXiv: Cost Estimation in Database Systems](https://arxiv.org/abs/2510.05612)
- [ScienceDirect: Learned Cost Model for Big Data](https://www.sciencedirect.com/science/article/abs/pii/S0020025524005632)

### Requirements Engineering
- [Requirements Elicitation - Wikipedia](https://en.wikipedia.org/wiki/Requirements_elicitation)
- [GeeksforGeeks: Requirements Elicitation](https://www.geeksforgeeks.org/software-engineering-requirements-elicitation/)
- [Apriorit: Requirement Elicitation Guide](https://www.apriorit.com/white-papers/699-requirement-elicitation)
- [Medium: Requirements Engineering](https://medium.com/omarelgabrys-blog/requirements-engineering-elicitation-analysis-part-2-a02db801f135)

### Monte Carlo Tree Search
- [Monte Carlo Tree Search - Wikipedia](https://en.wikipedia.org/wiki/Monte_Carlo_tree_search)
- [MCTS in Reinforcement Learning](https://gibberblot.github.io/rl-notes/single-agent/mcts.html)
- [UW Lecture: MCTS](https://courses.cs.washington.edu/courses/cse599i/18wi/resources/lecture19/lecture19.pdf)
- [GeeksforGeeks: MCTS](https://www.geeksforgeeks.org/machine-learning/monte-carlo-tree-search-mcts-in-machine-learning/)
- [Medium: MCTS in AlphaGo Zero](https://jonathan-hui.medium.com/monte-carlo-tree-search-mcts-in-alphago-zero-8a403588276a)

### Work Breakdown Structure
- [Institute PM: WBS Guide](https://instituteprojectmanagement.com/blog/work-breakdown-structure/)
- [WBS - Wikipedia](https://en.wikipedia.org/wiki/Work_breakdown_structure)
- [ProjectManager: WBS Guide](https://www.projectmanager.com/guides/work-breakdown-structure)
- [Atlassian: Work Breakdown Structure](https://www.atlassian.com/work-management/project-management/work-breakdown-structure)
- [ProofHub: WBS Detailed Guide](https://www.proofhub.com/articles/work-breakdown-structure)

### SAT/SMT Solvers
- [Satisfiability Modulo Theories - Wikipedia](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories)
- [Berkeley: SMT Chapter](https://people.eecs.berkeley.edu/~sseshia/pubdir/SMT-BookChapter.pdf)
- [arXiv: SAT and SMT on Sudoku](https://arxiv.org/html/2501.08569v1)
- [SAT Solver - Wikipedia](https://en.wikipedia.org/wiki/SAT_solver)

### API Orchestration
- [Gartner: Service Orchestration Platforms](https://www.gartner.com/reviews/market/service-orchestration-and-automation-platforms)
- [Top API Orchestration Tools](https://www.digitalapi.ai/blogs/top-api-orchestration-tools)
- [Camunda: API Orchestration](https://camunda.com/solutions/api-orchestration/)
- [Zapier: API Orchestration](https://zapier.com/blog/api-orchestration/)
- [IBM: API Orchestration](https://www.ibm.com/think/topics/api-orchestration)

### Dynamic Programming
- [Divide and Conquer DP](https://cp-algorithms.com/dynamic_programming/divide-and-conquer-dp.html)
- [Divide-and-Conquer - Wikipedia](https://en.wikipedia.org/wiki/Divide-and-conquer_algorithm)
- [GeeksforGeeks: Algorithm Comparison](https://www.geeksforgeeks.org/dsa/comparison-among-greedy-divide-and-conquer-and-dynamic-programming-algorithm/)
- [Baeldung: Divide and Conquer vs DP](https://www.baeldung.com/cs/divide-and-conquer-vs-dynamic-programming)

### Goal Decomposition
- [IJCAI: Hierarchical Planning](https://www.ijcai.org/Proceedings/16/Papers/429.pdf)
- [arXiv: Task Planning with LLMs](https://arxiv.org/html/2409.19250v1)
- [UMD: Hierarchical Goal Networks](https://www.cs.umd.edu/~nau/papers/roberts2024hierarchical.pdf)
- [Noeon: Goal Decomposition](https://noeon.ai/blog/goal-decomposition/)
- [PMC: Humans Decompose Tasks](https://pmc.ncbi.nlm.nih.gov/articles/PMC10234566/)
- [COGNOSCERE: Task Decomposition with LLMs](https://cognoscerellc.com/task-decomposition-and-planning-with-llms-for-complex-goals/)
