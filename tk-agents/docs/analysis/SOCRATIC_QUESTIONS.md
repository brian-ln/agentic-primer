# Socratic Questions: tk-agents Task/Knowledge Graph Actor Protocol

## I. Architectural Assumptions

### On Message Passing as the Universal Primitive

**Q1: You claim "Everything is a node. All interactions are messages." But your `Graph.send()` is synchronous and blocking. How is this "message passing" any different from direct method calls with extra steps?**

The Registry's `await actor.send(message)` blocks the caller until the actor responds. Erlang's `Pid ! Message` returns immediately. If your system truly used message passing, why would a slow ClaudeActor (taking 30 seconds to respond) block all other callers? Isn't this just an expensive function call API?

**Q2: The SEND primitive routes to `handleMessage()`, which uses a giant switch statement. Why is this better than direct method calls?**

You've created `task.handleMessage({ type: "start", payload: {} })` instead of `task.start()`. What architectural property are you gaining that justifies the loss of type safety, IDE autocomplete, and debuggability? If the answer is "flexibility," what are you planning to be flexible about that couldn't be handled with interfaces?

**Q3: Your design doc mentions "Erlang patterns" and "actor model," but you have no mailboxes, no supervision trees, no process isolation, and no asynchronous message delivery. Are you building an actor system, or are you building an object system with a message-shaped API?**

### On the Graph as Central Authority

**Q4: The Graph stores all nodes and edges in memory. What happens when you have 10,000 tasks and 50,000 edges? How do you persist this state across restarts?**

Your `Graph.nodes` Map grows unbounded. When does it get persisted? How do you handle concurrent access? How do you recover after a crash? Or is this intended to be ephemeral?

**Q5: If the Graph is the "central store," doesn't that make it a bottleneck and single point of failure?**

All messages route through `Graph.send()`. All queries traverse `Graph.edges`. This is the opposite of distributed actor systems where actors communicate peer-to-peer. Why call this an "actor protocol" when it's actually a centralized registry pattern?

**Q6: Your DESIGN.md shows a "Registry" singleton (`export const registry = new Registry()`). How do you plan to test components that depend on global mutable state?**

Can you create multiple independent graphs for parallel test suites? Can you mock the registry? Or does every test touch shared global state?

### On Task/Knowledge Duality

**Q7: TaskNode and KnowledgeNode share almost no message handlers. Why are they in the same graph?**

TaskNode has: start, spawn, eval, complete, block, query_status.
KnowledgeNode has: append, query, synthesize.

They have orthogonal concerns. What does putting them in the same graph buy you that separate TaskGraph and KnowledgeGraph wouldn't?

**Q8: The "informationGaps" array in TaskProperties is just a list of strings. How does this actually link to KnowledgeNodes?**

You store `informationGaps: ["what is the API schema?"]` but then what? There's no mechanism to discover which KnowledgeNode answers that gap. Are you planning to implement semantic search? Or is this just aspirational metadata?

---

## II. State Machine Completeness

### On Task Lifecycle

**Q9: Your state machine allows `created -> active` but what validates that a task is actually *ready* to start?**

The `handleStart()` method checks if state is `created` or `ready`, but never validates that prerequisites are met. Can a task with missing information gaps transition to active? If yes, why have information gaps? If no, where's the validation?

**Q10: The "blocked" state has a `blockReason: string`. How does a human or agent know what to do with "Waiting for API schema"?**

Is the reason machine-readable? Can it trigger automatic actions (like spawning a knowledge-gathering task)? Or is it just a message for humans to read in logs?

**Q11: When a parent task completes, what happens to its incomplete child tasks?**

Your `handleComplete()` checks that children are completed before allowing parent completion. But what if a child is permanently blocked? Does the parent block forever? Is there a timeout? A failure cascade?

**Q12: You have states: created, ready, active, blocked, completed, failed. When does "failed" ever occur?**

There's no message handler that transitions to "failed". `handleComplete()` returns `success: false` but leaves state as-is. Are tasks immortal until they succeed?

### On Evaluation Criteria

**Q13: ObjectiveCriterion has an `actual` field that must be set "before eval." Who sets it, and how do they know when to set it?**

The `handleEval()` method reads `criterion.actual` but TaskNode never writes to it. Is this meant to be set by external agents? If so, how do they know the criterion exists and needs evaluation?

**Q14: Your criteria comparison is: `actual === threshold` for booleans, `actual >= threshold` for numbers. What about string criteria? What about range checks? What about fuzzy matches?**

If a criterion is "response time < 200ms but > 50ms", how do you express that? If it's "output contains 'success' but not 'error'", where does that logic go?

**Q15: Subjective criteria are stored but never evaluated. Why include them if they don't affect task completion?**

The type definition has `subjectiveSuccessCriteria?: string[]` but `handleEval()` ignores them. Are these just documentation? If they matter, who evaluates them?

---

## III. Knowledge Management Assumptions

### On Knowledge Querying

**Q16: The `handleQuery()` method uses keyword matching against lowercase strings. Is this a prototype placeholder or the actual design?**

The comment says "In a real implementation, this would use embeddings/LLM." But the rest of the system is production-quality. Is KnowledgeNode a stub? If yes, what's the real design?

**Q17: Query confidence is calculated as `matchCount / keywords.length`. A question like "What is the API?" with 100% keyword match could return garbage if the knowledge node is about "what is the weather API?" How is this useful?**

False positives seem guaranteed. If the answer is "this is temporary," when does the real implementation arrive, and what does it look like?

**Q18: The `synthesize` handler concatenates content from multiple nodes with headers like "[Source 1]". How does this differ from just reading multiple nodes sequentially?**

What does "synthesis" mean here? If it's just concatenation, why not let callers do that? If it's meant to be LLM-based summarization, why ship keyword matching?

### On Knowledge Lifecycle

**Q19: KnowledgeNode has `version: number` that increments on append. But there's no versioned history. If you append wrong data, how do you rollback?**

Version numbers imply you might want to reference "knowledge_5 at version 3." But you only store current version. Is this vestigial?

**Q20: Knowledge content grows via `append`, which concatenates with `\n\n` separator. What happens when you append 10,000 times?**

Do you ever prune or summarize? Does the string keep growing? At what point does keyword search become too slow?

---

## IV. Pragmatism vs. Theoretical Elegance

### On Actor Purity

**Q21: You cite Erlang and Smalltalk message passing, but then use synchronous promises. Why not fully commit to async message queues, or fully embrace synchronous method calls?**

Your design is stuck in the middle. Erlang actors don't return promises—they send messages to mailboxes and continue. Your actors block on `await`. Why reference Erlang if you're not using its concurrency model?

**Q22: The ERLANG_ARCHITECTURE_ANALYSIS.md proposes adding SupervisorActor, Mailbox, HealthMonitor, etc. If these are necessary, why weren't they in the initial design?**

It seems like you're retconning actor patterns onto an object-oriented design. Did you start with "graph of objects" and later discover you need supervision? Or did you design for supervision from the start but haven't implemented it?

**Q23: You have 87 lines of Erlang comparison proposing features you don't have. Does the system need these features, or are you over-engineering based on theoretical ideals?**

Erlang needs supervision trees because processes crash independently and need to restart. Your Node.js process crashes as a single unit. Do you actually need per-actor supervision, or is that cargo-culting?

### On Graph Protocol vs. REST API

**Q24: Your message protocol (get, observe, update, link, unlink, delete) maps 1:1 to REST verbs (GET, GET, PUT, POST, DELETE, DELETE). Why not just use HTTP?**

If external access is planned, why invent a message protocol that's isomorphic to REST? Why not define OpenAPI schemas and use standard tooling?

**Q25: The ResourcePathRegistry proposal suggests hierarchical paths like `/graphs/graph1/tasks/task-1`. Isn't this just REST with extra steps?**

If you're going to have URL-like paths, HTTP methods, and JSON payloads, why not use actual HTTP? What does the graph abstraction add?

### On Type Safety

**Q26: Your messages are `{ type: string; payload: Record<string, unknown> }`. Every handler casts payload. Why use TypeScript if you throw away type safety at boundaries?**

You write `message.payload as { goal: string; deliverables: string[]; ... }` everywhere. Why not `type StartMessage = { type: "start"; payload: StartPayload }`? Are you planning codegen, or did you just skip discriminated unions?

**Q27: The factory functions `createTask()` and `createKnowledge()` return concrete classes. How do you mock these in tests?**

If a test wants to verify that `handleSpawn` creates a child task, it has to stub `TaskNode` constructor. Why not inject a factory interface?

---

## V. Implementation Reality Checks

### On Concurrency

**Q28: ClaudeActor spawns child processes via the Claude CLI. What happens if you send 100 tasks to it simultaneously?**

Do you spawn 100 processes? Do you queue them? Is there a rate limit? The `send()` method is async, but there's no pooling or throttling. Does this work, or does it exhaust file descriptors?

**Q29: If two tasks simultaneously call `graph.send(knowledgeNodeId, "append", ...)`, do you get race conditions on the version number?**

Your `handleAppend()` does `this.properties.version++`. If two appends happen concurrently, do they both get version 2? Or is Node.js event loop serialization good enough?

**Q30: The BashActor runs shell commands. What prevents command injection if task goals come from user input?**

Is there sanitization? Escaping? Or is this a demo system where you trust all input?

### On Error Handling

**Q31: The Registry catches exceptions in `send()` and returns `{ success: false, error: message }`. But many message handlers don't check if child operations succeeded. What happens when `graph.addEdge()` throws?**

Does the caller see an exception? A generic error? Does state remain consistent, or can you end up with a task that thinks it has a child but the edge wasn't created?

**Q32: TaskNode has no retry logic. If `handleEval()` fails due to transient error (network timeout, etc.), does the task fail forever?**

Your criteria might call external APIs. If those flake, does eval just return `passed: false`? Or is there error handling?

**Q33: When a TaskNode is deleted, do its children get orphaned?**

`handleDelete()` removes edges, but do child tasks know their parent is gone? Do they fail? Do they become independent? Or do they just reference a missing parent?

### On Testing

**Q34: Your integration tests "require Claude CLI, run outside Claude." How do you test ClaudeActor in CI/CD without API keys and external dependencies?**

Is there a mock ClaudeActor? Or do integration tests just not run in CI? If they don't run, how do you prevent regressions?

**Q35: The demo.ts file shows interactive examples. How do you verify these examples stay correct as the API evolves?**

Are these tested? Or are they documentation that drifts out of sync with code?

**Q36: TaskNode.calculateProgress() recursively walks child tasks. What prevents infinite loops if there's a cycle in the task graph?**

Can tasks spawn grandchildren that spawn great-grandchildren that link back to the original task? If yes, does `calculateProgress()` hang?

---

## VI. Memory and Scale

### On Memory Reconsolidation

**Q37: The conversation mentions "memory reconsolidation." What is being reconsolidated, when, and by whom?**

Your system stores task properties and knowledge content. Is there a compaction mechanism? Do completed tasks get summarized? Or does the graph grow indefinitely?

**Q38: If memory reconsolidation happens, how do you preserve lineage and attribution?**

If you summarize 1000 completed tasks into a pattern, how do you trace back to the original task that taught you that pattern? Or do you throw away the source?

### On Access Patterns

**Q39: The conversation discusses "time travel, event sourcing, forking timelines." But your code has no event log. How do you replay history if there's no history?**

Are tasks immutable with event sourcing? Or are they mutable objects that get modified in place? If the latter, how do you implement time travel?

**Q40: Your `Graph.dump()` method returns all nodes and edges. What happens when you have 10,000 nodes?**

Is this for debugging only? Or is this how you plan to implement persistence? If it's the latter, how do you incrementally serialize?

---

## VII. Agent Autonomy

### On Deterministic vs. Agentic Actors

**Q41: Your actor spectrum shows "deterministic ← → non-deterministic" with BashActor and ClaudeActor at the extremes. But both just execute commands. Where's the agency?**

ClaudeActor calls `claude --session-id` and returns output. That's deterministic I/O, not agency. What makes it "non-deterministic" other than stochastic LLM sampling?

**Q42: If ClaudeActor is "agentic," who decides what tools it can use?**

Does it have tool-calling? If yes, how does it discover available BashActors or KnowledgeNodes? If no, in what sense is it an agent?

**Q43: The ChainedActors pattern pipes output from one ClaudeActor to another. How is this different from a shell pipeline?**

You could write `claude "plan" | claude "execute"` and get the same behavior. What does the ChainedActors abstraction add?

### On Task Delegation

**Q44: The BDD scenario shows a "coordinator" delegating to a "worker." But delegation isn't implemented—it's just an example. How would delegation actually work?**

Does the coordinator call `graph.send(workerId, ...)`? If so, it needs to know the worker's ID. How does it discover workers? Process groups? Registry query?

**Q45: If a CoordinatorActor delegates 10 subtasks to workers, how does it know when they're all done?**

Does it poll? Does it wait for messages? Your system has no monitor/notify mechanism. Does the coordinator just call `graph.send(childId, "query_status")` in a loop?

---

## VIII. Unstated Dependencies

### On External Systems

**Q46: KnowledgeNode will "use embeddings/LLM" for real queries. Does that mean every query hits an API? What's the latency? Cost?**

If each `query` message costs 0.1 seconds and $0.001, and you have 1000 tasks querying knowledge, that's 100 seconds and $1. Is that acceptable?

**Q47: The task lifecycle assumes someone or something sets `criterion.actual` before eval. Who is that? A human? An agent? A monitoring system?**

If it's an agent, where's the agent actor? If it's a human, where's the approval workflow? If it's automatic, where's the instrumentation hook?

### On Human-in-the-Loop

**Q48: Subjective criteria and manual evaluation are mentioned but not implemented. When a task needs human judgment, what happens?**

Does it block? Does it send an email? Does it create a "human review" task? Or is this out of scope?

**Q49: Your DESIGN.md says "Human-in-the-loop approval flow" is future work. But if that's core to task governance, why build the rest without it?**

Are you building a prototype to explore the design? Or are you building production infrastructure that will later need to be refactored for human approval?

---

## IX. Missing Guarantees

### On Consistency

**Q50: If two tasks simultaneously spawn children with the same goal, do you get duplicate tasks? Do you deduplicate? Do you care?**

There's no uniqueness check. Is it valid to have multiple "task_3", "task_4" with identical goals? Or should the graph enforce task identity?

**Q51: When you delete a task, edges are removed. But what if another task is concurrently creating an edge to the deleted task?**

Race condition: Task A calls `handleDelete()` while Task B calls `graph.addEdge(taskB.id, taskA.id, "depends_on")`. Does the edge get created? Does it reference a missing node?

### On Observability

**Q52: The `observe` message returns human-readable strings like "Task 'X' is active". How does a machine parse that?**

If an agent wants to know "is task X blocked?", does it parse "Task 'Build API' is blocked" as a string? Why not return structured data?

**Q53: There's no audit log. If a task transitions from active → completed, who did it, when, and why?**

Debugging failed workflows requires history. How do you trace a sequence of messages that led to a bad state?

---

## X. Philosophical Foundations

### On Category Theory and Lambda Calculus

**Q54: The conversation mentions category theory, set theory, lambda calculus connections. How do these inform the current implementation?**

Is `SEND` a morphism? Are nodes objects in a category? If yes, what are the composition rules? If no, why mention category theory?

**Q55: If tasks and knowledge are nodes in a graph, and edges represent relationships, what's the algebraic structure?**

Is this a DAG? Can it have cycles? Is it a monoid? A free category? Or is it just a directed graph with no additional structure?

**Q56: Smalltalk message passing is invoked as inspiration. But Smalltalk has "everything is an object" and "message as method invocation." Your system has "everything is a node" and "message as protocol type." Are these the same?**

Smalltalk's `object.message()` is synchronous method dispatch. Your `graph.send(id, type, payload)` is RPC over a registry. Where's the philosophical alignment?

---

## XI. The Real Question

**Q57: What problem are you actually solving?**

Is this a task runner? A workflow engine? A knowledge base? A multi-agent system? All of the above? 

If you removed the "actor" framing and just called things methods, would the system be simpler? If yes, what does the actor abstraction buy you?

If you fully committed to actors (async mailboxes, supervision trees, process isolation), would the system be more complex but more robust? If yes, why not do that?

**What is the core value proposition of tk-agents that justifies its current design?**

---

## Closing Socratic Reflection

You've built a system that:
- Claims to use "actor model" but has synchronous blocking calls
- Claims to use "message passing" but is really method dispatch with a string-based router
- Claims to use "Erlang patterns" but has no supervision, no mailboxes, no isolation
- References category theory and lambda calculus without showing how they apply
- Has rich type definitions but casts payloads as `unknown` everywhere
- Has sophisticated state machines in DESIGN.md but simple switch statements in code
- Promises LLM-based knowledge but ships keyword matching

**Is this a prototype to explore ideas, or production infrastructure?**
**Is it over-engineered for what it does, or under-engineered for what it claims?**
**Does the complexity pay for itself, or is it accidental?**

The honest answer to these questions will determine whether tk-agents is:
1. A **pragmatic graph-based task system** (own the simplicity)
2. An **actor framework prototype** (invest in supervision and async)
3. A **research exploration** (embrace the theoretical connections)

But it cannot be all three at once.

Which one is it?
