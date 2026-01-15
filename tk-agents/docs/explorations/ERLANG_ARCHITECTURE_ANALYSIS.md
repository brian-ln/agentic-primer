# Erlang/BEAM Architecture Analysis for tk-agents

## Executive Summary

This document analyzes the tk-agents architecture through the lens of Erlang/BEAM's concurrent, fault-tolerant design patterns. We examine four key aspects of Erlang's approach and compare them to our current TypeScript implementation:

1. **Process Supervision Trees and Fault Tolerance**
2. **OTP Behaviors (gen_server, supervisor, gen_statem)**
3. **Process Naming and Registration**
4. **Message Passing and Mailboxes**

The analysis reveals that while tk-agents implements actor-based patterns, there are significant opportunities to adopt Erlang's battle-tested approaches to supervision, lifecycle management, and error handling.

---

## 1. Process Supervision Trees and Fault Tolerance

### Erlang/BEAM Approach

In Erlang, processes are organized into **supervision trees** where supervisor processes monitor worker processes and restart them according to configurable strategies. This creates a hierarchy of fault containment:

```erlang
%% Erlang supervision tree example
-module(task_supervisor).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,    % Restart individual failing children
        intensity => 10,             % Max 10 restarts
        period => 60                 % Within 60 seconds
    },

    ChildSpecs = [
        #{
            id => task_manager,
            start => {task_manager, start_link, []},
            restart => permanent,     % Always restart
            shutdown => 5000,
            type => worker
        },
        #{
            id => claude_actor_pool,
            start => {claude_pool_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor          % Nested supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Key Erlang Principles:**
- **Let it crash**: Don't defensively handle every error; let supervisors restart failed processes
- **Fault isolation**: Each process has isolated memory; failures don't cascade
- **Restart strategies**:
  - `one_for_one`: Restart only the failed child
  - `one_for_all`: Restart all children if one fails
  - `rest_for_one`: Restart the failed child and all children started after it
  - `simple_one_for_one`: Dynamic pool of identical workers
- **Restart intensity**: Prevents restart loops from thrashing the system
- **Shutdown timeouts**: Graceful vs brutal kill for cleanup

### Current tk-agents Design

**What we have:**
```typescript
// src/actors/registry.ts
export class Registry {
  private actors: Map<string, ActorInfo> = new Map();

  async send(actorId: string, message: Message): Promise<Response> {
    const info = this.actors.get(actorId);
    if (!info) {
      return { success: false, error: `Actor not found: ${actorId}` };
    }

    try {
      return await info.actor.send(message);
    } catch (error) {
      // ERROR: Catches but doesn't recover or restart
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }
}
```

**What we're missing:**
1. No supervision hierarchy - the Registry is flat
2. No automatic restart on failure
3. No restart strategies or intensity limits
4. No fault isolation (all actors share Node.js process memory)
5. ClaudeActor spawns processes but doesn't supervise them
6. BashActor has basic timeout but no retry logic
7. Task graph has no failure recovery mechanisms

**Architecture gaps:**
```typescript
// src/task.ts - TaskNode lifecycle
private handleComplete(...): { success: boolean; finalState: TaskState } {
  const evalResult = this.handleEval(graph);

  if (!evalResult.passed) {
    // PROBLEM: Fails but doesn't record failure mode
    return { success: false, finalState: this.properties.state };
  }
  // No retry, no escalation, no supervision notification
}
```

### Recommendations

**1. Implement Supervisor Actors**

Create a `SupervisorActor` that monitors child actors and restarts them according to strategies:

```typescript
// Recommended: src/actors/supervisor.ts
interface SupervisorConfig {
  id: string;
  strategy: 'one_for_one' | 'one_for_all' | 'rest_for_one';
  maxRestarts: number;      // e.g., 10
  windowMs: number;         // e.g., 60000 (1 minute)
  children: ChildSpec[];
}

interface ChildSpec {
  id: string;
  factory: () => Actor;     // Function to create/recreate actor
  restart: 'permanent' | 'temporary' | 'transient';
  shutdownMs?: number;
}

class SupervisorActor implements Actor {
  private children: Map<string, {
    actor: Actor;
    spec: ChildSpec;
    restarts: Array<Date>;  // Track restart history
  }>;

  async handleChildCrash(childId: string, error: Error): Promise<void> {
    const child = this.children.get(childId);

    // Check restart intensity
    const recentRestarts = this.countRestartsInWindow(childId);
    if (recentRestarts >= this.config.maxRestarts) {
      // Escalate to parent supervisor or shutdown
      throw new Error(`Child ${childId} exceeded restart limit`);
    }

    // Apply restart strategy
    switch (this.config.strategy) {
      case 'one_for_one':
        await this.restartChild(childId);
        break;
      case 'one_for_all':
        await this.restartAllChildren();
        break;
      case 'rest_for_one':
        await this.restartFromChild(childId);
        break;
    }
  }
}
```

**2. Add Fault Recovery to Tasks**

Tasks should track failure modes and support retry with backoff:

```typescript
// Recommended addition to TaskProperties
interface TaskProperties extends NodeProperties {
  // ... existing properties ...

  failures?: Array<{
    occurredAt: Date;
    reason: string;
    attemptNumber: number;
  }>;
  retryPolicy?: {
    maxAttempts: number;
    backoffMs: number;
    backoffMultiplier: number;  // Exponential backoff
  };
}

// In TaskNode
private async handleRetriableFailure(error: Error): Promise<void> {
  const attempt = (this.properties.failures?.length ?? 0) + 1;
  const maxAttempts = this.properties.retryPolicy?.maxAttempts ?? 3;

  if (attempt >= maxAttempts) {
    this.properties.state = 'failed';
    return;
  }

  // Record failure
  this.properties.failures = [
    ...(this.properties.failures ?? []),
    { occurredAt: new Date(), reason: error.message, attemptNumber: attempt }
  ];

  // Calculate backoff
  const baseBackoff = this.properties.retryPolicy?.backoffMs ?? 1000;
  const multiplier = this.properties.retryPolicy?.backoffMultiplier ?? 2;
  const backoffMs = baseBackoff * Math.pow(multiplier, attempt - 1);

  // Schedule retry (would need event loop integration)
  setTimeout(() => this.handleStart({}), backoffMs);
}
```

**3. Process Health Monitoring**

Add health checks and circuit breakers for external actors (ClaudeActor, BashActor):

```typescript
// Recommended: src/actors/health.ts
interface HealthStatus {
  healthy: boolean;
  lastCheck: Date;
  consecutiveFailures: number;
  circuitState: 'closed' | 'open' | 'half_open';
}

class HealthMonitor {
  private health: Map<string, HealthStatus> = new Map();

  async checkHealth(actor: Actor): Promise<boolean> {
    const status = this.health.get(actor.id) ?? this.defaultStatus();

    // If circuit is open, fail fast
    if (status.circuitState === 'open') {
      const timeSinceOpen = Date.now() - status.lastCheck.getTime();
      if (timeSinceOpen < 30000) {  // 30s timeout
        return false;
      }
      status.circuitState = 'half_open';  // Try recovery
    }

    try {
      // Send ping message
      const response = await actor.send({
        id: 'health-check',
        type: 'ping',
        payload: {}
      });

      if (response.success) {
        status.healthy = true;
        status.consecutiveFailures = 0;
        status.circuitState = 'closed';
      } else {
        this.recordFailure(status);
      }
    } catch (error) {
      this.recordFailure(status);
    }

    this.health.set(actor.id, status);
    return status.healthy;
  }

  private recordFailure(status: HealthStatus): void {
    status.healthy = false;
    status.consecutiveFailures++;
    status.lastCheck = new Date();

    // Open circuit after 5 consecutive failures
    if (status.consecutiveFailures >= 5) {
      status.circuitState = 'open';
    }
  }
}
```

**4. Hierarchical Task Supervision**

Organize tasks into supervision trees where parent tasks supervise children:

```typescript
// Enhancement to TaskNode
class TaskNode implements NodeActor {
  async superviseChild(childId: string, graph: Graph): Promise<void> {
    const child = graph.getNode(childId);
    const childProps = child?.properties as TaskProperties;

    if (childProps.state === 'failed') {
      // Apply supervision strategy
      const strategy = this.properties.childSupervisionStrategy ?? 'fail_parent';

      switch (strategy) {
        case 'ignore':
          // Continue with other children
          break;
        case 'restart':
          // Attempt to restart child task
          await this.restartChild(childId, graph);
          break;
        case 'fail_parent':
          // Propagate failure up the tree
          this.properties.state = 'failed';
          break;
      }
    }
  }
}
```

---

## 2. OTP Behaviors (gen_server, supervisor, gen_statem)

### Erlang/BEAM Approach

Erlang's OTP (Open Telecom Platform) provides **behavior modules** that standardize common patterns. These are not libraries but *contracts* that ensure actors follow well-understood lifecycles.

#### gen_server (Generic Server)

The most common OTP behavior for stateful actors:

```erlang
-module(task_actor).
-behaviour(gen_server).

%% Client API
-export([start_link/1, send_message/2, get_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% State record
-record(state, {
    task_id,
    status,
    goal,
    criteria,
    children = []
}).

%% Synchronous call - blocks until response
handle_call({send_message, Msg}, _From, State) ->
    case process_message(Msg, State) of
        {ok, NewState, Reply} ->
            {reply, {ok, Reply}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State}.

%% Asynchronous cast - fire and forget
handle_cast({update_status, NewStatus}, State) ->
    NewState = State#state{status = NewStatus},
    {noreply, NewState}.

%% Handle system messages, timeouts, exits
handle_info({task_completed, ChildId}, State) ->
    NewChildren = lists:delete(ChildId, State#state.children),
    {noreply, State#state{children = NewChildren}}.

%% Cleanup on shutdown
terminate(_Reason, State) ->
    cleanup_resources(State),
    ok.
```

**Key gen_server features:**
- **Synchronous calls** (`handle_call`): Request-response with timeout
- **Asynchronous casts** (`handle_cast`): Fire-and-forget messages
- **Info messages** (`handle_info`): System events, monitor signals, timers
- **Lifecycle hooks**: `init/1`, `terminate/2` for setup/cleanup
- **Built-in timeout handling**: Calls can specify timeout and fallback
- **Hot code swapping**: `code_change/3` callback for live upgrades

#### supervisor Behavior

Already covered in Section 1, but worth noting it's a formal behavior with guaranteed semantics.

#### gen_statem (State Machine)

For actors with explicit state transitions (like TaskNode):

```erlang
-module(task_statem).
-behaviour(gen_statem).

%% State machine states
-export([created/3, active/3, blocked/3, completed/3]).

%% Callbacks
-export([init/1, callback_mode/0, terminate/3]).

callback_mode() -> state_functions.  % One function per state

init(Args) ->
    InitialState = created,
    Data = #{
        goal => maps:get(goal, Args),
        criteria => maps:get(criteria, Args)
    },
    {ok, InitialState, Data}.

%% State: created
created({call, From}, start, Data) ->
    %% Transition to active
    NewData = Data#{started_at => erlang:system_time()},
    {next_state, active, NewData, [{reply, From, ok}]};

created({call, From}, _OtherEvent, Data) ->
    {keep_state, Data, [{reply, From, {error, not_started}}]}.

%% State: active
active({call, From}, eval, Data) ->
    case evaluate_criteria(Data) of
        {passed, Score} ->
            {keep_state, Data#{score => Score}, [{reply, From, {ok, Score}}]};
        {failed, Reason} ->
            {next_state, blocked, Data#{block_reason => Reason},
             [{reply, From, {blocked, Reason}}]}
    end;

active({call, From}, complete, Data) ->
    %% Can only complete from active state
    {next_state, completed, Data#{completed_at => erlang:system_time()},
     [{reply, From, ok}]}.

%% State: blocked
blocked({call, From}, unblock, Data) ->
    {next_state, active, maps:remove(block_reason, Data),
     [{reply, From, ok}]};

blocked({call, From}, complete, _Data) ->
    {keep_state_and_data, [{reply, From, {error, task_blocked}}]}.

%% Terminal state
completed({call, From}, _AnyEvent, _Data) ->
    {keep_state_and_data, [{reply, From, {error, already_completed}}]}.

terminate(_Reason, _State, _Data) ->
    ok.
```

**Key gen_statem features:**
- **Explicit state transitions**: `{next_state, NewState, NewData}`
- **State-based message handling**: Different logic per state
- **Automatic invalid transition handling**: Unhandled events return errors
- **Entry/exit actions**: Code that runs on state transitions
- **Timeout transitions**: Automatic state changes after delay

### Current tk-agents Design

**What we have:**

```typescript
// src/actors/base.ts - Generic Actor interface
export interface Actor {
  readonly id: string;
  readonly type: "deterministic" | "agent";
  send(message: Message): Promise<Response>;
  stream?(message: Message): AsyncGenerator<StreamEvent, Response>;
  start?(): Promise<void>;
  stop?(): Promise<void>;
}
```

**Pros:**
- Simple, unified interface
- TypeScript type safety
- Supports both sync (send) and async (stream) patterns

**Cons:**
- No standard lifecycle hooks (no `init`, `terminate`)
- No distinction between synchronous calls and asynchronous casts
- No built-in timeout handling
- No code hot-swapping support
- State machine logic is implicit (TaskNode's `switch` statements)

**Example of implicit state machine:**

```typescript
// src/task.ts - TaskNode message handling
handleMessage(message: Message, graph: Graph): unknown {
  switch (message.type) {
    case "start":
      return this.handleStart(...);  // Changes state internally
    case "complete":
      return this.handleComplete(...);  // Changes state internally
    // ... no enforcement of valid state transitions
  }
}

private handleStart(payload: { context?: Record<string, unknown> }):
  { success: boolean; state: TaskState } {

  // STATE TRANSITION LOGIC BURIED IN IMPLEMENTATION
  if (this.properties.state !== "created" && this.properties.state !== "ready") {
    return { success: false, state: this.properties.state };
  }

  this.properties.state = "active";  // Direct mutation
  // ...
}
```

The state machine is there, but it's **implicit** and **unchecked**. There's no enforcement that `complete` can only be called from `active`, or that `block` transitions are valid.

### Recommendations

**1. Formalize Actor Behaviors as TypeScript Interfaces**

Create explicit behavior contracts with lifecycle guarantees:

```typescript
// Recommended: src/actors/behaviors.ts

/**
 * GenServer-like behavior for request-response actors
 */
export interface GenServerBehavior<State> extends Actor {
  // Synchronous call with timeout
  call<R>(request: Message, timeoutMs?: number): Promise<R>;

  // Asynchronous cast (no response expected)
  cast(message: Message): Promise<void>;

  // Lifecycle hooks
  init(args: unknown): Promise<State>;
  handleCall(message: Message, state: State): Promise<{ reply: unknown; newState: State }>;
  handleCast(message: Message, state: State): Promise<State>;
  handleInfo(info: SystemMessage, state: State): Promise<State>;
  terminate(reason: string, state: State): Promise<void>;
}

/**
 * StateMachine behavior for actors with explicit state transitions
 */
export interface StateMachineBehavior<S extends string, D> extends Actor {
  // Get current state
  getState(): S;

  // Transition to new state
  transition(event: Message, currentState: S, data: D):
    Promise<{ nextState: S; newData: D; reply?: unknown }>;

  // Validate transition
  isValidTransition(from: S, to: S): boolean;

  // State entry/exit hooks
  onEnter?(state: S, data: D): Promise<void>;
  onExit?(state: S, data: D): Promise<void>;
}

/**
 * Supervisor behavior for managing child actors
 */
export interface SupervisorBehavior extends Actor {
  startChild(spec: ChildSpec): Promise<Actor>;
  stopChild(childId: string, reason: string): Promise<void>;
  restartChild(childId: string): Promise<void>;
  getChildren(): Array<{ id: string; actor: Actor; status: 'running' | 'failed' }>;
}
```

**2. Implement TaskNode as gen_statem**

Refactor TaskNode to use explicit state machine behavior:

```typescript
// Recommended: src/task-statem.ts
type TaskState = "created" | "ready" | "active" | "blocked" | "completed" | "failed";

interface TaskData {
  goal: string;
  criteria: ObjectiveCriterion[];
  startedAt?: Date;
  completedAt?: Date;
  blockReason?: string;
}

class TaskStateMachine implements StateMachineBehavior<TaskState, TaskData> {
  private currentState: TaskState = "created";
  private data: TaskData;

  // Define valid transitions (like Erlang state_functions)
  private readonly stateHandlers: Record<TaskState, StateHandler> = {
    created: this.handleCreatedState.bind(this),
    ready: this.handleReadyState.bind(this),
    active: this.handleActiveState.bind(this),
    blocked: this.handleBlockedState.bind(this),
    completed: this.handleCompletedState.bind(this),
    failed: this.handleFailedState.bind(this),
  };

  async transition(event: Message, state: TaskState, data: TaskData) {
    const handler = this.stateHandlers[state];

    if (!handler) {
      throw new Error(`No handler for state: ${state}`);
    }

    return await handler(event, data);
  }

  private async handleCreatedState(event: Message, data: TaskData) {
    switch (event.type) {
      case "start":
        return {
          nextState: "active" as TaskState,
          newData: { ...data, startedAt: new Date() },
          reply: { success: true }
        };

      default:
        return {
          nextState: "created" as TaskState,
          newData: data,
          reply: { success: false, error: "Task not started" }
        };
    }
  }

  private async handleActiveState(event: Message, data: TaskData) {
    switch (event.type) {
      case "eval":
        const passed = this.evaluateCriteria(data);
        if (!passed) {
          return {
            nextState: "blocked" as TaskState,
            newData: { ...data, blockReason: "Criteria not met" },
            reply: { success: false, blocked: true }
          };
        }
        return {
          nextState: "active" as TaskState,
          newData: data,
          reply: { success: true, passed: true }
        };

      case "complete":
        if (!this.evaluateCriteria(data)) {
          return {
            nextState: "active" as TaskState,
            newData: data,
            reply: { success: false, error: "Cannot complete: criteria not met" }
          };
        }
        return {
          nextState: "completed" as TaskState,
          newData: { ...data, completedAt: new Date() },
          reply: { success: true }
        };

      case "block":
        return {
          nextState: "blocked" as TaskState,
          newData: { ...data, blockReason: event.payload.reason },
          reply: { success: true }
        };

      default:
        return { nextState: "active" as TaskState, newData: data };
    }
  }

  private async handleBlockedState(event: Message, data: TaskData) {
    switch (event.type) {
      case "unblock":
        return {
          nextState: "active" as TaskState,
          newData: { ...data, blockReason: undefined },
          reply: { success: true }
        };

      case "complete":
        return {
          nextState: "blocked" as TaskState,
          newData: data,
          reply: { success: false, error: "Cannot complete blocked task" }
        };

      default:
        return { nextState: "blocked" as TaskState, newData: data };
    }
  }

  private async handleCompletedState(event: Message, data: TaskData) {
    // Terminal state - reject all transitions
    return {
      nextState: "completed" as TaskState,
      newData: data,
      reply: { success: false, error: "Task already completed" }
    };
  }

  private async handleFailedState(event: Message, data: TaskData) {
    // Terminal state - only allow restart (would spawn new task)
    if (event.type === "restart") {
      return {
        nextState: "created" as TaskState,
        newData: { ...data, startedAt: undefined, completedAt: undefined },
        reply: { success: true }
      };
    }

    return {
      nextState: "failed" as TaskState,
      newData: data,
      reply: { success: false, error: "Task failed" }
    };
  }

  isValidTransition(from: TaskState, to: TaskState): boolean {
    const validTransitions: Record<TaskState, TaskState[]> = {
      created: ["active", "ready"],
      ready: ["active"],
      active: ["blocked", "completed", "failed"],
      blocked: ["active", "failed"],
      completed: [],  // Terminal
      failed: ["created"],  // Only via restart
    };

    return validTransitions[from]?.includes(to) ?? false;
  }
}
```

**3. Add GenServer-style Call/Cast to ClaudeActor**

Differentiate between synchronous requests (waiting for response) and async events:

```typescript
// Enhanced ClaudeActor with gen_server semantics
class ClaudeActorGenServer implements GenServerBehavior<ClaudeState> {
  private state: ClaudeState;

  // Synchronous call with timeout (like gen_server:call)
  async call<R>(request: Message, timeoutMs: number = 30000): Promise<R> {
    const timeoutPromise = new Promise<never>((_, reject) => {
      setTimeout(() => reject(new Error('Call timeout')), timeoutMs);
    });

    const callPromise = this.handleCall(request, this.state);

    try {
      const { reply, newState } = await Promise.race([callPromise, timeoutPromise]);
      this.state = newState;
      return reply as R;
    } catch (error) {
      // Call failed - state unchanged
      throw error;
    }
  }

  // Asynchronous cast (like gen_server:cast)
  async cast(message: Message): Promise<void> {
    const newState = await this.handleCast(message, this.state);
    this.state = newState;
    // No return value, no waiting for completion
  }

  async handleCall(message: Message, state: ClaudeState):
    Promise<{ reply: unknown; newState: ClaudeState }> {

    switch (message.type) {
      case "prompt":
        // Synchronous request - wait for Claude response
        const response = await this.executeClaude(message.payload as string);
        return {
          reply: response,
          newState: { ...state, turnCount: state.turnCount + 1 }
        };

      case "get_session_id":
        // Query state - no state change
        return {
          reply: state.sessionId,
          newState: state
        };

      default:
        throw new Error(`Unknown call: ${message.type}`);
    }
  }

  async handleCast(message: Message, state: ClaudeState): Promise<ClaudeState> {
    switch (message.type) {
      case "reset_session":
        // Async update - caller doesn't wait for completion
        return {
          ...state,
          sessionId: crypto.randomUUID(),
          turnCount: 0
        };

      case "update_config":
        return {
          ...state,
          config: { ...state.config, ...message.payload }
        };

      default:
        return state;
    }
  }

  async terminate(reason: string, state: ClaudeState): Promise<void> {
    console.log(`Terminating ClaudeActor ${state.sessionId}: ${reason}`);
    // Cleanup: kill any running child processes, flush buffers, etc.
  }
}
```

**4. Create Base Behavior Classes**

Provide abstract base classes that implement common patterns:

```typescript
// Recommended: src/actors/gen-server-base.ts
export abstract class GenServerBase<State> implements GenServerBehavior<State> {
  readonly id: string;
  readonly type: "deterministic" | "agent";

  protected state: State;
  private infoQueue: SystemMessage[] = [];

  constructor(id: string, type: "deterministic" | "agent") {
    this.id = id;
    this.type = type;
  }

  // Public API delegates to behavior callbacks
  async call<R>(request: Message, timeoutMs?: number): Promise<R> {
    const { reply, newState } = await this.handleCall(request, this.state);
    this.state = newState;
    return reply as R;
  }

  async cast(message: Message): Promise<void> {
    this.state = await this.handleCast(message, this.state);
  }

  // Process queued system messages (timers, monitors, etc.)
  async processInfo(): Promise<void> {
    while (this.infoQueue.length > 0) {
      const info = this.infoQueue.shift()!;
      this.state = await this.handleInfo(info, this.state);
    }
  }

  // Subclasses implement these
  abstract init(args: unknown): Promise<State>;
  abstract handleCall(message: Message, state: State):
    Promise<{ reply: unknown; newState: State }>;
  abstract handleCast(message: Message, state: State): Promise<State>;
  abstract handleInfo(info: SystemMessage, state: State): Promise<State>;
  abstract terminate(reason: string, state: State): Promise<void>;

  // Compatibility with existing Actor interface
  async send(message: Message): Promise<Response> {
    try {
      const reply = await this.call(message);
      return { success: true, data: reply };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }
}
```

---

## 3. Process Naming and Registration

### Erlang/BEAM Approach

Erlang provides multiple registration mechanisms for addressing processes:

#### Local Registration

```erlang
%% Register a process with a local name (atom)
register(task_manager, spawn(task_manager, start, [])).

%% Send message by name
task_manager ! {add_task, TaskId, Goal}.

%% Unregister
unregister(task_manager).
```

**Characteristics:**
- Globally unique within a node
- Atom-based (not strings)
- Automatic cleanup on process death
- Fast lookup (O(1) hash table)

#### Process Groups (pg module)

```erlang
%% Join a process group
pg:join(claude_actors, self()).

%% Get all members of a group
Members = pg:get_members(claude_actors),

%% Send to all members (broadcast)
[Pid ! Message || Pid <- Members].
```

**Use cases:**
- Load balancing across actor pools
- Broadcast messages to all actors of a type
- Dynamic discovery of actors

#### Global Registration (distributed systems)

```erlang
%% Register globally across all connected nodes
global:register_name(task_coordinator, Pid),

%% Lookup from any node
Pid = global:whereis_name(task_coordinator),
global:send(task_coordinator, Message).
```

**Characteristics:**
- Works across distributed nodes
- Automatic re-registration on node failure
- Consistency guarantees during network splits

#### Named ETS Tables (in-memory database)

```erlang
%% Create named table
ets:new(tasks, [named_table, public, set]),

%% Direct access by name
ets:insert(tasks, {task_id_1, #task{goal="Deploy"}}),
Task = ets:lookup(tasks, task_id_1).
```

**Use cases:**
- Shared state without message passing
- Fast lookups for large datasets
- Pattern matching queries

### Current tk-agents Design

**What we have:**

```typescript
// src/actors/registry.ts
export class Registry {
  private actors: Map<string, ActorInfo> = new Map();

  register(actor: Actor): void {
    if (this.actors.has(actor.id)) {
      throw new Error(`Actor already registered: ${actor.id}`);
    }
    this.actors.set(actor.id, { actor, /* ... */ });
  }

  get(actorId: string): Actor | undefined {
    return this.actors.get(actorId)?.actor;
  }
}

// Singleton instance
export const registry = new Registry();
```

**Pros:**
- Simple string-based IDs
- Fast lookups (Map is O(1))
- Type-safe with TypeScript

**Cons:**
- No automatic cleanup on actor death
- No process groups or broadcast support
- No distributed registration (single process only)
- No named shared memory (like ETS)
- Registry is global singleton (not composable)
- No monitoring/linking between registry and actors

**Example of missing features:**

```typescript
// Current limitation: No way to find all ClaudeActors
const allClaude = registry.list().filter(a => /* how to identify ClaudeActor? */);

// No broadcast to all actors of a type
const bashActors = /* ??? */;
for (const actor of bashActors) {
  await actor.send({ type: 'shutdown', payload: {} });
}

// No way to detect when actor dies
const actor = registry.get('some-id');
// If actor crashes, registry still holds stale reference
```

### Recommendations

**1. Add Process Groups**

Implement actor groups for type-based discovery and broadcast:

```typescript
// Recommended: src/actors/process-groups.ts
class ProcessGroupRegistry {
  private groups: Map<string, Set<string>> = new Map();  // group -> actor IDs
  private actorGroups: Map<string, Set<string>> = new Map();  // actor -> groups

  join(groupName: string, actorId: string): void {
    if (!this.groups.has(groupName)) {
      this.groups.set(groupName, new Set());
    }
    this.groups.get(groupName)!.add(actorId);

    if (!this.actorGroups.has(actorId)) {
      this.actorGroups.set(actorId, new Set());
    }
    this.actorGroups.get(actorId)!.add(groupName);
  }

  leave(groupName: string, actorId: string): void {
    this.groups.get(groupName)?.delete(actorId);
    this.actorGroups.get(actorId)?.delete(groupName);
  }

  getMembers(groupName: string): string[] {
    return [...(this.groups.get(groupName) ?? [])];
  }

  getGroups(actorId: string): string[] {
    return [...(this.actorGroups.get(actorId) ?? [])];
  }

  // Cleanup when actor dies
  removeActor(actorId: string): void {
    const groups = this.actorGroups.get(actorId) ?? new Set();
    for (const group of groups) {
      this.leave(group, actorId);
    }
    this.actorGroups.delete(actorId);
  }

  // Broadcast message to all group members
  async broadcast(
    groupName: string,
    message: Message,
    registry: Registry
  ): Promise<Response[]> {
    const members = this.getMembers(groupName);
    return await Promise.all(
      members.map(id => registry.send(id, message))
    );
  }
}

// Usage:
const pg = new ProcessGroupRegistry();

// Register ClaudeActors in a group
pg.join('claude-actors', claude1.id);
pg.join('claude-actors', claude2.id);

// Find all Claude actors
const claudeIds = pg.getMembers('claude-actors');

// Broadcast shutdown
await pg.broadcast('claude-actors', { type: 'shutdown', payload: {} }, registry);
```

**2. Implement Actor Monitoring and Linking**

Add Erlang-style monitoring so registry knows when actors die:

```typescript
// Recommended: src/actors/monitoring.ts
type MonitorRef = string;

interface MonitorEvent {
  type: 'down' | 'exit';
  monitoredId: string;
  monitoringId: string;
  reason: string;
}

class ActorMonitor {
  private monitors: Map<MonitorRef, {
    monitoredId: string;
    monitoringId: string;
  }> = new Map();

  private reverseIndex: Map<string, Set<MonitorRef>> = new Map();

  // Start monitoring an actor
  monitor(monitoringId: string, monitoredId: string): MonitorRef {
    const ref = `mon_${Date.now()}_${Math.random()}`;

    this.monitors.set(ref, { monitoredId, monitoringId });

    if (!this.reverseIndex.has(monitoredId)) {
      this.reverseIndex.set(monitoredId, new Set());
    }
    this.reverseIndex.get(monitoredId)!.add(ref);

    return ref;
  }

  // Stop monitoring
  demonitor(ref: MonitorRef): void {
    const info = this.monitors.get(ref);
    if (!info) return;

    this.monitors.delete(ref);
    this.reverseIndex.get(info.monitoredId)?.delete(ref);
  }

  // Notify all monitors when an actor dies
  async notifyDown(
    actorId: string,
    reason: string,
    registry: Registry
  ): Promise<void> {
    const refs = this.reverseIndex.get(actorId) ?? new Set();

    for (const ref of refs) {
      const info = this.monitors.get(ref);
      if (!info) continue;

      const monitoring = registry.get(info.monitoringId);
      if (!monitoring) continue;

      // Send 'down' message to monitoring actor
      const event: MonitorEvent = {
        type: 'down',
        monitoredId: actorId,
        monitoringId: info.monitoringId,
        reason,
      };

      await monitoring.send({ type: 'monitor_down', payload: event });

      this.demonitor(ref);
    }

    this.reverseIndex.delete(actorId);
  }
}

// Enhanced Registry with monitoring
class MonitoringRegistry extends Registry {
  private monitor = new ActorMonitor();

  register(actor: Actor): void {
    super.register(actor);

    // Monitor the actor if it has a stop hook
    if (actor.stop) {
      const originalStop = actor.stop.bind(actor);
      actor.stop = async () => {
        await originalStop();
        await this.handleActorExit(actor.id, 'normal');
      };
    }
  }

  private async handleActorExit(actorId: string, reason: string): Promise<void> {
    // Notify all monitors
    await this.monitor.notifyDown(actorId, reason, this);

    // Remove from registry
    this.unregister(actorId);
  }

  // Public API for monitoring
  monitorActor(monitoringId: string, monitoredId: string): MonitorRef {
    return this.monitor.monitor(monitoringId, monitoredId);
  }

  demonitorActor(ref: MonitorRef): void {
    this.monitor.demonitor(ref);
  }
}
```

**3. Add Named Shared State (ETS-like)**

For high-performance shared data access without message passing:

```typescript
// Recommended: src/actors/shared-tables.ts
type TableAccess = 'public' | 'protected' | 'private';

interface TableOptions {
  name: string;
  access: TableAccess;
  type: 'set' | 'bag' | 'ordered_set';
}

class SharedTable<K, V> {
  private data: Map<K, V> = new Map();
  private owner: string;
  private options: TableOptions;

  constructor(owner: string, options: TableOptions) {
    this.owner = owner;
    this.options = options;
  }

  insert(key: K, value: V, callerId: string): void {
    this.checkAccess(callerId, 'write');
    this.data.set(key, value);
  }

  lookup(key: K, callerId: string): V | undefined {
    this.checkAccess(callerId, 'read');
    return this.data.get(key);
  }

  delete(key: K, callerId: string): void {
    this.checkAccess(callerId, 'write');
    this.data.delete(key);
  }

  // Pattern matching (simplified)
  match(pattern: Partial<V>, callerId: string): V[] {
    this.checkAccess(callerId, 'read');

    return [...this.data.values()].filter(value => {
      return Object.entries(pattern).every(([k, v]) =>
        (value as any)[k] === v
      );
    });
  }

  private checkAccess(callerId: string, operation: 'read' | 'write'): void {
    if (this.options.access === 'private' && callerId !== this.owner) {
      throw new Error(`Access denied: ${this.options.name} is private to ${this.owner}`);
    }

    if (this.options.access === 'protected' && operation === 'write' && callerId !== this.owner) {
      throw new Error(`Write denied: ${this.options.name} is protected`);
    }
  }
}

class SharedTableManager {
  private tables: Map<string, SharedTable<any, any>> = new Map();

  createTable(owner: string, options: TableOptions): SharedTable<any, any> {
    if (this.tables.has(options.name)) {
      throw new Error(`Table already exists: ${options.name}`);
    }

    const table = new SharedTable(owner, options);
    this.tables.set(options.name, table);
    return table;
  }

  getTable(name: string): SharedTable<any, any> | undefined {
    return this.tables.get(name);
  }

  deleteTable(name: string): void {
    this.tables.delete(name);
  }
}

// Usage example:
const tableMgr = new SharedTableManager();

// Create shared task index
const taskTable = tableMgr.createTable('task-manager', {
  name: 'tasks',
  access: 'public',
  type: 'set'
});

// Fast lookups without message passing
taskTable.insert('task-1', { goal: 'Deploy', state: 'active' }, 'task-manager');
const task = taskTable.lookup('task-1', 'some-actor');

// Pattern matching
const activeTasks = taskTable.match({ state: 'active' }, 'coordinator');
```

**4. Add REST-style Resource Addressing**

Complement actor IDs with hierarchical resource paths:

```typescript
// Recommended: src/actors/resource-paths.ts
class ResourcePathRegistry {
  private pathToId: Map<string, string> = new Map();
  private idToPath: Map<string, string> = new Map();

  registerPath(path: string, actorId: string): void {
    this.pathToId.set(path, actorId);
    this.idToPath.set(actorId, path);
  }

  resolvePath(path: string): string | undefined {
    return this.pathToId.get(path);
  }

  getPath(actorId: string): string | undefined {
    return this.idToPath.get(actorId);
  }

  // Pattern matching (e.g., /graphs/graph1/tasks/*)
  matchPattern(pattern: string): string[] {
    const regex = new RegExp('^' + pattern.replace('*', '.*') + '$');
    return [...this.pathToId.keys()].filter(p => regex.test(p));
  }
}

// Usage:
const pathRegistry = new ResourcePathRegistry();

// Register with hierarchical paths
pathRegistry.registerPath('/graphs/graph1/tasks/task-1', 'task-1');
pathRegistry.registerPath('/graphs/graph1/tasks/task-2', 'task-2');
pathRegistry.registerPath('/graphs/graph1/knowledge/kb-1', 'knowledge-1');

// Resolve path to actor ID
const actorId = pathRegistry.resolvePath('/graphs/graph1/tasks/task-1');

// Find all tasks in graph1
const taskPaths = pathRegistry.matchPattern('/graphs/graph1/tasks/*');
```

---

## 4. Message Passing and Mailboxes

### Erlang/BEAM Approach

Erlang's message passing is **asynchronous**, **unbounded**, and **ordered**. Each process has an **implicit mailbox** that queues incoming messages.

#### Sending Messages

```erlang
%% Send is always asynchronous (fire and forget)
Pid ! {add_task, TaskId, Goal},

%% Sender continues immediately, no blocking
Pid ! message1,
Pid ! message2,
Pid ! message3.  % All three send immediately
```

**Key properties:**
- **Non-blocking**: Send never waits
- **Asynchronous by default**: No response expected unless you build request-response pattern
- **No delivery guarantees**: If target process dies, message is lost silently

#### Receiving Messages

```erlang
%% Selective receive - pattern match on messages
receive
    {task_completed, TaskId} ->
        handle_completion(TaskId);
    {task_failed, TaskId, Reason} ->
        handle_failure(TaskId, Reason);
    {status_request, From} ->
        From ! {status, get_status()};
    _Other ->
        %% Ignore unknown messages
        ok
after 5000 ->
    %% Timeout after 5 seconds
    timeout
end.
```

**Key properties:**
- **Pattern matching**: Process selects which messages to handle
- **Message ordering**: Messages from same sender arrive in order
- **Selective consumption**: Can skip messages and handle them later
- **Timeout support**: Built-in timeout for receives

#### Mailbox Behavior

```erlang
%% Mailbox is FIFO queue, but selective receive can skip messages
-module(mailbox_example).

loop(State) ->
    receive
        %% Priority message - handle immediately even if queue is full
        {urgent, Msg} ->
            handle_urgent(Msg),
            loop(State);

        %% Normal message - only handle if no urgent messages
        {normal, Msg} ->
            handle_normal(Msg),
            loop(State)
    after 100 ->
        %% No messages, do periodic work
        periodic_cleanup(),
        loop(State)
    end.
```

**Mailbox can grow unbounded**, leading to memory issues. Erlang provides introspection:

```erlang
%% Check mailbox size
{message_queue_len, N} = process_info(self(), message_queue_len),

%% Get mailbox messages (debugging)
{messages, MsgList} = process_info(self(), messages).
```

#### Request-Response Pattern (manual implementation)

```erlang
%% Client: send request and wait for response
request(ServerPid, Request) ->
    Ref = make_ref(),  % Unique reference for this request
    ServerPid ! {request, self(), Ref, Request},

    receive
        {response, Ref, Reply} ->
            {ok, Reply}
    after 5000 ->
        {error, timeout}
    end.

%% Server: reply to specific request
handle_request({request, From, Ref, Request}) ->
    Reply = process_request(Request),
    From ! {response, Ref, Reply}.
```

### Current tk-agents Design

**What we have:**

```typescript
// src/actors/base.ts
export interface Actor {
  send(message: Message): Promise<Response>;
  stream?(message: Message): AsyncGenerator<StreamEvent, Response>;
}

// src/actors/registry.ts
async send(actorId: string, message: Message): Promise<Response> {
  const info = this.actors.get(actorId);
  // ...
  return await info.actor.send(message);  // BLOCKING AWAIT
}
```

**Characteristics:**
- **Synchronous-style**: `await actor.send()` blocks until response
- **No mailbox**: Messages are processed immediately or fail
- **No queuing**: If actor is busy, caller waits (or times out in JS runtime)
- **No selective receive**: Actor must handle every message in order
- **No timeout primitives**: Timeout handling is manual

**Example showing blocking behavior:**

```typescript
// If ClaudeActor takes 30 seconds to respond, caller blocks for 30s
const response1 = await registry.send('claude', { type: 'prompt', payload: 'Long task' });

// Second request can't start until first completes
const response2 = await registry.send('claude', { type: 'prompt', payload: 'Another task' });
```

**No message prioritization:**

```typescript
// If BashActor receives 1000 low-priority messages,
// urgent message must wait in line (or isn't handled at all)
for (let i = 0; i < 1000; i++) {
  await actor.send({ type: 'low_priority', payload: i });
}

// This urgent message has to wait for all 1000 to complete
await actor.send({ type: 'urgent', payload: 'CRITICAL' });
```

### Recommendations

**1. Implement Actor Mailboxes**

Add explicit mailbox queues with selective receive:

```typescript
// Recommended: src/actors/mailbox.ts
interface QueuedMessage {
  message: Message;
  priority: number;
  receivedAt: Date;
  correlationId?: string;
}

class Mailbox {
  private queue: QueuedMessage[] = [];
  private maxSize: number;
  private processingMessage: boolean = false;

  constructor(maxSize: number = 1000) {
    this.maxSize = maxSize;
  }

  // Non-blocking send
  post(message: Message, priority: number = 0): void {
    if (this.queue.length >= this.maxSize) {
      throw new Error(`Mailbox full (${this.maxSize} messages)`);
    }

    this.queue.push({
      message,
      priority,
      receivedAt: new Date(),
      correlationId: message.correlationId,
    });

    // Sort by priority (higher first)
    this.queue.sort((a, b) => b.priority - a.priority);
  }

  // Selective receive with pattern matching
  async receive(
    pattern: (msg: Message) => boolean,
    timeoutMs?: number
  ): Promise<Message | null> {

    const startTime = Date.now();

    while (true) {
      // Check for matching message in queue
      const index = this.queue.findIndex(qm => pattern(qm.message));

      if (index !== -1) {
        const qm = this.queue.splice(index, 1)[0];
        return qm.message;
      }

      // No match - wait for new message or timeout
      if (timeoutMs !== undefined) {
        const elapsed = Date.now() - startTime;
        if (elapsed >= timeoutMs) {
          return null;  // Timeout
        }
      }

      // Sleep briefly and check again (in real impl, use event emitter)
      await new Promise(resolve => setTimeout(resolve, 10));
    }
  }

  // Peek at queue without consuming
  peek(): Message | null {
    return this.queue[0]?.message ?? null;
  }

  // Get queue size (for monitoring)
  size(): number {
    return this.queue.length;
  }

  // Drain all messages (for shutdown)
  drain(): Message[] {
    const messages = this.queue.map(qm => qm.message);
    this.queue = [];
    return messages;
  }
}

// Enhanced Actor with mailbox
abstract class MailboxActor implements Actor {
  readonly id: string;
  readonly type: "deterministic" | "agent";

  protected mailbox: Mailbox;
  private processingLoop: Promise<void> | null = null;

  constructor(id: string, type: "deterministic" | "agent", mailboxSize?: number) {
    this.id = id;
    this.type = type;
    this.mailbox = new Mailbox(mailboxSize);
  }

  // Non-blocking send
  async send(message: Message): Promise<Response> {
    this.mailbox.post(message);

    // Wait for response (uses correlationId to match)
    return await this.awaitResponse(message.id);
  }

  // Start the message processing loop
  async start(): Promise<void> {
    if (this.processingLoop) return;

    this.processingLoop = this.runLoop();
  }

  private async runLoop(): Promise<void> {
    while (true) {
      // Selective receive with pattern matching
      const message = await this.mailbox.receive(
        msg => this.shouldHandle(msg),
        100  // 100ms timeout for periodic checks
      );

      if (message) {
        try {
          const response = await this.handleMessage(message);
          await this.sendResponse(message.id, response);
        } catch (error) {
          await this.sendResponse(message.id, {
            success: false,
            error: error instanceof Error ? error.message : String(error)
          });
        }
      }

      // Periodic housekeeping
      await this.periodicMaintenance();
    }
  }

  // Subclasses override to implement pattern matching
  protected shouldHandle(message: Message): boolean {
    return true;  // Default: handle all messages
  }

  // Subclasses implement actual message handling
  protected abstract handleMessage(message: Message): Promise<Response>;

  // Response handling (would need response routing mechanism)
  private async sendResponse(messageId: string, response: Response): Promise<void> {
    // Implementation depends on how responses are routed back to callers
  }

  private async awaitResponse(messageId: string): Promise<Response> {
    // Implementation depends on response routing
    throw new Error('Not implemented');
  }

  private async periodicMaintenance(): Promise<void> {
    // Check mailbox size, log warnings, etc.
    const size = this.mailbox.size();
    if (size > 100) {
      console.warn(`Actor ${this.id} mailbox growing: ${size} messages`);
    }
  }
}
```

**2. Implement Priority Queuing**

Handle urgent messages before normal ones:

```typescript
// Enhanced mailbox with priority lanes
class PriorityMailbox extends Mailbox {
  private urgentQueue: QueuedMessage[] = [];
  private normalQueue: QueuedMessage[] = [];
  private lowQueue: QueuedMessage[] = [];

  post(message: Message, priority: number = 0): void {
    const qm: QueuedMessage = {
      message,
      priority,
      receivedAt: new Date(),
      correlationId: message.correlationId,
    };

    if (priority >= 10) {
      this.urgentQueue.push(qm);
    } else if (priority >= 5) {
      this.normalQueue.push(qm);
    } else {
      this.lowQueue.push(qm);
    }

    // Enforce max size
    const total = this.urgentQueue.length + this.normalQueue.length + this.lowQueue.length;
    if (total > this.maxSize) {
      // Drop lowest priority messages first
      this.lowQueue.shift();
    }
  }

  async receive(
    pattern: (msg: Message) => boolean,
    timeoutMs?: number
  ): Promise<Message | null> {

    // Check urgent queue first
    let index = this.urgentQueue.findIndex(qm => pattern(qm.message));
    if (index !== -1) {
      return this.urgentQueue.splice(index, 1)[0].message;
    }

    // Then normal queue
    index = this.normalQueue.findIndex(qm => pattern(qm.message));
    if (index !== -1) {
      return this.normalQueue.splice(index, 1)[0].message;
    }

    // Finally low priority
    index = this.lowQueue.findIndex(qm => pattern(qm.message));
    if (index !== -1) {
      return this.lowQueue.splice(index, 1)[0].message;
    }

    // Wait for message or timeout (as before)
    // ...
  }
}

// Usage:
const actor = new MyActor('worker-1');
await actor.start();

// Low priority background task
actor.send({ type: 'cleanup', payload: {} });  // priority = 0

// High priority urgent message jumps the queue
actor.send({ type: 'shutdown', payload: {} });  // priority = 10
```

**3. Add Message Pattern Matching**

Allow actors to selectively handle messages:

```typescript
// Pattern matching DSL
type MessagePattern =
  | { type: string }
  | { type: string; payload: { [key: string]: unknown } }
  | ((msg: Message) => boolean);

class PatternMatcher {
  static matches(message: Message, pattern: MessagePattern): boolean {
    if (typeof pattern === 'function') {
      return pattern(message);
    }

    if ('type' in pattern && message.type !== pattern.type) {
      return false;
    }

    if ('payload' in pattern && pattern.payload) {
      return this.matchesPayload(message.payload, pattern.payload);
    }

    return true;
  }

  private static matchesPayload(actual: unknown, expected: unknown): boolean {
    if (typeof expected !== 'object' || expected === null) {
      return actual === expected;
    }

    if (typeof actual !== 'object' || actual === null) {
      return false;
    }

    for (const [key, value] of Object.entries(expected)) {
      if (!this.matchesPayload((actual as any)[key], value)) {
        return false;
      }
    }

    return true;
  }
}

// Usage in actor:
class TaskActor extends MailboxActor {
  protected shouldHandle(message: Message): boolean {
    // Only handle task-related messages when active
    if (this.state === 'active') {
      return PatternMatcher.matches(message, { type: 'eval' }) ||
             PatternMatcher.matches(message, { type: 'complete' });
    }

    // When completed, ignore task messages
    if (this.state === 'completed') {
      return false;  // Let them accumulate or drop
    }

    return true;
  }
}
```

**4. Implement Request-Response Pattern with Correlation**

Properly handle request-response over async mailboxes:

```typescript
// Request-response coordinator
class RequestResponseBroker {
  private pending: Map<string, {
    resolve: (response: Response) => void;
    reject: (error: Error) => void;
    timer: NodeJS.Timeout;
  }> = new Map();

  // Send request and wait for response
  async call(
    actor: MailboxActor,
    message: Message,
    timeoutMs: number = 5000
  ): Promise<Response> {

    const correlationId = message.id;

    return new Promise((resolve, reject) => {
      // Set timeout
      const timer = setTimeout(() => {
        this.pending.delete(correlationId);
        reject(new Error(`Request timeout after ${timeoutMs}ms`));
      }, timeoutMs);

      // Store pending request
      this.pending.set(correlationId, { resolve, reject, timer });

      // Send message (non-blocking)
      actor.mailbox.post(message);
    });
  }

  // Called by actor to send response
  respond(correlationId: string, response: Response): void {
    const pending = this.pending.get(correlationId);
    if (!pending) {
      console.warn(`No pending request for ${correlationId}`);
      return;
    }

    clearTimeout(pending.timer);
    pending.resolve(response);
    this.pending.delete(correlationId);
  }

  // Called by actor to send error
  respondError(correlationId: string, error: Error): void {
    const pending = this.pending.get(correlationId);
    if (!pending) return;

    clearTimeout(pending.timer);
    pending.reject(error);
    this.pending.delete(correlationId);
  }
}

// Usage:
const broker = new RequestResponseBroker();

// Async request-response
const response = await broker.call(
  actor,
  { id: 'msg-1', type: 'eval', payload: {} },
  5000
);
```

**5. Add Backpressure and Flow Control**

Prevent mailbox overflow when actors can't keep up:

```typescript
// Bounded mailbox with backpressure
class BoundedMailbox extends Mailbox {
  private blocked: boolean = false;

  post(message: Message, priority: number = 0): void {
    if (this.queue.length >= this.maxSize) {
      this.blocked = true;
      throw new Error(`Mailbox full: ${this.maxSize} messages (BACKPRESSURE)`);
    }

    super.post(message, priority);
  }

  // Check if mailbox can accept more messages
  canAccept(): boolean {
    return this.queue.length < this.maxSize * 0.8;  // 80% threshold
  }

  // Get backpressure signal
  isBackpressured(): boolean {
    return this.blocked || this.queue.length > this.maxSize * 0.8;
  }
}

// Sending actor checks backpressure
class BackpressureAwareActor extends MailboxActor {
  async sendTo(target: MailboxActor, message: Message): Promise<void> {
    // Check if target can accept message
    if (target.mailbox.isBackpressured()) {
      console.warn(`Target ${target.id} is backpressured, slowing down`);
      await new Promise(resolve => setTimeout(resolve, 1000));  // Wait 1s
    }

    await target.send(message);
  }
}
```

---

## Summary of Recommendations

### High Priority (Implement First)

1. **Supervision and Fault Tolerance**
   - Create `SupervisorActor` with restart strategies
   - Add health monitoring and circuit breakers
   - Implement retry policies for tasks

2. **State Machine Formalization**
   - Refactor `TaskNode` to use `gen_statem` pattern
   - Enforce valid state transitions
   - Add state entry/exit hooks

3. **Actor Monitoring and Linking**
   - Implement monitor/demonitor in Registry
   - Automatic cleanup on actor death
   - Send 'down' messages to monitoring actors

### Medium Priority (Next Phase)

4. **Process Groups**
   - Add group-based actor discovery
   - Implement broadcast to groups
   - Type-based actor pools

5. **Mailbox and Message Queuing**
   - Add explicit mailboxes to actors
   - Implement priority queuing
   - Add selective receive with pattern matching

6. **Call/Cast Distinction**
   - Separate synchronous calls from async casts
   - Add timeout handling for calls
   - Implement proper request-response correlation

### Low Priority (Future Work)

7. **Shared State Tables**
   - ETS-like shared memory for fast lookups
   - Pattern matching queries
   - Access control (public/protected/private)

8. **Distributed Registry**
   - Global registration across processes/machines
   - Automatic re-registration on failure
   - Consistency during network partitions

9. **Hot Code Swapping**
   - Support for upgrading actor implementations without downtime
   - State migration between versions

---

## Architectural Gaps Summary

| Erlang/BEAM Feature | Current tk-agents | Gap | Impact |
|---------------------|-------------------|-----|--------|
| Supervision trees | Registry (flat) | No hierarchy, no restart | Crashes are not recovered |
| Restart strategies | None | No automatic recovery | Manual intervention required |
| OTP behaviors | Implicit patterns | No enforced contracts | Inconsistent actor implementations |
| State machines | Switch statements | No transition validation | Invalid states possible |
| Process groups | None | No type-based discovery | Can't broadcast to actor types |
| Monitoring/linking | None | No death detection | Stale actor references |
| Async mailboxes | `await actor.send()` | Blocking, no queue | Callers wait for responses |
| Selective receive | Sequential processing | No prioritization | Urgent messages wait in line |
| Pattern matching | Type checks | No payload matching | Can't selectively handle messages |
| Fault isolation | Shared memory | Crashes affect all actors | No memory protection |

---

## Comparison to Current Design Philosophy

### What We Do Well (Keep These)

1. **TypeScript Type Safety**: Strong typing prevents many runtime errors
2. **Async/Await Ergonomics**: Easier to reason about than Erlang's receive loops
3. **Streaming Support**: `AsyncGenerator` for long-running operations
4. **Unified Actor Interface**: Simple `send()` contract
5. **Graph-Based Relationships**: Task/knowledge graph is more flexible than pure actor hierarchy

### What We Should Adopt from Erlang

1. **Let It Crash**: Stop defensive error handling, add supervision
2. **Explicit State Machines**: Formalize TaskNode state transitions
3. **Process Groups**: Enable type-based actor discovery
4. **Monitoring**: Detect and react to actor failures
5. **Mailboxes**: Decouple message sending from processing
6. **Fault Isolation**: Consider WebWorkers for true actor isolation

### Hybrid Approach Recommended

Don't try to replicate Erlang exactly in TypeScript. Instead:

- **Use Erlang patterns where they add value** (supervision, state machines, monitoring)
- **Keep TypeScript idioms where they're better** (async/await, type safety, promises)
- **Add Erlang-inspired features incrementally** (start with supervision, then mailboxes)
- **Maintain pragmatic tradeoffs** (single-process architecture is fine for MVP)

---

## Next Steps

1. **Prototype SupervisorActor** with `one_for_one` strategy
2. **Refactor TaskNode** to use explicit state machine pattern
3. **Add monitoring to Registry** for actor death detection
4. **Write BDD specs** for supervision scenarios
5. **Benchmark mailbox overhead** vs current `await` approach
6. **Design distributed registry** for multi-process future

---

## References

- Erlang OTP Design Principles: https://www.erlang.org/doc/design_principles/des_princ.html
- gen_server Behavior: https://www.erlang.org/doc/man/gen_server.html
- gen_statem Behavior: https://www.erlang.org/doc/man/gen_statem.html
- Supervisor Behavior: https://www.erlang.org/doc/man/supervisor.html
- Learn You Some Erlang (Supervisors): http://learnyousomeerlang.com/supervisors
- Designing for Scalability with Erlang/OTP (Book by Francesco Cesarini & Steve Vinoski)
