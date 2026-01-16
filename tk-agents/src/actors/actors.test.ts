// Test scenarios for actors

import { describe, test, expect, beforeEach } from "bun:test";
import { Registry } from "./registry";
import { BashActor, createBashActor } from "./bash";
import { MockActor, createEchoMock, createClaudeMock, createFailingMock } from "./mock";
import { HumanActor, createHumanActor } from "./human";
import { createMessage } from "./base";

describe("Registry", () => {
  let registry: Registry;

  beforeEach(() => {
    registry = new Registry();
  });

  test("register and retrieve actor", () => {
    const actor = createEchoMock("test-1");
    registry.register(actor);

    expect(registry.has("test-1")).toBe(true);
    expect(registry.get("test-1")).toBe(actor);
  });

  test("list registered actors", () => {
    registry.register(createEchoMock("actor-1"));
    registry.register(createEchoMock("actor-2"));

    const list = registry.list();
    expect(list.length).toBe(2);
    expect(list.map(a => a.id)).toContain("actor-1");
    expect(list.map(a => a.id)).toContain("actor-2");
  });

  test("unregister actor", () => {
    registry.register(createEchoMock("temp"));
    expect(registry.has("temp")).toBe(true);

    registry.unregister("temp");
    expect(registry.has("temp")).toBe(false);
  });

  test("send message to actor", async () => {
    const actor = createEchoMock("echo");
    registry.register(actor);

    const response = await registry.sendTo("echo", "test", { hello: "world" });

    expect(response.success).toBe(true);
    expect(response.data).toEqual({ hello: "world" });
  });

  test("send to non-existent actor returns error", async () => {
    const response = await registry.sendTo("not-found", "test", {});

    expect(response.success).toBe(false);
    expect(response.error).toContain("not found");
  });

  test("tracks message count", async () => {
    registry.register(createEchoMock("counter"));

    await registry.sendTo("counter", "msg", {});
    await registry.sendTo("counter", "msg", {});
    await registry.sendTo("counter", "msg", {});

    const list = registry.list();
    const actor = list.find(a => a.id === "counter");
    expect(actor?.messageCount).toBe(3);
  });
});

describe("BashActor", () => {
  test("executes simple command", async () => {
    const actor = createBashActor({ id: "bash-1" });
    const response = await actor.receive(createMessage("exec", "echo hello"));

    expect(response.success).toBe(true);
    expect(response.data).toBe("hello\n");
  });

  test("captures exit code on failure", async () => {
    const actor = createBashActor({ id: "bash-2" });
    const response = await actor.receive(createMessage("exec", "exit 1"));

    expect(response.success).toBe(false);
  });

  test("respects timeout", async () => {
    const actor = createBashActor({ id: "bash-3", timeout: 100 });
    const response = await actor.receive(createMessage("exec", "sleep 5"));

    expect(response.success).toBe(false);
    expect(response.error).toContain("timed out");
  });

  test("uses custom cwd", async () => {
    const actor = createBashActor({ id: "bash-4", cwd: "/tmp" });
    const response = await actor.receive(createMessage("exec", "pwd"));

    expect(response.success).toBe(true);
    // macOS: /tmp -> /private/tmp
    expect(["/tmp\n", "/private/tmp\n"]).toContain(response.data);
  });

  test("uses custom env", async () => {
    const actor = createBashActor({
      id: "bash-5",
      env: { MY_VAR: "test-value" },
    });
    const response = await actor.receive(createMessage("exec", "echo $MY_VAR"));

    expect(response.success).toBe(true);
    expect(response.data).toBe("test-value\n");
  });
});

describe("MockActor", () => {
  test("echo mock returns payload", async () => {
    const actor = createEchoMock("echo");
    const response = await actor.receive(createMessage("test", { foo: "bar" }));

    expect(response.success).toBe(true);
    expect(response.data).toEqual({ foo: "bar" });
  });

  test("failing mock returns error", async () => {
    const actor = createFailingMock("fail", "Something went wrong");
    const response = await actor.receive(createMessage("test", {}));

    expect(response.success).toBe(false);
    expect(response.error).toBe("Something went wrong");
  });

  test("tracks received messages", async () => {
    const actor = createEchoMock("tracker");

    await actor.receive(createMessage("first", { n: 1 }));
    await actor.receive(createMessage("second", { n: 2 }));

    expect(actor.receivedMessages.length).toBe(2);
    expect(actor.receivedMessages[0].type).toBe("first");
    expect(actor.receivedMessages[1].type).toBe("second");
  });

  test("claude mock simulates agent responses", async () => {
    const actor = createClaudeMock("claude", [
      "I'll help you with that.",
      "Here's the plan...",
    ]);

    const r1 = await actor.receive(createMessage("prompt", "Help me"));
    const r2 = await actor.receive(createMessage("prompt", "What's next?"));

    expect(r1.success).toBe(true);
    expect(r1.data).toBe("I'll help you with that.");
    expect(r1.metadata?.sessionId).toBeDefined();

    expect(r2.success).toBe(true);
    expect(r2.data).toBe("Here's the plan...");
  });

  test("response queue exhaustion uses default", async () => {
    const actor = new MockActor({
      id: "queue",
      responses: [
        { response: { success: true, data: "first" } },
      ],
    });

    const r1 = await actor.receive(createMessage("test", {}));
    const r2 = await actor.receive(createMessage("test", { x: 1 }));

    expect(r1.data).toBe("first");
    expect(r2.data).toEqual({ echo: { x: 1 } }); // Default echo
  });
});

describe("Scenario: Task Delegation", () => {
  test("coordinator delegates to worker", async () => {
    const registry = new Registry();

    // Coordinator decides what to do
    const coordinator = new MockActor({
      id: "coordinator",
      type: "agent",
      handler: (msg) => ({
        response: {
          success: true,
          data: {
            action: "delegate",
            to: "worker",
            task: `Process: ${msg.payload}`,
          },
        },
      }),
    });

    // Worker executes
    const worker = new MockActor({
      id: "worker",
      type: "deterministic",
      handler: (msg) => ({
        response: {
          success: true,
          data: { completed: true, input: msg.payload },
        },
      }),
    });

    registry.register(coordinator);
    registry.register(worker);

    // Simulate coordination flow
    const decision = await registry.sendTo("coordinator", "plan", "build feature X");
    expect(decision.success).toBe(true);

    const delegationInfo = decision.data as { to: string; task: string };
    const result = await registry.sendTo(delegationInfo.to, "execute", delegationInfo.task);

    expect(result.success).toBe(true);
    expect((result.data as { completed: boolean }).completed).toBe(true);
  });
});

describe("Scenario: Multi-turn Conversation", () => {
  test("simulates resumable session", async () => {
    // Track conversation history
    const history: string[] = [];

    const agent = new MockActor({
      id: "conversational",
      type: "agent",
      handler: (msg) => {
        history.push(msg.payload as string);
        return {
          response: {
            success: true,
            data: `Received ${history.length} messages. Last: "${msg.payload}"`,
            metadata: { sessionId: "session-123", turnCount: history.length },
          },
        };
      },
    });

    const r1 = await agent.receive(createMessage("user", "Hello"));
    const r2 = await agent.receive(createMessage("user", "How are you?"));
    const r3 = await agent.receive(createMessage("user", "Goodbye"));

    expect(r1.metadata?.turnCount).toBe(1);
    expect(r2.metadata?.turnCount).toBe(2);
    expect(r3.metadata?.turnCount).toBe(3);
    expect(history).toEqual(["Hello", "How are you?", "Goodbye"]);
  });
});

describe("Scenario: Pipeline", () => {
  test("simulates A -> B chain", async () => {
    const registry = new Registry();

    // Stage A: Planner
    const planner = new MockActor({
      id: "planner",
      type: "agent",
      handler: (msg) => ({
        response: {
          success: true,
          data: {
            plan: [
              { step: 1, action: "research", input: msg.payload },
              { step: 2, action: "implement" },
              { step: 3, action: "test" },
            ],
          },
        },
      }),
    });

    // Stage B: Executor
    const executor = new MockActor({
      id: "executor",
      type: "agent",
      handler: (msg) => {
        const plan = (msg.payload as { plan: unknown[] }).plan;
        return {
          response: {
            success: true,
            data: {
              executed: true,
              stepsCompleted: plan.length,
            },
          },
        };
      },
    });

    registry.register(planner);
    registry.register(executor);

    // Pipeline flow
    const planResult = await registry.sendTo("planner", "plan", "Build login feature");
    expect(planResult.success).toBe(true);

    const execResult = await registry.sendTo("executor", "execute", planResult.data);
    expect(execResult.success).toBe(true);
    expect((execResult.data as { stepsCompleted: number }).stepsCompleted).toBe(3);
  });
});

describe("Mailbox Integration", () => {
  test("messages delivered via mailboxes", async () => {
    const registry = new Registry();
    const actor = createEchoMock("mailbox-test");
    registry.register(actor);

    // Send message - should go through mailbox
    const response = await registry.sendTo("mailbox-test", "test", { data: "hello" });

    // Response should arrive (promise resolves after processing)
    expect(response.success).toBe(true);
  });

  test("multiple messages delivered in FIFO order", async () => {
    const registry = new Registry();
    const receivedOrder: number[] = [];

    const actor = new MockActor({
      id: "fifo-test",
      handler: (msg) => {
        receivedOrder.push(msg.payload as number);
        return { response: { success: true, data: { received: msg.payload } } };
      },
    });

    registry.register(actor);

    // Send multiple messages quickly
    const promises = [
      registry.sendTo("fifo-test", "msg", 1),
      registry.sendTo("fifo-test", "msg", 2),
      registry.sendTo("fifo-test", "msg", 3),
    ];

    await Promise.all(promises);

    // Wait for processing
    await new Promise(r => setTimeout(r, 100));

    // Verify FIFO order
    expect(receivedOrder).toEqual([1, 2, 3]);
  });

  test("mailbox status can be queried", async () => {
    const registry = new Registry();
    const actor = createEchoMock("status-test");
    registry.register(actor);

    // Send a message
    await registry.sendTo("status-test", "test", {});

    // Check mailbox status
    const statusResponse = await registry.getMailboxStatus("status-test");

    expect(statusResponse.success).toBe(true);
    expect(statusResponse.data).toHaveProperty("exists");
  });

  test("mailbox full scenario handled gracefully", async () => {
    const registry = new Registry();
    const slowActor = new MockActor({
      id: "slow-actor",
      handler: async (msg) => {
        // Slow processing to fill mailbox
        await new Promise(r => setTimeout(r, 100));
        return { response: { success: true, data: msg.payload } };
      },
    });

    registry.register(slowActor);

    // Try to overwhelm the mailbox
    // Default mailbox size is 1000, so we'd need many messages
    // Instead, test that messages are queued properly
    const promises = [];
    for (let i = 0; i < 10; i++) {
      promises.push(registry.sendTo("slow-actor", "work", { n: i }));
    }

    const results = await Promise.all(promises);

    // All should enqueue successfully (mailbox not full)
    results.forEach(r => {
      expect(r.success).toBe(true);
    });

    // Clean up - wait for processing to complete
    await new Promise(r => setTimeout(r, 200));
  });
});

describe("Death Detection", () => {
  test("emits actor_died on exception", async () => {
    const registry = new Registry();

    // Create actor that throws exception
    const crashingActor = new MockActor({
      id: "crasher",
      handler: () => {
        throw new Error("Actor crashed!");
      },
    });

    registry.register(crashingActor);

    // Listen for actor_died event
    let deathEvent: unknown = null;
    registry.once('actor_died', (event) => {
      deathEvent = event;
    });

    // Send message that triggers exception
    const response = await registry.sendTo("crasher", "test", {});

    // Verify response indicates failure
    expect(response.success).toBe(false);
    expect(response.error).toContain("died");
    expect(response.error).toContain("Actor crashed!");

    // Verify actor_died event was emitted
    expect(deathEvent).toBeDefined();
    expect((deathEvent as { actorId: string }).actorId).toBe("crasher");
    expect((deathEvent as { reason: string }).reason).toBe("exception");
    expect((deathEvent as { error: string }).error).toContain("Actor crashed!");

    // Verify actor was removed from registry
    expect(registry.has("crasher")).toBe(false);
  });

  test("handles ping message", async () => {
    const registry = new Registry();
    const actor = createEchoMock("pinger");
    registry.register(actor);

    const response = await registry.send(actor.id, {
      type: 'ping',
      id: 'test-ping',
      payload: {},
    });

    expect(response.success).toBe(true);
    expect((response.data as { alive: boolean }).alive).toBe(true);
    expect((response.data as { timestamp: number }).timestamp).toBeGreaterThan(0);
  });

  test("startHeartbeat monitors actor health", async () => {
    const registry = new Registry();
    const actor = createEchoMock("monitored");
    registry.register(actor);

    // Start heartbeat with short interval
    registry.startHeartbeat("monitored", 100);

    // Wait for at least one heartbeat cycle
    await new Promise(r => setTimeout(r, 150));

    // Actor should have received at least one ping
    const pings = actor.receivedMessages.filter(msg => msg.type === 'ping');
    expect(pings.length).toBeGreaterThanOrEqual(1);

    // Stop heartbeat
    registry.stopHeartbeat("monitored");
  });

  test("heartbeat detects unresponsive actor", async () => {
    const registry = new Registry();

    // Create actor that fails on ping
    let callCount = 0;
    const unresponsiveActor = new MockActor({
      id: "unresponsive",
      handler: (msg) => {
        if (msg.type === 'ping') {
          callCount++;
          if (callCount > 1) {
            throw new Error("Actor became unresponsive");
          }
        }
        return { response: { success: true, data: {} } };
      },
    });

    registry.register(unresponsiveActor);

    // Listen for actor_died event
    let deathEvent: unknown = null;
    registry.once('actor_died', (event) => {
      deathEvent = event;
    });

    // Start heartbeat with short interval
    registry.startHeartbeat("unresponsive", 100);

    // Wait for heartbeat to detect failure
    await new Promise(r => setTimeout(r, 300));

    // Verify actor_died event was emitted
    expect(deathEvent).toBeDefined();
    expect((deathEvent as { actorId: string }).actorId).toBe("unresponsive");
    // When actor throws during ping, it's detected as 'exception', not 'heartbeat_timeout'
    expect((deathEvent as { reason: string }).reason).toBe("exception");

    // Verify actor was removed from registry
    expect(registry.has("unresponsive")).toBe(false);
  });

  test("stopHeartbeat prevents further monitoring", async () => {
    const registry = new Registry();
    const actor = createEchoMock("stopped");
    registry.register(actor);

    // Start then immediately stop heartbeat
    registry.startHeartbeat("stopped", 100);
    registry.stopHeartbeat("stopped");

    // Wait to ensure no pings occur
    await new Promise(r => setTimeout(r, 250));

    // Actor should have received no pings
    const pings = actor.receivedMessages.filter(msg => msg.type === 'ping');
    expect(pings.length).toBe(0);
  });

  test("unregister cleans up heartbeat", async () => {
    const registry = new Registry();
    const actor = createEchoMock("cleanup");
    registry.register(actor);

    // Start heartbeat
    registry.startHeartbeat("cleanup", 100);

    // Unregister should clean up
    registry.unregister("cleanup");

    // Wait to ensure no more pings occur
    const initialPings = actor.receivedMessages.filter(msg => msg.type === 'ping').length;
    await new Promise(r => setTimeout(r, 250));

    // No new pings should arrive
    const finalPings = actor.receivedMessages.filter(msg => msg.type === 'ping').length;
    expect(finalPings).toBe(initialPings);
  });

  test("startHeartbeat throws for non-existent actor", () => {
    const registry = new Registry();

    expect(() => {
      registry.startHeartbeat("not-found");
    }).toThrow("Cannot start heartbeat: Actor not found not-found");
  });
});

describe("Structured Errors", () => {
  test("actor can return structured error", async () => {
    const registry = new Registry();

    // Create actor that returns structured error
    const { validationError } = await import("./errors");
    const actor = new MockActor({
      id: "validator",
      handler: (msg) => {
        if (!msg.payload || typeof msg.payload !== 'object') {
          return {
            response: {
              success: false,
              error: validationError("Payload must be an object", {
                received: typeof msg.payload,
              }),
            },
          };
        }
        return {
          response: { success: true, data: { validated: true } },
        };
      },
    });

    registry.register(actor);

    // Send invalid payload
    const response = await registry.sendTo("validator", "validate", null);

    expect(response.success).toBe(false);
    expect(response.error).toBeDefined();

    // Check if it's a structured error
    if (typeof response.error === 'object' && response.error !== null && 'category' in response.error) {
      const structuredError = response.error as { category: string; retryable: boolean };
      expect(structuredError.category).toBe("validation");
      expect(structuredError.retryable).toBe(false);
    }
  });

  test("actor can return transient error", async () => {
    const registry = new Registry();

    const { transientError } = await import("./errors");
    let attemptCount = 0;

    const actor = new MockActor({
      id: "unreliable",
      handler: () => {
        attemptCount++;
        if (attemptCount < 3) {
          return {
            response: {
              success: false,
              error: transientError("Service temporarily unavailable", undefined, {
                attemptCount,
              }),
            },
          };
        }
        return {
          response: { success: true, data: { result: "success" } },
        };
      },
    });

    registry.register(actor);

    // First attempt fails
    const response1 = await registry.sendTo("unreliable", "call", {});
    expect(response1.success).toBe(false);
    if (typeof response1.error === 'object' && response1.error !== null && 'retryable' in response1.error) {
      const error = response1.error as { retryable: boolean };
      expect(error.retryable).toBe(true);
    }

    // Second attempt fails
    const response2 = await registry.sendTo("unreliable", "call", {});
    expect(response2.success).toBe(false);

    // Third attempt succeeds
    const response3 = await registry.sendTo("unreliable", "call", {});
    expect(response3.success).toBe(true);
  });

  test("error helpers create correct error types", () => {
    const { validationError, transientError, permanentError, fatalError } = require("./errors");

    const valError = validationError("Invalid input");
    expect(valError.category).toBe("validation");
    expect(valError.retryable).toBe(false);

    const tempError = transientError("Timeout");
    expect(tempError.category).toBe("transient");
    expect(tempError.retryable).toBe(true);

    const permError = permanentError("Permission denied");
    expect(permError.category).toBe("permanent");
    expect(permError.retryable).toBe(false);

    const fatal = fatalError("Out of memory");
    expect(fatal.category).toBe("fatal");
    expect(fatal.retryable).toBe(false);
  });
});

describe("HumanActor", () => {
  test("creates human actor with basic config", () => {
    const human = createHumanActor({ id: "human-1" });

    expect(human.id).toBe("human-1");
    expect(human.type).toBe("agent");
  });

  test("responds with awaiting_human_response by default", async () => {
    const human = createHumanActor({ id: "human-1" });

    const response = await human.receive(createMessage("question", { text: "What should I do?" }));

    expect(response.success).toBe(true);
    expect(response.data).toBeDefined();
    const data = response.data as { status: string; message: string };
    expect(data.status).toBe("awaiting_human_response");
    expect(data.message).toBe("Human input required");
  });

  test("handles ping message", async () => {
    const human = createHumanActor({ id: "human-1" });

    const response = await human.receive(createMessage("ping", {}));

    expect(response.success).toBe(true);
    const data = response.data as { alive: boolean };
    expect(data.alive).toBe(true);
  });

  test("uses callback when provided", async () => {
    const human = createHumanActor({
      id: "human-1",
      onInputNeeded: async (msg) => {
        // Simulate human providing input
        return "Yes, proceed with the plan";
      },
    });

    const response = await human.receive(createMessage("approval", { action: "deploy" }));

    expect(response.success).toBe(true);
    const data = response.data as { humanResponse: string };
    expect(data.humanResponse).toBe("Yes, proceed with the plan");
  });

  test("tracks pending messages", async () => {
    const human = createHumanActor({ id: "human-1" });

    expect(human.getPendingMessages().length).toBe(0);

    await human.receive(createMessage("question", { text: "Question 1" }));
    await human.receive(createMessage("question", { text: "Question 2" }));

    expect(human.getPendingMessages().length).toBe(2);

    human.clearPendingMessages();
    expect(human.getPendingMessages().length).toBe(0);
  });

  test("can be registered in registry", async () => {
    const registry = new Registry();
    const human = createHumanActor({ id: "human-approver" });

    registry.register(human);

    expect(registry.has("human-approver")).toBe(true);

    const response = await registry.sendTo("human-approver", "decision", { action: "merge PR" });
    expect(response.success).toBe(true);
  });

  test("works in multi-actor workflow", async () => {
    const registry = new Registry();

    // AI agent makes a plan
    const planner = createClaudeMock("planner", ["Here's my plan: deploy to production"]);

    // Human reviews and approves
    const human = createHumanActor({
      id: "human-reviewer",
      onInputNeeded: async (msg) => "Approved",
    });

    // Execution actor carries out approved plan
    const executor = createEchoMock("executor");

    registry.register(planner);
    registry.register(human);
    registry.register(executor);

    // Workflow: planner → human → executor
    const plan = await registry.sendTo("planner", "create_plan", {});
    expect(plan.success).toBe(true);

    const approval = await registry.sendTo("human-reviewer", "review", { plan: plan.data });
    expect(approval.success).toBe(true);
    const approvalData = approval.data as { humanResponse: string };
    expect(approvalData.humanResponse).toBe("Approved");

    const execution = await registry.sendTo("executor", "execute", { plan: plan.data });
    expect(execution.success).toBe(true);
  });
});
