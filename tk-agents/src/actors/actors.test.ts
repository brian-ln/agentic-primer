// Test scenarios for actors

import { describe, test, expect, beforeEach } from "bun:test";
import { Registry } from "./registry";
import { BashActor, createBashActor } from "./bash";
import { MockActor, createEchoMock, createClaudeMock, createFailingMock } from "./mock";
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
    const response = await actor.send(createMessage("exec", "echo hello"));

    expect(response.success).toBe(true);
    expect(response.data).toBe("hello\n");
  });

  test("captures exit code on failure", async () => {
    const actor = createBashActor({ id: "bash-2" });
    const response = await actor.send(createMessage("exec", "exit 1"));

    expect(response.success).toBe(false);
  });

  test("respects timeout", async () => {
    const actor = createBashActor({ id: "bash-3", timeout: 100 });
    const response = await actor.send(createMessage("exec", "sleep 5"));

    expect(response.success).toBe(false);
    expect(response.error).toContain("timed out");
  });

  test("uses custom cwd", async () => {
    const actor = createBashActor({ id: "bash-4", cwd: "/tmp" });
    const response = await actor.send(createMessage("exec", "pwd"));

    expect(response.success).toBe(true);
    // macOS: /tmp -> /private/tmp
    expect(["/tmp\n", "/private/tmp\n"]).toContain(response.data);
  });

  test("uses custom env", async () => {
    const actor = createBashActor({
      id: "bash-5",
      env: { MY_VAR: "test-value" },
    });
    const response = await actor.send(createMessage("exec", "echo $MY_VAR"));

    expect(response.success).toBe(true);
    expect(response.data).toBe("test-value\n");
  });
});

describe("MockActor", () => {
  test("echo mock returns payload", async () => {
    const actor = createEchoMock("echo");
    const response = await actor.send(createMessage("test", { foo: "bar" }));

    expect(response.success).toBe(true);
    expect(response.data).toEqual({ foo: "bar" });
  });

  test("failing mock returns error", async () => {
    const actor = createFailingMock("fail", "Something went wrong");
    const response = await actor.send(createMessage("test", {}));

    expect(response.success).toBe(false);
    expect(response.error).toBe("Something went wrong");
  });

  test("tracks received messages", async () => {
    const actor = createEchoMock("tracker");

    await actor.send(createMessage("first", { n: 1 }));
    await actor.send(createMessage("second", { n: 2 }));

    expect(actor.receivedMessages.length).toBe(2);
    expect(actor.receivedMessages[0].type).toBe("first");
    expect(actor.receivedMessages[1].type).toBe("second");
  });

  test("claude mock simulates agent responses", async () => {
    const actor = createClaudeMock("claude", [
      "I'll help you with that.",
      "Here's the plan...",
    ]);

    const r1 = await actor.send(createMessage("prompt", "Help me"));
    const r2 = await actor.send(createMessage("prompt", "What's next?"));

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

    const r1 = await actor.send(createMessage("test", {}));
    const r2 = await actor.send(createMessage("test", { x: 1 }));

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

    const r1 = await agent.send(createMessage("user", "Hello"));
    const r2 = await agent.send(createMessage("user", "How are you?"));
    const r3 = await agent.send(createMessage("user", "Goodbye"));

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
