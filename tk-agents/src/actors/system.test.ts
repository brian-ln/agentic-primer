// Tests for System class (Phase 1 - Hewitt Actor Model)

import { test, expect } from "bun:test";
import { System } from "./system";
import type { Actor, Message, Response } from "./base";

// Simple test actor
class TestActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === "ping") {
      return { success: true, data: { alive: true } };
    }
    return { success: true, data: { received: message.payload } };
  }
}

test("System - registers and lists actors", async () => {
  const system = new System();
  const actor = new TestActor("test-1");

  const regResponse = await system.receive({
    id: "reg-1",
    type: "register",
    payload: { actor },
  });

  expect(regResponse.success).toBe(true);

  const listResponse = await system.receive({
    id: "list-1",
    type: "list",
    payload: {},
  });

  expect(listResponse.success).toBe(true);
  expect((listResponse.data as any).actors).toHaveLength(1);
  expect((listResponse.data as any).actors[0].id).toBe("test-1");
});

test("System - routes messages to actors", async () => {
  const system = new System();
  const actor = new TestActor("test-1");

  await system.receive({
    id: "reg-1",
    type: "register",
    payload: { actor },
  });

  const response = await system.receive({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "test-1",
      message: {
        id: "msg-1",
        type: "echo",
        payload: "hello",
      },
    },
  });

  expect(response.success).toBe(true);
  expect((response.data as any).received).toBe("hello");
});

test("System - handles ping", async () => {
  const system = new System();

  const response = await system.receive({
    id: "ping-1",
    type: "ping",
    payload: {},
  });

  expect(response.success).toBe(true);
  expect((response.data as any).alive).toBe(true);
});

test("System - unregisters actors", async () => {
  const system = new System();
  const actor = new TestActor("test-1");

  await system.receive({
    id: "reg-1",
    type: "register",
    payload: { actor },
  });

  const unregResponse = await system.receive({
    id: "unreg-1",
    type: "unregister",
    payload: { actorId: "test-1" },
  });

  expect(unregResponse.success).toBe(true);

  const listResponse = await system.receive({
    id: "list-1",
    type: "list",
    payload: {},
  });

  expect((listResponse.data as any).actors).toHaveLength(0);
});

test("System - returns error for unknown actor", async () => {
  const system = new System();

  const response = await system.receive({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "nonexistent",
      message: {
        id: "msg-1",
        type: "test",
        payload: {},
      },
    },
  });

  expect(response.success).toBe(false);
  expect(response.error).toContain("Actor not found");
});

test("System - convenience sendTo method", async () => {
  const system = new System();
  const actor = new TestActor("test-1");

  system.register(actor);

  const response = await system.sendTo("test-1", {
    id: "msg-1",
    type: "echo",
    payload: "test",
  });

  expect(response.success).toBe(true);
  expect((response.data as any).received).toBe("test");
});
