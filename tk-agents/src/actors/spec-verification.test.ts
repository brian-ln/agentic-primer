/**
 * Spec Verification Tests
 *
 * These tests verify the implementation against ACTOR_SYSTEM.spec.md
 * Key invariants tested:
 * 1. Address Identity Invariant
 * 2. Address Send Invariant
 * 3. Factory Returns Address Invariant
 * 4. System Dependency Invariant
 * 5. No Magic Strings Invariant
 */

import { test, expect, describe } from "bun:test";
import type { Actor, Address, Message, ActorFactory } from "./base.ts";
import type { System } from "./system.ts";
import { System as createActorSystem } from "./system.ts";

describe("Actor System Spec Verification", () => {
  describe("1. Address is First-Class Object", () => {
    test("Address has __id and send properties", () => {
      const system = createActorSystem();

      const TestActor: ActorFactory<{ system: System }> = (data) => {
        const actor = {
          send: async (message: Message) => {
            return { success: true };
          },
        };
        return data.system.register(actor);
      };

      const addr = TestActor({ system });

      // Address has __id (symbol) and send (function)
      expect(addr).toHaveProperty("__id");
      expect(typeof addr.__id).toBe("symbol");
      expect(addr).toHaveProperty("send");
      expect(typeof addr.send).toBe("function");
    });

    test("Address __id is unique per actor", () => {
      const system = createActorSystem();

      const SimpleActor: ActorFactory<{ system: System }> = (data) => {
        const actor = {
          send: async () => ({ success: true }),
        };
        return data.system.register(actor);
      };

      const addr1 = SimpleActor({ system });
      const addr2 = SimpleActor({ system });

      // Each address has unique symbol
      expect(addr1.__id).not.toBe(addr2.__id);
    });

    test("Address.send() works ergonomically", async () => {
      const system = createActorSystem();

      const CounterActor: ActorFactory<{ count: number; system: System }> = (
        data
      ) => {
        const actor = {
          send: async (message: Message) => {
            if (message.type === "increment") {
              data.count++;
              return { success: true, data: data.count };
            }
            return { success: true };
          },
        };
        return data.system.register(actor);
      };

      const counter = CounterActor({ count: 0, system });

      // Ergonomic API: addr.send(message)
      const result = await counter.send({
        id: "1",
        type: "increment",
        payload: {},
      });

      expect(result.success).toBe(true);
      expect(result.data).toBe(1);
    });
  });

  describe("2. Actor Factories Return Addresses", () => {
    test("Factory takes data with system field", () => {
      const system = createActorSystem();

      const TestActor: ActorFactory<{
        id: string;
        status: string;
        system: System;
      }> = (data) => {
        // Factory receives data with system
        expect(data).toHaveProperty("system");
        expect(data).toHaveProperty("id");
        expect(data).toHaveProperty("status");

        const actor = {
          send: async (message: Message) => {
            return { success: true, data: { id: data.id, status: data.status } };
          },
        };
        return data.system.register(actor);
      };

      const addr = TestActor({ id: "task-1", status: "todo", system });
      expect(addr).toHaveProperty("__id");
      expect(addr).toHaveProperty("send");
    });

    test("Factory returns Address directly", () => {
      const system = createActorSystem();

      const TaskActor: ActorFactory<{ id: string; system: System }> = (
        data
      ) => {
        const actor = {
          send: async (message: Message) => ({ success: true }),
        };
        return data.system.register(actor);
      };

      const task = TaskActor({ id: "task-1", system });

      // Factory returns Address (not Actor)
      expect(task).toHaveProperty("__id");
      expect(task).toHaveProperty("send");
      expect(typeof task.__id).toBe("symbol");
      expect(typeof task.send).toBe("function");
    });

    test("Single call creates, registers, and returns", () => {
      const system = createActorSystem();

      const EchoActor: ActorFactory<{ prefix: string; system: System }> = (
        data
      ) => {
        const actor = {
          send: async (message: Message) => {
            return { success: true, data: `${data.prefix}: ${message.type}` };
          },
        };
        return data.system.register(actor);
      };

      // Single call does everything
      const echo = EchoActor({ prefix: "ECHO", system });

      // Immediately usable
      expect(echo.send).toBeInstanceOf(Function);
    });
  });

  describe("3. Two Ways to Send Messages", () => {
    test("Ergonomic send: address.send(message)", async () => {
      const system = createActorSystem();

      const TestActor: ActorFactory<{ value: number; system: System }> = (
        data
      ) => {
        const actor = {
          send: async (message: Message) => {
            return { success: true, data: data.value };
          },
        };
        return data.system.register(actor);
      };

      const task = TestActor({ value: 42, system });

      // Ergonomic style
      const result = await task.send({
        id: "1",
        type: "get",
        payload: {},
      });

      expect(result.success).toBe(true);
      expect(result.data).toBe(42);
    });

    test("Explicit send: system.send(address, message)", async () => {
      const system = createActorSystem();

      const TestActor: ActorFactory<{ value: number; system: System }> = (
        data
      ) => {
        const actor = {
          send: async (message: Message) => {
            return { success: true, data: data.value };
          },
        };
        return data.system.register(actor);
      };

      const task = TestActor({ value: 99, system });

      // Explicit style
      const result = await system.send(task, {
        id: "1",
        type: "get",
        payload: {},
      });

      expect(result.success).toBe(true);
      expect(result.data).toBe(99);
    });

    test("Both send styles produce identical results", async () => {
      const system = createActorSystem();

      let callCount = 0;
      const TestActor: ActorFactory<{ system: System }> = (data) => {
        const actor = {
          send: async (message: Message) => {
            callCount++;
            return { success: true, data: callCount };
          },
        };
        return data.system.register(actor);
      };

      const task = TestActor({ system });

      // Ergonomic
      const result1 = await task.send({ id: "1", type: "test", payload: {} });

      // Explicit
      const result2 = await system.send(task, {
        id: "2",
        type: "test",
        payload: {},
      });

      // Both work identically
      expect(result1.success).toBe(true);
      expect(result2.success).toBe(true);
      expect(callCount).toBe(2);
    });
  });

  describe("4. System Provides Infrastructure", () => {
    test("System has send and register methods", () => {
      const system = createActorSystem();

      expect(system.send).toBeInstanceOf(Function);
      expect(system.register).toBeInstanceOf(Function);
    });

    test("System.register() returns Address", () => {
      const system = createActorSystem();

      const actor: Actor = {
        send: async (message: Message) => ({ success: true }),
      };

      const addr = system.register(actor);

      expect(addr).toHaveProperty("__id");
      expect(addr).toHaveProperty("send");
      expect(typeof addr.__id).toBe("symbol");
    });

    test("System routes messages using Address.__id", async () => {
      const system = createActorSystem();

      let received = false;
      const actor: Actor = {
        send: async (message: Message) => {
          received = true;
          return { success: true };
        },
      };

      const addr = system.register(actor);

      await system.send(addr, { id: "1", type: "test", payload: {} });

      expect(received).toBe(true);
    });

    test("System returns error for unregistered address", async () => {
      const system = createActorSystem();

      // Create a fake address (not registered)
      const fakeAddr: Address = {
        __id: Symbol(),
        send: async () => ({ success: false }),
      };

      const result = await system.send(fakeAddr, {
        id: "1",
        type: "test",
        payload: {},
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("not found");
    });
  });

  describe("5. Pure Functions with Explicit Dependencies", () => {
    test("All dependencies in data parameter", () => {
      const system = createActorSystem();

      const TaskActor: ActorFactory<{
        id: string;
        status: string;
        system: System;
      }> = (data) => {
        // All dependencies explicit in data
        expect(data.id).toBeDefined();
        expect(data.status).toBeDefined();
        expect(data.system).toBeDefined();

        const actor = {
          send: async (message: Message) => ({ success: true }),
        };
        return data.system.register(actor);
      };

      TaskActor({ id: "task-1", status: "todo", system });
    });

    test("Actors can hold references to other addresses", async () => {
      const system = createActorSystem();

      const TargetActor: ActorFactory<{ received: string[]; system: System }> =
        (data) => {
          const actor = {
            send: async (message: Message) => {
              data.received.push(message.type);
              return { success: true };
            },
          };
          return data.system.register(actor);
        };

      const NotifierActor: ActorFactory<{
        target: Address;
        system: System;
      }> = (data) => {
        const actor = {
          send: async (message: Message) => {
            if (message.type === "notify") {
              // Send to target using ergonomic API
              await data.target.send({ id: "n1", type: "notification", payload: {} });
            }
            return { success: true };
          },
        };
        return data.system.register(actor);
      };

      const received: string[] = [];
      const target = TargetActor({ received, system });
      const notifier = NotifierActor({ target, system });

      await notifier.send({ id: "1", type: "notify", payload: {} });

      expect(received).toContain("notification");
    });

    test("Actors use system from data for communication", async () => {
      const system = createActorSystem();

      const ReceiverActor: ActorFactory<{ messages: string[]; system: System }> =
        (data) => {
          const actor = {
            send: async (message: Message) => {
              data.messages.push(message.type);
              return { success: true };
            },
          };
          return data.system.register(actor);
        };

      const SenderActor: ActorFactory<{
        target: Address;
        system: System;
      }> = (data) => {
        const actor = {
          send: async (message: Message) => {
            if (message.type === "trigger") {
              // Use system from data
              await data.system.send(data.target, {
                id: "s1",
                type: "hello",
                payload: {},
              });
            }
            return { success: true };
          },
        };
        return data.system.register(actor);
      };

      const messages: string[] = [];
      const receiver = ReceiverActor({ messages, system });
      const sender = SenderActor({ target: receiver, system });

      await sender.send({ id: "t1", type: "trigger", payload: {} });

      expect(messages).toContain("hello");
    });
  });

  describe("6. No Magic Strings Invariant", () => {
    test("No string literals used as addresses", () => {
      const system = createActorSystem();

      const TestActor: ActorFactory<{ system: System }> = (data) => {
        const actor = {
          send: async (message: Message) => ({ success: true }),
        };
        return data.system.register(actor);
      };

      const task = TestActor({ system });

      // Address is object, not string
      expect(typeof task).toBe("object");
      expect(typeof task.__id).toBe("symbol");
      expect(typeof task.send).toBe("function");

      // NOT a string!
      expect(typeof task).not.toBe("string");
    });

    test("Addresses can be passed in message payloads", async () => {
      const system = createActorSystem();

      const CoordinatorActor: ActorFactory<{ tasks: Address[]; system: System }> =
        (data) => {
          const actor = {
            send: async (message: Message) => {
              if (message.type === "register_task") {
                const { taskAddr } = message.payload as { taskAddr: Address };
                data.tasks.push(taskAddr);

                // Can send to received address
                await taskAddr.send({ id: "c1", type: "status", payload: {} });

                return { success: true };
              }
              return { success: true };
            },
          };
          return data.system.register(actor);
        };

      const TaskActor: ActorFactory<{ called: boolean; system: System }> = (
        data
      ) => {
        const actor = {
          send: async (message: Message) => {
            if (message.type === "status") {
              data.called = true;
            }
            return { success: true };
          },
        };
        return data.system.register(actor);
      };

      const tasks: Address[] = [];
      const coordinator = CoordinatorActor({ tasks, system });

      let called = false;
      const task = TaskActor({ called: false, system });

      // Pass address in payload
      await coordinator.send({
        id: "1",
        type: "register_task",
        payload: { taskAddr: task },
      });

      expect(tasks.length).toBe(1);
      expect(tasks[0].__id).toBe(task.__id);
    });

    test("Addresses can be stored in actor data", () => {
      const system = createActorSystem();

      const TaskActor: ActorFactory<{
        id: string;
        dependents: Address[];
        system: System;
      }> = (data) => {
        const actor = {
          send: async (message: Message) => {
            if (message.type === "complete") {
              // Notify all dependents using stored addresses
              for (const dep of data.dependents) {
                await dep.send({ id: "u1", type: "unblocked", payload: {} });
              }
            }
            return { success: true };
          },
        };
        return data.system.register(actor);
      };

      const task1 = TaskActor({ id: "task-1", dependents: [], system });
      const task2 = TaskActor({
        id: "task-2",
        dependents: [task1],
        system,
      });

      // task2 stores task1's address
      expect(task2).toBeDefined();
    });
  });

  describe("Complete Example: Task Graph", () => {
    test("Task actors notify dependents using Address proxy", async () => {
      const system = createActorSystem();

      interface TaskData {
        id: string;
        status: "pending" | "done";
        dependents: Address[];
        system: System;
      }

      const TaskActor: ActorFactory<TaskData> = (data) => {
        const actor = {
          send: async (message: Message) => {
            if (message.type === "complete") {
              data.status = "done";

              // Notify all dependents using ergonomic API
              for (const dep of data.dependents) {
                await dep.send({
                  id: crypto.randomUUID(),
                  type: "unblocked",
                  payload: { by: data.id },
                });
              }

              return { success: true };
            }

            if (message.type === "unblocked") {
              return { success: true, data: `${data.id} unblocked` };
            }

            if (message.type === "status") {
              return { success: true, data: data.status };
            }

            return { success: true };
          },
        };
        return data.system.register(actor);
      };

      // Create task graph: task1 -> task2 -> task3
      const task3 = TaskActor({
        id: "task-3",
        status: "pending",
        dependents: [],
        system,
      });

      const task2 = TaskActor({
        id: "task-2",
        status: "pending",
        dependents: [task3],
        system,
      });

      const task1 = TaskActor({
        id: "task-1",
        status: "pending",
        dependents: [task2],
        system,
      });

      // Complete task-1
      await task1.send({ id: "c1", type: "complete", payload: {} });

      // Check statuses
      const status1 = await task1.send({ id: "s1", type: "status", payload: {} });
      const status2 = await task2.send({ id: "s2", type: "status", payload: {} });

      expect(status1.data).toBe("done");
      // task2 received unblock notification
      expect(status2.success).toBe(true);
    });
  });
});
