import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { TopicNode, QueueNode } from "../../seag/messaging";

describe("SEAG Phase 6: Messaging Subsystem", () => {
  
  test("Objective 6.1.1: TopicNode Pub/Sub with Filtering", async () => {
    const system = new System();
    system.spawn("seag://system/topic/alerts", TopicNode);

    let receivedCount = 0;
    let lastAlert = "";

    class Subscriber extends Actor {
      async receive(msg: Message) {
        if (msg.type === "NOTIFY") {
          receivedCount++;
          lastAlert = msg.payload.text;
        }
      }
    }

    system.spawn("seag://local/sub-all", Subscriber);
    system.spawn("seag://local/sub-error", Subscriber);

    // 1. Subscribe
    system.send("seag://system/topic/alerts", {
      type: "SUBSCRIBE",
      payload: { consumer_id: "seag://local/sub-all", filter: "*" }
    });

    system.send("seag://system/topic/alerts", {
      type: "SUBSCRIBE",
      payload: { consumer_id: "seag://local/sub-error", filter: "CRITICAL" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 2. Publish Non-matching
    system.send("seag://system/topic/alerts", {
      type: "PUBLISH",
      payload: { text: "System nominal" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(receivedCount).toBe(1); // Only sub-all got it
    expect(lastAlert).toBe("System nominal");

    // 3. Publish Matching
    system.send("seag://system/topic/alerts", {
      type: "PUBLISH",
      payload: { text: "CRITICAL ERROR" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(receivedCount).toBe(3); // sub-all got it + sub-error got it (1+2=3)
    expect(lastAlert).toBe("CRITICAL ERROR");
  });

  test("Objective 6.1.2: QueueNode Work Distribution", async () => {
    const system = new System();
    system.spawn("seag://system/queue/tasks", QueueNode);

    let completedTasks = 0;

    class Worker extends Actor {
      async receive(msg: Message) {
        if (msg.type === "DO_WORK") {
          completedTasks++;
          // ACK the task
          this.send(msg.sender!, {
            type: "ACK",
            payload: { msg_id: msg.id }
          });
        }
      }
    }

    system.spawn("seag://local/worker-1", Worker);
    
    // 1. Register worker
    system.send("seag://system/queue/tasks", {
      type: "REGISTER_WORKER",
      payload: { worker_id: "seag://local/worker-1" }
    });

    // 2. Enqueue tasks
    system.send("seag://system/queue/tasks", {
      type: "ENQUEUE",
      payload: { task: "A" }
    });
    system.send("seag://system/queue/tasks", {
      type: "ENQUEUE",
      payload: { task: "B" }
    });

    await new Promise(resolve => setTimeout(resolve, 100));
    expect(completedTasks).toBe(2);
  });

  test("Objective 6.1.3: QueueNode Lease Timeout and Re-delivery", async () => {
    const system = new System();
    const queue = system.spawn("seag://system/queue/lease-test", QueueNode);
    // Set a very short lease for testing
    (queue as any).leaseDuration = 100;

    let deliveryCount = 0;

    class ForgetfulWorker extends Actor {
      async receive(msg: Message) {
        if (msg.type === "DO_WORK") {
          deliveryCount++;
          // NEVER ACK
        }
      }
    }

    system.spawn("seag://local/forgetful", ForgetfulWorker);
    
    system.send("seag://system/queue/lease-test", {
      type: "REGISTER_WORKER",
      payload: { worker_id: "seag://local/forgetful" }
    });

    system.send("seag://system/queue/lease-test", {
      type: "ENQUEUE",
      payload: { task: "Urgent Work" }
    });

    // 1. First delivery
    await new Promise(resolve => setTimeout(resolve, 50));
    expect(deliveryCount).toBe(1);

    // 2. Wait for timeout and check re-delivery
    // Trigger check-timeouts manually to avoid waiting for interval
    await new Promise(resolve => setTimeout(resolve, 200)); // Ensure expiresAt is passed
    system.send("seag://system/queue/lease-test", { type: "CHECK_TIMEOUTS" });
    
    await new Promise(resolve => setTimeout(resolve, 500));
    expect(deliveryCount).toBeGreaterThan(1);
  });
});
