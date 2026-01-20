import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";
import { TopicNode, QueueNode } from "../../seag/messaging";

describe("SEAG Phase 6: Messaging Subsystem", () => {
  // Helper actor to send messages in tests
  class TestClientActor extends Actor {
    public receivedMessages: Message[] = [];
    constructor(id: string, system: System) {
      super(id, system);
    }
    async receive(msg: Message) {
      this.receivedMessages.push(msg);
    }
    // Helper to send a message and wait for a response if needed
    sendAndWait(target: ActorAddress, msg: Message, timeout = 100) {
      return new Promise<Message | null>(resolve => {
        this.receivedMessages = []; // Clear previous messages
        this.send(target, { ...msg, sender: this.id });
        const timer = setTimeout(() => resolve(null), timeout);
        const check = setInterval(() => {
          if (this.receivedMessages.length > 0) {
            clearInterval(check);
            clearTimeout(timer);
            resolve(this.receivedMessages[0]);
          }
        }, 10);
      });
    }
  }

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

    const client = system.spawn("seag://local/client-topic", TestClientActor);

    // 1. Subscribe
    client.send("seag://system/topic/alerts", {
      type: "SUBSCRIBE",
      payload: { consumer_id: "seag://local/sub-all", filter: "*" }
    });

    client.send("seag://system/topic/alerts", {
      type: "SUBSCRIBE",
      payload: { consumer_id: "seag://local/sub-error", filter: "CRITICAL" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 2. Publish Non-matching
    client.send("seag://system/topic/alerts", {
      type: "PUBLISH",
      payload: { text: "System nominal" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(receivedCount).toBe(1); // Only sub-all got it
    expect(lastAlert).toBe("System nominal");

    // 3. Publish Matching
    client.send("seag://system/topic/alerts", {
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
    
    const client = system.spawn("seag://local/client-queue", TestClientActor);

    // 1. Register worker
    client.send("seag://system/queue/tasks", {
      type: "REGISTER_WORKER",
      payload: { worker_id: "seag://local/worker-1" }
    });

    // 2. Enqueue tasks
    client.send("seag://system/queue/tasks", {
      type: "ENQUEUE",
      payload: { task: "A" }
    });
    client.send("seag://system/queue/tasks", {
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
    
    const client = system.spawn("seag://local/client-lease", TestClientActor);

    client.send("seag://system/queue/lease-test", {
      type: "REGISTER_WORKER",
      payload: { worker_id: "seag://local/forgetful" }
    });

    client.send("seag://system/queue/lease-test", {
      type: "ENQUEUE",
      payload: { task: "Urgent Work" }
    });

    // 1. First delivery
    await new Promise(resolve => setTimeout(resolve, 50));
    expect(deliveryCount).toBe(1);

    // 2. Wait for timeout and check re-delivery
    // Trigger check-timeouts manually to avoid waiting for interval
    await new Promise(resolve => setTimeout(resolve, 200)); // Ensure expiresAt is passed
    client.send("seag://system/queue/lease-test", { type: "CHECK_TIMEOUTS" });
    
    await new Promise(resolve => setTimeout(resolve, 500));
    expect(deliveryCount).toBeGreaterThan(1);
  });
});