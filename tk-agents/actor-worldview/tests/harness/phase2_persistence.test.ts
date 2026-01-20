import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";
import { EventLogActor } from "../../seag/event-log";

describe("SEAG Phase 2: Persistence", () => {
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

  test("Objective 2.1: Automatic Event Logging", async () => {
    const system = new System();
    system.spawn("seag://system/event-log", EventLogActor);
    system.setEventLog("seag://system/event-log");

    let eventLog: Message[] = [];
    class StateActor extends Actor {
      private value = 0;
      async receive(msg: Message) {
        if (msg.type === "UPDATE_VALUE") {
          this.value = msg.payload.amount;
        }
        if (msg.type === "GET_VALUE") {
          this.send(msg.sender!, { type: "VALUE", payload: { value: this.value } });
        }
      }
    }
    system.spawn("seag://local/counter", StateActor);

    const client = system.spawn("seag://local/client-log", TestClientActor);

    // Send a message with a mutator name (automatic event)
    client.send("seag://local/counter", {
      type: "UPDATE_VALUE",
      payload: { amount: 10 }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // Directly query the EventLog (which would normally be done by a projector)
    const logResponse = await client.sendAndWait("seag://system/event-log", { type: "GET_EVENTS" });
    eventLog = logResponse?.payload.events;

    expect(eventLog.length).toBeGreaterThan(0);
    expect(eventLog[0].type).toBe("UPDATE_VALUE");
    expect(eventLog[0].source).toBe("seag://local/counter");
  });

  test("Objective 2.3: Event Replay", async () => {
    const system = new System();
    system.spawn("seag://system/event-log", EventLogActor);
    system.setEventLog("seag://system/event-log");

    const client = system.spawn("seag://local/client-replay", TestClientActor);

    // 1. Populate EventLog with some events
    client.send("seag://system/event-log", { type: "APPEND", payload: { id: "e1", source: "a", type: "DATA", payload: "msg1" } });
    client.send("seag://system/event-log", { type: "APPEND", payload: { id: "e2", source: "b", type: "DATA", payload: "msg2" } });

    await new Promise(resolve => setTimeout(resolve, 50));

    let replayedEvents: Message[] = [];
    class ReplayReceiver extends Actor {
      async receive(msg: Message) {
        if (msg.type === "REPLAY_RESULT") replayedEvents = msg.payload.events;
      }
    }
    system.spawn("seag://local/receiver", ReplayReceiver);

    // Trigger replay
    client.send("seag://system/event-log", { type: "REPLAY", sender: "seag://local/receiver" });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(replayedEvents.length).toBe(2);
    expect(replayedEvents[0].payload).toEqual({ id: "e1", source: "a", type: "DATA", payload: "msg1" });
  });
});