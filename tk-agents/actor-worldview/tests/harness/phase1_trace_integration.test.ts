import { expect, test, describe, mock } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { GatewayRelay } from "../../seag/gateway";

describe("SEAG Phase 1: Trace Integration", () => {

  test("Objective 1.8: Gateway Subscribes to Trace Topic", async () => {
    const system = new System();
    
    // 1. Mock the Trace Topic
    let subscriptionReceived = false;
    class MockTraceTopic extends Actor {
      async receive(msg: Message) {
        if (msg.type === "SUBSCRIBE" && msg.payload.consumer_id === "seag://system/gateway-relay") {
          subscriptionReceived = true;
        }
      }
    }
    system.spawn("seag://system/topic/trace", MockTraceTopic);

    // 2. Spawn Gateway Relay
    system.spawn("seag://system/gateway-relay", GatewayRelay);

    await new Promise(resolve => setTimeout(resolve, 20));
    
    expect(subscriptionReceived).toBe(true);
  });

  test("Objective 1.9: Gateway Transforms Trace Events", async () => {
    const system = new System();
    
    // 1. Spawn Gateway Relay with mocked WebSocket
    const gateway = system.spawn("seag://system/gateway-relay", GatewayRelay);
    const mockWs = {
      send: mock((data: string) => {})
    };
    gateway.ws = mockWs;

    // 2. Simulate a Trace Notification from the Topic
    const traceEvent = {
      traceId: "trace-123",
      sender: "seag://test/sender",
      target: "seag://test/target",
      messageType: "TEST",
      timestamp: 1234567890
    };

    system.send("seag://system/gateway-relay", {
      type: "NOTIFY",
      payload: traceEvent,
      sender: "seag://system/topic/trace"
    });

    await new Promise(resolve => setTimeout(resolve, 20));

    // 3. Verify WebSocket output
    expect(mockWs.send).toHaveBeenCalled();
    const sentData = JSON.parse(mockWs.send.mock.calls[0][0]);
    
    expect(sentData.type).toBe("SIGNAL");
    expect(sentData.payload).toEqual(traceEvent);
  });

});
