import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { InferenceRouter } from "../../seag/inference/router";

describe("SEAG Phase 5.2: Inference Routing", () => {

  test("Objective 5.2.2: Router correctly handles 'provider' param", async () => {
    const system = new System();
    const routerUri = "seag://system/inference";
    system.spawn(routerUri, InferenceRouter);
    
    system.send(routerUri, {
      type: "REGISTER_PROVIDER",
      payload: { model_id: "google:gemini", actor_address: "seag://local/target-1" }
    });
    system.send(routerUri, {
      type: "REGISTER_PROVIDER",
      payload: { model_id: "gemini", actor_address: "seag://local/target-2" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    let lastMessage: any = null;
    class MockTarget extends Actor {
      async receive(msg: Message) { 
        if (msg.type === "PROMPT") lastMessage = msg; 
      }
    }
    system.spawn("seag://local/target-1", MockTarget);

    // 1. Explicit provider+model
    system.send(routerUri, {
      type: "PROMPT",
      sender: "seag://local/user",
      payload: { params: { provider: "google", model: "gemini" }, text: "test" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(lastMessage).not.toBeNull();
    expect(lastMessage.payload.text).toBe("test");
  });

  test("Objective 5.2.3: Router falls back to model if provider mismatch", async () => {
    const system = new System();
    const routerUri = "seag://system/inference";
    system.spawn(routerUri, InferenceRouter);
    
    system.send(routerUri, {
      type: "REGISTER_PROVIDER",
      payload: { model_id: "gemini", actor_address: "seag://local/target-2" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    let lastMessage: any = null;
    class MockTarget extends Actor {
      async receive(msg: Message) { 
        if (msg.type === "PROMPT") lastMessage = msg; 
      }
    }
    system.spawn("seag://local/target-2", MockTarget);

    // Send with a provider that isn't registered specifically
    system.send(routerUri, {
      type: "PROMPT",
      sender: "seag://local/user",
      payload: { params: { provider: "unknown", model: "gemini" }, text: "fallback-test" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(lastMessage).not.toBeNull();
    expect(lastMessage.payload.text).toBe("fallback-test");
  });
});
