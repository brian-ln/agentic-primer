import { expect, test, describe, mock } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { GeminiInferenceActor } from "../../seag/inference-actor";

describe("SEAG Phase 5: AI (Unit Tests)", () => {

  test("Objective 5.1.2: InferenceActor.handlePrompt (Unit Test)", async () => {
    // 1. Mock the global fetch function
    global.fetch = mock(() => Promise.resolve(new Response(JSON.stringify({
      candidates: [{ content: { parts: [{ text: "Mocked AI Response" }] } }]
    }), { status: 200 })));

    // 2. Setup Mocks
    const mockSystem = {
      send: mock(() => {}),
    } as unknown as System;

    const actor = new GeminiInferenceActor("seag://test/inference", mockSystem);

    // 3. Manually set internal state for the test
    (actor as any).credential = { type: "api_key", value: "mock_key" };

    // 4. Create the message and call the method directly
    const promptMessage: Message = {
      type: "PROMPT",
      sender: "seag://test/client",
      payload: { text: "Test Prompt" }
    };
    
    await actor.handlePrompt(promptMessage);

    // 5. Assert that the actor tried to send the correct response
    expect(mockSystem.send).toHaveBeenCalledTimes(1);
    const callArgs = (mockSystem.send as any).mock.calls[0];
    const target = callArgs[0];
    const responseMsg = callArgs[1];

    expect(target).toBe("seag://test/client");
    expect(responseMsg.type).toBe("RESPONSE");
    expect(responseMsg.payload.text).toBe("Mocked AI Response");
  });
});