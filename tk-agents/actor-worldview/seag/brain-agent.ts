import { Actor, Message } from "./kernel";

/**
 * BrainAgent: The central intelligence coordinator of the SEAG.
 * Follows ap/REPL_AGENT.model.lisp
 */
export class BrainAgent extends Actor {
  
  async receive(msg: Message) {
    if (msg.type === "THINK") {
      const input = msg.payload.input;
      console.log(`[Brain] Thinking about: ${input}`);

      // 1. Emit "Thinking" signal
      this.send(msg.sender!, { 
        type: "SIGNAL", 
        payload: { status: "thinking", detail: "Querying knowledge graph..." } 
      });

      // 2. Sense: Query the Graph Projector
      // (Mocking a graph query for the MVP)
      this.send("seag://system/projector", {
        type: "QUERY",
        payload: { predicate: "get_node", args: { id: "seag://local/managed-doc" } }
      });

      // For the MVP, we'll simulate a heuristic response
      setTimeout(() => {
        const response = this.formulateHeuristicResponse(input);
        
        // 3. Act: Reply to the UserProxy
        this.send(msg.sender!, {
          type: "OUTPUT",
          payload: { content: response }
        });
      }, 100);
    }
  }

  private formulateHeuristicResponse(input: string): string {
    if (input.toLowerCase().includes("hello")) {
      return "Hello Human! I am the SEAG Brain. I can see the graph.";
    }
    return `I processed your request: "${input}". The graph is evolving.`;
  }
}
