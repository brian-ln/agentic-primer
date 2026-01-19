import { Actor, Message, Event, ActorAddress } from "./kernel";
import { Actor as ActorModel, Handler } from "./lib/meta";

/**
 * GraphProjector: Maintains a queryable view of the SEAG graph.
 * This is a "Minimal Viable Index" that mimics Datalog rules.
 * Follows ap/GRAPH_RULES.datalog logic.
 */
@ActorModel("GraphProjector")
export class GraphProjector extends Actor {
  private nodes: Map<ActorAddress, any> = new Map();
  private edges: { from: ActorAddress; to: ActorAddress; type: string }[] = [];
  private vectors: Map<ActorAddress, number[]> = new Map();

  @Handler("APPEND")
  @Handler("LINK_TO")
  @Handler("CREATE_EDGE")
  @Handler("UPDATE_STATE")
  @Handler("SET_VECTOR")
  @Handler("QUERY")
  async receive(msg: Message) {
    if (msg.type === "APPEND") {
      const event: Event = msg.payload;
      this.project(event);
    }

    if (msg.type === "LINK_TO" || msg.type === "CREATE_EDGE") {
      const { from, to, type } = msg.payload;
      this.edges.push({ from: from || msg.sender!, to, type });
    }

    if (msg.type === "UPDATE_STATE") {
      this.nodes.set(msg.sender!, msg.payload);
    }

    if (msg.type === "SET_VECTOR") {
      this.vectors.set(msg.sender!, msg.payload.vector);
    }

    if (msg.type === "QUERY") {
      const { predicate, args } = msg.payload;
      const result = this.handleQuery(predicate, args);
      this.send(msg.sender!, { type: "QUERY_RESULT", payload: result });
    }
  }

  private project(event: Event) {
    if (event.type === "UPDATE_STATE" || event.type.startsWith("UPDATE")) {
      this.nodes.set(event.source, event.payload);
    }

    if (event.type === "LINK_TO" || event.type === "CREATE_EDGE") {
      const { to, type } = event.payload;
      this.edges.push({ from: event.source, to, type });
    }
  }

  private handleQuery(predicate: string, args: any): any {
    switch (predicate) {
      case "linked":
        return this.edges.filter(e => e.from === args.from || e.to === args.to);
      
      case "reachable":
        return this.calculateReachability(args.from);

      case "get_node":
        return this.nodes.get(args.id);

      case "vector_search":
        return this.vectorSearch(args.vector, args.limit || 5);

      default:
        return [];
    }
  }

  private vectorSearch(queryVector: number[], limit: number) {
    const scores: { id: ActorAddress; score: number }[] = [];
    
    for (const [id, vector] of this.vectors.entries()) {
      const score = this.cosineSimilarity(queryVector, vector);
      scores.push({ id, score });
    }

    return scores
      .sort((a, b) => b.score - a.score)
      .slice(0, limit);
  }

  private cosineSimilarity(a: number[], b: number[]): number {
    let dot = 0;
    let normA = 0;
    let normB = 0;
    for (let i = 0; i < a.length; i++) {
      dot += a[i] * b[i];
      normA += a[i] * a[i];
      normB += b[i] * b[i];
    }
    return dot / (Math.sqrt(normA) * Math.sqrt(normB));
  }

  private calculateReachability(start: ActorAddress): ActorAddress[] {
    const visited = new Set<ActorAddress>();
    const queue = [start];

    while (queue.length > 0) {
      const current = queue.shift()!;
      if (visited.has(current)) continue;
      visited.add(current);

      const neighbors = this.edges
        .filter(e => e.from === current)
        .map(e => e.to);
      
      queue.push(...neighbors);
    }

    return Array.from(visited);
  }
}
