import { Actor, Message, Event, ActorAddress } from "./kernel";

/**
 * GraphProjector: Maintains a queryable view of the SEAG graph.
 * This is a "Minimal Viable Index" that mimics Datalog rules.
 * Follows ap/GRAPH_RULES.datalog logic.
 */
export class GraphProjector extends Actor {
  private nodes: Map<ActorAddress, any> = new Map();
  private edges: { from: ActorAddress; to: ActorAddress; type: string }[] = [];

  async receive(msg: Message) {
    if (msg.type === "APPEND") {
      const event: Event = msg.payload;
      this.project(event);
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

      default:
        return [];
    }
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
