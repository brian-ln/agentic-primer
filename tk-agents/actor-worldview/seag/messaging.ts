import { Actor, Message, ActorAddress } from "./kernel";
import { Actor as ActorModel, Handler } from "./lib/meta";

/**
 * TopicNode: Implements Pub/Sub logic with simple filtering.
 * Follows ap/MESSAGING.model.lisp
 */
@ActorModel("TopicNode")
export class TopicNode extends Actor {
  private subscribers: Set<{ id: ActorAddress; filter: string }> = new Set();

  @Handler("SUBSCRIBE")
  @Handler("PUBLISH")
  async receive(msg: Message) {
    if (msg.type === "SUBSCRIBE") {
      const { consumer_id, filter } = msg.payload;
      this.subscribers.add({ id: consumer_id, filter: filter || "*" });
      console.log(`[Topic] ${this.id} subscribed: ${consumer_id} (Filter: ${filter})`);
    }

    if (msg.type === "PUBLISH") {
      const content = msg.payload;
      for (const sub of this.subscribers) {
        if (this.matchFilter(sub.filter, content)) {
          this.send(sub.id, { 
            type: "NOTIFY", 
            payload: content,
            sender: this.id 
          });
        }
      }
    }
  }

  private matchFilter(filter: string, message: any): boolean {
    if (filter === "*") return true;
    // Basic string matching for now, could be extended to JSONPath/Datalog
    const msgStr = JSON.stringify(message);
    return msgStr.includes(filter);
  }
}

/**
 * QueueNode: Implements reliable work distribution (Load Balancing).
 * Follows ap/MESSAGING.model.lisp
 */
@ActorModel("QueueNode")
export class QueueNode extends Actor {
  private backlog: Message[] = [];
  private pending: Map<string, { worker: ActorAddress; timestamp: number; msg: Message }> = new Map();
  private workers: Set<ActorAddress> = new Set();

  @Handler("ENQUEUE")
  @Handler("REGISTER_WORKER")
  @Handler("ACK")
  @Handler("NACK")
  @Handler("DISTRIBUTE")
  async receive(msg: Message) {
    if (msg.type === "ENQUEUE") {
      const workMsg = { ...msg, id: msg.id || `work-${Math.random().toString(36).substring(7)}` };
      this.backlog.push(workMsg);
      await this.distribute();
    }

    if (msg.type === "REGISTER_WORKER") {
      this.workers.add(msg.payload.worker_id);
      console.log(`[Queue] ${this.id} registered worker: ${msg.payload.worker_id}`);
      await this.distribute();
    }

    if (msg.type === "ACK") {
      this.pending.delete(msg.payload.msg_id);
    }

    if (msg.type === "NACK") {
      const entry = this.pending.get(msg.payload.msg_id);
      if (entry) {
        this.backlog.push(entry.msg);
        this.pending.delete(msg.payload.msg_id);
        await this.distribute();
      }
    }

    if (msg.type === "DISTRIBUTE") {
      await this.distribute();
    }
  }

  @Handler("HANDLE_DISTRIBUTE")
  private async distribute() {
    while (this.backlog.length > 0 && this.workers.size > 0) {
      const work = this.backlog.shift()!;
      const workerList = Array.from(this.workers);
      const worker = workerList[0]; // Simple Round-Robin/FCFS for now
      
      // Move worker to end of line or temporary "busy" set? 
      // For simplicity, we just keep them in the set for now.
      
      this.pending.set(work.id!, { worker, timestamp: Date.now(), msg: work });
      
      this.send(worker, {
        type: "DO_WORK",
        payload: work.payload,
        sender: this.id,
        traceId: work.traceId
      });
    }
  }
}
