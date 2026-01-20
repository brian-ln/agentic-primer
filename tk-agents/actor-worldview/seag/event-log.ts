import { Actor, Message, Event } from "./kernel";
import { Actor as ActorModel, Implements } from "./lib/meta";

/**
 * EventLogActor: Durable Event Storage
 * Follows ap/LOG_SYSTEM.spec.md
 */
@ActorModel("EventLogActor")
@Implements("BaseNode")
export class EventLogActor extends Actor {
  private logPath: string = "data/events.jsonl";

  async onStart() {
    // Ensure data directory exists
    const fs = await import("node:fs/promises");
    const path = await import("node:path");
    await fs.mkdir(path.dirname(this.logPath), { recursive: true });
  }

  async receive(msg: Message) {
    if (msg.type === "APPEND") {
      // console.log(`[EventLog] Appending event: ${msg.payload.id || 'unnamed'}`);
      const event: Event = msg.payload;
      await this.persist(event);
      if (msg.sender) {
        this.send(msg.sender, { type: "ACK_APPEND", payload: { id: event.id } });
      }
    }

    if (msg.type === "REPLAY") {
      const events = await this.readLog();
      this.send(msg.sender!, { type: "REPLAY_RESULT", payload: events });
    }
  }

  private async persist(event: Event) {
    const fs = await import("node:fs/promises");
    const line = JSON.stringify(event) + "\n";
    await fs.appendFile(this.logPath, line);
  }

  private async readLog(): Promise<Event[]> {
    const fs = await import("node:fs/promises");
    try {
      const content = await fs.readFile(this.logPath, "utf-8");
      return content
        .trim()
        .split("\n")
        .filter(line => line.length > 0)
        .map(line => JSON.parse(line));
    } catch (err) {
      return [];
    }
  }
}
