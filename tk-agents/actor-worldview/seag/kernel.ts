/**
 * SEAG Kernel: Minimal Actor Runtime
 * Follows ap/GRAPH_SYSTEM.spec.md and ap/SYSTEM.model.lisp
 */

export type ActorAddress = string;
export type RestartPolicy = "permanent" | "transient" | "temporary";

export interface Message {
  type: string;
  sender?: ActorAddress;
  payload?: any;
  traceId?: string;
  hops?: number;
}

export interface ActorMetadata {
  id: ActorAddress;
  ActorClass: new (id: ActorAddress, system: System) => Actor;
  policy: RestartPolicy;
}

/**
 * Envelope: The wire format for messages leaving the process.
 * Compliant with ap/STABILITY.spec.md Section 2.
 */
export interface Envelope {
  to: ActorAddress;
  from: ActorAddress;
  traceId: string;
  hops: number;
  data: string | Uint8Array;
  timestamp: number;
}

/**
 * Serializer: Defines HOW a message is turned into bytes/strings.
 */
export interface Serializer {
  serialize(msg: Message): string | Uint8Array;
  deserialize(data: string | Uint8Array): Message;
}

/**
 * Transport: Defines WHERE a message goes.
 */
export interface Transport {
  send(envelope: Envelope): Promise<void>;
  onMessage(callback: (envelope: Envelope) => void): void;
}

export abstract class Actor {
  public readonly id: ActorAddress;
  protected system: System;
  public policy: RestartPolicy = "transient";
  public currentMessage: Message | null = null; // Track current context

  constructor(id: ActorAddress, system: System) {
    this.id = id;
    this.system = system;
  }

  abstract receive(msg: Message): Promise<void> | void;

  protected send(target: ActorAddress, msg: Message): void {
    // Inherit causality from current message if not explicitly set
    if (this.currentMessage) {
      msg.traceId = msg.traceId || this.currentMessage.traceId;
      msg.hops = msg.hops || this.currentMessage.hops;
    }
    this.system.send(target, { ...msg, sender: this.id });
  }

  async onStart(): Promise<void> {}
}

/**
 * RootSupervisor: The "Guardian" actor that restarts failed children.
 */
export class RootSupervisor extends Actor {
  async receive(msg: Message) {
    if (msg.type === "CHILD_CRASHED") {
      const { id, error } = msg.payload;
      console.error(`[RootSupervisor] Child ${id} crashed:`, error.message);
      
      const metadata = this.system.getMetadata(id);
      if (metadata && metadata.policy === "permanent") {
        console.log(`[RootSupervisor] Restarting permanent actor: ${id}`);
        this.system.spawn(id, metadata.ActorClass, metadata.policy, true);
      }
    }
  }
}

/**
 * Default Serializer: Uses standard JSON.
 */
class JSONSerializer implements Serializer {
  serialize(msg: Message): string {
    return JSON.stringify(msg);
  }
  deserialize(data: string | Uint8Array): Message {
    return JSON.parse(data.toString());
  }
}

export class System {
  private actors: Map<ActorAddress, Actor> = new Map();
  private registry: Map<ActorAddress, ActorMetadata> = new Map();
  private transports: Map<string, Transport> = new Map();
  private serializer: Serializer = new JSONSerializer();
  private supervisorAddress: ActorAddress | null = null;

  constructor(serializer?: Serializer) {
    if (serializer) this.serializer = serializer;
  }

  setSupervisor(address: ActorAddress) {
    this.supervisorAddress = address;
  }

  spawn<T extends Actor>(
    id: ActorAddress,
    ActorClass: new (id: ActorAddress, system: System) => T,
    policy: RestartPolicy = "transient",
    force: boolean = false
  ): T {
    if (this.actors.has(id) && !force) {
      throw new Error(`Actor already exists at address: ${id}`);
    }

    const actor = new ActorClass(id, this);
    actor.policy = policy;
    
    this.actors.set(id, actor);
    this.registry.set(id, { id, ActorClass, policy });

    Promise.resolve().then(() => actor.onStart());
    return actor;
  }

  registerTransport(prefix: string, transport: Transport) {
    this.transports.set(prefix, transport);
    transport.onMessage((env) => {
      const msg = this.serializer.deserialize(env.data);
      // Re-inject wire headers into the message object
      msg.traceId = env.traceId;
      msg.hops = env.hops;
      this.receiveExternal(env.to, { ...msg, sender: env.from });
    });
  }

  send(target: ActorAddress, msg: Message): void {
    // Initialize or increment observability metadata
    msg.traceId = msg.traceId || `trace-${Math.random().toString(36).substring(2, 11)}`;
    msg.hops = (msg.hops || 0) + 1;

    // Loop Avoidance: Drop messages that exceed the safety threshold
    if (msg.hops > 100) {
      console.error(`[System] Cyclic Dead Letter: Message ${msg.type} exceeded 100 hops. Trace: ${msg.traceId}`);
      return;
    }

    const isRemote = Array.from(this.transports.keys()).find(p => target.startsWith(p));

    if (isRemote) {
      const transport = this.transports.get(isRemote)!;
      const envelope: Envelope = {
        to: target,
        from: msg.sender || "seag://system/anonymous",
        traceId: msg.traceId,
        hops: msg.hops,
        data: this.serializer.serialize(msg),
        timestamp: Date.now()
      };
      transport.send(envelope);
    } else {
      this.dispatchLocal(target, msg);
    }
  }

  private dispatchLocal(target: ActorAddress, msg: Message): void {
    const actor = this.actors.get(target);
    if (!actor) {
      console.warn(`[System] Dead Letter: ${target}`);
      return;
    }

    const isolatedMsg = structuredClone(msg);

    setTimeout(async () => {
      try {
        actor.currentMessage = isolatedMsg; // Set context
        await actor.receive(isolatedMsg);
        actor.currentMessage = null; // Clear context
      } catch (err: any) {
        this.handleError(target, err);
      }
    }, 0);
  }

  private handleError(target: ActorAddress, err: Error) {
    if (this.supervisorAddress && target !== this.supervisorAddress) {
      this.send(this.supervisorAddress, {
        type: "CHILD_CRASHED",
        payload: { id: target, error: { message: err.message } }
      });
    } else {
      console.error(`[System] Unsupervised crash in ${target}:`, err);
    }
  }

  public getMetadata(id: ActorAddress): ActorMetadata | undefined {
    return this.registry.get(id);
  }

  private receiveExternal(target: ActorAddress, msg: Message): void {
    this.send(target, msg);
  }

  inspect(): ActorAddress[] {
    return Array.from(this.actors.keys());
  }
}