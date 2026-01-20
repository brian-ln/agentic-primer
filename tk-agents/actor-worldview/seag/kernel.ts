import { Actor as ActorModel, Handler } from "./lib/meta";

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
  isEvent?: boolean;
  capabilityToken?: string; // CT for restricted actions (ap/SECURITY.spec.md)
}

/**
 * CapabilityToken: Simple representation for the MVP.
 * In production, this would be a signed JWT or Macaroon.
 */
export interface CapabilityToken {
  resource: string;
  action: string;
  expiresAt: number;
}

/**
 * Event: A recorded fact in the SEAG history.
 */
export interface Event {
  id: string;
  type: string;
  source: ActorAddress;
  traceId: string;
  payload: any;
  timestamp: number;
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

  @Handler("START")
  async onStart(): Promise<void> {}
}

/**
 * RootSupervisor: The "Guardian" actor that restarts failed children.
 */
@ActorModel("RootSupervisor")
export class RootSupervisor extends Actor {
  @Handler("START")
  async onStart() {
    console.log("[RootSupervisor] Started");
  }

  @Handler("CHILD_CRASHED")
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

@ActorModel("Kernel")
export class System {
  private actors: Map<ActorAddress, Actor> = new Map();
  private registry: Map<ActorAddress, ActorMetadata> = new Map();
  private transports: Map<string, Transport> = new Map();
  private mailboxes: Map<ActorAddress, Message[]> = new Map();
  private processing: Map<ActorAddress, boolean> = new Map();
  private serializer: Serializer = new JSONSerializer();
  private supervisorAddress: ActorAddress | null = null;
  private eventLogAddress: ActorAddress | null = null;

  constructor(serializer?: Serializer) {
    if (serializer) this.serializer = serializer;
  }

  setSupervisor(address: ActorAddress) {
    this.supervisorAddress = address;
  }

  setEventLog(address: ActorAddress) {
    this.eventLogAddress = address;
  }

  spawn<T extends Actor>(
    id: ActorAddress,
    ActorClass: new (id: ActorAddress, system: System) => T,
    policy: RestartPolicy = "transient",
    force: boolean = false
  ): T {
    const existing = this.actors.get(id);
    if (existing && !force) {
      return existing as T;
    }

    const actor = new ActorClass(id, this);
    actor.policy = policy;
    
    this.actors.set(id, actor);
    this.registry.set(id, { id, ActorClass, policy });
    this.mailboxes.set(id, []);
    this.processing.set(id, false);

    Promise.resolve().then(() => actor.onStart()).catch(err => {
      console.error(`[System] Failed to start actor ${id}:`, err);
    });
    return actor;
  }

  registerTransport(prefix: string, transport: Transport) {
    this.transports.set(prefix, transport);
    transport.onMessage((env) => {
      const msg = this.serializer.deserialize(env.data);
      msg.traceId = env.traceId;
      msg.hops = env.hops;
      this.receiveExternal(env.to, { ...msg, sender: env.from });
    });
  }

  send(target: ActorAddress, msg: Message): void {
    msg.traceId = msg.traceId || `trace-${Math.random().toString(36).substring(2, 11)}`;
    msg.hops = (msg.hops || 0) + 1;

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

  @Handler("DISPATCH")
  private dispatchLocal(target: ActorAddress, msg: Message): void {
    const actor = this.actors.get(target);
    if (!actor) {
      console.warn(`[System] Dead Letter: ${target} (Type: ${msg.type}, Sender: ${msg.sender})`);
      return;
    }

    const isolatedMsg = structuredClone(msg);

    // Queue the message
    const mailbox = this.mailboxes.get(target)!;
    mailbox.push(isolatedMsg);

    // Write-Ahead Logic
    const mutatorPattern = /^(patch|update|append|link|delete|set|create|add)/i;
    const isMutator = msg.isEvent || mutatorPattern.test(msg.type);
    if (isMutator && this.eventLogAddress && target !== this.eventLogAddress) {
      this.recordEvent(target, isolatedMsg);
    }

    this.processMailbox(target);
  }

  private async processMailbox(target: ActorAddress) {
    if (this.processing.get(target)) return;
    
    const actor = this.actors.get(target)!;
    const mailbox = this.mailboxes.get(target)!;

    if (mailbox.length === 0) return;

    this.processing.set(target, true);

    while (mailbox.length > 0) {
      const msg = mailbox.shift()!;
      try {
        actor.currentMessage = msg;
        await actor.receive(msg);
        actor.currentMessage = null;
      } catch (err: any) {
        this.handleError(target, err);
      }
    }

    this.processing.set(target, false);
  }

  @Handler("RECORD_EVENT")
  private recordEvent(source: ActorAddress, msg: Message) {
    const event: Event = {
      id: `ev-${Math.random().toString(36).substring(2, 11)}`,
      type: msg.type,
      source,
      traceId: msg.traceId!,
      payload: msg.payload,
      timestamp: Date.now()
    };

    this.send(this.eventLogAddress!, {
      type: "APPEND",
      payload: event,
      traceId: msg.traceId,
      sender: "seag://system/kernel"
    });
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
