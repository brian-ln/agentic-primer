import { System, RootSupervisor } from "./kernel";
import { EventLogActor } from "./event-log";
import { GraphProjector } from "./graph-projector";
import { FileEffectActor } from "./file-effect";
import { DocumentParser } from "./shredder";
import { UserProxy } from "./user-proxy";
import { BrainAgent } from "./brain-agent";
import { GeminiInferenceActor } from "./inference-actor";
import { GeminiEmbeddingActor } from "./embedding-actor";
import { Gateway } from "./gateway";

async function bootstrap() {
  const system = new System();
  console.log("SEAG Bootstrapping");
  system.spawn("seag://system/supervisor", RootSupervisor);
  system.setSupervisor("seag://system/supervisor");
  system.spawn("seag://system/event-log", EventLogActor, "permanent");
  system.setEventLog("seag://system/event-log");
  system.spawn("seag://system/projector", GraphProjector, "permanent");
  system.spawn("seag://system/parser", DocumentParser, "permanent");
  system.spawn("seag://system/file-io", FileEffectActor, "permanent");
  system.spawn("seag://system/inference", GeminiInferenceActor, "permanent");
  system.spawn("seag://system/embedder", GeminiEmbeddingActor, "permanent");
  system.spawn("seag://local/user-proxy", UserProxy);
  system.spawn("seag://system/brain", BrainAgent);
  const gateway = new Gateway(system);
  gateway.start(3000);
  console.log("SEAG MVP is Online: http://localhost:3000");
}

bootstrap();