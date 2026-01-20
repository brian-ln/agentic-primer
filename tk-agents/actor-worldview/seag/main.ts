import { System, RootSupervisor } from "./kernel";
import { EventLogActor } from "./event-log";
import { GraphProjector } from "./graph-projector";
import { FileEffectActor } from "./file-effect";
import { DocumentParser } from "./shredder";
import { UserProxy } from "./user-proxy";
import { BrainAgent } from "./brain-agent";
import { VertexInferenceActor } from "./inference/google/vertex";
import { StudioInferenceActor } from "./inference/google/studio";
import { GeminiEmbeddingActor } from "./embedding-actor";
import { InferenceRouter } from "./inference/router";
import { PersistenceManager } from "./persistence-manager";
import { CredentialProviderActor } from "./credential-provider";
import { TopicNode, TraceTopic, QueueNode } from "./messaging";
import { Gateway } from "./gateway";

async function bootstrap() {
// ...
  // Messaging default hubs
  system.spawn("seag://system/topic/main", TopicNode, "permanent");
  system.spawn("seag://system/topic/trace", TraceTopic, "permanent");
  system.spawn("seag://system/queue/main", QueueNode, "permanent");

  system.spawn("seag://local/user-proxy", UserProxy);
  system.spawn("seag://system/brain", BrainAgent);
  
  const gateway = new Gateway(system);
  gateway.start(3000);
  console.log(`SEAG MVP is Online: http://localhost:3000`);

  // Keep alive heartbeat to prevent process from exiting
  setInterval(() => {
    // heartbeat
  }, 1000 * 60); 
}

bootstrap();