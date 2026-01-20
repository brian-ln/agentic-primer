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
  const system = new System();
  console.log(`SEAG Bootstrapping at ${new Date().toISOString()}`);
  system.spawn("seag://system/supervisor", RootSupervisor);
  system.setSupervisor("seag://system/supervisor");
  system.spawn("seag://system/event-log", EventLogActor, "permanent");
  system.setEventLog("seag://system/event-log");
  system.spawn("seag://system/projector", GraphProjector, "permanent");
  system.spawn("seag://system/parser", DocumentParser, "permanent");
  system.spawn("seag://system/file-io", FileEffectActor, "permanent");
  system.spawn("seag://system/persistence", PersistenceManager, "permanent");
  system.spawn("seag://system/credentials", CredentialProviderActor, "permanent");
  system.spawn("seag://system/interaction-log", EventLogActor, "permanent");
  
  // Concrete providers
  system.spawn("seag://system/inference/vertex", VertexInferenceActor, "permanent");
  system.spawn("seag://system/inference/studio", StudioInferenceActor, "permanent");
  system.spawn("seag://system/embedder", GeminiEmbeddingActor, "permanent");
  
  // Stable Router
  system.spawn("seag://system/inference", InferenceRouter, "permanent");
  
  // Register Providers with Router
  // Vertex (The Default)
  system.send("seag://system/inference", {
    type: "REGISTER_PROVIDER",
    payload: { 
      model_id: "gemini-2.0-flash-exp", 
      actor_address: "seag://system/inference/vertex",
      is_default: true
    }
  });
  system.send("seag://system/inference", {
    type: "REGISTER_PROVIDER",
    payload: { 
      model_id: "vertex:gemini-2.0-flash-exp", 
      actor_address: "seag://system/inference/vertex"
    }
  });

  // Studio
  system.send("seag://system/inference", {
    type: "REGISTER_PROVIDER",
    payload: { 
      model_id: "models/gemini-2.0-flash-exp", 
      actor_address: "seag://system/inference/studio"
    }
  });
  system.send("seag://system/inference", {
    type: "REGISTER_PROVIDER",
    payload: { 
      model_id: "studio:gemini-2.0-flash-exp", 
      actor_address: "seag://system/inference/studio"
    }
  });

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
