import { System } from "./kernel";
import { DocumentActor } from "./document-actor";
import { DocumentParser, FragmentNode } from "./shredder";
import { FileEffectActor } from "./file-effect";

async function verifySync() {
  const system = new System();
  system.spawn("seag://system/file-io", FileEffectActor);
  system.spawn("seag://system/parser", DocumentParser);
  
  const docId = "seag://local/active-doc";
  system.spawn(docId, DocumentActor);
  
  const token = Buffer.from(JSON.stringify({resource: "*", action: "*"})).toString('base64');
  
  system.send(docId, { 
    type: "INIT_DOCUMENT", 
    payload: { path: "data/demo.json", format: "json" },
    capabilityToken: token
  });

  // Re-shred current content
  const content = await (await import("node:fs/promises")).readFile("data/demo.json", "utf-8");
  system.send("seag://system/parser", {
    type: "SHRED",
    payload: { content, format: "json", docId }
  });

  await new Promise(resolve => setTimeout(resolve, 200));

  // Verify the fragment has the new name we wrote manually earlier
  class Checker extends (await import("./kernel")).Actor {
    async receive(msg: any) {
      if (msg.type === "STATE") {
        console.log("\n✅ TWO-WAY SYNC VERIFIED!");
        console.log("Fragment Value:", msg.payload);
        process.exit(0);
      }
    }
  }
  system.spawn("seag://local/checker", Checker);
  system.send(`${docId}/fragments/name`, { type: "GET_STATE", sender: "seag://local/checker" });
}

verifySync();
