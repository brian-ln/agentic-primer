import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";
import { parse, SExpr } from "../seag/lib/lisp-reader";

// --- Types ---

interface Protocol {
  name: string;
  inputs: string[]; // Messages this protocol handles
  outputs: string[]; // Messages this protocol yields
  file: string;
}

interface ActorModel {
  name: string;
  implements: string[]; // Protocols implemented
  handlers: string[];
  file: string;
}

interface ActorImpl {
  name: string;      // The Class Name
  modelName: string; // The @Actor("Name") tag
  implements: string[]; // The @Implements("Proto") tag
  handlers: string[]; // The @Handler("MSG") tags
  file: string;
}

// --- Extractor ---

function extractDefinitions(sexprs: SExpr[], filename: string) {
  const models: ActorModel[] = [];
  const protocols: Protocol[] = [];

  function walk(node: SExpr) {
    if (Array.isArray(node)) {
      const [type, name, ...rest] = node;
      
      // 1. Detect Definitions
      if ((type === "defprotocol" || type === "protocol") && typeof name === "string") {
        const inputs: string[] = [];
        const outputs: string[] = [];
        // Scan for (on MSG ...) or (message MSG ...) blocks
        for (const item of rest) {
          if (Array.isArray(item)) {
            const head = item[0];
            if (head === "on" || head === "message") {
              const msg = item[1];
              if (typeof msg === "string") inputs.push(msg.toUpperCase().replace(/-/g, '_'));
              
              // Helper to recurse into logic flow
              const scanYields = (node: any) => {
                if (!Array.isArray(node)) return;
                const type = node[0];
                // Quantifiers
                if (["seq", "or", "any", "some", "opt", "one"].includes(type)) {
                  node.slice(1).forEach(scanYields);
                }
                // Legacy support or direct yield (if used)
                if (type === "yields") {
                  if (typeof node[1] === "string") outputs.push(node[1].toUpperCase().replace(/-/g, '_'));
                }
                // New syntax: (one MSG) implies MSG is the output type
                if (type === "one" || type === "any" || type === "some" || type === "opt") {
                   // node[1] might be the message name directly: (one MSG (args))
                   const candidate = node[1];
                   if (typeof candidate === "string" && /^[A-Z]/.test(candidate)) {
                      outputs.push(candidate.toUpperCase().replace(/-/g, '_'));
                   }
                }
              };

              // Scan the body of the handler
              item.slice(2).forEach(scanYields);
            }
          }
        }
        protocols.push({ name, inputs, outputs, file: filename });
      }

      if (type === "actor" && typeof name === "string") {
        const handlers: string[] = [];
        const impls: string[] = [];
        
        const behavior = rest.find(item => Array.isArray(item) && item[0] === "behavior");
        if (Array.isArray(behavior)) {
          for (const clause of behavior.slice(1)) {
            if (Array.isArray(clause) && clause[0] === "on") {
              const msgType = clause[1];
              if (typeof msgType === "string") {
                handlers.push(msgType.toUpperCase().replace(/-/g, '_'));
              }
            }
          }
        }

        const implBlock = rest.find(item => Array.isArray(item) && item[0] === "implements");
        if (Array.isArray(implBlock)) {
          implBlock.slice(1).forEach(p => typeof p === "string" && impls.push(p));
        }

        models.push({ name, implements: impls, handlers: [...new Set(handlers)], file: filename });
      }

      // 2. Recursive Walk (The Fix: Walk EVERYTHING)
      node.forEach(child => {
        if (Array.isArray(child)) walk(child);
      });
    }
  }

  sexprs.forEach(walk);
  return { models, protocols };
}

// --- TypeScript Parser (Decorator Aware) ---

function parseTsImpl(content: string, filename: string): ActorImpl[] {
  const impls: ActorImpl[] = [];
  
  // 1. Find class block
  // This simplistic regex approach is getting stretched. 
  // We scan line by line to keep context. 
  
  const lines = content.split('\n');
  let currentClass: Partial<ActorImpl> | null = null;

  for (const line of lines) {
    // Check for decorators
    const actorMatch = line.match(/@(Actor|ActorModel)\s*\(\s*["']([^"']+)["']\s*\)/);
    if (actorMatch) {
      if (currentClass) impls.push(currentClass as ActorImpl); // Flush previous
      currentClass = { 
        modelName: actorMatch[2], 
        handlers: [], 
        implements: [], 
        file: filename,
        name: "Unknown" 
      };
    }

    const implMatch = line.match(/@Implements\s*\(([^)]+)\)/);
    if (implMatch && currentClass) {
      // Parse "Proto1", "Proto2"
      const protos = implMatch[1].match(/["']([^"']+)["']/g);
      if (protos) {
        currentClass.implements = protos.map(p => p.replace(/['"]/g, ''));
      }
    }

    const classMatch = line.match(/export\s+class\s+([A-Za-z0-9]+)/);
    if (classMatch && currentClass) {
      currentClass.name = classMatch[1];
    }

    const handlerMatch = line.match(/@Handler\s*\(\s*["']([^"']+)["']\s*\)/);
    if (handlerMatch && currentClass) {
      currentClass.handlers!.push(handlerMatch[1]);
    }
  }
  if (currentClass) impls.push(currentClass as ActorImpl);

  return impls;
}

// --- Main ---

async function main() {
  console.log("🔍 SEAG Consistency Linter (v3: Protocols)\n");

  const apDir = join(process.cwd(), "ap");
  const modelFiles = (await readdir(apDir)).filter(f => f.endsWith(".model.lisp"));
  
  const allModels: ActorModel[] = [];
  const allProtocols: Map<string, Protocol> = new Map();

  for (const file of modelFiles) {
    console.log(`Processing ${file}...`);
    const content = await readFile(join(apDir, file), "utf-8");
    try {
      const ast = parse(content);
      const { models, protocols } = extractDefinitions(ast, file);
      models.forEach(m => allModels.push(m));
      protocols.forEach(p => allProtocols.set(p.name, p));
    } catch (e) {
      console.error(`💥 Failed to parse model ${file}:`, e.message);
    }
  }

  const seagDir = join(process.cwd(), "seag");
  const codeFiles = (await readdir(seagDir)).filter(f => f.endsWith(".ts"));
  const impls: ActorImpl[] = [];

  for (const file of codeFiles) {
    const content = await readFile(join(seagDir, file), "utf-8");
    impls.push(...parseTsImpl(content, file));
  }

  let errors = 0;
  let warnings = 0;

  console.log(`Verifying ${allModels.length} Models & ${allProtocols.size} Protocols against ${impls.length} Implementations...\n`);

  // Debug: print found protocols
  // console.log("Protocols:", Array.from(allProtocols.keys()));

  for (const model of allModels) {
    const impl = impls.find(i => i.modelName === model.name);

    if (!impl) {
      console.error(`❌ Missing Implementation: Model '${model.name}' (${model.file}) not found in code.`);
      errors++;
      continue;
    }

    console.log(`✅ ${model.name} matched to class ${impl.name} in ${impl.file}`);

    // 1. Check Direct Handlers
    for (const handler of model.handlers) {
      if (!impl.handlers.includes(handler)) {
        console.warn(`   ⚠️  [${impl.name}] Missing Handler: (on ${handler} ...)`);
        warnings++;
      }
    }

    // 2. Check Protocol Compliance
    for (const protoName of model.implements) {
      const protocol = allProtocols.get(protoName);
      if (!protocol) {
        console.error(`   ❓ [${impl.name}] Implements unknown protocol '${protoName}'`);
        errors++;
        continue;
      }

      // Check if code declares it implements this protocol
      if (!impl.implements.includes(protoName)) {
        console.warn(`   ⚠️  [${impl.name}] Missing @Implements("${protoName}") decorator.`);
        warnings++;
      }

      // Check if handlers exist
      for (const input of protocol.inputs) {
        if (!impl.handlers.includes(input)) {
          console.error(`   ❌ [${impl.name}] Protocol '${protoName}' requires @Handler("${input}")`);
          errors++;
        }
      }
    }
  }

  console.log(`\n---\nResult: ${errors} Errors, ${warnings} Warnings`);
  
  if (errors > 0) {
    console.log("\n💡 Tip: Use @Actor('Name') and @Handler('MSG') in your code to link it to the model.");
    process.exit(1);
  }
}

main().catch(console.error);
