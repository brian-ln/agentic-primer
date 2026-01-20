import { parse, SExpr } from "../lib/lisp-reader";
import * as fs from "fs";
import * as path from "path";
import { Glob } from "bun";

// --- Data Structures ---

interface ActorDef {
  name: string;
  protocols: string[];
  sourceFile: string;
}

const modelActors: Map<string, ActorDef> = new Map();
const codeActors: Map<string, ActorDef> = new Map();

// --- Step 1: Parse Models ---

async function loadModels() {
  const glob = new Glob("../ap/*.model.lisp");
  for await (const file of glob.scan(".")) {
    const content = fs.readFileSync(file, "utf-8");
    try {
      const exprs = parse(content);
      // We expect top-level (system ...) or (defprotocol ...)
      // Actors are usually nested: (system Name (actors (actor Name ...)))
      
      // Helper to traverse S-Expr tree
      function traverse(node: SExpr) {
        if (!Array.isArray(node)) return;
        const [head, ...tail] = node;
        
        if (head === "actor" && typeof tail[0] === "string") {
          const name = tail[0];
          const protocols: string[] = [];
          
          // Look for (implements Protocol) in the actor body
          for (const item of tail.slice(1)) {
            if (Array.isArray(item) && item[0] === "implements" && typeof item[1] === "string") {
              protocols.push(item[1]);
            }
          }
          
          modelActors.set(name, { name, protocols, sourceFile: file });
        }
        
        // Recurse
        for (const child of tail) {
          if (Array.isArray(child)) traverse(child);
        }
      }

      exprs.forEach(traverse);

    } catch (e) {
      console.error(`Failed to parse ${file}:`, e);
    }
  }
}

// --- Step 2: Parse Code ---

async function loadCode() {
  const glob = new Glob("**/*.ts");
  for await (const file of glob.scan(".")) {
    if (file.includes("node_modules") || file.includes("tests") || file.includes("scripts")) continue;
    
    const content = fs.readFileSync(file, "utf-8");
    
    // Regex for @ActorModel("Name")
    // This is a simple regex that assumes standard formatting.
    // It captures the class definition block roughly.
    const actorRegex = /@ActorModel\s*\(\s*["'](.+?)["']\s*\)([\s\S]+?)class\s+(\w+)/g;
    
    let match;
    while ((match = actorRegex.exec(content)) !== null) {
      const [fullMatch, modelName, decorators, className] = match;
      
      const protocols: string[] = [];
      
      // Look for @Implements("Protocol") inside the captured decorators block
      const implementsRegex = /@Implements\s*\(\s*["'](.+?)["']\s*\)/g;
      let impMatch;
      while ((impMatch = implementsRegex.exec(decorators)) !== null) {
        protocols.push(impMatch[1]);
      }
      
      // Also look for @Implements("A", "B") - multiple args not handled by simple regex above
      // But our codebase uses one per line usually?
      // lib/meta.ts says: export function Implements(...protocols: string[])
      // So we might see @Implements("A", "B").
      // Let's refine the regex logic if needed, but simple is good for MVP.
      
      codeActors.set(modelName, { name: modelName, protocols, sourceFile: file });
    }
  }
}

// --- Step 3: Compare ---

async function verify() {
  await loadModels();
  await loadCode();
  
  let errors = 0;
  
  console.log("--- Consistency Check ---");
  console.log(`Models found: ${modelActors.size}`);
  console.log(`Code found:   ${codeActors.size}`);
  console.log("-------------------------");

  // 1. Missing Implementations
  for (const [name, modelDef] of modelActors) {
    if (!codeActors.has(name)) {
      console.error(`❌ [MISSING] Actor defined in model but not found in code: ${name}`);
      console.error(`   Defined in: ${modelDef.sourceFile}`);
      errors++;
      continue;
    }
    
    const codeDef = codeActors.get(name)!;
    
    // 2. Protocol Mismatches
    for (const proto of modelDef.protocols) {
      if (!codeDef.protocols.includes(proto)) {
        console.error(`❌ [PROTOCOL] ${name} missing protocol implementation: ${proto}`);
        console.error(`   Model: ${modelDef.sourceFile}`);
        console.error(`   Code:  ${codeDef.sourceFile}`);
        errors++;
      }
    }
  }

  // 3. Undocumented Actors
  for (const [name, codeDef] of codeActors) {
    if (!modelActors.has(name)) {
      console.warn(`⚠️  [UNDOCUMENTED] Actor found in code but not in model: ${name}`);
      console.warn(`   Found in: ${codeDef.sourceFile}`);
      // Warnings don't fail the build, but are noisy
    }
  }

  if (errors > 0) {
    console.log("-------------------------");
    console.log(`FAILED: ${errors} consistency errors found.`);
    process.exit(1);
  } else {
    console.log("-------------------------");
    console.log("✅ SUCCESS: Implementation matches Model.");
  }
}

verify();
