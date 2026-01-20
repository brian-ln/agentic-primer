import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";
import ts from "typescript";
import { parse, SExpr } from "../seag/lib/lisp-reader";

// --- Types ---
// (No changes to types)

// --- Lisp Extractor ---
// (No changes to extractDefinitions)

// --- TypeScript AST Visitor ---

function parseTsImplWithAST(content: string, filename: string): ActorImpl[] {
  const impls: ActorImpl[] = [];
  const sourceFile = ts.createSourceFile(
    filename,
    content,
    ts.ScriptTarget.ESNext,
    true
  );

  function visit(node: ts.Node) {
    if (ts.isClassDeclaration(node)) {
      const className = node.name?.getText(sourceFile);
      if (!className) return;

      let modelName = "";
      const implementedProtocols: string[] = [];
      const handlers: string[] = [];

      const decorators = ts.canHaveDecorators(node) ? ts.getDecorators(node) : undefined;
      
      decorators?.forEach(decorator => {
        const expr = decorator.expression;
        if (ts.isCallExpression(expr)) {
          const decoratorName = expr.expression.getText(sourceFile);
          const arg = expr.arguments[0];

          if ((decoratorName === "ActorModel" || decoratorName === "Actor") && ts.isStringLiteral(arg)) {
            modelName = arg.text;
          }
          if (decoratorName === "Implements" && ts.isStringLiteral(arg)) {
            implementedProtocols.push(arg.text);
          }
        }
      });

      // Find handlers within the class
      node.members.forEach(member => {
        if (ts.isMethodDeclaration(member)) {
          const memberDecorators = ts.canHaveDecorators(member) ? ts.getDecorators(member) : undefined;
          memberDecorators?.forEach(decorator => {
            const expr = decorator.expression;
            if (ts.isCallExpression(expr)) {
              const decoratorName = expr.expression.getText(sourceFile);
              const arg = expr.arguments[0];
              if (decoratorName === "Handler" && ts.isStringLiteral(arg)) {
                handlers.push(arg.text);
              }
            }
          });
        }
      });

      if (modelName) {
        impls.push({
          name: className,
          modelName: modelName,
          implements: implementedProtocols,
          handlers: [...new Set(handlers)],
          file: filename
        });
      }
    }
    ts.forEachChild(node, visit);
  }

  visit(sourceFile);
  return impls;
}

// --- Main ---

async function main() {
  console.log("🔍 SEAG Consistency Linter (v4: AST-based)\n");

  // ... (rest of main is the same, just change the call)    
  const seagDir = join(process.cwd(), "seag");
  const codeFiles = (await readdir(seagDir)).filter(f => f.endsWith(".ts"));
  const impls: ActorImpl[] = [];

  for (const file of codeFiles) {
    const content = await readFile(join(seagDir, file), "utf-8");
    // USE THE NEW PARSER
    impls.push(...parseTsImplWithAST(content, file));
  }
  
  // ... (rest of main is the same)
}

// Helper to re-insert the main function logic
async function run() {
  console.log("🔍 SEAG Consistency Linter (v4: AST-based)\n");
  
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
      console.error(`💥 Failed to parse model ${file}:`, (e as Error).message);
    }
  }

  const seagDir = join(process.cwd(), "seag");
  const codeFiles = (await readdir(seagDir)).filter(f => f.endsWith(".ts"));
  const impls: ActorImpl[] = [];

  for (const file of codeFiles) {
    const content = await readFile(join(seagDir, file), "utf-8");
    impls.push(...parseTsImplWithAST(content, file));
  }

  let errors = 0;
  let warnings = 0;

  console.log(`\nVerifying ${allModels.length} Models & ${allProtocols.size} Protocols against ${impls.length} Implementations...\n`);

  for (const model of allModels) {
    const impl = impls.find(i => i.modelName === model.name);

    if (!impl) {
      console.error(`❌ Missing Implementation: Model '${model.name}' (${model.file}) not found in code.`);
      errors++;
      continue;
    }

    console.log(`✅ ${model.name} matched to class ${impl.name} in ${impl.file}`);

    for (const handler of model.handlers) {
      if (!impl.handlers.includes(handler)) {
        console.warn(`   ⚠️  [${impl.name}] Missing Handler: (on ${handler} ...)`);
        warnings++;
      }
    }
    
    for (const protoName of model.implements) {
      const protocol = allProtocols.get(protoName);
      if (!protocol) {
        console.error(`   ❓ [${impl.name}] Implements unknown protocol '${protoName}'`);
        errors++;
        continue;
      }
      
      if (!impl.implements.includes(protoName)) {
        console.warn(`   ⚠️  [${impl.name}] Missing @Implements("${protoName}") decorator.`);
        warnings++;
      }
      
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

// Dummy extractDefinitions for type safety
interface Protocol { name: string; inputs: string[]; outputs: string[]; file: string; }
interface ActorModel { name: string; implements: string[]; handlers: string[]; file: string; }
interface ActorImpl { name: string; modelName: string; implements: string[]; handlers: string[]; file: string; }
function extractDefinitions(sexprs: SExpr[], filename: string): { models: ActorModel[], protocols: Protocol[] } {
  const models: ActorModel[] = [];
  const protocols: Protocol[] = [];

  function walk(node: SExpr) {
    if (Array.isArray(node)) {
      const [type, name, ...rest] = node;
      
      if ((type === "defprotocol" || type === "protocol") && typeof name === "string") {
        const inputs: string[] = [];
        const outputs: string[] = [];
        for (const item of rest) {
          if (Array.isArray(item)) {
            const head = item[0];
            if (head === "on" || head === "message") {
              const msg = item[1];
              if (typeof msg === "string") inputs.push(msg.toUpperCase().replace(/-/g, '_'));
              
              const scanYields = (node: any) => {
                if (!Array.isArray(node)) return;
                const type = node[0];
                if (["seq", "or", "any", "some", "opt", "one"].includes(type)) {
                  node.slice(1).forEach(scanYields);
                }
                if (type === "one" || type === "any" || type === "some" || type === "opt") {
                   const candidate = node[1];
                   if (typeof candidate === "string" && /^[A-Z]/.test(candidate)) {
                      outputs.push(candidate.toUpperCase().replace(/-/g, '_'));
                   }
                }
              };
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

        rest.forEach(item => {
          if (Array.isArray(item) && item[0] === "implements") {
            item.slice(1).forEach(p => typeof p === "string" && impls.push(p));
          }
        });

        models.push({ name, implements: impls, handlers: [...new Set(handlers)], file: filename });
      }

      node.forEach(child => {
        if (Array.isArray(child)) walk(child);
      });
    }
  }

  sexprs.forEach(walk);
  return { models, protocols };
}


run().catch(console.error);