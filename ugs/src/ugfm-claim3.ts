// ugfm-claim3.ts
// UGFM Claim 3 demonstration: actor graph → TypeScript actor boilerplate generator
// This is a projection function: it reads graph structure and synthesizes code.
// The graph IS the specification; this function IS the projection.

import { GraphStore } from './graph';

export interface GeneratedActor {
  className: string;
  role: string;
  messageTypes: string[];
  source: string;  // TypeScript source code for this actor
}

export interface GenerationResult {
  actors: GeneratedActor[];
  combinedSource: string;  // All actors in one TypeScript module
  actorCount: number;
  messageTypeCount: number;
}

// Absolute path to @agentic-primer/actors — used in generated files that run outside workspace
const ACTORS_PKG_PATH = new URL(
  '../../packages/actors/src/index.ts',
  import.meta.url
).pathname;

/**
 * Project an actor graph to runnable TypeScript classes extending Actor.
 *
 * Reads Actor nodes and MessageChannel edges from the GraphStore and
 * generates a TypeScript module with one class per actor. Each class
 * extends Actor and has an async handleMessage(message: Message) method
 * with real implementations that log and route messages.
 *
 * This is a pure function: same graph → identical output (deterministic).
 * No side effects beyond the returned GenerationResult.
 *
 * API notes (graph.ts):
 *   - store.getByType('Actor')  → Node[]  (sync)
 *   - store.adjacencyOut.get(nodeId)  → Array<{ to, edgeId, type, weight }>
 *   - store.edges.get(edgeId)  → Edge (has .properties Map<string, any>)
 *   - Node.properties  → Map<string, any>
 */
export function generateActorBoilerplate(store: GraphStore): GenerationResult {
  const actorNodes = store.getByType('Actor');

  // Build a lookup: nodeId → className (for routing comments)
  const nodeIdToClassName = new Map<string, string>();
  for (const node of actorNodes) {
    const name = String(node.properties.get('name') ?? node.id);
    nodeIdToClassName.set(node.id, toPascalCase(name) + 'Actor');
  }

  const actors: GeneratedActor[] = [];

  for (const node of actorNodes) {
    const name = String(node.properties.get('name') ?? node.id);
    const role = String(node.properties.get('role') ?? 'unknown');
    const className = toPascalCase(name) + 'Actor';

    // Resolve outgoing edges via adjacencyOut (public Map on GraphStore)
    // adjacencyOut entries: { to, edgeId, type, weight }
    const outAdjacency = store.adjacencyOut.get(node.id) ?? [];
    const messageChannelEdges = outAdjacency.filter(adj => adj.type === 'MessageChannel');

    const messageTypes = messageChannelEdges.map(adj => {
      const edge = store.edges.get(adj.edgeId);
      return String(edge?.properties.get('messageType') ?? 'UnknownMessage');
    });

    // Build routing info: messageType → destination className
    const routingMap = new Map<string, string>();
    for (const adj of messageChannelEdges) {
      const edge = store.edges.get(adj.edgeId);
      const msgType = String(edge?.properties.get('messageType') ?? 'UnknownMessage');
      const destClass = nodeIdToClassName.get(adj.to) ?? adj.to;
      routingMap.set(msgType, destClass);
    }

    const source = renderActorClass(className, role, messageTypes, routingMap);
    actors.push({ className, role, messageTypes, source });
  }

  const combinedSource = renderModule(actors);

  return {
    actors,
    combinedSource,
    actorCount: actors.length,
    messageTypeCount: actors.reduce((n, a) => n + a.messageTypes.length, 0),
  };
}

function toPascalCase(str: string): string {
  return str
    .replace(/[-_\s]+(.)/g, (_, c: string) => c.toUpperCase())
    .replace(/^(.)/, (c: string) => c.toUpperCase());
}

function renderActorClass(
  className: string,
  role: string,
  messageTypes: string[],
  routingMap: Map<string, string>
): string {
  let cases: string;

  if (messageTypes.length > 0) {
    cases = messageTypes
      .map(mt => {
        const dest = routingMap.get(mt);
        if (dest) {
          return [
            `      case '${mt}':`,
            `        // Routed to: ${dest}`,
            `        console.log('[${className}] routing ${mt} to ${dest}');`,
            `        break;`,
          ].join('\n');
        }
        return [
          `      case '${mt}':`,
          `        console.log('[${className}] handling ${mt}');`,
          `        break;`,
        ].join('\n');
      })
      .join('\n');
  } else {
    cases = `      // No outgoing message channels defined`;
  }

  return `/** Generated actor: role=${role} */
export class ${className} extends Actor {
  async handleMessage(message: Message): Promise<MessageResponse> {
    console.log(\`[${className}] received: \${message.type}\`, message.payload);
    switch (message.type) {
${cases}
      default:
        console.log(\`[${className}] unhandled: \${message.type}\`);
    }
    return {
      id: crypto.randomUUID(),
      correlationId: message.correlationId ?? message.id,
      from: this.address,
      to: message.from,
      success: true,
      timestamp: Date.now(),
    };
  }
}`;
}

function renderModule(actors: GeneratedActor[]): string {
  const header = `// Generated by UGFM Claim 3 demonstration — actor graph → TypeScript boilerplate
// This file is auto-generated. Do not edit manually.
// Source: ugfm-claim3.ts generateActorBoilerplate()

import { Actor } from '${ACTORS_PKG_PATH}';
import type { Message, MessageResponse } from '${ACTORS_PKG_PATH}';

`;
  return header + actors.map(a => a.source).join('\n\n') + '\n';
}
