#!/usr/bin/env bun
/**
 * Demo: Graph-Actor Message Layer
 *
 * Demonstrates message-based communication with UGS programs.
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { ActorSystem, address } from './src/messaging/index.ts';

async function main() {
  console.log('🎭 Graph-Actor Message Layer Demo\n');

  // Initialize UGS components
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const actorSystem = new ActorSystem(store, programManager);

  // Create a simple calculator program
  console.log('📝 Creating calculator program...');
  await programManager.createProgram(
    'calculator',
    `
    // Calculator program
    const { operation, a, b } = input.message;

    switch(operation) {
      case 'add': return a + b;
      case 'multiply': return a * b;
      case 'subtract': return a - b;
      default: throw new Error('Unknown operation: ' + operation);
    }
    `,
    {
      name: 'Calculator',
      description: 'Simple calculator program',
      inputSchema: {
        type: 'object',
        properties: {
          operation: { type: 'string', enum: ['add', 'multiply', 'subtract'] },
          a: { type: 'number' },
          b: { type: 'number' },
        },
      },
    }
  );

  await programManager.publishProgram('calculator');
  console.log('✓ Calculator published as @(calculator)\n');

  // Create an echo program (returns input)
  console.log('📝 Creating echo program...');
  await programManager.createProgram(
    'echo',
    `
    // Echo program - returns what you send
    return { echo: input.message, timestamp: Date.now() };
    `,
    {
      name: 'Echo',
      description: 'Returns input message',
    }
  );

  await programManager.publishProgram('echo');
  console.log('✓ Echo published as @(echo)\n');

  // Demo 1: Send message to calculator (ask pattern)
  console.log('🔢 Demo 1: Ask pattern with calculator');
  console.log('Sending: @(calculator) { operation: "add", a: 5, b: 3 }');

  const calcResponse = await actorSystem.send(
    address('demo/calculator'),
    'calculate',
    { operation: 'add', a: 5, b: 3 }
  );

  console.log('Response:', calcResponse);
  console.log(`Result: 5 + 3 = ${calcResponse.payload}\n`);

  // Demo 2: Multiple operations
  console.log('🔢 Demo 2: Multiple operations');

  const operations = [
    { op: 'multiply', a: 7, b: 6 },
    { op: 'subtract', a: 10, b: 4 },
  ];

  for (const { op, a, b } of operations) {
    const response = await actorSystem.send(
      address('demo/calculator'),
      'calculate',
      { operation: op, a, b }
    );
    console.log(`${op}(${a}, ${b}) = ${response.payload}`);
  }
  console.log();

  // Demo 3: Echo program (tell + ask)
  console.log('📢 Demo 3: Echo program');
  console.log('Sending: @(echo) "Hello from actor system"');

  const echoResponse = await actorSystem.send(
    address('demo/echo'),
    'echo',
    'Hello from actor system'
  );

  console.log('Response:', echoResponse.payload);
  console.log();

  // Demo 4: Document actor (query node data)
  console.log('📄 Demo 4: Document actor');
  console.log('Querying @(calculator) as document...');

  const docResponse = await actorSystem.send(
    address('demo/calculator'),
    'query',
    { fields: ['name', 'description', 'state'] }
  );

  console.log('Document:', docResponse.payload);
  console.log();

  // Demo 5: Actor-to-actor communication
  console.log('🎭 Demo 5: Actor-to-actor');
  console.log('Getting actor references...');

  const calcActor = actorSystem.actor('calculator');
  const echoActor = actorSystem.actor('echo');

  console.log('Calculator actor asking Echo actor...');
  const actorResponse = await calcActor.ask(
    address('demo/echo'),
    'echo',
    { message: 'Hello from calculator actor!' }
  );

  console.log('Echo response:', actorResponse.payload);
  console.log();

  // System stats
  console.log('📊 System Stats:');
  console.log(actorSystem.getStats());
  console.log();

  console.log('✨ Demo complete!\n');
  console.log('Key concepts demonstrated:');
  console.log('  • Programs as executable actors');
  console.log('  • Message-based communication (@(id) addressing)');
  console.log('  • Ask/Tell patterns');
  console.log('  • Document actors (query-only)');
  console.log('  • Actor-to-actor messaging');
  console.log();
  console.log('🎯 Next steps:');
  console.log('  • Add SessionActor (agent execution)');
  console.log('  • Add ToolActor (bash, file ops)');
  console.log('  • Add ChannelActor (WhatsApp, Telegram)');
  console.log('  • Add supervision and fault tolerance');
}

main().catch(console.error);
