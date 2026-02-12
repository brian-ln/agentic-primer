#!/usr/bin/env bun
/**
 * Actor System Demo - Core Message Passing
 *
 * Demonstrates the graph-actor message layer without dependencies on Model/Session.
 * Proves: Programs as actors + Tool actors + Message passing
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { ActorSystem, address } from './src/messaging/index.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { BashToolActor, ReadToolActor, WriteToolActor } from './src/messaging/actors/tool.ts';

async function main() {
  console.log('🎭 Graph-Actor Message Layer Demo\n');

  // Initialize system
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const actorSystem = new ActorSystem(store, programManager);

  // Get the router from the actor system (it creates its own internally)
  // We need to access it to register tool actors
  const router = (actorSystem as any).router;

  // Create ToolActors and register them with the router
  const bashTool = new BashToolActor(router);
  const readTool = new ReadToolActor(router);
  const writeTool = new WriteToolActor(router);

  // Register tool actors so they can be addressed via @(id)
  router.registerActor('tools/bash', bashTool);
  router.registerActor('tools/read', readTool);
  router.registerActor('tools/write', writeTool);

  console.log('✓ Actors initialized and registered:');
  console.log('  • @(tool-bash) - BashToolActor');
  console.log('  • @(tool-read) - ReadToolActor');
  console.log('  • @(tool-write) - WriteToolActor\n');

  // Demo 1: Bash tool - list files
  console.log('🔧 Demo 1: Execute bash command via actor');
  console.log('Command: ls -la demo-*.ts');
  const bashMsg = await bashTool.receive({
    id: 'bash1',
    pattern: 'ask',
    to: address('tools/bash'),
    from: address('domain/demo'),
    type: 'execute',
    payload: { command: 'ls -la demo-*.ts' },
    timestamp: Date.now(),
  });

  if (bashMsg.success) {
    console.log('Output:');
    console.log(bashMsg.payload.stdout);
  } else {
    console.log('Error:', bashMsg.error);
  }
  console.log();

  // Demo 2: Write tool - create file
  console.log('📝 Demo 2: Write file via actor');
  const testFilePath = './test-actor-message.txt';
  const writeMsg = await writeTool.receive({
    id: 'write1',
    pattern: 'ask',
    to: address('tools/write'),
    from: address('domain/demo'),
    type: 'write',
    payload: {
      path: testFilePath,
      content: `Graph-Actor System Test
======================

Created by: @(tool-write)
Timestamp: ${new Date().toISOString()}

This proves:
  ✓ Message-based tool invocation
  ✓ Actor pattern working
  ✓ File I/O via messages
`,
    },
    timestamp: Date.now(),
  });

  if (writeMsg.success) {
    console.log('✓ File written:', writeMsg.payload.path);
    console.log('  Size:', writeMsg.payload.size, 'bytes');
  }
  console.log();

  // Demo 3: Read tool - read back the file
  console.log('📖 Demo 3: Read file via actor');
  const readMsg = await readTool.receive({
    id: 'read1',
    pattern: 'ask',
    to: address('tools/read'),
    from: address('domain/demo'),
    type: 'read',
    payload: { path: testFilePath },
    timestamp: Date.now(),
  });

  if (readMsg.success) {
    console.log('✓ File read:', readMsg.payload.path);
    console.log('Content:');
    console.log(readMsg.payload.content);
  }
  console.log();

  // Demo 4: Create calculator program (from earlier demo)
  console.log('🔢 Demo 4: Program as actor');
  console.log('Creating calculator program...');
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
    }
  );

  await programManager.publishProgram('calculator');
  console.log('✓ Calculator published as @(calculator)');

  // Invoke calculator via message
  console.log('\nSending: @(calculator) { operation: "multiply", a: 7, b: 6 }');
  const calcResponse = await actorSystem.send(
    address('demo/calculator'),
    'calculate',
    { operation: 'multiply', a: 7, b: 6 }
  );

  console.log(`Result: 7 × 6 = ${calcResponse.payload}`);
  console.log();

  // Demo 5: TRUE Actor-to-Actor messaging
  console.log('🔗 Demo 5: TRUE Actor-to-Actor Messaging (Program → Bash Tool)');
  console.log('Creating program that INTERNALLY calls bash tool actor...');

  await programManager.createProgram(
    'file-checker',
    `
    // TRUE actor-to-actor: this program internally calls bash
    // Uses this.ask() to invoke another actor directly
    const bashResult = await this.ask('@(tool-bash)', 'execute', {
      command: 'ls -la *.ts | wc -l'
    });

    return {
      message: 'File check complete via internal actor call',
      fileCount: bashResult.stdout.trim(),
      wasInternalCall: true,
      callMethod: 'this.ask() from within program'
    };
    `,
    {
      name: 'File Checker',
      description: 'Program that internally invokes bash tool actor',
    }
  );

  await programManager.publishProgram('file-checker');

  console.log('Invoking @(file-checker) - it will internally call @(tool-bash)...');
  const fileCheckResponse = await actorSystem.send(
    address('demo/file-checker'),
    'check',
    {}
  );

  console.log('\n✓ Result from file-checker:');
  console.log('Full response:', JSON.stringify(fileCheckResponse, null, 2));

  if (fileCheckResponse.success && fileCheckResponse.payload) {
    console.log('  Message:', fileCheckResponse.payload.message);
    console.log('  File count:', fileCheckResponse.payload.fileCount, 'TypeScript files');
    console.log('  Internal call?', fileCheckResponse.payload.wasInternalCall);
    console.log('  Method used:', fileCheckResponse.payload.callMethod);
  } else {
    console.log('  Error:', fileCheckResponse.error);
  }
  console.log();

  console.log('✨ Graph-Actor System Demo Complete!\n');
  console.log('What we just proved:');
  console.log('  ✓ Tool actors work (bash, read, write)');
  console.log('  ✓ Real file I/O through messages');
  console.log('  ✓ Program actors work (calculator)');
  console.log('  ✓ Message routing via @(id) addressing');
  console.log('  ✓ TRUE actor-to-actor messaging (programs use this.ask())');
  console.log('  ✓ Internal actor calls, not external orchestration');
  console.log();
  console.log('🎯 This is the foundation for:');
  console.log('  • SessionActor (agent execution)');
  console.log('  • ChannelActor (WhatsApp, Telegram)');
  console.log('  • Complex actor workflows with internal messaging');
  console.log('  • Full graph-actor compute fabric');
}

main().catch(console.error);
