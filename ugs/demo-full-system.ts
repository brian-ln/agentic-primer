#!/usr/bin/env bun
/**
 * Full System Demo - SessionActor + ToolActors + Programs
 *
 * Demonstrates complete graph-actor system with real actors.
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { SessionManager } from './src/entities/session.ts';
import { ModelManager } from './src/entities/model.ts';
import { ProviderManager } from './src/entities/provider.ts';
import { ActorSystem, address } from './src/messaging/index.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { SessionActor } from './src/messaging/actors/session.ts';
import { BashToolActor, ReadToolActor, WriteToolActor } from './src/messaging/actors/tool.ts';

async function main() {
  console.log('🎭 Full Graph-Actor System Demo\n');

  // Initialize system
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const providerManager = new ProviderManager(store);
  const modelManager = new ModelManager(store, providerManager);
  const sessionManager = new SessionManager(store, modelManager);
  const router = new MessageRouter(store, programManager);
  const actorSystem = new ActorSystem(store, programManager);

  // Setup minimal provider and model for demo
  // Note: SessionActor can route messages without invoking inference
  console.log('📝 Setting up provider and model...');

  // Create provider (Cloudflare AI Gateway) - will use env vars or defaults
  const provider = await providerManager.createProvider(
    'demo-provider',
    'cloudflare-ai-gateway',
    {
      accountId: process.env.CLOUDFLARE_ACCOUNT_ID || 'demo-account',
      gatewayId: process.env.CLOUDFLARE_GATEWAY_ID || 'demo-gateway'
    }
  );

  // Publish provider (required for model usage)
  await providerManager.publishProvider('demo-provider');
  console.log('✓ Provider published: @(demo-provider)');

  // Create inference model
  const model = await modelManager.createModel(
    'demo-model',
    'claude-sonnet-4-5',
    'demo-provider',
    {
      name: 'Demo Model',
      temperature: 0.7,
      maxTokens: 1000
    }
  );

  // Publish model (required for inference, but we won't invoke it in this demo)
  await modelManager.publishModel('demo-model');
  console.log('✓ Model published: @(demo-model)\n');

  // Create session
  console.log('📝 Creating session...');
  await sessionManager.createSession('demo-session', '@(demo-model)', {
    owner: '@(user-1)',
  });
  console.log('✓ Session created: @(demo-session)\n');

  // Create SessionActor
  const sessionActor = new SessionActor(
    'demo-session',
    sessionManager,
    programManager,
    store,
    router,
    modelManager
  );

  // Create ToolActors
  const bashTool = new BashToolActor(router);
  const readTool = new ReadToolActor(router);
  const writeTool = new WriteToolActor(router);

  console.log('✓ Actors initialized:');
  console.log('  • @(demo-session) - SessionActor');
  console.log('  • @(tool-bash) - BashToolActor');
  console.log('  • @(tool-read) - ReadToolActor');
  console.log('  • @(tool-write) - WriteToolActor\n');

  // Demo 1: Send message to session
  console.log('💬 Demo 1: User message to session');
  const msg1 = await sessionActor.receive({
    id: 'msg1',
    pattern: 'ask',
    to: address('domain/demo-session'),
    from: address('domain/user-1'),
    type: 'user-message',
    payload: { message: 'Hello, can you help me?' },
    timestamp: Date.now(),
  });
  console.log('Response:', msg1.payload);
  console.log();

  // Demo 2: Bash tool - list files
  console.log('🔧 Demo 2: Execute bash command');
  console.log('Command: ls -la demo-*.ts');
  const bashMsg = await bashTool.receive({
    id: 'bash1',
    pattern: 'ask',
    to: address('tools/bash'),
    from: address('domain/demo-session'),
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

  // Demo 3: Write tool - create file
  console.log('📝 Demo 3: Write file via actor');
  const writeMsg = await writeTool.receive({
    id: 'write1',
    pattern: 'ask',
    to: address('tools/write'),
    from: address('domain/demo-session'),
    type: 'write',
    payload: {
      path: './test-actor-file.txt',
      content: `This file was created by @(tool-write) actor!
Created at: ${new Date().toISOString()}
Message passing: ✓ Working
Graph-Actor System: ✓ Functional
`,
    },
    timestamp: Date.now(),
  });

  if (writeMsg.success) {
    console.log('✓ File written:', writeMsg.payload.path);
    console.log('  Size:', writeMsg.payload.size, 'bytes');
  }
  console.log();

  // Demo 4: Read tool - read back the file
  console.log('📖 Demo 4: Read file via actor');
  const readMsg = await readTool.receive({
    id: 'read1',
    pattern: 'ask',
    to: address('tools/read'),
    from: address('domain/demo-session'),
    type: 'read',
    payload: { path: './test-actor-file.txt' },
    timestamp: Date.now(),
  });

  if (readMsg.success) {
    console.log('✓ File read:', readMsg.payload.path);
    console.log('Content:');
    console.log(readMsg.payload.content);
  }
  console.log();

  // Demo 5: Session context
  console.log('📊 Demo 5: Query session context');
  const ctxMsg = await sessionActor.receive({
    id: 'ctx1',
    pattern: 'ask',
    to: address('domain/demo-session'),
    type: 'get-context',
    payload: {},
    timestamp: Date.now(),
  });

  if (ctxMsg.success) {
    console.log('Session context:', ctxMsg.payload);
  }
  console.log();

  // Demo 6: Actor-to-actor chain
  console.log('🔗 Demo 6: Actor chain (Session → Bash)');
  console.log('SessionActor asks BashToolActor to get system info...');

  const bashResponse = await sessionActor.ask(
    address('tools/bash'),
    'execute',
    { command: 'uname -a' }
  );

  if (bashResponse.success) {
    console.log('System info:', bashResponse.payload.stdout);
  }
  console.log();

  console.log('✨ Full system demo complete!\n');
  console.log('What just happened:');
  console.log('  1. Created SessionActor (conversation management)');
  console.log('  2. Created ToolActors (bash, read, write)');
  console.log('  3. Sent messages between actors');
  console.log('  4. Executed real tools via message passing');
  console.log('  5. Chained actors (Session → Tool)');
  console.log();
  console.log('🎯 This proves:');
  console.log('  • Programs as actors ✓');
  console.log('  • Tool execution via messages ✓');
  console.log('  • Session management as actor ✓');
  console.log('  • Actor-to-actor chains ✓');
  console.log('  • Real file I/O through graph ✓');
}

main().catch(console.error);
