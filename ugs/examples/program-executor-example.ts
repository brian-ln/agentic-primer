#!/usr/bin/env bun
/**
 * Example usage of ProgramExecutorActor
 */

import { MessageRouter } from '../src/messaging/router.ts';
import { ProgramExecutorActor } from '../src/messaging/actors/program-executor.ts';
import { address, createMessage, generateCorrelationId } from '../src/messaging/message.ts';

async function main() {
  const router = new MessageRouter();
  const actor = new ProgramExecutorActor(router);

  // Example 1: Simple echo
  console.log('Example 1: Echo command');
  const echoMsg = createMessage(
    address('services/program-executor'),
    'execute',
    {
      command: 'echo',
      args: ['Hello from ProgramExecutor!'],
    },
    {
      pattern: 'ask',
      from: address('domain/test'),
      correlationId: generateCorrelationId(),
    }
  );
  const echoResult = await actor.receive(echoMsg);
  console.log('  Result:', echoResult.payload.stdout.trim());
  console.log('  Exit code:', echoResult.payload.exitCode);
  console.log('  Duration:', echoResult.payload.duration, 'ms\n');

  // Example 2: List files
  console.log('Example 2: List files');
  const lsMsg = createMessage(
    address('services/program-executor'),
    'execute',
    {
      command: 'ls',
      args: ['-la'],
      cwd: process.cwd(),
    },
    {
      pattern: 'ask',
      from: address('domain/test'),
      correlationId: generateCorrelationId(),
    }
  );
  const lsResult = await actor.receive(lsMsg);
  const lines = lsResult.payload.stdout.trim().split('\n').slice(0, 3);
  console.log('  First 3 lines:');
  lines.forEach(line => console.log('   ', line));
  console.log('  Exit code:', lsResult.payload.exitCode, '\n');

  // Example 3: Environment variables
  console.log('Example 3: Environment variables');
  const envMsg = createMessage(
    address('services/program-executor'),
    'execute',
    {
      command: 'env',
      env: {
        CUSTOM_VAR: 'custom-value',
      },
    },
    {
      pattern: 'ask',
      from: address('domain/test'),
      correlationId: generateCorrelationId(),
    }
  );
  const envResult = await actor.receive(envMsg);
  const hasCustomVar = envResult.payload.stdout.includes('CUSTOM_VAR=custom-value');
  console.log('  Custom var injected:', hasCustomVar, '\n');

  // Example 4: Get stats
  console.log('Example 4: Actor stats');
  const stats = actor.getStats();
  console.log('  Running processes:', stats.runningProcesses);
  console.log('  Max concurrent:', stats.maxConcurrent, '\n');

  await actor.cleanup();
  console.log('All examples completed successfully!');
}

main().catch(console.error);
