#!/usr/bin/env bun
/**
 * Security Comparison: UnsafeCodeComputeActor vs WorkerCodeComputeActor
 *
 * Demonstrates the security vulnerabilities in UnsafeCodeComputeActor
 * and how WorkerCodeComputeActor blocks them.
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { UnsafeCodeComputeActor } from './src/messaging/actors/compute/unsafe-code.ts';
import { WorkerCodeComputeActor } from './src/messaging/actors/compute/worker-code.ts';
import { createMessage, generateCorrelationId } from './src/messaging/message.ts';

// CE-001: Constructor chain escape exploit
const CE001_EXPLOIT = `
const AsyncFunction = (async function(){}).constructor;
const fn = new AsyncFunction('return this.process || this.Bun');
const result = fn();
result ? 'COMPROMISED: Got parent process access' : 'blocked'
`;

// CE-002: Function.constructor escape exploit
const CE002_EXPLOIT = `
const fn = Function('return this');
const glob = fn();
const result = glob.process || glob.Bun;
result ? 'COMPROMISED: Got global access' : 'blocked'
`;

async function testActor(name: string, actor: any, exploitCode: string, exploitName: string) {
  const message = createMessage(
    actor.address,
    'execute',
    { code: exploitCode, language: 'javascript' },
    { pattern: 'ask', correlationId: generateCorrelationId() }
  );

  const response = await actor.receive(message);

  if (response.success) {
    const result = response.payload.result;
    if (typeof result === 'string' && result.includes('COMPROMISED')) {
      console.log(`  🔓 ${exploitName}: ${result}`);
      return 'vulnerable';
    } else if (typeof result === 'object' && result !== null) {
      console.log(`  🔓 ${exploitName}: COMPROMISED - Got object access`);
      return 'vulnerable';
    } else {
      console.log(`  ✅ ${exploitName}: Blocked (${result})`);
      return 'blocked';
    }
  } else {
    console.log(`  ✅ ${exploitName}: Blocked (execution failed)`);
    return 'blocked';
  }
}

async function main() {
  console.log('\n🔒 Security Comparison: Unsafe vs Worker Code Execution\n');

  // Setup
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);

  console.log('Testing UnsafeCodeComputeActor (Known Vulnerabilities):');
  console.log('━'.repeat(60));
  const unsafeActor = new UnsafeCodeComputeActor(router, 5000);
  const unsafeCE001 = await testActor('UnsafeCodeComputeActor', unsafeActor, CE001_EXPLOIT, 'CE-001');
  const unsafeCE002 = await testActor('UnsafeCodeComputeActor', unsafeActor, CE002_EXPLOIT, 'CE-002');

  console.log('\nTesting WorkerCodeComputeActor (Secure):');
  console.log('━'.repeat(60));
  const workerActor = new WorkerCodeComputeActor(router, 5000);
  const workerCE001 = await testActor('WorkerCodeComputeActor', workerActor, CE001_EXPLOIT, 'CE-001');
  const workerCE002 = await testActor('WorkerCodeComputeActor', workerActor, CE002_EXPLOIT, 'CE-002');

  console.log('\n' + '='.repeat(60));
  console.log('Summary:');
  console.log('='.repeat(60));
  console.log(`UnsafeCodeComputeActor: ${unsafeCE001 === 'vulnerable' || unsafeCE002 === 'vulnerable' ? '❌ VULNERABLE' : '✅ SECURE'}`);
  console.log(`WorkerCodeComputeActor: ${workerCE001 === 'blocked' && workerCE002 === 'blocked' ? '✅ SECURE' : '❌ VULNERABLE'}`);
  console.log('='.repeat(60));

  console.log('\n📝 Recommendation:');
  if (unsafeCE001 === 'vulnerable' || unsafeCE002 === 'vulnerable') {
    console.log('⚠️  UnsafeCodeComputeActor has known security vulnerabilities.');
    console.log('   Use only with trusted code or for demo/testing purposes.');
  }
  if (workerCE001 === 'blocked' && workerCE002 === 'blocked') {
    console.log('✅ WorkerCodeComputeActor successfully blocks constructor escapes.');
    console.log('   Safe for use with untrusted code in production.');
  }
  console.log();
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
