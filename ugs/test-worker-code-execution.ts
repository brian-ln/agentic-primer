#!/usr/bin/env bun
/**
 * Test WorkerCodeComputeActor - Verify secure code execution
 *
 * Tests:
 * 1. Basic code execution
 * 2. Console output capture
 * 3. Error handling
 * 4. Timeout enforcement
 * 5. CE-001: Constructor chain escape (MUST BE BLOCKED)
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { WorkerCodeComputeActor } from './src/messaging/actors/compute/worker-code.ts';
import { createMessage, generateCorrelationId } from './src/messaging/message.ts';

async function runTest(name: string, testFn: () => Promise<void>) {
  try {
    await testFn();
    console.log(`✅ ${name}`);
    return true;
  } catch (error: any) {
    console.error(`❌ ${name}`);
    console.error(`   ${error.message}`);
    return false;
  }
}

async function main() {
  console.log('\n🧪 Testing WorkerCodeComputeActor\n');

  // Setup
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);
  const actor = new WorkerCodeComputeActor(router, 5000);

  // Register actor with router
  (router as any).customActors = new Map();
  (router as any).customActors.set('code-execution', actor);

  let passedTests = 0;
  let totalTests = 0;

  // Test 1: Basic execution
  totalTests++;
  if (await runTest('Basic code execution', async () => {
    const message = createMessage(
      actor.address,
      'execute',
      { code: '2 + 2', language: 'javascript' },
      { pattern: 'ask', correlationId: generateCorrelationId() }
    );

    const response = await actor.receive(message);

    if (!response.success) {
      throw new Error(`Execution failed: ${response.error}`);
    }
    if (response.payload.result !== 4) {
      throw new Error(`Expected 4, got ${response.payload.result}`);
    }
  })) passedTests++;

  // Test 2: Console output capture
  totalTests++;
  if (await runTest('Console output capture', async () => {
    const message = createMessage(
      actor.address,
      'execute',
      {
        code: 'console.log("Hello"); console.error("Error"); "done"',
        language: 'javascript'
      },
      { pattern: 'ask', correlationId: generateCorrelationId() }
    );

    const response = await actor.receive(message);

    if (!response.success) {
      throw new Error(`Execution failed: ${response.error}`);
    }
    if (!response.payload.logs.includes('Hello')) {
      throw new Error('Missing log output');
    }
    if (!response.payload.logs.some((log: string) => log.includes('ERROR'))) {
      throw new Error('Missing error output');
    }
  })) passedTests++;

  // Test 3: Error handling
  totalTests++;
  if (await runTest('Error handling', async () => {
    const message = createMessage(
      actor.address,
      'execute',
      { code: 'throw new Error("Test error")', language: 'javascript' },
      { pattern: 'ask', correlationId: generateCorrelationId() }
    );

    const response = await actor.receive(message);

    if (response.success) {
      throw new Error('Expected execution to fail');
    }
    if (!response.error?.includes('Test error')) {
      throw new Error('Error message not captured');
    }
  })) passedTests++;

  // Test 4: Timeout enforcement
  totalTests++;
  if (await runTest('Timeout enforcement', async () => {
    const message = createMessage(
      actor.address,
      'execute',
      {
        code: 'while(true) {}', // Infinite loop
        language: 'javascript',
        timeout: 1000
      },
      { pattern: 'ask', correlationId: generateCorrelationId() }
    );

    const response = await actor.receive(message);

    if (response.success) {
      throw new Error('Expected timeout');
    }
    if (!response.error?.includes('timed out')) {
      throw new Error(`Expected timeout error, got: ${response.error}`);
    }
  })) passedTests++;

  // Test 5: CE-001 Constructor chain escape (MUST BE BLOCKED)
  totalTests++;
  if (await runTest('CE-001: Constructor chain escape blocked', async () => {
    const exploitCode = `
      // Try to access parent process via async function constructor
      const AsyncFunction = (async function(){}).constructor;
      const fn = new AsyncFunction('return this.process || this.Bun || "blocked"');
      fn()
    `;

    const message = createMessage(
      actor.address,
      'execute',
      { code: exploitCode, language: 'javascript' },
      { pattern: 'ask', correlationId: generateCorrelationId() }
    );

    const response = await actor.receive(message);

    // Should either fail or return "blocked"
    if (response.success) {
      const result = response.payload.result;
      if (result !== 'blocked' && typeof result === 'object' && result !== null) {
        throw new Error(`SECURITY BREACH: Constructor escape succeeded! Got: ${JSON.stringify(result)}`);
      }
      // Success with "blocked" result is acceptable
    }
    // Failure is also acceptable (means the escape was caught)
  })) passedTests++;

  // Test 6: CE-002 Function.constructor escape (MUST BE BLOCKED)
  totalTests++;
  if (await runTest('CE-002: Function.constructor escape blocked', async () => {
    const exploitCode = `
      // Try to access global scope via Function constructor
      const fn = Function('return this');
      const glob = fn();
      glob.process || glob.Bun || "blocked"
    `;

    const message = createMessage(
      actor.address,
      'execute',
      { code: exploitCode, language: 'javascript' },
      { pattern: 'ask', correlationId: generateCorrelationId() }
    );

    const response = await actor.receive(message);

    // Should either fail or return "blocked"
    if (response.success) {
      const result = response.payload.result;
      if (result !== 'blocked' && typeof result === 'object' && result !== null) {
        throw new Error(`SECURITY BREACH: Function.constructor escape succeeded! Got: ${JSON.stringify(result)}`);
      }
    }
  })) passedTests++;

  // Test 7: TypeScript support
  totalTests++;
  if (await runTest('TypeScript code execution', async () => {
    const message = createMessage(
      actor.address,
      'execute',
      {
        code: 'const x: number = 42; x * 2',
        language: 'typescript'
      },
      { pattern: 'ask', correlationId: generateCorrelationId() }
    );

    const response = await actor.receive(message);

    if (!response.success) {
      throw new Error(`Execution failed: ${response.error}`);
    }
    if (response.payload.result !== 84) {
      throw new Error(`Expected 84, got ${response.payload.result}`);
    }
  })) passedTests++;

  // Results
  console.log(`\n${'='.repeat(50)}`);
  console.log(`📊 Results: ${passedTests}/${totalTests} tests passed`);
  console.log(`${'='.repeat(50)}\n`);

  if (passedTests === totalTests) {
    console.log('🎉 All tests passed! WorkerCodeComputeActor is secure.\n');
    process.exit(0);
  } else {
    console.log('❌ Some tests failed. Review the implementation.\n');
    process.exit(1);
  }
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
