#!/usr/bin/env bun
/**
 * Demo: WorkerCodeComputeActor
 *
 * Demonstrates Web Worker-based code execution with better isolation and
 * infinite loop protection. Shows the advantages over UnsafeCodeComputeActor.
 */

import { MessageRouter } from './src/messaging/router.ts';
import { WorkerCodeComputeActor } from './src/messaging/actors/compute/worker-code.ts';
import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';

async function demo() {
  console.log('=== WorkerCodeComputeActor Demo ===\n');

  // Setup
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);

  // Create WorkerCodeComputeActor with 5 second default timeout
  const codeActor = new WorkerCodeComputeActor(router, 5000);

  console.log('✓ WorkerCodeComputeActor initialized with 5s timeout');
  console.log('✓ Each execution spawns a new isolated Worker thread');
  console.log();

  // Demo 1: Simple Math operations
  console.log('1. Executing simple Math operations...');
  const mathResponse = await codeActor.receive({
    id: 'msg_1',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        const result = Math.sqrt(16) + Math.pow(2, 3);
        console.log('Square root of 16:', Math.sqrt(16));
        console.log('2 to the power of 3:', Math.pow(2, 3));
        result;
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (mathResponse.success) {
    console.log('✓ Execution successful');
    console.log('  Result:', mathResponse.payload.result);
    console.log('  Logs:', mathResponse.payload.logs);
    console.log('  Time:', mathResponse.payload.executionTime + 'ms');
  } else {
    console.log('✗ Execution failed:', mathResponse.error);
  }
  console.log();

  // Demo 2: Console output capture
  console.log('2. Testing console output capture...');
  const consoleResponse = await codeActor.receive({
    id: 'msg_2',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        console.log('Hello from Worker!');
        console.warn('This is a warning');
        console.error('This is an error');
        console.info('This is info');
        'done';
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (consoleResponse.success) {
    console.log('✓ Console capture working');
    console.log('  Captured logs:');
    consoleResponse.payload.logs.forEach((log: string) => {
      console.log('    -', log);
    });
  } else {
    console.log('✗ Execution failed:', consoleResponse.error);
  }
  console.log();

  // Demo 3: Infinite loop protection (ADVANTAGE over UnsafeCodeComputeActor)
  console.log('3. Testing infinite loop protection...');
  console.log('  Starting infinite loop with 1s timeout...');
  const startTime = Date.now();
  const loopResponse = await codeActor.receive({
    id: 'msg_3',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: 'while(true) { /* infinite loop */ }',
      language: 'javascript',
      timeout: 1000,
    },
    timestamp: Date.now(),
  });
  const elapsed = Date.now() - startTime;

  if (!loopResponse.success) {
    console.log('✓ Infinite loop terminated correctly');
    console.log('  Error:', loopResponse.error);
    console.log('  Terminated after:', elapsed + 'ms');
    console.log('  ✅ This is a KEY ADVANTAGE over UnsafeCodeComputeActor!');
  } else {
    console.log('✗ Expected timeout but execution succeeded');
  }
  console.log();

  // Demo 4: Error handling
  console.log('4. Testing error handling...');
  const errorResponse = await codeActor.receive({
    id: 'msg_4',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: 'throw new Error("Intentional error for testing")',
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (!errorResponse.success) {
    console.log('✓ Error handled correctly');
    console.log('  Error:', errorResponse.error);
  } else {
    console.log('✗ Expected error but execution succeeded');
  }
  console.log();

  // Demo 5: TypeScript support
  console.log('5. Testing TypeScript support...');
  const tsResponse = await codeActor.receive({
    id: 'msg_5',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        const greet = (name: string): string => {
          return 'Hello, ' + name;
        };
        greet('Worker');
      `,
      language: 'typescript',
    },
    timestamp: Date.now(),
  });

  if (tsResponse.success) {
    console.log('✓ TypeScript execution successful');
    console.log('  Result:', tsResponse.payload.result);
  } else {
    console.log('✗ TypeScript execution failed:', tsResponse.error);
  }
  console.log();

  // Demo 6: Prototype pollution protection
  console.log('6. Testing prototype pollution protection...');
  console.log('  Attempting to pollute Object.prototype in first execution...');
  const pollute1 = await codeActor.receive({
    id: 'msg_6a',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        Object.prototype.polluted = 'YES';
        'pollution attempt 1';
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  console.log('  First execution:', pollute1.success ? 'completed' : 'failed');

  console.log('  Checking for pollution in second execution...');
  const pollute2 = await codeActor.receive({
    id: 'msg_6b',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        const obj = {};
        obj.polluted || 'CLEAN';
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (pollute2.success && pollute2.payload.result === 'CLEAN') {
    console.log('✓ Prototype pollution blocked!');
    console.log('  Worker termination prevents pollution from persisting');
    console.log('  ✅ This is a KEY ADVANTAGE over UnsafeCodeComputeActor!');
  } else {
    console.log('✗ Prototype pollution detected:', pollute2.payload.result);
  }
  console.log();

  // Summary
  console.log('='.repeat(60));
  console.log('Summary: WorkerCodeComputeActor Advantages');
  console.log('='.repeat(60));
  console.log('✅ Can terminate infinite loops (UnsafeCodeComputeActor cannot)');
  console.log('✅ Prevents prototype pollution (worker terminates)');
  console.log('✅ Better resource isolation (separate thread)');
  console.log('✅ Automatic cleanup (no state leaks)');
  console.log();
  console.log('Trade-offs:');
  console.log('⚠️  Slower (~10-50ms overhead vs ~0.1ms for UnsafeCodeComputeActor)');
  console.log('⚠️  Limited isolation in Bun (constructor escapes may still work)');
  console.log();
  console.log('Use WorkerCodeComputeActor when:');
  console.log('  - Infinite loop protection is needed');
  console.log('  - Prototype pollution prevention is important');
  console.log('  - Better resource isolation is desired');
  console.log('  - Code is semi-trusted or from known sources');
  console.log();
  console.log('For maximum security with untrusted code:');
  console.log('  - Use SubprocessCodeComputeActor (when implemented)');
  console.log('='.repeat(60));
}

demo().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
