#!/usr/bin/env bun
/**
 * Demo: UnsafeCodeComputeActor
 *
 * Demonstrates safe code execution with sandboxing and timeout protection.
 * Shows safe operations, timeout enforcement, and security measures.
 */

import { MessageRouter } from './src/messaging/router.ts';
import { UnsafeCodeComputeActor } from './src/messaging/actors/compute/unsafe-code.ts';
import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';

async function demo() {
  console.log('=== UnsafeCodeComputeActor Demo ===\n');

  // Setup
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);

  // Create and register UnsafeCodeComputeActor with 5 second default timeout
  const codeActor = new UnsafeCodeComputeActor(router, { timeout: 5000, iUnderstandThisIsUnsafe: true });
  router.registerActor('test/code-execution', codeActor);

  console.log('✓ UnsafeCodeComputeActor initialized with 5s timeout');
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
        return result;
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (mathResponse.success) {
    console.log('✓ Execution successful');
    console.log('  Result:', mathResponse.payload?.result);
    console.log('  Execution time:', mathResponse.payload?.executionTime, 'ms');
    console.log('  Console output:');
    mathResponse.payload?.logs.forEach((log: string) => {
      console.log('    ', log);
    });
  } else {
    console.log('✗ Execution failed:', mathResponse.error);
  }
  console.log();

  // Demo 2: Date operations
  console.log('2. Executing Date operations...');
  const dateResponse = await codeActor.receive({
    id: 'msg_2',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        const now = new Date();
        const year = now.getFullYear();
        console.log('Current year:', year);
        return { year, timestamp: now.getTime() };
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (dateResponse.success) {
    console.log('✓ Execution successful');
    console.log('  Result:', JSON.stringify(dateResponse.payload?.result));
    console.log('  Console output:');
    dateResponse.payload?.logs.forEach((log: string) => {
      console.log('    ', log);
    });
  } else {
    console.log('✗ Execution failed:', dateResponse.error);
  }
  console.log();

  // Demo 3: Array and Object operations
  console.log('3. Executing Array/Object operations...');
  const arrayResponse = await codeActor.receive({
    id: 'msg_3',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        const numbers = [1, 2, 3, 4, 5];
        const doubled = numbers.map(n => n * 2);
        const sum = doubled.reduce((a, b) => a + b, 0);
        console.log('Original:', numbers);
        console.log('Doubled:', doubled);
        console.log('Sum:', sum);
        return sum;
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (arrayResponse.success) {
    console.log('✓ Execution successful');
    console.log('  Result:', arrayResponse.payload?.result);
    console.log('  Console output:');
    arrayResponse.payload?.logs.forEach((log: string) => {
      console.log('    ', log);
    });
  } else {
    console.log('✗ Execution failed:', arrayResponse.error);
  }
  console.log();

  // Demo 4: Large computation (finite, but takes time)
  console.log('4. Testing execution with large computation...');
  const largeResponse = await codeActor.receive({
    id: 'msg_4',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        console.log('Computing sum of large array...');
        let sum = 0;
        for (let i = 0; i < 10000000; i++) {
          sum += i;
        }
        console.log('Computation complete');
        return sum;
      `,
      language: 'javascript',
      timeout: 5000,
    },
    timestamp: Date.now(),
  });

  if (largeResponse.success) {
    console.log('✓ Large computation completed successfully');
    console.log('  Result:', largeResponse.payload?.result);
    console.log('  Execution time:', largeResponse.payload?.executionTime, 'ms');
    console.log('  Console output:');
    largeResponse.payload?.logs.forEach((log: string) => {
      console.log('    ', log);
    });
  } else {
    console.log('✗ Execution failed:', largeResponse.error);
  }
  console.log();

  // Demo 5: Sandboxing - attempt file system access
  console.log('5. Testing sandboxing (file system access attempt)...');
  const fsResponse = await codeActor.receive({
    id: 'msg_5',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        console.log('Attempting to access file system...');
        const fs = require('fs');
        return fs.readFileSync('/etc/passwd', 'utf-8');
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (fsResponse.success) {
    console.log('✗ Security vulnerability! Sandbox failed - file system accessed!');
  } else {
    console.log('✓ Sandbox protection working correctly');
    console.log('  Error:', fsResponse.error);
  }
  console.log();

  // Demo 6: Sandboxing - attempt process access
  console.log('6. Testing sandboxing (process access attempt)...');
  const processResponse = await codeActor.receive({
    id: 'msg_6',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        console.log('Attempting to access process...');
        return process.env;
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (processResponse.success) {
    console.log('✗ Security vulnerability! Sandbox failed - process accessed!');
  } else {
    console.log('✓ Sandbox protection working correctly');
    console.log('  Error:', processResponse.error);
  }
  console.log();

  // Demo 7: Sandboxing - attempt network access
  console.log('7. Testing sandboxing (network access attempt)...');
  const networkResponse = await codeActor.receive({
    id: 'msg_7',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        console.log('Attempting network request...');
        const response = await fetch('https://example.com');
        return response.text();
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  });

  if (networkResponse.success) {
    console.log('✗ Security vulnerability! Sandbox failed - network accessed!');
  } else {
    console.log('✓ Sandbox protection working correctly');
    console.log('  Error:', networkResponse.error);
  }
  console.log();

  // Demo 8: TypeScript execution
  console.log('8. Executing TypeScript code...');
  const tsResponse = await codeActor.receive({
    id: 'msg_8',
    pattern: 'ask',
    to: '@(code-execution)',
    from: '@(demo)',
    type: 'execute',
    payload: {
      code: `
        const multiply = (a: number, b: number): number => {
          return a * b;
        };
        console.log('Multiplying 6 * 7');
        return multiply(6, 7);
      `,
      language: 'typescript',
    },
    timestamp: Date.now(),
  });

  if (tsResponse.success) {
    console.log('✓ TypeScript execution successful');
    console.log('  Result:', tsResponse.payload?.result);
    console.log('  Console output:');
    tsResponse.payload?.logs.forEach((log: string) => {
      console.log('    ', log);
    });
  } else {
    console.log('✗ Execution failed:', tsResponse.error);
  }
  console.log();

  console.log('=== Demo Complete ===');
}

demo().catch(console.error);
