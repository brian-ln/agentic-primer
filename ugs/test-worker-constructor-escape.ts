#!/usr/bin/env bun
/**
 * Test if Bun's Web Workers actually prevent constructor escapes
 * This tests CE-001 and CE-002 vulnerabilities
 */

import { MessageRouter } from './src/messaging/router.ts';
import { WorkerCodeComputeActor } from './src/messaging/actors/compute/worker-code.ts';

const router = new MessageRouter();
const actor = new WorkerCodeComputeActor(router, 5000);

console.log('Testing Constructor Escapes in Bun Web Workers\n');
console.log('='.repeat(60));

// Test 1: Constructor chain escape (CE-001)
console.log('\n🧪 Test 1: Constructor Chain Escape');
const test1Code = `
  const AsyncFunction = (async function(){}).constructor;
  const func = AsyncFunction('return process');
  func();  // Returns process object if escape works
`;

const result1 = await actor.receive({
  id: 'test1',
  type: 'execute',
  from: '@(test)',
  to: '@(worker)',
  payload: { code: test1Code, timeout: 2000 },
  timestamp: Date.now(),
});

console.log('Success:', result1.success);
if (result1.success) {
  const hasProcess = result1.payload?.result?.env !== undefined;
  if (hasProcess) {
    console.log('🚨 VULNERABLE: Constructor escape works! Got process.env');
    console.log('   Keys:', Object.keys(result1.payload.result.env).slice(0, 5));
  } else {
    console.log('✅ SAFE: Constructor escape blocked');
    console.log('   Result:', result1.payload?.result);
  }
} else {
  console.log('✅ SAFE: Execution failed (blocked)');
  console.log('   Error:', result1.error);
}

// Test 2: Function.constructor escape (CE-002)
console.log('\n🧪 Test 2: Function.constructor Escape');
const test2Code = `
  const FunctionConstructor = (function(){}).constructor.constructor;
  FunctionConstructor('return this')();  // Returns global if escape works
`;

const result2 = await actor.receive({
  id: 'test2',
  type: 'execute',
  from: '@(test)',
  to: '@(worker)',
  payload: { code: test2Code, timeout: 2000 },
  timestamp: Date.now(),
});

console.log('Success:', result2.success);
if (result2.success) {
  const hasGlobals = result2.payload?.result?.Bun !== undefined ||
                     result2.payload?.result?.process !== undefined;
  if (hasGlobals) {
    console.log('🚨 VULNERABLE: Function.constructor escape works!');
    console.log('   Has Bun?:', result2.payload?.result?.Bun !== undefined);
    console.log('   Has process?:', result2.payload?.result?.process !== undefined);
  } else {
    console.log('✅ SAFE: Function.constructor escape blocked');
    console.log('   Result type:', typeof result2.payload?.result);
  }
} else {
  console.log('✅ SAFE: Execution failed (blocked)');
  console.log('   Error:', result2.error);
}

// Test 3: Direct global access
console.log('\n🧪 Test 3: Direct Global Access Test');
const test3Code = `
  ({
    hasBun: typeof Bun !== 'undefined',
    hasProcess: typeof process !== 'undefined',
    hasGlobalThis: typeof globalThis !== 'undefined',
    globals: typeof globalThis === 'undefined' ? [] : Object.keys(globalThis).slice(0, 10)
  });
`;

const result3 = await actor.receive({
  id: 'test3',
  type: 'execute',
  from: '@(test)',
  to: '@(worker)',
  payload: { code: test3Code, timeout: 2000 },
  timestamp: Date.now(),
});

if (result3.success) {
  const info = result3.payload?.result;
  console.log('Has Bun:', info?.hasBun);
  console.log('Has process:', info?.hasProcess);
  console.log('Has globalThis:', info?.hasGlobalThis);
  console.log('Global keys:', info?.globals);
}

console.log('\n' + '='.repeat(60));
console.log('\n📊 Verdict:');
console.log('This test shows the ACTUAL security properties of Bun Workers.');
console.log('Based on results above, update documentation accordingly.\n');

process.exit(0);
