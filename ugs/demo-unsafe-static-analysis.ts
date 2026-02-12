#!/usr/bin/env bun
/**
 * Demo: Optional Static Analysis in UnsafeCodeExecutionActor
 *
 * Shows three modes:
 * - 'off': No checks (fastest, ~0.1ms)
 * - 'warn': Log warnings (still executes, ~0.2ms)
 * - 'strict': Block dangerous code (prevents execution, ~0.2ms)
 */

import { MessageRouter } from './src/messaging/router.ts';
import { UnsafeCodeComputeActor } from './src/messaging/actors/compute/unsafe-code.ts';

console.log('🔒 UnsafeCodeComputeActor - Static Analysis Demo\n');
console.log('='.repeat(70));

// Dangerous code that tries constructor escape
const dangerousCode = `
  const AsyncFunction = (async function(){}).constructor;
  const func = AsyncFunction('return process');
  func();
`;

// Safe code
const safeCode = `
  const result = Math.sqrt(16) + Math.pow(2, 3);
  console.log('Calculation result:', result);
  result;
`;

const router = new MessageRouter();

// Mode 1: Off (no protection)
console.log('\n📍 Mode 1: staticAnalysis: "off" (no checks)\n');
const actorOff = new UnsafeCodeComputeActor(router, {
  timeout: 5000,
  iUnderstandThisIsUnsafe: true,
  staticAnalysis: 'off',
});

const result1 = await actorOff.receive({
  id: 'test1',
  type: 'execute',
  from: '@(demo)',
  to: '@(unsafe)',
  payload: { code: dangerousCode, timeout: 1000 },
  timestamp: Date.now(),
});

console.log('   Result:', result1.success ? '✓ Executed' : '✗ Failed');
if (result1.success) {
  console.log('   ⚠️  Dangerous code executed (no protection)');
} else {
  console.log('   Error:', result1.error);
}

// Mode 2: Warn (logs but allows)
console.log('\n📍 Mode 2: staticAnalysis: "warn" (logs warnings)\n');
const actorWarn = new UnsafeCodeComputeActor(router, {
  timeout: 5000,
  iUnderstandThisIsUnsafe: true,
  staticAnalysis: 'warn',
});

const result2 = await actorWarn.receive({
  id: 'test2',
  type: 'execute',
  from: '@(demo)',
  to: '@(unsafe)',
  payload: { code: dangerousCode, timeout: 1000 },
  timestamp: Date.now(),
});

console.log('   Result:', result2.success ? '✓ Executed' : '✗ Failed');
if (result2.success) {
  console.log('   ⚠️  Dangerous code executed with warnings');
  console.log('   Logs captured:', result2.payload?.logs?.length ?? 0);
} else {
  console.log('   Error:', result2.error);
}

// Mode 3: Strict (blocks execution)
console.log('\n📍 Mode 3: staticAnalysis: "strict" (blocks dangerous code)\n');
const actorStrict = new UnsafeCodeComputeActor(router, {
  timeout: 5000,
  iUnderstandThisIsUnsafe: true,
  staticAnalysis: 'strict',
});

const result3 = await actorStrict.receive({
  id: 'test3',
  type: 'execute',
  from: '@(demo)',
  to: '@(unsafe)',
  payload: { code: dangerousCode, timeout: 1000 },
  timestamp: Date.now(),
});

console.log('   Result:', result3.success ? '✓ Executed' : '✗ Blocked');
if (!result3.success) {
  console.log('   ✅ Dangerous code blocked!');
  console.log('   Reason:', result3.error?.split('\n')[0]);
} else {
  console.log('   ⚠️  Should have blocked but didn\'t');
}

// Mode 3 with safe code (should work)
console.log('\n📍 Mode 3: staticAnalysis: "strict" (safe code allowed)\n');

const result4 = await actorStrict.receive({
  id: 'test4',
  type: 'execute',
  from: '@(demo)',
  to: '@(unsafe)',
  payload: { code: safeCode, timeout: 1000 },
  timestamp: Date.now(),
});

console.log('   Result:', result4.success ? '✓ Executed' : '✗ Failed');
if (result4.success) {
  console.log('   ✅ Safe code executed successfully');
  console.log('   Output:', result4.payload?.result);
} else {
  console.log('   Error:', result4.error);
}

console.log('\n' + '='.repeat(70));
console.log('\n📊 Summary:\n');
console.log('   • staticAnalysis: "off"    → Fastest, no protection (0.1ms)');
console.log('   • staticAnalysis: "warn"   → Logs warnings, still executes (0.2ms)');
console.log('   • staticAnalysis: "strict" → Blocks dangerous patterns (0.2ms)');
console.log('\n   For real security, use WorkerCodeExecutionActor or SubprocessCodeExecutionActor.');
console.log('   Static analysis is defense-in-depth, NOT a replacement for isolation.\n');
