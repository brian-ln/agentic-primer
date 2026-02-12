#!/usr/bin/env bun
/**
 * Demo script for SubprocessCodeComputeActor
 *
 * Demonstrates:
 * 1. Basic code execution
 * 2. Console output capture
 * 3. Error handling
 * 4. **CRITICAL**: Timeout killing synchronous infinite loops
 */

import { MessageRouter } from '../router.ts';
import { SubprocessCodeComputeActor } from './compute/subprocess-code.ts';
import type { Message } from '@agentic-primer/actors';

const router = new MessageRouter();
const actor = new SubprocessCodeComputeActor(router, 5000);

async function runDemo(title: string, code: string, language = 'javascript', timeout = 5000) {
  console.log(`\n${'='.repeat(60)}`);
  console.log(`Demo: ${title}`);
  console.log(`${'='.repeat(60)}`);
  console.log(`Code:\n${code}\n`);

  const message: Message = {
    id: `demo-${Date.now()}`,
    from: 'demo',
    to: 'code-execution',
    type: 'execute',
    payload: { code, language, timeout },
    timestamp: Date.now(),
  };

  const startTime = Date.now();
  const response = await actor.receive(message);
  const elapsed = Date.now() - startTime;

  console.log(`Result (${elapsed}ms):`);
  if (response.success) {
    console.log(`✅ Success`);
    console.log(`   Result: ${JSON.stringify(response.payload.result)}`);
    if (response.payload.logs.length > 0) {
      console.log(`   Logs: ${response.payload.logs.join(', ')}`);
    }
    console.log(`   Execution time: ${response.payload.executionTime}ms`);
  } else {
    console.log(`❌ Error: ${response.error}`);
    if (response.payload?.logs?.length > 0) {
      console.log(`   Logs: ${response.payload.logs.join(', ')}`);
    }
    if (response.payload?.killedByTimeout) {
      console.log(`   ⚠️  Process was killed due to timeout`);
    }
  }
}

console.log('\n🚀 SubprocessCodeComputeActor Demo\n');

// Demo 1: Basic execution
await runDemo(
  'Basic Math',
  '2 + 2'
);

// Demo 2: Console output
await runDemo(
  'Console Output',
  `
console.log('Starting calculation...');
const result = 10 * 5;
console.log('Result:', result);
result
`
);

// Demo 3: Complex objects
await runDemo(
  'Complex Objects',
  `
const data = {
  name: 'Test',
  values: [1, 2, 3],
  nested: { x: 10 }
};
console.log('Processing data:', data);
data
`
);

// Demo 4: Error handling
await runDemo(
  'Error Handling',
  `throw new Error('Something went wrong!')`
);

// Demo 5: TypeScript support
await runDemo(
  'TypeScript Support',
  `
const greet = (name: string): string => {
  return 'Hello, ' + name;
};
greet('World')
`,
  'typescript'
);

// Demo 6: CRITICAL - Synchronous infinite loop with timeout
console.log('\n⚠️  CRITICAL TEST: Synchronous Infinite Loop');
console.log('This demonstrates the key advantage of subprocess isolation.');
console.log('The infinite loop WILL BE KILLED after timeout.\n');

await runDemo(
  'Infinite Loop (Will Timeout)',
  `
console.log('Starting infinite loop...');
while(true) {
  // This will run forever, but subprocess will be killed
}
`,
  'javascript',
  1000  // 1 second timeout
);

// Demo 7: CPU-intensive task with timeout
await runDemo(
  'CPU-Intensive Task (Will Timeout)',
  `
console.log('Starting CPU-intensive task...');
let sum = 0;
for (let i = 0; i < 100000000000; i++) {
  sum += i;
}
sum
`,
  'javascript',
  500  // 500ms timeout
);

// Demo 8: Security - blocked globals
await runDemo(
  'Security Test (Blocked Globals)',
  `
({
  process: typeof process,
  Bun: typeof Bun,
  fetch: typeof fetch,
  require: typeof require,
  Buffer: typeof Buffer,
})
`
);

console.log('\n✅ All demos completed!\n');
console.log('Key Takeaways:');
console.log('- Subprocess provides maximum isolation');
console.log('- Can kill synchronous infinite loops (unlike Worker)');
console.log('- Clean timeout handling with process termination');
console.log('- Secure sandbox with blocked dangerous globals');
console.log('- Suitable for production with untrusted code');
console.log('');
