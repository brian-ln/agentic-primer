/**
 * FunctionRegistryActor Demo
 *
 * Demonstrates how to use the FunctionRegistryActor to:
 * - Register functions manually
 * - Auto-discover functions from directory
 * - Query and list functions
 * - Handle UAP messages
 */

import FunctionRegistryActor from '../src/actors/function-registry.js';
import { PROTOCOLS, ACTIONS } from '../src/protocol.js';
import { resolve } from 'node:path';

async function demo() {
  console.log('FunctionRegistryActor Demo');
  console.log('='.repeat(60));

  // Create a new registry instance
  const registry = new FunctionRegistryActor();

  // Example 1: Register a code function manually
  console.log('\n1. Register Code Function Manually');
  console.log('-'.repeat(60));
  const codeResult = registry.registerFunction('manual-echo', {
    type: 'code',
    path: './functions/echo.js',
    maxStackDepth: 10,
    metadata: {
      name: 'Manual Echo',
      description: 'Manually registered echo function',
      author: 'demo'
    }
  });
  console.log('Result:', JSON.stringify(codeResult, null, 2));

  // Example 2: Register an agent function manually
  console.log('\n2. Register Agent Function Manually');
  console.log('-'.repeat(60));
  const agentResult = registry.registerFunction('manual-agent', {
    type: 'agent',
    agentCommand: 'claude',
    path: './functions/analyze.agent.js',
    maxStackDepth: 5,
    metadata: {
      name: 'Manual Agent',
      description: 'Manually registered analysis agent',
      author: 'demo'
    }
  });
  console.log('Result:', JSON.stringify(agentResult, null, 2));

  // Example 3: Get a function by ID
  console.log('\n3. Get Function by ID');
  console.log('-'.repeat(60));
  const getResult = registry.getFunction('manual-echo');
  console.log('Result:', JSON.stringify(getResult, null, 2));

  // Example 4: List all functions
  console.log('\n4. List All Functions');
  console.log('-'.repeat(60));
  const listResult = registry.listFunctions();
  console.log(`Total functions: ${listResult.data.count}`);
  listResult.data.functions.forEach(fn => {
    console.log(`  - ${fn.functionId} (${fn.type}): ${fn.metadata.name}`);
  });

  // Example 5: List functions by type
  console.log('\n5. List Functions by Type');
  console.log('-'.repeat(60));
  const codeListResult = registry.listFunctions({ type: 'code' });
  console.log(`Code functions: ${codeListResult.data.count}`);
  codeListResult.data.functions.forEach(fn => {
    console.log(`  - ${fn.functionId}: ${fn.path}`);
  });

  const agentListResult = registry.listFunctions({ type: 'agent' });
  console.log(`Agent functions: ${agentListResult.data.count}`);
  agentListResult.data.functions.forEach(fn => {
    console.log(`  - ${fn.functionId}: ${fn.agentCommand}`);
  });

  // Example 6: Scan functions directory
  console.log('\n6. Auto-Discover Functions from Directory');
  console.log('-'.repeat(60));
  const functionsDir = resolve(process.cwd(), 'functions');
  console.log(`Scanning: ${functionsDir}`);
  const scanResult = await registry.scanDirectory(functionsDir);
  console.log(`Discovered: ${scanResult.data.discoveredCount} functions`);
  scanResult.data.discovered.forEach(fn => {
    console.log(`  - ${fn.functionId} (${fn.type})`);
  });
  if (scanResult.data.skipped.length > 0) {
    console.log(`Skipped: ${scanResult.data.skippedCount} (already registered)`);
  }

  // Example 7: Get statistics
  console.log('\n7. Get Registry Statistics');
  console.log('-'.repeat(60));
  const stats = registry.getStats();
  console.log('Statistics:', JSON.stringify(stats, null, 2));

  // Example 8: Handle UAP message
  console.log('\n8. Handle UAP Message');
  console.log('-'.repeat(60));
  const message = {
    protocol: PROTOCOLS.REGISTRY,
    action: ACTIONS.LIST,
    data: {
      filters: { type: 'code' }
    }
  };
  console.log('Incoming message:', JSON.stringify(message, null, 2));
  const response = await registry.handleMessage(message);
  console.log('Response:', JSON.stringify(response, null, 2));

  // Example 9: Unregister a function
  console.log('\n9. Unregister Function');
  console.log('-'.repeat(60));
  const unregisterResult = registry.unregisterFunction('manual-echo');
  console.log('Result:', JSON.stringify(unregisterResult, null, 2));
  console.log(`Functions remaining: ${registry.functions.size}`);

  // Example 10: Error handling - invalid function type
  console.log('\n10. Error Handling - Invalid Function Type');
  console.log('-'.repeat(60));
  const errorResult = registry.registerFunction('bad-function', {
    type: 'invalid-type',
    path: './test.js'
  });
  console.log('Result:', JSON.stringify(errorResult, null, 2));

  // Final summary
  console.log('\n' + '='.repeat(60));
  console.log('Demo Complete!');
  console.log(`Final registry state: ${registry.functions.size} functions`);
  console.log('='.repeat(60));
}

// Run the demo
demo().catch(err => {
  console.error('Demo failed:', err);
  process.exit(1);
});
