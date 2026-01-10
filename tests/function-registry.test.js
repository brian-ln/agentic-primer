/**
 * Test suite for FunctionRegistryActor
 */

import FunctionRegistryActor from '../src/actors/function-registry.js';
import { PROTOCOLS, ACTIONS } from '../src/protocol.js';
import { writeFile, mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';

async function runTests() {
  console.log('Starting FunctionRegistryActor tests...\n');

  const registry = new FunctionRegistryActor();
  let passed = 0;
  let failed = 0;

  // Helper function
  function assert(condition, testName) {
    if (condition) {
      console.log(`✓ ${testName}`);
      passed++;
    } else {
      console.error(`✗ ${testName}`);
      failed++;
    }
  }

  // Test 1: Register code function
  console.log('Test 1: Register code function');
  const codeResult = registry.registerFunction('test-echo', {
    type: 'code',
    path: './functions/echo.js',
    metadata: {
      name: 'Echo Function',
      description: 'Echoes event data',
      author: 'test'
    }
  });
  assert(
    codeResult.action === ACTIONS.REGISTER && codeResult.data.success,
    'Should register code function successfully'
  );
  assert(
    registry.functions.size === 1,
    'Registry should contain 1 function'
  );

  // Test 2: Register agent function
  console.log('\nTest 2: Register agent function');
  const agentResult = registry.registerFunction('test-agent', {
    type: 'agent',
    agentCommand: 'claude',
    path: './functions/analyze.agent.js',
    metadata: {
      name: 'Analyze Agent',
      description: 'Analyzes events using Claude',
      author: 'test'
    }
  });
  assert(
    agentResult.action === ACTIONS.REGISTER && agentResult.data.success,
    'Should register agent function successfully'
  );
  assert(
    registry.functions.size === 2,
    'Registry should contain 2 functions'
  );

  // Test 3: Get function by ID
  console.log('\nTest 3: Get function by ID');
  const getResult = registry.getFunction('test-echo');
  assert(
    getResult.action === ACTIONS.RESPONSE && getResult.data.function.type === 'code',
    'Should retrieve code function metadata'
  );

  // Test 4: List all functions
  console.log('\nTest 4: List all functions');
  const listResult = registry.listFunctions();
  assert(
    listResult.data.count === 2,
    'Should list 2 functions'
  );
  assert(
    listResult.data.functions.length === 2,
    'Should return array of 2 functions'
  );

  // Test 5: List functions filtered by type
  console.log('\nTest 5: List functions filtered by type');
  const codeListResult = registry.listFunctions({ type: 'code' });
  assert(
    codeListResult.data.count === 1,
    'Should list 1 code function'
  );
  const agentListResult = registry.listFunctions({ type: 'agent' });
  assert(
    agentListResult.data.count === 1,
    'Should list 1 agent function'
  );

  // Test 6: Unregister function
  console.log('\nTest 6: Unregister function');
  const unregisterResult = registry.unregisterFunction('test-echo');
  assert(
    unregisterResult.data.success && unregisterResult.data.existed,
    'Should unregister existing function'
  );
  assert(
    registry.functions.size === 1,
    'Registry should contain 1 function after unregister'
  );

  // Test 7: Get non-existent function
  console.log('\nTest 7: Get non-existent function');
  const notFoundResult = registry.getFunction('non-existent');
  assert(
    notFoundResult.action === ACTIONS.ERROR,
    'Should return error for non-existent function'
  );

  // Test 8: Unregister non-existent function
  console.log('\nTest 8: Unregister non-existent function');
  const unregisterNotFoundResult = registry.unregisterFunction('non-existent');
  assert(
    unregisterNotFoundResult.data.success && !unregisterNotFoundResult.data.existed,
    'Should succeed but indicate function was not registered'
  );

  // Test 9: Validation - missing functionId
  console.log('\nTest 9: Validation - missing functionId');
  const noIdResult = registry.registerFunction('', { type: 'code', path: './test.js' });
  assert(
    noIdResult.action === ACTIONS.ERROR,
    'Should return error for missing functionId'
  );

  // Test 10: Validation - invalid type
  console.log('\nTest 10: Validation - invalid type');
  const invalidTypeResult = registry.registerFunction('test', { type: 'invalid' });
  assert(
    invalidTypeResult.action === ACTIONS.ERROR,
    'Should return error for invalid type'
  );

  // Test 11: Validation - code function without path
  console.log('\nTest 11: Validation - code function without path');
  const noPathResult = registry.registerFunction('test', { type: 'code' });
  assert(
    noPathResult.action === ACTIONS.ERROR,
    'Should return error for code function without path'
  );

  // Test 12: Validation - agent function without agentCommand
  console.log('\nTest 12: Validation - agent function without agentCommand');
  const noCommandResult = registry.registerFunction('test', { type: 'agent' });
  assert(
    noCommandResult.action === ACTIONS.ERROR,
    'Should return error for agent function without agentCommand'
  );

  // Test 13: Get statistics
  console.log('\nTest 13: Get statistics');
  registry.clear();
  registry.registerFunction('code-fn', { type: 'code', path: './test1.js' });
  registry.registerFunction('agent-fn', { type: 'agent', agentCommand: 'claude' });
  const stats = registry.getStats();
  assert(
    stats.totalFunctions === 2 && stats.codeFunctions === 1 && stats.agentFunctions === 1,
    'Should return correct statistics'
  );

  // Test 14: Directory scanning (requires test directory)
  console.log('\nTest 14: Directory scanning');
  const testDir = '/tmp/test-functions-' + Date.now();
  try {
    // Create test directory with sample functions
    await mkdir(testDir, { recursive: true });
    await writeFile(join(testDir, 'echo.js'), 'export default function() {}');
    await writeFile(join(testDir, 'transform.js'), 'export default function() {}');
    await writeFile(join(testDir, 'analyze.agent.js'), '// Agent function');
    await writeFile(join(testDir, 'readme.txt'), 'Not a function file');

    registry.clear();
    const scanResult = await registry.scanDirectory(testDir);

    assert(
      scanResult.data.success && scanResult.data.discoveredCount === 3,
      'Should discover 3 function files (2 code, 1 agent)'
    );

    const afterScanStats = registry.getStats();
    assert(
      afterScanStats.codeFunctions === 2 && afterScanStats.agentFunctions === 1,
      'Should register 2 code functions and 1 agent function'
    );

    // Clean up
    await rm(testDir, { recursive: true, force: true });
  } catch (err) {
    console.error('Error during directory scan test:', err);
    failed++;
  }

  // Test 15: Directory scanning with skip existing
  console.log('\nTest 15: Directory scanning with skip existing');
  const testDir2 = '/tmp/test-functions-skip-' + Date.now();
  try {
    await mkdir(testDir2, { recursive: true });
    await writeFile(join(testDir2, 'existing.js'), 'export default function() {}');
    await writeFile(join(testDir2, 'new.js'), 'export default function() {}');

    registry.clear();
    registry.registerFunction('existing', { type: 'code', path: './old-path.js' });

    const scanResult = await registry.scanDirectory(testDir2, { overwrite: false });

    assert(
      scanResult.data.discoveredCount === 1 && scanResult.data.skippedCount === 1,
      'Should discover 1 new function and skip 1 existing'
    );

    // Clean up
    await rm(testDir2, { recursive: true, force: true });
  } catch (err) {
    console.error('Error during skip existing test:', err);
    failed++;
  }

  // Test 16: Handle UAP messages
  console.log('\nTest 16: Handle UAP messages');
  registry.clear();
  const registerMsg = {
    protocol: PROTOCOLS.REGISTRY,
    action: ACTIONS.REGISTER,
    data: {
      functionId: 'uap-test',
      metadata: {
        type: 'code',
        path: './test.js'
      }
    }
  };
  const uapResult = await registry.handleMessage(registerMsg);
  assert(
    uapResult.data.success,
    'Should handle UAP register message'
  );

  // Summary
  console.log('\n' + '='.repeat(50));
  console.log(`Tests completed: ${passed} passed, ${failed} failed`);
  console.log('='.repeat(50));

  return failed === 0;
}

// Run tests
runTests().then(success => {
  process.exit(success ? 0 : 1);
}).catch(err => {
  console.error('Test suite failed:', err);
  process.exit(1);
});
