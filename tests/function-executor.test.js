/**
 * Test suite for FunctionExecutorActor
 */

import { FunctionExecutorActor } from '../src/actors/function-executor.js';
import { PROTOCOLS, ACTIONS } from '../src/protocol.js';
import { resolve } from 'node:path';

async function runTests() {
  console.log('Starting FunctionExecutorActor tests...\n');

  const executor = new FunctionExecutorActor({ testMode: true });
  let passed = 0;
  let failed = 0;
  const emittedEvents = [];

  // Set up emit callback to capture emitted events
  executor.setEmitCallback(async (event) => {
    emittedEvents.push(event);
  });

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

  // Test 1: Execute echo function
  console.log('Test 1: Execute echo function');
  emittedEvents.length = 0;
  const echoResult = await executor.execute({
    functionId: 'test-echo',
    functionPath: resolve('./tests/fixtures/echo.js'),
    event: {
      id: 'evt_001',
      type: 'test.input',
      data: { message: 'Hello, World!' },
      metadata: { source: 'test', depth: 0 }
    }
  });
  assert(
    echoResult.action === ACTIONS.COMPLETE && echoResult.data.success,
    'Should execute echo function successfully'
  );
  assert(
    echoResult.data.result.echoed.message === 'Hello, World!',
    'Should return echoed data'
  );
  assert(
    echoResult.data.executionTime >= 0,
    'Should track execution time'
  );
  assert(
    emittedEvents.length === 1 && emittedEvents[0].type === 'function.executed',
    'Should emit function.executed event'
  );
  assert(
    emittedEvents[0].data.functionId === 'test-echo',
    'Emitted event should contain functionId'
  );

  // Test 2: Execute sync function
  console.log('\nTest 2: Execute sync function');
  emittedEvents.length = 0;
  const syncResult = await executor.execute({
    functionId: 'test-sync',
    functionPath: resolve('./tests/fixtures/sync-function.js'),
    event: {
      id: 'evt_002',
      type: 'test.sync',
      data: {},
      metadata: { source: 'test', depth: 0 }
    }
  });
  assert(
    syncResult.action === ACTIONS.COMPLETE && syncResult.data.success,
    'Should execute sync function successfully'
  );
  assert(
    syncResult.data.result.sync === true,
    'Should return sync function result'
  );
  assert(
    syncResult.data.result.configPresent === true,
    'Should pass config to function context'
  );

  // Test 3: Execute function that emits events
  console.log('\nTest 3: Execute function that emits events');
  emittedEvents.length = 0;
  const emitResult = await executor.execute({
    functionId: 'test-emit',
    functionPath: resolve('./tests/fixtures/emit-event.js'),
    event: {
      id: 'evt_003',
      type: 'test.parent',
      data: {},
      metadata: { source: 'test', depth: 0 }
    }
  });
  assert(
    emitResult.action === ACTIONS.COMPLETE,
    'Should complete successfully'
  );
  assert(
    emittedEvents.length === 2,
    'Should emit child event and function.executed event'
  );
  assert(
    emittedEvents.some(e => e.type === 'test.child'),
    'Should emit child event with correct type'
  );
  assert(
    emittedEvents.find(e => e.type === 'test.child')?.metadata?.depth === 1,
    'Child event should have depth 1'
  );
  assert(
    emittedEvents.find(e => e.type === 'test.child')?.metadata?.triggeredBy === 'evt_003',
    'Child event should reference parent event ID'
  );

  // Test 4: Handle function execution error
  console.log('\nTest 4: Handle function execution error');
  emittedEvents.length = 0;
  const errorResult = await executor.execute({
    functionId: 'test-error',
    functionPath: resolve('./tests/fixtures/throws-error.js'),
    event: {
      id: 'evt_004',
      type: 'test.error',
      data: {},
      metadata: { source: 'test', depth: 0 }
    }
  });
  assert(
    errorResult.action === ACTIONS.ERROR,
    'Should return error action'
  );
  assert(
    errorResult.data.error.includes('Intentional test error'),
    'Should include error message'
  );
  assert(
    errorResult.data.phase === 'execution',
    'Should identify execution phase'
  );
  assert(
    emittedEvents.length === 1 && emittedEvents[0].type === 'function.error',
    'Should emit function.error event'
  );
  assert(
    emittedEvents[0].data.error.includes('Intentional test error'),
    'Error event should contain error message'
  );

  // Test 5: Handle non-existent function file
  console.log('\nTest 5: Handle non-existent function file');
  emittedEvents.length = 0;
  const notFoundResult = await executor.execute({
    functionId: 'test-notfound',
    functionPath: './tests/fixtures/non-existent.js',
    event: {
      id: 'evt_005',
      type: 'test.notfound',
      data: {},
      metadata: { source: 'test', depth: 0 }
    }
  });
  assert(
    notFoundResult.action === ACTIONS.ERROR,
    'Should return error for non-existent file'
  );
  assert(
    notFoundResult.data.phase === 'import',
    'Should identify import phase'
  );
  assert(
    emittedEvents.some(e => e.type === 'function.error'),
    'Should emit function.error event'
  );

  // Test 6: Validate missing functionId
  console.log('\nTest 6: Validate missing functionId');
  const noIdResult = await executor.execute({
    functionPath: './tests/fixtures/echo.js',
    event: { id: 'evt_006', type: 'test', data: {} }
  });
  assert(
    noIdResult.action === ACTIONS.ERROR,
    'Should return error for missing functionId'
  );
  assert(
    noIdResult.data.error.includes('functionId is required'),
    'Should include validation error message'
  );

  // Test 7: Validate missing functionPath
  console.log('\nTest 7: Validate missing functionPath');
  const noPathResult = await executor.execute({
    functionId: 'test',
    event: { id: 'evt_007', type: 'test', data: {} }
  });
  assert(
    noPathResult.action === ACTIONS.ERROR,
    'Should return error for missing functionPath'
  );
  assert(
    noPathResult.data.error.includes('functionPath is required'),
    'Should include validation error message'
  );

  // Test 8: Validate missing event
  console.log('\nTest 8: Validate missing event');
  const noEventResult = await executor.execute({
    functionId: 'test',
    functionPath: './tests/fixtures/echo.js'
  });
  assert(
    noEventResult.action === ACTIONS.ERROR,
    'Should return error for missing event'
  );
  assert(
    noEventResult.data.error.includes('event is required'),
    'Should include validation error message'
  );

  // Test 9: Handle UAP message
  console.log('\nTest 9: Handle UAP message');
  emittedEvents.length = 0;
  const uapMessage = {
    protocol: PROTOCOLS.FUNCTION,
    action: ACTIONS.EXECUTE,
    data: {
      functionId: 'test-uap',
      functionPath: resolve('./tests/fixtures/echo.js'),
      event: {
        id: 'evt_009',
        type: 'test.uap',
        data: { uap: true },
        metadata: { source: 'test', depth: 0 }
      }
    }
  };
  const uapResult = await executor.handleMessage(uapMessage);
  assert(
    uapResult.action === ACTIONS.COMPLETE,
    'Should handle UAP execute message'
  );
  assert(
    uapResult.data.result.echoed.uap === true,
    'Should execute function via UAP message'
  );

  // Test 10: Handle invalid UAP protocol
  console.log('\nTest 10: Handle invalid UAP protocol');
  const invalidProtocolMsg = {
    protocol: 'invalid.v1',
    action: ACTIONS.EXECUTE,
    data: {}
  };
  const invalidProtocolResult = await executor.handleMessage(invalidProtocolMsg);
  assert(
    invalidProtocolResult.action === ACTIONS.ERROR,
    'Should return error for invalid protocol'
  );
  assert(
    invalidProtocolResult.data.error.includes('Invalid protocol'),
    'Should include protocol error message'
  );

  // Test 11: Handle unknown action
  console.log('\nTest 11: Handle unknown action');
  const unknownActionMsg = {
    protocol: PROTOCOLS.FUNCTION,
    action: 'unknown',
    data: {}
  };
  const unknownActionResult = await executor.handleMessage(unknownActionMsg);
  assert(
    unknownActionResult.action === ACTIONS.ERROR,
    'Should return error for unknown action'
  );
  assert(
    unknownActionResult.data.error.includes('Unknown action'),
    'Should include action error message'
  );

  // Test 12: Depth tracking
  console.log('\nTest 12: Depth tracking');
  emittedEvents.length = 0;
  await executor.execute({
    functionId: 'test-depth',
    functionPath: resolve('./tests/fixtures/emit-event.js'),
    event: {
      id: 'evt_012',
      type: 'test.depth',
      data: {},
      metadata: { source: 'test', depth: 5 }
    }
  });
  const childEvent = emittedEvents.find(e => e.type === 'test.child');
  assert(
    childEvent?.metadata?.depth === 6,
    'Should increment depth for child events'
  );

  // Test 13: Execute function without emit callback
  console.log('\nTest 13: Execute function without emit callback');
  const executorNoCallback = new FunctionExecutorActor();
  const noCallbackResult = await executorNoCallback.execute({
    functionId: 'test-nocallback',
    functionPath: resolve('./tests/fixtures/echo.js'),
    event: {
      id: 'evt_013',
      type: 'test.nocallback',
      data: { test: true },
      metadata: { source: 'test', depth: 0 }
    }
  });
  assert(
    noCallbackResult.action === ACTIONS.COMPLETE,
    'Should execute successfully without emit callback'
  );
  assert(
    noCallbackResult.data.result.echoed.test === true,
    'Should return correct result without emit callback'
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
