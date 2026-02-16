#!/usr/bin/env tsx
/**
 * Quick test to verify hub message validators work correctly
 */

import { HubMessageValidators, validateHubMessage } from '../packages/protocols/src/hub-messages.validators';

console.log('Testing Hub Message Validators...\n');

// Test 1: Valid hub:connect metadata
console.log('✓ Test 1: Valid hub:connect metadata');
const validConnect = {
  protocolVersion: '0.1.0',
  capabilities: ['send', 'broadcast'],
  authToken: 'bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...'
};

const result1 = validateHubMessage(HubMessageValidators.hubConnectMetadata, validConnect);
console.log('  Result:', result1.success ? '✅ PASS' : '❌ FAIL');
if (!result1.success) {
  console.error('  Errors:', result1.error.issues);
}

// Test 2: Invalid hub:connect metadata (empty capabilities)
console.log('\n✓ Test 2: Invalid hub:connect metadata (empty capabilities)');
const invalidConnect = {
  protocolVersion: '0.1.0',
  capabilities: []
};

const result2 = validateHubMessage(HubMessageValidators.hubConnectMetadata, invalidConnect);
console.log('  Result:', !result2.success ? '✅ PASS (correctly rejected)' : '❌ FAIL (should reject)');
if (!result2.success) {
  console.log('  Expected error:', result2.error.issues[0].message);
}

// Test 3: Valid hub:register payload
console.log('\n✓ Test 3: Valid hub:register payload');
const validRegister = {
  actorAddress: '@(browser/widget-123)',
  capabilities: ['render', 'handle-click'],
  metadata: {
    widgetType: 'chart',
    version: '2.1.0'
  },
  ttlSeconds: 300
};

const result3 = validateHubMessage(HubMessageValidators.hubRegisterPayload, validRegister);
console.log('  Result:', result3.success ? '✅ PASS' : '❌ FAIL');
if (!result3.success) {
  console.error('  Errors:', result3.error.issues);
}

// Test 4: Valid hub:error payload
console.log('\n✓ Test 4: Valid hub:error payload');
const validError = {
  code: 'timeout',
  message: 'Message delivery timed out after 30s',
  details: { targetAddress: '@(offline/actor-456)' },
  retryable: true
};

const result4 = validateHubMessage(HubMessageValidators.hubErrorPayload, validError);
console.log('  Result:', result4.success ? '✅ PASS' : '❌ FAIL');
if (!result4.success) {
  console.error('  Errors:', result4.error.issues);
}

// Test 5: Invalid hub:error payload (invalid error code)
console.log('\n✓ Test 5: Invalid hub:error payload (invalid error code)');
const invalidError = {
  code: 'invalid_code',
  message: 'Some error',
  retryable: false
};

const result5 = validateHubMessage(HubMessageValidators.hubErrorPayload, invalidError);
console.log('  Result:', !result5.success ? '✅ PASS (correctly rejected)' : '❌ FAIL (should reject)');
if (!result5.success) {
  console.log('  Expected error:', result5.error.issues[0].message);
}

// Test 6: Valid actor-registration
console.log('\n✓ Test 6: Valid actor-registration');
const validActorReg = {
  actorAddress: '@(browser/widget-123)',
  capabilities: ['render', 'handle-click'],
  metadata: { widgetType: 'chart' },
  connectionId: 'conn-abc',
  registeredAt: 1739731000000,
  expiresAt: 1739731300000,
  version: 1
};

const result6 = validateHubMessage(HubMessageValidators.actorRegistration, validActorReg);
console.log('  Result:', result6.success ? '✅ PASS' : '❌ FAIL');
if (!result6.success) {
  console.error('  Errors:', result6.error.issues);
}

// Test 7: Strict validation (rejects extra properties)
console.log('\n✓ Test 7: Strict validation (rejects extra properties)');
const withExtraProps = {
  protocolVersion: '0.1.0',
  capabilities: ['send'],
  extraField: 'should be rejected'
};

const result7 = validateHubMessage(HubMessageValidators.hubConnectMetadata, withExtraProps);
console.log('  Result:', !result7.success ? '✅ PASS (correctly rejected extra property)' : '❌ FAIL (should reject)');
if (!result7.success) {
  console.log('  Expected error:', result7.error.issues[0].message);
}

console.log('\n=== All tests completed ===');
