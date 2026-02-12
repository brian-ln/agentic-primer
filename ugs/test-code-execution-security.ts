#!/usr/bin/env bun
/**
 * UnsafeCodeComputeActor Security Tests
 *
 * Tests security vulnerabilities identified in QUALITY_REVIEW.md:
 * 1. Constructor chain escape attempts
 * 2. Prototype pollution attacks
 * 3. Global scope access attempts
 * 4. Timeout limitation with synchronous infinite loops
 */

import { MessageRouter } from './src/messaging/router.ts';
import { UnsafeCodeComputeActor } from './src/messaging/actors/compute/unsafe-code.ts';
import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';

// Color codes for output
const RED = '\x1b[31m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const BLUE = '\x1b[34m';
const RESET = '\x1b[0m';

interface TestResult {
  name: string;
  status: 'PASS' | 'FAIL' | 'VULNERABLE' | 'LIMITATION';
  description: string;
  evidence?: string;
  severity?: 'CRITICAL' | 'HIGH' | 'MEDIUM' | 'LOW' | 'INFO';
  recommendation?: string;
}

const results: TestResult[] = [];

function log(color: string, prefix: string, message: string) {
  console.log(`${color}${prefix}${RESET} ${message}`);
}

async function test1_ConstructorChainEscape(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Constructor Chain to Access Process';
  log(YELLOW, '[TEST 1]', testName);

  const exploitCode = `
    // Attempt to escape sandbox via constructor chain
    const AsyncFunction = (async function(){}).constructor;
    const func = AsyncFunction('return process');
    return func();
  `;

  try {
    const response = await codeActor.receive({
      id: 'test1',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result?.env) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Constructor chain successfully escaped sandbox and accessed process',
        evidence: `Accessed process.env: ${JSON.stringify(Object.keys(response.payload.result.env).slice(0, 5))}...`,
        severity: 'CRITICAL',
        recommendation: 'Use Worker threads or vm2 for production. Document this limitation.',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Constructor chain escape blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Constructor chain returned undefined/null',
        evidence: `Result: ${response.payload?.result}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test2_FunctionConstructorEscape(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Function.constructor Escape';
  log(YELLOW, '[TEST 2]', testName);

  const exploitCode = `
    // Attempt to access Function constructor
    const FunctionConstructor = (function(){}).constructor.constructor;
    return FunctionConstructor('return this')();
  `;

  try {
    const response = await codeActor.receive({
      id: 'test2',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result?.process) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Function constructor escaped to global scope',
        evidence: `Accessed global: ${JSON.stringify(Object.keys(response.payload.result).slice(0, 10))}...`,
        severity: 'CRITICAL',
        recommendation: 'Freeze constructor properties or use isolated Worker threads.',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Function constructor escape blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Function constructor returned safe value',
        evidence: `Result type: ${typeof response.payload?.result}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test3_PrototypePollution(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Prototype Pollution Attack';
  log(YELLOW, '[TEST 3]', testName);

  const exploitCode = `
    // Attempt prototype pollution
    Object.prototype.polluted = 'EXPLOITED';
    const test = {};
    return test.polluted;
  `;

  try {
    const response = await codeActor.receive({
      id: 'test3',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result === 'EXPLOITED') {
      // Check if pollution persists across executions
      const checkCode = `
        const test = {};
        return test.polluted;
      `;
      const checkResponse = await codeActor.receive({
        id: 'test3_check',
        type: 'execute',
        from: '@(test)',
        to: '@(code-execution)',
        payload: { code: checkCode, timeout: 2000 },
        timestamp: Date.now(),
      });

      if (checkResponse.success && checkResponse.payload?.result === 'EXPLOITED') {
        return {
          name: testName,
          status: 'VULNERABLE',
          description: 'Prototype pollution persists across executions',
          evidence: 'Pollution survived sandbox reset',
          severity: 'HIGH',
          recommendation: 'Isolate each execution in a fresh context or Worker thread.',
        };
      } else {
        return {
          name: testName,
          status: 'PASS',
          description: 'Prototype pollution contained to single execution',
          evidence: 'Pollution did not persist',
          severity: 'LOW',
        };
      }
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Prototype pollution blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Prototype pollution had no effect',
        evidence: `Result: ${response.payload?.result}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test4_GlobalThisAccess(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Access to Global Scope via this';
  log(YELLOW, '[TEST 4]', testName);

  const exploitCode = `
    // Attempt to access global via 'this' in non-strict mode
    return (function() { return this; })();
  `;

  try {
    const response = await codeActor.receive({
      id: 'test4',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result?.process) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Accessed global scope via this',
        evidence: `Global object keys: ${JSON.stringify(Object.keys(response.payload.result).slice(0, 10))}...`,
        severity: 'CRITICAL',
        recommendation: 'Use strict mode or Worker threads.',
      };
    } else if (response.success && response.payload?.result === undefined) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Global scope access returned undefined',
        evidence: 'Strict mode or sandbox prevented global access',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Global scope access blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Global scope access contained',
        evidence: `Result type: ${typeof response.payload?.result}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test5_ImportMetaAccess(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Access import.meta';
  log(YELLOW, '[TEST 5]', testName);

  const exploitCode = `
    // Attempt to access import.meta
    return import.meta;
  `;

  try {
    const response = await codeActor.receive({
      id: 'test5',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result?.url) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Accessed import.meta with file system information',
        evidence: `URL: ${response.payload.result.url}`,
        severity: 'MEDIUM',
        recommendation: 'Block import.meta access.',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'import.meta access blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'import.meta returned undefined',
        evidence: `Result: ${response.payload?.result}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test6_SyncInfiniteLoop(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Synchronous Infinite Loop (Timeout Limitation)';
  log(YELLOW, '[TEST 6]', testName);
  log(BLUE, '[INFO]', 'This test will hang for 3 seconds to demonstrate the limitation');

  const exploitCode = `
    // Synchronous infinite loop that timeout cannot stop
    const start = Date.now();
    while (Date.now() - start < 3000) {
      // Busy wait for 3 seconds
    }
    return 'Loop completed';
  `;

  try {
    const startTime = Date.now();
    const response = await codeActor.receive({
      id: 'test6',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 1000 }, // 1 second timeout
      timestamp: Date.now(),
    });
    const elapsedTime = Date.now() - startTime;

    if (response.success && elapsedTime > 2000) {
      return {
        name: testName,
        status: 'LIMITATION',
        description: 'Timeout CANNOT stop synchronous loops (known JavaScript limitation)',
        evidence: `Code ran for ${elapsedTime}ms despite 1000ms timeout`,
        severity: 'INFO',
        recommendation: 'Use Worker threads with termination for production. Document this limitation clearly.',
      };
    } else if (!response.success && response.error?.includes('timeout')) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Timeout successfully interrupted execution',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'FAIL',
        description: 'Unexpected test behavior',
        evidence: `Time: ${elapsedTime}ms, Success: ${response.success}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test7_EvalAccess(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Access to eval()';
  log(YELLOW, '[TEST 7]', testName);

  const exploitCode = `
    // Attempt to use eval to escape sandbox
    return eval('process');
  `;

  try {
    const response = await codeActor.receive({
      id: 'test7',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result?.env) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'eval() escaped sandbox',
        evidence: 'Accessed process via eval',
        severity: 'CRITICAL',
        recommendation: 'Disable eval or use CSP-equivalent restrictions.',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'eval() access blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'eval() returned safe value',
        evidence: `Result: ${response.payload?.result}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test8_SymbolAccess(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Access to Symbol primitives';
  log(YELLOW, '[TEST 8]', testName);

  const exploitCode = `
    // Attempt to access hidden properties via Symbol
    const symbols = Object.getOwnPropertySymbols(Object.prototype);
    return symbols.length;
  `;

  try {
    const response = await codeActor.receive({
      id: 'test8',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Symbol access allowed (low risk)',
        evidence: `Symbol count: ${response.payload?.result}`,
        severity: 'LOW',
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Symbol access blocked',
        evidence: `Error: ${response.error}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test9_ProxyTrap(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Proxy-based Escape Attempt';
  log(YELLOW, '[TEST 9]', testName);

  const exploitCode = `
    // Attempt to use Proxy to intercept and escape
    const handler = {
      get: function(target, prop) {
        if (prop === 'process') {
          return process;
        }
        return target[prop];
      }
    };
    const proxy = new Proxy({}, handler);
    return proxy.process;
  `;

  try {
    const response = await codeActor.receive({
      id: 'test9',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result?.env) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Proxy successfully accessed process',
        evidence: 'Process accessible via Proxy',
        severity: 'CRITICAL',
        recommendation: 'Restrict Proxy usage or use Worker threads.',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Proxy escape blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Proxy returned undefined',
        evidence: `Result: ${response.payload?.result}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

async function test10_ErrorStackAccess(codeActor: UnsafeCodeComputeActor): Promise<TestResult> {
  const testName = 'Information Leak via Error Stack';
  log(YELLOW, '[TEST 10]', testName);

  const exploitCode = `
    // Attempt to extract file system info from error stack
    try {
      throw new Error('test');
    } catch (e) {
      return e.stack;
    }
  `;

  try {
    const response = await codeActor.receive({
      id: 'test10',
      type: 'execute',
      from: '@(test)',
      to: '@(code-execution)',
      payload: { code: exploitCode, timeout: 2000 },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.result?.includes('/')) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Error stack reveals file system paths',
        evidence: `Stack trace contains paths: ${response.payload.result.substring(0, 100)}...`,
        severity: 'MEDIUM',
        recommendation: 'Sanitize error stacks before returning.',
      };
    } else if (response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Error stack does not leak sensitive info',
        evidence: `Stack: ${response.payload?.result?.substring(0, 50)}...`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Error handling blocked',
        evidence: `Error: ${response.error}`,
      };
    }
  } catch (error: any) {
    return {
      name: testName,
      status: 'FAIL',
      description: 'Test execution error',
      evidence: error.message,
    };
  }
}

function printResults(results: TestResult[]) {
  console.log('\n' + '='.repeat(80));
  console.log('UnsafeCodeComputeActor Security Test Results');
  console.log('='.repeat(80) + '\n');

  const vulnerabilities = results.filter(r => r.status === 'VULNERABLE');
  const limitations = results.filter(r => r.status === 'LIMITATION');
  const passes = results.filter(r => r.status === 'PASS');
  const failures = results.filter(r => r.status === 'FAIL');

  // Print vulnerabilities first (most important)
  if (vulnerabilities.length > 0) {
    log(RED, '\n❌ VULNERABILITIES FOUND:', `${vulnerabilities.length} test(s)`);
    vulnerabilities.forEach(result => {
      console.log(`\n  ${RED}✗ ${result.name}${RESET} [${result.severity}]`);
      console.log(`    ${result.description}`);
      if (result.evidence) {
        console.log(`    Evidence: ${result.evidence}`);
      }
      if (result.recommendation) {
        console.log(`    ${YELLOW}→ Recommendation: ${result.recommendation}${RESET}`);
      }
    });
  }

  // Print limitations
  if (limitations.length > 0) {
    log(BLUE, '\n⚠ KNOWN LIMITATIONS:', `${limitations.length} test(s)`);
    limitations.forEach(result => {
      console.log(`\n  ${BLUE}⚠ ${result.name}${RESET} [${result.severity}]`);
      console.log(`    ${result.description}`);
      if (result.evidence) {
        console.log(`    Evidence: ${result.evidence}`);
      }
      if (result.recommendation) {
        console.log(`    ${YELLOW}→ Recommendation: ${result.recommendation}${RESET}`);
      }
    });
  }

  // Print passes
  if (passes.length > 0) {
    log(GREEN, '\n✓ TESTS PASSED:', `${passes.length} test(s)`);
    passes.forEach(result => {
      console.log(`  ${GREEN}✓ ${result.name}${RESET}`);
      if (result.severity && result.severity === 'LOW') {
        console.log(`    ${result.description} [${result.severity}]`);
      }
      if (result.evidence && result.severity) {
        console.log(`    Evidence: ${result.evidence}`);
      }
    });
  }

  // Print failures (test errors)
  if (failures.length > 0) {
    log(YELLOW, '\n⚠ TEST FAILURES:', `${failures.length} test(s)`);
    failures.forEach(result => {
      console.log(`  ${YELLOW}⚠ ${result.name}${RESET}`);
      console.log(`    ${result.description}`);
      if (result.evidence) {
        console.log(`    ${result.evidence}`);
      }
    });
  }

  // Summary
  console.log('\n' + '='.repeat(80));
  console.log('SUMMARY');
  console.log('='.repeat(80));
  console.log(`Total Tests:     ${results.length}`);
  console.log(`${GREEN}Passed:          ${passes.length}${RESET}`);
  console.log(`${RED}Vulnerabilities: ${vulnerabilities.length}${RESET}`);
  console.log(`${BLUE}Limitations:     ${limitations.length}${RESET}`);
  console.log(`${YELLOW}Test Failures:   ${failures.length}${RESET}`);

  if (vulnerabilities.length === 0 && failures.length === 0) {
    log(GREEN, '\n✓ SECURITY STATUS:', 'All sandbox escape attempts blocked');
    if (limitations.length > 0) {
      log(BLUE, '  NOTE:', 'Known limitations documented (see above)');
    }
  } else if (vulnerabilities.length > 0) {
    log(RED, '\n✗ SECURITY STATUS:', 'SANDBOX CAN BE ESCAPED - use Worker threads for production');
  } else {
    log(YELLOW, '\n⚠ SECURITY STATUS:', 'Some tests failed to execute - manual verification needed');
  }

  console.log('='.repeat(80) + '\n');
}

async function runTests() {
  console.log(`
╔════════════════════════════════════════════════════════════════════════════╗
║          UnsafeCodeComputeActor Security Vulnerability Testing                 ║
║                                                                            ║
║  Testing for vulnerabilities identified in QUALITY_REVIEW.md:             ║
║  - Constructor chain escape attempts                                       ║
║  - Prototype pollution attacks                                             ║
║  - Global scope access                                                     ║
║  - Timeout limitations (synchronous infinite loops)                        ║
╚════════════════════════════════════════════════════════════════════════════╝
  `);

  try {
    // Initialize actor system
    const store = new GraphStore();
    const programManager = new ProgramManager(store);
    const router = new MessageRouter(store, programManager);
    const codeActor = new UnsafeCodeComputeActor(router, { timeout: 5000, iUnderstandThisIsUnsafe: true });

    // Run all tests
    results.push(await test1_ConstructorChainEscape(codeActor));
    results.push(await test2_FunctionConstructorEscape(codeActor));
    results.push(await test3_PrototypePollution(codeActor));
    results.push(await test4_GlobalThisAccess(codeActor));
    results.push(await test5_ImportMetaAccess(codeActor));
    results.push(await test6_SyncInfiniteLoop(codeActor));
    results.push(await test7_EvalAccess(codeActor));
    results.push(await test8_SymbolAccess(codeActor));
    results.push(await test9_ProxyTrap(codeActor));
    results.push(await test10_ErrorStackAccess(codeActor));

    printResults(results);

  } catch (error) {
    console.error('Fatal error:', error);
    process.exit(2);
  }

  // Exit with appropriate code
  const hasVulnerabilities = results.some(r => r.status === 'VULNERABLE');
  process.exit(hasVulnerabilities ? 1 : 0);
}

// Run tests
runTests().catch(error => {
  console.error('Fatal error:', error);
  process.exit(2);
});
