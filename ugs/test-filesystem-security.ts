#!/usr/bin/env bun
/**
 * FileSystemActor Security Tests
 *
 * Tests security vulnerabilities identified in QUALITY_REVIEW.md:
 * 1. Symlink escape attempts
 * 2. Absolute path handling
 * 3. Path traversal variants
 * 4. Edge cases in path validation
 */

import { MessageRouter } from './src/messaging/router.ts';
import { FileSystemActor } from './src/messaging/actors/filesystem.ts';
import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { mkdirSync, writeFileSync, symlinkSync, existsSync, rmSync, readFileSync } from 'node:fs';
import { resolve, join } from 'node:path';

// Test configuration
const TEST_BASE_PATH = './data/security-test-fs';
const SENSITIVE_FILE_PATH = '/tmp/sensitive-data.txt';
const SENSITIVE_CONTENT = 'SENSITIVE: This should never be accessible';

// Color codes for output
const RED = '\x1b[31m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const BLUE = '\x1b[34m';
const RESET = '\x1b[0m';

interface TestResult {
  name: string;
  status: 'PASS' | 'FAIL' | 'VULNERABLE';
  description: string;
  evidence?: string;
  severity?: 'CRITICAL' | 'HIGH' | 'MEDIUM' | 'LOW';
}

const results: TestResult[] = [];

function log(color: string, prefix: string, message: string) {
  console.log(`${color}${prefix}${RESET} ${message}`);
}

function setupTestEnvironment() {
  log(BLUE, '[SETUP]', 'Creating test environment...');

  // Clean up if exists
  if (existsSync(TEST_BASE_PATH)) {
    rmSync(TEST_BASE_PATH, { recursive: true, force: true });
  }

  // Create test directory structure
  mkdirSync(TEST_BASE_PATH, { recursive: true });
  mkdirSync(join(TEST_BASE_PATH, 'subdir'), { recursive: true });

  // Create sensitive file outside basePath
  writeFileSync(SENSITIVE_FILE_PATH, SENSITIVE_CONTENT);

  // Create test files
  writeFileSync(join(TEST_BASE_PATH, 'normal.txt'), 'Normal file content');
  writeFileSync(join(TEST_BASE_PATH, 'subdir', 'nested.txt'), 'Nested file content');

  log(GREEN, '[SETUP]', 'Test environment created');
}

function cleanupTestEnvironment() {
  log(BLUE, '[CLEANUP]', 'Cleaning up test environment...');

  if (existsSync(TEST_BASE_PATH)) {
    rmSync(TEST_BASE_PATH, { recursive: true, force: true });
  }

  if (existsSync(SENSITIVE_FILE_PATH)) {
    rmSync(SENSITIVE_FILE_PATH, { force: true });
  }

  log(GREEN, '[CLEANUP]', 'Test environment cleaned');
}

async function test1_SymlinkToSensitiveFile(fsActor: FileSystemActor): Promise<TestResult> {
  const testName = 'Symlink to Sensitive File Outside basePath';
  log(YELLOW, '[TEST 1]', testName);

  try {
    // Create symlink inside basePath pointing to sensitive file outside
    const symlinkPath = join(TEST_BASE_PATH, 'evil-link');
    symlinkSync(SENSITIVE_FILE_PATH, symlinkPath);

    // Attempt to read via FileSystemActor
    const response = await fsActor.receive({
      id: 'test1',
      type: 'read_file',
      from: '@(test)',
      to: '@(filesystem)',
      payload: { path: 'evil-link' },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.content === SENSITIVE_CONTENT) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Symlink successfully bypassed path validation and read sensitive file',
        evidence: `Content read: "${response.payload.content}"`,
        severity: 'CRITICAL',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Symlink attack blocked by path validation',
        evidence: `Error: ${response.error}`,
        severity: undefined,
      };
    } else {
      return {
        name: testName,
        status: 'FAIL',
        description: 'Unexpected response from symlink test',
        evidence: JSON.stringify(response),
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

async function test2_AbsolutePathAttempt(fsActor: FileSystemActor): Promise<TestResult> {
  const testName = 'Absolute Path to Sensitive File';
  log(YELLOW, '[TEST 2]', testName);

  try {
    const response = await fsActor.receive({
      id: 'test2',
      type: 'read_file',
      from: '@(test)',
      to: '@(filesystem)',
      payload: { path: SENSITIVE_FILE_PATH },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.content === SENSITIVE_CONTENT) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Absolute path bypassed path validation',
        evidence: `Content read: "${response.payload.content}"`,
        severity: 'CRITICAL',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Absolute path attack blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'FAIL',
        description: 'Unexpected response',
        evidence: JSON.stringify(response),
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

async function test3_DirectoryTraversalVariants(fsActor: FileSystemActor): Promise<TestResult> {
  const testName = 'Directory Traversal Variants';
  log(YELLOW, '[TEST 3]', testName);

  const traversalAttempts = [
    '../../../etc/passwd',
    '../../sensitive-data.txt',
    './../../sensitive-data.txt',
    'subdir/../../../sensitive-data.txt',
    'subdir/../../sensitive-data.txt',
  ];

  const vulnerabilities: string[] = [];

  for (const attempt of traversalAttempts) {
    try {
      const response = await fsActor.receive({
        id: `test3_${attempt}`,
        type: 'read_file',
        from: '@(test)',
        to: '@(filesystem)',
        payload: { path: attempt },
        timestamp: Date.now(),
      });

      if (response.success) {
        vulnerabilities.push(`"${attempt}" succeeded`);
      }
    } catch (error) {
      // Expected - path validation should block these
    }
  }

  if (vulnerabilities.length > 0) {
    return {
      name: testName,
      status: 'VULNERABLE',
      description: 'Some traversal attempts succeeded',
      evidence: vulnerabilities.join(', '),
      severity: 'HIGH',
    };
  } else {
    return {
      name: testName,
      status: 'PASS',
      description: 'All directory traversal attempts blocked',
      evidence: `Tested ${traversalAttempts.length} variants`,
    };
  }
}

async function test4_SymlinkChain(fsActor: FileSystemActor): Promise<TestResult> {
  const testName = 'Symlink Chain Attack';
  log(YELLOW, '[TEST 4]', testName);

  try {
    // Create chain: link1 -> link2 -> sensitive file
    const link1Path = join(TEST_BASE_PATH, 'link1');
    const link2Path = join(TEST_BASE_PATH, 'link2');

    symlinkSync(SENSITIVE_FILE_PATH, link2Path);
    symlinkSync(link2Path, link1Path);

    const response = await fsActor.receive({
      id: 'test4',
      type: 'read_file',
      from: '@(test)',
      to: '@(filesystem)',
      payload: { path: 'link1' },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.content === SENSITIVE_CONTENT) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Symlink chain bypassed path validation',
        evidence: `Content read through chain: "${response.payload.content}"`,
        severity: 'CRITICAL',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Symlink chain attack blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'FAIL',
        description: 'Unexpected response',
        evidence: JSON.stringify(response),
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

async function test5_SymlinkInSubdirectory(fsActor: FileSystemActor): Promise<TestResult> {
  const testName = 'Symlink in Subdirectory';
  log(YELLOW, '[TEST 5]', testName);

  try {
    // Create symlink in subdirectory
    const symlinkPath = join(TEST_BASE_PATH, 'subdir', 'sneaky-link');
    symlinkSync(SENSITIVE_FILE_PATH, symlinkPath);

    const response = await fsActor.receive({
      id: 'test5',
      type: 'read_file',
      from: '@(test)',
      to: '@(filesystem)',
      payload: { path: 'subdir/sneaky-link' },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.content === SENSITIVE_CONTENT) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Symlink in subdirectory bypassed validation',
        evidence: `Content read: "${response.payload.content}"`,
        severity: 'CRITICAL',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Symlink in subdirectory blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'FAIL',
        description: 'Unexpected response',
        evidence: JSON.stringify(response),
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

async function test6_WriteViaSymlink(fsActor: FileSystemActor): Promise<TestResult> {
  const testName = 'Write via Symlink';
  log(YELLOW, '[TEST 6]', testName);

  try {
    const symlinkPath = join(TEST_BASE_PATH, 'write-link');
    const targetPath = '/tmp/symlink-write-test.txt';

    // Create symlink pointing outside basePath
    symlinkSync(targetPath, symlinkPath);

    const maliciousContent = 'EXPLOIT: Written via symlink';

    const response = await fsActor.receive({
      id: 'test6',
      type: 'write_file',
      from: '@(test)',
      to: '@(filesystem)',
      payload: {
        path: 'write-link',
        content: maliciousContent,
      },
      timestamp: Date.now(),
    });

    // Check if file was written outside basePath
    const fileWritten = existsSync(targetPath);
    if (fileWritten) {
      const content = readFileSync(targetPath, 'utf-8');
      rmSync(targetPath, { force: true });

      if (content === maliciousContent) {
        return {
          name: testName,
          status: 'VULNERABLE',
          description: 'Write via symlink succeeded, file created outside basePath',
          evidence: `File written to ${targetPath}: "${content}"`,
          severity: 'CRITICAL',
        };
      }
    }

    if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Write via symlink blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Write was contained within basePath',
        evidence: 'No file created outside basePath',
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

async function test7_NullByteInjection(fsActor: FileSystemActor): Promise<TestResult> {
  const testName = 'Null Byte Injection';
  log(YELLOW, '[TEST 7]', testName);

  try {
    // Try to bypass path check with null byte
    const response = await fsActor.receive({
      id: 'test7',
      type: 'read_file',
      from: '@(test)',
      to: '@(filesystem)',
      payload: { path: 'normal.txt\0../../sensitive-data.txt' },
      timestamp: Date.now(),
    });

    if (response.success && response.payload?.content === SENSITIVE_CONTENT) {
      return {
        name: testName,
        status: 'VULNERABLE',
        description: 'Null byte injection bypassed path validation',
        evidence: `Content read: "${response.payload.content}"`,
        severity: 'HIGH',
      };
    } else if (!response.success) {
      return {
        name: testName,
        status: 'PASS',
        description: 'Null byte injection blocked',
        evidence: `Error: ${response.error}`,
      };
    } else {
      return {
        name: testName,
        status: 'PASS',
        description: 'Null byte had no effect',
        evidence: 'Read normal file instead',
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
  console.log('FileSystemActor Security Test Results');
  console.log('='.repeat(80) + '\n');

  const vulnerabilities = results.filter(r => r.status === 'VULNERABLE');
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
    });
  }

  // Print passes
  if (passes.length > 0) {
    log(GREEN, '\n✓ TESTS PASSED:', `${passes.length} test(s)`);
    passes.forEach(result => {
      console.log(`  ${GREEN}✓ ${result.name}${RESET}`);
      console.log(`    ${result.description}`);
      if (result.evidence) {
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
  console.log(`${YELLOW}Test Failures:   ${failures.length}${RESET}`);

  if (vulnerabilities.length === 0 && failures.length === 0) {
    log(GREEN, '\n✓ SECURITY STATUS:', 'All tests passed - no vulnerabilities detected');
  } else if (vulnerabilities.length > 0) {
    log(RED, '\n✗ SECURITY STATUS:', 'VULNERABILITIES DETECTED - immediate action required');
  } else {
    log(YELLOW, '\n⚠ SECURITY STATUS:', 'Some tests failed to execute - manual verification needed');
  }

  console.log('='.repeat(80) + '\n');
}

async function runTests() {
  console.log(`
╔════════════════════════════════════════════════════════════════════════════╗
║           FileSystemActor Security Vulnerability Testing                   ║
║                                                                            ║
║  Testing for vulnerabilities identified in QUALITY_REVIEW.md:             ║
║  - Symlink escape attempts                                                 ║
║  - Path traversal attacks                                                  ║
║  - Absolute path handling                                                  ║
║  - Edge cases in path validation                                           ║
╚════════════════════════════════════════════════════════════════════════════╝
  `);

  setupTestEnvironment();

  try {
    // Initialize actor system
    const store = new GraphStore();
    const programManager = new ProgramManager(store);
    const router = new MessageRouter(store, programManager);
    const fsActor = new FileSystemActor(router, TEST_BASE_PATH);

    // Run all tests
    results.push(await test1_SymlinkToSensitiveFile(fsActor));
    results.push(await test2_AbsolutePathAttempt(fsActor));
    results.push(await test3_DirectoryTraversalVariants(fsActor));
    results.push(await test4_SymlinkChain(fsActor));
    results.push(await test5_SymlinkInSubdirectory(fsActor));
    results.push(await test6_WriteViaSymlink(fsActor));
    results.push(await test7_NullByteInjection(fsActor));

    printResults(results);

  } finally {
    cleanupTestEnvironment();
  }

  // Exit with appropriate code
  const hasVulnerabilities = results.some(r => r.status === 'VULNERABLE');
  process.exit(hasVulnerabilities ? 1 : 0);
}

// Run tests
runTests().catch(error => {
  console.error('Fatal error:', error);
  cleanupTestEnvironment();
  process.exit(2);
});
