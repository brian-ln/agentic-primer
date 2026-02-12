#!/usr/bin/env bun
/**
 * Integration Tests - Streaming, FileSystemActor, UnsafeCodeComputeActor
 *
 * Comprehensive integration tests validating all three implementations working together:
 * 1. Stream LLM response to FileSystemActor
 * 2. Execute code, capture output, save to file
 * 3. All actors under concurrent load
 * 4. Cross-actor workflows
 */

import { describe, test, expect, beforeAll, afterAll } from 'bun:test';
import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { SessionManager } from './src/entities/session.ts';
import { ModelManager } from './src/entities/model.ts';
import { ProviderManager } from './src/entities/provider.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { SessionActor } from './src/messaging/actors/session.ts';
import { FileSystemActor } from './src/messaging/actors/filesystem.ts';
import { UnsafeCodeComputeActor } from './src/messaging/actors/compute/unsafe-code.ts';
import { address, type TokenStreamEvent } from './src/messaging/message.ts';
import { mkdir, rm } from 'node:fs/promises';
import { resolve } from 'node:path';

// Test setup
let store: GraphStore;
let programManager: ProgramManager;
let providerManager: ProviderManager;
let modelManager: ModelManager;
let sessionManager: SessionManager;
let router: MessageRouter;
let sessionActor: SessionActor;
let fsActor: FileSystemActor;
let codeActor: UnsafeCodeComputeActor;

const testDataDir = resolve('./data/test-integration');

beforeAll(async () => {
  // Initialize system
  store = new GraphStore();
  programManager = new ProgramManager(store);
  providerManager = new ProviderManager(store);
  modelManager = new ModelManager(store, providerManager);
  sessionManager = new SessionManager(store, modelManager);
  router = new MessageRouter(store, programManager);

  // Setup provider and model
  await providerManager.createProvider(
    'test-provider',
    'cloudflare-ai-gateway',
    {
      accountId: process.env.CLOUDFLARE_ACCOUNT_ID || 'test-account',
      gatewayId: process.env.CLOUDFLARE_GATEWAY_ID || 'test-gateway',
    }
  );
  await providerManager.publishProvider('test-provider');

  await modelManager.createModel(
    'test-model',
    'claude-sonnet-4-5',
    'test-provider',
    {
      name: 'Test Model',
      temperature: 0.7,
      maxTokens: 200,
    }
  );
  await modelManager.publishModel('test-model');

  // Create session
  await sessionManager.createSession('test-session', '@(test-model)', {
    owner: '@(test-user)',
  });

  // Create test data directory
  await mkdir(testDataDir, { recursive: true });

  // Initialize actors
  sessionActor = new SessionActor(
    'test-session',
    sessionManager,
    programManager,
    store,
    router,
    modelManager
  );
  fsActor = new FileSystemActor(router, testDataDir);
  codeActor = new UnsafeCodeComputeActor(router);

  // Register actors
  router.registerActor('test/session', sessionActor);
  router.registerActor('test/filesystem', fsActor);
  router.registerActor('test/code-execution', codeActor);
});

afterAll(async () => {
  // Cleanup test data
  try {
    await rm(testDataDir, { recursive: true, force: true });
  } catch (err) {
    // Ignore cleanup errors
  }
});

describe('Integration Tests', () => {
  test('Test 1: Stream mock inference to file', async () => {
    // Mock streaming (credentials may not be available)
    let fullResponse = '';
    const mockTokens = ['Hello', ' ', 'World', '!'];

    // Simulate streaming by using mock streaming actor
    class MockStreamingSessionActor extends SessionActor {
      async stream(payload: any, onChunk: (event: TokenStreamEvent) => Promise<void>) {
        for (const token of mockTokens) {
          await onChunk({
            type: 'token',
            content: token,
            timestamp: Date.now(),
          });
          await new Promise(resolve => setTimeout(resolve, 5));
        }
        await onChunk({ type: 'done', timestamp: Date.now() });
      }
    }

    const mockSessionActor = new MockStreamingSessionActor(
      'test-session-mock',
      sessionManager,
      programManager,
      store,
      router,
      modelManager
    );
    router.registerActor('test/session-mock', mockSessionActor);

    // Stream inference with callback
    await router.streamAsk(
      address('test/session-mock'),
      'inference',
      { message: 'Test streaming' },
      {
        onChunk: async (event) => {
          if (event.type === 'token' && event.content) {
            fullResponse += event.content;
          }
        },
      }
    );

    expect(fullResponse).toBe('Hello World!');

    // Save streamed response to file
    const writeResult = await fsActor.receive({
      id: 'write-stream-output',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'write_file',
      payload: { path: 'stream-output.txt', content: fullResponse },
      timestamp: Date.now(),
    });

    expect(writeResult.success).toBe(true);
    expect(writeResult.payload?.size).toBeGreaterThan(0);

    // Verify file contents
    const readResult = await fsActor.receive({
      id: 'read-stream-output',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'read_file',
      payload: { path: 'stream-output.txt' },
      timestamp: Date.now(),
    });

    expect(readResult.success).toBe(true);
    expect(readResult.payload?.content).toBe(fullResponse);
  }, 10000);

  test('Test 2: Execute code, capture output, save to file', async () => {
    // Execute code with console output
    const codeResult = await codeActor.receive({
      id: 'execute-code',
      pattern: 'ask',
      to: address('test/code-execution'),
      from: address('test/session'),
      type: 'execute',
      payload: {
        code: `
          console.log("Starting computation...");
          const result = 21 * 2;
          console.log("Result calculated:", result);
          return result;
        `,
        language: 'javascript',
      },
      timestamp: Date.now(),
    });

    expect(codeResult.success).toBe(true);
    expect(codeResult.payload?.result).toBe(42);
    expect(codeResult.payload?.logs.length).toBeGreaterThan(0);

    // Format output for file
    const output = [
      'Code Execution Results',
      '======================',
      '',
      `Result: ${codeResult.payload?.result}`,
      '',
      'Console Output:',
      ...codeResult.payload?.logs.map((log: string) => `  ${log}`) || [],
      '',
      `Execution Time: ${codeResult.payload?.executionTime}ms`,
    ].join('\n');

    // Save to file
    const writeResult = await fsActor.receive({
      id: 'write-code-output',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'write_file',
      payload: { path: 'code-output.txt', content: output },
      timestamp: Date.now(),
    });

    expect(writeResult.success).toBe(true);

    // Verify file was created
    const readResult = await fsActor.receive({
      id: 'read-code-output',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'read_file',
      payload: { path: 'code-output.txt' },
      timestamp: Date.now(),
    });

    expect(readResult.success).toBe(true);
    expect(readResult.payload?.content).toContain('Result: 42');
    expect(readResult.payload?.content).toContain('Starting computation...');
  }, 10000);

  test('Test 3: Concurrent operations across all actors', async () => {
    const operations = [];

    // 10 streaming operations (using mock)
    class QuickMockStreamingActor extends SessionActor {
      async stream(payload: any, onChunk: (event: TokenStreamEvent) => Promise<void>) {
        await onChunk({ type: 'token', content: 'OK', timestamp: Date.now() });
        await onChunk({ type: 'done', timestamp: Date.now() });
      }
    }

    const quickMockActor = new QuickMockStreamingActor(
      'quick-mock',
      sessionManager,
      programManager,
      store,
      router,
      modelManager
    );
    router.registerActor('test/quick-mock', quickMockActor);

    for (let i = 0; i < 10; i++) {
      operations.push(
        (async () => {
          let content = '';
          await router.streamAsk(
            address('test/quick-mock'),
            'inference',
            { message: `Test ${i}` },
            {
              onChunk: async (event) => {
                if (event.type === 'token' && event.content) {
                  content += event.content;
                }
              },
            }
          );
          return { type: 'stream', content };
        })()
      );
    }

    // 10 file operations
    for (let i = 0; i < 10; i++) {
      operations.push(
        fsActor.receive({
          id: `write-concurrent-${i}`,
          pattern: 'ask',
          to: address('test/filesystem'),
          from: address('test/session'),
          type: 'write_file',
          payload: { path: `concurrent-${i}.txt`, content: `Test file ${i}` },
          timestamp: Date.now(),
        })
      );
    }

    // 10 code executions
    for (let i = 0; i < 10; i++) {
      operations.push(
        codeActor.receive({
          id: `execute-concurrent-${i}`,
          pattern: 'ask',
          to: address('test/code-execution'),
          from: address('test/session'),
          type: 'execute',
          payload: {
            code: `return ${i} * 2;`,
            language: 'javascript',
          },
          timestamp: Date.now(),
        })
      );
    }

    // Execute all concurrently
    const results = await Promise.all(operations);

    // Verify all succeeded
    const successCount = results.filter(r =>
      (r as any).type === 'stream' ? true : (r as any).success
    ).length;

    expect(successCount).toBe(30);
    expect(results.length).toBe(30);
  }, 15000);

  test('Test 4: Cross-actor workflow - code execution with file I/O', async () => {
    // Step 1: Write input data to file
    const inputData = JSON.stringify({ numbers: [1, 2, 3, 4, 5] });
    const writeInput = await fsActor.receive({
      id: 'write-workflow-input',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'write_file',
      payload: { path: 'workflow-input.json', content: inputData },
      timestamp: Date.now(),
    });

    expect(writeInput.success).toBe(true);

    // Step 2: Read input data
    const readInput = await fsActor.receive({
      id: 'read-workflow-input',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'read_file',
      payload: { path: 'workflow-input.json' },
      timestamp: Date.now(),
    });

    expect(readInput.success).toBe(true);

    // Step 3: Execute code to process data
    const data = JSON.parse(readInput.payload?.content || '{}');
    const codeResult = await codeActor.receive({
      id: 'execute-workflow-process',
      pattern: 'ask',
      to: address('test/code-execution'),
      from: address('test/session'),
      type: 'execute',
      payload: {
        code: `
          const numbers = ${JSON.stringify(data.numbers)};
          const sum = numbers.reduce((a, b) => a + b, 0);
          const avg = sum / numbers.length;
          return { sum, avg, count: numbers.length };
        `,
        language: 'javascript',
      },
      timestamp: Date.now(),
    });

    expect(codeResult.success).toBe(true);
    expect(codeResult.payload?.result.sum).toBe(15);
    expect(codeResult.payload?.result.avg).toBe(3);
    expect(codeResult.payload?.result.count).toBe(5);

    // Step 4: Write results to file
    const outputData = JSON.stringify(codeResult.payload?.result, null, 2);
    const writeOutput = await fsActor.receive({
      id: 'write-workflow-output',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'write_file',
      payload: { path: 'workflow-output.json', content: outputData },
      timestamp: Date.now(),
    });

    expect(writeOutput.success).toBe(true);

    // Verify complete workflow
    const readOutput = await fsActor.receive({
      id: 'read-workflow-output',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'read_file',
      payload: { path: 'workflow-output.json' },
      timestamp: Date.now(),
    });

    expect(readOutput.success).toBe(true);
    const finalResult = JSON.parse(readOutput.payload?.content || '{}');
    expect(finalResult.sum).toBe(15);
    expect(finalResult.avg).toBe(3);
  }, 10000);

  test('Test 5: Error handling across actors', async () => {
    // Test filesystem error handling
    const readNonExistent = await fsActor.receive({
      id: 'read-nonexistent',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'read_file',
      payload: { path: 'nonexistent-file.txt' },
      timestamp: Date.now(),
    });

    expect(readNonExistent.success).toBe(false);
    expect(readNonExistent.error).toContain('File not found');

    // Test code execution error handling
    const executeError = await codeActor.receive({
      id: 'execute-error',
      pattern: 'ask',
      to: address('test/code-execution'),
      from: address('test/session'),
      type: 'execute',
      payload: {
        code: 'throw new Error("Test error");',
        language: 'javascript',
      },
      timestamp: Date.now(),
    });

    expect(executeError.success).toBe(false);
    expect(executeError.error).toContain('Execution error');

    // Test invalid code execution
    const invalidCode = await codeActor.receive({
      id: 'execute-invalid',
      pattern: 'ask',
      to: address('test/code-execution'),
      from: address('test/session'),
      type: 'execute',
      payload: {
        code: '',
        language: 'javascript',
      },
      timestamp: Date.now(),
    });

    expect(invalidCode.success).toBe(false);
    expect(invalidCode.error).toContain('No code provided');
  });

  test('Test 6: List directory contents', async () => {
    // Create multiple files for listing
    const filesToCreate = ['list-test-1.txt', 'list-test-2.txt', 'list-test-3.txt'];

    for (const filename of filesToCreate) {
      await fsActor.receive({
        id: `create-${filename}`,
        pattern: 'ask',
        to: address('test/filesystem'),
        from: address('test/session'),
        type: 'write_file',
        payload: { path: filename, content: `Content of ${filename}` },
        timestamp: Date.now(),
      });
    }

    // List directory
    const listResult = await fsActor.receive({
      id: 'list-directory',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'list_dir',
      payload: { path: '.' },
      timestamp: Date.now(),
    });

    expect(listResult.success).toBe(true);
    expect(listResult.payload?.entries.length).toBeGreaterThanOrEqual(3);

    const fileNames = listResult.payload?.entries.map((e: any) => e.name) || [];
    for (const filename of filesToCreate) {
      expect(fileNames).toContain(filename);
    }
  });

  test('Test 7: Delete file operation', async () => {
    // Create a file to delete
    const createResult = await fsActor.receive({
      id: 'create-for-delete',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'write_file',
      payload: { path: 'to-delete.txt', content: 'Delete me!' },
      timestamp: Date.now(),
    });

    expect(createResult.success).toBe(true);

    // Delete the file
    const deleteResult = await fsActor.receive({
      id: 'delete-file',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'delete_file',
      payload: { path: 'to-delete.txt' },
      timestamp: Date.now(),
    });

    expect(deleteResult.success).toBe(true);
    expect(deleteResult.payload?.deleted).toBe(true);

    // Verify file is deleted
    const readDeleted = await fsActor.receive({
      id: 'read-deleted',
      pattern: 'ask',
      to: address('test/filesystem'),
      from: address('test/session'),
      type: 'read_file',
      payload: { path: 'to-delete.txt' },
      timestamp: Date.now(),
    });

    expect(readDeleted.success).toBe(false);
    expect(readDeleted.error).toContain('File not found');
  });
});

console.log('\n🧪 Integration Test Suite Complete\n');
