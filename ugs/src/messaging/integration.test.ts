#!/usr/bin/env bun
/**
 * Integration Tests - Message Layer Workflows
 *
 * Tests design intent from GRAPH_ACTOR_IMPLEMENTATION.md (lines 200-640):
 * 1. Programs as Actors (lines 308-324)
 * 2. Actor-to-Actor Messaging (lines 291-296, 609-611)
 * 3. Document Actors (lines 280-283, 339-349)
 * 4. Tool Orchestration (lines 364, 285-289)
 *
 * These tests validate actual use cases and workflows, not just line coverage.
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import GraphStore from '@src/graph.ts';
import { ProgramManager } from '@src/entities/program.ts';
import { ActorSystem, address } from './index.ts';
import { MessageRouter } from './router.ts';
import { BashToolActor, ReadToolActor, WriteToolActor } from './actors/tool.ts';
import { existsSync } from 'fs';
import { unlink, mkdir } from 'fs/promises';
import { join } from 'path';

const TEST_DIR = join(__dirname, '.integration-tmp');

describe('Integration: Programs as Actors', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let actorSystem: ActorSystem;

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    actorSystem = new ActorSystem(store, programManager);
  });

  test('calculator program addressable via @(calculator)', async () => {
    // Demo scenario from lines 308-324
    await programManager.createProgram(
      'calculator',
      `
      const { operation, a, b } = input.message;
      switch(operation) {
        case 'add': return a + b;
        case 'multiply': return a * b;
        case 'subtract': return a - b;
        default: throw new Error('Unknown operation');
      }
      `,
      { name: 'Calculator' }
    );

    await programManager.publishProgram('calculator');

    // Test addition
    const addResult = await actorSystem.send(
      address('calculator'),
      'calculate',
      { operation: 'add', a: 5, b: 3 }
    );
    expect(addResult.success).toBe(true);
    expect(addResult.payload).toBe(8);

    // Test multiplication
    const mulResult = await actorSystem.send(
      address('calculator'),
      'calculate',
      { operation: 'multiply', a: 7, b: 6 }
    );
    expect(mulResult.success).toBe(true);
    expect(mulResult.payload).toBe(42);

    // Test subtraction
    const subResult = await actorSystem.send(
      address('calculator'),
      'calculate',
      { operation: 'subtract', a: 10, b: 4 }
    );
    expect(subResult.success).toBe(true);
    expect(subResult.payload).toBe(6);
  });

  test('programs can use this.ask() internally', async () => {
    // Create a program that uses this.ask() to call another program
    await programManager.createProgram(
      'echo',
      `
      return { echo: input.message, timestamp: Date.now() };
      `,
      { name: 'Echo' }
    );

    await programManager.publishProgram('echo');

    await programManager.createProgram(
      'echo-caller',
      `
      // Program internally calls another actor
      const echoResult = await this.ask('@(echo)', 'echo', 'Hello from program');
      return {
        received: echoResult,
        message: 'Successfully called echo actor internally'
      };
      `,
      { name: 'Echo Caller' }
    );

    await programManager.publishProgram('echo-caller');

    const result = await actorSystem.send(
      address('echo-caller'),
      'call',
      {}
    );

    expect(result.success).toBe(true);
    expect(result.payload.message).toBe('Successfully called echo actor internally');
    expect(result.payload.received).toBeDefined();
  });

  test('programs can use this.tell() for fire-and-forget', async () => {
    // Create a log collector program
    const logs: any[] = [];
    await programManager.createProgram(
      'logger',
      `
      // Store log in-memory (in real use, would persist)
      return { logged: true, message: input.message };
      `,
      { name: 'Logger' }
    );

    await programManager.publishProgram('logger');

    await programManager.createProgram(
      'worker',
      `
      // Fire-and-forget log message
      await this.tell('@(logger)', 'log', { event: 'work_started' });

      // Do work
      const result = 42;

      // Fire-and-forget log completion
      await this.tell('@(logger)', 'log', { event: 'work_completed', result });

      return { result, logged: true };
      `,
      { name: 'Worker' }
    );

    await programManager.publishProgram('worker');

    const result = await actorSystem.send(
      address('worker'),
      'work',
      {}
    );

    expect(result.success).toBe(true);
    expect(result.payload.result).toBe(42);
    expect(result.payload.logged).toBe(true);
  });
});

describe('Integration: Actor-to-Actor Messaging', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let actorSystem: ActorSystem;
  let router: MessageRouter;
  let bashTool: BashToolActor;

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    actorSystem = new ActorSystem(store, programManager);
    router = (actorSystem as any).router;

    // Register tool actors
    bashTool = new BashToolActor(router);
    router.registerActor('tool-bash', bashTool);
  });

  test('program internally invokes bash tool actor', async () => {
    // Demo scenario from lines 609-611
    await programManager.createProgram(
      'file-checker',
      `
      // Program internally calls bash tool via this.ask()
      const bashResult = await this.ask('@(tool-bash)', 'execute', {
        command: 'echo "test" | wc -c'
      });

      return {
        output: bashResult.stdout.trim(),
        message: 'Called bash internally',
        wasActorCall: true
      };
      `,
      { name: 'File Checker' }
    );

    await programManager.publishProgram('file-checker');

    const result = await actorSystem.send(
      address('file-checker'),
      'check',
      {}
    );

    expect(result.success).toBe(true);
    expect(result.payload.message).toBe('Called bash internally');
    expect(result.payload.wasActorCall).toBe(true);
    expect(result.payload.output).toBeTruthy();
  });

  test('chained actor invocations (3 levels deep)', async () => {
    // Create a chain: program-a -> program-b -> program-c
    await programManager.createProgram(
      'program-c',
      `
      return { level: 'c', value: 100 };
      `,
      { name: 'Program C' }
    );

    await programManager.publishProgram('program-c');

    await programManager.createProgram(
      'program-b',
      `
      const cResult = await this.ask('@(program-c)', 'execute', {});
      return { level: 'b', fromC: cResult, value: cResult.value + 10 };
      `,
      { name: 'Program B' }
    );

    await programManager.publishProgram('program-b');

    await programManager.createProgram(
      'program-a',
      `
      const bResult = await this.ask('@(program-b)', 'execute', {});
      return { level: 'a', fromB: bResult, finalValue: bResult.value + 5 };
      `,
      { name: 'Program A' }
    );

    await programManager.publishProgram('program-a');

    const result = await actorSystem.send(
      address('program-a'),
      'execute',
      {}
    );

    expect(result.success).toBe(true);
    expect(result.payload.level).toBe('a');
    expect(result.payload.finalValue).toBe(115); // 100 + 10 + 5
    expect(result.payload.fromB.fromC.value).toBe(100);
  });

  test('program orchestrates multiple tool actors', async () => {
    const writeTool = new WriteToolActor(router);
    const readTool = new ReadToolActor(router);
    router.registerActor('tool-write', writeTool);
    router.registerActor('tool-read', readTool);

    const testFile = join(TEST_DIR, 'orchestration-test.txt');

    await mkdir(TEST_DIR, { recursive: true });

    await programManager.createProgram(
      'file-processor',
      `
      const filePath = input.message.path;
      const content = input.message.content;

      // Write file
      const writeResult = await this.ask('@(tool-write)', 'write', {
        path: filePath,
        content: content
      });

      // Read it back
      const readResult = await this.ask('@(tool-read)', 'read', {
        path: filePath
      });

      // Verify with bash
      const bashResult = await this.ask('@(tool-bash)', 'execute', {
        command: \`wc -c < \${filePath}\`
      });

      return {
        wrote: writeResult.path,
        read: readResult.content,
        size: bashResult.stdout.trim(),
        orchestrated: true
      };
      `,
      { name: 'File Processor' }
    );

    await programManager.publishProgram('file-processor');

    const result = await actorSystem.send(
      address('file-processor'),
      'process',
      { path: testFile, content: 'Hello World!' }
    );

    expect(result.success).toBe(true);
    expect(result.payload.orchestrated).toBe(true);
    expect(result.payload.read).toBe('Hello World!');

    // Cleanup
    if (existsSync(testFile)) {
      await unlink(testFile);
    }
  });
});

describe('Integration: Document Actors', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let actorSystem: ActorSystem;

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    actorSystem = new ActorSystem(store, programManager);
  });

  test('query session/information nodes via messages', async () => {
    // Demo scenario from lines 280-283
    // Create an information node
    const sessionNode = await store.addNode('test-session', 'session', {
      name: 'Test Session',
      created: Date.now(),
    });

    // Query it as a document actor
    const result = await actorSystem.send(
      address(sessionNode.id),
      'query',
      { fields: ['name', 'created', 'type'] }
    );

    expect(result.success).toBe(true);
    expect(result.payload.id).toBe(sessionNode.id);
    expect(result.payload.type).toBe('session');
    expect(result.payload.properties.name).toBe('Test Session');
  });

  test('program queries document actor for data', async () => {
    // Create information node
    const configNode = await store.addNode('test-config', 'information', {
      apiEndpoint: 'https://api.example.com',
      timeout: 5000,
    });

    await programManager.createProgram(
      'api-client',
      `
      // Program queries configuration document
      const config = await this.ask('@(${configNode.id})', 'query', {});

      return {
        endpoint: config.properties.apiEndpoint,
        timeout: config.properties.timeout,
        usedDocumentActor: true
      };
      `,
      { name: 'API Client' }
    );

    await programManager.publishProgram('api-client');

    const result = await actorSystem.send(
      address('api-client'),
      'config',
      {}
    );

    expect(result.success).toBe(true);
    expect(result.payload.endpoint).toBe('https://api.example.com');
    expect(result.payload.timeout).toBe(5000);
    expect(result.payload.usedDocumentActor).toBe(true);
  });
});

describe('Integration: Tool Orchestration', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let actorSystem: ActorSystem;
  let router: MessageRouter;

  beforeEach(async () => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    actorSystem = new ActorSystem(store, programManager);
    router = (actorSystem as any).router;

    // Register all tool actors
    router.registerActor('tool-bash', new BashToolActor(router));
    router.registerActor('tool-read', new ReadToolActor(router));
    router.registerActor('tool-write', new WriteToolActor(router));

    await mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    // Cleanup test files
    const testFiles = [
      join(TEST_DIR, 'workflow-test.txt'),
      join(TEST_DIR, 'pipeline-input.txt'),
      join(TEST_DIR, 'pipeline-output.txt'),
    ];

    for (const file of testFiles) {
      if (existsSync(file)) {
        await unlink(file);
      }
    }
  });

  test('real file I/O through actor interface', async () => {
    // Demo scenario from lines 285-289
    const testFile = join(TEST_DIR, 'workflow-test.txt');

    await programManager.createProgram(
      'file-workflow',
      `
      const path = input.message.path;

      // Step 1: Write file
      await this.ask('@(tool-write)', 'write', {
        path: path,
        content: 'Step 1: Written\\nStep 2: Processing\\n'
      });

      // Step 2: Read file
      const readResult = await this.ask('@(tool-read)', 'read', {
        path: path
      });

      // Step 3: Count lines with bash
      const countResult = await this.ask('@(tool-bash)', 'execute', {
        command: \`wc -l < \${path}\`
      });

      return {
        content: readResult.content,
        lineCount: parseInt(countResult.stdout.trim()),
        workflow: 'write -> read -> bash'
      };
      `,
      { name: 'File Workflow' }
    );

    await programManager.publishProgram('file-workflow');

    const result = await actorSystem.send(
      address('file-workflow'),
      'execute',
      { path: testFile }
    );

    expect(result.success).toBe(true);
    expect(result.payload.lineCount).toBe(2);
    expect(result.payload.workflow).toBe('write -> read -> bash');
    expect(result.payload.content).toContain('Step 1');
  });

  test('complex tool pipeline with error handling', async () => {
    const inputFile = join(TEST_DIR, 'pipeline-input.txt');
    const outputFile = join(TEST_DIR, 'pipeline-output.txt');

    await programManager.createProgram(
      'data-pipeline',
      `
      const inputPath = input.message.input;
      const outputPath = input.message.output;

      try {
        // Write input data
        await this.ask('@(tool-write)', 'write', {
          path: inputPath,
          content: 'line1\\nline2\\nline3\\n'
        });

        // Process with bash
        const processResult = await this.ask('@(tool-bash)', 'execute', {
          command: \`cat \${inputPath} | grep -v line2 > \${outputPath}\`
        });

        // Read result
        const resultContent = await this.ask('@(tool-read)', 'read', {
          path: outputPath
        });

        return {
          success: true,
          output: resultContent.content,
          steps: ['write', 'process', 'read']
        };
      } catch (error) {
        return {
          success: false,
          error: error.message
        };
      }
      `,
      { name: 'Data Pipeline' }
    );

    await programManager.publishProgram('data-pipeline');

    const result = await actorSystem.send(
      address('data-pipeline'),
      'run',
      { input: inputFile, output: outputFile }
    );

    expect(result.success).toBe(true);
    expect(result.payload.steps).toEqual(['write', 'process', 'read']);
    expect(result.payload.output).not.toContain('line2');
  });

  test('concurrent tool invocations', async () => {
    await programManager.createProgram(
      'parallel-executor',
      `
      // Execute multiple bash commands in parallel
      const [result1, result2, result3] = await Promise.all([
        this.ask('@(tool-bash)', 'execute', { command: 'echo "task1"' }),
        this.ask('@(tool-bash)', 'execute', { command: 'echo "task2"' }),
        this.ask('@(tool-bash)', 'execute', { command: 'echo "task3"' })
      ]);

      return {
        results: [
          result1.stdout.trim(),
          result2.stdout.trim(),
          result3.stdout.trim()
        ],
        concurrent: true
      };
      `,
      { name: 'Parallel Executor' }
    );

    await programManager.publishProgram('parallel-executor');

    const start = Date.now();
    const result = await actorSystem.send(
      address('parallel-executor'),
      'execute',
      {}
    );
    const duration = Date.now() - start;

    expect(result.success).toBe(true);
    expect(result.payload.concurrent).toBe(true);
    expect(result.payload.results).toEqual(['task1', 'task2', 'task3']);

    // Should be faster than sequential (< 1 second for 3 echo commands)
    expect(duration).toBeLessThan(1000);
  });
});

describe('Integration: End-to-End Workflows', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let actorSystem: ActorSystem;
  let router: MessageRouter;

  beforeEach(async () => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    actorSystem = new ActorSystem(store, programManager);
    router = (actorSystem as any).router;

    router.registerActor('tool-bash', new BashToolActor(router));
    router.registerActor('tool-read', new ReadToolActor(router));
    router.registerActor('tool-write', new WriteToolActor(router));

    await mkdir(TEST_DIR, { recursive: true });
  });

  test('complete workflow: config -> program -> tools -> result', async () => {
    // Create configuration document
    const configNode = await store.addNode('test-workflow-config', 'information', {
      outputDir: TEST_DIR,
      prefix: 'test-',
    });

    // Create workflow program
    await programManager.createProgram(
      'report-generator',
      `
      // Get configuration
      const config = await this.ask('@(${configNode.id})', 'query', {});
      const { outputDir, prefix } = config.properties;

      // Generate report content
      const timestamp = Date.now();
      const filename = \`\${outputDir}/\${prefix}\${timestamp}.txt\`;

      // Write report
      await this.ask('@(tool-write)', 'write', {
        path: filename,
        content: \`Report Generated: \${new Date().toISOString()}\\nStatus: Success\\n\`
      });

      // Verify with bash
      const verifyResult = await this.ask('@(tool-bash)', 'execute', {
        command: \`test -f \${filename} && echo "exists"\`
      });

      return {
        reportFile: filename,
        verified: verifyResult.stdout.trim() === 'exists',
        workflow: 'config -> program -> tools'
      };
      `,
      { name: 'Report Generator' }
    );

    await programManager.publishProgram('report-generator');

    const result = await actorSystem.send(
      address('report-generator'),
      'generate',
      {}
    );

    expect(result.success).toBe(true);
    expect(result.payload.verified).toBe(true);
    expect(result.payload.workflow).toBe('config -> program -> tools');
    expect(existsSync(result.payload.reportFile)).toBe(true);

    // Cleanup
    if (existsSync(result.payload.reportFile)) {
      await unlink(result.payload.reportFile);
    }
  });

  test('error propagation through actor chain', async () => {
    await programManager.createProgram(
      'failing-program',
      `
      throw new Error('Intentional failure');
      `,
      { name: 'Failing Program' }
    );

    await programManager.publishProgram('failing-program');

    await programManager.createProgram(
      'error-handler',
      `
      try {
        await this.ask('@(failing-program)', 'execute', {});
        return { caught: false };
      } catch (error) {
        return {
          caught: true,
          errorMessage: error.message,
          handled: true
        };
      }
      `,
      { name: 'Error Handler' }
    );

    await programManager.publishProgram('error-handler');

    const result = await actorSystem.send(
      address('error-handler'),
      'handle',
      {}
    );

    // The error should be caught and handled
    expect(result.success).toBe(true);
    expect(result.payload.caught).toBe(true);
    expect(result.payload.handled).toBe(true);
  });
});
