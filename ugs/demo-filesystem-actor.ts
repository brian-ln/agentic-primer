#!/usr/bin/env bun
/**
 * Demo: FileSystemActor
 *
 * Demonstrates secure file system operations with path validation.
 * Shows read, write, list, and delete operations with error handling.
 */

import { MessageRouter } from './src/messaging/router.ts';
import { FileSystemActor } from './src/messaging/actors/filesystem.ts';
import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { mkdir } from 'node:fs/promises';
import { resolve } from 'node:path';

async function demo() {
  console.log('=== FileSystemActor Demo ===\n');

  // Setup
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);

  // Ensure data directory exists
  const dataDir = resolve('./data');
  try {
    await mkdir(dataDir, { recursive: true });
  } catch (err) {
    // Directory may already exist
  }

  // Create and register FileSystemActor
  const fsActor = new FileSystemActor(router, dataDir);
  router.registerActor('test/filesystem', fsActor);

  console.log('✓ FileSystemActor initialized with basePath:', dataDir);
  console.log();

  // Demo 1: Write a file
  console.log('1. Writing a file...');
  const writeResponse = await fsActor.receive({
    id: 'msg_1',
    pattern: 'ask',
    to: '@(filesystem)',
    from: '@(demo)',
    type: 'write_file',
    payload: {
      path: 'test.txt',
      content: 'Hello, FileSystemActor!\nThis is a test file.',
    },
    timestamp: Date.now(),
  });

  if (writeResponse.success) {
    console.log('✓ File written successfully');
    console.log('  Path:', writeResponse.payload?.path);
    console.log('  Size:', writeResponse.payload?.size, 'bytes');
  } else {
    console.log('✗ Write failed:', writeResponse.error);
  }
  console.log();

  // Demo 2: Read the file
  console.log('2. Reading the file...');
  const readResponse = await fsActor.receive({
    id: 'msg_2',
    pattern: 'ask',
    to: '@(filesystem)',
    from: '@(demo)',
    type: 'read_file',
    payload: {
      path: 'test.txt',
    },
    timestamp: Date.now(),
  });

  if (readResponse.success) {
    console.log('✓ File read successfully');
    console.log('  Content:', readResponse.payload?.content);
    console.log('  Size:', readResponse.payload?.size, 'bytes');
  } else {
    console.log('✗ Read failed:', readResponse.error);
  }
  console.log();

  // Demo 3: List directory contents
  console.log('3. Listing directory contents...');
  const listResponse = await fsActor.receive({
    id: 'msg_3',
    pattern: 'ask',
    to: '@(filesystem)',
    from: '@(demo)',
    type: 'list_dir',
    payload: {
      path: '.',
    },
    timestamp: Date.now(),
  });

  if (listResponse.success) {
    console.log('✓ Directory listed successfully');
    console.log('  Entries:', listResponse.payload?.entries.length);
    listResponse.payload?.entries.forEach((entry: any) => {
      console.log(`    - ${entry.name} (${entry.type})`);
    });
  } else {
    console.log('✗ List failed:', listResponse.error);
  }
  console.log();

  // Demo 4: Path validation - try to escape base directory
  console.log('4. Testing path validation (directory traversal attack)...');
  const maliciousReadResponse = await fsActor.receive({
    id: 'msg_4',
    pattern: 'ask',
    to: '@(filesystem)',
    from: '@(demo)',
    type: 'read_file',
    payload: {
      path: '../../../etc/passwd',
    },
    timestamp: Date.now(),
  });

  if (maliciousReadResponse.success) {
    console.log('✗ Security vulnerability! Path validation failed!');
  } else {
    console.log('✓ Path validation working correctly');
    console.log('  Error:', maliciousReadResponse.error);
  }
  console.log();

  // Demo 5: Delete the file
  console.log('5. Deleting the file...');
  const deleteResponse = await fsActor.receive({
    id: 'msg_5',
    pattern: 'ask',
    to: '@(filesystem)',
    from: '@(demo)',
    type: 'delete_file',
    payload: {
      path: 'test.txt',
    },
    timestamp: Date.now(),
  });

  if (deleteResponse.success) {
    console.log('✓ File deleted successfully');
    console.log('  Path:', deleteResponse.payload?.path);
  } else {
    console.log('✗ Delete failed:', deleteResponse.error);
  }
  console.log();

  // Demo 6: Try to read deleted file
  console.log('6. Attempting to read deleted file...');
  const readDeletedResponse = await fsActor.receive({
    id: 'msg_6',
    pattern: 'ask',
    to: '@(filesystem)',
    from: '@(demo)',
    type: 'read_file',
    payload: {
      path: 'test.txt',
    },
    timestamp: Date.now(),
  });

  if (readDeletedResponse.success) {
    console.log('✗ Unexpected success - file should not exist');
  } else {
    console.log('✓ Correctly failed to read deleted file');
    console.log('  Error:', readDeletedResponse.error);
  }
  console.log();

  console.log('=== Demo Complete ===');
}

demo().catch(console.error);
