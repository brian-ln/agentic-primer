#!/usr/bin/env bun
/**
 * Test worker isolation in Bun's Web Worker implementation
 */

const workerCode = `
self.onmessage = (event) => {
  try {
    // Test 1: Direct access
    const directProcess = typeof process;
    const directBun = typeof Bun;

    // Test 2: Via this
    const thisProcess = typeof this.process;
    const thisBun = typeof this.Bun;

    // Test 3: Via globalThis
    const globalProcess = typeof globalThis.process;
    const globalBun = typeof globalThis.Bun;

    // Test 4: Via Function constructor
    const fn = Function('return this');
    const glob = fn();
    const funcProcess = typeof glob.process;
    const funcBun = typeof glob.Bun;

    // Test 5: Via AsyncFunction constructor
    const AsyncFunction = (async function(){}).constructor;
    const asyncFn = new AsyncFunction('return this.process || this.Bun');
    const asyncResult = asyncFn();

    self.postMessage({
      direct: { process: directProcess, Bun: directBun },
      this: { process: thisProcess, Bun: thisBun },
      globalThis: { process: globalProcess, Bun: globalBun },
      Function: { process: funcProcess, Bun: funcBun },
      AsyncFunction: { hasAccess: asyncResult !== undefined, value: typeof asyncResult },
    });
  } catch (error) {
    self.postMessage({ error: error.message });
  }
};
`;

// Create a data URL worker
const blob = new Blob([workerCode], { type: 'text/javascript' });
const worker = new Worker(URL.createObjectURL(blob));

worker.onmessage = (event) => {
  console.log('Worker isolation test results:');
  console.log(JSON.stringify(event.data, null, 2));
  worker.terminate();
};

worker.onerror = (error) => {
  console.error('Worker error:', error);
  worker.terminate();
};
