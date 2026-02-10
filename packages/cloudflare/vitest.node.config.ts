/**
 * Vitest config for Node.js-side tests (provisioner, etc.)
 * These run in standard Node.js, NOT in the Workers runtime.
 * Use the default vitest.config.ts for pool-workers tests.
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['src/__tests__/provisioner*.test.ts'],
    testTimeout: 30_000,
  },
});
