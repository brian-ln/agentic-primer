import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['**/*.test.ts'],
    exclude: ['**/node_modules/**', '**/dist/**'],
    testTimeout: 30000, // 30s timeout for integration tests
    hookTimeout: 30000,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['helpers/**/*.ts', 'setup.ts'],
      exclude: ['**/*.test.ts', '**/__tests__/**'],
    },
    // Run tests sequentially to avoid port conflicts with Miniflare
    sequence: {
      concurrent: false,
    },
  },
});
