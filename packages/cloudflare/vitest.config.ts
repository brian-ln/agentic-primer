import { defineWorkersConfig } from '@cloudflare/vitest-pool-workers/config';

export default defineWorkersConfig({
  test: {
    // Exclude provisioner tests — they run in Node.js via vitest.node.config.ts
    exclude: ['src/__tests__/provisioner*.test.ts', 'node_modules/**'],
    poolOptions: {
      workers: {
        wrangler: {
          configPath: './wrangler.toml',
        },
      },
    },
  },
});
