import { defineConfig, devices } from '@playwright/test';

/**
 * Playwright configuration for Concept Graph Web Server tests
 * @see https://playwright.dev/docs/test-configuration
 */
export default defineConfig({
  testDir: './tests',

  // Maximum time one test can run
  timeout: 30 * 1000,

  expect: {
    // Maximum time expect() should wait for condition to be met
    timeout: 5000,
  },

  // Run tests in parallel
  fullyParallel: false, // Sequential to avoid port conflicts

  // Fail the build on CI if you accidentally left test.only in the source code
  forbidOnly: !!process.env.CI,

  // Retry on CI only
  retries: process.env.CI ? 2 : 0,

  // Opt out of parallel tests on CI
  workers: process.env.CI ? 1 : 1,

  // Reporter to use
  reporter: 'html',

  // Shared settings for all projects
  use: {
    // Base URL for page.goto('/')
    // Note: Server uses random port, so we need to detect it
    baseURL: process.env.TEST_BASE_URL || 'http://localhost:3000',

    // Collect trace when retrying the failed test
    trace: 'on-first-retry',

    // Screenshot on failure
    screenshot: 'only-on-failure',
  },

  // Configure projects for major browsers
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
    // Uncomment for cross-browser testing
    // {
    //   name: 'firefox',
    //   use: { ...devices['Desktop Firefox'] },
    // },
    // {
    //   name: 'webkit',
    //   use: { ...devices['Desktop Safari'] },
    // },
  ],

  // Run local dev server before starting tests
  // Note: We use a fixed port for testing instead of random port
  webServer: {
    command: 'cd .. && PORT=3000 bun run CONCEPT_GRAPH/server.ts',
    port: 3000,
    reuseExistingServer: !process.env.CI,
    timeout: 10 * 1000,
    stdout: 'pipe',
    stderr: 'pipe',
  },
});
