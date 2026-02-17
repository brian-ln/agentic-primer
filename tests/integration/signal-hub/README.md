# Signal Hub Integration Tests

Comprehensive end-to-end integration tests for the Signal Hub system, validating cross-runtime messaging between SEAG (Node.js) and Browser actors.

## Overview

These tests validate the complete Signal Hub flow:

```
SEAG Actor (Node.js) ←→ Signal Hub (Cloudflare DO) ←→ Browser Actor (Chrome)
```

## Test Categories

### 1. Connection Lifecycle (`connection.test.ts`)
- ✅ SEAG actor connection and authentication
- ✅ Browser actor connection and authentication
- ✅ Session management
- ✅ Graceful disconnect
- ✅ Heartbeat mechanism
- ✅ Concurrent connections

### 2. Actor Discovery (`discovery.test.ts`)
- ✅ Actor registration with capabilities
- ✅ Cross-runtime discovery (SEAG finds Browser, Browser finds SEAG)
- ✅ Discovery filtering by capability
- ✅ Multiple actor discovery
- ✅ Actor unregistration

### 3. Cross-Runtime Messaging (`messaging.test.ts`)
- ✅ SEAG → Browser messaging
- ✅ Browser → SEAG messaging
- ✅ Fire-and-forget (tell pattern)
- ✅ Request-response (ask pattern with ack)
- ✅ Bidirectional communication
- ✅ Message payload integrity
- ✅ Message ordering
- ✅ Metadata preservation

### 4. Broadcast (`broadcast.test.ts`)
- ✅ Broadcast to all actors
- ✅ Cross-runtime broadcast
- ✅ Multi-actor broadcast delivery
- ✅ Broadcast acknowledgments
- ✅ Payload integrity in broadcasts

### 5. Pub/Sub (`pubsub.test.ts`)
- ✅ Subscribe to topics
- ✅ Publish to topics
- ✅ Cross-runtime pub/sub
- ✅ Multiple subscribers per topic
- ✅ Multiple topics with different subscriber sets
- ✅ Unsubscribe functionality

### 6. Error Scenarios (`errors.test.ts`)
- ✅ Unknown actor errors
- ✅ Invalid JWT / unauthorized
- ✅ Message size limits (1MB)
- ✅ Connection failures
- ✅ Invalid message formats
- ✅ Concurrent connection limits
- ✅ Race conditions

## Prerequisites

### Required Dependencies

```bash
# Install dependencies from project root
pnpm install

# Or install from this directory
cd tests/integration/signal-hub
pnpm install
```

### Required Packages

- **Signal Hub Service**: `services/signal-hub/`
- **SEAG Client**: `ugs/src/messaging/signal-hub/`
- **Browser Client**: `packages/signal-hub-client/`
- **Miniflare**: For local Signal Hub instance
- **Vitest**: Test framework
- **ws**: WebSocket polyfill for Node.js

## Running Tests

### All Tests

```bash
# From tests/integration/signal-hub/
pnpm test

# Or from project root
pnpm test --filter @agentic-primer/signal-hub-integration-tests
```

### Individual Test Suites

```bash
# Connection tests
pnpm test:connection

# Discovery tests
pnpm test:discovery

# Messaging tests
pnpm test:messaging

# Broadcast tests
pnpm test:broadcast

# Pub/sub tests
pnpm test:pubsub

# Error scenarios
pnpm test:errors
```

### Watch Mode

```bash
pnpm test:watch
```

### Coverage

```bash
pnpm test:coverage
```

## Test Architecture

### Directory Structure

```
tests/integration/signal-hub/
├── helpers/
│   ├── signal-hub.ts        # Signal Hub instance manager (Miniflare)
│   ├── seag-actor.ts        # SEAG client wrapper
│   ├── browser-actor.ts     # Browser client wrapper
│   └── jwt.ts               # JWT token generation
├── setup.ts                 # Test environment setup/teardown
├── connection.test.ts       # Connection lifecycle tests
├── discovery.test.ts        # Actor discovery tests
├── messaging.test.ts        # Cross-runtime messaging tests
├── broadcast.test.ts        # Broadcast tests
├── pubsub.test.ts          # Pub/sub tests
├── errors.test.ts          # Error scenario tests
├── package.json            # Test dependencies
├── vitest.config.ts        # Vitest configuration
└── README.md               # This file
```

### Test Helpers

#### Signal Hub Instance (`helpers/signal-hub.ts`)

Manages local Signal Hub instance using Miniflare:

```typescript
import { startSignalHub } from './helpers/signal-hub.js';

const hub = await startSignalHub();
// Use hub.url for WebSocket connections
await hub.stop(); // Cleanup
```

#### SEAG Actor Wrapper (`helpers/seag-actor.ts`)

Wraps `SignalHubClient` from `ugs/`:

```typescript
import { createSeagActor } from './helpers/seag-actor.js';

const seagActor = await createSeagActor({
  url: 'ws://localhost:8787/ws',
  jwt: 'eyJhbGci...',
  actorAddress: '@(local/test-seag)',
  capabilities: ['compute', 'inference'],
});

seagActor.send(targetAddress, 'test:message', { data: 'hello' });
await seagActor.disconnect();
```

#### Browser Actor Wrapper (`helpers/browser-actor.ts`)

Wraps `SignalHubClient` from `packages/signal-hub-client/`:

```typescript
import { createBrowserActor } from './helpers/browser-actor.js';

const browserActor = await createBrowserActor({
  url: 'ws://localhost:8787/ws',
  jwt: 'eyJhbGci...',
  actorAddress: '@(browser/test-browser)',
  capabilities: ['ui', 'interaction'],
});

browserActor.send(targetAddress, 'test:message', { data: 'hello' });
await browserActor.disconnect();
```

#### JWT Generator (`helpers/jwt.ts`)

Generates test JWTs:

```typescript
import { generateSeagActorJWT, generateBrowserActorJWT } from './helpers/jwt.js';

const seagJwt = await generateSeagActorJWT(
  '@(local/my-actor)' as CanonicalAddress,
  ['compute', 'inference']
);

const browserJwt = await generateBrowserActorJWT(
  '@(browser/my-widget)' as CanonicalAddress,
  ['ui', 'rendering']
);
```

### Test Environment Setup

Each test file uses a standardized setup/teardown:

```typescript
import { setupTestEnvironment, teardownTestEnvironment } from './setup.js';

let env: TestEnvironment;

beforeAll(async () => {
  env = await setupTestEnvironment();
  // env.hub - Signal Hub instance
  // env.seagJwt - SEAG actor JWT
  // env.browserJwt - Browser actor JWT
  // env.seagAddress - SEAG actor canonical address
  // env.browserAddress - Browser actor canonical address
});

afterAll(async () => {
  await teardownTestEnvironment(env);
});
```

## Writing New Tests

### Basic Test Pattern

```typescript
import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';

describe('My Test Suite', () => {
  let env: TestEnvironment;
  let seagActor: SeagActorWrapper;
  let browserActor: BrowserActorWrapper;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  beforeEach(async () => {
    [seagActor, browserActor] = await Promise.all([
      createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      }),
      createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      }),
    ]);
  });

  afterEach(async () => {
    if (seagActor) await seagActor.disconnect();
    if (browserActor) await browserActor.disconnect();
  });

  it('should test something', async () => {
    const messagePromise = browserActor.waitForMessage(
      msg => msg.type === 'test:ping',
      5000
    );

    seagActor.send(env.browserAddress, 'test:ping', { data: 'hello' });

    const received = await messagePromise;
    expect(received.payload).toEqual({ data: 'hello' });
  });
});
```

### Waiting for Messages

```typescript
// Wait for specific message type
const msg = await browserActor.waitForMessage(
  msg => msg.type === 'test:response',
  5000 // timeout
);

// Wait for message with specific payload
const msg = await browserActor.waitForMessage(
  msg => msg.type === 'test:data' && msg.payload.id === 'expected-id',
  10000
);
```

### Testing Cross-Runtime Flows

```typescript
// SEAG sends to Browser
const browserPromise = browserActor.waitForMessage(
  msg => msg.type === 'test:request',
  5000
);

seagActor.send(env.browserAddress, 'test:request', { step: 1 });

const browserMsg = await browserPromise;
expect(browserMsg.payload).toEqual({ step: 1 });

// Browser responds to SEAG
const seagPromise = seagActor.waitForMessage(
  msg => msg.type === 'test:response',
  5000
);

browserActor.send(env.seagAddress, 'test:response', { step: 2 });

const seagMsg = await seagPromise;
expect(seagMsg.payload).toEqual({ step: 2 });
```

## CI Integration

### GitHub Actions Example

```yaml
name: Signal Hub Integration Tests

on: [push, pull_request]

jobs:
  integration:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
        with:
          version: 10
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'pnpm'
      - run: pnpm install
      - run: pnpm test --filter @agentic-primer/signal-hub-integration-tests
```

## Troubleshooting

### Tests Timeout

- Check that Miniflare is starting correctly
- Increase test timeout in `vitest.config.ts`
- Check for port conflicts (Miniflare uses random ports)

### Connection Failures

- Verify Signal Hub service builds correctly
- Check JWT generation (default secret: `dev-secret-change-in-production`)
- Ensure WebSocket polyfill is loaded for Node.js

### Message Not Received

- Verify both actors are connected (`getState() === 'connected'`)
- Check actor addresses are correctly formatted
- Increase wait timeout in `waitForMessage()`
- Check message type matches exactly

### Miniflare Errors

- Update Miniflare to latest version
- Check `wrangler.toml` configuration
- Verify Durable Object bindings

## Performance

### Test Execution Time

- Full suite: ~60-90 seconds
- Individual test file: ~10-20 seconds
- Connection tests: ~15 seconds (includes heartbeat test)

### Optimizations

- Tests run sequentially to avoid port conflicts
- Actors are created fresh for each test (clean state)
- Miniflare instance is shared per test file (via `beforeAll`/`afterAll`)

## Future Enhancements

- [ ] Add reconnection tests
- [ ] Add stress tests (1000+ messages)
- [ ] Add network partition simulation
- [ ] Add latency testing
- [ ] Add concurrent actor limits testing
- [ ] Add message replay tests
- [ ] Add snapshot testing for message formats

## Related Documentation

- [Signal Hub Protocol](../../../docs/signal-hub/PROTOCOL.md)
- [Signal Hub Architecture](../../../docs/signal-hub/ARCHITECTURE.md)
- [SEAG Integration](../../../ugs/src/messaging/signal-hub/README.md)
- [Browser Client](../../../packages/signal-hub-client/README.md)

## Support

For issues or questions:
1. Check this README
2. Review test examples
3. Check Signal Hub unit tests (`services/signal-hub/src/**/*.test.ts`)
4. Open an issue with test failure logs
