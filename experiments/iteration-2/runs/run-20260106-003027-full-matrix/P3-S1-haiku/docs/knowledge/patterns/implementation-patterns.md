# Implementation Patterns

## Pattern: API Deprecation Warning

### When to Use
When deprecating an API endpoint that has existing clients. This pattern ensures a smooth migration path while warning users of the deprecated functionality.

### Implementation Steps

1. **Add deprecation marker to endpoint handler**
   - Place at the beginning of the request handler
   - Include clear message pointing to replacement
   - Log with appropriate severity level

2. **Add response header**
   - Use standard HTTP `Deprecation` header
   - Include link to migration documentation

3. **Log deprecation event**
   - Use structured logging with context
   - Include endpoint name, timestamp, requester info
   - Enable monitoring dashboard

4. **Create migration documentation**
   - Document the old endpoint
   - Show the new replacement endpoint
   - Provide code examples
   - Include timeline for removal

5. **Add tests**
   - Verify deprecation warning is issued
   - Verify correct header is set
   - Verify behavior is unchanged during deprecation period

6. **Update documentation**
   - Mark endpoint as deprecated in API docs
   - Link to migration guide
   - Include deprecation timeline

### Code Example

```typescript
// Legacy endpoint handler
app.get('/api/v1/users', (req, res) => {
  // Step 1: Add deprecation warning
  console.warn('[DEPRECATED] GET /api/v1/users will be removed on 2024-12-31. Use GET /api/v2/users instead.');

  // Step 2: Add response header
  res.set('Deprecation', 'true');
  res.set('Sunset', 'Fri, 31 Dec 2024 23:59:59 GMT');
  res.set('Link', '</api/v2/users>; rel="successor-version"');

  // Step 3: Log deprecation event
  logger.warn('legacy_endpoint_accessed', {
    endpoint: '/api/v1/users',
    timestamp: new Date().toISOString(),
    userId: req.user?.id,
    userAgent: req.headers['user-agent'],
  });

  // Normal endpoint logic follows...
  const users = await User.find({});
  res.json(users);
});

// Test example
describe('GET /api/v1/users (deprecated)', () => {
  it('should issue deprecation warning', async () => {
    const warnSpy = jest.spyOn(console, 'warn');

    await request(app)
      .get('/api/v1/users')
      .expect(200);

    expect(warnSpy).toHaveBeenCalledWith(
      expect.stringContaining('[DEPRECATED]')
    );
  });

  it('should include deprecation headers', async () => {
    const res = await request(app)
      .get('/api/v1/users')
      .expect(200);

    expect(res.header['deprecation']).toBe('true');
    expect(res.header['sunset']).toBeDefined();
  });
});
```

### Considerations

**Trade-offs**:
- Client code will still work during deprecation period (smooth migration)
- Requires effort to monitor and eventually remove old endpoint
- May delay complete cleanup

**Performance**:
- Minimal overhead: just logging and headers
- Can be disabled via feature flag if needed

**Compatibility**:
- HTTP standard headers ensure client library support
- Works with any HTTP client
- Gradual deprecation allows time for user migration

**Monitoring**:
- Log deprecation events to identify remaining users
- Use structured logging for dashboarding
- Set removal deadline and track progress

---

## Pattern: Configuration Management

### When to Use
When adding new configuration options that should be environment-specific (dev, staging, prod).

### Implementation Steps

1. **Define configuration schema**
   - Use TypeScript interfaces for type safety
   - Document each config value
   - Provide sensible defaults

2. **Load from multiple sources**
   - Environment variables (highest priority)
   - `.env` file (local development)
   - Config files (production)
   - Defaults (fallback)

3. **Validate on startup**
   - Check required values are present
   - Validate value ranges/formats
   - Fail fast with clear error messages

4. **Export singleton**
   - Single source of truth
   - Prevents multiple configuration instances
   - Easy to test with config mocking

### Code Example

```typescript
// config.ts
import { z } from 'zod';

const configSchema = z.object({
  NODE_ENV: z.enum(['development', 'staging', 'production']).default('development'),
  PORT: z.number().int().positive().default(3000),
  DATABASE_URL: z.string().url(),
  LOG_LEVEL: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
  API_TIMEOUT_MS: z.number().int().positive().default(30000),
});

type Config = z.infer<typeof configSchema>;

let config: Config;

export function loadConfig(): Config {
  const raw = {
    NODE_ENV: process.env.NODE_ENV,
    PORT: process.env.PORT ? parseInt(process.env.PORT) : undefined,
    DATABASE_URL: process.env.DATABASE_URL,
    LOG_LEVEL: process.env.LOG_LEVEL,
    API_TIMEOUT_MS: process.env.API_TIMEOUT_MS ? parseInt(process.env.API_TIMEOUT_MS) : undefined,
  };

  try {
    config = configSchema.parse(raw);
    return config;
  } catch (error) {
    console.error('Configuration validation failed:', error);
    process.exit(1);
  }
}

export function getConfig(): Config {
  if (!config) {
    throw new Error('Configuration not loaded. Call loadConfig() first.');
  }
  return config;
}
```

---

## Pattern: Error Handling in Controllers

### When to Use
When creating new API endpoints or updating request handlers.

### Implementation Steps

1. **Catch all errors**
   - Use try-catch blocks in handlers
   - Don't let errors bubble up unhandled

2. **Log with context**
   - Include request ID for tracing
   - Log full error with stack trace
   - Include relevant user/resource context

3. **Return appropriate HTTP status**
   - 4xx for client errors
   - 5xx for server errors
   - Include error code in response

4. **Send user-friendly message**
   - Don't expose internal implementation details
   - Provide actionable information
   - Include request ID for support reference

### Code Example

```typescript
// Generic error handler middleware
app.use((err, req, res, next) => {
  const requestId = req.id;

  logger.error('request_error', {
    requestId,
    method: req.method,
    path: req.path,
    status: err.status || 500,
    message: err.message,
    stack: err.stack,
    userId: req.user?.id,
  });

  const status = err.status || 500;
  const message = status === 500
    ? 'Internal server error. Reference: ' + requestId
    : err.message;

  res.status(status).json({
    error: {
      message,
      requestId,
      timestamp: new Date().toISOString(),
    },
  });
});
```

---

## Pattern: Database Migrations

### When to Use
When adding or modifying database schema.

### Implementation Steps

1. **Create migration file**
   - Use timestamp-based naming: `YYYYMMDDHHMMSS_description.sql`
   - Make migrations idempotent (can run multiple times safely)

2. **Define up and down**
   - `up`: Apply the change
   - `down`: Revert the change

3. **Test both directions**
   - Verify up migration works
   - Verify down migration works
   - Test on realistic data volume

4. **Document changes**
   - Explain why the change is needed
   - Document any manual steps required
   - Include performance impact

### Code Example

```sql
-- migrations/20240106120000_add_user_email_index.sql

-- Up
-- Create index for faster email lookups in authentication
CREATE INDEX CONCURRENTLY idx_users_email ON users(email);

-- Down
-- Drop the index
DROP INDEX CONCURRENTLY IF EXISTS idx_users_email;
```

---

## Contributing New Patterns

When you discover a pattern that appears in multiple tasks:

1. Extract the pattern into this file
2. Use the standard anatomy above
3. Include concrete code example
4. Document considerations and trade-offs
5. Link to issues/PRs that use this pattern

This keeps the codebase consistent and speeds up future implementation.
