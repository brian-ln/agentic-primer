# Pattern: API Error Handling

**Category:** Patterns
**Domain:** API Design
**Last Updated:** 2026-01-08
**Status:** Active

## Problem

API endpoints return inconsistent error responses, making it difficult for clients to:
- Parse error information reliably
- Display meaningful error messages to users
- Implement consistent error handling logic
- Debug production issues

**Symptoms:**
- Different endpoints use different error formats
- Status codes don't match error types
- Missing context in error responses
- Client code has duplicated error handling

## Solution

Use a standardized error response format across all API endpoints with:
1. Consistent HTTP status codes
2. Structured error response body
3. Error codes for programmatic handling
4. Human-readable error messages
5. Additional context when available

## Implementation

### Error Response Schema

```typescript
interface ErrorResponse {
  error: {
    code: string;           // Machine-readable error code (e.g., "INVALID_INPUT")
    message: string;        // Human-readable error message
    details?: string;       // Additional context (optional)
    field?: string;         // Specific field that caused error (optional)
    timestamp: string;      // ISO 8601 timestamp
    requestId: string;      // Request tracking ID for debugging
  }
}
```

### HTTP Status Code Mapping

| Status Code | Use Case | Example Error Code |
|-------------|----------|-------------------|
| 400 | Invalid request input | `INVALID_INPUT`, `VALIDATION_ERROR` |
| 401 | Authentication required | `UNAUTHORIZED`, `TOKEN_EXPIRED` |
| 403 | Insufficient permissions | `FORBIDDEN`, `ACCESS_DENIED` |
| 404 | Resource not found | `NOT_FOUND` |
| 409 | Resource conflict | `CONFLICT`, `DUPLICATE_ENTRY` |
| 422 | Valid syntax but unprocessable | `BUSINESS_LOGIC_ERROR` |
| 429 | Rate limit exceeded | `RATE_LIMIT_EXCEEDED` |
| 500 | Internal server error | `INTERNAL_ERROR` |
| 503 | Service unavailable | `SERVICE_UNAVAILABLE` |

### Example Implementation (Node.js/Express)

```typescript
// Error handler middleware
class APIError extends Error {
  constructor(
    public statusCode: number,
    public code: string,
    public message: string,
    public details?: string,
    public field?: string
  ) {
    super(message);
    this.name = 'APIError';
  }
}

// Error response builder
function buildErrorResponse(error: APIError, requestId: string): ErrorResponse {
  return {
    error: {
      code: error.code,
      message: error.message,
      details: error.details,
      field: error.field,
      timestamp: new Date().toISOString(),
      requestId: requestId
    }
  };
}

// Express error middleware
app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
  // Generate unique request ID
  const requestId = req.headers['x-request-id'] as string || generateRequestId();

  // Handle known API errors
  if (err instanceof APIError) {
    const errorResponse = buildErrorResponse(err, requestId);
    return res.status(err.statusCode).json(errorResponse);
  }

  // Handle unexpected errors
  const errorResponse = buildErrorResponse(
    new APIError(500, 'INTERNAL_ERROR', 'An unexpected error occurred'),
    requestId
  );

  // Log unexpected errors for debugging
  console.error('Unexpected error:', err, { requestId });

  return res.status(500).json(errorResponse);
});

// Usage in route handlers
app.post('/api/users', async (req, res, next) => {
  try {
    const { email, password } = req.body;

    // Validation
    if (!email || !isValidEmail(email)) {
      throw new APIError(
        400,
        'INVALID_INPUT',
        'Invalid email address',
        'Email must be a valid email format',
        'email'
      );
    }

    // Business logic
    const existingUser = await findUserByEmail(email);
    if (existingUser) {
      throw new APIError(
        409,
        'DUPLICATE_ENTRY',
        'User already exists',
        'A user with this email address is already registered',
        'email'
      );
    }

    // Create user
    const user = await createUser({ email, password });
    res.status(201).json({ user });

  } catch (error) {
    next(error); // Pass to error middleware
  }
});
```

### Example Error Responses

**Validation Error:**
```json
{
  "error": {
    "code": "INVALID_INPUT",
    "message": "Invalid email address",
    "details": "Email must be a valid email format",
    "field": "email",
    "timestamp": "2026-01-08T12:34:56.789Z",
    "requestId": "req_abc123xyz"
  }
}
```

**Authentication Error:**
```json
{
  "error": {
    "code": "UNAUTHORIZED",
    "message": "Authentication required",
    "details": "Please provide a valid API token",
    "timestamp": "2026-01-08T12:34:56.789Z",
    "requestId": "req_def456uvw"
  }
}
```

**Not Found Error:**
```json
{
  "error": {
    "code": "NOT_FOUND",
    "message": "User not found",
    "details": "No user exists with ID 12345",
    "timestamp": "2026-01-08T12:34:56.789Z",
    "requestId": "req_ghi789rst"
  }
}
```

## Benefits

1. **Consistent Client Handling:** Clients can parse all errors the same way
2. **Better Debugging:** Request IDs link errors to logs
3. **User-Friendly:** Clear messages for display to users
4. **Type-Safe:** Error codes enable programmatic handling
5. **Maintainable:** Centralized error handling logic

## Trade-offs

**Pros:**
- Reduces client-side error handling complexity
- Improves API developer experience
- Easier to document and test
- Better observability with request IDs

**Cons:**
- Requires discipline to maintain consistency
- May expose more information than desired (use carefully)
- Overhead of creating custom error classes

## When to Use

✅ **Use this pattern when:**
- Building REST APIs for external clients
- Need consistent error handling across endpoints
- Want better debugging and observability
- Building APIs consumed by multiple clients

❌ **Consider alternatives when:**
- Building internal-only microservices (simpler formats may suffice)
- Using GraphQL (different error handling conventions)
- Performance is critical (minimal overhead acceptable)

## Testing

```typescript
describe('API Error Handling', () => {
  it('should return 400 for invalid input', async () => {
    const response = await request(app)
      .post('/api/users')
      .send({ email: 'invalid-email' });

    expect(response.status).toBe(400);
    expect(response.body.error.code).toBe('INVALID_INPUT');
    expect(response.body.error.field).toBe('email');
    expect(response.body.error.requestId).toBeDefined();
  });

  it('should return 409 for duplicate entries', async () => {
    await createUser({ email: 'test@example.com', password: 'pass123' });

    const response = await request(app)
      .post('/api/users')
      .send({ email: 'test@example.com', password: 'pass456' });

    expect(response.status).toBe(409);
    expect(response.body.error.code).toBe('DUPLICATE_ENTRY');
  });
});
```

## Related Knowledge

- **Decisions:** [001-use-rest-api.md](../decisions/001-use-rest-api.md) - Why we chose REST
- **Insights:** [copilot-best-practices.md](../insights/copilot-best-practices.md) - Error handling in generated code

## References

- [RFC 7807: Problem Details for HTTP APIs](https://datatracker.ietf.org/doc/html/rfc7807)
- [Best Practices for REST API Error Handling](https://www.baeldung.com/rest-api-error-handling-best-practices)
- [HTTP Status Code Definitions](https://httpstatuses.com/)

## Changelog

- **2026-01-08:** Initial pattern documentation
- **Future:** Add rate limiting error examples
