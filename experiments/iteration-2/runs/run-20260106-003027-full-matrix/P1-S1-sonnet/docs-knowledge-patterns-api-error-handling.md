# Pattern: API Error Handling

## Pattern Name
Standardized API Error Response Format

## Problem
API endpoints return errors in inconsistent formats, making it difficult for:
- Frontend developers to parse and display errors
- API consumers to handle errors programmatically
- Monitoring systems to aggregate error metrics

**Symptoms:**
- Error responses vary by endpoint (string vs object vs array)
- Missing context (error codes, request IDs, stack traces)
- Difficult to distinguish client vs server errors

## Context
Use this pattern when:
- Building REST or GraphQL APIs
- Designing error responses for any public-facing service
- Standardizing error handling across microservices

## Solution

### Error Response Schema

```javascript
{
  "error": {
    "code": "RESOURCE_NOT_FOUND",        // Machine-readable error code
    "message": "User with ID 123 not found",  // Human-readable message
    "details": {                          // Optional: Additional context
      "resource": "User",
      "id": "123",
      "suggestion": "Check if user ID is correct"
    },
    "requestId": "req_abc123xyz",        // For support/debugging
    "timestamp": "2026-01-06T00:32:00Z", // When error occurred
    "path": "/api/users/123"             // Which endpoint failed
  }
}
```

### HTTP Status Code Mapping

| Status | Code Pattern | Use Case |
|--------|-------------|----------|
| 400 | `INVALID_*` | Validation errors, malformed requests |
| 401 | `UNAUTHORIZED` | Missing or invalid authentication |
| 403 | `FORBIDDEN` | Valid auth, insufficient permissions |
| 404 | `*_NOT_FOUND` | Resource doesn't exist |
| 409 | `CONFLICT` | State conflict (duplicate, concurrent edit) |
| 422 | `VALIDATION_FAILED` | Semantic validation errors |
| 429 | `RATE_LIMIT_EXCEEDED` | Too many requests |
| 500 | `INTERNAL_ERROR` | Unexpected server error |
| 503 | `SERVICE_UNAVAILABLE` | Temporary outage, maintenance |

### Implementation Example

```javascript
// error-handler.js
class APIError extends Error {
  constructor(code, message, statusCode = 500, details = {}) {
    super(message);
    this.code = code;
    this.statusCode = statusCode;
    this.details = details;
  }
}

function errorMiddleware(err, req, res, next) {
  // Handle known API errors
  if (err instanceof APIError) {
    return res.status(err.statusCode).json({
      error: {
        code: err.code,
        message: err.message,
        details: err.details,
        requestId: req.id,
        timestamp: new Date().toISOString(),
        path: req.path
      }
    });
  }

  // Handle unexpected errors (don't leak internals)
  console.error('Unexpected error:', err);
  return res.status(500).json({
    error: {
      code: 'INTERNAL_ERROR',
      message: 'An unexpected error occurred',
      requestId: req.id,
      timestamp: new Date().toISOString(),
      path: req.path
    }
  });
}

// Usage in endpoint
app.get('/api/users/:id', async (req, res, next) => {
  try {
    const user = await findUser(req.params.id);
    if (!user) {
      throw new APIError(
        'RESOURCE_NOT_FOUND',
        `User with ID ${req.params.id} not found`,
        404,
        { resource: 'User', id: req.params.id }
      );
    }
    res.json({ data: user });
  } catch (err) {
    next(err);
  }
});

module.exports = { APIError, errorMiddleware };
```

### Client-Side Usage

```javascript
// Frontend error handling
async function fetchUser(userId) {
  try {
    const response = await fetch(`/api/users/${userId}`);
    const data = await response.json();

    if (!response.ok) {
      // Standardized error format
      const { error } = data;

      // Handle specific error codes
      switch (error.code) {
        case 'RESOURCE_NOT_FOUND':
          showNotification('User not found', 'error');
          break;
        case 'UNAUTHORIZED':
          redirectToLogin();
          break;
        default:
          showNotification(error.message, 'error');
          logError(error.requestId, error.code);
      }

      throw new Error(error.message);
    }

    return data;
  } catch (err) {
    console.error('Failed to fetch user:', err);
    throw err;
  }
}
```

## Consequences

### Benefits
✅ **Consistency** - All errors follow same structure
✅ **Debuggability** - Request IDs enable log correlation
✅ **Client-Friendly** - Machine-readable codes + human messages
✅ **Monitoring** - Easy to aggregate by error code
✅ **Security** - Internal errors don't leak stack traces

### Drawbacks
❌ **Overhead** - More verbose than simple string errors
❌ **Migration** - Existing clients may expect old format
❌ **Maintenance** - Error codes require documentation

### Trade-offs
- Verbosity vs Clarity (chose clarity)
- Performance vs Debuggability (negligible perf impact)
- Flexibility vs Consistency (chose consistency)

## Examples from Codebase

- `src/api/auth/login.js` - Authentication errors
- `src/api/users/controller.js` - CRUD error handling
- `tests/api/error-handling.test.js` - Test suite

## Related Patterns

- [Logging Strategy](./logging-strategy.md) - How errors are logged
- [Request ID Propagation](./request-id-propagation.md) - Tracing requests
- [Validation Errors](./validation-errors.md) - Specific format for validation

## References

- [RFC 7807: Problem Details for HTTP APIs](https://datatracker.ietf.org/doc/html/rfc7807)
- [Google API Design Guide - Errors](https://cloud.google.com/apis/design/errors)
- [Microsoft REST API Guidelines - Error Responses](https://github.com/microsoft/api-guidelines/blob/vNext/Guidelines.md#7102-error-condition-responses)

---

**Created:** 2026-01-06
**Author:** @copilot (via knowledge base bootstrap)
**Status:** Active
**Review Date:** 2026-07-06
