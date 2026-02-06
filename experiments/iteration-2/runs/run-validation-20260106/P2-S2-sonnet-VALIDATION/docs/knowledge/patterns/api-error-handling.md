# API Error Handling Pattern

## Context

When building REST APIs, consistent error handling across all endpoints is critical for:
- Client-side error handling and retry logic
- Debugging and troubleshooting
- User experience and helpful error messages
- API documentation and contract clarity

## Problem

Without a standard error handling pattern, APIs suffer from:
- **Inconsistent formats:** Different endpoints return errors in different shapes
- **Missing context:** Error responses lack information needed for debugging
- **Poor UX:** Generic error messages that don't help users
- **Difficult monitoring:** Hard to track and categorize errors

## Solution

Implement a standardized error response format and centralized error handling middleware.

### Error Response Format

All error responses follow this structure:

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "User-friendly error message",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format"
      }
    ],
    "timestamp": "2026-01-06T19:30:00Z",
    "requestId": "req_abc123",
    "documentation": "https://api.example.com/docs/errors#VALIDATION_ERROR"
  }
}
```

### Implementation

#### 1. Define Error Classes

```javascript
// src/errors/ApiError.js

class ApiError extends Error {
  constructor(code, message, statusCode = 500, details = null) {
    super(message);
    this.code = code;
    this.statusCode = statusCode;
    this.details = details;
    this.timestamp = new Date().toISOString();
  }

  toJSON() {
    return {
      error: {
        code: this.code,
        message: this.message,
        details: this.details,
        timestamp: this.timestamp,
        requestId: this.requestId || null,
        documentation: `https://api.example.com/docs/errors#${this.code}`
      }
    };
  }
}

// Specific error types
class ValidationError extends ApiError {
  constructor(message, details = null) {
    super('VALIDATION_ERROR', message, 400, details);
  }
}

class NotFoundError extends ApiError {
  constructor(resource) {
    super('NOT_FOUND', `${resource} not found`, 404);
  }
}

class AuthenticationError extends ApiError {
  constructor(message = 'Authentication required') {
    super('AUTHENTICATION_ERROR', message, 401);
  }
}

class AuthorizationError extends ApiError {
  constructor(message = 'Insufficient permissions') {
    super('AUTHORIZATION_ERROR', message, 403);
  }
}

class RateLimitError extends ApiError {
  constructor(retryAfter = 60) {
    super('RATE_LIMIT_EXCEEDED', 'Too many requests', 429);
    this.retryAfter = retryAfter;
  }
}

module.exports = {
  ApiError,
  ValidationError,
  NotFoundError,
  AuthenticationError,
  AuthorizationError,
  RateLimitError
};
```

#### 2. Error Handling Middleware

```javascript
// src/middleware/errorHandler.js

const { ApiError } = require('../errors/ApiError');

function errorHandler(err, req, res, next) {
  // Attach request ID to error
  if (req.id) {
    err.requestId = req.id;
  }

  // Log error (use structured logging in production)
  console.error('API Error:', {
    code: err.code || 'INTERNAL_ERROR',
    message: err.message,
    statusCode: err.statusCode || 500,
    requestId: req.id,
    path: req.path,
    method: req.method,
    stack: err.stack
  });

  // Handle known API errors
  if (err instanceof ApiError) {
    return res.status(err.statusCode).json(err.toJSON());
  }

  // Handle validation errors from libraries (e.g., express-validator)
  if (err.name === 'ValidationError') {
    const details = Object.keys(err.errors).map(key => ({
      field: key,
      message: err.errors[key].message
    }));

    return res.status(400).json({
      error: {
        code: 'VALIDATION_ERROR',
        message: 'Validation failed',
        details,
        timestamp: new Date().toISOString(),
        requestId: req.id
      }
    });
  }

  // Handle unexpected errors (don't leak internal details)
  return res.status(500).json({
    error: {
      code: 'INTERNAL_ERROR',
      message: 'An unexpected error occurred',
      timestamp: new Date().toISOString(),
      requestId: req.id,
      documentation: 'https://api.example.com/docs/errors#INTERNAL_ERROR'
    }
  });
}

module.exports = errorHandler;
```

#### 3. Usage in Endpoints

```javascript
// src/api/users.js

const express = require('express');
const { ValidationError, NotFoundError } = require('../errors/ApiError');

const router = express.Router();

router.post('/users', async (req, res, next) => {
  try {
    const { email, name } = req.body;

    // Validation
    if (!email || !email.includes('@')) {
      throw new ValidationError('Invalid email address', [
        { field: 'email', message: 'Must be a valid email address' }
      ]);
    }

    if (!name || name.length < 2) {
      throw new ValidationError('Invalid name', [
        { field: 'name', message: 'Must be at least 2 characters' }
      ]);
    }

    // Business logic
    const user = await createUser({ email, name });

    res.status(201).json({ data: user });
  } catch (err) {
    next(err); // Pass to error handler middleware
  }
});

router.get('/users/:id', async (req, res, next) => {
  try {
    const user = await getUserById(req.params.id);

    if (!user) {
      throw new NotFoundError('User');
    }

    res.json({ data: user });
  } catch (err) {
    next(err);
  }
});

module.exports = router;
```

## Consequences

### Benefits

- **Consistency:** All errors follow the same structure
- **Debuggability:** Request IDs and timestamps enable log correlation
- **Better UX:** Field-level validation errors help users fix issues
- **Type safety:** Error classes make it explicit what can fail
- **Monitoring:** Standardized codes enable error categorization and alerting

### Trade-offs

- **Additional code:** More boilerplate than ad-hoc error handling
- **Documentation overhead:** Error codes need to be documented
- **Learning curve:** Team needs to understand error class hierarchy

## Related Patterns

- **Logging Standards:** How to log errors for monitoring
- **API Response Format:** Consistent success response structure
- **Request ID Middleware:** Generating and tracking request IDs

## References

- [RFC 7807 - Problem Details for HTTP APIs](https://datatracker.ietf.org/doc/html/rfc7807)
- [Google Cloud API Error Model](https://cloud.google.com/apis/design/errors)
- [Stripe API Errors](https://stripe.com/docs/api/errors)
