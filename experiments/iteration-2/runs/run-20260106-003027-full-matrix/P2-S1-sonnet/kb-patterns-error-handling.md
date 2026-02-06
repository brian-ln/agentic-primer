# Error Handling Patterns

## Custom Error Classes

```javascript
// Base error class
class AppError extends Error {
  constructor(message, statusCode = 500, code = 'INTERNAL_ERROR') {
    super(message);
    this.name = this.constructor.name;
    this.statusCode = statusCode;
    this.code = code;
    this.isOperational = true;  // vs programming errors
    Error.captureStackTrace(this, this.constructor);
  }
}

// Specific error types
class ValidationError extends AppError {
  constructor(message, details = []) {
    super(message, 422, 'VALIDATION_ERROR');
    this.details = details;
  }
}

class NotFoundError extends AppError {
  constructor(resource, identifier) {
    super(`${resource} not found: ${identifier}`, 404, 'NOT_FOUND');
    this.resource = resource;
    this.identifier = identifier;
  }
}

class UnauthorizedError extends AppError {
  constructor(message = 'Authentication required') {
    super(message, 401, 'UNAUTHORIZED');
  }
}

class ForbiddenError extends AppError {
  constructor(message = 'Access denied') {
    super(message, 403, 'FORBIDDEN');
  }
}

class ConflictError extends AppError {
  constructor(message) {
    super(message, 409, 'CONFLICT');
  }
}
```

## Global Error Handler

```javascript
// Express error handling middleware
function errorHandler(err, req, res, next) {
  // Log error
  logger.error('Error occurred', {
    error: err.message,
    stack: err.stack,
    code: err.code,
    requestId: req.id,
    path: req.path,
    method: req.method
  });

  // Operational errors (expected)
  if (err.isOperational) {
    return res.status(err.statusCode).json({
      error: {
        code: err.code,
        message: err.message,
        details: err.details || undefined,
        requestId: req.id,
        timestamp: new Date().toISOString()
      }
    });
  }

  // Programming errors (unexpected)
  // Don't leak error details to client
  return res.status(500).json({
    error: {
      code: 'INTERNAL_ERROR',
      message: 'An unexpected error occurred',
      requestId: req.id,
      timestamp: new Date().toISOString()
    }
  });
}

app.use(errorHandler);
```

## Try-Catch Patterns

### Async/Await

```javascript
// Route handler with error handling
async function getUser(req, res, next) {
  try {
    const { id } = req.params;

    const user = await userService.getUserById(id);

    if (!user) {
      throw new NotFoundError('User', id);
    }

    res.json({ data: user });

  } catch (error) {
    next(error);  // Pass to error handler
  }
}

// Wrapper to avoid repetitive try-catch
function asyncHandler(fn) {
  return (req, res, next) => {
    Promise.resolve(fn(req, res, next)).catch(next);
  };
}

// Usage
router.get('/users/:id', asyncHandler(async (req, res) => {
  const user = await userService.getUserById(req.params.id);

  if (!user) {
    throw new NotFoundError('User', req.params.id);
  }

  res.json({ data: user });
}));
```

### Service Layer

```javascript
class UserService {
  async getUserById(id) {
    // Validate input
    if (!id || id < 1) {
      throw new ValidationError('Invalid user ID', [
        { field: 'id', message: 'Must be a positive integer' }
      ]);
    }

    // Database query with error handling
    try {
      const user = await db.users.findById(id);
      return user;
    } catch (error) {
      // Re-throw with context
      throw new AppError(
        `Failed to fetch user: ${error.message}`,
        500,
        'DATABASE_ERROR'
      );
    }
  }

  async createUser(data) {
    // Validate
    const validation = validateUserData(data);
    if (!validation.valid) {
      throw new ValidationError('Invalid user data', validation.errors);
    }

    // Check for conflicts
    const existing = await db.users.findByEmail(data.email);
    if (existing) {
      throw new ConflictError('User with this email already exists');
    }

    // Create user
    try {
      const user = await db.users.create(data);
      return user;
    } catch (error) {
      if (error.code === '23505') {  // Postgres unique violation
        throw new ConflictError('Duplicate email address');
      }
      throw error;
    }
  }
}
```

## Validation Errors

```javascript
function validateUserData(data) {
  const errors = [];

  if (!data.email || !isValidEmail(data.email)) {
    errors.push({
      field: 'email',
      message: 'Valid email address is required'
    });
  }

  if (!data.name || data.name.length < 2) {
    errors.push({
      field: 'name',
      message: 'Name must be at least 2 characters'
    });
  }

  if (errors.length > 0) {
    throw new ValidationError('Validation failed', errors);
  }

  return data;
}
```

## Database Error Handling

```javascript
async function executeQuery(query, params) {
  try {
    return await db.query(query, params);
  } catch (error) {
    // Map database errors to application errors
    switch (error.code) {
      case '23505':  // Unique violation
        throw new ConflictError('Duplicate entry');

      case '23503':  // Foreign key violation
        throw new ValidationError('Referenced record does not exist');

      case '42P01':  // Undefined table
        logger.error('Database schema error', { error });
        throw new AppError('Database configuration error');

      default:
        logger.error('Database error', { error, query });
        throw new AppError('Database operation failed');
    }
  }
}
```

## External API Error Handling

```javascript
async function fetchExternalData(url) {
  try {
    const response = await fetch(url, {
      timeout: 5000  // 5 second timeout
    });

    if (!response.ok) {
      throw new AppError(
        `External API error: ${response.status}`,
        502,  // Bad Gateway
        'EXTERNAL_API_ERROR'
      );
    }

    return await response.json();

  } catch (error) {
    if (error.name === 'AbortError') {
      throw new AppError('External API timeout', 504, 'GATEWAY_TIMEOUT');
    }

    throw new AppError(
      `Failed to fetch external data: ${error.message}`,
      502,
      'EXTERNAL_API_ERROR'
    );
  }
}
```

## Retry Logic

```javascript
async function retryWithBackoff(fn, maxRetries = 3, baseDelay = 1000) {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      if (attempt === maxRetries) {
        throw error;  // Final attempt failed
      }

      if (!isRetryable(error)) {
        throw error;  // Don't retry client errors
      }

      // Exponential backoff: 1s, 2s, 4s
      const delay = baseDelay * Math.pow(2, attempt - 1);
      logger.warn(`Attempt ${attempt} failed, retrying in ${delay}ms`, {
        error: error.message
      });

      await sleep(delay);
    }
  }
}

function isRetryable(error) {
  // Retry server errors and timeouts, not client errors
  return error.statusCode >= 500 || error.code === 'ECONNRESET';
}

// Usage
const data = await retryWithBackoff(
  () => fetchExternalData('https://api.example.com/data')
);
```

## Graceful Degradation

```javascript
async function getUserWithCache(id) {
  try {
    // Try cache first
    const cached = await cache.get(`user:${id}`);
    if (cached) return cached;

    // Fetch from database
    const user = await db.users.findById(id);

    // Update cache (fire and forget, don't fail if cache fails)
    cache.set(`user:${id}`, user).catch(err => {
      logger.warn('Failed to update cache', { error: err.message });
    });

    return user;

  } catch (error) {
    logger.error('Failed to fetch user', { id, error: error.message });

    // Return default/empty object instead of failing
    return { id, name: 'Unknown User', email: null };
  }
}
```

## Process-Level Error Handling

```javascript
// Uncaught exceptions
process.on('uncaughtException', (error) => {
  logger.fatal('Uncaught exception', { error });
  // Graceful shutdown
  process.exit(1);
});

// Unhandled promise rejections
process.on('unhandledRejection', (reason, promise) => {
  logger.fatal('Unhandled rejection', { reason, promise });
  // Graceful shutdown
  process.exit(1);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM received, shutting down gracefully');

  // Close server
  server.close(() => {
    logger.info('HTTP server closed');
  });

  // Close database connections
  await db.close();

  process.exit(0);
});
```

---

**Key Principles**:
1. Fail fast with clear error messages
2. Distinguish operational errors from programming errors
3. Never expose internal details to clients
4. Log everything with context
5. Use retries for transient failures
6. Provide fallbacks when possible
