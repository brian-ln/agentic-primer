# Reusable Code Patterns

## Authentication & Authorization

### JWT Token Verification Pattern
```typescript
export async function verifyJWT(token: string): Promise<DecodedToken> {
  try {
    const decoded = jwt.verify(token, process.env.JWT_SECRET!);
    return decoded as DecodedToken;
  } catch (error) {
    if (error instanceof jwt.TokenExpiredError) {
      throw new UnauthorizedError('Token expired');
    }
    throw new UnauthorizedError('Invalid token');
  }
}
```

**When to use**: Every endpoint that requires authentication

**Assumptions**: `process.env.JWT_SECRET` is set and secure

### Middleware Factory Pattern
```typescript
export function createAuthMiddleware(roles: string[]): Middleware {
  return async (req, res, next) => {
    const token = req.headers.authorization?.split(' ')[1];
    if (!token) throw new UnauthorizedError('Missing token');

    const user = await verifyJWT(token);
    if (!roles.includes(user.role)) {
      throw new ForbiddenError('Insufficient permissions');
    }

    req.user = user;
    next();
  };
}
```

## Error Handling

### Custom Error Class Pattern
```typescript
export class AppError extends Error {
  constructor(
    message: string,
    public statusCode: number,
    public code: string
  ) {
    super(message);
    Object.setPrototypeOf(this, AppError.prototype);
  }
}

export class ValidationError extends AppError {
  constructor(message: string) {
    super(message, 400, 'VALIDATION_ERROR');
  }
}
```

### Error Handler Middleware Pattern
```typescript
export const errorHandler = (err: any, req: any, res: any, next: any) => {
  if (err instanceof AppError) {
    return res.status(err.statusCode).json({
      error: {
        message: err.message,
        code: err.code
      }
    });
  }

  console.error('Unexpected error:', err);
  return res.status(500).json({
    error: {
      message: 'Internal server error',
      code: 'INTERNAL_ERROR'
    }
  });
};
```

## Testing

### Unit Test Template
```typescript
describe('Module', () => {
  beforeEach(() => {
    // Setup
  });

  afterEach(() => {
    // Cleanup
  });

  it('should perform expected action', () => {
    // Arrange
    const input = setupTestData();

    // Act
    const result = functionUnderTest(input);

    // Assert
    expect(result).toEqual(expectedOutput);
  });
});
```

### Mock Service Pattern
```typescript
export function createMockService(overrides?: Partial<Service>): Service {
  return {
    getData: jest.fn().mockResolvedValue({}),
    saveData: jest.fn().mockResolvedValue(true),
    ...overrides
  };
}
```

## Database

### Connection Pool Pattern
```typescript
export const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
  max: 20,
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
});

export async function query(sql: string, values?: any[]) {
  const result = await pool.query(sql, values);
  return result.rows;
}
```

### Repository Pattern
```typescript
export class UserRepository {
  async findById(id: string): Promise<User | null> {
    const users = await query(
      'SELECT * FROM users WHERE id = $1',
      [id]
    );
    return users[0] || null;
  }

  async save(user: User): Promise<void> {
    await query(
      'INSERT INTO users (id, name, email) VALUES ($1, $2, $3)',
      [user.id, user.name, user.email]
    );
  }
}
```

## API Design

### RESTful Endpoint Pattern
```typescript
router.get('/api/v1/users/:id',
  authenticate,
  async (req, res) => {
    const user = await userService.findById(req.params.id);
    res.json(user);
  }
);

router.post('/api/v1/users',
  authenticate,
  validateBody(createUserSchema),
  async (req, res) => {
    const user = await userService.create(req.body);
    res.status(201).json(user);
  }
);
```

### Request Validation Pattern
```typescript
const createUserSchema = z.object({
  name: z.string().min(1).max(100),
  email: z.string().email(),
  role: z.enum(['admin', 'user']).default('user')
});

export const validateBody = (schema: any) => (req: any, res: any, next: any) => {
  try {
    req.body = schema.parse(req.body);
    next();
  } catch (error) {
    throw new ValidationError(error.message);
  }
};
```

## Rate Limiting

### Redis-Based Rate Limiter Pattern
```typescript
export async function rateLimit(key: string, limit: number, window: number): Promise<boolean> {
  const current = await redis.incr(key);
  if (current === 1) {
    await redis.expire(key, window);
  }
  return current <= limit;
}

export const rateLimitMiddleware = (limit: number = 100, window: number = 60) => {
  return async (req: any, res: any, next: any) => {
    const key = `rate-limit:${req.ip}`;
    const allowed = await rateLimit(key, limit, window);

    if (!allowed) {
      throw new Error('Rate limit exceeded');
    }
    next();
  };
};
```

## Caching

### LRU Cache Pattern
```typescript
class LRUCache<K, V> {
  private cache: Map<K, V> = new Map();
  private maxSize: number;

  constructor(maxSize: number = 100) {
    this.maxSize = maxSize;
  }

  get(key: K): V | undefined {
    if (!this.cache.has(key)) return undefined;

    const value = this.cache.get(key)!;
    this.cache.delete(key);
    this.cache.set(key, value);
    return value;
  }

  set(key: K, value: V): void {
    if (this.cache.has(key)) {
      this.cache.delete(key);
    } else if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
    this.cache.set(key, value);
  }
}
```

## Logging

### Structured Logging Pattern
```typescript
interface LogEntry {
  timestamp: string;
  level: 'info' | 'warn' | 'error';
  message: string;
  context?: Record<string, any>;
  error?: Error;
}

export function log(entry: LogEntry): void {
  const output = {
    ...entry,
    timestamp: new Date().toISOString(),
    stack: entry.error?.stack
  };
  console.log(JSON.stringify(output));
}

export const logRequest = (req: any, res: any, next: any) => {
  const start = Date.now();
  res.on('finish', () => {
    const duration = Date.now() - start;
    log({
      level: 'info',
      message: 'HTTP request',
      context: {
        method: req.method,
        path: req.path,
        status: res.statusCode,
        duration
      }
    });
  });
  next();
};
```
