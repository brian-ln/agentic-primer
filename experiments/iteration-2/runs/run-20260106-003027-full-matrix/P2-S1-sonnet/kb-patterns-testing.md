# Testing Strategy and Patterns

## Test Pyramid

```
       /\
      /e2e\      <- Few end-to-end tests (expensive, slow)
     /------\
    /integra\   <- More integration tests (moderate cost)
   /----------\
  /   unit     \ <- Many unit tests (cheap, fast)
 /--------------\
```

Target ratio: 70% unit, 20% integration, 10% e2e

## Unit Testing

### Testing Service Layer

```javascript
describe('UserService', () => {
  let userService;
  let mockDb;

  beforeEach(() => {
    // Create mock database
    mockDb = {
      users: {
        findById: jest.fn(),
        create: jest.fn(),
        update: jest.fn()
      }
    };

    userService = new UserService(mockDb);
  });

  describe('getUserById', () => {
    it('should return user when found', async () => {
      const mockUser = { id: 1, name: 'John', email: 'john@example.com' };
      mockDb.users.findById.mockResolvedValue(mockUser);

      const result = await userService.getUserById(1);

      expect(result).toEqual(mockUser);
      expect(mockDb.users.findById).toHaveBeenCalledWith(1);
    });

    it('should throw NotFoundError when user does not exist', async () => {
      mockDb.users.findById.mockResolvedValue(null);

      await expect(userService.getUserById(999))
        .rejects
        .toThrow(NotFoundError);
    });

    it('should throw ValidationError for invalid ID', async () => {
      await expect(userService.getUserById(-1))
        .rejects
        .toThrow(ValidationError);
    });
  });

  describe('createUser', () => {
    it('should create user with valid data', async () => {
      const userData = {
        email: 'new@example.com',
        name: 'New User'
      };

      const mockCreated = { id: 2, ...userData };
      mockDb.users.create.mockResolvedValue(mockCreated);

      const result = await userService.createUser(userData);

      expect(result).toEqual(mockCreated);
      expect(mockDb.users.create).toHaveBeenCalledWith(userData);
    });

    it('should throw ValidationError for invalid email', async () => {
      const userData = { email: 'invalid', name: 'User' };

      await expect(userService.createUser(userData))
        .rejects
        .toThrow(ValidationError);

      expect(mockDb.users.create).not.toHaveBeenCalled();
    });
  });
});
```

### Testing Pure Functions

```javascript
describe('calculateDiscount', () => {
  it('should apply 10% discount for orders over $100', () => {
    expect(calculateDiscount(150)).toBe(15);
  });

  it('should apply no discount for orders under $100', () => {
    expect(calculateDiscount(50)).toBe(0);
  });

  it('should handle edge case of exactly $100', () => {
    expect(calculateDiscount(100)).toBe(0);
  });

  it('should throw for negative amounts', () => {
    expect(() => calculateDiscount(-10)).toThrow(ValidationError);
  });
});
```

## Integration Testing

### Testing API Endpoints

```javascript
const request = require('supertest');
const app = require('../app');
const db = require('../db');

describe('User API', () => {
  beforeAll(async () => {
    // Setup test database
    await db.migrate.latest();
  });

  afterAll(async () => {
    // Cleanup
    await db.destroy();
  });

  beforeEach(async () => {
    // Clear data before each test
    await db('users').del();
  });

  describe('GET /api/users/:id', () => {
    it('should return user when exists', async () => {
      // Create test user
      const [userId] = await db('users').insert({
        email: 'test@example.com',
        name: 'Test User'
      });

      // Make request
      const response = await request(app)
        .get(`/api/users/${userId}`)
        .expect(200);

      expect(response.body.data).toMatchObject({
        id: userId,
        email: 'test@example.com',
        name: 'Test User'
      });
    });

    it('should return 404 when user not found', async () => {
      const response = await request(app)
        .get('/api/users/999')
        .expect(404);

      expect(response.body.error.code).toBe('NOT_FOUND');
    });

    it('should require authentication', async () => {
      await request(app)
        .get('/api/users/1')
        .expect(401);
    });
  });

  describe('POST /api/users', () => {
    it('should create user with valid data', async () => {
      const userData = {
        email: 'new@example.com',
        name: 'New User'
      };

      const response = await request(app)
        .post('/api/users')
        .send(userData)
        .expect(201);

      expect(response.body.data).toMatchObject(userData);

      // Verify in database
      const user = await db('users').where({ email: userData.email }).first();
      expect(user).toBeDefined();
    });

    it('should return 422 for invalid email', async () => {
      const response = await request(app)
        .post('/api/users')
        .send({ email: 'invalid', name: 'User' })
        .expect(422);

      expect(response.body.error.code).toBe('VALIDATION_ERROR');
    });

    it('should return 409 for duplicate email', async () => {
      await db('users').insert({
        email: 'existing@example.com',
        name: 'Existing'
      });

      await request(app)
        .post('/api/users')
        .send({ email: 'existing@example.com', name: 'New' })
        .expect(409);
    });
  });
});
```

### Testing Database Integration

```javascript
describe('Database Operations', () => {
  beforeEach(async () => {
    await db.migrate.rollback();
    await db.migrate.latest();
    await db.seed.run();
  });

  it('should enforce unique email constraint', async () => {
    await expect(
      db('users').insert([
        { email: 'test@example.com', name: 'User 1' },
        { email: 'test@example.com', name: 'User 2' }
      ])
    ).rejects.toThrow();
  });

  it('should cascade delete related records', async () => {
    const [userId] = await db('users').insert({
      email: 'test@example.com',
      name: 'Test'
    });

    await db('orders').insert({
      userId,
      total: 100
    });

    await db('users').where({ id: userId }).del();

    const orders = await db('orders').where({ userId });
    expect(orders).toHaveLength(0);
  });
});
```

## End-to-End Testing

```javascript
describe('User Registration Flow', () => {
  it('should complete full registration process', async () => {
    // 1. Register new user
    const registerResponse = await request(app)
      .post('/api/auth/register')
      .send({
        email: 'newuser@example.com',
        password: 'SecurePass123!',
        name: 'New User'
      })
      .expect(201);

    expect(registerResponse.body.data.email).toBe('newuser@example.com');

    // 2. Verify email (simulate)
    const verifyToken = registerResponse.body.data.verifyToken;
    await request(app)
      .post('/api/auth/verify')
      .send({ token: verifyToken })
      .expect(200);

    // 3. Login
    const loginResponse = await request(app)
      .post('/api/auth/login')
      .send({
        email: 'newuser@example.com',
        password: 'SecurePass123!'
      })
      .expect(200);

    const { accessToken } = loginResponse.body.data;

    // 4. Access protected resource
    const profileResponse = await request(app)
      .get('/api/users/me')
      .set('Authorization', `Bearer ${accessToken}`)
      .expect(200);

    expect(profileResponse.body.data.email).toBe('newuser@example.com');
  });
});
```

## Test Organization

### File Structure

```
tests/
  unit/
    services/
      user.service.test.js
      order.service.test.js
    utils/
      validation.test.js
  integration/
    api/
      users.test.js
      orders.test.js
    database/
      migrations.test.js
  e2e/
    flows/
      registration.test.js
      checkout.test.js
  fixtures/
    users.js
    orders.js
  helpers/
    test-db.js
    auth-helper.js
```

## Mocking Strategies

### Mock External APIs

```javascript
const nock = require('nock');

describe('External API Integration', () => {
  afterEach(() => {
    nock.cleanAll();
  });

  it('should fetch user data from external API', async () => {
    nock('https://api.external.com')
      .get('/users/123')
      .reply(200, {
        id: 123,
        name: 'External User'
      });

    const result = await externalService.fetchUser(123);

    expect(result.name).toBe('External User');
  });

  it('should handle API errors gracefully', async () => {
    nock('https://api.external.com')
      .get('/users/123')
      .reply(500);

    await expect(externalService.fetchUser(123))
      .rejects
      .toThrow('External API error');
  });
});
```

### Mock Time

```javascript
describe('Time-sensitive operations', () => {
  beforeEach(() => {
    jest.useFakeTimers();
    jest.setSystemTime(new Date('2026-01-08T10:00:00Z'));
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  it('should expire tokens after 1 hour', () => {
    const token = createToken({ userId: 1 }, { expiresIn: '1h' });

    expect(isTokenValid(token)).toBe(true);

    // Advance time 61 minutes
    jest.advanceTimersByTime(61 * 60 * 1000);

    expect(isTokenValid(token)).toBe(false);
  });
});
```

## Test Data Management

### Factories

```javascript
const { Factory } = require('fishery');

const userFactory = Factory.define(({ sequence }) => ({
  id: sequence,
  email: `user${sequence}@example.com`,
  name: `User ${sequence}`,
  role: 'viewer',
  createdAt: new Date()
}));

// Usage
const user = userFactory.build();  // Just object
const admin = userFactory.build({ role: 'admin' });
const users = userFactory.buildList(5);  // Array of 5
```

### Database Seeding

```javascript
exports.seed = async function(knex) {
  await knex('users').del();
  await knex('users').insert([
    { id: 1, email: 'admin@example.com', role: 'admin' },
    { id: 2, email: 'user@example.com', role: 'viewer' }
  ]);
};
```

## Coverage Requirements

- Minimum: 80% overall coverage
- Critical paths: 100% coverage
- New code: Must not decrease coverage

```json
{
  "jest": {
    "coverageThreshold": {
      "global": {
        "branches": 80,
        "functions": 80,
        "lines": 80,
        "statements": 80
      }
    }
  }
}
```

---

**Remember**: Tests are documentation. Write tests that explain how code should behave.
