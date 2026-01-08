# Code Style Standards

This document defines code formatting and style conventions for all project code.

## General Principles

- **Consistency**: Follow existing patterns in the codebase
- **Readability**: Code should be self-documenting
- **Simplicity**: Avoid unnecessary complexity
- **Maintainability**: Write code that others can easily understand and modify

## JavaScript/TypeScript Standards

### File Organization

```javascript
// 1. Imports (grouped by type)
import fs from 'fs';
import path from 'path';

import express from 'express';
import { Octokit } from '@octokit/rest';

import { UserService } from './services/user.service.js';
import { authMiddleware } from './middleware/auth.js';

// 2. Constants
const MAX_RETRIES = 3;
const DEFAULT_TIMEOUT = 5000;

// 3. Class/Function definitions
class FeatureService {
  // Implementation
}

// 4. Exports
export { FeatureService };
```

### Naming Conventions

- **Variables**: camelCase (`userName`, `apiKey`)
- **Constants**: UPPER_SNAKE_CASE (`MAX_RETRIES`, `API_BASE_URL`)
- **Functions**: camelCase, verb-first (`getUserById`, `createOrder`)
- **Classes**: PascalCase (`UserService`, `OrderController`)
- **Files**: kebab-case (`user-service.js`, `auth-middleware.js`)
- **Private methods**: prefix with underscore (`_validateInput`)

### Function Guidelines

```javascript
/**
 * Fetch user by ID with optional relations
 *
 * @param {number} userId - User ID to fetch
 * @param {Object} options - Fetch options
 * @param {boolean} options.includeOrders - Include user orders
 * @returns {Promise<User>} User object
 * @throws {NotFoundError} If user doesn't exist
 */
async function getUserById(userId, options = {}) {
  // Validate input
  if (!userId || userId < 1) {
    throw new ValidationError('Invalid user ID');
  }

  // Main logic
  const user = await db.users.findById(userId);

  if (!user) {
    throw new NotFoundError(`User ${userId} not found`);
  }

  // Optional relations
  if (options.includeOrders) {
    user.orders = await db.orders.findByUserId(userId);
  }

  return user;
}
```

**Rules**:
- Max 50 lines per function
- Single responsibility principle
- JSDoc comments for public functions
- Early returns for error cases
- Descriptive parameter names

### Error Handling

```javascript
// GOOD: Specific error types
class ValidationError extends Error {
  constructor(message) {
    super(message);
    this.name = 'ValidationError';
    this.statusCode = 400;
  }
}

// GOOD: Try-catch with context
try {
  await processPayment(order);
} catch (error) {
  logger.error('Payment processing failed', {
    orderId: order.id,
    error: error.message
  });
  throw new PaymentError('Failed to process payment', { cause: error });
}

// BAD: Generic errors
throw new Error('Something went wrong');

// BAD: Silent failures
try {
  await dangerousOperation();
} catch (error) {
  // Nothing
}
```

### Async/Await

```javascript
// GOOD: Clear async flow
async function createUser(userData) {
  const validatedData = await validateUserData(userData);
  const hashedPassword = await hashPassword(validatedData.password);
  const user = await db.users.create({
    ...validatedData,
    password: hashedPassword
  });
  return user;
}

// BAD: Promise hell
function createUser(userData) {
  return validateUserData(userData)
    .then(validatedData => hashPassword(validatedData.password)
      .then(hashedPassword => db.users.create({
        ...validatedData,
        password: hashedPassword
      })));
}
```

## Code Formatting

### Indentation and Spacing

- **Indentation**: 2 spaces (no tabs)
- **Line length**: Max 100 characters
- **Blank lines**: One blank line between functions, two between sections

### Braces and Brackets

```javascript
// GOOD: K&R style
if (condition) {
  doSomething();
} else {
  doSomethingElse();
}

// GOOD: Single line for simple cases
if (condition) return earlyExit();

// BAD: Inconsistent bracing
if (condition)
{
  doSomething();
}
```

### Object and Array Literals

```javascript
// GOOD: Readable formatting
const user = {
  id: 1,
  name: 'John Doe',
  email: 'john@example.com',
  roles: ['admin', 'user']
};

// GOOD: Destructuring
const { id, name, email } = user;
const [first, second, ...rest] = items;

// GOOD: Spread operator
const updated = { ...user, email: 'new@example.com' };
```

## Testing Standards

### Test Organization

```javascript
describe('UserService', () => {
  describe('getUserById', () => {
    it('should return user when ID is valid', async () => {
      const user = await userService.getUserById(1);
      expect(user).toBeDefined();
      expect(user.id).toBe(1);
    });

    it('should throw NotFoundError when user does not exist', async () => {
      await expect(userService.getUserById(999))
        .rejects
        .toThrow(NotFoundError);
    });

    it('should include orders when requested', async () => {
      const user = await userService.getUserById(1, { includeOrders: true });
      expect(user.orders).toBeDefined();
      expect(Array.isArray(user.orders)).toBe(true);
    });
  });
});
```

### Test Naming

- Describe what the test does
- Use "should" statements
- Be specific about expected behavior

## Comments and Documentation

### When to Comment

```javascript
// GOOD: Explain WHY, not WHAT
// Use exponential backoff to avoid overwhelming the API
// when it's experiencing high load
await retryWithBackoff(apiCall, { maxRetries: 5 });

// GOOD: Document non-obvious behavior
// Note: This returns cached results for 5 minutes to reduce
// database load. Clear cache with clearUserCache() if needed.
function getUsers() {
  return cache.get('users') || fetchUsersFromDB();
}

// BAD: Obvious comments
// Increment counter by 1
counter++;
```

### JSDoc Standards

Required for:
- All exported functions
- All class methods
- Complex utility functions

```javascript
/**
 * Process payment for an order
 *
 * @param {Order} order - Order to process payment for
 * @param {PaymentMethod} method - Payment method to use
 * @param {Object} options - Processing options
 * @param {boolean} options.sendReceipt - Send email receipt
 * @returns {Promise<Payment>} Completed payment record
 * @throws {PaymentError} If payment processing fails
 * @throws {ValidationError} If order or method is invalid
 */
async function processPayment(order, method, options = {}) {
  // Implementation
}
```

## Performance Guidelines

### Avoid Premature Optimization

```javascript
// GOOD: Clear and maintainable
const activeUsers = users.filter(u => u.isActive);

// ONLY optimize if profiling shows it's a bottleneck
// Document why optimization was needed
// Benchmark: Reduced execution time from 250ms to 15ms for 10k users
const activeUsers = [];
for (let i = 0; i < users.length; i++) {
  if (users[i].isActive) activeUsers.push(users[i]);
}
```

### Database Queries

```javascript
// GOOD: Use select to limit fields
const users = await db.users.find({}, { select: 'id name email' });

// GOOD: Use indexes for frequent queries
// Add index: CREATE INDEX idx_users_email ON users(email);
const user = await db.users.findOne({ email });

// BAD: Select all fields when only need a few
const users = await db.users.find({});
const names = users.map(u => u.name);
```

## Tools and Automation

### Required Tools

- **ESLint**: Lint JavaScript/TypeScript
- **Prettier**: Format code
- **Husky**: Git hooks for pre-commit linting

### Configuration

```json
// .eslintrc.json
{
  "extends": ["eslint:recommended"],
  "env": {
    "node": true,
    "es2022": true
  },
  "rules": {
    "no-console": "warn",
    "no-unused-vars": ["error", { "argsIgnorePattern": "^_" }],
    "prefer-const": "error"
  }
}
```

```json
// .prettierrc
{
  "semi": true,
  "singleQuote": true,
  "tabWidth": 2,
  "trailingComma": "es5",
  "printWidth": 100
}
```

## Enforcement

- Pre-commit hooks run linting and formatting
- CI pipeline fails on lint errors
- Code review focuses on logic, not style (tools handle style)
- Regular team discussions to update standards

---

**Remember**: These are guidelines, not laws. Use judgment and discuss with team when unsure.
