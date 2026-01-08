# Testing Insights

Lessons learned from testing practices in this project.

## Test Isolation is Critical

### The Problem
Early integration tests were flaky because they shared test data. One test would delete a user, causing subsequent tests to fail intermittently depending on execution order.

### The Solution
Each integration test now:
- Sets up its own test data in a transaction
- Rolls back after the test completes
- Uses unique identifiers to prevent conflicts

### Code Example
```typescript
describe('User Service (isolated)', () => {
  beforeEach(async () => {
    await db.transaction(async (trx) => {
      testData = {
        userId: await users.insert({ email: 'test@example.com' }).transacting(trx),
      };
      // Transaction will rollback after test
    });
  });

  it('should fetch user', async () => {
    const user = await users.findById(testData.userId);
    expect(user).toBeDefined();
  });

  // User automatically deleted after test
});
```

## Mock External Services Aggressively

### The Problem
Tests would fail when external APIs (Stripe, SendGrid) were unreachable or rate-limited.

### The Solution
Mock all external services by default. Only test actual integration in dedicated integration test suite.

### Lesson
- **Unit tests**: Always mock (100% isolation)
- **Integration tests**: Mock external APIs unless specifically testing integration
- **E2E tests**: Use real services (or sandbox environments)

## Test Data Factories Beat Fixtures

### The Problem
Static JSON fixtures became stale. When we added a required field, fixtures didn't update, causing test failures.

### The Solution
Use factory functions to generate test data with sensible defaults.

```typescript
// Good: Factory
function createUser(overrides = {}) {
  return {
    id: randomId(),
    email: 'test@example.com',
    createdAt: new Date(),
    ...overrides,
  };
}

// Bad: Static fixture
const userFixture = {
  id: 'user-1',
  email: 'test@example.com',
  // Missing newly-required fields
};
```

## Error Cases Need Tests

### The Problem
Most tests only covered the happy path. Real issues happened in error scenarios.

### The Solution
For each feature, write at least one test per error case:
- Input validation errors
- Database errors
- Network timeouts
- Permission errors

```typescript
describe('createUser errors', () => {
  it('should reject invalid email', async () => {
    const result = await service.createUser({ email: 'invalid' });
    expect(result.error).toBe('INVALID_EMAIL');
  });

  it('should handle database connection failure', async () => {
    db.simulateError('CONNECTION_REFUSED');
    const result = await service.createUser({ email: 'test@example.com' });
    expect(result.error).toBe('DATABASE_ERROR');
  });
});
```

## Async Code Needs Explicit Waits

### The Problem
Tests would pass even when async operations failed because the test completed before the promise rejected.

### The Solution
Always explicitly wait for async operations:
- Use `await` in test code
- Use `jest.runAllTimers()` for setTimeout/setInterval
- Use `jest.waitFor()` for eventually-true assertions

```typescript
// Bad: Race condition
it('should update cache', () => {
  service.updateCache(); // Fire and forget
  expect(cache.value).toBe(newValue); // Runs before cache updates!
});

// Good: Explicit wait
it('should update cache', async () => {
  await service.updateCache(); // Wait for completion
  expect(cache.value).toBe(newValue); // Now runs after update
});
```

## Snapshot Testing Hides Changes

### The Problem
Snapshot tests passed even when output changed, because developers just regenerated snapshots without review.

### The Solution
Use snapshots sparingly, only for complex structures where the exact format matters:
- Component rendering (React)
- Complex object transformations
- Error message formatting

For most cases, use explicit assertions instead:

```typescript
// Bad: Too loose
expect(result).toMatchSnapshot(); // Hides any change

// Good: Explicit
expect(result).toHaveProperty('userId');
expect(result.userId).toMatch(/^user-\d+$/);
expect(result.createdAt).toBeInstanceOf(Date);
```

## Test Naming Matters

### The Problem
Tests named `it('works')` or `it('should do the thing')` didn't communicate what was being tested. Debugging failed tests was slow.

### The Solution
Use descriptive names that explain:
1. What is being tested
2. What condition/input
3. What the expected outcome is

```typescript
// Bad
it('works');
it('should create user');

// Good
it('should create user with hashed password when given plain text password');
it('should reject user creation when email already exists in database');
it('should include timestamp in response when user is created via API');
```

## Timing Dependencies Cause Flakiness

### The Problem
Tests would sometimes pass and sometimes fail based on machine load or timing.

### The Solution
Never depend on real time in tests:
- Mock time with `jest.useFakeTimers()`
- Advance time explicitly with `jest.advanceTimersByTime()`
- Never use arbitrary `setTimeout` in tests (use `jest.advanceTimersByTime` instead)

```typescript
// Bad: Flaky
it('should retry after 5 seconds', (done) => {
  setTimeout(() => {
    expect(retried).toBe(true);
    done();
  }, 5000);
});

// Good: Deterministic
it('should retry after 5 seconds', () => {
  jest.useFakeTimers();
  service.start();
  jest.advanceTimersByTime(5000);
  expect(retried).toBe(true);
});
```

## Coverage Metrics Can Mislead

### The Problem
High code coverage (>90%) but issues still shipped to production. Coverage only measures lines executed, not logic correctness.

### The Solution
Use coverage as a minimum bar, not a goal:
- Target: >80% coverage
- Focus on: Branch coverage (if/else, loops)
- Review: Critical path tests are thorough
- Avoid: Reaching 100% with meaningless tests

Coverage report should show:
- Lines: % of code executed
- Branches: % of conditional paths tested
- Functions: % of functions called
- Statements: % of statements executed

---

## Checklist for New Tests

When writing a new test:

- [ ] Test has descriptive name explaining what/why/expected result
- [ ] Test is isolated (no shared state with other tests)
- [ ] Test data is generated, not static
- [ ] External services/APIs are mocked
- [ ] Error cases have explicit tests
- [ ] Async operations are awaited
- [ ] Time-dependent code uses fake timers
- [ ] Assertions are specific (not too loose snapshots)
- [ ] Test passes reliably (run 10x in a row)

---

## Further Reading

- [jest docs](https://jestjs.io/)
- [Testing Library principles](https://testing-library.com/docs/queries/about)
- [Common mistakes in testing](https://kentcdodds.com/blog/common-mistakes-with-react-testing-library)
