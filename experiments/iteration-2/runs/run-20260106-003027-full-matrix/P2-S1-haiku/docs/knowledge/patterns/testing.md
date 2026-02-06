# Testing Strategies and Best Practices

**Last Updated**: 2026-01-06
**Version**: 1.0
**Maintainer**: QA Team

## Overview

Standard testing approaches used across our projects to ensure reliability and quality.

## Testing Pyramid

```
       /\
      /  \    E2E Tests (10%)
     /────\   Few integration points
    /      \
   /        \  Integration Tests (30%)
  /──────────\ Test component interactions
 /            \
/──────────────\ Unit Tests (60%)
 Individual     Fast, focused tests
```

## Unit Testing Standards

### Coverage Requirements
- Minimum 80% code coverage
- 100% coverage for critical paths (auth, payment, data validation)
- Exclude generated code and boilerplate from coverage metrics

### Test Structure
```javascript
describe('UserService', () => {
  describe('createUser', () => {
    it('should create user with valid input', () => {
      // Arrange: Set up test data
      const input = { email: 'test@example.com', name: 'John' };

      // Act: Execute function
      const result = userService.createUser(input);

      // Assert: Verify results
      expect(result.id).toBeDefined();
      expect(result.email).toBe('test@example.com');
    });

    it('should reject invalid email', () => {
      const input = { email: 'invalid', name: 'John' };
      expect(() => userService.createUser(input)).toThrow();
    });
  });
});
```

### Mocking Strategy
- Mock external dependencies (databases, APIs, file system)
- Use real implementations for utility functions
- Create realistic mock data (avoid overly simplified stubs)

## Integration Testing

### Scope
- Test interaction between multiple components
- Use real database (in-memory or test instance)
- Mock external APIs (payment providers, email services)

### Example
```javascript
describe('UserRegistration Integration', () => {
  it('should send welcome email after registration', async () => {
    // Create user in database
    const user = await User.create({ email: 'new@example.com' });

    // Trigger email service
    await emailService.sendWelcome(user);

    // Verify email was queued
    expect(mockEmailQueue.length).toBe(1);
    expect(mockEmailQueue[0].to).toBe('new@example.com');
  });
});
```

## End-to-End Testing

### Tools
- Playwright (our standard)
- Cypress (UI testing alternative)

### Test Structure
```javascript
test('user can register and receive welcome email', async ({ page }) => {
  // Navigate to registration page
  await page.goto('/register');

  // Fill form
  await page.fill('input[name="email"]', 'new@example.com');
  await page.fill('input[name="password"]', 'SecurePassword123!');

  // Submit
  await page.click('button[type="submit"]');

  // Verify success
  await expect(page).toHaveURL('/welcome');
  await expect(page.locator('.success-message')).toBeVisible();
});
```

## Performance Testing

### Load Testing
- Use k6 for load and stress testing
- Test at 2x expected peak load
- Monitor response times, error rates, resource utilization

### Profiling
```bash
# Node.js profiling
node --prof app.js
node --prof-process isolate-*.log > profile.txt

# Python profiling
python -m cProfile -s cumulative app.py
```

## Test Data Management

### Fixtures
- Store realistic test data in fixtures/
- Update fixtures quarterly based on production data patterns
- Never use production data directly in tests

### Database Reset
- Reset database before each test suite
- Use transactions and rollback for isolation
- Implement database seeding for consistent state

## Continuous Integration

### Test Execution
```bash
# Run tests before commit
npm test

# Generate coverage report
npm run test:coverage

# Run specific test file
npm test -- --testNamePattern="UserService"

# Run with coverage threshold
npm test -- --coverage --coverageThreshold='{"global":{"branches":80}}'
```

### CI/CD Pipeline
- Run unit tests on every commit (< 5 minutes)
- Run integration tests on PR (< 15 minutes)
- Run E2E tests nightly (< 30 minutes)
- Generate and track coverage trends

## Test Maintenance

### Guidelines
- Update tests when requirements change
- Delete tests for removed features
- Refactor tests to reduce duplication
- Keep test documentation current

### Warning Signs
- Tests take > 5 minutes to run (too slow)
- Tests are flaky (> 99% pass rate not maintained)
- Tests have high cyclomatic complexity (hard to understand)
- Coverage is decreasing (regression in quality discipline)

## Common Testing Patterns

### Testing Async Code
```javascript
it('should fetch user data', async () => {
  const promise = userService.fetchUser(123);
  expect(promise).toBeInstanceOf(Promise);

  const user = await promise;
  expect(user.id).toBe(123);
});
```

### Testing Error Cases
```javascript
it('should handle network errors', async () => {
  mockAPI.throwError(new NetworkError('Connection timeout'));

  await expect(service.fetchData()).rejects.toThrow(NetworkError);
});
```

### Testing State Changes
```javascript
it('should update user status', () => {
  const user = new User({ status: 'active' });
  user.deactivate();

  expect(user.status).toBe('inactive');
});
```

## Quality Metrics to Track

| Metric | Target | Frequency |
|--------|--------|-----------|
| Test Coverage | 80%+ | Daily |
| Test Pass Rate | 99%+ | Every commit |
| Test Execution Time | < 5min | Every commit |
| Flaky Test Rate | 0% | Weekly |
| Code Review Comments | < 3 per PR | Per PR |

## See Also

- [API Design Patterns](api-design.md) - How to design testable APIs
- [Performance Insights](../insights/performance.md) - Performance testing approaches
