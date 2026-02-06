# Testing Patterns for @copilot

## Test File Organization

### JavaScript/Node.js with Jest

```javascript
// tests/module.test.js
describe('Module Name', () => {
  // Setup before each test
  beforeEach(() => {
    // Initialize test fixtures
  });

  // Cleanup after each test
  afterEach(() => {
    // Clean up resources
  });

  describe('Function Name', () => {
    it('should return expected value with valid input', () => {
      const input = { valid: true };
      const expected = { result: 'success' };
      const actual = functionUnderTest(input);
      expect(actual).toEqual(expected);
    });

    it('should throw on invalid input', () => {
      const input = null;
      expect(() => functionUnderTest(input)).toThrow(TypeError);
    });

    it('should handle edge cases', () => {
      expect(functionUnderTest('')).toBe('');
      expect(functionUnderTest(0)).toBe(0);
      expect(functionUnderTest(false)).toBe(false);
    });
  });
});
```

### Python with pytest

```python
# tests/test_module.py
import pytest
from module import function_under_test


class TestFunctionName:
    """Test suite for function_under_test."""

    @pytest.fixture(autouse=True)
    def setup(self):
        """Set up test fixtures."""
        self.valid_input = {'valid': True}
        yield
        # Cleanup

    def test_returns_expected_value_with_valid_input(self):
        """Should return expected value with valid input."""
        expected = {'result': 'success'}
        actual = function_under_test(self.valid_input)
        assert actual == expected

    def test_raises_on_invalid_input(self):
        """Should raise TypeError on invalid input."""
        with pytest.raises(TypeError):
            function_under_test(None)

    def test_handles_edge_cases(self):
        """Should handle edge cases."""
        assert function_under_test('') == ''
        assert function_under_test(0) == 0
        assert function_under_test(False) is False
```

### Bash with bats

```bash
# tests/module.bats
#!/usr/bin/env bats

setup() {
  # Set up test fixtures
  export TEST_DIR=$(mktemp -d)
}

teardown() {
  # Clean up
  rm -rf "$TEST_DIR"
}

@test "function returns expected value with valid input" {
  result=$(module_function "valid input")
  [ "$result" = "expected output" ]
}

@test "function errors on invalid input" {
  run module_function ""
  [ "$status" -eq 1 ]
  [[ "$output" =~ "error" ]]
}
```

## Test Types

### Unit Tests
Test individual functions or methods in isolation.

```javascript
describe('calculateSum', () => {
  it('should add two positive numbers', () => {
    expect(calculateSum(2, 3)).toBe(5);
  });

  it('should handle negative numbers', () => {
    expect(calculateSum(-1, 1)).toBe(0);
  });

  it('should handle zero', () => {
    expect(calculateSum(0, 0)).toBe(0);
  });
});
```

### Integration Tests
Test how multiple components work together.

```javascript
describe('User Service Integration', () => {
  let database;
  let userService;

  beforeEach(async () => {
    database = new TestDatabase();
    await database.connect();
    userService = new UserService(database);
  });

  it('should create and retrieve user', async () => {
    const user = await userService.create({ name: 'John' });
    const retrieved = await userService.getById(user.id);
    expect(retrieved.name).toBe('John');
  });
});
```

### End-to-End Tests
Test complete workflows from start to finish.

```javascript
describe('Issue Processing Workflow', () => {
  it('should process issue from creation to PR', async () => {
    // Create issue
    const issue = await github.issues.create({
      title: 'Test Issue',
      body: 'Test Body'
    });

    // Wait for processing
    await wait(5000);

    // Verify PR created
    const prs = await github.pulls.list();
    expect(prs).toHaveLength(1);
    expect(prs[0].title).toContain('Test Issue');
  });
});
```

## Test Coverage

### Coverage Targets
- **Lines**: >=80% covered
- **Branches**: >=75% covered
- **Functions**: >=80% covered
- **Statements**: >=80% covered

### Checking Coverage

```bash
# JavaScript/Jest
npm test -- --coverage

# Python/pytest
pytest --cov=src --cov-report=html

# Display coverage report
open htmlcov/index.html  # macOS
xdg-open htmlcov/index.html  # Linux
```

### Improving Coverage

1. **Identify gaps**: Review coverage report
2. **Add tests**: Write tests for uncovered code
3. **Test branches**: Test both if/else paths
4. **Test errors**: Test error conditions
5. **Test edge cases**: Test boundary conditions

## Mocking and Stubbing

### JavaScript with Jest

```javascript
describe('API Integration', () => {
  it('should fetch data from API', async () => {
    // Mock the fetch function
    global.fetch = jest.fn(() =>
      Promise.resolve({
        json: () => Promise.resolve({ id: 1, name: 'Test' })
      })
    );

    const result = await fetchData();
    expect(result.name).toBe('Test');
    expect(fetch).toHaveBeenCalledWith('https://api.example.com');
  });
});
```

### Python with pytest-mock

```python
def test_api_integration(mocker):
    """Should fetch data from API."""
    mock_response = {'id': 1, 'name': 'Test'}
    mocker.patch('requests.get', return_value=mock_response)

    result = fetch_data()
    assert result['name'] == 'Test'
```

## Test Organization

### Test Structure (AAA Pattern)

```javascript
describe('Payment Processing', () => {
  it('should process valid credit card', () => {
    // ARRANGE - Set up test data
    const payment = {
      cardNumber: '4111111111111111',
      amount: 100,
      cvv: '123'
    };

    // ACT - Execute the function
    const result = processPayment(payment);

    // ASSERT - Verify the result
    expect(result.status).toBe('success');
    expect(result.transactionId).toBeDefined();
  });
});
```

## Parametrized Tests

### JavaScript with Jest

```javascript
describe('Validation', () => {
  const testCases = [
    { input: 'valid@email.com', expected: true },
    { input: 'invalid-email', expected: false },
    { input: '', expected: false },
    { input: null, expected: false }
  ];

  testCases.forEach(({ input, expected }) => {
    it(`should validate email: ${input}`, () => {
      expect(validateEmail(input)).toBe(expected);
    });
  });
});
```

### Python with pytest

```python
import pytest

@pytest.mark.parametrize('input,expected', [
    ('valid@email.com', True),
    ('invalid-email', False),
    ('', False),
    (None, False)
])
def test_validate_email(input, expected):
    """Test email validation with multiple inputs."""
    assert validate_email(input) == expected
```

## Async Testing

### JavaScript

```javascript
describe('Async Operations', () => {
  it('should resolve promise', async () => {
    const result = await asyncFunction();
    expect(result).toBe('success');
  });

  it('should handle promise rejection', async () => {
    await expect(failingAsyncFunction()).rejects.toThrow('error');
  });

  it('should timeout on slow operation', async () => {
    jest.setTimeout(5000);
    const result = await slowFunction();
    expect(result).toBeDefined();
  });
});
```

### Python

```python
@pytest.mark.asyncio
async def test_async_operation():
    """Should execute async operation."""
    result = await async_function()
    assert result == 'success'

@pytest.mark.asyncio
async def test_async_exception():
    """Should handle async exceptions."""
    with pytest.raises(Exception):
        await failing_async_function()
```

## Snapshot Testing

### JavaScript with Jest

```javascript
describe('UI Rendering', () => {
  it('should render user card', () => {
    const user = { id: 1, name: 'John', email: 'john@example.com' };
    const component = renderUserCard(user);

    // First run: creates snapshot
    // Subsequent runs: compares to snapshot
    expect(component).toMatchSnapshot();
  });
});

// To update snapshots after intentional changes:
// npm test -- -u
```

## Performance Testing

### Measuring Execution Time

```javascript
describe('Performance', () => {
  it('should process large array efficiently', () => {
    const largeArray = Array.from({ length: 10000 }, (_, i) => i);

    const startTime = performance.now();
    const result = processArray(largeArray);
    const endTime = performance.now();

    expect(result).toHaveLength(10000);
    expect(endTime - startTime).toBeLessThan(100); // Less than 100ms
  });
});
```

## Common Testing Mistakes to Avoid

### Don't
```javascript
// Too broad assertions
expect(result).toBeTruthy(); // Vague

// Multiple assertions in one test
it('should do everything', () => {
  // Test multiple unrelated things

// Not cleaning up resources
beforeEach(() => {
  database = new Database();
  // No afterEach to close it
});

// Flaky timeouts
it('should load data', async () => {
  setTimeout(() => {
    // Timing-dependent test
  }, 100);
});
```

### Do
```javascript
// Specific assertions
expect(result.id).toBe(123);
expect(result.status).toBe('success');

// One logical assertion per test
it('should return user with correct ID', () => {
  // One clear purpose

// Proper cleanup
afterEach(() => {
  database.close();
});

// Wait for conditions
await waitFor(() => {
  expect(element).toBeVisible();
});
```

## Test Documentation

### Comment Complex Tests

```javascript
/**
 * Tests that the issue processor correctly handles
 * concurrent issue updates without data loss.
 *
 * Scenario:
 * 1. Create issue with initial state
 * 2. Update from two processes simultaneously
 * 3. Verify final state has both updates
 */
it('should handle concurrent updates', async () => {
  // Test implementation
});
```

## Continuous Testing

### Pre-Commit Testing
```bash
#!/bin/bash
# .git/hooks/pre-commit
npm test || exit 1
```

### Watch Mode
```bash
# JavaScript
npm test -- --watch

# Python
pytest-watch
```

### CI/CD Integration
Tests should run automatically on every push and PR.
