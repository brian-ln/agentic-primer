# Error Handling Patterns for @copilot

## Standard Error Handling Pattern

### JavaScript/Node.js

```javascript
// Basic try-catch pattern
async function processData(input) {
  try {
    const result = await validateInput(input);
    return result;
  } catch (error) {
    console.error('Error processing data:', error.message);
    throw new Error(`Processing failed: ${error.message}`);
  }
}

// With cleanup
async function readFile(filePath) {
  let fileHandle;
  try {
    fileHandle = await fs.open(filePath, 'r');
    return await fileHandle.readFile('utf-8');
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`File not found: ${filePath}`);
    }
    throw error;
  } finally {
    if (fileHandle) await fileHandle.close();
  }
}

// With retry logic
async function fetchWithRetry(url, maxRetries = 3) {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }
      return response.json();
    } catch (error) {
      if (attempt === maxRetries) throw error;
      await new Promise(resolve => setTimeout(resolve, 1000 * attempt));
    }
  }
}
```

### Python

```python
# Basic try-except pattern
def process_data(input_data):
    try:
        result = validate_input(input_data)
        return result
    except ValueError as error:
        logger.error(f"Validation error: {error}")
        raise

# With context manager
def read_file(file_path):
    try:
        with open(file_path, 'r') as f:
            return f.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"File not found: {file_path}")
    except IOError as error:
        logger.error(f"IO error: {error}")
        raise

# With retry logic
import time
def fetch_with_retry(url, max_retries=3):
    for attempt in range(1, max_retries + 1):
        try:
            response = requests.get(url, timeout=10)
            response.raise_for_status()
            return response.json()
        except requests.RequestException as error:
            if attempt == max_retries:
                raise
            time.sleep(1 * attempt)
```

### Bash/Shell

```bash
#!/bin/bash

# Set error handling
set -euo pipefail

# Function with error handling
process_file() {
  local file="$1"

  if [ ! -f "$file" ]; then
    echo "ERROR: File not found: $file" >&2
    return 1
  fi

  # Use local to scope variables
  local result
  if ! result=$(cat "$file" 2>/dev/null); then
    echo "ERROR: Failed to read file: $file" >&2
    return 2
  fi

  echo "$result"
}

# Trap errors for cleanup
cleanup() {
  local exit_code=$?
  echo "Cleaning up..." >&2
  # Cleanup code here
  return $exit_code
}

trap cleanup EXIT

# Main logic
main() {
  local file="$1"

  if ! process_file "$file"; then
    echo "ERROR: Processing failed" >&2
    return 1
  fi

  echo "Processing succeeded"
}

main "$@"
```

## Error Classification

### User/Input Errors
- Invalid input format
- Missing required fields
- Type mismatch

**Response**: Validate early, provide clear error message

```javascript
function validateEmail(email) {
  if (!email || typeof email !== 'string') {
    throw new Error('Email must be a non-empty string');
  }
  if (!email.includes('@')) {
    throw new Error('Invalid email format');
  }
  return email.toLowerCase();
}
```

### System/Environment Errors
- File not found
- Permission denied
- Network timeout

**Response**: Retry with backoff, provide fallback if available

```javascript
async function connectWithRetry(config, maxRetries = 3) {
  for (let i = 0; i < maxRetries; i++) {
    try {
      return await db.connect(config);
    } catch (error) {
      if (i === maxRetries - 1) throw error;
      await sleep(1000 * Math.pow(2, i)); // Exponential backoff
    }
  }
}
```

### Logic Errors
- Unexpected state
- Invariant violation
- Algorithm failure

**Response**: Log with context, escalate to maintainer

```javascript
if (state === 'invalid') {
  console.error('Invariant violated:', {
    state,
    context: JSON.stringify(context),
    stack: new Error().stack
  });
  throw new Error('Internal error: Invalid state reached');
}
```

## Error Logging

### What to Log
- Error message and type
- Relevant context (IDs, values)
- Stack trace
- Timestamp
- Severity level

### How to Log

```javascript
const logger = {
  error: (message, context) => {
    console.error(JSON.stringify({
      level: 'ERROR',
      message,
      context,
      timestamp: new Date().toISOString(),
      stack: new Error().stack
    }));
  }
};

logger.error('Failed to process issue', {
  issue_id: 123,
  error_type: 'VALIDATION_ERROR'
});
```

### Severity Levels
- **ERROR**: Something failed, action needed
- **WARN**: Potential issue, operation continues
- **INFO**: Normal operational event
- **DEBUG**: Detailed diagnostic information

## Error Recovery

### Strategies

1. **Retry**: Retry failed operation (with backoff)
2. **Fallback**: Use default or alternate value
3. **Abort**: Stop processing, report error
4. **Partial**: Complete what succeeded, report failures

### Example: Resilient Processing

```javascript
async function processIssues(issues) {
  const results = {
    successful: [],
    failed: []
  };

  for (const issue of issues) {
    try {
      const result = await processIssue(issue);
      results.successful.push(result);
    } catch (error) {
      results.failed.push({
        issue_id: issue.id,
        error: error.message
      });
      logger.error(`Failed to process issue ${issue.id}`, error);
    }
  }

  return results;
}
```

## Testing Error Conditions

### Unit Tests

```javascript
describe('Error Handling', () => {
  it('should throw on invalid input', () => {
    expect(() => validate(null)).toThrow('Input required');
  });

  it('should handle missing files gracefully', async () => {
    const result = await readWithDefault('missing.txt', 'default');
    expect(result).toBe('default');
  });

  it('should retry on temporary failures', async () => {
    let attempts = 0;
    const fn = async () => {
      attempts++;
      if (attempts < 3) throw new Error('Retry');
      return 'success';
    };
    const result = await retryWithBackoff(fn);
    expect(result).toBe('success');
    expect(attempts).toBe(3);
  });
});
```

## Common Patterns to Avoid

### Don't
```javascript
// Empty catch
try {
  doSomething();
} catch (error) {
  // Silent failure - BAD
}

// Too broad catch
try {
  doSomething();
} catch (error) {
  // Can't distinguish error type
}

// Swallowing important info
try {
  doSomething();
} catch (error) {
  throw new Error('Error occurred'); // Lost original error
}
```

### Do
```javascript
// Specific error handling
try {
  doSomething();
} catch (error) {
  if (error instanceof ValidationError) {
    // Handle validation error
  } else if (error instanceof NetworkError) {
    // Handle network error
  } else {
    // Unknown error - log and rethrow
    logger.error('Unexpected error', error);
    throw error;
  }
}

// Preserve original error
try {
  doSomething();
} catch (error) {
  throw new Error(`Processing failed: ${error.message}`, { cause: error });
}
```

## Documentation

Always document:
- What errors the function can throw
- Under what conditions they occur
- How to handle each error type

```javascript
/**
 * Validates and processes user input
 * @param {string} input - User input to process
 * @returns {Promise<ProcessedData>}
 * @throws {ValidationError} If input format is invalid
 * @throws {ProcessingError} If processing fails
 * @example
 * try {
 *   const result = await process('input');
 * } catch (error) {
 *   if (error instanceof ValidationError) {
 *     // Handle validation error
 *   }
 * }
 */
async function process(input) {
  // Implementation
}
```
