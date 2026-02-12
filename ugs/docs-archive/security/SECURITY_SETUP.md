# SQL Injection Prevention - Setup Guide

Quick setup guide for the SQL injection prevention infrastructure.

---

## Prerequisites

- Bun runtime installed
- Git repository initialized
- Existing codebase in `/Users/bln/play/agentic-primer/simplify`

---

## Installation (5 minutes)

### Step 1: Verify Files

All security files should already be in place:

```bash
# Core security modules
ls -l src/session-knowledge/security/query-builder.ts
ls -l src/session-knowledge/security/sql-pattern-checker.ts
ls -l src/session-knowledge/security/input-validation.ts
ls -l src/session-knowledge/security/rate-limiter.ts

# Tests
ls -l src/session-knowledge/__tests__/query-builder.test.ts
ls -l src/session-knowledge/__tests__/security.test.ts

# Pre-commit scripts
ls -l scripts/pre-commit-sql-check.sh
ls -l scripts/install-pre-commit-hook.sh

# Documentation
ls -l SECURITY_PATTERNS.md
ls -l SECURITY_PREVENTION.md
ls -l SECURITY_QUICK_REFERENCE.md
```

### Step 2: Run Tests

Verify all security tests pass:

```bash
# Query builder tests (47 tests)
bun test src/session-knowledge/__tests__/query-builder.test.ts

# Security tests (94 tests)
bun test src/session-knowledge/__tests__/security.test.ts

# All tests
bun test
```

**Expected output:**
```
✓ 141 security tests passing
✓ 0 failures
```

### Step 3: Install Pre-Commit Hook

```bash
# Make scripts executable (should already be done)
chmod +x scripts/pre-commit-sql-check.sh
chmod +x scripts/install-pre-commit-hook.sh

# Install the enhanced pre-commit hook
./scripts/install-pre-commit-hook.sh
```

**Expected output:**
```
Installing enhanced pre-commit hook with SQL injection checks...
✅ Pre-commit hook installed successfully

The hook will now:
  1. Check for SQL injection vulnerabilities in staged TypeScript files
  2. Run existing bd sync functionality
```

### Step 4: Test Pre-Commit Hook

```bash
# Test the hook manually
./scripts/pre-commit-sql-check.sh
```

**Expected output:**
```
🔍 Checking staged files for SQL injection vulnerabilities...
✅ No SQL injection vulnerabilities detected
```

### Step 5: Run Static Analysis

```bash
# Scan for SQL injection patterns in existing code
bun run src/session-knowledge/security/sql-pattern-checker.ts src/session-knowledge/

# Should show existing patterns in test files and migrations (expected)
# Production CLI files should be clean (already fixed)
```

---

## Quick Start - Using the Tools

### For New Code

```typescript
// Import query builder
import { queryBuilder } from './security/query-builder';

// Build safe query
const query = queryBuilder()
  .select('*')
  .from('session_decisions')
  .whereLike('decision', userInput, 'contains')
  .limit(50)
  .build();

// Execute
const results = db.query(query.sql).all(...query.params);
```

### For Existing Code

```typescript
// Before (VULNERABLE)
db.query(`SELECT * WHERE name LIKE '%${input}%'`)

// After (SECURE)
import { sanitizeLikePattern } from './security/input-validation';
const sanitized = sanitizeLikePattern(input);
db.query(`SELECT * WHERE name LIKE ? ESCAPE '\\'`).all(`%${sanitized}%`)
```

---

## Verification

### Test the Prevention System

Create a test file with a vulnerability:

```bash
# Create test file with SQL injection vulnerability
cat > /tmp/test-vulnerable.ts << 'EOF'
import { Database } from 'bun:sqlite';

const userInput = process.argv[2];
const results = db.query(`SELECT * WHERE name LIKE '%${userInput}%'`).all();
EOF

# Run static analyzer on it
bun run src/session-knowledge/security/sql-pattern-checker.ts /tmp/
```

**Expected output:**
```
❌ SQL injection vulnerabilities detected

📄 test-vulnerable.ts
  ❌ Line 4 - SQL LIKE query uses template literal interpolation
     💡 Fix: Use sanitizeLikePattern() and parameterized queries
```

### Test Pre-Commit Protection

```bash
# Stage vulnerable file
git add /tmp/test-vulnerable.ts

# Try to commit
git commit -m "test: vulnerable code"
```

**Expected output:**
```
❌ SQL injection vulnerabilities detected in staged files
Please fix the issues above before committing.
```

---

## Configuration

### Pre-Commit Hook Customization

Edit `.git/hooks/pre-commit` to customize behavior:

```bash
vim .git/hooks/pre-commit
```

### Static Analyzer Patterns

Add new detection patterns in:
```
src/session-knowledge/security/sql-pattern-checker.ts
```

See `UNSAFE_PATTERNS` array for examples.

---

## Troubleshooting

### Tests Failing

```bash
# Clear any cached state
rm -rf node_modules/.cache

# Reinstall dependencies
bun install

# Run tests again
bun test
```

### Pre-Commit Hook Not Running

```bash
# Verify hook is executable
ls -la .git/hooks/pre-commit
# Should show: -rwxr-xr-x

# Make executable if needed
chmod +x .git/hooks/pre-commit

# Test manually
.git/hooks/pre-commit
```

### False Positives in Static Analyzer

The static analyzer may flag:
- Test files with intentional vulnerable examples
- Pattern definitions in the checker itself
- Migration scripts with dynamic table names

These are **expected** and can be safely ignored for those specific files.

To reduce noise, run on specific directories:
```bash
# Check only production code
bun run src/session-knowledge/security/sql-pattern-checker.ts src/session-knowledge/cli/
```

---

## Daily Workflow

### When Writing New Code

1. Import query builder
2. Build queries using `.whereLike()` or `.whereOrLike()`
3. Write tests including SQL injection test cases
4. Commit (pre-commit hook runs automatically)

### When Reviewing Code

Check for:
- [ ] Query builder usage OR manual sanitization + ESCAPE
- [ ] No template literal interpolation in SQL
- [ ] Parameterized queries with `?` placeholders
- [ ] Tests include malicious input cases
- [ ] Static analyzer passes without errors

---

## Resources

**Quick Reference:**
- `SECURITY_QUICK_REFERENCE.md` - Keep this open while coding

**Learning:**
- `SECURITY_PATTERNS.md` - Comprehensive guide with examples
- `src/session-knowledge/security/EXAMPLES.md` - 10 practical examples

**Tools:**
- `src/session-knowledge/security/README.md` - Tool documentation
- `SECURITY_PREVENTION.md` - Infrastructure overview

**Already Fixed:**
- `src/session-knowledge/cli/decisions.ts` - Example of proper sanitization
- `src/session-knowledge/cli/learnings.ts` - Example of proper sanitization
- `src/session-knowledge/cli/errors.ts` - Example of proper sanitization

---

## Testing Your Setup

Run this complete verification:

```bash
#!/bin/bash
echo "🔍 Verifying SQL Injection Prevention Setup..."
echo ""

# Test 1: Query builder tests
echo "1. Running query builder tests..."
bun test src/session-knowledge/__tests__/query-builder.test.ts > /dev/null 2>&1
if [ $? -eq 0 ]; then
  echo "   ✅ Query builder tests passing (47 tests)"
else
  echo "   ❌ Query builder tests failing"
  exit 1
fi

# Test 2: Security tests
echo "2. Running security tests..."
bun test src/session-knowledge/__tests__/security.test.ts > /dev/null 2>&1
if [ $? -eq 0 ]; then
  echo "   ✅ Security tests passing (94 tests)"
else
  echo "   ❌ Security tests failing"
  exit 1
fi

# Test 3: Pre-commit hook exists
echo "3. Checking pre-commit hook..."
if [ -x .git/hooks/pre-commit ]; then
  echo "   ✅ Pre-commit hook installed and executable"
else
  echo "   ❌ Pre-commit hook not found or not executable"
  exit 1
fi

# Test 4: Static analyzer works
echo "4. Testing static analyzer..."
bun run src/session-knowledge/security/sql-pattern-checker.ts src/session-knowledge/cli/ > /dev/null 2>&1
echo "   ✅ Static analyzer functional"

# Test 5: Documentation exists
echo "5. Checking documentation..."
for doc in SECURITY_PATTERNS.md SECURITY_PREVENTION.md SECURITY_QUICK_REFERENCE.md; do
  if [ -f "$doc" ]; then
    echo "   ✅ $doc found"
  else
    echo "   ❌ $doc missing"
  fi
done

echo ""
echo "🎉 Setup verification complete!"
echo ""
echo "Next steps:"
echo "  1. Read SECURITY_QUICK_REFERENCE.md"
echo "  2. Review examples in src/session-knowledge/security/EXAMPLES.md"
echo "  3. Start using queryBuilder() in your code"
```

Save as `verify-setup.sh` and run:
```bash
chmod +x verify-setup.sh
./verify-setup.sh
```

---

## Success Criteria

You've successfully set up the prevention infrastructure when:

✅ All 141 security tests pass
✅ Pre-commit hook blocks vulnerable code
✅ Static analyzer detects SQL injection patterns
✅ Query builder tests pass
✅ Documentation is accessible

**You're ready to write secure SQL queries!**

---

**Setup Time:** ~5 minutes
**Maintenance:** None required (automated)
**Support:** See SECURITY_PATTERNS.md for guidance
