# Code Review Protocol

Automated pre-push verification that code, specs, and tests are aligned.

## The Alignment Triangle

```
           SPEC (docs/specifications/UGS_CURRENT_SPEC.md)
          /    \
    (1)  /      \  (4)
        /        \
     CODE  ←(2)→  TESTS
     (src/)   (3)   (*.test.ts)
```

## Checks

### Check 1: SPEC → CODE (Features Claimed = Features Exist)

**Input:**
- UGS_CURRENT_SPEC.md section "WHAT WE HAVE ✅"
- All `export` statements from src/**/*.ts

**Algorithm:**
```
FOR each line in spec containing "✅":
  EXTRACT feature_name (e.g., "Node Tags", "Streaming", "Embeddings")
  SEARCH code exports for related functions/classes:
    - "Node Tags" → expect: addTags, removeTags, getByTag, getByTags
    - "Streaming" → expect: streamText usage, onToken callback, stream option
    - "Embeddings" → expect: EmbeddingManager, embed, findSimilar
  IF no matching exports found:
    FAIL("Spec claims ✅ {feature_name} but no code found")

FOR each line in spec containing "❌":
  EXTRACT feature_name
  SEARCH code exports for related functions
  IF matching exports found:
    FAIL("Spec claims ❌ {feature_name} but code exists")
```

**Pass Criteria:** Every ✅ has code, no ❌ has code.

---

### Check 2: CODE → TESTS (Exports Have Tests)

**Input:**
- All `export class X` and `export function Y` from src/**/*.ts
- All `describe()` and `test()` from src/**/*.test.ts

**Algorithm:**
```
FOR each exported class/function in src/entities/*.ts:
  FIND corresponding test file (e.g., model.ts → model.test.ts)
  IF test file missing:
    FAIL("No test file for {filename}")

  SEARCH test file for tests mentioning the export:
    - Class "ModelManager" → expect tests with "ModelManager" or "model"
    - Function "addTags" → expect test("addTags...") or test("...add tags...")
  IF no tests found for export:
    FAIL("Export {name} in {file} has no tests")
```

**Pass Criteria:** Every public export has at least one test.

---

### Check 3: TESTS → BEHAVIOR (Tests Are Real)

**Input:**
- All test bodies from *.test.ts files

**Algorithm:**
```
FOR each test() block:
  EXTRACT test body

  CHECK for red flags:
    - expect(true).toBe(true)
    - expect(1).toBe(1)
    - Empty test body
    - Only .toBeDefined() checks
    - Commented out assertions (// expect...)
    - .skip() without explanation

  IF red flags found:
    FAIL("Test '{name}' appears to be gaming: {reason}")

  CHECK for good patterns:
    - Calls a function/method from the code under test
    - Makes assertion on the result
    - Tests both success and error cases
```

**Pass Criteria:** No gaming patterns, tests call real code.

---

### Check 4: COUNTS (Documented = Actual)

**Input:**
- Test count from `bun test` output: "N pass"
- Test count from UGS_CURRENT_SPEC.md: "N unit tests"

**Algorithm:**
```
EXTRACT actual_count from test runner output
EXTRACT documented_count from spec (grep for "tests passing" or "unit tests")
IF actual_count != documented_count:
  FAIL("Spec says {documented_count} tests but actual is {actual_count}")
```

**Pass Criteria:** Numbers match exactly.

---

## Overall Pass/Fail

```
IF all checks pass:
  OUTPUT "PASS"
ELSE:
  OUTPUT "FAIL: {list of specific failures}"
```

## Example Failures

```
FAIL: Spec claims ✅ Embeddings but no EmbeddingManager export found
FAIL: Export cosineSimilarity in embedding.ts has no tests
FAIL: Test 'should work' in model.test.ts appears to be gaming: expect(true).toBe(true)
FAIL: Spec says 322 tests but actual is 388
```

## Running the Check

```bash
# On changes only (runs on git push)
./scripts/check-push.sh

# Full codebase review
./scripts/check-push.sh --force
```
