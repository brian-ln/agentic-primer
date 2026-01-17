# Agent Note: Testing and Spec Updates Required

**To:** Agent afb7bd4 (CLI Optimization Implementation)
**From:** Parent Agent
**Priority:** CRITICAL

## Additional Requirements for Your Task

After implementing the P0 CLI features, you MUST also:

### 1. Testing (CRITICAL)

Create comprehensive tests for all new features:

- **Test Location:** Create `examples/cli-tests/` or `tests/cli/` directory
- **Test Coverage Required:**
  - JSON output for all commands (list, show, ready, search, add, update, delete)
  - Batch-add with sample JSON files
  - Batch-update with sample JSON files
  - --yes non-interactive mode
  - --fields projection
  - Stdin/stdout piping

- **Test Format:** Either:
  - Bun test files if test infrastructure exists
  - Shell scripts with assertions
  - Example files showing usage and expected output

- **Minimum Tests:**
  ```bash
  # Example test structure
  examples/cli-tests/
    test-json-output.sh
    test-batch-add.sh
    test-batch-update.sh
    test-fields-projection.sh
    test-piping.sh
    sample-tasks.json
    sample-updates.json
  ```

### 2. Specification Updates (CRITICAL)

Update or create formal specifications following the project's spec pattern:

**Files to Update/Create:**
1. **src/cli/TASK_CLI.spec.md** - Human-readable specification
   - Document all new commands
   - Document all new flags (--json, --yes, --fields)
   - Document new data structures
   - Show examples of usage

2. **src/cli/TASK_CLI.model.lisp** - Formal model in Lisp
   - Define command signatures
   - Define data structures (JsonResponse, BatchTaskSpec, BatchUpdateSpec)
   - Model state transitions
   - Express invariants

**Reference Pattern:**
Look at these existing specs for the pattern to follow:
- `GRAPH_SYSTEM.spec.md` / `GRAPH_SYSTEM.model.lisp`
- `TASK_SYSTEM.spec.md` / `TASK_SYSTEM.model.lisp`
- `KNOWLEDGE_SYSTEM.spec.md` / `KNOWLEDGE_SYSTEM.model.lisp`

### 3. Updated Deliverables

Your completion report should include:
1. ✅ Modified src/cli/task.ts (all P0 features)
2. ✅ Test files/examples (comprehensive coverage)
3. ✅ src/cli/TASK_CLI.spec.md (created or updated)
4. ✅ src/cli/TASK_CLI.model.lisp (created or updated)
5. ✅ Test results (all tests pass)
6. ✅ Documentation of new commands in --help text

### 4. Success Criteria Update

All of these must be true:
- All P0 features implemented
- All features have test coverage
- All tests pass
- Specs accurately reflect implementation
- Documentation complete
- Backward compatibility verified

## Why This Matters

Testing ensures the features work correctly and won't break in the future. Specifications provide formal documentation that other agents and developers can rely on. Both are essential for production-quality code.

## Questions?

If you need clarification on spec format or test requirements, use the CLARIFICATION_NEEDED protocol.
