# Test Runner Scripts

Helper scripts for running Concept Graph Playwright tests.

## Scripts

### `run-tests.sh` (Standard Runner)
**Purpose:** Run tests in foreground with live output
**Use when:** You want to see test results immediately

```bash
./run-tests.sh                    # Run all tests
./run-tests.sh --grep "search"    # Run specific tests
```

### `run-tests-bg.sh` (Background Runner)
**Purpose:** Run tests in background with output capture
**Use when:** Running tests while doing other work, or for CI/automation

```bash
./run-tests-bg.sh                 # Start tests in background
./run-tests-bg.sh --grep "API"    # Run specific tests in background
```

**Output:**
- Creates `test-logs/` directory
- Logs to timestamped file: `test-logs/test-output-YYYYMMDD-HHMMSS.log`
- Creates symlink: `test-logs/latest.log` → most recent run
- Stores PID in: `test-logs/test-runner.pid`

### `watch-tests.sh` (Output Watcher)
**Purpose:** Watch background test output in real-time
**Use when:** Tests are running in background and you want to see progress

```bash
./watch-tests.sh                  # Follow latest test output (tail -f)
```

Press `Ctrl+C` to stop watching (tests continue running).

## Typical Workflow

```bash
# Terminal 1: Start background tests
./run-tests-bg.sh

# Terminal 2: Watch output
./watch-tests.sh

# Or in same terminal:
./run-tests-bg.sh && sleep 2 && ./watch-tests.sh
```

## Managing Background Tests

```bash
# Check if tests are running
cat test-logs/test-runner.pid
ps -p $(cat test-logs/test-runner.pid)

# Kill background tests
kill $(cat test-logs/test-runner.pid)

# View completed test output
less test-logs/latest.log

# List all test runs
ls -lt test-logs/*.log
```

## Log Files

```
test-logs/
├── latest.log -> test-output-20260116-071345.log  # Symlink to latest
├── test-output-20260116-071345.log                # Timestamped logs
├── test-output-20260116-070823.log
└── test-runner.pid                                # Current runner PID
```

## Exit Codes

- `0` - All tests passed
- `1` - Some tests failed
- Check log file for details

## Integration with CI

```bash
# Run tests and capture output
./run-tests-bg.sh
TEST_PID=$(cat test-logs/test-runner.pid)

# Wait for completion
wait $TEST_PID
TEST_EXIT_CODE=$?

# Check results
if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo "Tests passed"
else
    echo "Tests failed"
    cat test-logs/latest.log
fi
```

## Notes

- Background runner installs Playwright browsers automatically if needed
- All scripts change to correct directory, can be run from anywhere
- Test logs persist across runs (manual cleanup required)
- Watch script uses `tail -f` for real-time following
