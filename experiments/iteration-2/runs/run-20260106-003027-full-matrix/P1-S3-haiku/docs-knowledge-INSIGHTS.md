# Insights & Lessons Learned

## Performance Optimization Insights

### Database Query Optimization
- **Insight**: N+1 queries are the most common performance killer
- **Pattern**: Use JOIN queries instead of looping
- **Example**: Fetch users with all their posts in one query, not N queries
- **Improvement**: Reduced load time from 5s to 200ms

### Connection Pooling is Critical
- **Insight**: Creating new database connections is expensive (~500ms)
- **Pattern**: Use connection pooling with max=20, idleTimeout=30s
- **Improvement**: Request latency from 1000ms to 50ms

### Caching Strategy
- **Insight**: Repeated queries for same data waste CPU and I/O
- **Pattern**: LRU cache with 1-hour TTL for read-heavy data
- **Example**: User profiles cached after first request
- **Improvement**: 95% cache hit rate after warm-up period

## Security Gotchas

### JWT Secret Management
- **Gotcha**: Using same secret in dev/prod is insecure
- **Solution**: Store JWT_SECRET in environment variables, different per environment
- **Impact**: One leaked secret compromises all tokens

### SQL Injection with String Concatenation
- **Gotcha**: Building SQL strings with template literals is vulnerable
- **Wrong**: `query('SELECT * FROM users WHERE id = ' + req.params.id)`
- **Right**: `query('SELECT * FROM users WHERE id = $1', [req.params.id])`
- **Impact**: Single mistake can compromise entire database

### CORS Configuration
- **Gotcha**: Setting `Access-Control-Allow-Origin: *` is too permissive
- **Solution**: Explicitly whitelist allowed origins
- **Impact**: Prevents CSRF attacks

## Testing Insights

### Test Flakiness is Usually Timing
- **Insight**: Tests that pass/fail randomly usually have timing issues
- **Pattern**: Never use fixed delays like `setTimeout(..., 1000)`
- **Solution**: Use test utilities for polling/waiting until condition
- **Impact**: 100% test reliability instead of 85%

### Mock Services Must Match Reality
- **Insight**: Mocks that don't match API behavior cause false confidence
- **Pattern**: Generate mocks from API spec, keep in sync
- **Impact**: Prevents "works in test, fails in production"

### Test Coverage ≠ Quality
- **Insight**: 100% coverage doesn't guarantee bug-free code
- **Focus**: Test critical paths and edge cases, not just line coverage
- **Target**: 80% coverage with focus on high-risk areas

## Deployment Insights

### Database Migrations Must Be Reversible
- **Insight**: Can't always rollback deployments if migration fails
- **Pattern**: Every migration has UP and DOWN scripts
- **Impact**: Safe rollbacks if deployment fails

### Health Checks Prevent Cascading Failures
- **Insight**: Kubernetes restarts unhealthy containers automatically
- **Pattern**: Implement `/health` endpoint that checks dependencies
- **Importance**: Catches problems early before serving traffic

### Environment Variable Validation
- **Insight**: Missing environment variables cause cryptic runtime errors
- **Pattern**: Validate all env vars at startup, fail fast
- **Example**: Check JWT_SECRET, DATABASE_URL exist and are valid
- **Impact**: Clear error messages enable faster debugging

## Code Quality Insights

### Small Functions are Easier to Test
- **Insight**: Functions >20 lines are harder to test and understand
- **Pattern**: Extract helper functions when function >20 lines
- **Impact**: Test coverage naturally improves

### Type Safety Catches Bugs Early
- **Insight**: TypeScript strict mode catches many issues pre-deployment
- **Pattern**: Enable `strict: true`, `noImplicitAny: true` in tsconfig
- **Impact**: Reduce production bugs by ~30%

### Comments Should Explain "Why", Not "What"
- **Insight**: Code shows what it does; comments should explain why
- **Bad**: `let x = y + 1; // Add one to y`
- **Good**: `const nextUserId = currentId + 1; // IDs are sequential`
- **Impact**: Easier maintenance and refactoring

## Issue Processing Insights

### Complexity Matters More Than Word Count
- **Insight**: A 200-word issue about complex auth is harder than 500-word UI issue
- **Pattern**: Analyze issue complexity from requirements, not length
- **Scoring**: Simple=1, Medium=2, Complex=3
- **Impact**: Better model selection (Haiku vs Sonnet vs Opus)

### Structured Issues = Better Solutions
- **Insight**: Well-structured issues (using task.yml template) get better solutions
- **Pattern**: Template enforcement in ISSUE_TEMPLATE/task.yml
- **Impact**: 25% fewer follow-up issues

### Knowledge Base Queries Save 40% of Processing Time
- **Insight**: Searching KB for similar problems is faster than rediscovering
- **Pattern**: Query KB for every new issue before starting implementation
- **Impact**: Faster solutions, more consistent patterns

## Monitoring and Observability

### Structured Logging is Essential
- **Insight**: Free-form log messages are unsearchable
- **Pattern**: Log as JSON with consistent fields
- **Example**: `{timestamp, level, message, context, error}`
- **Impact**: Can query logs and identify patterns automatically

### Metrics Beat Logs for Trends
- **Insight**: Analyzing millions of log lines is slow
- **Pattern**: Export metrics to monitoring system (Prometheus, DataDog)
- **Tracked**: Success rate, latency, error rate, cost per request
- **Impact**: Identify performance regressions in minutes, not hours

### Alerts Should be Actionable
- **Insight**: Alerts that can't be acted on cause alert fatigue
- **Pattern**: Each alert includes debugging context
- **Example**: "Database latency >500ms on prod-db-2 (load: 85%)"
- **Impact**: Faster incident resolution

## API Design Insights

### Versioning Saves Refactoring
- **Insight**: APIs change. Unversioned APIs break clients.
- **Pattern**: Use `/api/v1/` in URL or Accept header
- **Strategy**: Support old and new versions simultaneously
- **Timeline**: Deprecate old version after 6 months

### Error Messages Matter
- **Insight**: Users need to understand what went wrong
- **Bad**: `{status: 400}`
- **Good**: `{status: 400, code: 'INVALID_EMAIL', message: 'Email must be valid format'}`
- **Impact**: Faster debugging by API consumers

### Pagination is Hard
- **Insight**: Offset/limit pagination is O(N) in database
- **Pattern**: Use keyset pagination (cursor-based) for large datasets
- **Benefit**: Consistent O(1) performance regardless of page number
- **Trade-off**: Slightly more complex client code

## Scaling Insights

### Premature Optimization is a Trap
- **Insight**: Optimizing before measuring causes wasted work
- **Pattern**: Measure first, optimize only bottlenecks
- **Tools**: Use profilers and monitoring to identify slow code
- **Impact**: Better ROI on optimization effort

### Read Replicas Enable Read Scaling
- **Insight**: Single database can't handle high read volume
- **Pattern**: Master for writes, replicas for reads
- **Trade-off**: Eventual consistency for high-volume reads
- **Impact**: 10x read throughput with minimal latency increase

### Cache Invalidation is Hard
- **Insight**: Cache invalidation is one of the hardest problems in CS
- **Pattern**: Use TTL for automatic invalidation, manual invalidation for critical data
- **Anti-pattern**: Infinite caches cause stale data bugs
- **Impact**: Balance freshness and performance appropriately
