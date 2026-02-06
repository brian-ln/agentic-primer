# Performance Optimization Insights

**Last Updated**: 2026-01-06
**Version**: 1.0
**Source**: Production Incidents & Performance Reviews
**Maintainer**: Infrastructure Team

## Overview

Lessons learned from optimizing performance across our services. These insights are based on real production issues and profiling results.

## Critical Learnings

### 1. Database Query Optimization

**The Problem**: API endpoints returning user data were taking 2-3 seconds despite database being healthy.

**Root Cause**: N+1 queries - fetching user details triggered separate queries for each user's profile, orders, and settings.

**Solution Applied**:
- Used database joins instead of application-level mapping
- Implemented eager loading in ORM (`.joinedload()` in SQLAlchemy)
- Added query result caching (Redis, 5-minute TTL)

**Performance Gain**: 2300ms → 180ms (92% reduction)

**Lessons**:
- Always use database profilers (PostgreSQL EXPLAIN ANALYZE)
- Prefer database joins over application loops
- Cache read-heavy query results
- Monitor N+1 query patterns with APM tools

### 2. Connection Pool Configuration

**The Problem**: Service degradation every evening (7-9pm) when user traffic peaked.

**Root Cause**: Connection pool exhaustion - pool size too small for peak traffic.

**Solution Applied**:
- Increased connection pool from 10 to 50 connections
- Added connection timeout alerts (warn at 80% utilization)
- Implemented queue timeout to fail fast (3-second wait limit)

**Performance Gain**: Error rates: 0.5% → 0.01% during peak hours

**Lessons**:
- Monitor connection pool utilization in production
- Size pool for peak traffic + 20% headroom
- Implement circuit breakers for cascading failures
- Use connection pooling middleware (pgbouncer)

### 3. Caching Strategy

**The Problem**: User dashboard took 8 seconds to load (multiple data fetches).

**Root Cause**: No caching of frequently accessed data (user profile loaded on every request).

**Solution Applied**:
```
- L1 Cache: Application in-memory (1-hour TTL)
- L2 Cache: Redis (24-hour TTL)
- L3 Cache: HTTP caching headers (browser cache)
```

**Performance Gain**: 8000ms → 400ms (95% reduction)

**Lessons**:
- Implement multi-layer caching strategy
- Cache invalidation on data updates only
- Use versioned cache keys for safe invalidation
- Monitor cache hit rates (aim for 80%+)

### 4. API Response Size

**The Problem**: Mobile app users complained about slow API requests on 4G networks.

**Root Cause**: API responses contained unnecessary fields (full user objects when only name needed).

**Solution Applied**:
- Implement field selection via query parameters: `/api/users?fields=name,email`
- Create lightweight "summary" endpoints for list views
- Gzip compression enabled (90% of responses < 50KB)

**Performance Gain**: Average response size: 450KB → 45KB (90% reduction)

**Lessons**:
- Only return fields needed by client
- Create separate endpoints for different use cases
- Enable gzip compression by default
- Monitor response sizes per endpoint

### 5. Index Strategy

**The Problem**: Search functionality became unusable at scale (> 1M records).

**Root Cause**: Missing database indexes on frequently filtered columns.

**Solution Applied**:
- Added composite indexes on (status, created_at) for filtering
- Added partial indexes for boolean columns (WHERE active = true)
- Removed unused indexes (disk space, write overhead)

**Performance Gain**: 12s searches → 200ms searches

**Lessons**:
- Profile queries with EXPLAIN ANALYZE before deploying
- Use composite indexes for multi-column filters
- Use partial indexes for sparse data
- Review index effectiveness quarterly
- Monitor index bloat

### 6. Batch Processing

**The Problem**: Nightly jobs sending individual notification emails took 6 hours to process 1M users.

**Root Cause**: One email per loop iteration, network overhead repeated 1M times.

**Solution Applied**:
- Batch emails into groups of 100
- Use bulk SES API instead of individual send calls
- Parallel processing with 10 concurrent workers

**Performance Gain**: 6 hours → 12 minutes (30x faster)

**Lessons**:
- Batch external API calls (database, email, payment services)
- Use parallel workers for I/O-bound operations
- Implement backpressure/circuit breakers
- Monitor resource utilization during batch runs

### 7. Monitoring & Alerting

**The Problem**: Performance degradation went unnoticed for hours before customer complaints.

**Root Cause**: No real-time monitoring of key metrics.

**Solution Applied**:
```
Metrics Tracked:
- API response times (p50, p95, p99)
- Database query times
- Error rates
- Cache hit rates
- Connection pool utilization

Alerts Set:
- Response time > 1s (warn)
- Response time > 5s (critical)
- Error rate > 1% (warn)
- Error rate > 5% (critical)
```

**Lessons**:
- Monitor before optimizing (measure what you improve)
- Track percentiles not just averages (p99 matters more than p50)
- Alert on trends (degradation over time), not just spikes
- Use APM tools (DataDog, New Relic) for visibility

## Performance Checklist

Before deploying new features:

- [ ] Profile database queries (EXPLAIN ANALYZE)
- [ ] Check for N+1 query patterns
- [ ] Size caches appropriately
- [ ] Verify index strategy
- [ ] Load test with peak traffic volume
- [ ] Review API response sizes
- [ ] Enable appropriate caching headers
- [ ] Implement monitoring/alerting
- [ ] Test on slow networks (4G simulation)

## Tools & Techniques

### Database Profiling
```
EXPLAIN ANALYZE SELECT * FROM users WHERE status = 'active';
```

### Application Profiling
- Python: cProfile, py-spy
- Node.js: node --prof, clinic.js
- Java: JFR, async-profiler

### Load Testing
- k6 (modern load testing)
- JMeter (enterprise option)
- Artillery (Node.js focused)

### Monitoring
- Prometheus + Grafana
- DataDog
- New Relic
- AWS CloudWatch

## References

- [Database Technology Choice Decision](../decisions/db-choice.md)
- [API Design Patterns](../patterns/api-design.md)
- [PostgreSQL Performance Wiki](https://wiki.postgresql.org/wiki/Performance_Optimization)
