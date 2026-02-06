# Database Technology Choice

**Decision Date**: 2025-12-15
**Version**: 1.0
**Decision Maker**: Architecture Team
**Status**: Approved and Active

## Summary

Chose PostgreSQL as the primary database technology for all new projects and services.

## Decision

**Use PostgreSQL 14+ as the default database for:**
- All new backend services
- Microservices requiring persistence
- Long-term data storage
- OLTP workloads

**Alternatives Evaluated:**
- MongoDB (document database)
- MySQL (open-source relational)
- DynamoDB (AWS managed)
- Cassandra (distributed)

## Rationale

### PostgreSQL Selected Because:

1. **ACID Compliance**
   - Full support for transactions
   - Data integrity guarantees
   - Suitable for financial/critical data

2. **Advanced Features**
   - JSON/JSONB data types (flexible schema)
   - Full-text search capabilities
   - Array and range types
   - Window functions for analytics

3. **Ecosystem & Tooling**
   - Mature, stable, battle-tested
   - Excellent ORM support (SQLAlchemy, Django ORM, Sequelize)
   - Strong monitoring tools
   - Active open-source community

4. **Performance**
   - Efficient query planner
   - Excellent indexing options
   - Connection pooling support
   - Proven at scale (thousands of TPS)

5. **Cost**
   - Open source (free license)
   - Runs on commodity hardware
   - Minimal licensing concerns
   - AWS RDS instances reasonably priced

### Trade-offs Accepted:

- **Schema definition required upfront** (vs. MongoDB's flexibility)
  - *Accepted because*: Schema clarity improves code reliability

- **Vertical scaling limits** (vs. Cassandra's horizontal scaling)
  - *Accepted because*: Vertical scaling sufficient for current needs

- **Not ideal for unstructured data** (vs. MongoDB)
  - *Accepted because*: JSONB provides sufficient flexibility for our use cases

## Implementation Details

### Version Standard
- Production: PostgreSQL 14 or higher
- Development: PostgreSQL 12 or higher
- Docker image: `postgres:14-alpine`

### Connection Configuration
```
Host: [RDS endpoint]
Port: 5432
Database: [service_name]_[environment]
User: [service_user]
SSL: Required (sslmode=require)
Connection Pool: 20-50 connections per service
```

### Backup & Recovery
- Automated daily backups (retention: 30 days)
- Point-in-time recovery enabled
- Backup testing monthly
- Recovery RTO: < 1 hour
- Recovery RPO: < 15 minutes

### Monitoring
- CloudWatch metrics (CPU, memory, connections)
- Query performance insights enabled
- Slow query log monitored (> 1 second queries)
- Disk space alerts at 80% threshold

## Migration Path

For services currently using other databases:

1. **Assess data structure**
   - Identify schema requirements
   - Check for unstructured data needs

2. **Plan migration**
   - Create equivalent PostgreSQL schema
   - Test data migration scripts
   - Plan cutover window

3. **Execute migration**
   - Run migration in staging environment
   - Validate data integrity
   - Perform cutover during low-traffic period

4. **Decommission old database**
   - Keep backups for 90 days post-migration
   - Document any data transformation rules

## Review & Reconsideration

This decision will be reviewed:
- **Annually** (or on major version releases)
- **If new requirements emerge** that PostgreSQL doesn't meet well
- **If performance issues** arise at scale
- **If team skills/preferences** significantly shift

Next review date: 2026-12-15

## Related Decisions

- [Deployment Strategy and Infrastructure](deployment.md) - How databases are deployed
- [API Design Patterns](../patterns/api-design.md) - How services interact with data

## Questions & Answers

**Q: Why not use a document database like MongoDB?**
A: PostgreSQL's JSONB support provides 80% of MongoDB's flexibility with full ACID guarantees. Document database complexity typically not needed.

**Q: What about scaling horizontally?**
A: PostgreSQL's vertical scaling capacity is sufficient. For horizontal scaling, we'll implement read replicas and database sharding at application layer when needed.

**Q: What's the upgrade path?**
A: PostgreSQL is designed for smooth major version upgrades. Test upgrades on staging quarterly. Production upgrades done during maintenance windows.
