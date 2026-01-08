# Code Patterns

Reusable solutions for common development problems that have been validated and documented.

## Available Patterns

### 1. REST API CRUD Operations

**ID:** rest-api-crud
**Complexity:** Moderate
**Tags:** api, rest, crud, validation, testing
**Status:** Active
**Last Updated:** 2026-01-06

Comprehensive pattern for implementing REST CRUD endpoints with validation, error handling, and testing. This is the most commonly used pattern in the codebase.

**Components:**
- Route definition with proper HTTP methods
- Request validation (input sanitization and type checking)
- CRUD operations with error handling
- Response formatting and status codes
- Unit tests with high coverage
- Integration tests with mocked services
- Documentation with examples

**When to Use:**
- Creating new API endpoints
- Implementing data persistence layer
- Adding CRUD operations to existing services

**Best Practices:**
- Always validate input before processing
- Use consistent error response format
- Test both happy path and error cases
- Document API contracts clearly
- Consider pagination for list endpoints

**Related Patterns:**
- See [Error Handling Pattern](#2-error-handling-and-logging) for error strategies
- See [Database Migration Pattern](#3-database-schema-migration) for schema changes

**Example Workflow:**
1. Define route handler
2. Apply input validation middleware
3. Implement CRUD logic
4. Add comprehensive error handling
5. Write unit tests (>80% coverage)
6. Write integration tests
7. Document endpoint contract

---

### 2. Error Handling and Logging

**ID:** error-handling
**Complexity:** Simple
**Tags:** errors, logging, debugging, monitoring
**Status:** Active
**Last Updated:** 2026-01-06

Structured approach to error handling and logging across the application. Ensures consistent error responses and complete audit trails.

**Principles:**
- Distinguish between client errors (4xx) and server errors (5xx)
- Always include error context in logs
- Use unique error IDs for tracking
- Log request ID for tracing
- Never expose internal details to clients

**Error Response Format:**
```json
{
  "error": {
    "id": "ERR_VALIDATION_FAILED",
    "message": "User-friendly message",
    "status": 400,
    "timestamp": "2026-01-06T00:31:27Z",
    "request_id": "req_abc123",
    "details": {}
  }
}
```

**Logging Levels:**
- **ERROR:** Application failures, unhandled exceptions
- **WARN:** Unusual conditions, deprecations
- **INFO:** Significant events (login, purchase, etc.)
- **DEBUG:** Detailed execution flow

**Related Patterns:**
- Use with [REST API CRUD Pattern](#1-rest-api-crud-operations) for endpoint errors
- Complements [Database Migration Pattern](#3-database-schema-migration) for migration logs

**Common Pitfalls to Avoid:**
- Logging passwords or sensitive data
- Swallowing exceptions silently
- Inconsistent error response formats
- Missing request tracing context

---

### 3. Database Schema Migration

**ID:** database-migration
**Complexity:** Complex
**Tags:** database, migration, schema, compatibility
**Status:** Active
**Last Updated:** 2026-01-06

Safe pattern for evolving database schema over time with forward and backward compatibility considerations. Critical for zero-downtime deployments.

**Core Practices:**

**1. Reversible Migrations**
- Always provide UP and DOWN migrations
- Test rollback scenarios
- Never use irreversible operations

**2. Forward/Backward Compatibility**
- Add new columns with defaults
- Don't remove columns immediately (mark as deprecated first)
- Handle both old and new schema versions in code

**3. Migration Safety**
- Run migrations on off-peak hours
- Test on staging environment first
- Have rollback plan ready
- Monitor performance impact

**4. Schema Evolution Strategy**
- Phase 1: Add new column with default
- Phase 2: Deploy code to use new column
- Phase 3: Migrate existing data
- Phase 4: Remove old column (optional)

**Testing Migrations:**
- Test on production-like dataset size
- Verify performance (check index usage)
- Test rollback procedure
- Validate data integrity after migration

**Related Patterns:**
- See [REST API CRUD Pattern](#1-rest-api-crud-operations) when schema affects API
- Reference [ADR: Database Choice](../decisions/adr-002-database-choice.md) for PostgreSQL specifics

**Example Migration Checklist:**
- [ ] Migration is reversible
- [ ] New columns have defaults
- [ ] Indexes created for new columns
- [ ] Application code handles both schemas
- [ ] Performance impact assessed
- [ ] Tested on staging environment
- [ ] Rollback procedure documented
- [ ] Team notified of deployment

---

## How to Add New Patterns

When you discover a reusable solution:

1. **Document the Pattern**
   - Create markdown file in `docs/knowledge/patterns/`
   - Use template: ID, Complexity, Tags, Status
   - Include components, when to use, best practices

2. **Add to Registry**
   - Update `docs/knowledge/index.json`
   - Add search tags for discoverability
   - Link from related patterns

3. **Update This Index**
   - Add entry to the patterns list
   - Include cross-references

4. **Share with Team**
   - Request feedback
   - Refine based on learnings
   - Mark as stable when validated

## Pattern Selection Guide

| What You're Building | Pattern | Estimated Time |
|----------------------|---------|-----------------|
| REST API endpoint for CRUD | rest-api-crud | 30 minutes |
| Error handling strategy | error-handling | 10 minutes |
| Schema evolution | database-migration | 1-2 hours |
| Multiple related patterns | Combine above | 2-3 hours |

## Pattern Quality Metrics

- **Completeness:** All essential components documented
- **Clarity:** Examples are clear and executable
- **Reusability:** Can be applied to multiple problems
- **Maintainability:** Updated as patterns evolve

## Current Coverage

- REST API development: ✅ Covered
- Error handling: ✅ Covered
- Database evolution: ✅ Covered
- Testing strategies: 🔄 In progress
- Security patterns: 🔄 In progress
- Deployment patterns: 📋 Planned

---

**Last Reviewed:** 2026-01-06
**Maintained By:** @copilot automation + team
**Status:** Production-ready
