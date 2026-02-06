# ADR 001: Use REST API Instead of GraphQL

**Status:** Accepted
**Date:** 2026-01-08
**Decision Makers:** Engineering Team
**Category:** Architecture Decision Record

## Context

We need to design an API for our application that will be consumed by:
- Web application (React)
- Mobile applications (iOS and Android)
- Third-party integrations
- Internal microservices

We evaluated two primary options: REST API and GraphQL.

## Decision

We will use **REST API with JSON** as our primary API design approach.

## Alternatives Considered

### Option 1: REST API
**Pros:**
- Team has 5+ years experience with REST
- Mature tooling and libraries
- Simple to understand and document
- Good caching with HTTP headers
- Wide adoption and support

**Cons:**
- Over-fetching or under-fetching data
- Multiple requests for related data
- Versioning can be complex
- Less flexible for evolving client needs

### Option 2: GraphQL
**Pros:**
- Clients request exactly what they need
- Single endpoint for all queries
- Strong type system
- Excellent for complex, nested data
- Self-documenting with introspection

**Cons:**
- Team has limited GraphQL experience
- Steeper learning curve
- Caching is more complex
- N+1 query problem requires careful optimization
- May be overkill for our use case

### Option 3: gRPC
**Pros:**
- High performance with Protocol Buffers
- Strong typing and code generation
- Good for microservices communication

**Cons:**
- Not browser-friendly (requires proxy)
- Limited team experience
- Overhead for simple CRUD operations

## Rationale

We chose REST API based on the following factors:

### 1. Team Experience
- All team members have REST API experience
- No GraphQL experience on the team
- Faster development velocity with familiar technology
- Lower onboarding cost for new team members

### 2. Use Case Simplicity
- Our data model is relatively flat (not deeply nested)
- Most client needs are predictable CRUD operations
- Don't need highly flexible querying
- Over-fetching is acceptable for our scale

### 3. Tooling and Ecosystem
- Excellent REST tooling: Swagger/OpenAPI, Postman, REST Client
- HTTP caching works out of the box
- API gateway support is mature
- Monitoring and debugging tools are well-established

### 4. Performance Requirements
- Current scale doesn't require GraphQL's optimization
- HTTP caching meets our performance needs
- Can optimize with API Gateway if needed
- REST is "good enough" for foreseeable future

### 5. Third-Party Integration
- Most third-party services expect REST APIs
- REST is the industry standard for public APIs
- Easier for partners to integrate

## Consequences

### Positive Consequences

1. **Fast Development:** Team can start building immediately
2. **Simple Documentation:** OpenAPI/Swagger is straightforward
3. **Easy Testing:** REST testing tools are mature
4. **Reliable Caching:** HTTP caching headers work well
5. **Wide Support:** All clients can consume REST easily

### Negative Consequences

1. **Over-fetching:** Clients may receive more data than needed
2. **Multiple Requests:** Related data may require multiple API calls
3. **Versioning:** Need to plan API versioning strategy
4. **Less Flexible:** Harder to adapt to evolving client requirements

### Mitigation Strategies

For the negative consequences:

1. **Over-fetching:**
   - Implement field filtering with query parameters
   - Example: `GET /users?fields=id,name,email`

2. **Multiple Requests:**
   - Provide composite endpoints for common use cases
   - Example: `GET /users/:id/posts` returns user with posts

3. **Versioning:**
   - Use URL versioning: `/api/v1/...`
   - Maintain backward compatibility for at least 6 months

4. **Flexibility:**
   - Monitor actual usage patterns
   - Add endpoints based on real needs
   - Consider GraphQL in future if requirements change

## Implementation Guidelines

### API Design Principles

1. **Resource-based URLs:**
   ```
   GET    /api/v1/users         # List users
   POST   /api/v1/users         # Create user
   GET    /api/v1/users/:id     # Get user
   PUT    /api/v1/users/:id     # Update user
   DELETE /api/v1/users/:id     # Delete user
   ```

2. **HTTP Methods:**
   - GET: Retrieve resources (idempotent)
   - POST: Create resources
   - PUT: Update resources (idempotent)
   - PATCH: Partial updates
   - DELETE: Remove resources (idempotent)

3. **Status Codes:**
   - 200: Success
   - 201: Created
   - 204: No Content
   - 400: Bad Request
   - 401: Unauthorized
   - 403: Forbidden
   - 404: Not Found
   - 500: Internal Server Error

4. **Response Format:**
   ```json
   {
     "data": { ... },
     "meta": {
       "timestamp": "2026-01-08T12:34:56Z",
       "requestId": "req_123"
     }
   }
   ```

5. **Error Handling:**
   - See [api-error-handling.md](../patterns/api-error-handling.md)

## Review and Reassessment

This decision should be reviewed if:
- Client needs become significantly more complex
- We experience performance issues due to over-fetching
- Team gains GraphQL expertise
- Industry standards shift significantly
- We receive feedback that REST is limiting product features

**Next review scheduled:** Q3 2026

## Related Decisions

- **Future ADR:** API versioning strategy
- **Future ADR:** Authentication and authorization approach
- **Future ADR:** Rate limiting implementation

## Related Patterns

- [API Error Handling](../patterns/api-error-handling.md)
- **Future:** Pagination pattern
- **Future:** Filtering and sorting pattern

## References

- [REST API Tutorial](https://restfulapi.net/)
- [OpenAPI Specification](https://swagger.io/specification/)
- [Roy Fielding's Dissertation on REST](https://www.ics.uci.edu/~fielding/pubs/dissertation/rest_arch_style.htm)
- [GraphQL vs REST: A Comparison](https://hasura.io/learn/graphql/intro-graphql/graphql-vs-rest/)
- [Best Practices for REST API Design](https://stackoverflow.blog/2020/03/02/best-practices-for-rest-api-design/)

## Approval

- **Proposed by:** Engineering Lead
- **Reviewed by:** Full Engineering Team
- **Approved by:** CTO
- **Date:** 2026-01-08

## Changelog

- **2026-01-08:** Initial ADR created and approved
