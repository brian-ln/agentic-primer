# 001. Use REST API for External Interface

**Date:** 2026-01-06
**Status:** Accepted
**Deciders:** @owner, engineering team

## Context and Problem Statement

We need to design an API for external clients (web frontend, mobile apps, third-party integrations) to interact with our system. The API must be easy to use, well-documented, and maintainable over time.

**Key requirements:**
- Simple for frontend developers to consume
- Support for standard HTTP caching
- Easy to version and evolve
- Wide tooling support
- Low learning curve for new team members

## Decision Drivers

- **Developer Experience** - Frontend team familiarity
- **Tooling Ecosystem** - Availability of clients, docs generators
- **Caching** - HTTP-level caching for performance
- **Simplicity** - Minimize cognitive overhead
- **Industry Standards** - Alignment with common practices

## Considered Options

1. **REST API** (Resource-oriented, HTTP verbs)
2. **GraphQL** (Query language, single endpoint)
3. **gRPC** (Protocol buffers, high performance)

## Decision Outcome

**Chosen option:** "REST API", because it provides the best balance of simplicity, tooling support, and developer familiarity for our use case.

### Positive Consequences

- ✅ Frontend team already familiar with REST patterns
- ✅ HTTP caching works out-of-the-box (CDN, browser cache)
- ✅ Simple versioning strategy (URL path: `/api/v1/`, `/api/v2/`)
- ✅ Excellent OpenAPI/Swagger tooling for docs
- ✅ Easy to test with curl, Postman, browser devtools
- ✅ GitHub Copilot has extensive REST API training data

### Negative Consequences

- ❌ Over-fetching: Clients may receive more data than needed
- ❌ Under-fetching: Multiple requests needed for related data
- ❌ No built-in schema validation (must add manually)
- ❌ Versioning requires maintenance of multiple endpoints

## Pros and Cons of the Options

### REST API

**Good:**
- Wide industry adoption, well-understood patterns
- Native HTTP caching support (ETags, Cache-Control)
- Simple mental model (resources + verbs)
- Excellent debugging tools (browser devtools, Postman)
- Easy to implement with standard web frameworks (Express, Flask, Django)

**Bad:**
- No automatic schema validation (need to add manually)
- Can lead to over-fetching or under-fetching data
- Multiple round trips for related resources
- Versioning strategy requires discipline

### GraphQL

**Good:**
- Clients request exactly the data they need (no over-fetching)
- Single request for complex, nested data
- Strong typing and introspection
- Automatic schema documentation
- Excellent for complex, graph-like data

**Bad:**
- Steeper learning curve for team
- Caching more complex (no HTTP-level caching by default)
- Potential for expensive queries (N+1 problem)
- Smaller ecosystem compared to REST
- May be overkill for simple CRUD operations

### gRPC

**Good:**
- High performance (binary protocol buffers)
- Strong typing with code generation
- Bidirectional streaming support
- Great for microservice-to-microservice communication

**Bad:**
- Not browser-native (requires gRPC-web proxy)
- Binary format makes debugging harder
- Smaller community compared to REST
- Overkill for our use case (frontend-backend communication)
- Limited HTTP caching capabilities

## Implementation Notes

We will use **OpenAPI 3.0** specification to document our REST API:
- Auto-generate documentation with Swagger UI
- Use schema validation middleware (e.g., `express-openapi-validator`)
- Generate TypeScript types for frontend from OpenAPI spec

**Standard patterns:**
- Use HTTP verbs semantically (GET, POST, PUT, PATCH, DELETE)
- Follow RESTful URL naming (`/api/v1/users`, `/api/v1/users/:id`)
- Return consistent error format (see [Pattern: API Error Handling](../patterns/api-error-handling.md))
- Use proper HTTP status codes (200, 201, 400, 404, 500, etc.)

## Related Decisions

- [Pattern: API Error Handling](../patterns/api-error-handling.md) - Error response format
- Future: ADR for API versioning strategy (URL vs header vs content negotiation)

## Links

**References:**
- [REST API Tutorial](https://restfulapi.net/)
- [OpenAPI Specification](https://swagger.io/specification/)
- [Richardson Maturity Model](https://martinfowler.com/articles/richardsonMaturityModel.html)
- [Google API Design Guide](https://cloud.google.com/apis/design)

**Alternatives Considered:**
- [GraphQL](https://graphql.org/)
- [gRPC](https://grpc.io/)

---

**Review Date:** 2026-07-06 (6 months from acceptance)
**Superseded By:** None
**Supersedes:** None
