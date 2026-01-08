# Architecture Decision Records

## ADR-001: Use TypeScript for Type Safety

**Status**: Accepted

**Context**: We started with JavaScript but encountered too many runtime type errors.

**Decision**: Migrate to TypeScript for all new code. Gradually migrate existing code.

**Rationale**:
- Catches type errors at compile time
- Better IDE support and refactoring
- Self-documenting code

**Consequences**:
- Build step required
- Slightly slower development for simple scripts
- Team needs TypeScript knowledge

**Implementation**: Use strict tsconfig.json with no-implicit-any enabled.

---

## ADR-002: Monorepo Structure with Workspaces

**Status**: Accepted

**Context**: Growing number of related packages. Need to share code and maintain consistency.

**Decision**: Adopt monorepo structure using npm workspaces.

**Rationale**:
- Single repository for all related packages
- Shared types and utilities
- Consistent versioning and release process
- Easier to manage dependencies

**Consequences**:
- More complex build process
- Requires careful dependency management
- Larger git history

**Packages**:
- `@org/api` - REST API
- `@org/sdk` - TypeScript SDK
- `@org/cli` - CLI tools
- `@org/common` - Shared utilities

---

## ADR-003: Use PostgreSQL for Primary Database

**Status**: Accepted

**Context**: Need robust, ACID-compliant relational database for user data.

**Decision**: Use PostgreSQL 15+ as primary database.

**Rationale**:
- ACID compliance for data integrity
- Excellent JSON support
- Strong ecosystem
- Cost-effective

**Consequences**:
- Requires database administration
- Migration strategy needed if switching
- Team needs PostgreSQL expertise

**Backup Strategy**: Nightly snapshots to S3.

---

## ADR-004: REST API with OpenAPI Specification

**Status**: Accepted

**Context**: Multiple clients (web, mobile, CLI) need to consume API. Need clear contract.

**Decision**: Design RESTful API with OpenAPI 3.0 specification.

**Rationale**:
- Clear API contracts
- Auto-generated client SDKs
- Better documentation
- Industry standard

**Consequences**:
- Spec must be kept up-to-date
- Changes require API versioning strategy

**Pattern**: Use v1, v2 URL prefixes for breaking changes.

---

## ADR-005: JWT Tokens with 1-Hour Expiry

**Status**: Accepted

**Context**: Need stateless authentication that's secure and practical.

**Decision**: Use JWT tokens with 1-hour expiry and refresh tokens.

**Rationale**:
- Stateless authentication (no session store)
- Can use across multiple services
- Refresh tokens enable long-lived sessions without exposing main token

**Consequences**:
- Must implement token refresh logic
- Client must handle token expiry

**Constraints**:
- Always use HTTPS
- Store secret securely in environment variables
- Use HS256 or RS256 algorithm

---

## ADR-006: Docker for Deployment

**Status**: Accepted

**Context**: Need consistent development/production environments.

**Decision**: Containerize all services with Docker.

**Rationale**:
- Eliminates "works on my machine" problems
- Enables Kubernetes deployments
- Clear dependency specifications

**Consequences**:
- Requires Docker knowledge
- Slightly slower startup for development

**Images**: Built with multi-stage Dockerfile, optimized for size.

---

## ADR-007: GitHub Actions for CI/CD

**Status**: Accepted

**Context**: Need automated testing and deployment pipeline.

**Decision**: Use GitHub Actions as primary CI/CD platform.

**Rationale**:
- Native GitHub integration
- No additional infrastructure
- Generous free tier
- Good security model

**Consequences**:
- Locked into GitHub
- Limited customization vs self-hosted
- Need to learn GitHub Actions syntax

**Workflows**:
- Lint and test on PR
- Build and deploy on merge to main
- Nightly security scans

---

## ADR-008: Async Processing with Bull Queue

**Status**: Accepted

**Context**: Some operations take >10 seconds (email, image processing).

**Decision**: Use Bull queue for async job processing.

**Rationale**:
- Keeps HTTP responses fast
- Automatic retries on failure
- Visibility into job status
- Redis-backed for persistence

**Consequences**:
- Need to manage Redis instance
- Job tracking increases complexity
- Eventual consistency instead of immediate

**Job Types**:
- Email sending
- Image resizing
- Report generation
- Data exports
