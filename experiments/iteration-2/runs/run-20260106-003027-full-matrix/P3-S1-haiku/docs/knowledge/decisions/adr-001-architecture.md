# ADR-001: Monorepo vs Polyrepo Architecture

## Status
Accepted

## Context

The project grew from a single service to multiple interconnected services. The team needed to decide between:
1. **Monorepo**: Single repository containing all services and shared code
2. **Polyrepo**: Separate repositories per service with shared libraries

This decision significantly impacts development velocity, code sharing, and deployment complexity.

### Driving Factors
- Multiple services share domain models and utilities
- Need for coordinated releases across services
- Team size and code review processes
- CI/CD pipeline complexity
- Developer onboarding time

## Decision

Adopt a **monorepo** structure with the following characteristics:

- Single Git repository containing all services and libraries
- Workspace structure using npm/yarn workspaces
- Shared code in `packages/` directory
- Services in `services/` directory
- Clear dependency boundaries with workspace separation

## Consequences

### Positive
- **Code reuse**: Shared libraries easily imported without versioning friction
- **Atomic commits**: Related changes across services in single commit
- **Unified testing**: Run all tests together for integration verification
- **Simplified dependencies**: No circular dependency issues across repos
- **Easier refactoring**: Move code across services without repository migration

### Negative
- **Repository size**: Larger clone/checkout times
- **CI/CD complexity**: Must carefully scope which tests/builds run on changes
- **Access control**: Cannot grant access to subset of code without repo-wide access
- **Deployment coupling**: Must be careful not to inadvertently couple service deployments

### Neutral
- **Tool support**: Requires workspace-aware tooling (yarn/npm/pnpm)
- **Dependency management**: More explicit than polyrepo but less friction than publishing

## Alternatives Considered

### Polyrepo with Private NPM Registry
- **Pros**: Clear boundaries, independent CI/CD, granular access control
- **Cons**: Versioning overhead, slower iteration on shared code, increased operational complexity
- **Rejected because**: Team prioritizes development velocity and ease of refactoring

### Hybrid Approach
- **Concept**: Monorepo for service code, separate repos for libraries
- **Pros**: Combines benefits of both
- **Cons**: Complex synchronization, versioning mismatch risk
- **Rejected because**: Added complexity outweighs benefits for current team size

## Implementation Details

### Directory Structure
```
/
├── services/
│   ├── api/
│   ├── auth/
│   └── worker/
├── packages/
│   ├── shared-types/
│   ├── logger/
│   └── database/
├── package.json (root workspace)
└── turbo.json (build orchestration)
```

### Workspace Configuration
```json
{
  "workspaces": [
    "services/*",
    "packages/*"
  ]
}
```

### CI/CD Implications
- Use tools like Turborepo to cache builds and tests
- Run only affected tests when changes are detected
- Maintain separate deployment pipelines per service
- Use version tags to trigger service-specific deployments

## Related Decisions
- None yet (this is the foundational architecture decision)

## Revision History
- **2024-01-06**: Initial acceptance by architecture team

---

# ADR-002: TypeScript Over JavaScript

## Status
Accepted

## Context

The project needed a language choice that balances developer productivity, type safety, and team skill levels.

## Decision

Use **TypeScript** for all new code, with gradual migration of existing JavaScript code.

## Consequences

### Positive
- **Type safety**: Catch errors at compile time
- **IDE support**: Better autocomplete and refactoring tools
- **Documentation**: Types serve as inline documentation
- **Maintainability**: Easier to refactor and understand large codebases
- **Team skill**: Patterns and practices reduce cognitive load

### Negative
- **Build step required**: Adds compilation overhead
- **Learning curve**: Team members need TypeScript knowledge
- **Verbosity**: More typing for complex types

## Trade-offs

- **Strictness**: Use `strict: true` in tsconfig for maximum safety
- **Any type**: Minimize `any` through gradual typing
- **Type definitions**: Maintain DefinitelyTyped or custom types for external libraries

## Alternatives Considered

### JavaScript with JSDoc
- **Pros**: No build step, familiar to all developers
- **Cons**: Type checking less strict, IDE support weaker
- **Rejected because**: TypeScript provides better tooling and safety

### Flow
- **Pros**: Incremental typing, optional
- **Cons**: Smaller ecosystem, less team familiarity
- **Rejected because**: TypeScript has larger community and better integration

## Related Decisions
- ADR-001 relies on workspace tooling that works well with TypeScript

---

# ADR-003: Testing Strategy - Unit + Integration

## Status
Accepted

## Context

The project needs testing strategy that ensures reliability without slowing development cycles.

## Decision

Implement **two-tier testing**: unit tests for business logic and integration tests for workflows.

### Test Coverage Targets
- Unit tests: Critical path logic (>80% coverage)
- Integration tests: Happy paths and error scenarios
- E2E tests: Critical user journeys (via separate test environment)

### Test Technology Stack
- **Unit tests**: Jest
- **Integration tests**: Jest with test containers for database/services
- **E2E tests**: Cypress (minimal set)

## Consequences

### Positive
- **Fast feedback**: Unit tests run in <100ms
- **Reliability**: Integration tests catch real-world issues
- **Maintainability**: Clear separation of concerns
- **Cost-effective**: Minimal E2E tests reduce CI/CD costs

### Negative
- **Setup overhead**: Integration tests need test database
- **Maintenance burden**: More tests to maintain
- **Flakiness risk**: Test isolation harder in integration tests

## Implementation Patterns

### Unit Test Structure
```typescript
describe('UserService.createUser', () => {
  it('should hash password before storing', async () => {
    // Arrange
    const hashFn = jest.fn().mockResolvedValue('hashed');

    // Act
    await service.createUser({ password: 'secret' }, { hash: hashFn });

    // Assert
    expect(hashFn).toHaveBeenCalledWith('secret');
  });
});
```

### Integration Test Structure
```typescript
describe('POST /users (integration)', () => {
  let db: TestDatabase;

  beforeAll(async () => {
    db = await TestDatabase.create();
  });

  it('should create user in database', async () => {
    const response = await request(app)
      .post('/users')
      .send({ email: 'test@example.com' })
      .expect(201);

    const user = await db.query('SELECT * FROM users WHERE email = ?', ['test@example.com']);
    expect(user).toBeDefined();
  });
});
```

## Related Decisions
- ADR-002: TypeScript required for type-safe test code

---

# ADR-004: Logging and Observability

## Status
Accepted

## Context

As the system grows, we need consistent logging and observability to diagnose issues in production.

## Decision

Implement **structured logging** with correlation IDs for request tracing.

### Logging Standards
- **Format**: JSON with structured fields
- **Levels**: debug, info, warn, error
- **Request tracing**: All logs for a request share a correlation ID
- **Context**: Include user ID, resource IDs, and relevant business context

### Tools
- **Logger**: winston or pino
- **Tracing**: Request middleware adds correlation ID
- **Storage**: JSON logs to stdout for container/cloud platforms

## Code Example

```typescript
// Middleware
app.use((req, res, next) => {
  req.id = crypto.randomUUID();
  logger.info('request_started', {
    requestId: req.id,
    method: req.method,
    path: req.path,
    userId: req.user?.id,
  });
  next();
});

// In handlers
logger.info('user_created', {
  requestId: req.id,
  userId: newUser.id,
  email: newUser.email,
  timestamp: new Date().toISOString(),
});
```

## Consequences

### Positive
- **Debuggability**: Easy to trace request flow
- **Monitoring**: Structured logs enable dashboarding
- **Alerting**: Can trigger alerts on specific log patterns
- **Compliance**: Audit trails for regulatory requirements

### Negative
- **Log volume**: JSON logging increases storage needs
- **Privacy**: Must carefully exclude PII from logs

## Related Decisions
- Uses standard logging library integrable with APM tools (relates to ADR-002 TypeScript choice)
