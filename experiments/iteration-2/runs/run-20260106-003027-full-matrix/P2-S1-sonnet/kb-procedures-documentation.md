# Documentation Standards

## Required Documentation

### 1. README.md (Project Root)

Every project must have a README with:

```markdown
# Project Name

Brief description (1-2 sentences)

## Features

- Key feature 1
- Key feature 2
- Key feature 3

## Prerequisites

- Node.js >= 20
- PostgreSQL >= 15
- Redis (optional)

## Installation

\`\`\`bash
# Clone repository
git clone https://github.com/org/project.git

# Install dependencies
npm install

# Configure environment
cp .env.example .env

# Run database migrations
npm run migrate

# Start development server
npm run dev
\`\`\`

## Configuration

Environment variables:
- `DATABASE_URL` - PostgreSQL connection string
- `REDIS_URL` - Redis connection string (optional)
- `JWT_SECRET` - Secret for signing JWT tokens

## Usage

\`\`\`bash
# Development
npm run dev

# Production
npm run build
npm start

# Tests
npm test
npm run test:watch
npm run test:coverage
\`\`\`

## API Documentation

See [API.md](./docs/API.md) or Swagger UI at `/api/docs`

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md)

## License

MIT
```

### 2. API Documentation

Document all endpoints:

```markdown
# API Documentation

## Authentication

### POST /api/auth/login

Authenticate user and receive JWT token.

**Request:**
\`\`\`json
{
  "email": "user@example.com",
  "password": "password123"
}
\`\`\`

**Response:** 200 OK
\`\`\`json
{
  "accessToken": "eyJhbGc...",
  "refreshToken": "dGhpc...",
  "expiresIn": 3600,
  "user": {
    "id": 1,
    "email": "user@example.com",
    "name": "John Doe"
  }
}
\`\`\`

**Errors:**
- `400` - Invalid request data
- `401` - Invalid credentials
- `429` - Too many requests

**Rate Limit:** 5 requests per 15 minutes
```

### 3. Inline Code Documentation

```javascript
/**
 * Fetch user by ID with optional related data
 *
 * @param {number} userId - ID of user to fetch
 * @param {Object} options - Fetch options
 * @param {boolean} options.includeOrders - Include user's orders
 * @param {boolean} options.includeProfile - Include user profile
 * @returns {Promise<User>} User object
 * @throws {NotFoundError} If user doesn't exist
 * @throws {ValidationError} If userId is invalid
 *
 * @example
 * const user = await getUserById(123, { includeOrders: true });
 * console.log(user.orders); // Array of orders
 */
async function getUserById(userId, options = {}) {
  // Implementation
}
```

### 4. Architecture Documentation

```markdown
# Architecture Overview

## System Components

\`\`\`
┌─────────────┐
│   Client    │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  API Layer  │  Express.js routes and middleware
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Service   │  Business logic
│    Layer    │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│    Data     │  Database and caching
│    Layer    │
└─────────────┘
\`\`\`

## Key Design Decisions

### Why Express.js?
- Mature, well-documented
- Large ecosystem
- Team expertise

### Why PostgreSQL?
- ACID compliance
- JSON support
- Strong community

## Data Flow

1. Client sends HTTP request
2. Middleware validates and authenticates
3. Controller parses request
4. Service layer executes business logic
5. Data layer persists changes
6. Response sent back to client
```

## Documentation Guidelines

### Keep Documentation Close to Code

```
src/
  services/
    user.service.js       # Implementation
    user.service.test.js  # Tests
    user.service.md       # Detailed docs (if complex)
```

### Update Documentation with Code Changes

When changing code:
1. Update inline JSDoc comments
2. Update API documentation if endpoints changed
3. Update README if setup steps changed
4. Add changelog entry

### Use Examples

```markdown
## Example Usage

\`\`\`javascript
// Create new user
const user = await userService.createUser({
  email: 'new@example.com',
  name: 'New User',
  password: 'SecurePass123!'
});

// Fetch with related data
const userWithOrders = await userService.getUserById(user.id, {
  includeOrders: true
});

// Update user
await userService.updateUser(user.id, {
  name: 'Updated Name'
});
\`\`\`
```

### Document Common Gotchas

```markdown
## Common Issues

### Database Connection Errors

**Problem:** `Error: connect ECONNREFUSED`

**Solution:** Ensure PostgreSQL is running:
\`\`\`bash
pg_ctl status
# or
docker ps | grep postgres
\`\`\`

### JWT Token Expiration

**Problem:** `401 Unauthorized` after 1 hour

**Solution:** This is expected. Use refresh token to get new access token:
\`\`\`javascript
POST /api/auth/refresh
{ "refreshToken": "..." }
\`\`\`
```

## Changelog

Keep CHANGELOG.md up to date:

```markdown
# Changelog

All notable changes to this project will be documented in this file.

## [1.2.0] - 2026-01-08

### Added
- User password reset functionality
- Email verification for new accounts
- Rate limiting on authentication endpoints

### Changed
- Updated JWT token expiration from 24h to 1h
- Improved error messages for validation failures

### Fixed
- Memory leak in user service cache
- SQL injection vulnerability in search endpoint

### Deprecated
- `/api/v1/users/search` (use `/api/v2/search/users` instead)

## [1.1.0] - 2025-12-15

...
```

## Architecture Decision Records (ADRs)

Document important decisions:

```markdown
# ADR-001: Use PostgreSQL for Primary Database

## Status

Accepted

## Context

We need to choose a database for our application. Requirements:
- ACID compliance
- Support for complex queries
- JSON data type support
- Strong consistency

## Decision

Use PostgreSQL as primary database.

## Consequences

**Positive:**
- ACID guarantees
- Rich query capabilities
- JSON support for flexible schemas
- Large community and ecosystem

**Negative:**
- More complex than NoSQL for simple use cases
- Requires schema migrations
- Vertical scaling limits (mitigated by read replicas)

## Alternatives Considered

- **MongoDB**: Better for unstructured data, but weaker consistency
- **MySQL**: Similar to Postgres, but weaker JSON support
- **DynamoDB**: Serverless, but vendor lock-in and limited queries
```

## Comment Guidelines

### When to Comment

```javascript
// GOOD: Explain non-obvious behavior
// Use exponential backoff to avoid overwhelming API
// during high load situations (max 3 retries)
await retryWithBackoff(apiCall, { maxRetries: 3 });

// GOOD: Document business rules
// Per GDPR, user data must be deleted within 30 days
const retentionDays = 30;

// GOOD: Warn about side effects
// WARNING: This function modifies the input array in-place
function sortUsersInPlace(users) {
  users.sort((a, b) => a.name.localeCompare(b.name));
}

// BAD: State the obvious
// Increment counter by 1
counter++;

// BAD: Commented-out code
// const oldImplementation = () => { ... }

// BAD: Vague comments
// TODO: Fix this later
// HACK: This is a workaround
```

### Comment Format

```javascript
// Single-line comment (sentence case, no period for short phrases)

/**
 * Multi-line comment for functions
 *
 * Longer description goes here. Use full sentences.
 *
 * @param {string} name - Parameter description
 * @returns {Promise<User>} Return value description
 */
```

## OpenAPI/Swagger Documentation

```javascript
/**
 * @swagger
 * /api/users/{id}:
 *   get:
 *     summary: Get user by ID
 *     tags: [Users]
 *     security:
 *       - bearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: integer
 *         description: User ID
 *     responses:
 *       200:
 *         description: User object
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/User'
 *       404:
 *         description: User not found
 */
router.get('/users/:id', getUser);
```

---

**Key Principle**: Documentation should be maintained, not created once and forgotten.
