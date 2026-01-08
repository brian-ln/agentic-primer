---
title: RESTful API Design Pattern
category: pattern
tags: [api, rest, http, design]
created: 2026-01-06
status: active
---

# RESTful API Design Pattern

## Context

When building HTTP APIs in this codebase, we follow RESTful conventions to ensure consistency, predictability, and ease of integration.

## Principles

### 1. Resource-Based URLs

Use nouns for resources, not verbs:

```
✅ Good:
GET    /api/users           # List users
GET    /api/users/123       # Get specific user
POST   /api/users           # Create user
PUT    /api/users/123       # Update user
DELETE /api/users/123       # Delete user

❌ Bad:
GET    /api/getUsers
POST   /api/createUser
POST   /api/updateUser/123
```

### 2. HTTP Method Semantics

Use standard HTTP methods appropriately:

- **GET** - Retrieve data (safe, idempotent, cacheable)
- **POST** - Create new resource (not idempotent)
- **PUT** - Replace entire resource (idempotent)
- **PATCH** - Partial update (not idempotent in general)
- **DELETE** - Remove resource (idempotent)

### 3. Status Codes

Return meaningful HTTP status codes:

```javascript
// Success responses
200 OK              // Successful GET, PUT, PATCH, DELETE
201 Created         // Successful POST (include Location header)
204 No Content      // Successful DELETE with no body

// Client errors
400 Bad Request     // Invalid input/validation failure
401 Unauthorized    // Authentication required
403 Forbidden       // Authenticated but not authorized
404 Not Found       // Resource doesn't exist
409 Conflict        // State conflict (e.g., duplicate)

// Server errors
500 Internal Server Error  // Unexpected server error
503 Service Unavailable    // Temporary unavailability
```

### 4. Response Format

Use consistent JSON structure:

```javascript
// Success response
{
  "data": {
    "id": 123,
    "name": "Example",
    "createdAt": "2026-01-06T12:00:00Z"
  }
}

// List response
{
  "data": [
    { "id": 1, "name": "Item 1" },
    { "id": 2, "name": "Item 2" }
  ],
  "pagination": {
    "page": 1,
    "perPage": 20,
    "total": 42,
    "totalPages": 3
  }
}

// Error response
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid input data",
    "details": [
      {
        "field": "email",
        "message": "Must be a valid email address"
      }
    ]
  }
}
```

### 5. Versioning

Include API version in URL:

```
/api/v1/users
/api/v2/users
```

**Rationale:**
- URL versioning is explicit and easy to route
- Allows parallel operation of multiple versions
- Clear migration path for clients

### 6. Pagination

For list endpoints, support pagination:

```
GET /api/v1/users?page=1&perPage=20
GET /api/v1/users?offset=0&limit=20
```

Always include pagination metadata in response.

### 7. Filtering and Sorting

Support common query operations:

```
GET /api/v1/users?status=active
GET /api/v1/users?sort=createdAt:desc
GET /api/v1/users?status=active&sort=name:asc&page=1
```

### 8. Authentication

Use standard authentication headers:

```
Authorization: Bearer <token>
```

Never pass credentials in query parameters.

## Examples in This Codebase

### User Management API

```javascript
// routes/api/v1/users.js
const express = require('express');
const router = express.Router();
const { validateUser } = require('../middleware/validation');
const { authenticate } = require('../middleware/auth');

// List users
router.get('/', authenticate, async (req, res) => {
  const { page = 1, perPage = 20, status } = req.query;

  try {
    const users = await User.find({ status })
      .skip((page - 1) * perPage)
      .limit(perPage);

    const total = await User.countDocuments({ status });

    res.json({
      data: users,
      pagination: {
        page: parseInt(page),
        perPage: parseInt(perPage),
        total,
        totalPages: Math.ceil(total / perPage)
      }
    });
  } catch (error) {
    res.status(500).json({
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to fetch users'
      }
    });
  }
});

// Create user
router.post('/', authenticate, validateUser, async (req, res) => {
  try {
    const user = await User.create(req.body);

    res.status(201)
      .location(`/api/v1/users/${user.id}`)
      .json({ data: user });
  } catch (error) {
    if (error.code === 'DUPLICATE_KEY') {
      res.status(409).json({
        error: {
          code: 'DUPLICATE_EMAIL',
          message: 'Email already exists'
        }
      });
    } else {
      res.status(500).json({
        error: {
          code: 'INTERNAL_ERROR',
          message: 'Failed to create user'
        }
      });
    }
  }
});

// Get user
router.get('/:id', authenticate, async (req, res) => {
  try {
    const user = await User.findById(req.params.id);

    if (!user) {
      return res.status(404).json({
        error: {
          code: 'NOT_FOUND',
          message: 'User not found'
        }
      });
    }

    res.json({ data: user });
  } catch (error) {
    res.status(500).json({
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to fetch user'
      }
    });
  }
});

// Update user
router.put('/:id', authenticate, validateUser, async (req, res) => {
  try {
    const user = await User.findByIdAndUpdate(
      req.params.id,
      req.body,
      { new: true, runValidators: true }
    );

    if (!user) {
      return res.status(404).json({
        error: {
          code: 'NOT_FOUND',
          message: 'User not found'
        }
      });
    }

    res.json({ data: user });
  } catch (error) {
    res.status(500).json({
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to update user'
      }
    });
  }
});

// Delete user
router.delete('/:id', authenticate, async (req, res) => {
  try {
    const user = await User.findByIdAndDelete(req.params.id);

    if (!user) {
      return res.status(404).json({
        error: {
          code: 'NOT_FOUND',
          message: 'User not found'
        }
      });
    }

    res.status(204).send();
  } catch (error) {
    res.status(500).json({
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to delete user'
      }
    });
  }
});

module.exports = router;
```

## Tradeoffs

### Advantages

- **Predictable** - Standard conventions reduce cognitive load
- **Self-documenting** - URLs and methods convey intent
- **Cacheable** - HTTP semantics enable caching strategies
- **Tooling** - Works with standard HTTP clients and proxies

### Disadvantages

- **Chatty** - May require multiple round trips for complex operations
- **Rigid** - Resource-based URLs don't fit all use cases
- **Over-fetching** - May return more data than client needs

### When to Deviate

Consider GraphQL or RPC-style endpoints for:
- Complex queries requiring multiple resources
- Real-time subscriptions
- Mobile apps with limited bandwidth
- Heavy client-side orchestration

Always document deviations in **decisions/** with rationale.

## Related

- **Decision:** [Why we chose REST over GraphQL](../decisions/rest-over-graphql.md) *(example - create if needed)*
- **Insight:** [API error handling learnings](../insights/api-error-handling.md) *(example - create if needed)*

## References

- [Roy Fielding's REST dissertation](https://www.ics.uci.edu/~fielding/pubs/dissertation/rest_arch_style.htm)
- [RFC 7231 - HTTP Semantics](https://tools.ietf.org/html/rfc7231)
- [JSON API Specification](https://jsonapi.org/)

---

**Last Updated:** 2026-01-06
**Maintainer:** Team
