---
title: RESTful API Design Pattern
category: pattern
tags: [api, rest, http, conventions]
created: 2026-01-08
---

# RESTful API Design Pattern

## Context

When building HTTP APIs that need to be predictable, discoverable, and maintainable, following RESTful conventions makes it easier for clients and developers to understand and use the API correctly. This pattern defines our standard approach to API design.

## Solution

Apply these principles when designing API endpoints:

### Resource-Oriented URLs

Use nouns for resources, not verbs:

```
✅ Good
GET    /api/v1/issues
GET    /api/v1/issues/123
POST   /api/v1/issues
PUT    /api/v1/issues/123
DELETE /api/v1/issues/123

❌ Bad
GET    /api/v1/get-issue?id=123
POST   /api/v1/create-issue
PUT    /api/v1/update-issue/123
DELETE /api/v1/delete-issue/123
```

### HTTP Methods

Use standard HTTP methods for standard operations:

- **GET** - Retrieve resource(s), read-only, safe, idempotent
- **POST** - Create new resource, not idempotent
- **PUT** - Replace entire resource, idempotent
- **PATCH** - Partial update, idempotent
- **DELETE** - Remove resource, idempotent

### Status Codes

Return appropriate HTTP status codes:

- **200 OK** - Request succeeded, response includes data
- **201 Created** - Resource successfully created, include Location header
- **204 No Content** - Request succeeded, no content to return
- **400 Bad Request** - Client error (validation, parsing)
- **401 Unauthorized** - Authentication required
- **403 Forbidden** - Authenticated but not authorized
- **404 Not Found** - Resource doesn't exist
- **409 Conflict** - Request conflicts with current state
- **500 Internal Server Error** - Unexpected server error

### Response Format

Use consistent JSON structure:

```json
{
  "success": true,
  "data": {
    "id": "123",
    "title": "Example",
    "created_at": "2026-01-08T10:00:00Z"
  },
  "errors": null,
  "meta": {
    "request_id": "req-abc123",
    "timestamp": "2026-01-08T10:00:00Z"
  }
}
```

Error response:

```json
{
  "success": false,
  "data": null,
  "errors": [
    {
      "code": "VALIDATION_ERROR",
      "message": "Title is required",
      "field": "title"
    }
  ],
  "meta": {
    "request_id": "req-abc123",
    "timestamp": "2026-01-08T10:00:00Z"
  }
}
```

### Pagination

For list endpoints returning many items:

```
GET /api/v1/issues?page=1&per_page=20&sort=created_at&order=desc

Response:
{
  "success": true,
  "data": [...],
  "meta": {
    "pagination": {
      "page": 1,
      "per_page": 20,
      "total_items": 150,
      "total_pages": 8
    }
  }
}
```

### Versioning

Use URL-based versioning:

```
GET /api/v1/issues      # Current version
GET /api/v2/issues      # Future version
```

## Examples

### Creating an Issue

```bash
POST /api/v1/issues
Content-Type: application/json

{
  "title": "Implement copilot automation",
  "description": "Set up GitHub Actions workflow",
  "priority": "high"
}

Response: 201 Created
Location: /api/v1/issues/42
{
  "success": true,
  "data": {
    "id": "42",
    "title": "Implement copilot automation",
    "status": "open",
    "created_at": "2026-01-08T10:00:00Z"
  }
}
```

### Listing Issues

```bash
GET /api/v1/issues?status=open&sort=created_at&order=desc

Response: 200 OK
{
  "success": true,
  "data": [
    {"id": "42", "title": "Issue 1", ...},
    {"id": "41", "title": "Issue 2", ...}
  ],
  "meta": {
    "pagination": {
      "page": 1,
      "per_page": 20,
      "total_items": 2,
      "total_pages": 1
    }
  }
}
```

## Tradeoffs

### Advantages

- **Predictability**: Clients can guess endpoints from resource names
- **Discoverability**: Standard methods and status codes are widely known
- **Tooling**: Works well with standard HTTP tools (curl, Postman, etc.)
- **Caching**: GET requests can be cached naturally
- **Documentation**: Standard patterns need less explanation

### Disadvantages

- **Efficiency**: May require multiple requests for related resources
- **Complexity**: Some operations don't map cleanly to CRUD
- **Versioning**: API changes require planning
- **Filtering**: Complex query parameters for advanced filtering

### When to Deviate

- **Complex operations**: Consider RPC-style for operations that don't fit CRUD
- **Real-time**: WebSockets or Server-Sent Events for streaming
- **Batch operations**: POST with multiple resources in body
- **Graph-heavy**: Consider GraphQL for complex relationships

---

**Last Updated:** 2026-01-08
**Status:** Active
**Owner:** Team
