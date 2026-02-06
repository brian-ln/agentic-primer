# Pattern: RESTful API Design

## Problem

When designing API endpoints, we need consistency in naming conventions, HTTP methods, status codes, error handling, and response formats. Without documented patterns, different parts of the codebase may implement APIs inconsistently, making them harder to use and maintain.

## Solution

Follow RESTful conventions with clear guidelines for endpoint design, versioning, error handling, and documentation.

### Core Principles

1. **Resource-Oriented**: Design around nouns (resources), not verbs (actions)
2. **Standard HTTP Methods**: Use GET, POST, PUT, PATCH, DELETE correctly
3. **Consistent Status Codes**: Follow HTTP status code semantics
4. **Uniform Error Format**: All errors follow the same response structure
5. **Versioning**: API version in URL or headers (not body)
6. **Pagination**: For list endpoints returning multiple items
7. **Rate Limiting**: Communicate limits clearly via headers

### Endpoint Design

#### Naming Convention

```
GET    /api/v1/resources              # List all resources
GET    /api/v1/resources/{id}         # Get specific resource
POST   /api/v1/resources              # Create new resource
PUT    /api/v1/resources/{id}         # Replace entire resource
PATCH  /api/v1/resources/{id}         # Partial update
DELETE /api/v1/resources/{id}         # Delete resource
GET    /api/v1/resources/{id}/sub     # Sub-resource
```

#### HTTP Status Codes

| Code | Usage | Example |
|------|-------|---------|
| 200 | GET success, PATCH/PUT success | GET /api/v1/users/1 |
| 201 | POST success (resource created) | POST /api/v1/users |
| 204 | No content (DELETE success) | DELETE /api/v1/users/1 |
| 400 | Bad request (validation error) | Invalid query parameters |
| 401 | Unauthorized (auth required) | Missing auth header |
| 403 | Forbidden (no permission) | User can't access resource |
| 404 | Not found | Non-existent resource ID |
| 409 | Conflict (resource exists) | Duplicate unique key |
| 429 | Rate limited | Too many requests |
| 500 | Server error | Unexpected exception |

### Response Format

#### Success Response

```json
{
  "data": {
    "id": "resource-123",
    "name": "Example",
    "created_at": "2026-01-08T12:00:00Z",
    "updated_at": "2026-01-08T12:00:00Z"
  },
  "meta": {
    "timestamp": "2026-01-08T12:00:00Z"
  }
}
```

#### Error Response

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Request validation failed",
    "details": {
      "field_name": ["error message"]
    }
  },
  "meta": {
    "timestamp": "2026-01-08T12:00:00Z",
    "request_id": "req-123456"
  }
}
```

### Versioning

Use URL path versioning (preferred for clarity):

```
/api/v1/resources      # Version 1
/api/v2/resources      # Version 2 (breaking changes)
```

## When to Use

- ✓ Designing public APIs
- ✓ Internal microservice APIs
- ✓ Webhook receivers
- ✓ Any HTTP endpoint handling structured data

## Related Patterns

- **Error Handling**: Consistent error response format
- **Authentication**: Security patterns for API access
- **Caching**: HTTP cache headers for performance

---

**Last Updated:** 2026-01-08
**Version:** 1.0
**Status:** Approved
