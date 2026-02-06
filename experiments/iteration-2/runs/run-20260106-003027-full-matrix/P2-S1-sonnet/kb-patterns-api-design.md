# API Design Patterns

## REST API Design

### Resource Naming

Use nouns (not verbs) for resources:

```
✅ GOOD
GET    /api/users
GET    /api/users/:id
POST   /api/users
PUT    /api/users/:id
DELETE /api/users/:id

❌ BAD
GET    /api/getUsers
POST   /api/createUser
```

### HTTP Methods

| Method | Purpose | Idempotent | Request Body | Response |
|--------|---------|------------|--------------|----------|
| GET | Retrieve resource(s) | Yes | No | Resource(s) |
| POST | Create new resource | No | Yes | Created resource + Location header |
| PUT | Replace entire resource | Yes | Yes | Updated resource |
| PATCH | Partial update | No | Yes | Updated resource |
| DELETE | Remove resource | Yes | No | 204 No Content |

### Status Codes

```javascript
// Success
200 OK               // Successful GET, PUT, PATCH
201 Created          // Successful POST
204 No Content       // Successful DELETE

// Client Errors
400 Bad Request      // Invalid request data
401 Unauthorized     // Missing or invalid authentication
403 Forbidden        // Authenticated but not authorized
404 Not Found        // Resource doesn't exist
409 Conflict         // Resource conflict (duplicate)
422 Unprocessable    // Validation failed

// Server Errors
500 Internal Error   // Unexpected server error
503 Service Unavail  // Temporary service issue
```

### Request/Response Format

```javascript
// POST /api/users
// Request
{
  "email": "user@example.com",
  "name": "John Doe",
  "role": "editor"
}

// Response: 201 Created
{
  "id": 123,
  "email": "user@example.com",
  "name": "John Doe",
  "role": "editor",
  "createdAt": "2026-01-08T10:00:00Z",
  "updatedAt": "2026-01-08T10:00:00Z"
}
```

### Error Response Format

```javascript
// Consistent error structure
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid request data",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format"
      },
      {
        "field": "name",
        "message": "Name is required"
      }
    ],
    "requestId": "req_abc123",
    "timestamp": "2026-01-08T10:00:00Z"
  }
}
```

### Pagination

```javascript
// Request
GET /api/users?page=2&limit=20&sort=-createdAt

// Response
{
  "data": [ /* user objects */ ],
  "pagination": {
    "page": 2,
    "limit": 20,
    "total": 143,
    "totalPages": 8,
    "hasNext": true,
    "hasPrev": true
  }
}
```

### Filtering and Searching

```javascript
// Filter by fields
GET /api/users?role=admin&status=active

// Search
GET /api/users?search=john&fields=name,email

// Combined
GET /api/users?role=admin&search=john&sort=name&page=1&limit=10
```

## Authentication Patterns

### JWT Bearer Token

```javascript
// Request
GET /api/users/me
Authorization: Bearer eyJhbGciOiJIUzI1NiIs...

// Middleware
async function authenticate(req, res, next) {
  const token = req.headers.authorization?.replace('Bearer ', '');

  if (!token) {
    return res.status(401).json({
      error: { message: 'Authentication required' }
    });
  }

  try {
    const decoded = jwt.verify(token, process.env.JWT_SECRET);
    req.user = await getUserById(decoded.userId);
    next();
  } catch (error) {
    return res.status(401).json({
      error: { message: 'Invalid or expired token' }
    });
  }
}
```

### Refresh Token Pattern

```javascript
// POST /api/auth/login
{
  "email": "user@example.com",
  "password": "password123"
}

// Response
{
  "accessToken": "eyJhbGc...",  // Expires in 1 hour
  "refreshToken": "dGhpcyBp...", // Expires in 30 days
  "expiresIn": 3600,
  "tokenType": "Bearer"
}

// POST /api/auth/refresh
{
  "refreshToken": "dGhpcyBp..."
}

// Response: New access token
{
  "accessToken": "eyJhbGc...",
  "expiresIn": 3600
}
```

## Rate Limiting

```javascript
// Response headers
HTTP/1.1 200 OK
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1641024000

// When limit exceeded
HTTP/1.1 429 Too Many Requests
Retry-After: 60

{
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "Too many requests. Try again in 60 seconds."
  }
}
```

## Versioning

### URL Versioning (Recommended)

```
/api/v1/users
/api/v2/users
```

### Header Versioning (Alternative)

```
GET /api/users
Accept: application/vnd.api+json; version=1
```

## Validation Pattern

```javascript
const Joi = require('joi');

// Define schema
const createUserSchema = Joi.object({
  email: Joi.string().email().required(),
  name: Joi.string().min(2).max(100).required(),
  role: Joi.string().valid('admin', 'editor', 'viewer').default('viewer'),
  age: Joi.number().integer().min(18).optional()
});

// Validation middleware
function validate(schema) {
  return (req, res, next) => {
    const { error, value } = schema.validate(req.body, {
      abortEarly: false,
      stripUnknown: true
    });

    if (error) {
      return res.status(422).json({
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Invalid request data',
          details: error.details.map(d => ({
            field: d.path.join('.'),
            message: d.message
          }))
        }
      });
    }

    req.body = value;  // Use validated/sanitized data
    next();
  };
}

// Usage
router.post('/users', validate(createUserSchema), createUser);
```

## CORS Configuration

```javascript
const cors = require('cors');

// Development: Allow all
app.use(cors());

// Production: Specific origins
app.use(cors({
  origin: ['https://app.example.com', 'https://admin.example.com'],
  credentials: true,
  maxAge: 86400
}));
```

## API Documentation

Use OpenAPI/Swagger:

```javascript
/**
 * @swagger
 * /api/users:
 *   get:
 *     summary: List all users
 *     tags: [Users]
 *     parameters:
 *       - in: query
 *         name: page
 *         schema:
 *           type: integer
 *         description: Page number
 *     responses:
 *       200:
 *         description: List of users
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 data:
 *                   type: array
 *                   items:
 *                     $ref: '#/components/schemas/User'
 */
```

---

**Key Principle**: APIs should be intuitive, consistent, and well-documented.
