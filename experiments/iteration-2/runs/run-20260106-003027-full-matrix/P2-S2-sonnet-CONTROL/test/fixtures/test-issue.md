# Test Issue: Implement User Authentication API

## Description

Create a RESTful API endpoint for user authentication that follows our established API design patterns.

## Requirements

### Functional Requirements

1. **Login endpoint**: `POST /api/v1/auth/login`
   - Accept email and password in request body
   - Return JWT token on successful authentication
   - Return appropriate error for invalid credentials

2. **Token validation**: `GET /api/v1/auth/verify`
   - Validate JWT token from Authorization header
   - Return user information if token is valid
   - Return 401 if token is invalid or expired

3. **Logout endpoint**: `POST /api/v1/auth/logout`
   - Invalidate user's current token
   - Return success confirmation

### Non-Functional Requirements

1. **Security**
   - Hash passwords using bcrypt (minimum 10 rounds)
   - Use secure JWT signing with RS256 algorithm
   - Implement rate limiting (5 login attempts per minute per IP)
   - Include CSRF protection

2. **Performance**
   - Login endpoint should respond in < 200ms (p95)
   - Token validation should respond in < 50ms (p95)

3. **Validation**
   - Email must be valid format
   - Password must be minimum 8 characters
   - Return clear validation error messages

## Acceptance Criteria

- [ ] All endpoints implemented according to API design pattern
- [ ] Uses proper HTTP status codes (200, 201, 400, 401, 403, 500)
- [ ] Returns consistent JSON response format
- [ ] Includes input validation with error details
- [ ] Password hashing implemented correctly
- [ ] JWT token generation and validation working
- [ ] Rate limiting implemented
- [ ] Unit tests cover all endpoints (>80% coverage)
- [ ] Integration tests verify end-to-end flows
- [ ] API documentation updated (OpenAPI/Swagger)
- [ ] Error handling follows established patterns

## Technical Guidance

Please consult the knowledge base for:
- RESTful API design patterns (`docs/knowledge/patterns/api-design.md`)
- Architectural decisions about authentication
- Security best practices and learnings

## Testing Instructions

1. **Manual Testing**
   ```bash
   # Valid login
   curl -X POST http://localhost:3000/api/v1/auth/login \
     -H "Content-Type: application/json" \
     -d '{"email":"test@example.com","password":"testpass123"}'

   # Should return: {"data": {"token": "eyJ..."}}

   # Invalid credentials
   curl -X POST http://localhost:3000/api/v1/auth/login \
     -H "Content-Type: application/json" \
     -d '{"email":"test@example.com","password":"wrongpassword"}'

   # Should return: {"error": {"code": "INVALID_CREDENTIALS", ...}}

   # Verify token
   curl -X GET http://localhost:3000/api/v1/auth/verify \
     -H "Authorization: Bearer <token>"

   # Should return: {"data": {"id": 1, "email": "test@example.com", ...}}
   ```

2. **Automated Testing**
   ```bash
   npm test -- auth.test.js
   npm run test:integration -- auth-flow.test.js
   ```

## Implementation Notes

- Use existing user model from `models/User.js`
- JWT secret should be loaded from environment variable `JWT_SECRET`
- Rate limiting should use Redis if available, in-memory store otherwise
- Follow error response format from API design pattern

## Dependencies

- `jsonwebtoken` - For JWT generation and verification
- `bcrypt` - For password hashing
- `express-rate-limit` - For rate limiting
- `joi` or `express-validator` - For input validation

## Timeline

**Estimated effort:** 4-6 hours
**Priority:** High
**Labels:** `copilot-task`, `feature`, `api`, `authentication`

## Related Issues

- None (first authentication implementation)

## Questions?

If implementation details are unclear, please:
1. Check the knowledge base (`docs/knowledge/`)
2. Review existing API endpoints for patterns
3. Comment on this issue for clarification

---

**Created by:** Test User (@testuser)
**Date:** 2026-01-06
**Epic:** User Management System
