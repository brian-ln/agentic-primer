# Security Review Requirements

## Security Checklist

All code changes must pass this security review before deployment.

### Authentication & Authorization

- [ ] All endpoints require authentication (except public endpoints)
- [ ] Authorization checks use principle of least privilege
- [ ] JWT tokens have reasonable expiration times (< 1 hour)
- [ ] Refresh tokens are rotated and invalidated on logout
- [ ] Password hashing uses bcrypt with cost factor ≥ 12
- [ ] Rate limiting applied to authentication endpoints
- [ ] Multi-factor authentication supported for sensitive operations

### Input Validation

- [ ] All user input is validated (type, format, range)
- [ ] SQL injection prevented (use parameterized queries)
- [ ] XSS prevented (escape output, Content-Security-Policy header)
- [ ] Command injection prevented (never pass user input to shell)
- [ ] Path traversal prevented (validate file paths)
- [ ] File upload restrictions (type, size, sanitization)

### Data Protection

- [ ] Sensitive data encrypted at rest
- [ ] Sensitive data encrypted in transit (HTTPS only)
- [ ] Database connection strings use SSL/TLS
- [ ] Passwords never logged or returned in API responses
- [ ] PII (Personal Identifiable Information) handled according to policy
- [ ] Secrets stored in environment variables or secret manager
- [ ] No hardcoded credentials in code

### API Security

- [ ] CORS configured for specific origins (not *)
- [ ] Rate limiting prevents abuse
- [ ] Request size limits enforced
- [ ] API versioning implemented
- [ ] Error messages don't leak sensitive information
- [ ] CSRF protection for state-changing operations
- [ ] Security headers configured (HSTS, X-Frame-Options, etc.)

### Dependencies

- [ ] No known vulnerabilities in dependencies (npm audit)
- [ ] Dependencies from trusted sources only
- [ ] Package-lock.json committed
- [ ] Regular dependency updates scheduled
- [ ] Unused dependencies removed

## Common Vulnerabilities

### SQL Injection

```javascript
// ❌ VULNERABLE
const userId = req.params.id;
const query = `SELECT * FROM users WHERE id = ${userId}`;
db.raw(query);  // SQL injection possible!

// ✅ SAFE
const userId = req.params.id;
const user = await db('users').where({ id: userId }).first();
```

### XSS (Cross-Site Scripting)

```javascript
// ❌ VULNERABLE
app.get('/search', (req, res) => {
  const query = req.query.q;
  res.send(`<h1>Results for: ${query}</h1>`);  // XSS!
});

// ✅ SAFE
const escapeHtml = require('escape-html');

app.get('/search', (req, res) => {
  const query = req.query.q;
  res.send(`<h1>Results for: ${escapeHtml(query)}</h1>`);
});

// Or use templating engine with auto-escaping
res.render('search', { query });
```

### Command Injection

```javascript
// ❌ VULNERABLE
const filename = req.params.filename;
exec(`cat ${filename}`, callback);  // Command injection!

// ✅ SAFE
const { readFile } = require('fs/promises');
const filename = path.basename(req.params.filename);  // Sanitize
const filepath = path.join('/safe/directory', filename);
const content = await readFile(filepath, 'utf8');
```

### Path Traversal

```javascript
// ❌ VULNERABLE
const file = req.params.file;
res.sendFile(`/uploads/${file}`);  // Can access ../../../etc/passwd

// ✅ SAFE
const path = require('path');
const file = path.basename(req.params.file);  // Remove path
const safePath = path.join(__dirname, 'uploads', file);

// Verify path is within uploads directory
if (!safePath.startsWith(path.join(__dirname, 'uploads'))) {
  throw new ForbiddenError('Invalid file path');
}

res.sendFile(safePath);
```

### Insecure Direct Object Reference (IDOR)

```javascript
// ❌ VULNERABLE
app.get('/api/orders/:id', async (req, res) => {
  const order = await db('orders').where({ id: req.params.id }).first();
  res.json(order);  // Any user can access any order!
});

// ✅ SAFE
app.get('/api/orders/:id', authenticate, async (req, res) => {
  const order = await db('orders')
    .where({ id: req.params.id, user_id: req.user.id })  // Verify ownership
    .first();

  if (!order) {
    throw new NotFoundError('Order', req.params.id);
  }

  res.json(order);
});
```

## Password Security

### Hashing

```javascript
const bcrypt = require('bcrypt');

// Hash password (cost factor 12)
async function hashPassword(password) {
  return await bcrypt.hash(password, 12);
}

// Verify password
async function verifyPassword(password, hash) {
  return await bcrypt.compare(password, hash);
}

// Password requirements
function validatePassword(password) {
  if (password.length < 12) {
    throw new ValidationError('Password must be at least 12 characters');
  }

  if (!/[A-Z]/.test(password)) {
    throw new ValidationError('Password must contain uppercase letter');
  }

  if (!/[a-z]/.test(password)) {
    throw new ValidationError('Password must contain lowercase letter');
  }

  if (!/[0-9]/.test(password)) {
    throw new ValidationError('Password must contain number');
  }

  if (!/[^A-Za-z0-9]/.test(password)) {
    throw new ValidationError('Password must contain special character');
  }
}
```

### Password Reset Flow

```javascript
// 1. Generate secure reset token
const crypto = require('crypto');
const resetToken = crypto.randomBytes(32).toString('hex');
const resetTokenHash = crypto
  .createHash('sha256')
  .update(resetToken)
  .digest('hex');

// 2. Store hash in database with expiration
await db('password_resets').insert({
  email: user.email,
  token: resetTokenHash,
  expires_at: new Date(Date.now() + 3600000)  // 1 hour
});

// 3. Send token via email (one-time use only)
await sendEmail(user.email, 'Password Reset', {
  resetUrl: `https://example.com/reset?token=${resetToken}`
});

// 4. Verify and use token
const tokenHash = crypto
  .createHash('sha256')
  .update(submittedToken)
  .digest('hex');

const reset = await db('password_resets')
  .where({ token: tokenHash })
  .where('expires_at', '>', new Date())
  .first();

if (!reset) {
  throw new UnauthorizedError('Invalid or expired reset token');
}

// 5. Update password and invalidate token
await db.transaction(async (trx) => {
  await trx('users')
    .where({ email: reset.email })
    .update({ password_hash: await hashPassword(newPassword) });

  await trx('password_resets').where({ token: tokenHash }).del();
});
```

## JWT Security

```javascript
const jwt = require('jsonwebtoken');

// Sign token (short expiration)
function createAccessToken(user) {
  return jwt.sign(
    { userId: user.id, role: user.role },
    process.env.JWT_SECRET,
    { expiresIn: '1h', algorithm: 'HS256' }
  );
}

// Create refresh token (longer expiration, stored in database)
function createRefreshToken(user) {
  const token = jwt.sign(
    { userId: user.id, type: 'refresh' },
    process.env.JWT_REFRESH_SECRET,
    { expiresIn: '30d', algorithm: 'HS256' }
  );

  // Store in database for revocation
  await db('refresh_tokens').insert({
    user_id: user.id,
    token_hash: hashToken(token),
    expires_at: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000)
  });

  return token;
}

// Verify token
function verifyAccessToken(token) {
  try {
    return jwt.verify(token, process.env.JWT_SECRET);
  } catch (error) {
    throw new UnauthorizedError('Invalid or expired token');
  }
}
```

## Rate Limiting

```javascript
const rateLimit = require('express-rate-limit');

// General API rate limit
const apiLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,  // 15 minutes
  max: 100,  // 100 requests per window
  message: 'Too many requests, please try again later',
  standardHeaders: true,
  legacyHeaders: false
});

app.use('/api/', apiLimiter);

// Strict limit for authentication endpoints
const authLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 5,  // 5 attempts per 15 minutes
  skipSuccessfulRequests: true  // Only count failed attempts
});

app.use('/api/auth/login', authLimiter);
app.use('/api/auth/register', authLimiter);
```

## Security Headers

```javascript
const helmet = require('helmet');

app.use(helmet({
  contentSecurityPolicy: {
    directives: {
      defaultSrc: ["'self'"],
      styleSrc: ["'self'", "'unsafe-inline'"],
      scriptSrc: ["'self'"],
      imgSrc: ["'self'", 'data:', 'https:'],
    }
  },
  hsts: {
    maxAge: 31536000,  // 1 year
    includeSubDomains: true,
    preload: true
  }
}));

// Additional headers
app.use((req, res, next) => {
  res.setHeader('X-Content-Type-Options', 'nosniff');
  res.setHeader('X-Frame-Options', 'DENY');
  res.setHeader('X-XSS-Protection', '1; mode=block');
  next();
});
```

## Audit Logging

```javascript
// Log all security-relevant events
function logSecurityEvent(event, user, details) {
  logger.info('Security event', {
    event,
    userId: user?.id,
    userEmail: user?.email,
    ipAddress: details.ip,
    userAgent: details.userAgent,
    timestamp: new Date(),
    ...details
  });

  // Store in audit log table
  db('audit_log').insert({
    event,
    user_id: user?.id,
    ip_address: details.ip,
    details: JSON.stringify(details)
  });
}

// Log authentication events
app.post('/api/auth/login', async (req, res) => {
  const { email, password } = req.body;

  try {
    const user = await authenticate(email, password);
    logSecurityEvent('login_success', user, { ip: req.ip });
    res.json({ token: createAccessToken(user) });
  } catch (error) {
    logSecurityEvent('login_failure', null, {
      email,
      ip: req.ip,
      reason: error.message
    });
    throw error;
  }
});
```

## Dependency Security

```bash
# Check for vulnerabilities
npm audit

# Fix vulnerabilities
npm audit fix

# For unfixable vulnerabilities, assess risk
npm audit --json | jq '.vulnerabilities'

# Regular updates
npm outdated
npm update
```

---

**Remember**: Security is not optional. When in doubt, consult security team.
