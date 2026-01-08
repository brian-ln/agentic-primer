# Deployment Procedures

## Pre-Deployment Checklist

- [ ] All tests passing (unit, integration, e2e)
- [ ] Code review approved
- [ ] Security scan completed
- [ ] Database migrations tested
- [ ] Environment variables configured
- [ ] Documentation updated
- [ ] Changelog updated
- [ ] Rollback plan prepared

## Deployment Environments

### Development
- Auto-deploy on push to `develop` branch
- Used for integration testing
- Connects to dev database

### Staging
- Manual deploy from `main` branch
- Production-like environment
- Final testing before production
- Connects to staging database (copy of prod)

### Production
- Manual deploy with approval
- Requires tagged release
- Zero-downtime deployment
- Connects to production database

## Deployment Process

### 1. Prepare Release

```bash
# Ensure on main branch
git checkout main
git pull origin main

# Run final tests
npm test
npm run lint

# Create release tag
git tag -a v1.2.0 -m "Release v1.2.0"
git push origin v1.2.0
```

### 2. Database Migrations

```bash
# Staging: Test migrations
npm run migrate:staging

# Verify migration succeeded
npm run migrate:status

# Production: Run migrations
npm run migrate:production

# If migration fails, rollback
npm run migrate:rollback
```

### 3. Deploy Application

```bash
# Build production assets
npm run build

# Deploy to staging first
npm run deploy:staging

# Smoke test staging
npm run test:smoke -- --env=staging

# Deploy to production
npm run deploy:production

# Smoke test production
npm run test:smoke -- --env=production
```

### 4. Post-Deployment Verification

```bash
# Check application health
curl https://api.example.com/health

# Check error rates in monitoring
npm run check:errors

# Verify key features
npm run test:critical-path -- --env=production
```

### 5. Monitoring

First 15 minutes after deployment:
- Watch error rates
- Monitor response times
- Check database connections
- Verify logs for exceptions

First 24 hours:
- Monitor user reports
- Check performance metrics
- Review error logs
- Track business metrics

## Zero-Downtime Deployment

### Blue-Green Deployment

```yaml
# 1. Deploy new version (green) alongside current (blue)
# 2. Run health checks on green
# 3. Switch traffic from blue to green
# 4. Keep blue running for quick rollback
# 5. After verification, shutdown blue

steps:
  - name: Deploy green
    run: |
      docker-compose -f docker-compose.green.yml up -d

  - name: Health check
    run: |
      ./scripts/wait-for-healthy.sh green

  - name: Switch traffic
    run: |
      ./scripts/switch-load-balancer.sh blue green

  - name: Verify
    run: |
      ./scripts/smoke-test.sh

  - name: Cleanup old
    run: |
      docker-compose -f docker-compose.blue.yml down
```

### Rolling Deployment

```yaml
# Update instances one at a time
# Ensures some instances always serving traffic

strategy:
  type: RollingUpdate
  maxUnavailable: 1
  maxSurge: 1
```

## Rollback Procedures

### Immediate Rollback (Critical Issue)

```bash
# 1. Revert to previous version
git revert HEAD
git push origin main

# 2. Deploy previous version
npm run deploy:production -- --version=v1.1.0

# 3. Rollback database if needed
npm run migrate:rollback

# 4. Notify team
./scripts/send-incident-alert.sh "Rollback initiated"
```

### Planned Rollback

```bash
# 1. Announce maintenance window
# 2. Stop accepting new requests
# 3. Drain existing requests
# 4. Rollback application
# 5. Rollback database
# 6. Verify system state
# 7. Resume traffic
```

## Hotfix Process

For critical production issues:

```bash
# 1. Create hotfix branch from main
git checkout -b hotfix/critical-security-fix main

# 2. Implement fix
# Edit files...
git commit -m "hotfix: patch security vulnerability"

# 3. Test thoroughly
npm test

# 4. Merge to main
git checkout main
git merge --no-ff hotfix/critical-security-fix

# 5. Tag and deploy immediately
git tag -a v1.1.1 -m "Hotfix v1.1.1"
npm run deploy:production

# 6. Merge back to develop
git checkout develop
git merge --no-ff hotfix/critical-security-fix
```

## Database Migration Best Practices

### Safe Migrations

```javascript
// SAFE: Add column with default
exports.up = function(knex) {
  return knex.schema.table('users', (table) => {
    table.string('phone_number').defaultTo(null);
  });
};

// UNSAFE: Remove column (can't rollback if data lost)
exports.up = function(knex) {
  return knex.schema.table('users', (table) => {
    table.dropColumn('old_field');  // Data lost forever!
  });
};

// SAFER: Two-phase column removal
// Migration 1: Stop using column in code
// Migration 2 (later): Remove column
```

### Migration Testing

```bash
# 1. Apply migration
npm run migrate:up

# 2. Test application works
npm test

# 3. Rollback
npm run migrate:down

# 4. Verify app still works on old schema
npm test

# 5. Re-apply
npm run migrate:up
```

## Environment Configuration

### Environment Variables

```bash
# .env.production
NODE_ENV=production
DATABASE_URL=postgresql://prod-db:5432/app
REDIS_URL=redis://prod-redis:6379
API_KEY=xxx
JWT_SECRET=xxx
LOG_LEVEL=warn
RATE_LIMIT_MAX=100
```

### Secrets Management

```bash
# Use secret manager (AWS Secrets Manager, HashiCorp Vault, etc.)
# Never commit secrets to git

# Fetch secrets at runtime
aws secretsmanager get-secret-value \
  --secret-id prod/api/jwt-secret \
  --query SecretString \
  --output text
```

## Health Checks

### Application Health Endpoint

```javascript
app.get('/health', async (req, res) => {
  try {
    // Check database
    await db.raw('SELECT 1');

    // Check Redis
    await redis.ping();

    // Check external dependencies
    await fetch('https://api.external.com/health');

    res.json({
      status: 'healthy',
      timestamp: new Date().toISOString(),
      version: process.env.APP_VERSION
    });

  } catch (error) {
    res.status(503).json({
      status: 'unhealthy',
      error: error.message
    });
  }
});
```

### Readiness vs Liveness

```javascript
// Liveness: Is app running?
app.get('/health/live', (req, res) => {
  res.json({ status: 'alive' });
});

// Readiness: Can app serve traffic?
app.get('/health/ready', async (req, res) => {
  const checks = await Promise.all([
    checkDatabase(),
    checkRedis(),
    checkQueueConnection()
  ]);

  const allReady = checks.every(c => c.ready);

  res.status(allReady ? 200 : 503).json({
    ready: allReady,
    checks
  });
});
```

## Monitoring and Alerts

### Key Metrics to Track

- Error rate (> 1% triggers alert)
- Response time (p95 > 1s triggers alert)
- Request rate (unusual spikes/drops)
- Database connection pool usage
- Memory usage (> 80% triggers alert)
- CPU usage (> 70% triggers alert)

### Alert Channels

- Critical: PagerDuty + SMS
- High: Slack #alerts channel
- Medium: Email to on-call engineer
- Low: Daily digest email

## Incident Response

### Severity Levels

**P0 (Critical)**: Complete outage
- Response time: < 15 minutes
- All hands on deck
- Immediate rollback if needed

**P1 (High)**: Major feature broken
- Response time: < 1 hour
- Coordinate with on-call engineer
- Plan fix or rollback

**P2 (Medium)**: Minor feature broken
- Response time: < 4 hours
- Fix in next deploy

**P3 (Low)**: Cosmetic issue
- Response time: < 24 hours
- Fix when convenient

---

**Remember**: Always have a rollback plan. Deploy during low-traffic hours when possible.
