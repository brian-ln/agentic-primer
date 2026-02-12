# Deferred Security Issues

**Last Updated:** 2026-02-04
**Status:** Acknowledged, deferred for future implementation

---

## M1: Missing CSRF Protection (MEDIUM Severity)

**Status:** Deferred until web interface implementation
**Priority:** P3 (only applicable when adding web features)

### Description
State-modifying operations (add, delete, update) lack CSRF token validation. This is not exploitable in the current CLI-only implementation but would be a vulnerability if a web interface is added.

### Impact
- **Current:** None (CLI has no CSRF attack surface)
- **Future Web Interface:** HIGH - Attackers could forge requests to modify user data

### Attack Scenario (Web Interface)
```html
<!-- Malicious site -->
<form action="https://victim-site.com/api/delete" method="POST">
  <input type="hidden" name="session_id" value="victim-session" />
</form>
<script>document.forms[0].submit();</script>
```

### Implementation Approach (When Needed)
1. Generate CSRF token on session creation
2. Include token in all state-modifying requests
3. Validate token server-side before execution
4. Use SameSite cookies as defense-in-depth

### References
- OWASP CSRF Prevention Cheat Sheet
- [Double Submit Cookie Pattern](https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html)

### Trigger Conditions for Implementation
- Adding web API endpoints
- Adding web UI for knowledge management
- Adding browser extension

---

## L3: Race Conditions in Rate Limiter (LOW Severity)

**Status:** Deferred (complex fix, low exploitability)
**Priority:** P2 (performance optimization)

### Description
The current RateLimiter uses non-atomic counters that may allow brief bursts of concurrent requests to bypass limits with precise timing.

### Impact
- **Severity:** LOW
- **Exploitability:** Requires precise timing across multiple threads/processes
- **Worst Case:** 2-3x rate limit briefly exceeded during concurrent burst

### Attack Scenario
```typescript
// 100 requests at exactly the same moment
const burst = Array(100).fill(0).map(() =>
  rateLimiter.throttle()
);
await Promise.all(burst);
// May bypass maxConcurrent=10 briefly
```

### Current Mitigation
- Database operations are fast enough that brief bursts are not DoS-level
- System load monitoring provides additional protection
- Rate limits are conservative (50 req/s for DB, 5 req/s for LLM)

### Implementation Approach (Future)
**Option 1: Atomic Operations (Simple)**
```typescript
import { Atomics } from 'atomics';

class AtomicRateLimiter {
  private sharedBuffer: SharedArrayBuffer;
  private counter: Int32Array;

  async acquire() {
    const old = Atomics.load(this.counter, 0);
    if (old >= this.maxConcurrent) {
      await this.wait();
    }
    Atomics.add(this.counter, 0, 1);
  }
}
```

**Option 2: Mutex Lock (Robust)**
```typescript
import { Mutex } from 'async-mutex';

class MutexRateLimiter {
  private mutex = new Mutex();

  async acquire() {
    const release = await this.mutex.acquire();
    try {
      // Rate limit logic (guaranteed atomic)
    } finally {
      release();
    }
  }
}
```

**Option 3: Redis-Based (Distributed)**
```typescript
// Use Redis INCR/DECR for distributed rate limiting
await redis.incr('rate:counter');
```

### Effort Estimate
- **Option 1:** 60 minutes (requires SharedArrayBuffer setup)
- **Option 2:** 90 minutes (new dependency + tests)
- **Option 3:** 120 minutes (Redis infrastructure + deployment)

### Test Approach
```typescript
test('handles 100 concurrent requests without exceeding limit', async () => {
  const limiter = new RateLimiter({ maxConcurrent: 10 });
  const active = { count: 0, max: 0 };

  const requests = Array(100).fill(0).map(async () => {
    await limiter.acquire();
    active.count++;
    active.max = Math.max(active.max, active.count);
    await sleep(10);
    active.count--;
    limiter.release();
  });

  await Promise.all(requests);
  expect(active.max).toBeLessThanOrEqual(10); // CURRENTLY FAILS
});
```

### Trigger Conditions for Implementation
- Load testing reveals actual DoS vulnerability
- Observing rate limit bypasses in production logs
- Adding multi-tenant deployment (needs stricter isolation)
- Performance optimization sprint

---

## Decision Log

**2026-02-04:** Deferred M1 and L3 after fixing HIGH severity issues (SQL injection, rate limiting enforcement). Focus on Phase 2 integration analysis.

**Rationale:**
- M1: Not applicable to current architecture (CLI only)
- L3: LOW severity, complex fix, existing mitigations sufficient

**Review Schedule:**
- M1: Review when web interface design begins
- L3: Review during performance optimization phase or if load testing reveals issues

---

## Monitoring

Track these indicators to determine if deferred issues need immediate attention:

**For M1 (CSRF):**
- [ ] Web API endpoints added
- [ ] Browser-based UI implemented
- [ ] External HTTP requests accepted

**For L3 (Race Conditions):**
- [ ] Rate limiter bypass events in logs
- [ ] Concurrent request counts exceeding limits
- [ ] Database performance degradation during bursts
- [ ] User reports of throttling inconsistency
