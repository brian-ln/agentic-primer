/**
 * Rate Limiter - Token bucket with exponential backoff
 * Pure algorithm, no runtime-specific dependencies.
 */

export interface RateLimiterOptions {
  maxRequestsPerSecond?: number;
  minDelayMs?: number;
  maxConcurrent?: number;
  backoffMultiplier?: number;
  maxBackoffMs?: number;
}

export interface RateLimiterStats {
  totalRequests: number;
  throttledRequests: number;
  averageDelayMs: number;
  concurrentRequests: number;
}

export class RateLimiter {
  private lastRequestTime = 0;
  private requestCount = 0;
  private throttledCount = 0;
  private totalDelayMs = 0;
  private concurrentRequests = 0;
  private failureCount = 0;

  private readonly maxRequestsPerSecond: number;
  private readonly minDelayMs: number;
  private readonly maxConcurrent: number;
  private readonly backoffMultiplier: number;
  private readonly maxBackoffMs: number;

  constructor(options: RateLimiterOptions = {}) {
    this.maxRequestsPerSecond = options.maxRequestsPerSecond ?? 10;
    this.minDelayMs = options.minDelayMs ?? 100;
    this.maxConcurrent = options.maxConcurrent ?? 5;
    this.backoffMultiplier = options.backoffMultiplier ?? 2;
    this.maxBackoffMs = options.maxBackoffMs ?? 30_000;
  }

  async throttle(): Promise<void> {
    while (this.concurrentRequests >= this.maxConcurrent) {
      await this.sleep(50);
    }
    this.concurrentRequests++;

    try {
      const now = Date.now();
      const timeSinceLastRequest = now - this.lastRequestTime;
      let delayMs = 0;

      if (timeSinceLastRequest < this.minDelayMs) {
        delayMs = this.minDelayMs - timeSinceLastRequest;
      }
      if (this.failureCount > 0) {
        const backoffDelay = Math.min(
          this.minDelayMs * Math.pow(this.backoffMultiplier, this.failureCount),
          this.maxBackoffMs
        );
        delayMs = Math.max(delayMs, backoffDelay);
      }
      if (delayMs > 0) {
        this.throttledCount++;
        this.totalDelayMs += delayMs;
        await this.sleep(delayMs);
      }

      this.lastRequestTime = Date.now();
      this.requestCount++;
    } finally {
      this.concurrentRequests--;
    }
  }

  recordSuccess(): void { this.failureCount = 0; }
  recordFailure(): void { this.failureCount++; }

  getStats(): RateLimiterStats {
    return {
      totalRequests: this.requestCount,
      throttledRequests: this.throttledCount,
      averageDelayMs: this.throttledCount > 0 ? this.totalDelayMs / this.throttledCount : 0,
      concurrentRequests: this.concurrentRequests,
    };
  }

  reset(): void {
    this.lastRequestTime = 0;
    this.requestCount = 0;
    this.throttledCount = 0;
    this.totalDelayMs = 0;
    this.concurrentRequests = 0;
    this.failureCount = 0;
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

export const llmRateLimiter = new RateLimiter({
  maxRequestsPerSecond: 5,
  minDelayMs: 200,
  maxConcurrent: 5,
  backoffMultiplier: 2,
  maxBackoffMs: 30_000,
});

export const dbRateLimiter = new RateLimiter({
  maxRequestsPerSecond: 50,
  minDelayMs: 20,
  maxConcurrent: 10,
  backoffMultiplier: 1.5,
  maxBackoffMs: 5_000,
});

export async function withRateLimit<T>(
  fn: () => Promise<T>,
  limiter: RateLimiter = llmRateLimiter,
  maxRetries: number = 3
): Promise<T> {
  let lastError: unknown;
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      await limiter.throttle();
      const result = await fn();
      limiter.recordSuccess();
      return result;
    } catch (error) {
      lastError = error;
      limiter.recordFailure();
      if (attempt === maxRetries) break;
    }
  }
  throw lastError;
}
