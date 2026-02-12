/**
 * Rate Limiter
 * Prevents API abuse and resource exhaustion
 * Epic: agentic-primer-0lg.2
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

/**
 * Token bucket rate limiter with exponential backoff
 */
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

  /**
   * Wait before allowing next request (throttle)
   * Implements token bucket algorithm with exponential backoff on failures
   */
  async throttle(): Promise<void> {
    // Wait for concurrent slot
    while (this.concurrentRequests >= this.maxConcurrent) {
      await this.sleep(50);
    }

    this.concurrentRequests++;

    try {
      const now = Date.now();
      const timeSinceLastRequest = now - this.lastRequestTime;

      // Calculate required delay
      let delayMs = 0;

      // Token bucket: ensure minimum delay between requests
      if (timeSinceLastRequest < this.minDelayMs) {
        delayMs = this.minDelayMs - timeSinceLastRequest;
      }

      // Exponential backoff on failures
      if (this.failureCount > 0) {
        const backoffDelay = Math.min(
          this.minDelayMs * Math.pow(this.backoffMultiplier, this.failureCount),
          this.maxBackoffMs
        );
        delayMs = Math.max(delayMs, backoffDelay);
      }

      // Apply delay if needed
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

  /**
   * Record a successful request (resets backoff)
   */
  recordSuccess(): void {
    this.failureCount = 0;
  }

  /**
   * Record a failed request (triggers exponential backoff)
   */
  recordFailure(): void {
    this.failureCount++;
  }

  /**
   * Get rate limiter statistics
   */
  getStats(): RateLimiterStats {
    return {
      totalRequests: this.requestCount,
      throttledRequests: this.throttledCount,
      averageDelayMs:
        this.throttledCount > 0 ? this.totalDelayMs / this.throttledCount : 0,
      concurrentRequests: this.concurrentRequests,
    };
  }

  /**
   * Reset rate limiter state
   */
  reset(): void {
    this.lastRequestTime = 0;
    this.requestCount = 0;
    this.throttledCount = 0;
    this.totalDelayMs = 0;
    this.concurrentRequests = 0;
    this.failureCount = 0;
  }

  /**
   * Sleep for specified milliseconds
   */
  private sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
}

/**
 * Global rate limiter instance for LLM API calls
 */
export const llmRateLimiter = new RateLimiter({
  maxRequestsPerSecond: 5,
  minDelayMs: 200,
  maxConcurrent: 5,
  backoffMultiplier: 2,
  maxBackoffMs: 30_000,
});

/**
 * Global rate limiter instance for database operations
 */
export const dbRateLimiter = new RateLimiter({
  maxRequestsPerSecond: 50,
  minDelayMs: 20,
  maxConcurrent: 10,
  backoffMultiplier: 1.5,
  maxBackoffMs: 5_000,
});

/**
 * Execute function with rate limiting and retry logic
 */
export async function withRateLimit<T>(
  fn: () => Promise<T>,
  limiter: RateLimiter = llmRateLimiter,
  maxRetries: number = 3
): Promise<T> {
  let lastError: Error | unknown;

  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      await limiter.throttle();
      const result = await fn();
      limiter.recordSuccess();
      return result;
    } catch (error) {
      lastError = error;
      limiter.recordFailure();

      // Don't retry on the last attempt
      if (attempt === maxRetries) {
        break;
      }

      // Log retry attempt
      console.warn(
        `Request failed (attempt ${attempt + 1}/${maxRetries + 1}), retrying...`
      );
    }
  }

  throw lastError;
}
