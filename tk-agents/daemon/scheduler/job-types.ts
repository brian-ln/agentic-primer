/**
 * Job Scheduling Type Definitions
 *
 * Core types for the daemon job scheduling system.
 * Supports 4 scheduling patterns: cron, event-driven, one-time, retry
 */

// ===== Job Definition =====

export interface Job {
  id: string;                    // Unique job ID
  name: string;                  // Human-readable name
  type: "cron" | "event" | "one-time" | "retry";
  schedule: ScheduleConfig;      // Type-specific schedule configuration
  handler: JobHandler;           // Execution function
  owner: string;                 // Who owns this job (for issue assignment)
  priority: 0 | 1 | 2 | 3 | 4;  // Issue priority on failure
  maxRetries?: number;           // Max automatic retries (default: 0)
  timeout?: number;              // Max execution time in ms (default: 30000)
  enabled: boolean;              // Can be disabled without deletion
  metadata?: Record<string, any>; // Additional metadata
}

// ===== Job Handler =====

export type JobHandler = (context: JobContext) => Promise<JobResult>;

export interface JobContext {
  jobId: string;
  runId: string;                 // Unique execution ID
  attempt: number;               // Retry attempt number (1-based)
  trigger: string;               // What triggered this run
  data?: any;                    // Event data (for event-driven jobs)
  startTime: number;             // Execution start timestamp
}

export interface JobResult {
  success: boolean;
  output?: any;                  // Optional result data
  error?: Error;                 // Error if failed
  duration: number;              // Execution time in ms
}

// ===== Schedule Configurations =====

export interface CronSchedule {
  type: "cron";
  interval: number;              // Milliseconds between runs
  offset?: number;               // Initial delay before first run
  maxRuns?: number;              // Stop after N runs (optional)
}

export interface EventSchedule {
  type: "event";
  eventType: string;             // Event type to listen for
  filter?: (event: any) => boolean;  // Optional event filter
}

export interface OneTimeSchedule {
  type: "one-time";
  runAt: Date | number;          // Absolute time (Date) or relative ms (number)
}

export interface RetrySchedule {
  type: "retry";
  baseDelay: number;             // Initial delay in ms
  maxDelay: number;              // Max backoff delay in ms
  multiplier: number;            // Backoff multiplier (default: 2)
  maxAttempts: number;           // Max retry attempts
}

export type ScheduleConfig = CronSchedule | EventSchedule | OneTimeSchedule | RetrySchedule;

// ===== Job Status =====

export interface JobStatus {
  jobId: string;
  name: string;
  type: string;
  enabled: boolean;
  lastRun?: {
    runId: string;
    timestamp: string;
    success: boolean;
    duration: number;
    error?: string;
  };
  nextRun?: string;              // ISO timestamp (for cron/one-time)
  totalRuns: number;
  successfulRuns: number;
  failedRuns: number;
  currentAttempt?: number;       // For retry jobs
}

// ===== Notification =====

export interface JobNotification {
  title: string;
  body: string;
  priority: 0 | 1 | 2 | 3 | 4;
  metadata: {
    jobId: string;
    runId: string;
    attempt: number;
    [key: string]: any;
  };
}

// ===== Internal Scheduler State =====

export interface CronJobState {
  job: Job;
  runCount: number;
  lastRun?: Date;
  nextRun?: Date;
}

export interface EventJobState {
  job: Job;
  eventCount: number;
  lastEvent?: Date;
}

export interface RetryJobState {
  job: Job;
  originalRunId: string;
  attempts: number;
  lastAttempt?: Date;
  nextAttempt?: Date;
}

// ===== Error Codes =====

export enum JobErrorCode {
  TIMEOUT = "JOB_TIMEOUT",
  HANDLER_ERROR = "HANDLER_ERROR",
  MAX_RETRIES = "MAX_RETRIES_EXCEEDED",
  INVALID_SCHEDULE = "INVALID_SCHEDULE",
  JOB_NOT_FOUND = "JOB_NOT_FOUND",
  JOB_DISABLED = "JOB_DISABLED",
}

export class JobError extends Error {
  constructor(
    public code: JobErrorCode,
    message: string,
    public details?: any
  ) {
    super(message);
    this.name = "JobError";
  }
}
