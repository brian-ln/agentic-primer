// Structured Error Types for tk-agents
//
// Design: Replace throw statements with structured error returns
// that can be categorized and handled appropriately.

/**
 * Error categories for different failure modes
 *
 * - validation: Input is invalid (e.g., missing required fields)
 * - transient: Temporary failure, retry likely to succeed (e.g., network timeout)
 * - permanent: Persistent failure, retry won't help (e.g., permission denied)
 * - fatal: System failure, cannot continue (e.g., out of memory)
 */
export type ErrorCategory = "validation" | "transient" | "permanent" | "fatal";

/**
 * Structured error information
 */
export interface ActorError {
  /** Category of error for determining retry strategy */
  category: ErrorCategory;

  /** Human-readable error message */
  message: string;

  /** Optional underlying cause */
  cause?: Error;

  /** Whether this error is retryable */
  retryable: boolean;

  /** Additional context for debugging */
  context?: Record<string, unknown>;
}

/**
 * Response type that includes structured errors
 */
export interface Response<T = unknown> {
  success: boolean;
  data?: T;
  error?: ActorError;
  metadata?: Record<string, unknown>;
}

/**
 * Helper to create validation errors
 */
export function validationError(message: string, context?: Record<string, unknown>): ActorError {
  return {
    category: "validation",
    message,
    retryable: false,
    context,
  };
}

/**
 * Helper to create transient errors
 */
export function transientError(message: string, cause?: Error, context?: Record<string, unknown>): ActorError {
  return {
    category: "transient",
    message,
    cause,
    retryable: true,
    context,
  };
}

/**
 * Helper to create permanent errors
 */
export function permanentError(message: string, cause?: Error, context?: Record<string, unknown>): ActorError {
  return {
    category: "permanent",
    message,
    cause,
    retryable: false,
    context,
  };
}

/**
 * Helper to create fatal errors
 */
export function fatalError(message: string, cause?: Error, context?: Record<string, unknown>): ActorError {
  return {
    category: "fatal",
    message,
    cause,
    retryable: false,
    context,
  };
}

/**
 * Helper to create error response
 */
export function errorResponse<T = unknown>(error: ActorError, metadata?: Record<string, unknown>): Response<T> {
  return {
    success: false,
    error,
    metadata,
  };
}

/**
 * Helper to create success response
 */
export function successResponse<T = unknown>(data: T, metadata?: Record<string, unknown>): Response<T> {
  return {
    success: true,
    data,
    metadata,
  };
}
