// Types
export type {
  EpistemicLevel,
  EvidenceLink,
  KnowledgeCategory,
  KnowledgeItem,
  RelationshipType,
  Relationship,
  KnowledgeFilter,
  RelationshipFilter,
} from './types.ts';

// Knowledge Store
export { KnowledgeStore } from './knowledge-store.ts';

// Embedding Generator
export { EmbeddingGenerator } from './embedding-generator.ts';
export type { EmbeddingConfig } from './embedding-generator.ts';

// Security
export {
  INPUT_LIMITS,
  validateLength,
  validateSessionId,
  sanitizeLikePattern,
  validateInteger,
  validateDateString,
  sanitizeOutput,
  sanitizeErrorMessage,
  sanitizeStackTrace,
  validateCategory,
  validateWorkflowType,
  validateErrorType,
} from './security/input-validation.ts';

export {
  QueryBuilder,
  queryBuilder,
  buildLikePattern,
  buildLikeQuery,
  buildSearchQuery,
} from './security/query-builder.ts';
export type { SafeQuery, LikeMode } from './security/query-builder.ts';

export {
  RateLimiter,
  llmRateLimiter,
  dbRateLimiter,
  withRateLimit,
} from './security/rate-limiter.ts';
export type { RateLimiterOptions, RateLimiterStats } from './security/rate-limiter.ts';

// Temporal
export { ConfidenceDecay } from './temporal/confidence-decay.ts';
export type { Domain, DecayConfig } from './temporal/confidence-decay.ts';

export { ArcDetector } from './temporal/arc-detector.ts';
export type { ArcType, ThinkingArc, ArcIndicators } from './temporal/arc-detector.ts';
