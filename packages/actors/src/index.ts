/**
 * @agentic-primer/actors - Portable actor core
 *
 * Web platform baseline. Runs on Node, Bun, Deno, browsers.
 *
 * Two complementary patterns:
 * - Class-based: extend Actor, implement receive()
 * - Functional: spawn(behavior, initialState)
 */

// Message types and helpers (simplify foundation)
export {
  type Address,
  type MessagePattern,
  type Message,
  type MessageResponse,
  type StreamEvent,
  type TokenStreamEvent,
  type AsyncStreamMessage,
  type StreamAsyncOptions,
  type StreamCallback,
  type MessageHandler,
  address,
  parseAddress,
  generateMessageId,
  generateCorrelationId,
  createMessage,
  createResponse,
  createErrorResponse,
} from './message.ts';

// Actor types (merged: simplify + brianln.ai)
export {
  type ActorBehavior,
  type ActorContext,
  type ActorInstance,
  type SupervisionDirective,
  type SupervisionStrategy,
  type ActorSystemConfig,
  type DeadLetterEntry,
} from './types.ts';

// Interfaces
export {
  type IActorRegistry,
  MapActorRegistry,
  type IActorCheckpoint,
  type ITransport,
  type ISerde,
  type ConnectionState,
  type IMessageRouter,
  // Storage interfaces (aligned with WIT domain.wit)
  type SqlValue,
  type SqlRow,
  type SqlResult,
  type SqlError,
  type ISqlStorage,
  type IKeyValueStorage,
  type IBlobStorage,
  type IVectorStore,
  type IEmbedder,
} from './interfaces.ts';

// Actor base class
export { Actor } from './actor.ts';
export { ActorWithIntrospection } from './actor-introspection.ts';

// Introspection protocol
export {
  type JSONSchema,
  type MessageAcceptance,
  type HandlerRegistration,
  type ActorMetadata,
  type IntrospectResponse,
  type MessageAcceptanceMetadata,
  type MessageConsequences,
  type MessageExample,
  type AcceptanceCriteria,
  type PingResponse,
  accepts,
} from './introspection.ts';

// Schema validator
export {
  type ValidationError,
  validateJSONSchema,
  validateJSONSchemaErrors,
  findClosestMatch,
} from './validation/schema-validator.ts';

// Message router
export { MessageRouter, type MessageRouterConfig } from './router.ts';

// Supervisor
export { SupervisorBase, LeafActor } from './supervisor.ts';

// Actor system
export { ActorSystem } from './actor-system.ts';

// Path utilities
export {
  parsePath,
  validatePath,
  isSafePath,
  normalizePath,
  matchPattern,
  getParentPath,
  getLocalName,
  joinPath,
  isChildOf,
} from './routing/path-resolver.ts';

export {
  type ParsedAddressInfo,
  parseAddressInfo,
  isHierarchicalPath,
} from './routing/address-parser.ts';

export {
  PatternError,
  type PatternOptions,
  validatePattern,
  expandAlternatives,
  matchPattern as matchAdvancedPattern,
  compilePattern,
  filterPaths,
  hasWildcards,
  hasAlternatives,
  estimateComplexity,
} from './routing/path-patterns.ts';

export { PathCache, type PathCacheConfig, type CacheMetrics } from './routing/path-cache.ts';

// Supervision
export {
  type RestartStrategyType,
  type RestartStrategy,
  type ErrorSeverity,
  type ErrorClassification,
  type ErrorClassifier,
  type HealthCheckConfig,
  type SupervisionOptions,
  type ChildStatus,
  type SupervisionStatus,
  type RestartEvent,
  type EscalationEvent,
  type SupervisedActorHooks,
  type SupervisionMetrics,
  DEFAULT_RESTART_STRATEGY,
  DEFAULT_HEALTH_CHECK,
  defaultErrorClassifier,
} from './supervision/types.ts';

export {
  type RestartDecision,
  type StrategyContext,
  applyOneForOne,
  applyOneForAll,
  applyRestForOne,
  applyRestartStrategy,
  isRestartAllowed,
  resetRestartCounter,
  incrementRestartCounter,
  calculateRestartBackoff,
  formatRestartStrategy,
} from './supervision/strategies.ts';

// Channels - unified async communication primitives
export {
  type Channel,
  type ChannelOptions,
  BaseChannel,
  ChannelClosedError,
  ChannelCancelledError,
  StreamChannel,
  createStreamChannel,
  PortChannel,
  createPortChannel,
  type SetupFn,
  type CleanupFn,
  BridgeChannel,
  createBridgeChannel,
} from './channels/index.ts';

// Transport and serialization implementations
export { LocalTransport } from './transport/local-transport.ts';
export { JsonSerde } from './transport/serde.ts';

// Storage validation (defense-in-depth)
export {
  validateSqlQuery,
  validateSqlParams,
  validateBatchSize,
  validateKvKey,
  validateKvValue,
} from './validation/storage-validation.ts';

export { ValidatedSqlStorage } from './validation/validated-sql-storage.ts';
export { ValidatedKvStorage } from './validation/validated-kv-storage.ts';

// Claim check pattern
export {
  CLAIM_CHECK_THRESHOLD,
  type ClaimCheckReference,
  shouldUseClaimCheck,
  isClaimCheckReference,
} from './storage/claim-check.ts';

export { ClaimCheckStore } from './storage/claim-check-store.ts';
