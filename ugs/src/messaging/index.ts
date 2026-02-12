#!/usr/bin/env bun
/**
 * Graph-Actor Messaging System
 *
 * Message-based communication layer for UGS.
 * Provides uniform actor interface for all graph nodes.
 *
 * ## Dual Routing Support (Phase 6)
 *
 * Supports both flat ID and hierarchical path addressing:
 * - Flat IDs: `@(actor-123)` (legacy, with alias resolution)
 * - Paths: `@(domain/inference)` (canonical)
 *
 * See: docs/DUAL_ROUTING_MIGRATION.md
 */

// Core exports from @agentic-primer/actors (base implementations)
export * from '@agentic-primer/actors';

// UGS-specific extensions (GraphStore integration)
export { MessageRouter } from './router.ts';
export { Actor, ActorSystem as UGSActorSystem } from './actor.ts';

// Re-export protocol adapter for convenience
// Enables: import { toProtocolAddress } from './messaging'
export * from '../protocol/index.ts';
