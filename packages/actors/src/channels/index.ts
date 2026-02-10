/**
 * Channel implementations for unified async communication.
 *
 * Re-exports all channel types and utilities.
 *
 * Three channel types:
 * - StreamChannel: Point-to-point (1:1) streaming over AsyncStreamMessage
 * - PortChannel: Pub/sub (1:N) broadcasting with per-subscriber buffers
 * - BridgeChannel: External async sources (callbacks, events) adapted to Channel
 *
 * Ported from simplify/src/messaging/channels/
 */

// Core types
export type { Channel, ChannelOptions } from './channel.ts';
export {
  BaseChannel,
  ChannelClosedError,
  ChannelCancelledError,
} from './channel.ts';

// StreamChannel - Point-to-point (1:1)
export { StreamChannel, createStreamChannel } from './stream.ts';

// PortChannel - Pub/sub (1:N)
export { PortChannel, createPortChannel } from './port.ts';

// BridgeChannel - External sources
export type { SetupFn, CleanupFn } from './bridge.ts';
export { BridgeChannel, createBridgeChannel } from './bridge.ts';
