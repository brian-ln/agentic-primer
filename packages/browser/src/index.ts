/**
 * @agentic-primer/browser - Browser runtime adapter
 *
 * Provides browser-specific actor implementations:
 * - UIActor: Web Components + TC39 Signals + lit-html + actor mailbox
 * - WidgetActor/ActorMixin: Web Component mixin for actor protocol
 * - BrowserActorRegistry: Browser-side actor lookup
 * - RemoteTransport: WebSocket transport with heartbeat/reconnection
 * - ServiceWorkerTransport: Service Worker MessageChannel transport
 * - RemoteProxy: Entangled distributed actor pattern
 */

// UIActor - Web Component + Signals + lit-html + actor mailbox
export {
  UIActor,
  setActorSystem,
  getActorSystem,
  signal,
  computed,
  effect,
  html,
  render,
  type Signal,
  type ReadonlySignal,
  type TemplateResult,
} from './ui-actor.ts';

// Widget Actor - Web Component mixin
export {
  ActorMixin,
  BaseWidgetActor,
  createWidgetActor,
  type Constructor,
  type WidgetActor,
} from './widget-actor.ts';

// Browser Actor Registry
export {
  BrowserActorRegistry,
  actorRegistry,
  ActorNotFoundError,
  DuplicateActorError,
} from './actor-registry.ts';

// Remote Proxy - entangled distributed actor
export {
  createRemoteProxyBehavior,
  spawnRemoteProxy,
  type RemoteProxyState,
} from './remote-proxy.ts';

// Transports
export {
  RemoteTransport,
  JsonSerde,
  HealthStatus,
  type Serde,
  type RemoteMessage,
  type ControlMessage,
  type HeartbeatStats,
  type RemoteTransportConfig,
} from './transports/remote-transport.ts';

export {
  ServiceWorkerTransport,
} from './transports/service-worker-transport.ts';

// Signal <-> Channel Bridges (browser-specific)
export {
  type BridgeSignal,
  signalToChannel,
  signalToChannelWithSubscribe,
  channelToSignal,
  domEventChannel,
  eventTargetChannel,
  mergeChannels,
} from './bridges.ts';
