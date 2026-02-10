/**
 * RemoteTransport - WebSocket transport for remote actor communication
 *
 * Features:
 * - Application-level heartbeat (PING/PONG)
 * - Latency tracking with exponential moving average
 * - Health status: UNKNOWN -> HEALTHY -> DEGRADED -> UNHEALTHY
 * - Multi-subscriber pattern for health events
 * - Reconnection with exponential backoff + jitter
 * - Message queuing during disconnection
 * - Pluggable serialization (Serde interface)
 *
 * Ported from brianln.ai/src/actors/transports/RemoteTransport.ts
 */

/**
 * Serde interface for transport serialization.
 * Uses string | ArrayBuffer (WebSocket-compatible) rather than Uint8Array.
 */
export interface Serde {
  serialize(message: unknown): string | ArrayBuffer;
  deserialize(data: string | ArrayBuffer): unknown;
  readonly contentType: string;
}

/** JSON serialization (default) */
export class JsonSerde implements Serde {
  readonly contentType = 'application/json';

  serialize(message: unknown): string {
    return JSON.stringify(message);
  }

  deserialize(data: string | ArrayBuffer): unknown {
    const text = typeof data === 'string' ? data : new TextDecoder().decode(data);
    return JSON.parse(text);
  }
}

export interface RemoteMessage {
  to: string;
  from?: string;
  type: string;
  payload: unknown;
  id?: string;
}

export interface ControlMessage extends RemoteMessage {
  type: 'HEARTBEAT_PING' | 'HEARTBEAT_PONG';
}

export enum HealthStatus {
  HEALTHY = 'HEALTHY',
  DEGRADED = 'DEGRADED',
  UNHEALTHY = 'UNHEALTHY',
  UNKNOWN = 'UNKNOWN',
}

export interface HeartbeatStats {
  lastPing: number;
  lastPong: number;
  consecutiveMisses: number;
  latency: number;
  averageLatency: number;
  status: HealthStatus;
}

export interface RemoteTransportConfig {
  url: string;
  serde?: Serde;
  reconnectAttempts?: number;
  reconnectDelay?: number;
  heartbeatInterval?: number;
  heartbeatTimeout?: number;
  missedHeartbeatThreshold?: number;
  onMessage?: (message: RemoteMessage) => void;
}

export class RemoteTransport {
  private ws: WebSocket | null = null;
  public config: Required<RemoteTransportConfig>;
  private reconnectCount = 0;
  private messageQueue: RemoteMessage[] = [];
  private connected = false;

  // Heartbeat management
  private heartbeatIntervalId?: ReturnType<typeof setInterval>;
  private heartbeatTimeoutId?: ReturnType<typeof setTimeout>;
  private heartbeatStats: HeartbeatStats = {
    lastPing: 0,
    lastPong: 0,
    consecutiveMisses: 0,
    latency: 0,
    averageLatency: 0,
    status: HealthStatus.UNKNOWN,
  };
  private pendingHeartbeats = new Map<string, number>();

  // Multi-subscriber event system
  private healthSubscribers = new Map<string, (stats: HeartbeatStats) => void>();
  private connectSubscribers = new Map<string, () => void>();
  private disconnectSubscribers = new Map<string, () => void>();
  private errorSubscribers = new Map<string, (error: Error) => void>();
  private previousLatency = 0;

  constructor(config: RemoteTransportConfig) {
    this.config = {
      serde: config.serde || new JsonSerde(),
      reconnectAttempts: config.reconnectAttempts ?? 5,
      reconnectDelay: config.reconnectDelay ?? 1000,
      heartbeatInterval: config.heartbeatInterval ?? 30000,
      heartbeatTimeout: config.heartbeatTimeout ?? 10000,
      missedHeartbeatThreshold: config.missedHeartbeatThreshold ?? 2,
      onMessage: config.onMessage || (() => {}),
      url: config.url,
    };
  }

  async connect(): Promise<void> {
    if (this.ws?.readyState === WebSocket.OPEN) {
      return;
    }

    return new Promise((resolve, reject) => {
      this.ws = new WebSocket(this.config.url);

      if (this.config.serde.contentType !== 'application/json') {
        this.ws.binaryType = 'arraybuffer';
      }

      this.ws.onopen = () => {
        this.connected = true;
        this.reconnectCount = 0;

        while (this.messageQueue.length > 0) {
          const msg = this.messageQueue.shift()!;
          this.sendImmediate(msg);
        }

        this.startHeartbeat();
        this.notifyConnect();
        resolve();
      };

      this.ws.onerror = (event) => {
        const error = new Error('WebSocket error');
        this.notifyError(error);
        reject(error);
      };

      this.ws.onclose = () => {
        this.connected = false;
        this.stopHeartbeat();
        this.notifyDisconnect();
        this.handleReconnect();
      };

      this.ws.onmessage = (event) => {
        try {
          const message = this.config.serde.deserialize(event.data) as RemoteMessage;

          if (message.type === 'HEARTBEAT_PONG') {
            this.handleHeartbeatPong(message as ControlMessage);
            return;
          }

          this.config.onMessage(message);
        } catch (error) {
          this.notifyError(error as Error);
        }
      };
    });
  }

  send(message: RemoteMessage): void {
    if (!this.connected) {
      this.messageQueue.push(message);
      return;
    }
    this.sendImmediate(message);
  }

  private sendImmediate(message: RemoteMessage): void {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
      return;
    }

    try {
      const serialized = this.config.serde.serialize(message);
      this.ws.send(serialized);
    } catch (error) {
      this.notifyError(error as Error);
    }
  }

  disconnect(): void {
    this.stopHeartbeat();
    if (this.ws) {
      this.ws.close();
      this.ws = null;
      this.connected = false;
    }
  }

  isConnected(): boolean {
    return this.connected;
  }

  getHealthStats(): HeartbeatStats {
    return { ...this.heartbeatStats };
  }

  // --- Multi-subscriber event API ---

  subscribe(subscriberId: string, callback: (stats: HeartbeatStats) => void): void {
    this.healthSubscribers.set(subscriberId, callback);
  }

  unsubscribe(subscriberId: string): void {
    this.healthSubscribers.delete(subscriberId);
    this.connectSubscribers.delete(subscriberId);
    this.disconnectSubscribers.delete(subscriberId);
    this.errorSubscribers.delete(subscriberId);
  }

  subscribeToConnect(subscriberId: string, callback: () => void): void {
    this.connectSubscribers.set(subscriberId, callback);
  }

  subscribeToDisconnect(subscriberId: string, callback: () => void): void {
    this.disconnectSubscribers.set(subscriberId, callback);
  }

  subscribeToError(subscriberId: string, callback: (error: Error) => void): void {
    this.errorSubscribers.set(subscriberId, callback);
  }

  // --- Event notification ---

  private notifyHealthChange(): void {
    const stats = this.getHealthStats();
    for (const [, callback] of this.healthSubscribers) {
      try { callback(stats); } catch { /* subscriber error */ }
    }
  }

  private notifyConnect(): void {
    for (const [, callback] of this.connectSubscribers) {
      try { callback(); } catch { /* subscriber error */ }
    }
  }

  private notifyDisconnect(): void {
    for (const [, callback] of this.disconnectSubscribers) {
      try { callback(); } catch { /* subscriber error */ }
    }
  }

  private notifyError(error: Error): void {
    for (const [, callback] of this.errorSubscribers) {
      try { callback(error); } catch { /* subscriber error */ }
    }
  }

  // --- Heartbeat ---

  private startHeartbeat(): void {
    if (this.heartbeatIntervalId) {
      clearInterval(this.heartbeatIntervalId);
    }
    this.sendHeartbeat();
    this.heartbeatIntervalId = setInterval(() => {
      this.sendHeartbeat();
    }, this.config.heartbeatInterval);
  }

  private stopHeartbeat(): void {
    if (this.heartbeatIntervalId) {
      clearInterval(this.heartbeatIntervalId);
      this.heartbeatIntervalId = undefined;
    }
    if (this.heartbeatTimeoutId) {
      clearTimeout(this.heartbeatTimeoutId);
      this.heartbeatTimeoutId = undefined;
    }
    this.pendingHeartbeats.clear();
  }

  private sendHeartbeat(): void {
    const id = `heartbeat-${Date.now()}`;
    const timestamp = Date.now();

    this.pendingHeartbeats.set(id, timestamp);
    this.heartbeatStats.lastPing = timestamp;

    const ping: ControlMessage = {
      type: 'HEARTBEAT_PING',
      to: '__system__',
      from: '__client__',
      payload: { timestamp },
      id,
    };

    this.sendImmediate(ping);

    this.heartbeatTimeoutId = setTimeout(() => {
      this.handleMissedHeartbeat(id);
    }, this.config.heartbeatTimeout);
  }

  private handleHeartbeatPong(message: ControlMessage): void {
    const sentTime = this.pendingHeartbeats.get(message.id!);
    if (!sentTime) return;

    this.pendingHeartbeats.delete(message.id!);

    const latency = Date.now() - sentTime;
    this.heartbeatStats.lastPong = Date.now();
    this.heartbeatStats.latency = latency;
    this.heartbeatStats.consecutiveMisses = 0;

    // Exponential moving average
    if (this.heartbeatStats.averageLatency === 0) {
      this.heartbeatStats.averageLatency = latency;
    } else {
      this.heartbeatStats.averageLatency =
        0.7 * this.heartbeatStats.averageLatency + 0.3 * latency;
    }

    const previousStatus = this.heartbeatStats.status;
    const latencyDelta = Math.abs(latency - this.previousLatency);
    this.heartbeatStats.status = this.calculateHealthStatus();

    const statusChanged = previousStatus !== this.heartbeatStats.status;
    const significantLatencyChange = latencyDelta > 20;

    if (statusChanged || significantLatencyChange) {
      this.notifyHealthChange();
    }

    this.previousLatency = latency;

    if (this.heartbeatTimeoutId) {
      clearTimeout(this.heartbeatTimeoutId);
      this.heartbeatTimeoutId = undefined;
    }
  }

  private handleMissedHeartbeat(id: string): void {
    this.pendingHeartbeats.delete(id);
    this.heartbeatStats.consecutiveMisses++;

    const previousStatus = this.heartbeatStats.status;
    this.heartbeatStats.status = this.calculateHealthStatus();

    if (previousStatus !== this.heartbeatStats.status) {
      this.notifyHealthChange();
    }

    if (this.heartbeatStats.consecutiveMisses >= this.config.missedHeartbeatThreshold) {
      this.handleConnectionDead();
    }
  }

  private calculateHealthStatus(): HealthStatus {
    const { consecutiveMisses, averageLatency } = this.heartbeatStats;

    if (consecutiveMisses >= this.config.missedHeartbeatThreshold) {
      return HealthStatus.UNHEALTHY;
    }
    if (consecutiveMisses >= 1 || averageLatency > 5000) {
      return HealthStatus.DEGRADED;
    }
    if (averageLatency > 0) {
      return HealthStatus.HEALTHY;
    }
    return HealthStatus.UNKNOWN;
  }

  private handleConnectionDead(): void {
    this.stopHeartbeat();
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.close();
    }
    this.connected = false;
    this.notifyDisconnect();
    this.handleReconnect();
  }

  private handleReconnect(): void {
    if (this.reconnectCount >= this.config.reconnectAttempts) {
      this.notifyError(new Error('Max reconnection attempts exceeded'));
      return;
    }

    this.reconnectCount++;

    const baseDelay = Math.min(
      this.config.reconnectDelay * Math.pow(2, this.reconnectCount - 1),
      30000
    );
    const jitter = baseDelay * 0.25 * (Math.random() - 0.5);
    const delay = Math.max(0, baseDelay + jitter);

    setTimeout(() => {
      this.connect().catch(() => {});
    }, delay);
  }
}
