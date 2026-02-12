#!/usr/bin/env bun
/**
 * Human Entity for Universal Graph System
 *
 * Humans are actors in the system that can receive notifications and approve requests.
 * State machine: available <-> busy, available <-> away, any -> offline, offline -> available
 */

import GraphStore, { Node } from '../graph.ts';

// Human states
export type HumanState = 'available' | 'busy' | 'away' | 'offline';

// Human event types
export type HumanEventType =
  | 'HUMAN_CREATED'
  | 'HUMAN_UPDATED'
  | 'HUMAN_STATUS_CHANGED'
  | 'HUMAN_NOTIFIED'
  | 'HUMAN_APPROVED'
  | 'HUMAN_REJECTED';

// Notification channel types
export type NotificationChannel = 'terminal' | 'email' | 'slack';

// Human permissions
export type HumanPermission = 'approve' | 'assign' | 'configure' | 'admin';

// Human preferences
export interface HumanPreferences {
  notificationChannel?: NotificationChannel;
  timezone?: string;
}

// Human config
export interface HumanConfig {
  name: string;
  email?: string;
  preferences?: HumanPreferences;
  permissions?: HumanPermission[];
}

// Human interface
export interface Human {
  id: string;
  type: 'program';
  programType: 'human';
  state: HumanState;  // Presence/availability state (not lifecycle)
  config: HumanConfig;
  version: number;
  created: number;
  modified: number;
}

// Approval request interface
export interface ApprovalRequest {
  id: string;
  humanId: string;
  description: string;
  requestedBy?: string;
  requestedAt: number;
  status: 'pending' | 'approved' | 'rejected';
  resolvedAt?: number;
  reason?: string;
  data?: any;
}

// Notification interface
export interface Notification {
  id: string;
  humanId: string;
  message: string;
  sentAt: number;
  channel: NotificationChannel;
  read: boolean;
}

// Human event structure
export interface HumanEvent {
  id: string;
  timestamp: number;
  type: HumanEventType;
  humanId: string;
  data: any;
}

/**
 * HumanManager - Manages human lifecycle using GraphStore
 */
export class HumanManager {
  private store: GraphStore;
  private humanEvents: HumanEvent[] = [];
  private eventCounter = 0;
  private approvalCounter = 0;
  private notificationCounter = 0;

  // In-memory storage for pending approvals and notifications
  private pendingApprovals: Map<string, ApprovalRequest[]> = new Map();
  private notifications: Map<string, Notification[]> = new Map();

  constructor(store: GraphStore) {
    this.store = store;
  }

  /**
   * Get the underlying GraphStore
   */
  getStore(): GraphStore {
    return this.store;
  }

  /**
   * Generate unique event ID
   */
  private generateEventId(): string {
    return `human_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Generate unique approval ID
   */
  private generateApprovalId(): string {
    return `approval_${Date.now()}_${++this.approvalCounter}`;
  }

  /**
   * Generate unique notification ID
   */
  private generateNotificationId(): string {
    return `notif_${Date.now()}_${++this.notificationCounter}`;
  }

  /**
   * Emit a human event
   */
  private async emitEvent(type: HumanEventType, humanId: string, data: any): Promise<void> {
    const event: HumanEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      humanId,
      data
    };

    this.humanEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'human_event', {
      eventType: type,
      humanId,
      ...data
    });
  }

  /**
   * Create a new human in available state
   */
  async createHuman(
    id: string,
    name: string,
    options: {
      email?: string;
      preferences?: HumanPreferences;
      permissions?: HumanPermission[];
      state?: HumanState;
    } = {}
  ): Promise<Human> {
    // Check if human already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Human already exists: ${id}`);
    }

    const config: HumanConfig = {
      name,
      email: options.email,
      preferences: options.preferences,
      permissions: options.permissions
    };

    const human: Human = {
      id,
      type: 'program',
      programType: 'human',
      state: options.state || 'available',
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'human', {
      programType: 'human',
      name,
      email: options.email,
      preferences: options.preferences ? JSON.stringify(options.preferences) : undefined,
      permissions: options.permissions ? JSON.stringify(options.permissions) : undefined,
      state: human.state,
      version: human.version
    });

    // Initialize approval and notification lists
    this.pendingApprovals.set(id, []);
    this.notifications.set(id, []);

    await this.emitEvent('HUMAN_CREATED', id, {
      name,
      state: human.state,
      version: human.version
    });

    return human;
  }

  /**
   * Get a human by ID
   */
  getHuman(id: string): Human | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'human') {
      return null;
    }

    return this.nodeToHuman(node);
  }

  /**
   * Convert a Node to a Human
   */
  private nodeToHuman(node: Node): Human {
    const preferencesStr = node.properties.get('preferences');
    const permissionsStr = node.properties.get('permissions');

    return {
      id: node.id,
      type: 'program',
      programType: 'human',
      state: (node.properties.get('state') || 'available') as HumanState,
      config: {
        name: node.properties.get('name') || node.id,
        email: node.properties.get('email'),
        preferences: preferencesStr ? JSON.parse(preferencesStr) : undefined,
        permissions: permissionsStr ? JSON.parse(permissionsStr) : undefined
      },
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Update a human's data
   */
  async updateHuman(
    id: string,
    updates: {
      name?: string;
      email?: string;
      preferences?: HumanPreferences;
      permissions?: HumanPermission[];
    }
  ): Promise<Human> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'human') {
      throw new Error(`Human not found: ${id}`);
    }

    // Build properties to update
    const propsToUpdate: Record<string, any> = {};

    if (updates.name !== undefined) {
      propsToUpdate.name = updates.name;
    }
    if (updates.email !== undefined) {
      propsToUpdate.email = updates.email;
    }
    if (updates.preferences !== undefined) {
      propsToUpdate.preferences = JSON.stringify(updates.preferences);
    }
    if (updates.permissions !== undefined) {
      propsToUpdate.permissions = JSON.stringify(updates.permissions);
    }

    // Increment version
    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    // Persist the update
    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('HUMAN_UPDATED', id, {
      updates: Object.keys(updates),
      newVersion: currentVersion + 1
    });

    return this.getHuman(id)!;
  }

  /**
   * Set human status
   * State transitions:
   * - available <-> busy (working on something)
   * - available <-> away (short absence)
   * - any -> offline (disconnected)
   * - offline -> available (reconnect)
   */
  async setStatus(id: string, newState: HumanState): Promise<Human> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'human') {
      throw new Error(`Human not found: ${id}`);
    }

    const currentState = node.properties.get('state') as HumanState;

    // Validate state transitions
    const validTransitions: Record<HumanState, HumanState[]> = {
      'available': ['busy', 'away', 'offline'],
      'busy': ['available', 'offline'],
      'away': ['available', 'offline'],
      'offline': ['available']
    };

    if (currentState === newState) {
      // No change needed
      return this.getHuman(id)!;
    }

    if (!validTransitions[currentState]?.includes(newState)) {
      throw new Error(`Invalid state transition: ${currentState} -> ${newState}`);
    }

    // Persist the state change
    await this.store.updateNode(id, { state: newState });

    await this.emitEvent('HUMAN_STATUS_CHANGED', id, {
      previousState: currentState,
      newState
    });

    return this.getHuman(id)!;
  }

  /**
   * Send a notification to a human
   */
  async notify(
    id: string,
    message: string,
    options: {
      channel?: NotificationChannel;
    } = {}
  ): Promise<Notification> {
    const human = this.getHuman(id);
    if (!human) {
      throw new Error(`Human not found: ${id}`);
    }

    // Determine channel (from options, preferences, or default)
    const channel = options.channel ||
      human.config.preferences?.notificationChannel ||
      'terminal';

    const notification: Notification = {
      id: this.generateNotificationId(),
      humanId: id,
      message,
      sentAt: Date.now(),
      channel,
      read: false
    };

    // Store the notification
    if (!this.notifications.has(id)) {
      this.notifications.set(id, []);
    }
    this.notifications.get(id)!.push(notification);

    // Log to console for terminal channel (simple implementation)
    if (channel === 'terminal') {
      console.log(`[NOTIFICATION to ${human.config.name}] ${message}`);
    }

    await this.emitEvent('HUMAN_NOTIFIED', id, {
      notificationId: notification.id,
      message,
      channel
    });

    return notification;
  }

  /**
   * Request approval from a human
   */
  async requestApproval(
    id: string,
    request: {
      description: string;
      requestedBy?: string;
      data?: any;
    }
  ): Promise<ApprovalRequest> {
    const human = this.getHuman(id);
    if (!human) {
      throw new Error(`Human not found: ${id}`);
    }

    // Check if human has approve permission
    if (human.config.permissions && !human.config.permissions.includes('approve')) {
      throw new Error(`Human ${id} does not have approval permission`);
    }

    const approval: ApprovalRequest = {
      id: this.generateApprovalId(),
      humanId: id,
      description: request.description,
      requestedBy: request.requestedBy,
      requestedAt: Date.now(),
      status: 'pending',
      data: request.data
    };

    // Store the pending approval
    if (!this.pendingApprovals.has(id)) {
      this.pendingApprovals.set(id, []);
    }
    this.pendingApprovals.get(id)!.push(approval);

    // Send a notification about the approval request
    await this.notify(id, `Approval requested: ${request.description}`);

    return approval;
  }

  /**
   * Approve a pending request
   */
  async approve(humanId: string, approvalId: string): Promise<ApprovalRequest> {
    const human = this.getHuman(humanId);
    if (!human) {
      throw new Error(`Human not found: ${humanId}`);
    }

    const approvals = this.pendingApprovals.get(humanId) || [];
    const approval = approvals.find(a => a.id === approvalId);

    if (!approval) {
      throw new Error(`Approval request not found: ${approvalId}`);
    }

    if (approval.status !== 'pending') {
      throw new Error(`Approval request ${approvalId} is already ${approval.status}`);
    }

    approval.status = 'approved';
    approval.resolvedAt = Date.now();

    await this.emitEvent('HUMAN_APPROVED', humanId, {
      approvalId,
      description: approval.description,
      requestedBy: approval.requestedBy
    });

    return approval;
  }

  /**
   * Reject a pending request
   */
  async reject(humanId: string, approvalId: string, reason?: string): Promise<ApprovalRequest> {
    const human = this.getHuman(humanId);
    if (!human) {
      throw new Error(`Human not found: ${humanId}`);
    }

    const approvals = this.pendingApprovals.get(humanId) || [];
    const approval = approvals.find(a => a.id === approvalId);

    if (!approval) {
      throw new Error(`Approval request not found: ${approvalId}`);
    }

    if (approval.status !== 'pending') {
      throw new Error(`Approval request ${approvalId} is already ${approval.status}`);
    }

    approval.status = 'rejected';
    approval.resolvedAt = Date.now();
    approval.reason = reason;

    await this.emitEvent('HUMAN_REJECTED', humanId, {
      approvalId,
      description: approval.description,
      requestedBy: approval.requestedBy,
      reason
    });

    return approval;
  }

  /**
   * List all humans, optionally filtered by state
   */
  listHumans(state?: HumanState): Human[] {
    const humanNodes = this.store.getByType('human');
    let humans = humanNodes.map(node => this.nodeToHuman(node));

    if (state) {
      humans = humans.filter(h => h.state === state);
    }

    return humans;
  }

  /**
   * Get pending approvals for a human
   */
  getPendingApprovals(humanId: string): ApprovalRequest[] {
    return (this.pendingApprovals.get(humanId) || []).filter(a => a.status === 'pending');
  }

  /**
   * Get all approvals for a human (including resolved)
   */
  getAllApprovals(humanId: string): ApprovalRequest[] {
    return this.pendingApprovals.get(humanId) || [];
  }

  /**
   * Get approval by ID
   */
  getApproval(humanId: string, approvalId: string): ApprovalRequest | undefined {
    return (this.pendingApprovals.get(humanId) || []).find(a => a.id === approvalId);
  }

  /**
   * Get notifications for a human
   */
  getNotifications(humanId: string, unreadOnly: boolean = false): Notification[] {
    const notifications = this.notifications.get(humanId) || [];
    if (unreadOnly) {
      return notifications.filter(n => !n.read);
    }
    return notifications;
  }

  /**
   * Get all human events
   */
  getHumanEvents(limit?: number): HumanEvent[] {
    const events = this.humanEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific human
   */
  getHumanEventHistory(humanId: string): HumanEvent[] {
    return this.humanEvents.filter(e => e.humanId === humanId);
  }
}

export default HumanManager;
