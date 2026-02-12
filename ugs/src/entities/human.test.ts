import { test, expect, describe, beforeEach } from 'bun:test';
import { HumanManager, Human, HumanState, HumanPermission } from './human.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('HumanManager', () => {
  let store: GraphStore;
  let manager: HumanManager;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    store = new GraphStore(`/tmp/ugs-human-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    manager = new HumanManager(store);
  });

  describe('createHuman', () => {
    test('creates a human in available state by default', async () => {
      const human = await manager.createHuman('brian', 'Brian');

      expect(human.id).toBe('brian');
      expect(human.config.name).toBe('Brian');
      expect(human.state).toBe('available');
      expect(human.version).toBe(1);
      expect(human.type).toBe('program');
      expect(human.programType).toBe('human');
    });

    test('creates a human with custom options', async () => {
      const human = await manager.createHuman('alice', 'Alice', {
        email: 'alice@example.com',
        preferences: {
          notificationChannel: 'email',
          timezone: 'America/New_York'
        },
        permissions: ['approve', 'assign']
      });

      expect(human.config.name).toBe('Alice');
      expect(human.config.email).toBe('alice@example.com');
      expect(human.config.preferences?.notificationChannel).toBe('email');
      expect(human.config.preferences?.timezone).toBe('America/New_York');
      expect(human.config.permissions).toContain('approve');
      expect(human.config.permissions).toContain('assign');
    });

    test('creates a human with initial state', async () => {
      const human = await manager.createHuman('bob', 'Bob', {
        state: 'busy'
      });

      expect(human.state).toBe('busy');
    });

    test('throws error if human already exists', async () => {
      await manager.createHuman('dup-human', 'Dup');

      await expect(manager.createHuman('dup-human', 'Dup'))
        .rejects.toThrow('Human already exists: dup-human');
    });

    test('emits HUMAN_CREATED event', async () => {
      await manager.createHuman('event-test', 'Test');

      const events = manager.getHumanEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'HUMAN_CREATED' && e.humanId === 'event-test');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.state).toBe('available');
    });
  });

  describe('getHuman', () => {
    test('returns null for non-existent human', () => {
      const human = manager.getHuman('nonexistent');
      expect(human).toBeNull();
    });

    test('retrieves an existing human', async () => {
      await manager.createHuman('get-test', 'Get Test', {
        email: 'test@example.com'
      });

      const human = manager.getHuman('get-test');
      expect(human).not.toBeNull();
      expect(human!.id).toBe('get-test');
      expect(human!.config.email).toBe('test@example.com');
    });
  });

  describe('updateHuman', () => {
    test('updates human data', async () => {
      await manager.createHuman('update-test', 'Original Name');

      const updated = await manager.updateHuman('update-test', {
        name: 'New Name',
        email: 'new@example.com'
      });

      expect(updated.config.name).toBe('New Name');
      expect(updated.config.email).toBe('new@example.com');
      expect(updated.version).toBe(2);
    });

    test('updates human permissions', async () => {
      await manager.createHuman('perm-test', 'Perm Test');

      const updated = await manager.updateHuman('perm-test', {
        permissions: ['approve', 'assign', 'admin']
      });

      expect(updated.config.permissions).toContain('approve');
      expect(updated.config.permissions).toContain('admin');
    });

    test('throws error when updating non-existent human', async () => {
      await expect(manager.updateHuman('nonexistent', { name: 'test' }))
        .rejects.toThrow('Human not found: nonexistent');
    });

    test('emits HUMAN_UPDATED event', async () => {
      await manager.createHuman('update-evt', 'Test');
      await manager.updateHuman('update-evt', { name: 'Updated' });

      const events = manager.getHumanEventHistory('update-evt');
      const updateEvent = events.find(e => e.type === 'HUMAN_UPDATED');
      expect(updateEvent).toBeDefined();
      expect(updateEvent!.data.newVersion).toBe(2);
    });
  });

  describe('setStatus', () => {
    test('transitions from available to busy', async () => {
      await manager.createHuman('status-test', 'Test');

      const updated = await manager.setStatus('status-test', 'busy');

      expect(updated.state).toBe('busy');
    });

    test('transitions from available to away', async () => {
      await manager.createHuman('away-test', 'Test');

      const updated = await manager.setStatus('away-test', 'away');

      expect(updated.state).toBe('away');
    });

    test('transitions from busy to available', async () => {
      await manager.createHuman('back-test', 'Test');
      await manager.setStatus('back-test', 'busy');

      const updated = await manager.setStatus('back-test', 'available');

      expect(updated.state).toBe('available');
    });

    test('transitions any state to offline', async () => {
      await manager.createHuman('offline-test', 'Test');
      await manager.setStatus('offline-test', 'busy');

      const updated = await manager.setStatus('offline-test', 'offline');

      expect(updated.state).toBe('offline');
    });

    test('transitions from offline to available', async () => {
      await manager.createHuman('reconnect-test', 'Test');
      await manager.setStatus('reconnect-test', 'offline');

      const updated = await manager.setStatus('reconnect-test', 'available');

      expect(updated.state).toBe('available');
    });

    test('throws error for invalid state transition', async () => {
      await manager.createHuman('invalid-trans', 'Test');
      await manager.setStatus('invalid-trans', 'offline');

      // offline -> busy should be invalid
      await expect(manager.setStatus('invalid-trans', 'busy'))
        .rejects.toThrow('Invalid state transition: offline -> busy');
    });

    test('same state is a no-op', async () => {
      await manager.createHuman('same-state', 'Test');

      const updated = await manager.setStatus('same-state', 'available');

      expect(updated.state).toBe('available');
    });

    test('emits HUMAN_STATUS_CHANGED event', async () => {
      await manager.createHuman('status-evt', 'Test');
      await manager.setStatus('status-evt', 'busy');

      const events = manager.getHumanEventHistory('status-evt');
      const statusEvent = events.find(e => e.type === 'HUMAN_STATUS_CHANGED');
      expect(statusEvent).toBeDefined();
      expect(statusEvent!.data.previousState).toBe('available');
      expect(statusEvent!.data.newState).toBe('busy');
    });
  });

  describe('notify', () => {
    test('sends a notification to a human', async () => {
      await manager.createHuman('notify-test', 'Test');

      const notification = await manager.notify('notify-test', 'Hello, Test!');

      expect(notification.humanId).toBe('notify-test');
      expect(notification.message).toBe('Hello, Test!');
      expect(notification.channel).toBe('terminal');
      expect(notification.read).toBe(false);
    });

    test('uses preferred channel from human preferences', async () => {
      await manager.createHuman('pref-notify', 'Test', {
        preferences: { notificationChannel: 'email' }
      });

      const notification = await manager.notify('pref-notify', 'Test message');

      expect(notification.channel).toBe('email');
    });

    test('allows overriding channel', async () => {
      await manager.createHuman('override-notify', 'Test', {
        preferences: { notificationChannel: 'email' }
      });

      const notification = await manager.notify('override-notify', 'Test', { channel: 'slack' });

      expect(notification.channel).toBe('slack');
    });

    test('throws error for non-existent human', async () => {
      await expect(manager.notify('nonexistent', 'Hello'))
        .rejects.toThrow('Human not found: nonexistent');
    });

    test('emits HUMAN_NOTIFIED event', async () => {
      await manager.createHuman('notif-evt', 'Test');
      await manager.notify('notif-evt', 'Test notification');

      const events = manager.getHumanEventHistory('notif-evt');
      const notifyEvent = events.find(e => e.type === 'HUMAN_NOTIFIED');
      expect(notifyEvent).toBeDefined();
      expect(notifyEvent!.data.message).toBe('Test notification');
    });

    test('stores notifications for retrieval', async () => {
      await manager.createHuman('stored-notify', 'Test');
      await manager.notify('stored-notify', 'Message 1');
      await manager.notify('stored-notify', 'Message 2');

      const notifications = manager.getNotifications('stored-notify');

      expect(notifications.length).toBe(2);
    });
  });

  describe('requestApproval', () => {
    test('creates a pending approval request', async () => {
      await manager.createHuman('approver', 'Approver', {
        permissions: ['approve']
      });

      const approval = await manager.requestApproval('approver', {
        description: 'Deploy to production',
        requestedBy: 'agent-1'
      });

      expect(approval.status).toBe('pending');
      expect(approval.description).toBe('Deploy to production');
      expect(approval.requestedBy).toBe('agent-1');
      expect(approval.humanId).toBe('approver');
    });

    test('stores pending approvals for retrieval', async () => {
      await manager.createHuman('multi-approver', 'Approver', {
        permissions: ['approve']
      });
      await manager.requestApproval('multi-approver', { description: 'Request 1' });
      await manager.requestApproval('multi-approver', { description: 'Request 2' });

      const pending = manager.getPendingApprovals('multi-approver');

      expect(pending.length).toBe(2);
    });

    test('throws error if human lacks approve permission', async () => {
      await manager.createHuman('no-perm', 'No Perm', {
        permissions: ['assign']
      });

      await expect(manager.requestApproval('no-perm', { description: 'Test' }))
        .rejects.toThrow('Human no-perm does not have approval permission');
    });

    test('allows approval request if human has no permissions set', async () => {
      await manager.createHuman('any-approver', 'Any');

      // Should work - no explicit permissions means all allowed
      const approval = await manager.requestApproval('any-approver', { description: 'Test' });
      expect(approval.status).toBe('pending');
    });
  });

  describe('approve', () => {
    test('approves a pending request', async () => {
      await manager.createHuman('approve-test', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('approve-test', {
        description: 'Deploy request'
      });

      const approved = await manager.approve('approve-test', pending.id);

      expect(approved.status).toBe('approved');
      expect(approved.resolvedAt).toBeDefined();
    });

    test('removes from pending list after approval', async () => {
      await manager.createHuman('remove-pending', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('remove-pending', {
        description: 'Deploy request'
      });
      await manager.approve('remove-pending', pending.id);

      const remainingPending = manager.getPendingApprovals('remove-pending');

      expect(remainingPending.length).toBe(0);
    });

    test('throws error for non-existent approval', async () => {
      await manager.createHuman('no-approval', 'Test');

      await expect(manager.approve('no-approval', 'fake-id'))
        .rejects.toThrow('Approval request not found: fake-id');
    });

    test('throws error for already resolved approval', async () => {
      await manager.createHuman('already-approved', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('already-approved', {
        description: 'Test'
      });
      await manager.approve('already-approved', pending.id);

      await expect(manager.approve('already-approved', pending.id))
        .rejects.toThrow(`Approval request ${pending.id} is already approved`);
    });

    test('emits HUMAN_APPROVED event', async () => {
      await manager.createHuman('approve-evt', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('approve-evt', {
        description: 'Test'
      });
      await manager.approve('approve-evt', pending.id);

      const events = manager.getHumanEventHistory('approve-evt');
      const approveEvent = events.find(e => e.type === 'HUMAN_APPROVED');
      expect(approveEvent).toBeDefined();
      expect(approveEvent!.data.approvalId).toBe(pending.id);
    });
  });

  describe('reject', () => {
    test('rejects a pending request', async () => {
      await manager.createHuman('reject-test', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('reject-test', {
        description: 'Deploy request'
      });

      const rejected = await manager.reject('reject-test', pending.id, 'Not ready');

      expect(rejected.status).toBe('rejected');
      expect(rejected.reason).toBe('Not ready');
      expect(rejected.resolvedAt).toBeDefined();
    });

    test('throws error for already resolved approval', async () => {
      await manager.createHuman('already-rejected', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('already-rejected', {
        description: 'Test'
      });
      await manager.reject('already-rejected', pending.id);

      await expect(manager.reject('already-rejected', pending.id))
        .rejects.toThrow(`Approval request ${pending.id} is already rejected`);
    });

    test('emits HUMAN_REJECTED event', async () => {
      await manager.createHuman('reject-evt', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('reject-evt', {
        description: 'Test'
      });
      await manager.reject('reject-evt', pending.id, 'Denied');

      const events = manager.getHumanEventHistory('reject-evt');
      const rejectEvent = events.find(e => e.type === 'HUMAN_REJECTED');
      expect(rejectEvent).toBeDefined();
      expect(rejectEvent!.data.reason).toBe('Denied');
    });
  });

  describe('listHumans', () => {
    test('lists all humans', async () => {
      await manager.createHuman('list-1', 'Human 1');
      await manager.createHuman('list-2', 'Human 2');
      await manager.createHuman('list-3', 'Human 3');

      const humans = manager.listHumans();

      expect(humans.length).toBe(3);
    });

    test('filters humans by state', async () => {
      await manager.createHuman('avail-1', 'Available 1');
      await manager.createHuman('avail-2', 'Available 2');
      await manager.createHuman('busy-1', 'Busy 1');
      await manager.setStatus('busy-1', 'busy');

      const available = manager.listHumans('available');
      const busy = manager.listHumans('busy');

      expect(available.length).toBe(2);
      expect(busy.length).toBe(1);
      expect(busy[0].id).toBe('busy-1');
    });
  });

  describe('state machine enforcement', () => {
    test('available -> busy transition is valid', async () => {
      await manager.createHuman('sm-1', 'Test');
      const updated = await manager.setStatus('sm-1', 'busy');
      expect(updated.state).toBe('busy');
    });

    test('available -> away transition is valid', async () => {
      await manager.createHuman('sm-2', 'Test');
      const updated = await manager.setStatus('sm-2', 'away');
      expect(updated.state).toBe('away');
    });

    test('available -> offline transition is valid', async () => {
      await manager.createHuman('sm-3', 'Test');
      const updated = await manager.setStatus('sm-3', 'offline');
      expect(updated.state).toBe('offline');
    });

    test('busy -> available transition is valid', async () => {
      await manager.createHuman('sm-4', 'Test');
      await manager.setStatus('sm-4', 'busy');
      const updated = await manager.setStatus('sm-4', 'available');
      expect(updated.state).toBe('available');
    });

    test('busy -> away transition is invalid', async () => {
      await manager.createHuman('sm-5', 'Test');
      await manager.setStatus('sm-5', 'busy');
      await expect(manager.setStatus('sm-5', 'away'))
        .rejects.toThrow('Invalid state transition: busy -> away');
    });

    test('away -> busy transition is invalid', async () => {
      await manager.createHuman('sm-6', 'Test');
      await manager.setStatus('sm-6', 'away');
      await expect(manager.setStatus('sm-6', 'busy'))
        .rejects.toThrow('Invalid state transition: away -> busy');
    });

    test('offline -> busy transition is invalid', async () => {
      await manager.createHuman('sm-7', 'Test');
      await manager.setStatus('sm-7', 'offline');
      await expect(manager.setStatus('sm-7', 'busy'))
        .rejects.toThrow('Invalid state transition: offline -> busy');
    });

    test('offline -> away transition is invalid', async () => {
      await manager.createHuman('sm-8', 'Test');
      await manager.setStatus('sm-8', 'offline');
      await expect(manager.setStatus('sm-8', 'away'))
        .rejects.toThrow('Invalid state transition: offline -> away');
    });
  });

  describe('getApproval', () => {
    test('retrieves approval by ID', async () => {
      await manager.createHuman('get-approval', 'Approver', {
        permissions: ['approve']
      });
      const pending = await manager.requestApproval('get-approval', {
        description: 'Test request',
        data: { foo: 'bar' }
      });

      const retrieved = manager.getApproval('get-approval', pending.id);

      expect(retrieved).toBeDefined();
      expect(retrieved!.description).toBe('Test request');
      expect(retrieved!.data.foo).toBe('bar');
    });
  });

  describe('getAllApprovals', () => {
    test('retrieves all approvals including resolved', async () => {
      await manager.createHuman('all-approvals', 'Approver', {
        permissions: ['approve']
      });
      const pending1 = await manager.requestApproval('all-approvals', { description: 'Req 1' });
      const pending2 = await manager.requestApproval('all-approvals', { description: 'Req 2' });
      await manager.approve('all-approvals', pending1.id);

      const all = manager.getAllApprovals('all-approvals');
      const pending = manager.getPendingApprovals('all-approvals');

      expect(all.length).toBe(2);
      expect(pending.length).toBe(1);
    });
  });
});
