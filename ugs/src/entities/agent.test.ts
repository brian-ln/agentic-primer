import { test, expect, describe, beforeEach, mock } from 'bun:test';
import { AgentManager, Agent, AgentState, AgentHarness, AgentStreamingOptions } from './agent.ts';
import { TaskManager } from './task.ts';
import { ModelManager } from './model.ts';
import { ProviderManager } from './provider.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('AgentManager', () => {
  let store: GraphStore;
  let taskManager: TaskManager;
  let providerManager: ProviderManager;
  let modelManager: ModelManager;
  let agentManager: AgentManager;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    store = new GraphStore(`/tmp/ugs-agent-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    taskManager = new TaskManager(store);
    providerManager = new ProviderManager(store);
    modelManager = new ModelManager(store, providerManager);
    agentManager = new AgentManager(store, taskManager, modelManager);

    // Create a default provider and model for tests
    await providerManager.createProvider('test-provider', 'cloudflare-ai-gateway', {
      accountId: 'test-account',
      gatewayId: 'test-gateway'
    });
    await providerManager.publishProvider('test-provider');

    // Create claude-balanced (the default agent model)
    await modelManager.createModel('claude-balanced', 'claude-sonnet-4-5', 'test-provider');
    await modelManager.publishModel('claude-balanced');

    // Mock the model invocation to avoid real API calls
    modelManager.invokeModel = mock(async (id, opts) => {
      // Simulate streaming if requested
      if (opts?.stream && opts?.onToken) {
        opts.onToken('Mocked');
        opts.onToken(' response');
      }
      return {
        success: true,
        text: 'Mocked response for testing',
        model: 'claude-sonnet-4-5',
        usage: { promptTokens: 10, completionTokens: 15, totalTokens: 25 },
        duration: 100,
        timestamp: Date.now()
      };
    });
  });

  describe('createAgent', () => {
    test('creates an agent in idle state', async () => {
      const agent = await agentManager.createAgent(
        'test-agent',
        'Test Agent',
        'You are a helpful test agent.'
      );

      expect(agent.id).toBe('test-agent');
      expect(agent.config.name).toBe('Test Agent');
      expect(agent.config.systemPrompt).toBe('You are a helpful test agent.');
      expect(agent.state).toBe('idle');
      expect(agent.programType).toBe('agent');
      expect(agent.version).toBe(1);
    });

    test('creates an agent with custom options', async () => {
      const agent = await agentManager.createAgent(
        'custom-agent',
        'Custom Agent',
        'You are a custom agent.',
        {
          tools: ['@(tool1)', '@(tool2)'],
          defaultModel: '@(gpt-4)',
          harness: {
            maxTurns: 100,
            checkpointEvery: 10
          }
        }
      );

      expect(agent.config.tools).toEqual(['@(tool1)', '@(tool2)']);
      expect(agent.config.defaultModel).toBe('@(gpt-4)');
      expect(agent.config.harness.maxTurns).toBe(100);
      expect(agent.config.harness.checkpointEvery).toBe(10);
    });

    test('throws error if agent already exists', async () => {
      await agentManager.createAgent('dup-agent', 'Dup', 'prompt');

      await expect(agentManager.createAgent('dup-agent', 'Dup2', 'prompt2'))
        .rejects.toThrow('Agent already exists: dup-agent');
    });

    test('emits AGENT_CREATED event', async () => {
      await agentManager.createAgent('evt-agent', 'Event Agent', 'prompt');

      const events = agentManager.getAgentEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'AGENT_CREATED' && e.agentId === 'evt-agent');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.name).toBe('Event Agent');
      expect(createEvent!.data.state).toBe('idle');
    });

    test('uses default harness values', async () => {
      const agent = await agentManager.createAgent('default-agent', 'Default', 'prompt');

      expect(agent.config.harness.maxTurns).toBe(50);
      expect(agent.config.harness.reflectOnFailure).toBe(true);
      expect(agent.config.harness.checkpointEvery).toBe(5);
    });
  });

  describe('getAgent', () => {
    test('returns null for non-existent agent', () => {
      const agent = agentManager.getAgent('nonexistent');
      expect(agent).toBeNull();
    });

    test('retrieves an existing agent', async () => {
      await agentManager.createAgent('get-test', 'Get Test', 'prompt');

      const agent = agentManager.getAgent('get-test');
      expect(agent).not.toBeNull();
      expect(agent!.id).toBe('get-test');
      expect(agent!.config.name).toBe('Get Test');
    });
  });

  describe('configureAgent', () => {
    test('configures an idle agent', async () => {
      await agentManager.createAgent('config-agent', 'Config', 'old prompt');

      const updated = await agentManager.configureAgent('config-agent', {
        name: 'Reconfigured Agent',
        systemPrompt: 'new prompt',
        harness: { maxTurns: 200 }
      });

      expect(updated.config.name).toBe('Reconfigured Agent');
      expect(updated.config.systemPrompt).toBe('new prompt');
      expect(updated.config.harness.maxTurns).toBe(200);
      expect(updated.version).toBe(2);
    });

    test('throws error when configuring non-existent agent', async () => {
      await expect(agentManager.configureAgent('nonexistent', { name: 'Test' }))
        .rejects.toThrow('Agent not found: nonexistent');
    });

    test('throws error when configuring non-idle agent', async () => {
      await agentManager.createAgent('busy-agent', 'Busy', 'prompt');
      await taskManager.createTask('test-task', 'Test Task');
      await agentManager.assignTask('busy-agent', 'test-task');

      await expect(agentManager.configureAgent('busy-agent', { name: 'New Name' }))
        .rejects.toThrow('Cannot configure agent in thinking state');
    });

    test('emits AGENT_CONFIGURED event', async () => {
      await agentManager.createAgent('config-evt', 'Config Evt', 'prompt');
      await agentManager.configureAgent('config-evt', { harness: { maxTurns: 75 } });

      const events = agentManager.getAgentEventHistory('config-evt');
      const configEvent = events.find(e => e.type === 'AGENT_CONFIGURED');
      expect(configEvent).toBeDefined();
      expect(configEvent!.data.newVersion).toBe(2);
    });
  });

  describe('assignTask', () => {
    test('assigns a task to an idle agent', async () => {
      await agentManager.createAgent('assign-agent', 'Assign', 'prompt');
      await taskManager.createTask('assign-task', 'Assign Task');

      const agent = await agentManager.assignTask('assign-agent', 'assign-task');

      expect(agent.state).toBe('thinking');
      expect(agent.config.currentTask).toBe('assign-task');
      expect(agent.config.turnCount).toBe(0);
    });

    test('throws error when assigning to non-idle agent', async () => {
      await agentManager.createAgent('busy-assign', 'Busy', 'prompt');
      await taskManager.createTask('task1', 'Task 1');
      await taskManager.createTask('task2', 'Task 2');
      await agentManager.assignTask('busy-assign', 'task1');

      await expect(agentManager.assignTask('busy-assign', 'task2'))
        .rejects.toThrow('Cannot assign task to agent in thinking state');
    });

    test('throws error when task does not exist', async () => {
      await agentManager.createAgent('no-task-agent', 'No Task', 'prompt');

      await expect(agentManager.assignTask('no-task-agent', 'nonexistent-task'))
        .rejects.toThrow('Task not found: nonexistent-task');
    });

    test('emits AGENT_TASK_ASSIGNED event', async () => {
      await agentManager.createAgent('task-evt-agent', 'Task Evt', 'prompt');
      await taskManager.createTask('task-evt', 'Task Evt');
      await agentManager.assignTask('task-evt-agent', 'task-evt');

      const events = agentManager.getAgentEventHistory('task-evt-agent');
      const assignEvent = events.find(e => e.type === 'AGENT_TASK_ASSIGNED');
      expect(assignEvent).toBeDefined();
      expect(assignEvent!.data.taskId).toBe('task-evt');
      expect(assignEvent!.data.newState).toBe('thinking');
    });
  });

  describe('step', () => {
    test('executes one turn and increments count', async () => {
      await agentManager.createAgent('step-agent', 'Step', 'prompt');
      await taskManager.createTask('step-task', 'Step Task');
      await agentManager.assignTask('step-agent', 'step-task');

      const result = await agentManager.step('step-agent');

      expect(result.done).toBe(false);
      expect(result.turn).toBe(1);

      const agent = agentManager.getAgent('step-agent');
      expect(agent!.config.turnCount).toBe(1);
    });

    test('throws error when stepping non-thinking agent', async () => {
      await agentManager.createAgent('idle-step', 'Idle Step', 'prompt');

      await expect(agentManager.step('idle-step'))
        .rejects.toThrow('Cannot step agent in idle state');
    });

    test('completes when max turns reached', async () => {
      await agentManager.createAgent('max-turns-agent', 'Max Turns', 'prompt', {
        harness: { maxTurns: 2 }
      });
      await taskManager.createTask('max-task', 'Max Task');
      await agentManager.assignTask('max-turns-agent', 'max-task');

      await agentManager.step('max-turns-agent'); // Turn 1
      await agentManager.step('max-turns-agent'); // Turn 2
      const result = await agentManager.step('max-turns-agent'); // Turn 3 - exceeds

      expect(result.done).toBe(true);
      expect(result.reason).toBe('max_turns_reached');

      const agent = agentManager.getAgent('max-turns-agent');
      expect(agent!.state).toBe('completed');
    });

    test('emits AGENT_THINKING event', async () => {
      await agentManager.createAgent('think-evt', 'Think Evt', 'prompt');
      await taskManager.createTask('think-task', 'Think Task');
      await agentManager.assignTask('think-evt', 'think-task');

      await agentManager.step('think-evt');

      const events = agentManager.getAgentEventHistory('think-evt');
      const thinkEvent = events.find(e => e.type === 'AGENT_THINKING');
      expect(thinkEvent).toBeDefined();
      expect(thinkEvent!.data.turn).toBe(1);
    });

    test('emits AGENT_CHECKPOINT event at intervals', async () => {
      await agentManager.createAgent('checkpoint-agent', 'Checkpoint', 'prompt', {
        harness: { checkpointEvery: 2 }
      });
      await taskManager.createTask('checkpoint-task', 'Checkpoint Task');
      await agentManager.assignTask('checkpoint-agent', 'checkpoint-task');

      await agentManager.step('checkpoint-agent'); // Turn 1 - no checkpoint
      await agentManager.step('checkpoint-agent'); // Turn 2 - checkpoint

      const events = agentManager.getAgentEventHistory('checkpoint-agent');
      const checkpointEvent = events.find(e => e.type === 'AGENT_CHECKPOINT');
      expect(checkpointEvent).toBeDefined();
      expect(checkpointEvent!.data.turn).toBe(2);
    });

    test('AgentStreamingOptions interface is usable', () => {
      // Verify the streaming options interface works correctly
      const opts: AgentStreamingOptions = {
        stream: true,
        onToken: (token) => console.log(token)
      };
      expect(opts.stream).toBe(true);
      expect(typeof opts.onToken).toBe('function');
    });

    test('step accepts streaming options', async () => {
      await agentManager.createAgent('stream-step', 'Stream Step', 'prompt');
      await taskManager.createTask('stream-task', 'Stream Task');
      await agentManager.assignTask('stream-step', 'stream-task');

      const tokens: string[] = [];
      const result = await agentManager.step('stream-step', {
        stream: true,
        onToken: (token) => tokens.push(token)
      });

      // The step should complete with mocked response
      expect(result.done).toBe(false);
      expect(result.turn).toBe(1);
      expect(tokens.length).toBeGreaterThan(0);
    });

    test('step works without streaming options', async () => {
      await agentManager.createAgent('no-stream-step', 'No Stream', 'prompt');
      await taskManager.createTask('no-stream-task', 'No Stream Task');
      await agentManager.assignTask('no-stream-step', 'no-stream-task');

      // Verify step still works without any streaming options
      const result = await agentManager.step('no-stream-step');

      expect(result.done).toBe(false);
      expect(result.turn).toBe(1);
    });
  });

  describe('pause and resume', () => {
    test('pauses a thinking agent', async () => {
      await agentManager.createAgent('pause-agent', 'Pause', 'prompt');
      await taskManager.createTask('pause-task', 'Pause Task');
      await agentManager.assignTask('pause-agent', 'pause-task');

      const paused = await agentManager.pause('pause-agent', 'user requested');

      expect(paused.state).toBe('waiting');
    });

    test('resumes a waiting agent', async () => {
      await agentManager.createAgent('resume-agent', 'Resume', 'prompt');
      await taskManager.createTask('resume-task', 'Resume Task');
      await agentManager.assignTask('resume-agent', 'resume-task');
      await agentManager.pause('resume-agent');

      const resumed = await agentManager.resume('resume-agent', 'user input');

      expect(resumed.state).toBe('thinking');
    });

    test('throws error when pausing idle agent', async () => {
      await agentManager.createAgent('idle-pause', 'Idle Pause', 'prompt');

      await expect(agentManager.pause('idle-pause'))
        .rejects.toThrow('Cannot pause agent in idle state');
    });

    test('throws error when resuming non-waiting agent', async () => {
      await agentManager.createAgent('not-waiting', 'Not Waiting', 'prompt');

      await expect(agentManager.resume('not-waiting'))
        .rejects.toThrow('Cannot resume agent in idle state');
    });

    test('emits AGENT_WAITING event on pause', async () => {
      await agentManager.createAgent('wait-evt', 'Wait Evt', 'prompt');
      await taskManager.createTask('wait-task', 'Wait Task');
      await agentManager.assignTask('wait-evt', 'wait-task');
      await agentManager.pause('wait-evt', 'testing');

      const events = agentManager.getAgentEventHistory('wait-evt');
      const waitEvent = events.find(e => e.type === 'AGENT_WAITING');
      expect(waitEvent).toBeDefined();
      expect(waitEvent!.data.reason).toBe('testing');
    });
  });

  describe('stop and reset', () => {
    test('stops a running agent', async () => {
      await agentManager.createAgent('stop-agent', 'Stop', 'prompt');
      await taskManager.createTask('stop-task', 'Stop Task');
      await agentManager.assignTask('stop-agent', 'stop-task');

      const stopped = await agentManager.stop('stop-agent');

      expect(stopped.state).toBe('idle');
      expect(stopped.config.currentTask).toBeFalsy();
      expect(stopped.config.turnCount).toBe(0);
    });

    test('resets agent to clean state', async () => {
      await agentManager.createAgent('reset-agent', 'Reset', 'prompt');
      await taskManager.createTask('reset-task', 'Reset Task');
      await agentManager.assignTask('reset-agent', 'reset-task');
      await agentManager.step('reset-agent');
      await agentManager.step('reset-agent');

      const reset = await agentManager.reset('reset-agent');

      expect(reset.state).toBe('idle');
      expect(reset.config.turnCount).toBe(0);
      expect(reset.config.currentTask).toBeFalsy();
    });

    test('emits AGENT_RESET event', async () => {
      await agentManager.createAgent('reset-evt', 'Reset Evt', 'prompt');
      await agentManager.reset('reset-evt');

      const events = agentManager.getAgentEventHistory('reset-evt');
      const resetEvent = events.find(e => e.type === 'AGENT_RESET');
      expect(resetEvent).toBeDefined();
      expect(resetEvent!.data.reason).toBe('reset');
    });
  });

  describe('setError', () => {
    test('transitions agent to error state', async () => {
      await agentManager.createAgent('error-agent', 'Error', 'prompt');

      const errored = await agentManager.setError('error-agent', 'Something went wrong');

      expect(errored.state).toBe('error');
      expect(errored.config.lastError).toBe('Something went wrong');
    });

    test('emits AGENT_ERROR event', async () => {
      await agentManager.createAgent('error-evt', 'Error Evt', 'prompt');
      await agentManager.setError('error-evt', 'Test error');

      const events = agentManager.getAgentEventHistory('error-evt');
      const errorEvent = events.find(e => e.type === 'AGENT_ERROR');
      expect(errorEvent).toBeDefined();
      expect(errorEvent!.data.error).toBe('Test error');
    });
  });

  describe('getStatus', () => {
    test('returns status summary for existing agent', async () => {
      await agentManager.createAgent('status-agent', 'Status', 'prompt');
      await taskManager.createTask('status-task', 'Status Task');
      await agentManager.assignTask('status-agent', 'status-task');
      await agentManager.step('status-agent');

      const status = agentManager.getStatus('status-agent');

      expect(status).not.toBeNull();
      expect(status!.state).toBe('thinking');
      expect(status!.turnCount).toBe(1);
      expect(status!.currentTask).toBe('status-task');
    });

    test('returns null for non-existent agent', () => {
      const status = agentManager.getStatus('nonexistent');
      expect(status).toBeNull();
    });
  });

  describe('listAgents', () => {
    test('lists all agents', async () => {
      await agentManager.createAgent('list-1', 'List 1', 'prompt');
      await agentManager.createAgent('list-2', 'List 2', 'prompt');
      await agentManager.createAgent('list-3', 'List 3', 'prompt');

      const agents = agentManager.listAgents();

      expect(agents.length).toBe(3);
    });

    test('filters agents by state', async () => {
      await agentManager.createAgent('idle-1', 'Idle 1', 'prompt');
      await agentManager.createAgent('idle-2', 'Idle 2', 'prompt');
      await agentManager.createAgent('thinking-1', 'Thinking 1', 'prompt');
      await taskManager.createTask('filter-task', 'Filter Task');
      await agentManager.assignTask('thinking-1', 'filter-task');

      const idleAgents = agentManager.listAgents('idle');
      const thinkingAgents = agentManager.listAgents('thinking');

      expect(idleAgents.length).toBe(2);
      expect(thinkingAgents.length).toBe(1);
      expect(thinkingAgents[0].id).toBe('thinking-1');
    });
  });

  describe('state machine enforcement', () => {
    test('idle -> thinking via assignTask', async () => {
      await agentManager.createAgent('sm-1', 'SM 1', 'prompt');
      await taskManager.createTask('sm-task-1', 'SM Task 1');

      const agent = await agentManager.assignTask('sm-1', 'sm-task-1');
      expect(agent.state).toBe('thinking');
    });

    test('thinking -> waiting via pause', async () => {
      await agentManager.createAgent('sm-2', 'SM 2', 'prompt');
      await taskManager.createTask('sm-task-2', 'SM Task 2');
      await agentManager.assignTask('sm-2', 'sm-task-2');

      const agent = await agentManager.pause('sm-2');
      expect(agent.state).toBe('waiting');
    });

    test('waiting -> thinking via resume', async () => {
      await agentManager.createAgent('sm-3', 'SM 3', 'prompt');
      await taskManager.createTask('sm-task-3', 'SM Task 3');
      await agentManager.assignTask('sm-3', 'sm-task-3');
      await agentManager.pause('sm-3');

      const agent = await agentManager.resume('sm-3');
      expect(agent.state).toBe('thinking');
    });

    test('any state -> idle via reset', async () => {
      await agentManager.createAgent('sm-4', 'SM 4', 'prompt');
      await taskManager.createTask('sm-task-4', 'SM Task 4');
      await agentManager.assignTask('sm-4', 'sm-task-4');
      await agentManager.pause('sm-4');

      const agent = await agentManager.reset('sm-4');
      expect(agent.state).toBe('idle');
    });

    test('any state -> error via setError', async () => {
      await agentManager.createAgent('sm-5', 'SM 5', 'prompt');

      const agent = await agentManager.setError('sm-5', 'error');
      expect(agent.state).toBe('error');
    });

    test('completed/error -> idle via reset', async () => {
      await agentManager.createAgent('sm-6', 'SM 6', 'prompt');
      await agentManager.setError('sm-6', 'error');

      const agent = await agentManager.reset('sm-6');
      expect(agent.state).toBe('idle');
    });
  });
});
