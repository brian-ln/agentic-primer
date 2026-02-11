import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { ActorSystem } from '../actor-system.ts';
import { Actor } from '../actor.ts';
import { MessageRouter } from '../router.ts';
import { SupervisorBase, LeafActor } from '../supervisor.ts';
import { LocalTransport } from '../transport/local-transport.ts';
import { JsonSerde } from '../transport/serde.ts';
import type { ActorBehavior, ActorSystemConfig } from '../types.ts';
import type { Message, MessageResponse, Address } from '../message.ts';
import {
  address,
  parseAddress,
  createMessage,
  createResponse,
  createErrorResponse,
} from '../message.ts';
import {
  parsePath,
  validatePath,
  isSafePath,
  matchPattern,
  getParentPath,
  getLocalName,
  joinPath,
  isChildOf,
} from '../routing/path-resolver.ts';

// --- ActorSystem: Functional Behaviors ---

describe('ActorSystem', () => {
  it('spawns an actor and returns an address', () => {
    const system = new ActorSystem({ name: 'test' });
    const addr = system.spawn((s) => s, { count: 0 }, 'counter');
    assert.equal(addr, '@(counter)');
  });

  it('generates UUID address when no name provided', () => {
    const system = new ActorSystem({ name: 'test' });
    const addr = system.spawn((s) => s, {});
    assert.match(addr, /^@\([0-9a-f-]{36}\)$/);
  });

  it('processes messages through behavior', async () => {
    const system = new ActorSystem({ name: 'test' });
    let receivedType: string | null = null;

    const behavior: ActorBehavior<{ count: number }, any> = (state, message) => {
      receivedType = message.type;
      return { count: state.count + 1 };
    };

    const addr = system.spawn(behavior, { count: 0 }, 'counter');
    system.send(addr, 'INC', {});

    await new Promise((r) => setTimeout(r, 10));
    assert.equal(receivedType, 'INC');
  });

  it('processes messages in order (mailbox)', async () => {
    const system = new ActorSystem({ name: 'test' });
    const order: number[] = [];

    const behavior: ActorBehavior = (state, message) => {
      order.push(message.payload.value);
      return state;
    };

    const addr = system.spawn(behavior, {}, 'ordered');
    system.send(addr, 'MSG', { value: 1 });
    system.send(addr, 'MSG', { value: 2 });
    system.send(addr, 'MSG', { value: 3 });

    await new Promise((r) => setTimeout(r, 20));
    assert.deepEqual(order, [1, 2, 3]);
  });

  it('stop removes actor', async () => {
    const system = new ActorSystem({ name: 'test' });
    const addr = system.spawn((s) => s, {}, 'temp');

    system.stop(addr);

    // Sending to stopped actor should go to DLQ
    system.send(addr, 'HELLO', {});
    await new Promise((r) => setTimeout(r, 10));

    const dlq = system.getDeadLetterQueue();
    assert.ok(dlq.length > 0);
  });

  it('exposes system name', () => {
    const system = new ActorSystem({ name: 'my-system' });
    assert.equal(system.name, 'my-system');
  });
});

// --- ActorSystem: Class-Based Actors ---

describe('ActorSystem: Class-Based Actors', () => {
  it('registers a class-based actor', async () => {
    const system = new ActorSystem({ name: 'test' });
    const router = system.messageRouter;

    class EchoActor extends Actor {
      async receive(message: Message): Promise<MessageResponse> {
        return createResponse(message, { echo: message.payload });
      }
    }

    const echo = new EchoActor('echo', router);
    system.register(echo);

    const result = await system.ask(address('echo'), 'PING', { text: 'hello' });
    assert.deepEqual(result, { echo: { text: 'hello' } });
  });
});

// --- Service Discovery ---

describe('Service Discovery', () => {
  it('registerService + requestService resolves immediately', () => {
    const system = new ActorSystem({ name: 'test' });
    const addr = system.spawn((s) => s, {}, 'auth-actor');

    system.registerService('auth', addr);

    let resolved: Address | null = null;
    system.requestService('auth', 'requester-1', (a) => {
      resolved = a;
    });

    assert.ok(resolved);
    assert.equal(resolved, '@(auth-actor)');
  });

  it('requestService queues until service registered', () => {
    const system = new ActorSystem({ name: 'test' });

    let resolved: Address | null = null;
    system.requestService('db', 'requester-1', (a) => {
      resolved = a;
    });

    assert.equal(resolved, null);

    const addr = system.spawn((s) => s, {}, 'db-actor');
    system.registerService('db', addr);

    assert.ok(resolved);
    assert.equal(resolved, '@(db-actor)');
  });

  it('unregisterService removes service', () => {
    const system = new ActorSystem({ name: 'test' });
    const addr = system.spawn((s) => s, {}, 'cache');

    system.registerService('cache', addr);
    system.unregisterService('cache');

    let resolved = false;
    system.requestService('cache', 'requester', () => {
      resolved = true;
    });

    assert.equal(resolved, false);
  });
});

// --- Supervision ---

describe('Supervision', () => {
  it('Resume directive continues processing', async () => {
    const system = new ActorSystem({
      name: 'test',
      supervisionStrategy: {
        onFailure: () => 'Resume',
      },
    });

    let count = 0;
    const behavior: ActorBehavior = (state, message) => {
      count++;
      if (message.payload.fail) throw new Error('test error');
      return state;
    };

    const addr = system.spawn(behavior, {}, 'resilient');
    system.send(addr, 'OK', { fail: false });
    system.send(addr, 'FAIL', { fail: true });
    system.send(addr, 'OK', { fail: false });

    await new Promise((r) => setTimeout(r, 30));
    assert.equal(count, 3);
  });

  it('Stop directive removes actor', async () => {
    const system = new ActorSystem({
      name: 'test',
      supervisionStrategy: {
        onFailure: () => 'Stop',
      },
    });

    const behavior: ActorBehavior = (_state, message) => {
      if (message.payload.fail) throw new Error('fatal');
      return _state;
    };

    const addr = system.spawn(behavior, {}, 'fragile');
    system.send(addr, 'FAIL', { fail: true });

    await new Promise((r) => setTimeout(r, 20));

    // Sending to stopped actor should go to DLQ
    system.send(addr, 'HELLO', {});
    await new Promise((r) => setTimeout(r, 10));

    const dlq = system.getDeadLetterQueue();
    assert.ok(dlq.some(entry => parseAddress(entry.address) === 'fragile'));
  });
});

// --- Transport ---

describe('Transport', () => {
  it('registerTransport routes messages to transport', async () => {
    const system = new ActorSystem({ name: 'test' });
    const sent: Array<{ recipient: string; message: unknown }> = [];

    const transport: any = {
      state: 'connected',
      connect: async () => {},
      disconnect: async () => {},
      send: async (recipient: string, message: unknown) => {
        sent.push({ recipient, message });
      },
      onReceive: () => {},
    };

    system.registerTransport('worker', transport);

    system.send(address('worker://remote-actor'), 'HELLO', {});

    await new Promise((r) => setTimeout(r, 10));
    assert.equal(sent.length, 1);
    assert.equal(sent[0].recipient, 'worker://remote-actor');
  });
});

// --- Dead Letter Queue ---

describe('Dead Letter Queue', () => {
  it('captures undeliverable messages', async () => {
    const system = new ActorSystem({ name: 'test', deadLetterQueueSize: 5 });

    system.send(address('nonexistent'), 'LOST', {});
    await new Promise((r) => setTimeout(r, 10));

    const dlq = system.getDeadLetterQueue();
    assert.ok(dlq.length > 0);
    assert.equal(parseAddress(dlq[0].address), 'nonexistent');
  });
});

// --- JsonSerde ---

describe('JsonSerde', () => {
  it('round-trips objects', () => {
    const serde = new JsonSerde();
    const original = { type: 'INC', payload: { amount: 5 } };
    const bytes = serde.serialize(original);
    const restored = serde.deserialize(bytes);
    assert.deepEqual(restored, original);
  });

  it('reports content type', () => {
    const serde = new JsonSerde();
    assert.equal(serde.contentType, 'application/json');
  });
});

// --- LocalTransport ---

describe('LocalTransport', () => {
  it('delivers messages via receive handler', async () => {
    const transport = new LocalTransport();
    let received: { sender: string; message: unknown } | null = null;

    transport.onReceive((sender, message) => {
      received = { sender, message };
    });

    await transport.send('local://actor', { type: 'PING' });
    assert.ok(received);
    assert.equal(received!.sender, 'local://actor');
  });

  it('tracks connection state', async () => {
    const transport = new LocalTransport();
    assert.equal(transport.state, 'connected');

    await transport.disconnect();
    assert.equal(transport.state, 'disconnected');

    await transport.connect('any');
    assert.equal(transport.state, 'connected');
  });
});

// --- Message utilities ---

describe('Message utilities', () => {
  it('address creates canonical @(id) format', () => {
    assert.equal(address('test'), '@(test)');
    assert.equal(address('path/to/actor'), '@(path/to/actor)');
  });

  it('parseAddress extracts id from @(id)', () => {
    assert.equal(parseAddress('@(test)' as Address), 'test');
    assert.equal(parseAddress('@(a/b/c)' as Address), 'a/b/c');
  });

  it('parseAddress throws on invalid format', () => {
    assert.throws(() => parseAddress('invalid' as any), { message: /Invalid address/ });
  });

  it('createMessage builds envelope', () => {
    const msg = createMessage(address('target'), 'PING', { data: 1 });
    assert.equal(msg.to, '@(target)');
    assert.equal(msg.type, 'PING');
    assert.deepEqual(msg.payload, { data: 1 });
    assert.equal(msg.pattern, 'tell');
    assert.ok(msg.id);
    assert.ok(msg.timestamp);
  });

  it('createResponse builds reply', () => {
    const original = createMessage(address('target'), 'Q', {}, {
      from: address('sender'),
      correlationId: 'corr-1',
    });

    const response = createResponse(original, { answer: 42 });
    assert.equal(response.correlationId, 'corr-1');
    assert.equal(response.from, '@(target)');
    assert.equal(response.to, '@(sender)');
    assert.equal(response.success, true);
    assert.deepEqual(response.payload, { answer: 42 });
  });
});

// --- Path Utilities ---

describe('Path utilities', () => {
  it('parsePath splits and filters', () => {
    assert.deepEqual(parsePath('domain/inference'), ['domain', 'inference']);
    assert.deepEqual(parsePath('//domain//inference/'), ['domain', 'inference']);
    assert.deepEqual(parsePath(''), []);
  });

  it('validatePath rejects unsafe paths', () => {
    assert.equal(validatePath('domain/inference'), true);
    assert.equal(validatePath('domain/../inference'), false);
    assert.equal(validatePath('domain/.'), false);
    assert.equal(validatePath(''), false);
  });

  it('isSafePath applies strict checks', () => {
    assert.equal(isSafePath('domain/inference'), true);
    assert.equal(isSafePath('.hidden/file'), false);
  });

  it('matchPattern with wildcards', () => {
    assert.equal(matchPattern('domain/inference', 'domain/*'), true);
    assert.equal(matchPattern('domain/inference/task', 'domain/*'), false);
    assert.equal(matchPattern('domain/inference/task', 'domain/**'), true);
  });

  it('getParentPath and getLocalName', () => {
    assert.equal(getParentPath('domain/inference'), 'domain');
    assert.equal(getParentPath('domain'), null);
    assert.equal(getLocalName('domain/inference'), 'inference');
    assert.equal(getLocalName('domain'), 'domain');
  });

  it('joinPath combines segments', () => {
    assert.equal(joinPath('domain', 'inference'), 'domain/inference');
    assert.equal(joinPath('domain', '', 'inference'), 'domain/inference');
  });

  it('isChildOf checks hierarchy', () => {
    assert.equal(isChildOf('domain/inference', 'domain'), true);
    assert.equal(isChildOf('domain', 'domain/inference'), false);
    assert.equal(isChildOf('domain', 'domain'), false);
  });
});

// --- SupervisorBase ---

describe('SupervisorBase', () => {
  it('routes messages to children by path', async () => {
    const router = new MessageRouter();
    let received = false;

    const child = new LeafActor('inference', router, async (msg) => {
      received = true;
      return { handled: true };
    });

    const supervisor = new SupervisorBase('domain', router);
    supervisor.addChild('inference', child);

    router.registerActor('domain', supervisor);

    const msg = createMessage(address('domain/inference'), 'QUERY', {}, {
      from: address('test'),
      correlationId: 'c1',
    });

    const response = await supervisor.receive(msg);
    assert.equal(received, true);
    assert.equal(response.success, true);
  });

  it('returns error for unknown child', async () => {
    const router = new MessageRouter();
    const supervisor = new SupervisorBase('domain', router);

    const msg = createMessage(address('domain/unknown'), 'QUERY', {}, {
      from: address('test'),
    });

    const response = await supervisor.receive(msg);
    assert.equal(response.success, false);
    assert.ok(response.error?.includes('Child not found'));
  });

  it('handles message addressed to self', async () => {
    const router = new MessageRouter();
    const supervisor = new SupervisorBase('domain', router);

    const msg = createMessage(address('domain'), 'INFO', {}, {
      from: address('test'),
    });

    const response = await supervisor.receive(msg);
    assert.equal(response.success, true);
    assert.ok(response.payload);
  });
});
