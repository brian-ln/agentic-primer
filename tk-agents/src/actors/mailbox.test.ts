// Tests for Mailbox and MailboxManager
import { describe, test, expect, beforeEach } from "bun:test";
import { Mailbox } from "./mailbox";
import { MailboxManagerActor } from "./mailbox-manager";
import { Message } from "./base";

describe("Mailbox", () => {
  let mailbox: Mailbox;

  beforeEach(() => {
    mailbox = new Mailbox({ maxSize: 3 });
  });

  test("should enqueue and dequeue messages", () => {
    const msg1: Message = { id: "1", type: "test", payload: "hello" };
    const msg2: Message = { id: "2", type: "test", payload: "world" };

    expect(mailbox.enqueue(msg1)).toBe(true);
    expect(mailbox.enqueue(msg2)).toBe(true);
    expect(mailbox.size()).toBe(2);

    expect(mailbox.dequeue()).toEqual(msg1);
    expect(mailbox.dequeue()).toEqual(msg2);
    expect(mailbox.isEmpty()).toBe(true);
  });

  test("should reject messages when full", () => {
    const msg: Message = { id: "1", type: "test", payload: "test" };

    expect(mailbox.enqueue(msg)).toBe(true);
    expect(mailbox.enqueue(msg)).toBe(true);
    expect(mailbox.enqueue(msg)).toBe(true);
    expect(mailbox.isFull()).toBe(true);

    // 4th message should be rejected
    expect(mailbox.enqueue(msg)).toBe(false);
    expect(mailbox.size()).toBe(3);
  });

  test("should peek without removing", () => {
    const msg: Message = { id: "1", type: "test", payload: "test" };

    mailbox.enqueue(msg);
    expect(mailbox.peek()).toEqual(msg);
    expect(mailbox.size()).toBe(1); // Still there

    expect(mailbox.dequeue()).toEqual(msg);
    expect(mailbox.isEmpty()).toBe(true);
  });

  test("should report capacity correctly", () => {
    const msg: Message = { id: "1", type: "test", payload: "test" };

    expect(mailbox.availableCapacity()).toBe(3);

    mailbox.enqueue(msg);
    expect(mailbox.availableCapacity()).toBe(2);

    mailbox.enqueue(msg);
    expect(mailbox.availableCapacity()).toBe(1);

    mailbox.enqueue(msg);
    expect(mailbox.availableCapacity()).toBe(0);
  });

  test("should clear all messages", () => {
    const msg: Message = { id: "1", type: "test", payload: "test" };

    mailbox.enqueue(msg);
    mailbox.enqueue(msg);
    expect(mailbox.size()).toBe(2);

    mailbox.clear();
    expect(mailbox.isEmpty()).toBe(true);
    expect(mailbox.size()).toBe(0);
  });
});

describe("MailboxManagerActor", () => {
  let manager: MailboxManagerActor;

  beforeEach(() => {
    manager = new MailboxManagerActor();
  });

  test("should create mailboxes for actors", async () => {
    const response = await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    expect(response.success).toBe(true);
    expect(response.data).toEqual({
      actorId: "actor-1",
      maxSize: 1000,
    });
  });

  test("should reject duplicate mailbox creation", async () => {
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    const response = await manager.send({
      id: "msg-2",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    expect(response.success).toBe(false);
    expect(response.error?.message).toContain("already exists");
  });

  test("should enqueue messages to actor mailbox", async () => {
    // Create mailbox
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    // Enqueue message
    const msg: Message = { id: "test-1", type: "test", payload: "hello" };
    const response = await manager.send({
      id: "msg-2",
      type: "enqueue",
      payload: { actorId: "actor-1", message: msg },
    });

    expect(response.success).toBe(true);
    expect(response.data?.size).toBe(1);
  });

  test("should dequeue messages from actor mailbox", async () => {
    // Create mailbox
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    // Enqueue message
    const msg: Message = { id: "test-1", type: "test", payload: "hello" };
    await manager.send({
      id: "msg-2",
      type: "enqueue",
      payload: { actorId: "actor-1", message: msg },
    });

    // Dequeue message
    const response = await manager.send({
      id: "msg-3",
      type: "dequeue",
      payload: { actorId: "actor-1" },
    });

    expect(response.success).toBe(true);
    expect(response.data?.message).toEqual(msg);
    expect(response.data?.size).toBe(0);
  });

  test("should report mailbox status", async () => {
    // Create mailbox
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    // Enqueue some messages
    const msg: Message = { id: "test-1", type: "test", payload: "hello" };
    await manager.send({
      id: "msg-2",
      type: "enqueue",
      payload: { actorId: "actor-1", message: msg },
    });

    // Get status
    const response = await manager.send({
      id: "msg-3",
      type: "status",
      payload: { actorId: "actor-1" },
    });

    expect(response.success).toBe(true);
    expect(response.data?.exists).toBe(true);
    expect(response.data?.size).toBe(1);
    expect(response.data?.isEmpty).toBe(false);
    expect(response.data?.isFull).toBe(false);
  });

  test("should handle status request for non-existent mailbox", async () => {
    const response = await manager.send({
      id: "msg-1",
      type: "status",
      payload: { actorId: "actor-nonexistent" },
    });

    expect(response.success).toBe(true);
    expect(response.data?.exists).toBe(false);
  });

  test("should clear mailbox", async () => {
    // Create mailbox and add messages
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    const msg: Message = { id: "test-1", type: "test", payload: "hello" };
    await manager.send({
      id: "msg-2",
      type: "enqueue",
      payload: { actorId: "actor-1", message: msg },
    });

    // Clear
    const response = await manager.send({
      id: "msg-3",
      type: "clear",
      payload: { actorId: "actor-1" },
    });

    expect(response.success).toBe(true);
    expect(response.data?.size).toBe(0);
  });

  test("should delete mailbox", async () => {
    // Create mailbox
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    // Delete
    const response = await manager.send({
      id: "msg-2",
      type: "delete_mailbox",
      payload: { actorId: "actor-1" },
    });

    expect(response.success).toBe(true);
    expect(response.data?.existed).toBe(true);

    // Verify it's gone
    const statusResponse = await manager.send({
      id: "msg-3",
      type: "status",
      payload: { actorId: "actor-1" },
    });

    expect(statusResponse.data?.exists).toBe(false);
  });

  test("should list all mailboxes", async () => {
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1" },
    });

    await manager.send({
      id: "msg-2",
      type: "create_mailbox",
      payload: { actorId: "actor-2" },
    });

    const list = manager.listMailboxes();
    expect(list.length).toBe(2);
    expect(list.find((m) => m.actorId === "actor-1")).toBeDefined();
    expect(list.find((m) => m.actorId === "actor-2")).toBeDefined();
  });

  test("should reject enqueue when mailbox is full", async () => {
    // Create small mailbox
    await manager.send({
      id: "msg-1",
      type: "create_mailbox",
      payload: { actorId: "actor-1", config: { maxSize: 2 } },
    });

    const msg: Message = { id: "test-1", type: "test", payload: "hello" };

    // Fill mailbox
    await manager.send({
      id: "msg-2",
      type: "enqueue",
      payload: { actorId: "actor-1", message: msg },
    });
    await manager.send({
      id: "msg-3",
      type: "enqueue",
      payload: { actorId: "actor-1", message: msg },
    });

    // Try to enqueue when full
    const response = await manager.send({
      id: "msg-4",
      type: "enqueue",
      payload: { actorId: "actor-1", message: msg },
    });

    expect(response.success).toBe(false);
    expect(response.error?.category).toBe("transient");
    expect(response.error?.retryable).toBe(true);
    expect(response.error?.message).toContain("full");
  });
});
