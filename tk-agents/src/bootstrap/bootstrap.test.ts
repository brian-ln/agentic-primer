// Tests for Bootstrap system

import { describe, expect, test } from "bun:test";
import type { Actor, Message, Response } from "../actors/base";
import { Graph } from "../graph";
import { createBootstrap } from "./index";

// Test actor implementation
class TestActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;
  public messageLog: Message[] = [];

  constructor(id: string) {
    this.id = id;
  }

  async send(message: Message): Promise<Response> {
    this.messageLog.push(message);

    if (message.type === "ping") {
      return { success: true, data: { alive: true } };
    }

    return { success: true, data: { processed: message.type } };
  }
}

describe("Bootstrap System", () => {
  test("create agent-task registers both actor and task", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("test-actor-1");
    const agentTask = bootstrap.create({
      agent: actor,
      goal: "Test goal",
      deliverables: ["output.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    // Check agent-task created
    expect(agentTask.taskId).toBeDefined();
    expect(agentTask.agentId).toBe("test-actor-1");

    // Check actor registered
    expect(bootstrap.registry.has("test-actor-1")).toBe(true);

    // Check task node exists
    const taskNode = graph.getNode(agentTask.taskId);
    expect(taskNode).toBeDefined();
    expect(taskNode?.properties.type).toBe("task");
  });

  test("agent-task with parent creates proper hierarchy", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    // Create parent task first
    const actor1 = new TestActor("parent-actor");
    const parentAgentTask = bootstrap.create({
      agent: actor1,
      goal: "Parent goal",
      deliverables: ["parent.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    // Create child task
    const actor2 = new TestActor("child-actor");
    const childAgentTask = bootstrap.create({
      agent: actor2,
      goal: "Child goal",
      deliverables: ["child.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
      parentTaskId: parentAgentTask.taskId,
    });

    // Check hierarchy
    const children = graph.getChildTasks(parentAgentTask.taskId);
    expect(children.length).toBe(1);
    expect(children[0].properties.id).toBe(childAgentTask.taskId);
  });

  test("task injection creates new task with dependencies", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("test-actor-2");
    const parentAgentTask = bootstrap.create({
      agent: actor,
      goal: "Parent task",
      deliverables: ["parent.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    // Inject child task
    const injectedTask = bootstrap.inject({
      parentTaskId: parentAgentTask.taskId,
      goal: "Injected task",
      deliverables: ["injected.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
      makeDependency: true,
    });

    // Check task created
    expect(injectedTask.properties.id).toBeDefined();
    expect(injectedTask.properties.goal).toBe("Injected task");

    // Check it's a child of parent
    const children = graph.getChildTasks(parentAgentTask.taskId);
    expect(children.length).toBe(1);
    expect(children[0].properties.id).toBe(injectedTask.properties.id);

    // Check dependency edge exists
    const edges = graph.getEdgesFrom(parentAgentTask.taskId);
    const dependsOnEdge = edges.find((e) => e.type === "depends_on");
    expect(dependsOnEdge).toBeDefined();
    expect(dependsOnEdge?.toId).toBe(injectedTask.properties.id);
  });

  test("status returns task progress with agent info", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("test-actor-3");
    const agentTask = bootstrap.create({
      agent: actor,
      goal: "Test goal",
      deliverables: ["output.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    const status = bootstrap.status(agentTask.taskId);

    expect(status.taskId).toBe(agentTask.taskId);
    expect(status.agentId).toBe("test-actor-3");
    expect(status.state).toBe("active");
    expect(status.progress).toBeDefined();
  });

  test("projectStatus returns hierarchical status", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    // Parent task
    const parentActor = new TestActor("parent-actor-2");
    const parentAgentTask = bootstrap.create({
      agent: parentActor,
      goal: "Parent goal",
      deliverables: ["parent.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    // Child task 1
    const child1Actor = new TestActor("child1-actor");
    bootstrap.create({
      agent: child1Actor,
      goal: "Child 1 goal",
      deliverables: ["child1.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
      parentTaskId: parentAgentTask.taskId,
    });

    // Child task 2
    const child2Actor = new TestActor("child2-actor");
    bootstrap.create({
      agent: child2Actor,
      goal: "Child 2 goal",
      deliverables: ["child2.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
      parentTaskId: parentAgentTask.taskId,
    });

    const projectStatus = bootstrap.projectStatus(parentAgentTask.taskId);

    expect(projectStatus.taskId).toBe(parentAgentTask.taskId);
    expect(projectStatus.children.length).toBe(2);
    expect(projectStatus.children[0].agentId).toBeDefined();
    expect(projectStatus.children[1].agentId).toBeDefined();
  });

  test("getAgentTask returns correct mapping", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("test-actor-4");
    const agentTask = bootstrap.create({
      agent: actor,
      goal: "Test goal",
      deliverables: ["output.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    const retrieved = bootstrap.registry.getAgentTask("test-actor-4");
    expect(retrieved).toBeDefined();
    expect(retrieved?.taskId).toBe(agentTask.taskId);
    expect(retrieved?.agentId).toBe("test-actor-4");
  });

  test("getTaskId returns task ID for agent", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("test-actor-5");
    const agentTask = bootstrap.create({
      agent: actor,
      goal: "Test goal",
      deliverables: ["output.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    const taskId = bootstrap.registry.getTaskId("test-actor-5");
    expect(taskId).toBe(agentTask.taskId);
  });

  test("getAgentId returns agent ID for task", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("test-actor-6");
    const agentTask = bootstrap.create({
      agent: actor,
      goal: "Test goal",
      deliverables: ["output.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    const agentId = bootstrap.registry.getAgentId(agentTask.taskId);
    expect(agentId).toBe("test-actor-6");
  });

  test("list returns all agent-tasks", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor1 = new TestActor("actor-1");
    const actor2 = new TestActor("actor-2");

    bootstrap.create({
      agent: actor1,
      goal: "Goal 1",
      deliverables: ["out1.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    bootstrap.create({
      agent: actor2,
      goal: "Goal 2",
      deliverables: ["out2.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    const list = bootstrap.list();
    expect(list.length).toBe(2);
    expect(list.map((at) => at.agentId).sort()).toEqual(["actor-1", "actor-2"]);
  });

  test("clear removes all agent-tasks", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("test-actor-7");
    bootstrap.create({
      agent: actor,
      goal: "Test goal",
      deliverables: ["output.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    expect(bootstrap.list().length).toBe(1);

    bootstrap.registry.clear();

    expect(bootstrap.list().length).toBe(0);
  });
});

describe("Task Injector", () => {
  test("inject creates task with proper edges", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("parent-actor-inject");
    const parentAgentTask = bootstrap.create({
      agent: actor,
      goal: "Parent task",
      deliverables: ["parent.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    const injectedTask = bootstrap.injector.inject(
      {
        parentTaskId: parentAgentTask.taskId,
        goal: "Injected goal",
        deliverables: ["injected.txt"],
        criteria: [{ criterion: "done", measure: "exists", threshold: true }],
        makeDependency: true,
      },
      graph
    );

    expect(injectedTask.properties.goal).toBe("Injected goal");

    // Check spawned_by edge
    const spawnedByEdges = graph.getEdgesFrom(injectedTask.properties.id);
    const spawnedByEdge = spawnedByEdges.find((e) => e.type === "spawned_by");
    expect(spawnedByEdge).toBeDefined();
    expect(spawnedByEdge?.toId).toBe(parentAgentTask.taskId);

    // Check depends_on edge
    const dependsOnEdges = graph.getEdgesFrom(parentAgentTask.taskId);
    const dependsOnEdge = dependsOnEdges.find((e) => e.type === "depends_on");
    expect(dependsOnEdge).toBeDefined();
    expect(dependsOnEdge?.toId).toBe(injectedTask.properties.id);
  });

  test("registerRule and applyRule works", () => {
    const graph = new Graph();
    const bootstrap = createBootstrap(graph);

    const actor = new TestActor("rule-actor");
    const parentAgentTask = bootstrap.create({
      agent: actor,
      goal: "Parent task",
      deliverables: ["parent.txt"],
      criteria: [{ criterion: "done", measure: "exists", threshold: true }],
    });

    // Register a rule
    bootstrap.injector.registerRule({
      name: "test-rule",
      condition: "immediate",
      factory: () => ({
        goal: "Auto-injected task",
        deliverables: ["auto.txt"],
        criteria: [{ criterion: "done", measure: "exists", threshold: true }],
      }),
    });

    // Apply the rule
    const injectedTask = bootstrap.injector.applyRule(
      "test-rule",
      parentAgentTask.taskId,
      graph
    );

    expect(injectedTask.properties.goal).toBe("Auto-injected task");
  });

  test("listRules returns registered rules", () => {
    const bootstrap = createBootstrap(new Graph());

    bootstrap.injector.registerRule({
      name: "rule1",
      condition: "immediate",
      factory: () => ({
        goal: "Task 1",
        deliverables: ["t1.txt"],
        criteria: [],
      }),
    });

    bootstrap.injector.registerRule({
      name: "rule2",
      condition: "before_complete",
      factory: () => ({
        goal: "Task 2",
        deliverables: ["t2.txt"],
        criteria: [],
      }),
    });

    const rules = bootstrap.injector.listRules();
    expect(rules.length).toBeGreaterThanOrEqual(2);
    expect(rules.map((r) => r.name)).toContain("rule1");
    expect(rules.map((r) => r.name)).toContain("rule2");
  });

  test("removeRule removes registered rule", () => {
    const bootstrap = createBootstrap(new Graph());

    bootstrap.injector.registerRule({
      name: "temp-rule",
      condition: "immediate",
      factory: () => ({
        goal: "Temp task",
        deliverables: ["temp.txt"],
        criteria: [],
      }),
    });

    expect(
      bootstrap.injector.listRules().map((r) => r.name)
    ).toContain("temp-rule");

    bootstrap.injector.removeRule("temp-rule");

    expect(
      bootstrap.injector.listRules().map((r) => r.name)
    ).not.toContain("temp-rule");
  });
});
