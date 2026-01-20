// Tests for SessionGraph EventLog integration

import { describe, test, expect, beforeEach, afterEach } from "bun:test";
import { SessionGraph, type Project, type Session, type Agent } from "./session-graph";
import { createCozoClient } from "../cozo-wasm-client";
import { existsSync, unlinkSync } from "node:fs";

describe("SessionGraph EventLog Integration", () => {
  const testEventLogPath = "./test-session-graph-events.jsonl";
  let graph: SessionGraph;

  beforeEach(async () => {
    // Clean up test event log
    if (existsSync(testEventLogPath)) {
      unlinkSync(testEventLogPath);
    }

    // Create graph with test event log
    const db = await createCozoClient();
    graph = new SessionGraph(db, testEventLogPath);
    await graph.initializeSchema();
  });

  afterEach(() => {
    // Clean up test event log
    if (existsSync(testEventLogPath)) {
      unlinkSync(testEventLogPath);
    }
  });

  test("logs project creation events", async () => {
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };

    await graph.createProject(project);

    const events = graph.getEventsByType("project_created");
    expect(events.length).toBe(1);
    expect(events[0].nodeId).toBe("proj-1");
    expect((events[0].data as Project).name).toBe("Test Project");
  });

  test("logs session creation events", async () => {
    // Create project first
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    // Create session
    const session: Session = {
      id: "session-1",
      project_id: "proj-1",
      started_at: new Date().toISOString(),
      status: "active",
    };
    await graph.createSession(session, "proj-1");

    const events = graph.getEventsByType("session_created");
    expect(events.length).toBe(1);
    expect(events[0].nodeId).toBe("session-1");
  });

  test("logs session status update events", async () => {
    // Create project and session
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    const session: Session = {
      id: "session-1",
      project_id: "proj-1",
      started_at: new Date().toISOString(),
      status: "active",
    };
    await graph.createSession(session, "proj-1");

    // Update session status
    const endedAt = new Date().toISOString();
    await graph.updateSessionStatus("session-1", "completed", endedAt);

    const events = graph.getEventsByType("session_status_updated");
    expect(events.length).toBe(1);
    expect(events[0].nodeId).toBe("session-1");
    expect((events[0].data as { status: string }).status).toBe("completed");
  });

  test("logs agent creation events", async () => {
    // Create project and session
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    const session: Session = {
      id: "session-1",
      project_id: "proj-1",
      started_at: new Date().toISOString(),
      status: "active",
    };
    await graph.createSession(session, "proj-1");

    // Create agent
    const agent: Agent = {
      id: "agent-1",
      session_id: "session-1",
      command: "test command",
      status: "running",
      priority: "P1",
      started_at: new Date().toISOString(),
    };
    await graph.createAgent(agent, "session-1", new Date().toISOString());

    const events = graph.getEventsByType("agent_created");
    expect(events.length).toBe(1);
    expect(events[0].nodeId).toBe("agent-1");
  });

  test("logs agent status update events", async () => {
    // Create project, session, and agent
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    const session: Session = {
      id: "session-1",
      project_id: "proj-1",
      started_at: new Date().toISOString(),
      status: "active",
    };
    await graph.createSession(session, "proj-1");

    const agent: Agent = {
      id: "agent-1",
      session_id: "session-1",
      command: "test command",
      status: "running",
      priority: "P1",
      started_at: new Date().toISOString(),
    };
    await graph.createAgent(agent, "session-1", new Date().toISOString());

    // Update agent status
    const endedAt = new Date().toISOString();
    await graph.updateAgentStatus("agent-1", "completed", endedAt);

    const events = graph.getEventsByType("agent_status_updated");
    expect(events.length).toBe(1);
    expect(events[0].nodeId).toBe("agent-1");
    expect((events[0].data as { status: string }).status).toBe("completed");
  });

  test("logs dependency creation events", async () => {
    // Create project, session, and agents
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    const session: Session = {
      id: "session-1",
      project_id: "proj-1",
      started_at: new Date().toISOString(),
      status: "active",
    };
    await graph.createSession(session, "proj-1");

    const agent1: Agent = {
      id: "agent-1",
      session_id: "session-1",
      command: "test command 1",
      status: "running",
      priority: "P1",
      started_at: new Date().toISOString(),
    };
    await graph.createAgent(agent1, "session-1", new Date().toISOString());

    const agent2: Agent = {
      id: "agent-2",
      session_id: "session-1",
      command: "test command 2",
      status: "blocked",
      priority: "P2",
      started_at: new Date().toISOString(),
    };
    await graph.createAgent(agent2, "session-1", new Date().toISOString());

    // Create dependency
    await graph.createDependency("agent-2", "agent-1", new Date().toISOString());

    const events = graph.getEventsByType("dependency_created");
    expect(events.length).toBe(1);
    expect(events[0].nodeId).toBe("agent-2");
  });

  test("can query events by node", async () => {
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    const session: Session = {
      id: "session-1",
      project_id: "proj-1",
      started_at: new Date().toISOString(),
      status: "active",
    };
    await graph.createSession(session, "proj-1");

    // Update session multiple times
    await graph.updateSessionStatus("session-1", "paused");
    await graph.updateSessionStatus("session-1", "active");
    await graph.updateSessionStatus("session-1", "completed");

    // Query all events for session-1
    const sessionEvents = graph.getEventsByNode("session-1");
    expect(sessionEvents.length).toBe(4); // 1 create + 3 updates
  });

  test("persists events across instances", async () => {
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    // Create new graph instance with same event log
    const db2 = await createCozoClient();
    const graph2 = new SessionGraph(db2, testEventLogPath);

    // Events should be readable from second instance
    const events = graph2.getEventsByType("project_created");
    expect(events.length).toBe(1);
    expect(events[0].nodeId).toBe("proj-1");
  });

  test("records complete workflow", async () => {
    // Complete workflow: project -> session -> agent -> updates
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    const session: Session = {
      id: "session-1",
      project_id: "proj-1",
      started_at: new Date().toISOString(),
      status: "active",
    };
    await graph.createSession(session, "proj-1");

    const agent: Agent = {
      id: "agent-1",
      session_id: "session-1",
      command: "test command",
      status: "running",
      priority: "P1",
      started_at: new Date().toISOString(),
    };
    await graph.createAgent(agent, "session-1", new Date().toISOString());

    await graph.updateAgentStatus("agent-1", "completed", new Date().toISOString());
    await graph.updateSessionStatus("session-1", "completed", new Date().toISOString());

    // Verify all events logged
    const allEvents = graph.getAllEvents();
    expect(allEvents.length).toBe(5); // project, session, agent, agent update, session update

    // Verify event types
    const eventTypes = allEvents.map((e) => e.type);
    expect(eventTypes).toContain("project_created");
    expect(eventTypes).toContain("session_created");
    expect(eventTypes).toContain("agent_created");
    expect(eventTypes).toContain("agent_status_updated");
    expect(eventTypes).toContain("session_status_updated");
  });

  test("can access EventLog directly for advanced queries", async () => {
    const project: Project = {
      id: "proj-1",
      name: "Test Project",
      path: "/test/path",
      created_at: new Date().toISOString(),
    };
    await graph.createProject(project);

    // Get EventLog instance
    const eventLog = graph.getEventLog();

    // Use EventLog's replay functionality
    let eventCount = 0;
    eventLog.replay(() => {
      eventCount++;
    });

    expect(eventCount).toBe(1);
  });
});
