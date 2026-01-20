/**
 * Session Graph Manager
 *
 * Manages the CozoDB graph for projects, sessions, and agents.
 * Provides high-level API for common operations.
 */

import { CozoWasmClient, createCozoClient } from "../cozo-wasm-client";
import { EventLog, type Event } from "../persistence/event-log";

export interface Project {
  id: string;
  name: string;
  path: string;
  created_at: string;
}

export interface Session {
  id: string;
  project_id: string;
  started_at: string;
  ended_at?: string;
  status: string;
}

export interface Agent {
  id: string;
  session_id: string;
  task_id?: string;
  command: string;
  status: string;
  priority: string;
  started_at: string;
  ended_at?: string;
  output_path?: string;
}

export interface TimelineEvent {
  event_type: string;
  entity_id: string;
  timestamp: string;
}

export class SessionGraph {
  private db: CozoWasmClient;
  private eventLog: EventLog;

  constructor(db: CozoWasmClient, eventLogPath?: string) {
    this.db = db;
    this.eventLog = new EventLog(eventLogPath || "./session-graph-events.jsonl");
  }

  /**
   * Initialize schema by creating all relations and indexes
   */
  async initializeSchema(): Promise<void> {
    // Create project relation
    await this.db.run(`
      {:create project {
        id: String =>
        name: String,
        path: String,
        created_at: String
      }}
    `);

    // Create session relation
    await this.db.run(`
      {:create session {
        id: String =>
        project_id: String,
        started_at: String,
        ended_at: String?,
        status: String default "active"
      }}
    `);

    // Create agent relation
    await this.db.run(`
      {:create agent {
        id: String =>
        session_id: String,
        task_id: String?,
        command: String,
        status: String default "running",
        priority: String default "P2",
        started_at: String,
        ended_at: String?,
        output_path: String?
      }}
    `);

    // Create belongs_to edge
    await this.db.run(`
      {:create belongs_to {
        from: String,
        to: String =>
        relationship: String default "session_of_project"
      }}
    `);

    // Create spawned_by edge
    await this.db.run(`
      {:create spawned_by {
        child: String,
        parent: String =>
        relationship: String default "agent_spawned_by",
        spawned_at: String
      }}
    `);

    // Create depends_on edge
    await this.db.run(`
      {:create depends_on {
        dependent: String,
        dependency: String =>
        relationship: String default "blocks_on",
        created_at: String
      }}
    `);
  }

  // ============================================================================
  // Project Operations
  // ============================================================================

  async createProject(project: Project): Promise<void> {
    await this.db.run(
      `?[id, name, path, created_at] <- [[$id, $name, $path, $created_at]]
       :put project {id, name, path, created_at}`,
      project
    );

    // Log event
    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type: "project_created",
      nodeId: project.id,
      data: project,
    });
  }

  async getProject(id: string): Promise<Project | null> {
    const result = await this.db.run(
      `?[id, name, path, created_at] := *project{id, name, path, created_at}, id == $id`,
      { id }
    );

    if (result.rows.length === 0) return null;

    const [rowId, name, path, created_at] = result.rows[0] as [
      string,
      string,
      string,
      string
    ];
    return { id: rowId, name, path, created_at };
  }

  async listProjects(): Promise<Project[]> {
    const result = await this.db.run(
      `?[id, name, path, created_at] := *project{id, name, path, created_at}`
    );

    return result.rows.map(([id, name, path, created_at]) => ({
      id: id as string,
      name: name as string,
      path: path as string,
      created_at: created_at as string,
    }));
  }

  // ============================================================================
  // Session Operations
  // ============================================================================

  async createSession(session: Session, projectId: string): Promise<void> {
    // Create session node
    const params = {
      id: session.id,
      project_id: session.project_id,
      started_at: session.started_at,
      ended_at: session.ended_at ?? null,
      status: session.status,
    };

    await this.db.run(
      `?[id, project_id, started_at, ended_at, status] <- [[$id, $project_id, $started_at, $ended_at, $status]]
       :put session {id, project_id, started_at, ended_at, status}`,
      params
    );

    // Create belongs_to edge
    await this.db.run(
      `?[from, to, relationship] <- [[$from, $to, "session_of_project"]]
       :put belongs_to {from, to, relationship}`,
      { from: session.id, to: projectId }
    );

    // Log event
    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type: "session_created",
      nodeId: session.id,
      data: { session, projectId },
    });
  }

  async getSession(id: string): Promise<Session | null> {
    const result = await this.db.run(
      `?[id, project_id, started_at, ended_at, status] :=
       *session{id, project_id, started_at, ended_at, status},
       id == $id`,
      { id }
    );

    if (result.rows.length === 0) return null;

    const [rowId, project_id, started_at, ended_at, status] = result.rows[0] as [
      string,
      string,
      string,
      string | null,
      string
    ];
    return { id: rowId, project_id, started_at, ended_at: ended_at || undefined, status };
  }

  async listSessionsForProject(projectId: string): Promise<Session[]> {
    const result = await this.db.run(
      `?[id, project_id, started_at, ended_at, status] :=
       *belongs_to{from: id, to: $project_id},
       *session{id, project_id, started_at, ended_at, status}`,
      { project_id: projectId }
    );

    return result.rows.map(([id, project_id, started_at, ended_at, status]) => ({
      id: id as string,
      project_id: project_id as string,
      started_at: started_at as string,
      ended_at: ended_at as string | undefined,
      status: status as string,
    }));
  }

  async updateSessionStatus(id: string, status: string, ended_at?: string): Promise<void> {
    if (ended_at) {
      await this.db.run(
        `?[id, project_id, started_at, ended_at, status] :=
         *session{id, project_id, started_at},
         id == $id,
         ended_at = $ended_at,
         status = $status
         :put session {id, project_id, started_at, ended_at, status}`,
        { id, status, ended_at }
      );
    } else {
      await this.db.run(
        `?[id, project_id, started_at, ended_at, status] :=
         *session{id, project_id, started_at, ended_at},
         id == $id,
         status = $status
         :put session {id, project_id, started_at, ended_at, status}`,
        { id, status }
      );
    }

    // Log event
    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type: "session_status_updated",
      nodeId: id,
      data: { status, ended_at },
    });
  }

  // ============================================================================
  // Agent Operations
  // ============================================================================

  async createAgent(agent: Agent, parentId: string, spawnedAt: string): Promise<void> {
    // Create agent node
    const params = {
      id: agent.id,
      session_id: agent.session_id,
      task_id: agent.task_id ?? null,
      command: agent.command,
      status: agent.status,
      priority: agent.priority,
      started_at: agent.started_at,
      ended_at: agent.ended_at ?? null,
      output_path: agent.output_path ?? null,
    };

    await this.db.run(
      `?[id, session_id, task_id, command, status, priority, started_at, ended_at, output_path] <-
       [[$id, $session_id, $task_id, $command, $status, $priority, $started_at, $ended_at, $output_path]]
       :put agent {id, session_id, task_id, command, status, priority, started_at, ended_at, output_path}`,
      params
    );

    // Create spawned_by edge
    await this.db.run(
      `?[child, parent, relationship, spawned_at] <- [[$child, $parent, "agent_spawned_by", $spawned_at]]
       :put spawned_by {child, parent, relationship, spawned_at}`,
      { child: agent.id, parent: parentId, spawned_at: spawnedAt }
    );

    // Log event
    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type: "agent_created",
      nodeId: agent.id,
      data: { agent, parentId, spawnedAt },
    });
  }

  async getAgent(id: string): Promise<Agent | null> {
    const result = await this.db.run(
      `?[id, session_id, task_id, command, status, priority, started_at, ended_at, output_path] :=
       *agent{id, session_id, task_id, command, status, priority, started_at, ended_at, output_path},
       id == $id`,
      { id }
    );

    if (result.rows.length === 0) return null;

    const [
      rowId,
      session_id,
      task_id,
      command,
      status,
      priority,
      started_at,
      ended_at,
      output_path,
    ] = result.rows[0] as [
      string,
      string,
      string | null,
      string,
      string,
      string,
      string,
      string | null,
      string | null
    ];

    return {
      id: rowId,
      session_id,
      task_id: task_id || undefined,
      command,
      status,
      priority,
      started_at,
      ended_at: ended_at || undefined,
      output_path: output_path || undefined,
    };
  }

  async listAgentsForSession(sessionId: string): Promise<Agent[]> {
    const result = await this.db.run(
      `?[id, session_id, task_id, command, status, priority, started_at, ended_at, output_path] :=
       *spawned_by{child: id, parent: $session_id},
       *agent{id, session_id, task_id, command, status, priority, started_at, ended_at, output_path}`,
      { session_id: sessionId }
    );

    return result.rows.map(
      ([id, session_id, task_id, command, status, priority, started_at, ended_at, output_path]) => ({
        id: id as string,
        session_id: session_id as string,
        task_id: task_id as string | undefined,
        command: command as string,
        status: status as string,
        priority: priority as string,
        started_at: started_at as string,
        ended_at: ended_at as string | undefined,
        output_path: output_path as string | undefined,
      })
    );
  }

  async listActiveAgents(): Promise<Agent[]> {
    const result = await this.db.run(
      `?[id, session_id, task_id, command, status, priority, started_at, ended_at, output_path] :=
       *agent{id, session_id, task_id, command, status, priority, started_at, ended_at, output_path},
       status in ["running", "active"]`
    );

    return result.rows.map(
      ([id, session_id, task_id, command, status, priority, started_at, ended_at, output_path]) => ({
        id: id as string,
        session_id: session_id as string,
        task_id: task_id as string | undefined,
        command: command as string,
        status: status as string,
        priority: priority as string,
        started_at: started_at as string,
        ended_at: ended_at as string | undefined,
        output_path: output_path as string | undefined,
      })
    );
  }

  async updateAgentStatus(
    id: string,
    status: string,
    ended_at?: string
  ): Promise<void> {
    if (ended_at) {
      await this.db.run(
        `?[id, session_id, task_id, command, status, priority, started_at, ended_at, output_path] :=
         *agent{id, session_id, task_id, command, priority, started_at, output_path},
         id == $id,
         status = $status,
         ended_at = $ended_at
         :put agent {id, session_id, task_id, command, status, priority, started_at, ended_at, output_path}`,
        { id, status, ended_at }
      );
    } else {
      await this.db.run(
        `?[id, session_id, task_id, command, status, priority, started_at, ended_at, output_path] :=
         *agent{id, session_id, task_id, command, priority, started_at, ended_at, output_path},
         id == $id,
         status = $status
         :put agent {id, session_id, task_id, command, status, priority, started_at, ended_at, output_path}`,
        { id, status }
      );
    }

    // Log event
    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type: "agent_status_updated",
      nodeId: id,
      data: { status, ended_at },
    });
  }

  // ============================================================================
  // Dependency Operations
  // ============================================================================

  async createDependency(
    dependent: string,
    dependency: string,
    created_at: string
  ): Promise<void> {
    await this.db.run(
      `?[dependent, dependency, relationship, created_at] <-
       [[$dependent, $dependency, "blocks_on", $created_at]]
       :put depends_on {dependent, dependency, relationship, created_at}`,
      { dependent, dependency, created_at }
    );

    // Log event
    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type: "dependency_created",
      nodeId: dependent,
      data: { dependent, dependency, created_at },
    });
  }

  async listBlockedAgents(): Promise<
    Array<{ agent: Agent; blocking_on: string[] }>
  > {
    const result = await this.db.run(
      `?[agent_id, blocking_on] :=
       *agent{id: agent_id, status: "blocked"},
       *depends_on{dependent: agent_id, dependency: blocking_on}`
    );

    // Group by agent_id
    const agentDeps = new Map<string, string[]>();
    for (const [agent_id, blocking_on] of result.rows) {
      if (!agentDeps.has(agent_id as string)) {
        agentDeps.set(agent_id as string, []);
      }
      agentDeps.get(agent_id as string)!.push(blocking_on as string);
    }

    // Fetch full agent details
    const agents: Array<{ agent: Agent; blocking_on: string[] }> = [];
    for (const [agent_id, deps] of agentDeps.entries()) {
      const agent = await this.getAgent(agent_id);
      if (agent) {
        agents.push({ agent, blocking_on: deps });
      }
    }

    return agents;
  }

  // ============================================================================
  // Timeline and Analytics
  // ============================================================================

  async getSessionTimeline(sessionId: string): Promise<TimelineEvent[]> {
    const result = await this.db.run(
      `
      ?[event_type, entity_id, timestamp] :=
        *session{id: $session_id, started_at},
        event_type = "session_started",
        entity_id = $session_id,
        timestamp = started_at

      ?[event_type, entity_id, timestamp] :=
        *spawned_by{child: agent_id, parent: $session_id, spawned_at},
        event_type = "agent_spawned",
        entity_id = agent_id,
        timestamp = spawned_at

      ?[event_type, entity_id, timestamp] :=
        *spawned_by{child: agent_id, parent: $session_id},
        *agent{id: agent_id, ended_at},
        ended_at != null,
        event_type = "agent_completed",
        entity_id = agent_id,
        timestamp = ended_at

      :order timestamp
      `,
      { session_id: sessionId }
    );

    return result.rows.map(([event_type, entity_id, timestamp]) => ({
      event_type: event_type as string,
      entity_id: entity_id as string,
      timestamp: timestamp as string,
    }));
  }

  /**
   * Export entire graph as JSON
   */
  async exportGraph(): Promise<string> {
    return this.db.exportRelations([
      "project",
      "session",
      "agent",
      "belongs_to",
      "spawned_by",
      "depends_on",
    ]);
  }

  /**
   * Import graph from JSON
   */
  async importGraph(data: string | object): Promise<void> {
    this.db.importRelations(data);
  }

  // ============================================================================
  // Event Log Operations
  // ============================================================================

  /**
   * Get the EventLog instance for querying event history
   */
  getEventLog(): EventLog {
    return this.eventLog;
  }

  /**
   * Get all events of a specific type
   */
  async getEventsByType(eventType: string): Promise<Event[]> {
    return await this.eventLog.getEventsByType(eventType);
  }

  /**
   * Get all events for a specific node
   */
  async getEventsByNode(nodeId: string): Promise<Event[]> {
    return await this.eventLog.getEventsByNode(nodeId);
  }

  /**
   * Get all events
   */
  async getAllEvents(): Promise<Event[]> {
    return await this.eventLog.getAllEvents();
  }
}

/**
 * Create a new SessionGraph instance with initialized schema
 */
export async function createSessionGraph(): Promise<SessionGraph> {
  const db = await createCozoClient();
  const graph = new SessionGraph(db);
  await graph.initializeSchema();
  return graph;
}
