// API routes for daemon HTTP server

import type { Graph } from "../../src/graph.ts";
import type { EventLog } from "../../src/persistence/event-log.ts";
import type { DualWriteCoordinator } from "../../src/dual-write-coordinator.ts";
import type { TaskProperties, NodeProperties } from "../../src/types.ts";
import { IdeasSupervisor } from "../../src/actors/ideas-supervisor.ts";
import { createSignalsSupervisor } from "../../src/signals/signals-supervisor.ts";

interface Route {
  method: string;
  path: string | RegExp;
  handler: (req: Request, params: Record<string, string>) => Response | Promise<Response>;
  paramNames?: string[];
}

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type",
  "Content-Type": "application/json",
};

/**
 * JSON response helper
 */
function jsonResponse(data: any, status = 200): Response {
  return new Response(JSON.stringify(data, null, 2), {
    status,
    headers: CORS_HEADERS,
  });
}

/**
 * Error response helper
 */
function errorResponse(message: string, status = 500): Response {
  return jsonResponse({ error: message }, status);
}

/**
 * Create all API routes
 * Note: graph is passed as a getter function to support hot-reloading
 */
export function createRoutes(
  getGraph: () => Graph,
  eventLog: EventLog,
  coordinator: DualWriteCoordinator,
  wsManager: any
): Route[] {
  // Helper to get current graph (supports hot-reload)
  const graph = () => getGraph();

  // Initialize Ideas Supervisor
  const ideasSupervisor = new IdeasSupervisor(graph());

  // Initialize Signals Supervisor
  const signalsSupervisor = createSignalsSupervisor(graph());

  return [
    // Generic Actor Message Gateway
    {
      method: "POST",
      path: "/api/actor/message",
      handler: async (req) => {
        try {
          const { target, message } = await req.json();
          
          if (!target || !message) {
            return errorResponse("Missing target or message", 400);
          }

          // Simple routing logic for now
          // In a full implementation, this would use AddressResolver
          
          if (target === "primer.knowledge") {
            // Forward to KnowledgeCollectionActor logic
            if (message.type === "create") {
               const { title, content, sources } = message.payload;
               const { KnowledgeActor } = await import("../../src/knowledge.ts");
               
               const address = KnowledgeActor({
                 title, content, sources, graph: graph()
               });
               
               const ids = graph().getNodeIds();
               const id = ids[ids.length - 1];
               
               return jsonResponse({ success: true, data: { id, address: String(address) } });
            } else if (message.type === "list") {
               const nodes = graph().getNodeIds().filter(id => {
                 const p = graph().getNodeProperties(id);
                 return p?.type === "knowledge";
               }).map(id => ({ id, ...graph().getNodeProperties(id) }));
               
               return jsonResponse({ success: true, data: nodes });
            }
            
            return errorResponse(`Unknown message type for primer.knowledge: ${message.type}`, 400);
          }

          if (target === "primer.tasks") {
            // Virtual TaskCollectionActor
            switch (message.type) {
              case "create": {
                const id = await coordinator.createTask(message.payload);
                return jsonResponse({ success: true, data: { id } });
              }
              case "update": {
                const { id, action, ...options } = message.payload;
                await coordinator.updateTask(id, action, options);
                return jsonResponse({ success: true, data: { id, status: "updated" } });
              }
              case "delete": {
                const { id } = message.payload;
                await coordinator.deleteTask(id);
                return jsonResponse({ success: true, data: { id, status: "deleted" } });
              }
              case "list": {
                const { filters } = message.payload || {};
                const nodes = graph().getNodeIds().filter(id => {
                  const p = graph().getNodeProperties(id);
                  if (p?.type !== "task") return false;
                  // Apply filters
                  if (filters?.status && p.state !== filters.status) return false;
                  if (filters?.label && !p.labels?.includes(filters.label)) return false;
                  if (filters?.priority !== undefined && p.priority !== filters.priority) return false;
                  return true;
                }).map(id => ({ id, ...graph().getNodeProperties(id) }));
                return jsonResponse({ success: true, data: nodes });
              }
              case "get": {
                const { id } = message.payload;
                const props = graph().getNodeProperties(id);
                if (!props) return errorResponse("Task not found", 404);
                return jsonResponse({ success: true, data: props });
              }
              default:
                return errorResponse(`Unknown message type for primer.tasks: ${message.type}`, 400);
            }
          }

          if (target === "primer.graph") {
            // Virtual GraphActor
            switch (message.type) {
              case "create_node": {
                const { id, type, ...data } = message.payload;
                if (graph().getNodeProperties(id)) return errorResponse("Node exists", 409);
                const actor = { send: async () => ({ success: true, data: {} }) };
                const address = graph().getSystem().register(actor);
                graph().registerNode(id, address, { id, type, createdAt: new Date(), ...data });
                return jsonResponse({ success: true, data: { id } });
              }
              case "delete_node": {
                const success = graph().removeNode(message.payload.id);
                return jsonResponse({ success, data: { deleted: success } });
              }
              case "create_edge": {
                const { from, to, type, properties } = message.payload;
                const edge = graph().addEdge(from, to, type, properties);
                return jsonResponse({ success: true, data: { id: edge.id } });
              }
              case "delete_edge": {
                const success = graph().removeEdge(message.payload.id);
                return jsonResponse({ success, data: { deleted: success } });
              }
              case "list_nodes": {
                const { type } = message.payload || {};
                const nodes = graph().getNodeIds()
                  .map(id => ({ id, ...graph().getNodeProperties(id) }))
                  .filter(n => !type || n.type === type);
                return jsonResponse({ success: true, data: nodes });
              }
              case "list_edges": {
                const { from, to, type } = message.payload || {};
                let edges = graph().dump().edges;
                if (from) edges = edges.filter(e => e.fromId === from);
                if (to) edges = edges.filter(e => e.toId === to);
                if (type) edges = edges.filter(e => e.type === type);
                return jsonResponse({ success: true, data: edges });
              }
              case "get_node": {
                const { id } = message.payload;
                const props = graph().getNodeProperties(id);
                if (!props) return errorResponse("Node not found", 404);
                const outgoing = graph().getEdgesFrom(id);
                const incoming = graph().getEdgesTo(id);
                return jsonResponse({ success: true, data: { ...props, edges: { outgoing, incoming } } });
              }
              case "dump": {
                return jsonResponse({ success: true, data: graph().dump() });
              }
              default:
                return errorResponse(`Unknown message type for primer.graph: ${message.type}`, 400);
            }
          }
          
          // Default: try to send to a node by ID
          if (graph().getNode(target)) {
             const result = await graph().send(target, message.type, message.payload);
             return jsonResponse({ success: true, data: result });
          }

          return errorResponse(`Unknown target: ${target}`, 404);
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 500);
        }
      },
    },

    // Health check
    {
      method: "GET",
      path: "/api/health",
      handler: (req) => {
        return jsonResponse({
          status: "ok",
          uptime: process.uptime(),
          pid: process.pid,
          timestamp: new Date().toISOString(),
        });
      },
    },

    // Statistics
    {
      method: "GET",
      path: "/api/stats",
      handler: (req) => {
        const nodeIds = graph().getNodeIds();
        const tasks = nodeIds.filter((id) => {
          const props = graph().getNodeProperties(id);
          return props?.type === "task";
        });

        const tasksByState: Record<string, number> = {};
        let activeAgents = 0;
        let pendingReviews = 0;

        for (const id of tasks) {
          const props = graph().getNodeProperties(id) as TaskProperties;
          tasksByState[props.state] = (tasksByState[props.state] || 0) + 1;

          // Count active agents
          if (
            props.labels?.includes("agent") &&
            props.state === "active"
          ) {
            activeAgents++;
          }

          // Count pending reviews
          if (
            props.labels?.includes("review") &&
            props.state === "created"
          ) {
            pendingReviews++;
          }
        }

        return jsonResponse({
          totalTasks: tasks.length,
          tasksByState,
          activeAgents,
          pendingReviews,
          timestamp: new Date().toISOString(),
        });
      },
    },

    // List tasks
    {
      method: "GET",
      path: "/api/tasks",
      handler: (req) => {
        const url = new URL(req.url);
        const status = url.searchParams.get("status");
        const label = url.searchParams.get("label");
        const priority = url.searchParams.get("priority");

        const nodeIds = graph().getNodeIds();
        let tasks = nodeIds.filter((id) => {
          const props = graph().getNodeProperties(id);
          return props?.type === "task";
        });

        // Apply filters
        tasks = tasks.filter((id) => {
          const props = graph().getNodeProperties(id) as TaskProperties;

          if (status && props.state !== status) return false;
          if (label && !props.labels?.includes(label)) return false;
          if (priority !== null) {
            const priorityNum = parseInt(priority);
            if (!isNaN(priorityNum) && props.priority !== priorityNum) return false;
          }

          return true;
        });

        // Get full task data
        const taskList = tasks.map((id) => {
          const props = graph().getNodeProperties(id) as TaskProperties;
          return { id, ...props };
        });

        return jsonResponse(taskList);
      },
    },

    // Get task by ID
    {
      method: "GET",
      path: /^\/api\/tasks\/([^\/]+)$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        const taskId = params.id;
        const props = graph().getNodeProperties(taskId) as TaskProperties;

        if (!props || props.type !== "task") {
          return errorResponse("Task not found", 404);
        }

        const edges = graph().getAllEdges(taskId);

        return jsonResponse({
          id: taskId,
          ...props,
          edges,
        });
      },
    },

    // Create task
    {
      method: "POST",
      path: "/api/tasks",
      handler: async (req) => {
        try {
          const body = await req.json();
          const { goal, deliverables, criteria, labels, priority, parent } = body;

          if (!goal) {
            return errorResponse("Missing required field: goal", 400);
          }

          // Use DualWriteCoordinator for triple-write (Graph + EventLog + CozoDB)
          const taskId = await coordinator.createTask({
            goal,
            desiredDeliverables: deliverables || ["Task completion"],
            objectiveSuccessCriteria: criteria || [
              {
                criterion: "Task marked complete",
                measure: "Manual completion",
                threshold: true,
              },
            ],
            labels,
            priority,
            parentTaskId: parent,
            toolsAvailable: ["CLI"],
          });

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "task_created",
            data: { id: taskId, goal },
          });

          return jsonResponse({ id: taskId, goal, labels, priority }, 201);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            400
          );
        }
      },
    },

    // Update task
    {
      method: "PUT",
      path: /^\/api\/tasks\/([^\/]+)$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const taskId = params.id;
          const body = await req.json();
          const { action } = body;

          const props = graph().getNodeProperties(taskId) as TaskProperties;
          if (!props || props.type !== "task") {
            return errorResponse("Task not found", 404);
          }

          let result: any;

          switch (action) {
            case "start":
              // Use coordinator for triple-write
              await coordinator.updateTask(taskId, "start", {});
              result = { state: "active" };
              wsManager.broadcast({
                type: "task_updated",
                data: { id: taskId, state: "active" },
              });
              break;

            case "complete":
              // Auto-pass criteria before coordinator update
              for (const criterion of props.objectiveSuccessCriteria) {
                criterion.actual = criterion.threshold;
              }
              // Use coordinator for triple-write
              await coordinator.updateTask(taskId, "complete", {
                result: body.result || "Task completed",
              });
              result = { state: "completed" };
              wsManager.broadcast({
                type: "task_completed",
                data: { id: taskId, goal: props.goal },
              });
              break;

            case "block":
              // Use coordinator for triple-write
              await coordinator.updateTask(taskId, "block", {
                reason: body.reason || "Blocked by dependency",
              });
              result = { state: "blocked" };
              wsManager.broadcast({
                type: "task_blocked",
                data: { id: taskId, reason: body.reason },
              });
              break;

            default:
              return errorResponse(`Unknown action: ${action}`, 400);
          }

          return jsonResponse({ success: true, result });
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            400
          );
        }
      },
    },

    // Delete task
    {
      method: "DELETE",
      path: /^\/api\/tasks\/([^\/]+)$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const taskId = params.id;
          const props = graph().getNodeProperties(taskId);

          if (!props) {
            return errorResponse("Task not found", 404);
          }

          // Use coordinator for triple-write
          await coordinator.deleteTask(taskId);

          wsManager.broadcast({
            type: "task_deleted",
            data: { id: taskId },
          });

          return jsonResponse({ success: true });
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // List reviews (tasks with label=review)
    {
      method: "GET",
      path: "/api/reviews",
      handler: async (req) => {
        const url = new URL(req.url);
        const statusFilter = url.searchParams.get("status");

        const nodeIds = graph().getNodeIds();
        const { isReviewTask, getSlaRisk } = await import("../../src/review.ts");

        const reviews = nodeIds.filter((id) => {
          const props = graph().getNodeProperties(id) as TaskProperties;
          if (!props?.type || props.type !== "task" || !isReviewTask(props)) {
            return false;
          }

          // Apply status filter (default to "created" for pending reviews)
          if (statusFilter && props.state !== statusFilter) {
            return false;
          } else if (!statusFilter && props.state !== "created") {
            return false;
          }

          return true;
        });

        const reviewList = reviews.map((id) => {
          const props = graph().getNodeProperties(id) as TaskProperties;

          // Calculate SLA risk if review has deadline
          let slaRisk = "low";
          if ((props as any).slaDeadline && props.createdAt) {
            slaRisk = getSlaRisk(
              props.createdAt,
              (props as any).slaDeadline,
              new Date()
            );
          }

          return {
            id,
            ...props,
            slaRisk,
          };
        });

        // Sort by priority (P0 first), then by SLA risk
        reviewList.sort((a, b) => {
          const priorityA = a.priority ?? 4;
          const priorityB = b.priority ?? 4;

          if (priorityA !== priorityB) {
            return priorityA - priorityB;
          }

          // Within same priority, sort by SLA risk
          const riskOrder = { breached: 0, high: 1, medium: 2, low: 3 };
          return (riskOrder[a.slaRisk as keyof typeof riskOrder] || 3) -
                 (riskOrder[b.slaRisk as keyof typeof riskOrder] || 3);
        });

        return jsonResponse(reviewList);
      },
    },

    // Approve review
    {
      method: "POST",
      path: /^\/api\/reviews\/([^\/]+)\/approve$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const reviewId = params.id;
          const body = await req.json();
          const comment = body.comment || "";

          const { isReviewTask, ReviewStateMachine } = await import("../../src/review.ts");
          const props = graph().getNodeProperties(reviewId) as TaskProperties;

          if (!props || !isReviewTask(props)) {
            return errorResponse("Review not found", 404);
          }

          const stateMachine = new ReviewStateMachine();

          // Execute approval transition
          const result = stateMachine.transition(props.state as any, "approve", { comment });

          if (!result.success) {
            return errorResponse(result.error || "Cannot approve review", 400);
          }

          // Update review state
          props.state = result.newState;
          (props as any).completedAt = new Date();
          (props as any).approvalStatus = "approved";
          (props as any).approvalComment = comment;

          // Auto-pass criteria
          for (const criterion of props.objectiveSuccessCriteria) {
            criterion.actual = criterion.threshold;
          }

          // Log event
          eventLog.append({
            timestamp: new Date().toISOString(),
            type: "review_approved",
            nodeId: reviewId,
            data: { goal: props.goal, comment },
          });

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "review_approved",
            data: { id: reviewId, comment },
          });

          return jsonResponse({ success: true, newState: result.newState });
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            400
          );
        }
      },
    },

    // Reject review (request changes)
    {
      method: "POST",
      path: /^\/api\/reviews\/([^\/]+)\/reject$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const reviewId = params.id;
          const body = await req.json();
          const comment = body.comment || "";

          if (!comment) {
            return errorResponse("Comment is required for rejection", 400);
          }

          const { isReviewTask, ReviewStateMachine } = await import("../../src/review.ts");
          const props = graph().getNodeProperties(reviewId) as TaskProperties;

          if (!props || !isReviewTask(props)) {
            return errorResponse("Review not found", 404);
          }

          const stateMachine = new ReviewStateMachine();

          // Execute rejection transition
          const result = stateMachine.transition(props.state as any, "reject", { comment });

          if (!result.success) {
            return errorResponse(result.error || "Cannot reject review", 400);
          }

          // Update review state
          props.state = result.newState;
          (props as any).approvalStatus = "rejected";
          (props as any).approvalComment = comment;
          (props as any).blockReason = comment;

          // Log event
          eventLog.append({
            timestamp: new Date().toISOString(),
            type: "review_rejected",
            nodeId: reviewId,
            data: { goal: props.goal, comment },
          });

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "review_rejected",
            data: { id: reviewId, comment },
          });

          return jsonResponse({ success: true, newState: result.newState });
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            400
          );
        }
      },
    },

    // Start review (created -> active)
    {
      method: "POST",
      path: /^\/api\/reviews\/([^\/]+)\/start$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const reviewId = params.id;

          const { isReviewTask, ReviewStateMachine } = await import("../../src/review.ts");
          const props = graph().getNodeProperties(reviewId) as TaskProperties;

          if (!props || !isReviewTask(props)) {
            return errorResponse("Review not found", 404);
          }

          const stateMachine = new ReviewStateMachine();

          // Execute start transition
          const result = stateMachine.transition(props.state as any, "start", {});

          if (!result.success) {
            return errorResponse(result.error || "Cannot start review", 400);
          }

          // Update review state
          props.state = result.newState;
          (props as any).startedAt = new Date();

          // Log event
          eventLog.append({
            timestamp: new Date().toISOString(),
            type: "review_started",
            nodeId: reviewId,
            data: { goal: props.goal },
          });

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "review_started",
            data: { id: reviewId },
          });

          return jsonResponse({ success: true, newState: result.newState });
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            400
          );
        }
      },
    },

    // List active agents (tasks with label=agent, state=active)
    {
      method: "GET",
      path: "/api/agents",
      handler: (req) => {
        const nodeIds = graph().getNodeIds();
        const agents = nodeIds.filter((id) => {
          const props = graph().getNodeProperties(id) as TaskProperties;
          return (
            props?.type === "task" &&
            props.labels?.includes("agent") &&
            props.state === "active"
          );
        });

        const agentList = agents.map((id) => {
          const props = graph().getNodeProperties(id) as TaskProperties;
          return { id, ...props };
        });

        return jsonResponse(agentList);
      },
    },

    // Get timeline (from EventLog)
    {
      method: "GET",
      path: "/api/timeline",
      handler: (req) => {
        const url = new URL(req.url);
        const eventType = url.searchParams.get("type");
        const limit = parseInt(url.searchParams.get("limit") || "100");

        let events = eventType
          ? eventLog.getEventsByType(eventType)
          : eventLog.getAllEvents();

        // Reverse chronological order
        events = events.reverse();

        // Limit results
        if (limit > 0) {
          events = events.slice(0, limit);
        }

        return jsonResponse(events);
      },
    },

    // Task evaluation
    {
      method: "POST",
      path: /^\/api\/tasks\/([^\/]+)\/eval$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const taskId = params.id;
          const result = await graph().send(taskId, "eval", {});
          return jsonResponse(result);
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // Task status (with blockers)
    {
      method: "GET",
      path: /^\/api\/tasks\/([^\/]+)\/status$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const taskId = params.id;
          const result = await graph().send(taskId, "query_status", {});
          return jsonResponse(result);
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // Search tasks (local graph search)
    {
      method: "GET",
      path: "/api/tasks/search",
      handler: async (req) => {
        const url = new URL(req.url);
        const query = url.searchParams.get("q") || "";
        const normalizedQuery = query.toLowerCase();

        const nodeIds = graph().getNodeIds();
        const tasks = nodeIds.filter((id) => {
          const props = graph().getNodeProperties(id);
          return props?.type === "task";
        });

        const matches = tasks.filter(id => {
          const props = graph().getNodeProperties(id) as TaskProperties;
          return props.goal.toLowerCase().includes(normalizedQuery) ||
                 props.desiredDeliverables.some(d => d.toLowerCase().includes(normalizedQuery));
        }).map(id => ({ id, ...graph().getNodeProperties(id) }));

        return jsonResponse(matches);
      },
    },

    // Create edge (dependency)
    {
      method: "POST",
      path: "/api/edges",
      handler: async (req) => {
        try {
          const { fromId, toId, type, properties } = await req.json();
          if (!fromId || !toId || !type) {
            return errorResponse("Missing fromId, toId, or type", 400);
          }

          let edgeId: string;
          if (type === "depends_on") {
            edgeId = await coordinator.addDependency(fromId, toId);
          } else {
            const edge = graph().addEdge(fromId, toId, type as any, properties || {});
            edgeId = edge.id;
          }

          return jsonResponse({ id: edgeId, fromId, toId, type, properties }, 201);
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // Delete edge
    {
      method: "DELETE",
      path: /^\/api\/edges\/([^\/]+)$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const edgeId = params.id;
          const success = graph().removeEdge(edgeId);
          if (!success) return errorResponse("Edge not found", 404);
          return jsonResponse({ success: true });
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // CozoDB Ready Tasks
    {
      method: "GET",
      path: "/api/queries/ready",
      handler: async () => {
        const { TaskQueryService } = await import("../../src/dual-write-coordinator.ts");
        const queryService = new TaskQueryService(coordinator.getCozoDB());
        const result = await queryService.findReadyTasks();
        return jsonResponse(result);
      },
    },

    // CozoDB Blocked Tasks
    {
      method: "GET",
      path: "/api/queries/blocked",
      handler: async () => {
        const { TaskQueryService } = await import("../../src/dual-write-coordinator.ts");
        const queryService = new TaskQueryService(coordinator.getCozoDB());
        const result = await queryService.findBlockedTasks();
        return jsonResponse(result);
      },
    },

    // CozoDB Search
    {
      method: "GET",
      path: "/api/queries/search",
      handler: async (req) => {
        const url = new URL(req.url);
        const keyword = url.searchParams.get("q") || "";
        const { TaskQueryService } = await import("../../src/dual-write-coordinator.ts");
        const queryService = new TaskQueryService(coordinator.getCozoDB());
        const result = await queryService.searchTasks(keyword);
        return jsonResponse(result);
      },
    },

    // Batch Add Tasks
    {
      method: "POST",
      path: "/api/batch/tasks",
      handler: async (req) => {
        try {
          const { specs } = await req.json();
          if (!Array.isArray(specs)) {
            return errorResponse("Specs must be an array", 400);
          }

          const created: string[] = [];
          const errors: any[] = [];

          for (let i = 0; i < specs.length; i++) {
            const spec = specs[i];
            try {
              if (!spec.goal) throw new Error("Missing goal");

              const taskId = await coordinator.createTask({
                goal: spec.goal,
                desiredDeliverables: spec.deliverables || ["Task completion"],
                objectiveSuccessCriteria: spec.criteria || [
                  { criterion: "Task marked complete", measure: "Manual completion", threshold: true }
                ],
                labels: spec.labels,
                priority: spec.priority,
                parentTaskId: spec.parent,
                toolsAvailable: ["CLI"],
              });

              // Handle dependencies
              if (spec.depends && spec.depends.length > 0) {
                for (const depId of spec.depends) {
                  await coordinator.addDependency(taskId, depId);
                }
              }

              // Handle parent edge if not covered by parentTaskId logic
              // coordinator.createTask handles parentTaskId automatically via TaskActor props
              // But we might want explicit spawned_by edge if specified separately
              if (spec.parent) {
                 // Check if edge exists, if not add it
                 // Actually coordinator.createTask doesn't auto-add edge in graph? 
                 // Let's check. Yes, TaskActor logic doesn't auto-add edge.
                 // So we should add it.
                 try {
                   graph().addEdge(taskId, spec.parent, "spawned_by");
                 } catch (e) {
                   // Ignore if already exists or fails
                 }
              }

              created.push(taskId);
            } catch (error) {
              errors.push({
                index: i,
                error: error instanceof Error ? error.message : String(error),
              });
            }
          }

          return jsonResponse({ created, errors }, 201);
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // Batch Update Tasks
    {
      method: "POST",
      path: "/api/batch/tasks/update",
      handler: async (req) => {
        try {
          const { specs } = await req.json();
          if (!Array.isArray(specs)) {
            return errorResponse("Specs must be an array", 400);
          }

          const updated: string[] = [];
          const errors: any[] = [];

          for (let i = 0; i < specs.length; i++) {
            const spec = specs[i];
            try {
              if (!spec.id) throw new Error("Missing id");
              if (!spec.action) throw new Error("Missing action");

              const taskId = spec.id;
              
              // Handle completion auto-pass criteria logic similar to update endpoint
              if (spec.action === "complete") {
                 const props = graph().getNodeProperties(taskId) as TaskProperties;
                 if (props) {
                   for (const criterion of props.objectiveSuccessCriteria) {
                     criterion.actual = criterion.threshold;
                   }
                 }
              }

              await coordinator.updateTask(taskId, spec.action, {
                result: spec.result,
                reason: spec.reason,
              });

              updated.push(taskId);
            } catch (error) {
              errors.push({
                index: i,
                id: spec.id,
                error: error instanceof Error ? error.message : String(error),
              });
            }
          }

          return jsonResponse({ updated, errors });
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // ===== Semantic Analysis Routes =====

    // Capture idea/prompt
    {
      method: "POST",
      path: "/api/semantic/capture",
      handler: async (req) => {
        try {
          const message = await req.json();

          // Support both actor message format and direct payload
          const payload = message.payload || message;

          const result = await ideasSupervisor.handleMessage({
            id: message.id || crypto.randomUUID(),
            type: "capture",
            payload,
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "idea_captured",
            data: result.data,
          });

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Query ideas
    {
      method: "POST",
      path: "/api/ideas/query",
      handler: async (req) => {
        try {
          const message = await req.json();

          const payload = message.payload || message;

          const result = await ideasSupervisor.handleMessage({
            id: message.id || crypto.randomUUID(),
            type: "query",
            payload,
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Get specific cluster
    {
      method: "GET",
      path: /^\/api\/ideas\/clusters\/(\d+)$/,
      paramNames: ["clusterId"],
      handler: async (req, params) => {
        try {
          const clusterId = parseInt(params.clusterId);

          const result = await ideasSupervisor.handleMessage({
            id: crypto.randomUUID(),
            type: "get_cluster",
            payload: { clusterId },
          });

          if (!result.success) {
            return jsonResponse(result, result.code === "NOT_FOUND" ? 404 : 400);
          }

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Link cluster to task
    {
      method: "POST",
      path: "/api/ideas/link",
      handler: async (req) => {
        try {
          const message = await req.json();

          const payload = message.payload || message;

          const result = await ideasSupervisor.handleMessage({
            id: message.id || crypto.randomUUID(),
            type: "link_to_task",
            payload,
          });

          if (!result.success) {
            return jsonResponse(result, result.code === "NOT_FOUND" ? 404 : 400);
          }

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "idea_linked",
            data: result.data,
          });

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Get ideas statistics
    {
      method: "GET",
      path: "/api/ideas/stats",
      handler: async (req) => {
        try {
          const result = await ideasSupervisor.handleMessage({
            id: crypto.randomUUID(),
            type: "get_stats",
            payload: {},
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // ===== Signal Detection Routes =====

    // Fetch YouTube signals
    {
      method: "POST",
      path: "/api/signals/fetch/youtube",
      handler: async (req) => {
        try {
          const message = await req.json();
          const payload = message.payload || message;

          const result = await signalsSupervisor.send({
            id: message.id || crypto.randomUUID(),
            type: "fetch_signals",
            payload,
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "signals_updated",
            data: result.data,
          });

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Fetch signals from YouTube playlist
    {
      method: "POST",
      path: "/api/signals/fetch/playlist",
      handler: async (req) => {
        try {
          const message = await req.json();
          const payload = message.payload || message;
          const { playlistId } = payload;

          if (!playlistId) {
            return errorResponse("Missing playlistId", 400);
          }

          const result = await signalsSupervisor.send({
            id: message.id || crypto.randomUUID(),
            type: "fetch_from_playlist",
            payload: { playlistId },
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "signals_updated",
            data: result.data,
          });

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Process a single video
    {
      method: "POST",
      path: "/api/signals/process",
      handler: async (req) => {
        try {
          const message = await req.json();
          const payload = message.payload || message;

          const result = await signalsSupervisor.send({
            id: message.id || crypto.randomUUID(),
            type: "process_video",
            payload,
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          // Broadcast to WebSocket clients
          wsManager.broadcast({
            type: "signal_processed",
            data: result.data,
          });

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Search YouTube videos (doesn't store yet)
    {
      method: "POST",
      path: "/api/signals/search",
      handler: async (req) => {
        try {
          const message = await req.json();
          const payload = message.payload || message;

          const result = await signalsSupervisor.send({
            id: message.id || crypto.randomUUID(),
            type: "search_videos",
            payload,
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Query signals
    {
      method: "POST",
      path: "/api/signals/query",
      handler: async (req) => {
        try {
          const message = await req.json();
          const payload = message.payload || message;

          const result = await signalsSupervisor.send({
            id: message.id || crypto.randomUUID(),
            type: "query",
            payload,
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // List all nodes
    {
      method: "GET",
      path: "/api/nodes",
      handler: (req) => {
        const url = new URL(req.url);
        const type = url.searchParams.get("type");

        const nodeIds = graph().getNodeIds();
        let nodeList = nodeIds.map(id => ({ id, ...graph().getNodeProperties(id) }));

        if (type) {
          nodeList = nodeList.filter(n => n.type === type);
        }

        return jsonResponse(nodeList);
      },
    },

    // Get node by ID
    {
      method: "GET",
      path: /^\/api\/nodes\/([^\/]+)$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        const nodeId = params.id;
        const props = graph().getNodeProperties(nodeId);

        if (!props) {
          return errorResponse("Node not found", 404);
        }

        const outgoing = graph().getEdgesFrom(nodeId);
        const incoming = graph().getEdgesTo(nodeId);

        return jsonResponse({
          id: nodeId,
          ...props,
          edges: { outgoing, incoming },
        });
      },
    },

    // Create node
    {
      method: "POST",
      path: "/api/nodes",
      handler: async (req) => {
        try {
          const body = await req.json();
          const { id, type, ...data } = body;

          if (!id || !type) {
            return errorResponse("Missing id or type", 400);
          }

          if (graph().getNodeProperties(id)) {
            return errorResponse(`Node ${id} already exists`, 409);
          }

          // Register dummy actor for generic nodes
          const actor = {
            send: async () => ({ success: true, data: {} }),
          };
          const address = graph().getSystem().register(actor);
          graph().registerNode(id, address, {
            id,
            type,
            createdAt: new Date(),
            ...data,
          });

          return jsonResponse({ id, type, ...data }, 201);
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // Delete node
    {
      method: "DELETE",
      path: /^\/api\/nodes\/([^\/]+)$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const nodeId = params.id;
          const success = graph().removeNode(nodeId);
          if (!success) return errorResponse("Node not found", 404);
          return jsonResponse({ success: true });
        } catch (error) {
          return errorResponse(error instanceof Error ? error.message : String(error), 400);
        }
      },
    },

    // List all edges
    {
      method: "GET",
      path: "/api/edges",
      handler: (req) => {
        const url = new URL(req.url);
        const from = url.searchParams.get("from");
        const to = url.searchParams.get("to");
        const type = url.searchParams.get("type");

        const dump = graph().dump();
        let edges = dump.edges;

        if (from) edges = edges.filter(e => e.fromId === from);
        if (to) edges = edges.filter(e => e.toId === to);
        if (type) edges = edges.filter(e => e.type === type);

        return jsonResponse(edges);
      },
    },

    // Get full graph dump
    {
      method: "GET",
      path: "/api/graph/dump",
      handler: () => {
        return jsonResponse(graph().dump());
      },
    },

    // List all signals
    {
      method: "GET",
      path: "/api/signals",
      handler: async (req) => {
        try {
          const url = new URL(req.url);
          const source = url.searchParams.get("source") || undefined;
          const keyword = url.searchParams.get("keyword") || undefined;
          const limit = parseInt(url.searchParams.get("limit") || "50");
          const offset = parseInt(url.searchParams.get("offset") || "0");

          const result = await signalsSupervisor.send({
            id: crypto.randomUUID(),
            type: "query",
            payload: { source, keyword, limit, offset },
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          return jsonResponse(result.data);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Get signal statistics
    {
      method: "GET",
      path: "/api/signals/stats",
      handler: async (req) => {
        try {
          const result = await signalsSupervisor.send({
            id: crypto.randomUUID(),
            type: "get_stats",
            payload: {},
          });

          if (!result.success) {
            return jsonResponse(result, 400);
          }

          return jsonResponse(result);
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },

    // Get specific signal
    {
      method: "GET",
      path: /^\/api\/signals\/([^\/]+)$/,
      paramNames: ["id"],
      handler: async (req, params) => {
        try {
          const signalId = params.id;

          const props = graph().getNodeProperties(signalId);

          if (!props || (props as any).type !== "signal") {
            return errorResponse("Signal not found", 404);
          }

          const edges = graph().getAllEdges(signalId);

          return jsonResponse({
            id: signalId,
            ...props,
            edges,
          });
        } catch (error) {
          return errorResponse(
            error instanceof Error ? error.message : String(error),
            500
          );
        }
      },
    },
  ];
}
