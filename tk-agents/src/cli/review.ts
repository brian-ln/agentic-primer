#!/usr/bin/env bun

/**
 * Review CLI - Manage review workflow for agent deliverables
 *
 * Commands:
 *   review create <parent-task-id> <goal> [options]  Create a review task
 *   review list [--status] [--priority]              List pending reviews
 *   review show <review-id>                          Show review details
 *   review start <review-id>                         Start reviewing (created -> active)
 *   review approve <review-id> [--comment]           Approve review (active -> completed)
 *   review reject <review-id> --comment <reason>     Reject review (active -> blocked)
 *   review request-changes <review-id> --comment     Request changes (active -> blocked)
 *   review address <review-id> [--comment]           Address feedback (blocked -> active)
 */

import { Graph } from "../graph.ts";
import { TaskActor } from "../task.ts";
import type { TaskProperties } from "../types.ts";
import {
  ReviewStateMachine,
  createReviewProperties,
  isReviewTask,
  calculateSlaDeadline,
  getSlaRisk,
  type ReviewState,
  type ReviewTransition,
  type ReviewProperties,
} from "../review.ts";
import { readFileSync, writeFileSync, existsSync } from "fs";
import { resolve } from "path";

const TASKS_FILE = "tasks.json";

// File format - matches task.ts structure
interface TaskFile {
  nodes: any[];
  edges: any[];
}

// Load graph from tasks.json
function loadGraph(filePath: string): Graph {
  if (!existsSync(filePath)) {
    throw new Error(`Task file not found: ${filePath}`);
  }

  const content = readFileSync(filePath, "utf-8");
  const taskFile: TaskFile = JSON.parse(content);

  const graph = new Graph();

  // Recreate nodes
  for (const nodeProps of taskFile.nodes) {
    if (nodeProps.type === "task") {
      const taskProps = nodeProps as any;
      TaskActor({
        goal: taskProps.goal,
        desiredDeliverables: taskProps.desiredDeliverables,
        objectiveSuccessCriteria: taskProps.objectiveSuccessCriteria,
        subjectiveSuccessCriteria: taskProps.subjectiveSuccessCriteria,
        informationGaps: taskProps.informationGaps,
        toolsAvailable: taskProps.toolsAvailable,
        parentTaskId: taskProps.parentTaskId,
        labels: taskProps.labels,
        priority: taskProps.priority,
        graph,
      });

      // Fix ID mismatch
      const allIds = graph.getNodeIds();
      const newId = allIds[allIds.length - 1];
      if (newId !== nodeProps.id) {
         const address = (graph as any).nodes.get(newId);
         const props = graph.getNodeProperties(newId);
         (graph as any).nodes.delete(newId);
         (graph as any).nodeProperties.delete(newId);
         props.id = nodeProps.id;
         (graph as any).nodes.set(nodeProps.id, address);
         (graph as any).nodeProperties.set(nodeProps.id, props);
      }
      Object.assign(graph.getNodeProperties(nodeProps.id), nodeProps);
    } else {
      // Generic registration
      const actor = { send: async () => ({ success: true, data: {} }) };
      const address = graph.getSystem().register(actor);
      graph.registerNode(nodeProps.id, address, nodeProps);
    }
  }

  // Recreate edges
  for (const edge of taskFile.edges) {
    const edgeNum = parseInt(edge.id.replace("edge_", ""));
    if (!isNaN(edgeNum)) {
      graph.setEdgeCounter(Math.max(edgeNum, 0));
    }
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
  }

  return graph;
}

// Save graph to tasks.json
function saveGraph(graph: Graph, filePath: string): void {
  const nodes = graph.getNodeIds().map((id) => graph.getNodeProperties(id));
  const edges = graph.getNodeIds().flatMap((id) => graph.getAllEdges(id));

  // Deduplicate edges
  const uniqueEdges = Array.from(
    new Map(edges.map((e) => [e.id, e])).values()
  );

  const taskFile: TaskFile = { nodes, edges: uniqueEdges };
  writeFileSync(filePath, JSON.stringify(taskFile, null, 2));
}

// Generate unique review task ID
function generateReviewId(graph: Graph): string {
  const existingIds = graph.getNodeIds().filter((id) => id.startsWith("task_review_"));
  const numbers = existingIds
    .map((id) => parseInt(id.replace("task_review_", "")))
    .filter((n) => !isNaN(n));

  const nextNum = numbers.length > 0 ? Math.max(...numbers) + 1 : 1;
  return `task_review_${nextNum}`;
}

// Format duration (ms) as human-readable string
function formatDuration(ms: number): string {
  const hours = Math.floor(ms / (1000 * 60 * 60));
  const minutes = Math.floor((ms % (1000 * 60 * 60)) / (1000 * 60));

  if (hours > 24) {
    const days = Math.floor(hours / 24);
    return `${days}d ${hours % 24}h`;
  } else if (hours > 0) {
    return `${hours}h ${minutes}m`;
  } else {
    return `${minutes}m`;
  }
}

// Format SLA status
function formatSlaStatus(createdAt: Date | string, slaDeadline: Date | string, now: Date = new Date()): string {
  const risk = getSlaRisk(createdAt, slaDeadline, now);
  const deadlineDate = slaDeadline instanceof Date ? slaDeadline : new Date(slaDeadline);
  const timeRemaining = deadlineDate.getTime() - now.getTime();

  if (risk === "breached") {
    const overdue = Math.abs(timeRemaining);
    return `⚠️  BREACHED (${formatDuration(overdue)} overdue)`;
  } else {
    return `${formatDuration(timeRemaining)} remaining`;
  }
}

// Commands

function createReview(args: string[]): void {
  const parentTaskId = args[0];
  const goal = args[1];

  if (!parentTaskId || !goal) {
    console.error("Usage: review create <parent-task-id> <goal> [--labels <labels>] [--priority <0-4>] [--deliverables <files>] [--assignee <user>]");
    process.exit(1);
  }

  // Parse options
  let labels: string[] = [];
  let priority: 0 | 1 | 2 | 3 | 4 = 2; // Default P2
  let deliverables: string[] = [];
  let assignee = "bln";

  for (let i = 2; i < args.length; i++) {
    if (args[i] === "--labels" && args[i + 1]) {
      labels = args[i + 1].split(",");
      i++;
    } else if (args[i] === "--priority" && args[i + 1]) {
      priority = parseInt(args[i + 1]) as 0 | 1 | 2 | 3 | 4;
      i++;
    } else if (args[i] === "--deliverables" && args[i + 1]) {
      deliverables = args[i + 1].split(",");
      i++;
    } else if (args[i] === "--assignee" && args[i + 1]) {
      assignee = args[i + 1];
      i++;
    }
  }

  const filePath = resolve(process.cwd(), TASKS_FILE);
  const graph = loadGraph(filePath);

  // Validate parent task exists
  const parentTask = graph.getNodeProperties(parentTaskId);
  if (!parentTask) {
    console.error(`Error: Parent task not found: ${parentTaskId}`);
    process.exit(1);
  }

  // Create review properties
  const reviewProps = createReviewProperties({
    goal,
    parentTaskId,
    labels,
    priority,
    assignee,
    deliverables,
  });

  // Generate unique review ID
  const reviewId = generateReviewId(graph);

  // Create review task using TaskActor
  TaskActor({
    ...reviewProps,
    graph,
  });

  // Fix ID to use our generated review ID
  const allIds = graph.getNodeIds();
  const newId = allIds[allIds.length - 1];

  if (newId !== reviewId) {
    const address = (graph as any).nodes.get(newId);
    const props = graph.getNodeProperties(newId);

    (graph as any).nodes.delete(newId);
    (graph as any).nodeProperties.delete(newId);

    if (address && props) {
      props.id = reviewId;
      (graph as any).nodes.set(reviewId, address);
      (graph as any).nodeProperties.set(reviewId, props);
    }
  }

  // Set review-specific properties on the created task
  const taskProps = graph.getNodeProperties(reviewId);
  if (taskProps) {
    (taskProps as any).assignee = assignee;
    (taskProps as any).slaDeadline = reviewProps.slaDeadline;
    (taskProps as any).reviewType = reviewProps.reviewType;
  }

  // Add spawned_by edge to parent
  graph.addEdge(reviewId, parentTaskId, "spawned_by");

  // Save graph
  saveGraph(graph, filePath);

  console.log(`Created review: ${reviewId}`);
  console.log(`Goal: ${goal}`);
  console.log(`Parent: ${parentTaskId}`);
  console.log(`Priority: P${priority}`);
  console.log(`Assignee: ${assignee}`);
  if (deliverables.length > 0) {
    console.log(`Deliverables: ${deliverables.join(", ")}`);
  }
}

function listReviews(args: string[]): void {
  // Parse filters
  let statusFilter: ReviewState | undefined;
  let priorityFilter: number | undefined;

  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--status" && args[i + 1]) {
      statusFilter = args[i + 1] as ReviewState;
      i++;
    } else if (args[i] === "--priority" && args[i + 1]) {
      priorityFilter = parseInt(args[i + 1]);
      i++;
    }
  }

  const filePath = resolve(process.cwd(), TASKS_FILE);
  const graph = loadGraph(filePath);

  // Find all review tasks
  const reviewTasks = graph.getNodeIds()
    .map((id) => graph.getNodeProperties(id))
    .filter((props): props is ReviewProperties => {
      if (!props || !isReviewTask(props)) return false;

      // Apply filters
      if (statusFilter && props.state !== statusFilter) return false;
      if (priorityFilter !== undefined && props.priority !== priorityFilter) return false;

      return true;
    });

  // Sort by priority (P0 first) then by creation date
  reviewTasks.sort((a, b) => {
    if (a.priority !== b.priority) {
      return (a.priority ?? 4) - (b.priority ?? 4);
    }
    const dateA = a.createdAt instanceof Date ? a.createdAt : new Date(a.createdAt);
    const dateB = b.createdAt instanceof Date ? b.createdAt : new Date(b.createdAt);
    return dateA.getTime() - dateB.getTime();
  });

  if (reviewTasks.length === 0) {
    console.log("No reviews found matching filters.");
    return;
  }

  console.log(`\nPENDING REVIEWS (${reviewTasks.length})\n`);

  for (const review of reviewTasks) {
    const priorityLabel = `[P${review.priority ?? 4}]`;
    const stateLabel = review.state.toUpperCase();
    const created = new Date(review.createdAt);
    const now = new Date();
    const age = now.getTime() - created.getTime();

    console.log(`${priorityLabel} ${review.id} - ${review.goal}`);
    console.log(`     State: ${stateLabel}`);
    console.log(`     Created: ${formatDuration(age)} ago`);

    if (review.slaDeadline) {
      console.log(`     SLA: ${formatSlaStatus(review.createdAt, review.slaDeadline, now)}`);
    }

    if (review.parentTaskId) {
      console.log(`     Parent: ${review.parentTaskId}`);
    }

    if (review.approvalStatus) {
      console.log(`     Status: ${review.approvalStatus}`);
    }

    console.log();
  }
}

function showReview(args: string[]): void {
  const reviewId = args[0];

  if (!reviewId) {
    console.error("Usage: review show <review-id>");
    process.exit(1);
  }

  const filePath = resolve(process.cwd(), TASKS_FILE);
  const graph = loadGraph(filePath);

  const props = graph.getNodeProperties(reviewId);
  if (!props || !isReviewTask(props)) {
    console.error(`Error: Review not found: ${reviewId}`);
    process.exit(1);
  }

  const review = props as ReviewProperties;

  console.log(`\nREVIEW: ${review.id}`);
  console.log(`Goal: ${review.goal}`);
  console.log(`State: ${review.state.toUpperCase()}`);
  console.log(`Priority: P${review.priority ?? 4}`);
  console.log(`Assignee: ${review.assignee}`);

  if (review.slaDeadline) {
    console.log(`SLA: ${formatSlaStatus(review.createdAt, review.slaDeadline)}`);
  }

  if (review.parentTaskId) {
    const parentProps = graph.getNodeProperties(review.parentTaskId);
    console.log(`\nParent Task: ${review.parentTaskId}`);
    if (parentProps) {
      console.log(`  Goal: ${(parentProps as TaskProperties).goal}`);
      console.log(`  State: ${(parentProps as TaskProperties).state}`);
    }
  }

  if (review.desiredDeliverables.length > 0) {
    console.log(`\nDeliverables:`);
    review.desiredDeliverables.forEach((d) => console.log(`  - ${d}`));
  }

  if (review.approvalStatus) {
    console.log(`\nApproval Status: ${review.approvalStatus}`);
  }

  if (review.approvalComment) {
    console.log(`Comment: ${review.approvalComment}`);
  }

  console.log();
}

function executeTransition(
  reviewId: string,
  transition: ReviewTransition,
  comment?: string
): void {
  const filePath = resolve(process.cwd(), TASKS_FILE);
  const graph = loadGraph(filePath);

  const props = graph.getNodeProperties(reviewId);
  if (!props || !isReviewTask(props)) {
    console.error(`Error: Review not found: ${reviewId}`);
    process.exit(1);
  }

  const review = props as ReviewProperties;
  const stateMachine = new ReviewStateMachine();

  // Execute transition
  const result = stateMachine.transition(review.state as ReviewState, transition, {
    comment,
  });

  if (!result.success) {
    console.error(`Error: ${result.error}`);
    process.exit(1);
  }

  // Update review state
  review.state = result.newState;

  // Apply side effects
  if (transition === "start") {
    review.startedAt = new Date();
  } else if (transition === "approve") {
    review.completedAt = new Date();
    (review as any).approvalStatus = "approved";
    review.approvalComment = comment || "";
  } else if (transition === "reject") {
    (review as any).approvalStatus = "rejected";
    review.approvalComment = comment || "";
    (review as any).blockReason = comment;
  } else if (transition === "request_changes") {
    (review as any).approvalStatus = "changes_requested";
    review.approvalComment = comment || "";
    (review as any).blockReason = comment;
  } else if (transition === "address_feedback") {
    delete (review as any).blockReason;
  }

  // Save graph
  saveGraph(graph, filePath);

  console.log(`Review ${reviewId}: ${transition} -> ${result.newState}`);
  if (result.sideEffects && result.sideEffects.length > 0) {
    console.log("\nSide effects:");
    result.sideEffects.forEach((effect) => console.log(`  - ${effect}`));
  }
}

function startReview(args: string[]): void {
  const reviewId = args[0];
  if (!reviewId) {
    console.error("Usage: review start <review-id>");
    process.exit(1);
  }
  executeTransition(reviewId, "start");
}

function approveReview(args: string[]): void {
  const reviewId = args[0];
  if (!reviewId) {
    console.error("Usage: review approve <review-id> [--comment <text>]");
    process.exit(1);
  }

  let comment: string | undefined;
  for (let i = 1; i < args.length; i++) {
    if (args[i] === "--comment" && args[i + 1]) {
      comment = args[i + 1];
      i++;
    }
  }

  executeTransition(reviewId, "approve", comment);
}

function rejectReview(args: string[]): void {
  const reviewId = args[0];
  if (!reviewId) {
    console.error("Usage: review reject <review-id> --comment <reason>");
    process.exit(1);
  }

  let comment: string | undefined;
  for (let i = 1; i < args.length; i++) {
    if (args[i] === "--comment" && args[i + 1]) {
      comment = args[i + 1];
      i++;
    }
  }

  if (!comment) {
    console.error("Error: --comment is required for reject");
    process.exit(1);
  }

  executeTransition(reviewId, "reject", comment);
}

function requestChanges(args: string[]): void {
  const reviewId = args[0];
  if (!reviewId) {
    console.error("Usage: review request-changes <review-id> --comment <changes>");
    process.exit(1);
  }

  let comment: string | undefined;
  for (let i = 1; i < args.length; i++) {
    if (args[i] === "--comment" && args[i + 1]) {
      comment = args[i + 1];
      i++;
    }
  }

  if (!comment) {
    console.error("Error: --comment is required for request-changes");
    process.exit(1);
  }

  executeTransition(reviewId, "request_changes", comment);
}

function addressFeedback(args: string[]): void {
  const reviewId = args[0];
  if (!reviewId) {
    console.error("Usage: review address <review-id> [--comment <notes>]");
    process.exit(1);
  }

  let comment: string | undefined;
  for (let i = 1; i < args.length; i++) {
    if (args[i] === "--comment" && args[i + 1]) {
      comment = args[i + 1];
      i++;
    }
  }

  executeTransition(reviewId, "address_feedback", comment);
}

// Main CLI dispatcher
function main(): void {
  const args = process.argv.slice(2);
  const command = args[0];
  const commandArgs = args.slice(1);

  switch (command) {
    case "create":
      createReview(commandArgs);
      break;

    case "list":
      listReviews(commandArgs);
      break;

    case "show":
      showReview(commandArgs);
      break;

    case "start":
      startReview(commandArgs);
      break;

    case "approve":
      approveReview(commandArgs);
      break;

    case "reject":
      rejectReview(commandArgs);
      break;

    case "request-changes":
      requestChanges(commandArgs);
      break;

    case "address":
      addressFeedback(commandArgs);
      break;

    default:
      console.error("Unknown command:", command);
      console.error("\nAvailable commands:");
      console.error("  review create <parent-task-id> <goal> [options]");
      console.error("  review list [--status <state>] [--priority <0-4>]");
      console.error("  review show <review-id>");
      console.error("  review start <review-id>");
      console.error("  review approve <review-id> [--comment <text>]");
      console.error("  review reject <review-id> --comment <reason>");
      console.error("  review request-changes <review-id> --comment <changes>");
      console.error("  review address <review-id> [--comment <notes>]");
      process.exit(1);
  }
}

main();
