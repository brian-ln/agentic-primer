// Review Workflow - State machine and node type for managing agent deliverable reviews

import type { NodeProperties, TaskProperties, TaskState } from "./types.ts";

/**
 * Review-specific states that extend the base TaskState
 * Maps to task states: created, active, blocked, completed
 */
export type ReviewState = "created" | "active" | "blocked" | "completed";

/**
 * Review transitions define the allowed state changes
 */
export type ReviewTransition =
  | "start" // created -> active
  | "approve" // active -> completed
  | "reject" // active -> blocked
  | "request_changes" // active -> blocked
  | "address_feedback"; // blocked -> active

/**
 * Approval status for review results
 */
export type ApprovalStatus = "approved" | "rejected" | "changes_requested";

/**
 * Review node properties - extends TaskProperties with review-specific fields
 */
export interface ReviewProperties extends TaskProperties {
  type: "task";
  labels: string[]; // Must include "review"

  // Review-specific fields
  assignee: string; // User who will review (typically "bln")
  reviewType?: "approval" | "feedback" | "inspection";
  slaDeadline?: Date; // Calculated from priority

  // Results
  approvalStatus?: ApprovalStatus;
  approvalComment?: string;

  // Linkage (via parentTaskId from TaskProperties)
  // parentTaskId: string; // Agent task that spawned this review
}

/**
 * State transition result
 */
export interface TransitionResult {
  success: boolean;
  newState: ReviewState;
  error?: string;
  sideEffects?: string[];
}

/**
 * Review State Machine
 *
 * Manages state transitions with preconditions and postconditions
 *
 * States:
 * - created: Review task created, waiting to be started
 * - active: Review in progress
 * - blocked: Review rejected or changes requested
 * - completed: Review approved
 *
 * Transitions:
 * - start: created -> active
 * - approve: active -> completed
 * - reject: active -> blocked
 * - request_changes: active -> blocked
 * - address_feedback: blocked -> active
 */
export class ReviewStateMachine {
  /**
   * Validate a state transition
   */
  canTransition(currentState: ReviewState, transition: ReviewTransition): boolean {
    const validTransitions: Record<ReviewState, ReviewTransition[]> = {
      created: ["start"],
      active: ["approve", "reject", "request_changes"],
      blocked: ["address_feedback"],
      completed: [], // Terminal state
    };

    return validTransitions[currentState]?.includes(transition) ?? false;
  }

  /**
   * Execute a state transition with side effects
   */
  transition(
    currentState: ReviewState,
    transition: ReviewTransition,
    context: {
      comment?: string;
      userId?: string;
    } = {}
  ): TransitionResult {
    // Validate transition is allowed
    if (!this.canTransition(currentState, transition)) {
      return {
        success: false,
        newState: currentState,
        error: `Invalid transition '${transition}' from state '${currentState}'`,
      };
    }

    // Execute transition with preconditions and postconditions
    switch (transition) {
      case "start":
        return this.executeStart(currentState, context);

      case "approve":
        return this.executeApprove(currentState, context);

      case "reject":
        return this.executeReject(currentState, context);

      case "request_changes":
        return this.executeRequestChanges(currentState, context);

      case "address_feedback":
        return this.executeAddressFeedback(currentState, context);

      default:
        return {
          success: false,
          newState: currentState,
          error: `Unknown transition: ${transition}`,
        };
    }
  }

  /**
   * Start review: created -> active
   */
  private executeStart(
    _currentState: ReviewState,
    _context: { comment?: string; userId?: string }
  ): TransitionResult {
    return {
      success: true,
      newState: "active",
      sideEffects: [
        "Set startedAt timestamp",
        "Log review_started event",
        "Start SLA clock",
      ],
    };
  }

  /**
   * Approve review: active -> completed
   */
  private executeApprove(
    _currentState: ReviewState,
    context: { comment?: string; userId?: string }
  ): TransitionResult {
    return {
      success: true,
      newState: "completed",
      sideEffects: [
        "Set completedAt timestamp",
        "Set approvalStatus to 'approved'",
        context.comment ? `Set approvalComment: ${context.comment}` : "Set approvalComment to empty",
        "Log review_approved event",
        "Unblock parent task",
        "Unblock dependent tasks",
      ],
    };
  }

  /**
   * Reject review: active -> blocked
   */
  private executeReject(
    _currentState: ReviewState,
    context: { comment?: string; userId?: string }
  ): TransitionResult {
    // Precondition: Must provide rejection reason
    if (!context.comment) {
      return {
        success: false,
        newState: "active",
        error: "Rejection requires a comment explaining the reason",
      };
    }

    return {
      success: true,
      newState: "blocked",
      sideEffects: [
        "Set approvalStatus to 'rejected'",
        `Set blockReason: ${context.comment}`,
        `Set approvalComment: ${context.comment}`,
        "Log review_rejected event",
        "Block parent task",
        "Notify agent with feedback",
      ],
    };
  }

  /**
   * Request changes: active -> blocked
   */
  private executeRequestChanges(
    _currentState: ReviewState,
    context: { comment?: string; userId?: string }
  ): TransitionResult {
    // Precondition: Must provide change requests
    if (!context.comment) {
      return {
        success: false,
        newState: "active",
        error: "Request changes requires a comment with specific changes needed",
      };
    }

    return {
      success: true,
      newState: "blocked",
      sideEffects: [
        "Set approvalStatus to 'changes_requested'",
        `Set blockReason: ${context.comment}`,
        `Set approvalComment: ${context.comment}`,
        "Log review_changes_requested event",
        "Flag parent task",
        "Notify agent with feedback",
      ],
    };
  }

  /**
   * Address feedback: blocked -> active
   */
  private executeAddressFeedback(
    _currentState: ReviewState,
    context: { comment?: string; userId?: string }
  ): TransitionResult {
    return {
      success: true,
      newState: "active",
      sideEffects: [
        "Clear blockReason",
        context.comment ? `Add feedback note: ${context.comment}` : "Add feedback note",
        "Log review_feedback_addressed event",
        "Re-enter review queue",
        "Notify reviewer of updates",
      ],
    };
  }

  /**
   * Get all valid transitions for a given state
   */
  getValidTransitions(state: ReviewState): ReviewTransition[] {
    const validTransitions: Record<ReviewState, ReviewTransition[]> = {
      created: ["start"],
      active: ["approve", "reject", "request_changes"],
      blocked: ["address_feedback"],
      completed: [],
    };

    return validTransitions[state] ?? [];
  }

  /**
   * Get the target state for a transition (without executing)
   */
  getTargetState(transition: ReviewTransition): ReviewState {
    const targetStates: Record<ReviewTransition, ReviewState> = {
      start: "active",
      approve: "completed",
      reject: "blocked",
      request_changes: "blocked",
      address_feedback: "active",
    };

    return targetStates[transition];
  }
}

/**
 * Calculate SLA deadline based on priority
 */
export function calculateSlaDeadline(priority: 0 | 1 | 2 | 3 | 4, createdAt: Date): Date {
  const slaHours: Record<number, number> = {
    0: 1, // P0: 1 hour
    1: 8, // P1: 8 hours
    2: 48, // P2: 2 days
    3: 168, // P3: 1 week
    4: 168, // P4: 1 week (default)
  };

  const hours = slaHours[priority] ?? 168;
  const deadline = new Date(createdAt);
  deadline.setHours(deadline.getHours() + hours);
  return deadline;
}

/**
 * Check if a review is approaching SLA breach
 * Returns "breached", "high" (within 25% of SLA), "medium" (within 50%), or "low"
 *
 * @param createdAt - When the review was created
 * @param slaDeadline - The SLA deadline
 * @param now - Current time (defaults to now)
 */
export function getSlaRisk(createdAt: Date | string, slaDeadline: Date | string, now: Date | string = new Date()): "breached" | "high" | "medium" | "low" {
  const deadlineDate = slaDeadline instanceof Date ? slaDeadline : new Date(slaDeadline);
  const nowDate = now instanceof Date ? now : new Date(now);
  const createdDate = createdAt instanceof Date ? createdAt : new Date(createdAt);

  const timeRemaining = deadlineDate.getTime() - nowDate.getTime();

  if (timeRemaining <= 0) {
    return "breached";
  }

  // Calculate SLA duration from creation to deadline
  const slaDuration = deadlineDate.getTime() - createdDate.getTime();
  const percentRemaining = timeRemaining / slaDuration;

  if (percentRemaining <= 0.25) {
    return "high";
  } else if (percentRemaining <= 0.5) {
    return "medium";
  } else {
    return "low";
  }
}

/**
 * Validate that a task has the required properties for a review
 */
export function isReviewTask(props: NodeProperties): props is ReviewProperties {
  if (props.type !== "task") {
    return false;
  }

  const taskProps = props as TaskProperties;
  return taskProps.labels?.includes("review") ?? false;
}

/**
 * Create a review task properties object
 */
export function createReviewProperties(options: {
  goal: string;
  parentTaskId: string;
  labels: string[];
  priority: 0 | 1 | 2 | 3 | 4;
  assignee: string;
  deliverables: string[];
  reviewType?: "approval" | "feedback" | "inspection";
}): Omit<ReviewProperties, "id" | "createdAt"> {
  const now = new Date();
  const slaDeadline = calculateSlaDeadline(options.priority, now);

  // Ensure "review" label is included
  const labels = options.labels.includes("review")
    ? options.labels
    : ["review", ...options.labels];

  return {
    type: "task",
    state: "created",
    goal: options.goal,
    desiredDeliverables: options.deliverables,
    objectiveSuccessCriteria: [],
    knownInformation: [],
    informationGaps: [],
    toolsAvailable: [],
    labels,
    priority: options.priority,
    parentTaskId: options.parentTaskId,
    assignee: options.assignee,
    reviewType: options.reviewType ?? "approval",
    slaDeadline,
  };
}
