/**
 * Dashboard Module - Simple Direct Updates POC
 *
 * Demonstrates:
 * - Direct DOM updates (no full innerHTML replacement)
 * - Keyed list diffing for reviews/agents
 * - Granular WebSocket updates
 * - No external dependencies
 */

export class Dashboard {
  constructor(apiClient, wsClient) {
    this.api = apiClient;
    this.ws = wsClient;
    this.stats = null;
    this.reviews = [];
    this.agents = [];
    this.element = null;

    // Subscribe to WebSocket events
    this.ws.on("task_updated", (data) => this.handleTaskUpdate(data));
    this.ws.on("task_created", (data) => this.handleTaskCreated(data));
    this.ws.on("task_completed", (data) => this.handleTaskCompleted(data));
    this.ws.on("agent_started", (data) => this.handleAgentStarted(data));
    this.ws.on("agent_completed", (data) => this.handleAgentCompleted(data));
  }

  async mount(element) {
    this.element = element;

    // Initial render (only once!)
    this.renderInitial();

    // Load data
    await this.loadInitialData();

    // Set up event delegation
    this.element.addEventListener("click", (e) => this.handleClick(e));

    // No polling! WebSocket handles updates
  }

  unmount() {
    if (this.element) {
      this.element.removeEventListener("click", this.handleClick);
      this.element = null;
    }
  }

  async loadInitialData() {
    try {
      const [stats, reviews, agents] = await Promise.all([
        this.api.getStats(),
        this.api.getReviews(),
        this.api.getAgents(),
      ]);

      // Update each section independently
      this.updateStats(stats);
      this.updateReviews(reviews);
      this.updateAgents(agents);
    } catch (error) {
      console.error("Failed to load data:", error);
      this.showError(error.message);
    }
  }

  // ============================================================================
  // Initial Render (Only Called Once)
  // ============================================================================

  renderInitial() {
    if (!this.element) return;

    // Render static structure with containers
    this.element.innerHTML = `
      <div class="space-y-6">
        <!-- Stats Cards -->
        <div class="grid grid-cols-1 md:grid-cols-4 gap-4">
          <div class="bg-white overflow-hidden shadow rounded-lg">
            <div class="px-4 py-5 sm:p-6">
              <dt class="text-sm font-medium text-gray-500 truncate">Total Tasks</dt>
              <dd id="stat-total" class="mt-1 text-3xl font-semibold text-gray-900">-</dd>
            </div>
          </div>
          <div class="bg-white overflow-hidden shadow rounded-lg">
            <div class="px-4 py-5 sm:p-6">
              <dt class="text-sm font-medium text-gray-500 truncate">Active Agents</dt>
              <dd id="stat-agents" class="mt-1 text-3xl font-semibold text-blue-600">-</dd>
            </div>
          </div>
          <div class="bg-white overflow-hidden shadow rounded-lg">
            <div class="px-4 py-5 sm:p-6">
              <dt class="text-sm font-medium text-gray-500 truncate">Pending Reviews</dt>
              <dd id="stat-reviews" class="mt-1 text-3xl font-semibold text-orange-600">-</dd>
            </div>
          </div>
          <div class="bg-white overflow-hidden shadow rounded-lg">
            <div class="px-4 py-5 sm:p-6">
              <dt class="text-sm font-medium text-gray-500 truncate">Completed</dt>
              <dd id="stat-completed" class="mt-1 text-3xl font-semibold text-green-600">-</dd>
            </div>
          </div>
        </div>

        <!-- Reviews Section -->
        <div id="reviews-section" class="bg-white shadow rounded-lg" style="display: none;">
          <div class="px-4 py-5 sm:p-6">
            <h2 class="text-lg font-medium text-gray-900 mb-4">
              Pending Reviews (<span id="reviews-count">0</span>)
            </h2>
            <div id="reviews-list" class="space-y-3"></div>
          </div>
        </div>

        <!-- Agents Section -->
        <div id="agents-section" class="bg-white shadow rounded-lg" style="display: none;">
          <div class="px-4 py-5 sm:p-6">
            <h2 class="text-lg font-medium text-gray-900 mb-4">
              Active Agents (<span id="agents-count">0</span>)
            </h2>
            <div id="agents-list" class="space-y-3"></div>
          </div>
        </div>

        <!-- Error Display -->
        <div id="error-display" class="hidden bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded">
          <strong>Error:</strong> <span id="error-message"></span>
        </div>
      </div>
    `;
  }

  // ============================================================================
  // Granular Update Methods (Update Only What Changed)
  // ============================================================================

  updateStats(stats) {
    this.stats = stats;

    // Update just the numbers
    const totalEl = this.element.querySelector("#stat-total");
    const agentsEl = this.element.querySelector("#stat-agents");
    const reviewsEl = this.element.querySelector("#stat-reviews");
    const completedEl = this.element.querySelector("#stat-completed");

    if (totalEl) totalEl.textContent = stats.totalTasks || 0;
    if (agentsEl) agentsEl.textContent = stats.activeAgents || 0;
    if (reviewsEl) reviewsEl.textContent = stats.pendingReviews || 0;
    if (completedEl) completedEl.textContent = stats.tasksByState?.completed || 0;
  }

  updateReviews(reviews) {
    this.reviews = reviews;

    const section = this.element.querySelector("#reviews-section");
    const container = this.element.querySelector("#reviews-list");
    const countEl = this.element.querySelector("#reviews-count");

    if (!section || !container || !countEl) return;

    // Update count
    countEl.textContent = reviews.length;

    // Show/hide section
    section.style.display = reviews.length > 0 ? "block" : "none";

    if (reviews.length === 0) return;

    // Keyed list diffing
    this.updateList(
      container,
      reviews.slice(0, 5),
      (review) => review.id,
      (review) => this.createReviewElement(review),
      (el, review) => this.updateReviewElement(el, review)
    );
  }

  updateAgents(agents) {
    this.agents = agents;

    const section = this.element.querySelector("#agents-section");
    const container = this.element.querySelector("#agents-list");
    const countEl = this.element.querySelector("#agents-count");

    if (!section || !container || !countEl) return;

    // Update count
    countEl.textContent = agents.length;

    // Show/hide section
    section.style.display = agents.length > 0 ? "block" : "none";

    if (agents.length === 0) return;

    // Keyed list diffing
    this.updateList(
      container,
      agents,
      (agent) => agent.id,
      (agent) => this.createAgentElement(agent),
      (el, agent) => this.updateAgentElement(el, agent)
    );
  }

  // ============================================================================
  // Keyed List Diffing Utility (20 lines, handles all lists)
  // ============================================================================

  updateList(container, items, keyFn, createFn, updateFn) {
    // Build map of existing elements
    const existing = new Map();
    container.querySelectorAll("[data-key]").forEach((el) => {
      existing.set(el.dataset.key, el);
    });

    // Update or create elements
    const fragment = document.createDocumentFragment();

    items.forEach((item) => {
      const key = keyFn(item);
      let el = existing.get(key);

      if (el) {
        // Element exists - update it
        updateFn(el, item);
        existing.delete(key); // Mark as still in use
      } else {
        // New element - create it
        el = createFn(item);
        el.dataset.key = key;
      }

      fragment.appendChild(el);
    });

    // Remove elements that are no longer in the list
    existing.forEach((el) => el.remove());

    // Replace container contents
    container.innerHTML = "";
    container.appendChild(fragment);
  }

  // ============================================================================
  // Element Creation and Update
  // ============================================================================

  createReviewElement(review) {
    const div = document.createElement("div");
    div.className = "border border-gray-200 rounded-lg p-4 hover:bg-gray-50 task-card";
    div.dataset.key = review.id;

    this.updateReviewElement(div, review);
    return div;
  }

  updateReviewElement(el, review) {
    el.innerHTML = `
      <div class="flex items-start justify-between">
        <div class="flex-1">
          <div class="flex items-center gap-2">
            <span class="status-emoji">${this.getStatusEmoji(review.state)}</span>
            <span class="text-sm font-mono text-gray-500">${this.escapeHtml(review.id)}</span>
            ${
              review.priority !== undefined
                ? `
              <span class="priority-badge ${this.getPriorityClass(review.priority)}">
                P${review.priority}
              </span>
            `
                : ""
            }
          </div>
          <p class="mt-1 text-sm text-gray-900">${this.escapeHtml(review.goal)}</p>
        </div>
        <div class="flex gap-2 ml-4">
          <button
            data-action="approve"
            data-review-id="${review.id}"
            class="px-3 py-1 bg-green-600 text-white text-sm rounded hover:bg-green-700"
          >
            Approve
          </button>
          <button
            data-action="reject"
            data-review-id="${review.id}"
            class="px-3 py-1 bg-red-600 text-white text-sm rounded hover:bg-red-700"
          >
            Reject
          </button>
        </div>
      </div>
    `;
  }

  createAgentElement(agent) {
    const div = document.createElement("div");
    div.className = "border border-gray-200 rounded-lg p-4";
    div.dataset.key = agent.id;

    this.updateAgentElement(div, agent);
    return div;
  }

  updateAgentElement(el, agent) {
    el.innerHTML = `
      <div class="flex items-center gap-2">
        <span class="status-emoji">🔄</span>
        <span class="text-sm font-mono text-gray-500">${this.escapeHtml(agent.id)}</span>
      </div>
      <p class="mt-1 text-sm text-gray-900">${this.escapeHtml(agent.goal)}</p>
      ${
        agent.startedAt
          ? `
        <p class="mt-1 text-xs text-gray-500">
          Started: ${new Date(agent.startedAt).toLocaleString()}
        </p>
      `
          : ""
      }
    `;
  }

  // ============================================================================
  // WebSocket Event Handlers (Granular Updates)
  // ============================================================================

  handleTaskUpdate(data) {
    console.log("Task updated:", data);

    // Is it a review?
    const reviewIndex = this.reviews.findIndex((r) => r.id === data.id);
    if (reviewIndex !== -1) {
      this.reviews[reviewIndex] = { ...this.reviews[reviewIndex], ...data };
      this.updateReviews(this.reviews);
    }

    // Refresh stats (small fetch)
    this.api.getStats().then((stats) => this.updateStats(stats));
  }

  handleTaskCreated(data) {
    console.log("Task created:", data);

    // If it's a review, add to list
    if (data.labels?.includes("review")) {
      this.reviews.unshift(data);
      this.updateReviews(this.reviews);
    }

    // Update stats
    this.api.getStats().then((stats) => this.updateStats(stats));
  }

  handleTaskCompleted(data) {
    console.log("Task completed:", data);

    // Remove from reviews if present
    const reviewIndex = this.reviews.findIndex((r) => r.id === data.id);
    if (reviewIndex !== -1) {
      this.reviews.splice(reviewIndex, 1);
      this.updateReviews(this.reviews);
    }

    // Update stats
    this.api.getStats().then((stats) => this.updateStats(stats));
  }

  handleAgentStarted(data) {
    console.log("Agent started:", data);

    // Add to agents list
    this.agents.push(data);
    this.updateAgents(this.agents);

    // Update stats
    this.api.getStats().then((stats) => this.updateStats(stats));
  }

  handleAgentCompleted(data) {
    console.log("Agent completed:", data);

    // Remove from agents
    const agentIndex = this.agents.findIndex((a) => a.id === data.id);
    if (agentIndex !== -1) {
      this.agents.splice(agentIndex, 1);
      this.updateAgents(this.agents);
    }

    // Update stats
    this.api.getStats().then((stats) => this.updateStats(stats));
  }

  // ============================================================================
  // User Actions
  // ============================================================================

  handleClick(e) {
    const target = e.target.closest("[data-action]");
    if (!target) return;

    const action = target.dataset.action;
    const reviewId = target.dataset.reviewId;

    if (action === "approve") {
      this.handleApprove(reviewId);
    } else if (action === "reject") {
      this.handleReject(reviewId);
    }
  }

  async handleApprove(reviewId) {
    try {
      await this.api.approveReview(reviewId);
      // WebSocket will send task_completed event, which removes it
    } catch (error) {
      alert(`Failed to approve: ${error.message}`);
    }
  }

  async handleReject(reviewId) {
    const comment = prompt("Enter comment (reason for rejection):");
    if (!comment) return;

    try {
      await this.api.rejectReview(reviewId, comment);
      // WebSocket will send task_completed event, which removes it
    } catch (error) {
      alert(`Failed to reject: ${error.message}`);
    }
  }

  // ============================================================================
  // Utility Functions
  // ============================================================================

  showError(message) {
    const errorDisplay = this.element.querySelector("#error-display");
    const errorMessage = this.element.querySelector("#error-message");

    if (errorDisplay && errorMessage) {
      errorDisplay.classList.remove("hidden");
      errorMessage.textContent = message;

      // Auto-hide after 5 seconds
      setTimeout(() => {
        errorDisplay.classList.add("hidden");
      }, 5000);
    }
  }

  getStatusEmoji(state) {
    const emojis = {
      created: "⭕",
      ready: "🟡",
      active: "🔄",
      blocked: "🚫",
      completed: "✅",
      failed: "❌",
    };
    return emojis[state] || "❓";
  }

  getPriorityClass(priority) {
    if (priority === undefined) return "priority-p4";
    return `priority-p${priority}`;
  }

  escapeHtml(text) {
    const div = document.createElement("div");
    div.textContent = text;
    return div.innerHTML;
  }
}
