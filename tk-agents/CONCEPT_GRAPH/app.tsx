// Concept Graph Frontend Application
// Interactive graph visualization with D3.js

interface Concept {
  id: string;
  label: string;
  domains: string[];
  tags: string[];
  description: string;
  references: string[];
}

interface Relationship {
  from: string;
  to: string;
  type: string;
  description: string;
}

interface GraphNode extends Concept {
  x?: number;
  y?: number;
  fx?: number | null;
  fy?: number | null;
}

interface GraphLink extends Relationship {
  source: GraphNode | string;
  target: GraphNode | string;
}

// State
let concepts: GraphNode[] = [];
let relationships: GraphLink[] = [];
let selectedNodeId: string | null = null;
let searchQuery = "";

// D3 simulation
let simulation: any;
let svg: any;
let g: any;
let link: any;
let linkLabel: any;
let node: any;

// Domain color mapping
const domainColors: Record<string, string> = {
  "computer-science": "#3498db",
  mathematics: "#9b59b6",
  neuroscience: "#e74c3c",
  "cognitive-science": "#e67e22",
  "system-design": "#1abc9c",
  "software-engineering": "#16a085",
  "programming-languages": "#2980b9",
  "design-patterns": "#f39c12",
  "distributed-systems": "#27ae60",
  "runtime-systems": "#8e44ad",
  concurrency: "#c0392b",
  philosophy: "#d35400",
  history: "#7f8c8d",
  "knowledge-representation": "#2c3e50",
  ai: "#e91e63",
  biology: "#4caf50",
  "knowledge-management": "#607d8b",
  debugging: "#ff5722",
  architecture: "#009688",
  "business-logic": "#795548",
  "programming-paradigms": "#3f51b5",
  "operating-systems": "#00bcd4",
};

// Get color for concept based on primary domain
function getNodeColor(concept: Concept): string {
  const primaryDomain = concept.domains[0];
  return domainColors[primaryDomain] || "#95a5a6";
}

// Initialize application
async function init() {
  try {
    // Fetch data
    const [conceptsResponse, relationshipsResponse] = await Promise.all([
      fetch("/api/concepts"),
      fetch("/api/relationships"),
    ]);

    concepts = await conceptsResponse.json();
    const rawRelationships = await relationshipsResponse.json();

    // Create a set of valid concept IDs for fast lookup
    const conceptIds = new Set(concepts.map((c: Concept) => c.id));

    // Transform relationships from {from, to} to {source, target} for D3.js
    // Filter out relationships pointing to non-existent concepts
    relationships = rawRelationships
      .filter((r: any) => conceptIds.has(r.from) && conceptIds.has(r.to))
      .map((r: any) => ({
        ...r,
        source: r.from,
        target: r.to
      }));

    // Update stats
    document.getElementById("conceptCount")!.textContent =
      `${concepts.length} concepts, ${relationships.length} relationships`;

    // Initialize graph
    initGraph();

    // Setup event listeners
    setupEventListeners();

    // Create legend
    createLegend();
  } catch (error) {
    console.error("Failed to load data:", error);
    document.getElementById("graph")!.innerHTML =
      '<div class="loading">Error loading graph data</div>';
  }
}

// Initialize D3.js force-directed graph
function initGraph() {
  const container = document.getElementById("graph")!;
  const width = container.clientWidth;
  const height = container.clientHeight;

  // Create SVG
  svg = (window as any).d3
    .select("#graph")
    .append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [0, 0, width, height]);

  // Add zoom behavior
  const zoom = (window as any).d3
    .zoom()
    .scaleExtent([0.5, 3])
    .on("zoom", (event: any) => {
      g.attr("transform", event.transform);
    });

  svg.call(zoom);

  // Create container group for zoom/pan
  g = svg.append("g");

  // Create arrowhead markers
  svg
    .append("defs")
    .selectAll("marker")
    .data(["arrow", "arrow-highlighted"])
    .join("marker")
    .attr("id", (d: string) => d)
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 25)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .attr("class", (d: string) => d === "arrow-highlighted" ? "highlighted" : "")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5")
    .attr("fill", (d: string) => d === "arrow-highlighted" ? "#e74c3c" : "#999");

  // Create force simulation with boundary constraints
  const MARGIN = 50; // Keep nodes away from edges

  simulation = (window as any).d3
    .forceSimulation(concepts)
    .force(
      "link",
      (window as any).d3
        .forceLink(relationships)
        .id((d: any) => d.id)
        .distance(100)
    )
    .force("charge", (window as any).d3.forceManyBody().strength(-400))
    .force("center", (window as any).d3.forceCenter(width / 2, height / 2))
    .force("collision", (window as any).d3.forceCollide().radius(30))
    .force("x", (window as any).d3.forceX(width / 2).strength(0.05))
    .force("y", (window as any).d3.forceY(height / 2).strength(0.05));

  // Create links
  link = g
    .append("g")
    .attr("class", "links")
    .selectAll("path")
    .data(relationships)
    .join("path")
    .attr("class", "link")
    .attr("marker-end", "url(#arrow)");

  // Create link labels (initially hidden)
  linkLabel = g
    .append("g")
    .attr("class", "link-labels")
    .selectAll("text")
    .data(relationships)
    .join("text")
    .attr("class", "link-label")
    .attr("text-anchor", "middle")
    .text((d: Relationship) => d.type);

  // Create nodes
  const nodeGroup = g
    .append("g")
    .attr("class", "nodes")
    .selectAll("g")
    .data(concepts)
    .join("g")
    .attr("class", "node")
    .call(
      (window as any).d3
        .drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended)
    )
    .on("click", (event: any, d: Concept) => {
      event.stopPropagation();
      selectNode(d.id);
    });

  // Add circles to nodes
  nodeGroup
    .append("circle")
    .attr("r", 10)
    .attr("fill", (d: Concept) => getNodeColor(d));

  // Add labels to nodes
  nodeGroup
    .append("text")
    .attr("dy", "1.5em")
    .text((d: Concept) => d.label);

  node = nodeGroup;

  // Track simulation stability for testing
  let stabilityCheckCount = 0;
  const STABILITY_THRESHOLD = 0.05;
  const STABILITY_CHECKS_NEEDED = 5; // Consecutive ticks below threshold

  // Update positions on simulation tick
  simulation.on("tick", () => {
    // Constrain nodes within viewport boundaries
    concepts.forEach((d: any) => {
      d.x = Math.max(MARGIN, Math.min(width - MARGIN, d.x));
      d.y = Math.max(MARGIN, Math.min(height - MARGIN, d.y));
    });

    // Update visual positions
    link.attr("d", linkPath);

    linkLabel
      .attr("x", (d: any) => (d.source.x + d.target.x) / 2)
      .attr("y", (d: any) => (d.source.y + d.target.y) / 2);

    node.attr("transform", (d: any) => `translate(${d.x},${d.y})`);

    // Check if simulation has stabilized (for test automation)
    const currentAlpha = simulation.alpha();
    if (currentAlpha < STABILITY_THRESHOLD) {
      stabilityCheckCount++;
      if (stabilityCheckCount >= STABILITY_CHECKS_NEEDED) {
        document.body.setAttribute("data-simulation-stable", "true");
      }
    } else {
      stabilityCheckCount = 0;
      document.body.removeAttribute("data-simulation-stable");
    }
  });

  // Click on background to deselect
  svg.on("click", () => {
    if (selectedNodeId) {
      deselectNode();
    }
  });
}

// Create curved path for links
function linkPath(d: any) {
  const dx = d.target.x - d.source.x;
  const dy = d.target.y - d.source.y;
  const dr = Math.sqrt(dx * dx + dy * dy);
  return `M${d.source.x},${d.source.y}A${dr},${dr} 0 0,1 ${d.target.x},${d.target.y}`;
}

// Drag handlers
function dragstarted(event: any, d: any) {
  if (!event.active) {
    simulation.alphaTarget(0.3).restart();
    document.body.removeAttribute("data-simulation-stable");
  }
  d.fx = d.x;
  d.fy = d.y;
}

function dragged(event: any, d: any) {
  d.fx = event.x;
  d.fy = event.y;
}

function dragended(event: any, d: any) {
  if (!event.active) simulation.alphaTarget(0);
  d.fx = null;
  d.fy = null;
}

// Select node and show details
async function selectNode(nodeId: string) {
  selectedNodeId = nodeId;

  // Update visual selection
  node.classed("selected", (d: Concept) => d.id === nodeId);

  // Highlight connected links
  const connectedLinks = relationships.filter(
    (r) => r.from === nodeId || r.to === nodeId
  );

  link
    .classed("highlighted", (d: Relationship) =>
      connectedLinks.includes(d)
    )
    .attr("marker-end", (d: Relationship) =>
      connectedLinks.includes(d) ? "url(#arrow-highlighted)" : "url(#arrow)"
    );

  // Show connected link labels
  linkLabel.classed("visible", (d: Relationship) =>
    connectedLinks.includes(d)
  );

  // Fetch and display details
  try {
    const response = await fetch(`/api/concept/${nodeId}`);
    const data = await response.json();
    displayDetails(data.concept, data.relationships);
  } catch (error) {
    console.error("Failed to load concept details:", error);
  }
}

// Deselect node
function deselectNode() {
  selectedNodeId = null;

  // Remove visual selection
  node.classed("selected", false);
  link.classed("highlighted", false).attr("marker-end", "url(#arrow)");
  linkLabel.classed("visible", false);

  // Hide details panel
  document.getElementById("detailContent")!.style.display = "none";
  document.querySelector(".detail-empty")!.setAttribute("style", "display: flex");
}

// Display concept details in panel
function displayDetails(
  concept: Concept,
  relationships: {
    outgoing: Array<{ relationship: Relationship; concept: Concept }>;
    incoming: Array<{ relationship: Relationship; concept: Concept }>;
  }
) {
  const detailBody = document.getElementById("detailBody")!;

  let html = `
    <h2>${concept.label}</h2>
    <div class="concept-id">${concept.id}</div>

    <div class="section">
      <div class="section-title">Domains</div>
      <div class="domains">
        ${concept.domains.map((d) => `<span class="domain">${d}</span>`).join("")}
      </div>
    </div>

    <div class="section">
      <div class="section-title">Tags</div>
      <div class="tags">
        ${concept.tags.map((t) => `<span class="tag">${t}</span>`).join("")}
      </div>
    </div>

    <div class="section">
      <div class="section-title">Description</div>
      <div class="description">${concept.description}</div>
    </div>
  `;

  if (relationships.outgoing.length > 0) {
    html += `
      <div class="section">
        <div class="section-title">Outgoing Relationships (${relationships.outgoing.length})</div>
        <div class="relationships">
          ${relationships.outgoing
            .map(
              ({ relationship, concept: target }) => `
            <div class="relationship" data-concept-id="${target.id}">
              <div class="relationship-type">${relationship.type}</div>
              <div class="relationship-target">${target.label}</div>
              <div class="relationship-desc">${relationship.description}</div>
            </div>
          `
            )
            .join("")}
        </div>
      </div>
    `;
  }

  if (relationships.incoming.length > 0) {
    html += `
      <div class="section">
        <div class="section-title">Incoming Relationships (${relationships.incoming.length})</div>
        <div class="relationships">
          ${relationships.incoming
            .map(
              ({ relationship, concept: source }) => `
            <div class="relationship incoming" data-concept-id="${source.id}">
              <div class="relationship-type">${relationship.type}</div>
              <div class="relationship-target">${source.label}</div>
              <div class="relationship-desc">${relationship.description}</div>
            </div>
          `
            )
            .join("")}
        </div>
      </div>
    `;
  }

  detailBody.innerHTML = html;

  // Show detail panel
  document.querySelector(".detail-empty")!.setAttribute("style", "display: none");
  document.getElementById("detailContent")!.style.display = "block";

  // Add click handlers to relationship cards
  detailBody.querySelectorAll(".relationship").forEach((el) => {
    el.addEventListener("click", () => {
      const conceptId = el.getAttribute("data-concept-id");
      if (conceptId) {
        selectNode(conceptId);
      }
    });
  });
}

// Search and filter concepts
function performSearch(query: string) {
  searchQuery = query.toLowerCase();

  if (!searchQuery) {
    // Reset to show all
    node.classed("dimmed", false);
    link.classed("dimmed", false);
    return;
  }

  // Filter nodes
  node.classed("dimmed", (d: Concept) => {
    const matches =
      d.label.toLowerCase().includes(searchQuery) ||
      d.description.toLowerCase().includes(searchQuery) ||
      d.tags.some((t) => t.toLowerCase().includes(searchQuery)) ||
      d.domains.some((domain) => domain.toLowerCase().includes(searchQuery));
    return !matches;
  });

  // Dim links that don't connect to visible nodes
  link.classed("dimmed", (d: any) => {
    const sourceMatches =
      d.source.label.toLowerCase().includes(searchQuery) ||
      d.source.description.toLowerCase().includes(searchQuery);
    const targetMatches =
      d.target.label.toLowerCase().includes(searchQuery) ||
      d.target.description.toLowerCase().includes(searchQuery);
    return !(sourceMatches || targetMatches);
  });
}

// Create domain legend
function createLegend() {
  const legendContent = document.getElementById("legendContent")!;

  // Get unique domains
  const domainSet = new Set<string>();
  concepts.forEach((c) => c.domains.forEach((d) => domainSet.add(d)));
  const domains = Array.from(domainSet).sort();

  // Create legend items
  domains.forEach((domain) => {
    const color = domainColors[domain] || "#95a5a6";
    const item = document.createElement("div");
    item.className = "legend-item";
    item.innerHTML = `
      <div class="legend-color" style="background-color: ${color}"></div>
      <span>${domain}</span>
    `;
    legendContent.appendChild(item);
  });
}

// Setup event listeners
function setupEventListeners() {
  const searchInput = document.getElementById("searchInput") as HTMLInputElement;
  const clearButton = document.getElementById("clearSearch")!;
  const closeButton = document.getElementById("closeDetail")!;

  // Search input
  searchInput.addEventListener("input", (e) => {
    const target = e.target as HTMLInputElement;
    performSearch(target.value);
  });

  // Clear search
  clearButton.addEventListener("click", () => {
    searchInput.value = "";
    performSearch("");
  });

  // Close detail panel
  closeButton.addEventListener("click", () => {
    deselectNode();
  });

  // Window resize
  let resizeTimer: number;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => {
      const container = document.getElementById("graph")!;
      const width = container.clientWidth;
      const height = container.clientHeight;
      svg.attr("width", width).attr("height", height);
      simulation
        .force("center", (window as any).d3.forceCenter(width / 2, height / 2))
        .restart();
    }, 250);
  });
}

// Start application
init();
