#!/usr/bin/env bun
// Concept Graph Web Server
// Serves interactive graph visualization with Bun.serve()

import index from "./index.html";
import {
  concepts,
  relationships,
  getConcept,
  search,
  getRelatedConcepts,
  getStats,
  type SearchOptions,
} from "./search";

const PORT = process.env.PORT ? parseInt(process.env.PORT) : 0; // Use port 0 for random port

// API route handlers
const apiRoutes = {
  // Get all concepts
  "/api/concepts": {
    GET: () => {
      return Response.json(concepts);
    },
  },

  // Get all relationships
  "/api/relationships": {
    GET: () => {
      return Response.json(relationships);
    },
  },

  // Get graph statistics
  "/api/stats": {
    GET: () => {
      const stats = getStats();
      return Response.json(stats);
    },
  },

  // Get specific concept by ID
  "/api/concept/:id": {
    GET: (req: Request) => {
      const url = new URL(req.url);
      const id = url.pathname.split("/").pop();

      if (!id) {
        return new Response("Missing concept ID", { status: 400 });
      }

      const concept = getConcept(id);
      if (!concept) {
        return new Response("Concept not found", { status: 404 });
      }

      const related = getRelatedConcepts(id);

      return Response.json({
        concept,
        relationships: related,
      });
    },
  },

  // Search concepts
  "/api/search": {
    GET: (req: Request) => {
      const url = new URL(req.url);
      const query = url.searchParams.get("q");
      const domain = url.searchParams.get("domain");
      const tag = url.searchParams.get("tag");

      if (!query && !domain && !tag) {
        return new Response("Missing search parameters", { status: 400 });
      }

      const options: SearchOptions = {
        keyword: query || undefined,
        domain: domain || undefined,
        tag: tag || undefined,
      };

      const results = search(options);
      return Response.json(results);
    },
  },
};

// Create server
const server = Bun.serve({
  port: PORT,
  routes: {
    "/": index,
    "/app.tsx": Bun.file("./app.tsx"),
    "/style.css": Bun.file("./style.css"),
    ...apiRoutes,
  },
  development: {
    hmr: true, // Hot module reload
    console: true, // Show console logs
  },
  error(error) {
    console.error("Server error:", error);
    return new Response("Internal Server Error", { status: 500 });
  },
});

// Get the actual port (important when using port 0 for random assignment)
const actualPort = server.port;
const url = `http://localhost:${actualPort}`;

console.log(`
┌────────────────────────────────────────────────────────────┐
│                                                            │
│  Concept Graph Web Server                                 │
│                                                            │
│  🌐 Server running at: ${url.padEnd(38)}│
│                                                            │
│  📊 Loaded: ${concepts.length} concepts, ${relationships.length} relationships       │
│                                                            │
│  🔧 Hot Module Reload: Enabled                            │
│  📝 Development Mode: Active                              │
│                                                            │
│  Press Ctrl+C to stop                                     │
│                                                            │
└────────────────────────────────────────────────────────────┘

🔗 OPEN THIS URL: ${url}

Available API endpoints:
  GET  /api/concepts          - List all concepts
  GET  /api/relationships     - List all relationships
  GET  /api/stats             - Graph statistics
  GET  /api/concept/:id       - Get concept details
  GET  /api/search?q=...      - Search concepts

Examples:
  curl ${url}/api/concepts
  curl ${url}/api/concept/actor-model
  curl ${url}/api/search?q=actor
`);
