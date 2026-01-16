# Concept Graph Web Server - Specification

## Overview

The Concept Graph Web Server is a Bun.serve() HTTP server that provides an interactive graph visualization of interdisciplinary concepts. It serves a React frontend with D3.js visualization and provides RESTful API endpoints for querying concepts and relationships.

## System Context

```
┌─────────────┐      HTTP        ┌──────────────────┐
│   Browser   │ ◄────────────►  │   Bun.serve()    │
│   Client    │                  │   Web Server     │
└─────────────┘                  └──────────────────┘
                                          │
                                          ▼
                                 ┌──────────────────┐
                                 │  search.ts       │
                                 │  (Query Layer)   │
                                 └──────────────────┘
                                          │
                                          ▼
                                 ┌──────────────────┐
                                 │  concepts.json   │
                                 │  relationships.json│
                                 └──────────────────┘
```

## Data Model

### Concept
```typescript
interface Concept {
  id: string;              // Kebab-case identifier (e.g., "actor-model")
  label: string;           // Human-readable name (e.g., "Actor Model")
  domains: string[];       // Category domains (e.g., ["computer-science", "concurrency"])
  tags: string[];          // Searchable tags
  description: string;     // Detailed description
  references: string[];    // Related concept IDs
}
```

### Relationship
```typescript
interface Relationship {
  from: string;            // Source concept ID
  to: string;              // Target concept ID
  type: string;            // Relationship type (e.g., "implements", "uses", "inspires")
  description: string;     // Relationship description
}
```

### Statistics
```typescript
interface Statistics {
  totalConcepts: number;
  totalRelationships: number;
  domains: number;
  tags: number;
  relationshipTypes: number;
}
```

### Concept Details Response
```typescript
interface ConceptDetailsResponse {
  concept: Concept;
  relationships: {
    outgoing: Array<{
      relationship: Relationship;
      concept: Concept;
    }>;
    incoming: Array<{
      relationship: Relationship;
      concept: Concept;
    }>;
  };
}
```

## API Endpoints

### 1. GET /api/concepts

**Purpose**: Retrieve all concepts in the graph.

**Request**:
- Method: GET
- Headers: None required
- Body: None

**Response**:
- Status: 200 OK
- Content-Type: application/json
- Body: Array of Concept objects

**Example**:
```bash
curl http://localhost:3000/api/concepts
```

**Response**:
```json
[
  {
    "id": "actor-model",
    "label": "Actor Model",
    "domains": ["computer-science", "concurrency"],
    "tags": ["concurrency", "message-passing", "isolation", "distributed-systems"],
    "description": "A computational model where actors are the fundamental units...",
    "references": ["erlang-otp", "message-passing", "supervision-tree"]
  },
  ...
]
```

**Errors**: None (always returns array, empty if no data)

---

### 2. GET /api/relationships

**Purpose**: Retrieve all relationships between concepts.

**Request**:
- Method: GET
- Headers: None required
- Body: None

**Response**:
- Status: 200 OK
- Content-Type: application/json
- Body: Array of Relationship objects

**Example**:
```bash
curl http://localhost:3000/api/relationships
```

**Response**:
```json
[
  {
    "from": "actor-model",
    "to": "erlang-otp",
    "type": "implemented-in",
    "description": "Erlang/OTP provides a battle-tested implementation"
  },
  ...
]
```

**Errors**: None (always returns array, empty if no data)

---

### 3. GET /api/stats

**Purpose**: Retrieve graph statistics.

**Request**:
- Method: GET
- Headers: None required
- Body: None

**Response**:
- Status: 200 OK
- Content-Type: application/json
- Body: Statistics object

**Example**:
```bash
curl http://localhost:3000/api/stats
```

**Response**:
```json
{
  "totalConcepts": 50,
  "totalRelationships": 61,
  "domains": 15,
  "tags": 87,
  "relationshipTypes": 12
}
```

**Errors**: None (always computes from loaded data)

---

### 4. GET /api/concept/:id

**Purpose**: Retrieve detailed information about a specific concept, including all related concepts.

**Request**:
- Method: GET
- URL Parameters:
  - `id` (required): Concept identifier (string)
- Headers: None required
- Body: None

**Response (Success)**:
- Status: 200 OK
- Content-Type: application/json
- Body: ConceptDetailsResponse object

**Example**:
```bash
curl http://localhost:3000/api/concept/actor-model
```

**Response**:
```json
{
  "concept": {
    "id": "actor-model",
    "label": "Actor Model",
    "domains": ["computer-science", "concurrency"],
    "tags": ["concurrency", "message-passing"],
    "description": "A computational model...",
    "references": ["erlang-otp"]
  },
  "relationships": {
    "outgoing": [
      {
        "relationship": {
          "from": "actor-model",
          "to": "erlang-otp",
          "type": "implemented-in",
          "description": "..."
        },
        "concept": {
          "id": "erlang-otp",
          "label": "Erlang/OTP",
          ...
        }
      }
    ],
    "incoming": [
      {
        "relationship": {
          "from": "supervision-tree",
          "to": "actor-model",
          "type": "uses",
          "description": "..."
        },
        "concept": {
          "id": "supervision-tree",
          "label": "Supervision Tree",
          ...
        }
      }
    ]
  }
}
```

**Response (Error - Missing ID)**:
- Status: 400 Bad Request
- Body: "Missing concept ID"

**Response (Error - Not Found)**:
- Status: 404 Not Found
- Body: "Concept not found"

**Example Error**:
```bash
curl http://localhost:3000/api/concept/nonexistent-id
# Returns: 404 "Concept not found"
```

---

### 5. GET /api/search

**Purpose**: Search concepts by keyword, domain, or tag.

**Request**:
- Method: GET
- Query Parameters (at least one required):
  - `q` (optional): Keyword to search in label and description
  - `domain` (optional): Filter by domain
  - `tag` (optional): Filter by tag
- Headers: None required
- Body: None

**Response (Success)**:
- Status: 200 OK
- Content-Type: application/json
- Body: Array of Concept objects matching search criteria

**Example - Keyword Search**:
```bash
curl "http://localhost:3000/api/search?q=actor"
```

**Example - Domain Filter**:
```bash
curl "http://localhost:3000/api/search?domain=computer-science"
```

**Example - Tag Filter**:
```bash
curl "http://localhost:3000/api/search?tag=concurrency"
```

**Example - Combined Search**:
```bash
curl "http://localhost:3000/api/search?q=state&domain=computer-science"
```

**Response**:
```json
[
  {
    "id": "actor-model",
    "label": "Actor Model",
    ...
  },
  ...
]
```

**Response (Error - No Parameters)**:
- Status: 400 Bad Request
- Body: "Missing search parameters"

**Example Error**:
```bash
curl "http://localhost:3000/api/search"
# Returns: 400 "Missing search parameters"
```

**Search Behavior**:
- Keyword search is case-insensitive
- Keyword matches against both `label` and `description` fields
- Multiple criteria are combined with AND logic
- Results are returned in original array order
- Empty results return `[]` (not an error)

---

### 6. GET /

**Purpose**: Serve the main HTML application.

**Request**:
- Method: GET
- Headers: None required
- Body: None

**Response**:
- Status: 200 OK
- Content-Type: text/html
- Body: HTML document with embedded React/D3.js application

**Behavior**:
- Serves `index.html` as entry point
- Bun automatically bundles and transpiles imported `.tsx` and `.css` files
- Hot Module Reload (HMR) enabled in development mode

---

## Server Configuration

### Environment Variables
- `PORT` (optional): Server port (default: 0 for random assignment)

### Development Features
- **Hot Module Reload (HMR)**: Enabled via `development.hmr: true`
- **Console Logging**: Development console output via `development.console: true`
- **Random Port Assignment**: Uses port 0 by default for automatic port selection

### Error Handling
All unhandled server errors return:
- Status: 500 Internal Server Error
- Body: "Internal Server Error"

### Server Lifecycle
1. Load concepts.json and relationships.json via search.ts module
2. Initialize indexes (by ID, domain, tag, edges)
3. Start Bun.serve() on configured port
4. Log server URL and available endpoints to console
5. Accept HTTP requests until Ctrl+C

---

## Fixture Tables (Test Scenarios)

### API Endpoint Fixture: GET /api/concepts

| Test Case | Expected Status | Expected Body Type | Expected Count |
|-----------|----------------|-------------------|----------------|
| GET /api/concepts | 200 | Array | 50 |

### API Endpoint Fixture: GET /api/relationships

| Test Case | Expected Status | Expected Body Type | Expected Count |
|-----------|----------------|-------------------|----------------|
| GET /api/relationships | 200 | Array | 61 |

### API Endpoint Fixture: GET /api/stats

| Test Case | Expected Status | Expected Keys |
|-----------|----------------|--------------|
| GET /api/stats | 200 | totalConcepts, totalRelationships, domains, tags, relationshipTypes |

### API Endpoint Fixture: GET /api/concept/:id

| Test Case | Concept ID | Expected Status | Expected Behavior |
|-----------|-----------|----------------|------------------|
| Valid concept | actor-model | 200 | Returns concept + relationships |
| Invalid concept | nonexistent-id | 404 | Returns "Concept not found" |
| Missing ID | (empty) | 400 | Returns "Missing concept ID" |

### API Endpoint Fixture: GET /api/search

| Test Case | Query Params | Expected Status | Expected Behavior |
|-----------|-------------|----------------|------------------|
| Keyword search | q=actor | 200 | Returns matching concepts |
| Domain filter | domain=computer-science | 200 | Returns concepts in domain |
| Tag filter | tag=concurrency | 200 | Returns concepts with tag |
| Combined search | q=state&domain=computer-science | 200 | Returns concepts matching all |
| No parameters | (none) | 400 | Returns "Missing search parameters" |
| Empty results | q=zzzznonexistent | 200 | Returns [] |

### Browser Interaction Fixtures

| Test Case | User Action | Expected Behavior |
|-----------|------------|------------------|
| Page load | Navigate to / | Graph renders with 50 nodes, 61 edges |
| Node click | Click on actor-model node | Detail panel opens, shows concept info |
| Detail panel | Node selected | Shows label, domains, tags, description, relationships |
| Relationship click | Click related concept in detail panel | Navigates to that concept |
| Search filter | Type "actor" in search box | Dims non-matching nodes |
| Clear search | Click clear button (✕) | All nodes visible again |
| Close detail | Click close button (✕) in detail panel | Detail panel closes, node deselected |
| Background click | Click graph background | Deselects node, closes detail panel |
| Drag node | Drag a node | Node position changes |
| Zoom | Mouse wheel scroll | Graph zooms in/out |

### Edge Case Fixtures

| Test Case | Scenario | Expected Behavior |
|-----------|----------|------------------|
| Invalid route | GET /api/invalid | Returns 404 (Bun default behavior) |
| Malformed JSON data | concepts.json syntax error | Server startup fails with error |
| Missing concept in relationship | Relationship points to nonexistent ID | Frontend filters out invalid relationship |
| Concurrent requests | 10 simultaneous GET /api/concepts | All return 200 with same data |
| Large search results | Search returns all 50 concepts | Response completes without timeout |
| Special characters in ID | GET /api/concept/id-with-special!@# | URL encoding handled correctly |

---

## Non-Functional Requirements

### Performance
- API endpoints should respond within 50ms (in-memory data)
- Page load should complete within 2s on localhost
- Graph rendering should complete within 1s for 50 nodes

### Reliability
- Server should handle at least 100 concurrent connections
- No memory leaks during HMR cycles
- Graceful error handling for all API endpoints

### Security
- No authentication required (local development tool)
- No XSS vulnerabilities in rendered HTML
- Safe JSON serialization (no prototype pollution)

### Usability
- Clear console output with server URL and examples
- Helpful error messages for API errors
- Responsive UI (works on desktop browsers)

---

## Dependencies

### Runtime Dependencies
- **Bun**: JavaScript runtime and server framework
- **search.ts**: Query layer for concepts and relationships
- **concepts.json**: 50 concept definitions
- **relationships.json**: 61 relationship definitions

### Frontend Dependencies
- **D3.js v7**: Graph visualization (loaded from CDN)
- **React**: UI framework (implicit via .tsx)
- **style.css**: Styling

### Development Dependencies
- Bun's built-in bundler and transpiler
- Bun's HMR system

---

## Implementation Notes

### @implements Annotations
```typescript
// server.ts
// @implements server.spec.md#API-Endpoints
// @implements server.spec.md#Server-Configuration
// @implements server.spec.md#Error-Handling

// Example endpoint implementation
// @implements server.spec.md#GET-/api/concepts
"/api/concepts": {
  GET: () => Response.json(concepts)
}
```

### Design Decisions
1. **Port 0 for random assignment**: Avoids port conflicts in development
2. **In-memory indexes**: Fast lookups without database overhead
3. **RESTful API design**: Standard HTTP semantics for web clients
4. **Bun.serve() routes**: Native routing without Express.js overhead
5. **HMR enabled**: Fast development iteration

### Testability
- All API endpoints are pure functions of request → response
- No external dependencies beyond JSON files
- Server lifecycle is deterministic
- Frontend can be tested via browser automation

---

## Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-16 | Background Agent | Initial specification |
