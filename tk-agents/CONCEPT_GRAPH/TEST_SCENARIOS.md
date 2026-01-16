# Concept Graph Web Server - Test Scenarios

This document defines comprehensive test scenarios for the Concept Graph Web Server across three categories:
1. API Endpoint Tests (backend)
2. Browser Interaction Tests (frontend E2E)
3. Edge Cases and Error Handling

## Category 1: API Endpoint Test Scenarios

### Scenario 1.1: GET /api/concepts - Retrieve All Concepts

**Objective**: Verify that all concepts are returned correctly.

**Preconditions**: Server is running with loaded concepts.json

**Test Steps**:
1. Send GET request to `/api/concepts`
2. Verify response status is 200
3. Verify response is JSON array
4. Verify array contains exactly 50 concepts
5. Verify each concept has required fields (id, label, domains, tags, description, references)

**Expected Results**:
```json
Status: 200
Body: [
  {
    "id": "actor-model",
    "label": "Actor Model",
    "domains": ["computer-science", "concurrency"],
    "tags": ["concurrency", "message-passing", ...],
    "description": "...",
    "references": [...]
  },
  ...
]
Array length: 50
```

**Validation Points**:
- ✅ Status code is 200
- ✅ Content-Type is application/json
- ✅ Response is array with 50 elements
- ✅ All concepts have non-empty id and label
- ✅ All concepts have at least one domain

---

### Scenario 1.2: GET /api/relationships - Retrieve All Relationships

**Objective**: Verify that all relationships are returned correctly.

**Preconditions**: Server is running with loaded relationships.json

**Test Steps**:
1. Send GET request to `/api/relationships`
2. Verify response status is 200
3. Verify response is JSON array
4. Verify array contains exactly 61 relationships
5. Verify each relationship has required fields (from, to, type, description)

**Expected Results**:
```json
Status: 200
Body: [
  {
    "from": "actor-model",
    "to": "erlang-otp",
    "type": "implemented-in",
    "description": "..."
  },
  ...
]
Array length: 61
```

**Validation Points**:
- ✅ Status code is 200
- ✅ Content-Type is application/json
- ✅ Response is array with 61 elements
- ✅ All relationships have valid from/to concept IDs
- ✅ No self-referencing relationships (from === to)

---

### Scenario 1.3: GET /api/stats - Retrieve Graph Statistics

**Objective**: Verify that graph statistics are computed correctly.

**Preconditions**: Server is running with loaded data

**Test Steps**:
1. Send GET request to `/api/stats`
2. Verify response status is 200
3. Verify response contains all statistic fields
4. Verify values match expected counts

**Expected Results**:
```json
Status: 200
Body: {
  "totalConcepts": 50,
  "totalRelationships": 61,
  "domains": 15,
  "tags": 87,
  "relationshipTypes": 12
}
```

**Validation Points**:
- ✅ Status code is 200
- ✅ All fields are present (totalConcepts, totalRelationships, domains, tags, relationshipTypes)
- ✅ totalConcepts equals 50
- ✅ totalRelationships equals 61
- ✅ All values are positive integers

---

### Scenario 1.4: GET /api/concept/:id - Valid Concept ID

**Objective**: Verify retrieval of concept details with relationships.

**Preconditions**: Server is running, concept "actor-model" exists

**Test Steps**:
1. Send GET request to `/api/concept/actor-model`
2. Verify response status is 200
3. Verify response contains concept and relationships
4. Verify concept.id matches requested ID
5. Verify relationships are grouped into outgoing/incoming
6. Verify related concepts are populated (not just IDs)

**Expected Results**:
```json
Status: 200
Body: {
  "concept": {
    "id": "actor-model",
    "label": "Actor Model",
    ...
  },
  "relationships": {
    "outgoing": [
      {
        "relationship": {...},
        "concept": {...}
      }
    ],
    "incoming": [...]
  }
}
```

**Validation Points**:
- ✅ Status code is 200
- ✅ concept.id === "actor-model"
- ✅ relationships object has outgoing and incoming arrays
- ✅ Each relationship item has both relationship and concept fields
- ✅ Concept objects in relationships are fully populated

---

### Scenario 1.5: GET /api/concept/:id - Invalid Concept ID

**Objective**: Verify 404 error for non-existent concept.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/concept/nonexistent-id-12345`
2. Verify response status is 404
3. Verify response body contains error message

**Expected Results**:
```
Status: 404
Body: "Concept not found"
```

**Validation Points**:
- ✅ Status code is 404
- ✅ Response body is "Concept not found"

---

### Scenario 1.6: GET /api/concept/:id - Missing Concept ID

**Objective**: Verify 400 error for missing ID parameter.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/concept/` (trailing slash, no ID)
2. Verify response status is 400
3. Verify response body contains error message

**Expected Results**:
```
Status: 400
Body: "Missing concept ID"
```

**Validation Points**:
- ✅ Status code is 400
- ✅ Response body is "Missing concept ID"

---

### Scenario 1.7: GET /api/search - Keyword Search

**Objective**: Verify keyword search filters concepts correctly.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/search?q=actor`
2. Verify response status is 200
3. Verify all returned concepts match keyword
4. Verify matching is case-insensitive
5. Verify search matches both label and description

**Expected Results**:
```json
Status: 200
Body: [
  {
    "id": "actor-model",
    "label": "Actor Model",
    ...
  },
  ...
]
```

**Validation Points**:
- ✅ Status code is 200
- ✅ All returned concepts have "actor" in label or description (case-insensitive)
- ✅ Search is case-insensitive (finds "Actor" with query "actor")
- ✅ Empty results return [] (not error)

---

### Scenario 1.8: GET /api/search - Domain Filter

**Objective**: Verify domain filtering works correctly.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/search?domain=computer-science`
2. Verify response status is 200
3. Verify all returned concepts have "computer-science" in domains array

**Expected Results**:
```json
Status: 200
Body: [
  {
    "id": "actor-model",
    "domains": ["computer-science", "concurrency"],
    ...
  },
  ...
]
```

**Validation Points**:
- ✅ Status code is 200
- ✅ All returned concepts include "computer-science" in domains
- ✅ Concepts with only other domains are excluded

---

### Scenario 1.9: GET /api/search - Tag Filter

**Objective**: Verify tag filtering works correctly.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/search?tag=concurrency`
2. Verify response status is 200
3. Verify all returned concepts have "concurrency" in tags array

**Expected Results**:
```json
Status: 200
Body: [
  {
    "id": "actor-model",
    "tags": ["concurrency", "message-passing", ...],
    ...
  },
  ...
]
```

**Validation Points**:
- ✅ Status code is 200
- ✅ All returned concepts include "concurrency" in tags
- ✅ Concepts with only other tags are excluded

---

### Scenario 1.10: GET /api/search - Combined Filters

**Objective**: Verify multiple search criteria use AND logic.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/search?q=state&domain=computer-science`
2. Verify response status is 200
3. Verify all returned concepts match ALL criteria

**Expected Results**:
```json
Status: 200
Body: [concepts matching both "state" keyword AND "computer-science" domain]
```

**Validation Points**:
- ✅ Status code is 200
- ✅ All concepts have "state" in label/description
- ✅ All concepts have "computer-science" in domains
- ✅ Uses AND logic (not OR)

---

### Scenario 1.11: GET /api/search - No Parameters

**Objective**: Verify 400 error when no search parameters provided.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/search` (no query parameters)
2. Verify response status is 400
3. Verify response body contains error message

**Expected Results**:
```
Status: 400
Body: "Missing search parameters"
```

**Validation Points**:
- ✅ Status code is 400
- ✅ Response body is "Missing search parameters"

---

### Scenario 1.12: GET /api/search - Empty Results

**Objective**: Verify empty results return empty array (not error).

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/search?q=zzzznonexistent12345`
2. Verify response status is 200
3. Verify response body is empty array

**Expected Results**:
```json
Status: 200
Body: []
```

**Validation Points**:
- ✅ Status code is 200 (not 404)
- ✅ Response body is empty array []

---

### Scenario 1.13: GET / - Serve HTML Application

**Objective**: Verify main page serves HTML content.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/`
2. Verify response status is 200
3. Verify Content-Type is text/html
4. Verify response contains HTML document

**Expected Results**:
```
Status: 200
Content-Type: text/html
Body: <!DOCTYPE html>...
```

**Validation Points**:
- ✅ Status code is 200
- ✅ Content-Type includes "text/html"
- ✅ Response body is valid HTML
- ✅ HTML includes script tag for app.tsx

---

## Category 2: Browser Interaction Test Scenarios

### Scenario 2.1: Page Load and Initial Render

**Objective**: Verify the graph loads and renders correctly on page load.

**Preconditions**: Server is running

**Test Steps**:
1. Navigate to server URL in browser
2. Wait for page load complete
3. Verify header displays "Concept Graph Explorer"
4. Verify stats show "50 concepts, 61 relationships"
5. Verify SVG graph is rendered
6. Count visible nodes and edges
7. Verify legend shows all domains

**Expected Results**:
- Page loads without errors
- Header text visible
- Stats display correct counts
- Graph renders with 50 nodes (circles)
- Graph renders with 61 edges (paths)
- Legend shows ~15 domain colors

**Validation Points**:
- ✅ No JavaScript errors in console
- ✅ Header text is "Concept Graph Explorer"
- ✅ Stats show "50 concepts, 61 relationships"
- ✅ SVG element exists with class "graph"
- ✅ 50 circle elements (nodes) are rendered
- ✅ 61 path elements (edges) are rendered
- ✅ Legend contains domain color swatches

---

### Scenario 2.2: Click Node to Show Details

**Objective**: Verify clicking a node displays concept details in side panel.

**Preconditions**: Page is loaded, graph is rendered

**Test Steps**:
1. Locate a node (e.g., "Actor Model")
2. Click the node
3. Verify detail panel opens
4. Verify concept name is displayed
5. Verify domains, tags, description are shown
6. Verify relationships section is populated

**Expected Results**:
- Detail panel becomes visible (slides in from right)
- Concept label "Actor Model" displayed as heading
- Domains shown as colored badges
- Tags shown as small badges
- Description text visible
- Outgoing and incoming relationships listed

**Validation Points**:
- ✅ Detail panel has style "display: block"
- ✅ Concept heading matches clicked node
- ✅ Domains section contains at least one domain badge
- ✅ Tags section contains at least one tag
- ✅ Description text is non-empty
- ✅ Relationships section shows outgoing and/or incoming relationships

---

### Scenario 2.3: Visual Node Selection Highlighting

**Objective**: Verify selected node and connected edges are highlighted.

**Preconditions**: Page is loaded, graph is rendered

**Test Steps**:
1. Click a node (e.g., "Actor Model")
2. Verify node receives "selected" class
3. Verify connected edges receive "highlighted" class
4. Verify edge labels become visible for connected edges
5. Verify non-connected edges remain dimmed

**Expected Results**:
- Clicked node has class "selected"
- Connected edges have class "highlighted" and use red arrow marker
- Edge labels for connected relationships become visible
- Unrelated edges remain gray

**Validation Points**:
- ✅ Selected node has class "selected"
- ✅ Connected edges have class "highlighted"
- ✅ Connected edges use "arrow-highlighted" marker
- ✅ Edge labels (relationship types) visible for connected edges
- ✅ Unconnected edges remain with default styling

---

### Scenario 2.4: Click Related Concept to Navigate

**Objective**: Verify clicking a relationship in detail panel navigates to that concept.

**Preconditions**: Detail panel is open for a concept with relationships

**Test Steps**:
1. Open detail panel for "Actor Model"
2. Locate an outgoing relationship (e.g., to "Erlang/OTP")
3. Click the relationship card
4. Verify detail panel updates to show "Erlang/OTP"
5. Verify graph highlights change to new selection

**Expected Results**:
- Detail panel content updates to "Erlang/OTP"
- Graph selection changes to "Erlang/OTP" node
- Connected edges for "Erlang/OTP" are highlighted
- Previous selection is cleared

**Validation Points**:
- ✅ Detail panel heading changes to "Erlang/OTP"
- ✅ "Erlang/OTP" node has class "selected"
- ✅ "Actor Model" node no longer has class "selected"
- ✅ Highlighted edges updated to "Erlang/OTP" connections

---

### Scenario 2.5: Search Filters Nodes

**Objective**: Verify search input filters graph nodes correctly.

**Preconditions**: Page is loaded, graph is rendered

**Test Steps**:
1. Locate search input box
2. Type "actor" into search box
3. Verify non-matching nodes are dimmed
4. Verify matching nodes remain visible
5. Verify connected edges to matching nodes remain visible

**Expected Results**:
- Nodes matching "actor" remain fully visible
- Non-matching nodes receive "dimmed" class (opacity reduced)
- Edges connecting to visible nodes remain visible
- Edges between dimmed nodes are also dimmed

**Validation Points**:
- ✅ Nodes with "actor" in label/description/tags do NOT have "dimmed" class
- ✅ Nodes without "actor" have "dimmed" class
- ✅ Search is case-insensitive
- ✅ Search updates in real-time as typing

---

### Scenario 2.6: Clear Search Restores All Nodes

**Objective**: Verify clearing search restores full graph visibility.

**Preconditions**: Search filter is active (some nodes dimmed)

**Test Steps**:
1. Verify some nodes are dimmed (from previous search)
2. Click the clear button (✕) next to search input
3. Verify search input is cleared
4. Verify all nodes are restored to full visibility
5. Verify all edges are restored

**Expected Results**:
- Search input value becomes empty
- All nodes have "dimmed" class removed
- All edges have "dimmed" class removed
- Full graph is visible

**Validation Points**:
- ✅ Search input value is ""
- ✅ No nodes have "dimmed" class
- ✅ No edges have "dimmed" class
- ✅ All 50 nodes are fully visible

---

### Scenario 2.7: Close Detail Panel

**Objective**: Verify closing detail panel deselects node.

**Preconditions**: Detail panel is open with a selected node

**Test Steps**:
1. Verify detail panel is visible
2. Click the close button (✕) in detail panel
3. Verify detail panel closes
4. Verify node selection is cleared
5. Verify edge highlighting is removed

**Expected Results**:
- Detail panel has style "display: none"
- Empty state message "Click a concept node to view details" is shown
- Selected node no longer has "selected" class
- All edges revert to default styling
- Edge labels are hidden

**Validation Points**:
- ✅ Detail content div has style "display: none"
- ✅ Empty state div is visible
- ✅ No nodes have "selected" class
- ✅ No edges have "highlighted" class
- ✅ Edge labels are not visible

---

### Scenario 2.8: Click Background to Deselect

**Objective**: Verify clicking graph background deselects node.

**Preconditions**: A node is selected and detail panel is open

**Test Steps**:
1. Verify a node is selected
2. Click empty space in graph (SVG background)
3. Verify detail panel closes
4. Verify node selection is cleared

**Expected Results**:
- Same as Scenario 2.7 (close detail panel)

**Validation Points**:
- ✅ Detail panel closes
- ✅ Node selection cleared
- ✅ Edge highlighting removed

---

### Scenario 2.9: Drag Node to Reposition

**Objective**: Verify nodes can be dragged to reposition in graph.

**Preconditions**: Page is loaded, graph is rendered

**Test Steps**:
1. Locate a node
2. Record initial x, y position
3. Click and drag node to new position
4. Release mouse
5. Verify node position changed
6. Verify connected edges updated to follow node

**Expected Results**:
- Node position changes during drag
- Connected edges redraw to maintain connections
- Node remains in new position after drag ends
- Simulation may continue adjusting layout slightly

**Validation Points**:
- ✅ Node x, y coordinates change
- ✅ Node's transform attribute updates
- ✅ Connected edges redraw (path d attribute updates)
- ✅ Drag functionality works smoothly

---

### Scenario 2.10: Zoom In/Out with Mouse Wheel

**Objective**: Verify graph can be zoomed using mouse wheel.

**Preconditions**: Page is loaded, graph is rendered

**Test Steps**:
1. Record initial SVG transform scale
2. Scroll mouse wheel up (zoom in)
3. Verify graph scales larger
4. Scroll mouse wheel down (zoom out)
5. Verify graph scales smaller
6. Verify zoom is constrained (min/max limits)

**Expected Results**:
- Zooming in increases scale (e.g., 1.0 → 1.5)
- Zooming out decreases scale (e.g., 1.5 → 1.0)
- Zoom is constrained between 0.5x and 3x
- Graph centers on mouse position during zoom

**Validation Points**:
- ✅ SVG g element transform changes on zoom
- ✅ Scale increases on wheel up
- ✅ Scale decreases on wheel down
- ✅ Scale constrained to [0.5, 3.0]

---

### Scenario 2.11: Legend Shows All Domains

**Objective**: Verify legend displays all domains with correct colors.

**Preconditions**: Page is loaded

**Test Steps**:
1. Locate legend panel
2. Verify legend title is "Domains"
3. Count legend items
4. Verify each item has color swatch and domain name
5. Verify colors match node colors in graph

**Expected Results**:
- Legend shows ~15 unique domains
- Each domain has colored circle matching graph nodes
- Domain names are human-readable

**Validation Points**:
- ✅ Legend contains multiple items (at least 10)
- ✅ Each item has color div and text span
- ✅ Colors match domain color mapping
- ✅ Domain names match concept domains

---

## Category 3: Edge Cases and Error Handling

### Scenario 3.1: Invalid API Route

**Objective**: Verify 404 response for non-existent API endpoints.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/invalid-endpoint`
2. Verify response status is 404

**Expected Results**:
```
Status: 404
```

**Validation Points**:
- ✅ Status code is 404
- ✅ No server crash

---

### Scenario 3.2: Malformed Concept ID in URL

**Objective**: Verify graceful handling of special characters in concept ID.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/concept/id-with-special!@#$%`
2. Verify server handles URL encoding correctly
3. Verify returns 404 (concept not found)

**Expected Results**:
```
Status: 404
Body: "Concept not found"
```

**Validation Points**:
- ✅ No server crash
- ✅ Status code is 404
- ✅ URL decoding handled correctly

---

### Scenario 3.3: Very Long Search Query

**Objective**: Verify server handles extremely long search strings.

**Preconditions**: Server is running

**Test Steps**:
1. Generate 10,000 character search string
2. Send GET request to `/api/search?q={long_string}`
3. Verify server responds without crash
4. Verify response within reasonable time

**Expected Results**:
```
Status: 200
Body: [] or matching concepts
Response time: < 1 second
```

**Validation Points**:
- ✅ No server crash
- ✅ Response received
- ✅ Response time acceptable

---

### Scenario 3.4: Concurrent API Requests

**Objective**: Verify server handles multiple simultaneous requests.

**Preconditions**: Server is running

**Test Steps**:
1. Send 50 concurrent GET requests to various endpoints
   - 20x GET /api/concepts
   - 20x GET /api/relationships
   - 10x GET /api/search?q=test
2. Verify all requests complete successfully
3. Verify all return correct data
4. Verify no race conditions or data corruption

**Expected Results**:
- All 50 requests return status 200
- All responses contain expected data
- No server errors or crashes

**Validation Points**:
- ✅ All requests succeed
- ✅ All responses are correct
- ✅ No data corruption
- ✅ Response times reasonable

---

### Scenario 3.5: Missing Data Files on Startup

**Objective**: Verify server fails gracefully if JSON files missing.

**Preconditions**: Rename or delete concepts.json

**Test Steps**:
1. Start server without concepts.json
2. Verify server logs error
3. Verify server does not start (or starts with empty data)

**Expected Results**:
- Server logs file not found error
- Server exits or runs with empty data

**Validation Points**:
- ✅ Error message logged
- ✅ No silent failure

---

### Scenario 3.6: Malformed JSON Data Files

**Objective**: Verify server fails gracefully with syntax errors in JSON.

**Preconditions**: Modify concepts.json to have syntax error

**Test Steps**:
1. Add syntax error to concepts.json (e.g., missing comma)
2. Start server
3. Verify server logs parse error
4. Verify server does not start

**Expected Results**:
- Server logs JSON parse error
- Server exits with error code

**Validation Points**:
- ✅ Parse error logged
- ✅ Server does not start in broken state

---

### Scenario 3.7: Relationship Points to Non-Existent Concept

**Objective**: Verify frontend filters invalid relationships.

**Preconditions**: relationships.json contains entry pointing to "fake-concept-id"

**Test Steps**:
1. Load page in browser
2. Verify graph renders without errors
3. Verify invalid relationship is filtered out
4. Verify edge count matches valid relationships only

**Expected Results**:
- Graph renders successfully
- Invalid relationships are not rendered as edges
- No JavaScript errors

**Validation Points**:
- ✅ No console errors
- ✅ Graph renders
- ✅ Edge count excludes invalid relationships

---

### Scenario 3.8: Search with Special Characters

**Objective**: Verify search handles special regex characters safely.

**Preconditions**: Server is running

**Test Steps**:
1. Send GET request to `/api/search?q=actor.*model$`
2. Verify server treats query as literal string (not regex)
3. Verify no regex injection

**Expected Results**:
```
Status: 200
Body: [] (no concepts contain literal "actor.*model$")
```

**Validation Points**:
- ✅ No server error
- ✅ Query treated as literal string
- ✅ No regex execution

---

### Scenario 3.9: XSS Attack via Concept Data

**Objective**: Verify frontend escapes concept data to prevent XSS.

**Preconditions**: Concept contains HTML/JavaScript in description

**Test Steps**:
1. Modify concept description to include `<script>alert('XSS')</script>`
2. Load page in browser
3. Click concept to view details
4. Verify script does not execute
5. Verify HTML is escaped and displayed as text

**Expected Results**:
- Script tag displayed as text
- No JavaScript alert appears
- HTML is escaped in DOM

**Validation Points**:
- ✅ No alert popup
- ✅ Script tag visible as text in detail panel
- ✅ DOM shows escaped HTML entities

---

### Scenario 3.10: Memory Leak During HMR Cycles

**Objective**: Verify no memory leaks during hot module reload.

**Preconditions**: Server running in dev mode with HMR enabled

**Test Steps**:
1. Record initial memory usage
2. Modify app.tsx or server.ts to trigger HMR
3. Wait for reload complete
4. Repeat 10 times
5. Record final memory usage
6. Verify memory stable (no continuous growth)

**Expected Results**:
- Memory usage remains stable
- No continuous growth over HMR cycles

**Validation Points**:
- ✅ Memory after 10 HMR cycles ≈ initial memory (within 10%)
- ✅ No continuous upward trend

---

### Scenario 3.11: Large Number of Nodes/Edges (Scalability)

**Objective**: Verify graph performs acceptably with larger datasets.

**Preconditions**: Temporarily modify JSON to have 500 concepts, 1000 relationships

**Test Steps**:
1. Load page in browser
2. Measure page load time
3. Verify graph renders
4. Test node selection performance
5. Test search filter performance

**Expected Results**:
- Page loads within 5 seconds
- Graph renders (may be slower)
- Interactions remain responsive

**Validation Points**:
- ✅ Page loads without crash
- ✅ Graph renders all nodes/edges
- ✅ Node clicks respond within 100ms
- ✅ Search filters update within 200ms

---

### Scenario 3.12: Browser Window Resize

**Objective**: Verify graph adapts to window resize.

**Preconditions**: Page is loaded

**Test Steps**:
1. Record initial graph dimensions
2. Resize browser window to smaller size
3. Verify graph resizes
4. Resize to larger size
5. Verify graph expands
6. Verify no layout breaks

**Expected Results**:
- SVG width/height update on resize
- Graph force simulation adjusts center
- No visual artifacts or broken layout

**Validation Points**:
- ✅ SVG dimensions change on resize
- ✅ Graph remains centered
- ✅ All nodes and edges remain visible
- ✅ No console errors

---

## Test Execution Summary

### Coverage Matrix

| Category | Total Scenarios | Critical | Medium | Low |
|----------|----------------|----------|--------|-----|
| API Endpoints | 13 | 10 | 2 | 1 |
| Browser Interactions | 11 | 8 | 3 | 0 |
| Edge Cases | 12 | 5 | 5 | 2 |
| **TOTAL** | **36** | **23** | **10** | **3** |

### Priority Definitions

- **Critical**: Must pass for production readiness
- **Medium**: Should pass but may have acceptable workarounds
- **Low**: Nice-to-have, improvement areas

### Automation Targets

**Phase 1 (MVP)**: Automate critical scenarios
- All API endpoint tests (13 scenarios)
- Core browser interactions: load, click, search (5 scenarios)
- Total: 18 automated tests

**Phase 2 (Full Coverage)**: Add medium priority
- Remaining browser interactions (6 scenarios)
- Key edge cases (7 scenarios)
- Total: +13 automated tests (31 total)

**Phase 3 (Comprehensive)**: Add low priority
- All remaining scenarios
- Total: +5 automated tests (36 total)

---

## Notes for Test Implementation

### API Tests
- Use `fetch()` or `curl` to test endpoints
- Run against live server instance
- Can use Bun's built-in test framework

### Browser Tests
- Use Playwright for browser automation
- Test against Chrome, Firefox, Safari
- Use data-testid attributes for reliable selectors
- Implement wait strategies for async operations

### Edge Case Tests
- Some require data manipulation (modify JSON files)
- May need separate test fixtures
- Some scenarios (like HMR memory leaks) require manual testing

### Test Data Management
- Use fixtures directory for test-specific JSON files
- Restore original data after tests
- Consider using in-memory mock data for isolation
