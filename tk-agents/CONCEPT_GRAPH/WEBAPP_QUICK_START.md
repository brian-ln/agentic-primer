# Concept Graph Web App - Quick Start Guide

## What is this?

An interactive web visualization of 50 interdisciplinary concepts and 61 relationships, featuring:
- Force-directed graph visualization with D3.js
- Real-time search and filtering
- Click-to-explore drill-down navigation
- Color-coded domains
- Relationship highlighting

## Quick Start

### Start the server:
```bash
cd CONCEPT_GRAPH
./start.sh

# Or manually:
bun --hot server.ts
```

### Open in browser:
Navigate to `http://localhost:3000`

## How to Use

1. **Explore the graph**: Drag nodes, zoom with mouse wheel, pan by dragging background
2. **Search**: Type in search box to filter concepts in real-time
3. **View details**: Click any node to see full description and relationships
4. **Navigate**: Click related concept names to jump between nodes
5. **Reset view**: Clear search or click background to deselect

## Files Created

```
CONCEPT_GRAPH/
├── server.ts              # Bun.serve() HTTP server
├── index.html             # Main application page
├── app.tsx                # Frontend D3.js graph logic
├── style.css              # Styling and responsive layout
├── start.sh               # Quick start script
├── README_WEBAPP.md       # Comprehensive documentation
└── WEBAPP_QUICK_START.md  # This file
```

## API Endpoints

- `GET /` - Main app
- `GET /api/concepts` - All concepts JSON
- `GET /api/relationships` - All relationships JSON
- `GET /api/concept/:id` - Specific concept details
- `GET /api/search?q=<query>` - Search concepts

## Technology

- **Runtime**: Bun.js (TypeScript execution)
- **Server**: Bun.serve() with HTML imports
- **Visualization**: D3.js v7 force-directed graph
- **Frontend**: Vanilla TypeScript
- **Styling**: Modern CSS with Grid/Flexbox

## Use Cases

1. **Visual Knowledge Exploration** - See the entire graph structure at a glance
2. **Quick Concept Discovery** - Find concepts by keyword without knowing exact IDs
3. **Relationship Tracing** - Understand how concepts connect visually
4. **Domain-Focused Study** - Explore concepts by color-coded domains
5. **Teaching and Presentations** - Interactive demos during talks
6. **Knowledge Base Validation** - Identify structural issues visually

## Requirements

- Bun.js installed (https://bun.sh)
- Modern web browser with JavaScript enabled
- No additional dependencies needed

## Troubleshooting

**Port already in use?**
```bash
PORT=8080 bun server.ts
```

**Graph doesn't load?**
- Check browser console (F12)
- Verify concepts.json and relationships.json exist
- Ensure server started successfully

**Hot reload not working?**
- Use `bun --hot server.ts` (not just `bun server.ts`)

## Next Steps

See `README_WEBAPP.md` for:
- Detailed use cases
- Architecture documentation
- Extension ideas
- Performance considerations
- Mobile/responsive features

## Examples

```bash
# Start server on different port
PORT=8080 bun server.ts

# Test API from command line
curl http://localhost:3000/api/concepts
curl http://localhost:3000/api/concept/actor-model
curl http://localhost:3000/api/search?q=erlang

# Run without hot reload
bun server.ts
```

Enjoy exploring the Concept Graph!
