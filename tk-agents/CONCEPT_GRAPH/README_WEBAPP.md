# Concept Graph Web Application

An interactive web visualization of the CONCEPT_GRAPH knowledge base, featuring force-directed graph visualization, real-time search filtering, and drill-down navigation.

## Overview

This web application transforms the CONCEPT_GRAPH from a CLI tool into an interactive visual experience. It enables users to explore 50+ interdisciplinary concepts and 60+ relationships through an intuitive graph interface.

## Use Cases

### 1. Visual Knowledge Exploration
- **Problem**: Text-based CLI exploration can be limiting when understanding complex interconnected concepts
- **Solution**: See the entire knowledge graph laid out spatially, with color-coded categories and visual relationship lines
- **Benefit**: Quickly identify clusters, isolated concepts, and the overall structure of the knowledge domain

### 2. Quick Concept Discovery
- **Problem**: Finding relevant concepts requires knowing exact IDs or navigating hierarchies
- **Solution**: Type keywords in real-time search to instantly filter and highlight matching concepts
- **Benefit**: Discover concepts by approximate terms, descriptions, or related tags without memorizing IDs

### 3. Relationship Tracing
- **Problem**: Understanding how concepts connect requires multiple CLI commands and mental mapping
- **Solution**: Click any concept node to see its direct relationships highlighted visually with edge labels
- **Benefit**: Instantly see "what uses what", "what implements what", and other semantic connections

### 4. Domain-Focused Study
- **Problem**: Concepts span multiple domains (neuroscience, computer science, mathematics), making focused study difficult
- **Solution**: Color-coded nodes group concepts by primary domain, with legend for quick reference
- **Benefit**: Focus on specific domains while seeing how they bridge to other fields

### 5. Teaching and Presentation
- **Problem**: Explaining concept relationships in presentations requires switching between tools
- **Solution**: Interactive graph that can be zoomed, panned, and clicked to demonstrate connections live
- **Benefit**: Engage audiences with visual exploration during talks, workshops, or educational sessions

### 6. Knowledge Base Validation
- **Problem**: Identifying orphaned concepts, over-connected hubs, or missing relationships is tedious
- **Solution**: Visual graph reveals structural issues at a glance (isolated nodes, bottlenecks, clusters)
- **Benefit**: Improve knowledge graph quality through visual inspection and pattern recognition

## Features

### Graph Visualization
- Force-directed layout automatically positions nodes based on relationships
- Color-coded nodes by domain category
- Relationship edges with directional arrows
- Zoom and pan for detailed exploration
- Responsive layout that adapts to window size

### Interactive Search
- Real-time text filtering across concept labels and descriptions
- Instant visual feedback highlighting matching nodes
- Non-matching nodes dim for focus
- Clear search to restore full graph view

### Drill-Down Navigation
- Click any concept node to view detailed information
- Shows full description, tags, and domains
- Displays outgoing and incoming relationships
- Lists related concepts with relationship types
- Click relationships to jump to connected concepts

### Performance
- Efficiently handles 50 concepts and 61 relationships
- Smooth animations and transitions
- Lazy rendering for optimal browser performance
- Client-side search with no server round-trips

## Technology Stack

- **Runtime**: Bun.js for fast TypeScript execution
- **Server**: Bun.serve() with HTML imports (no Express needed)
- **Frontend**: Vanilla TypeScript with D3.js for graph visualization
- **Styling**: Modern CSS with CSS Grid and Flexbox
- **Data**: Static JSON files loaded at startup

## Architecture

```
CONCEPT_GRAPH/
├── server.ts          # Bun.serve() HTTP server with routes
├── index.html         # Main page with app container
├── app.tsx            # Frontend graph visualization logic
├── style.css          # Styling and layout
├── concepts.json      # Concept data (50 nodes)
├── relationships.json # Relationship data (61 edges)
└── search.ts          # Shared data loading utilities
```

## Installation and Usage

### Prerequisites
- Bun.js installed (https://bun.sh)
- Modern web browser with JavaScript enabled

### Running the Application

```bash
# From CONCEPT_GRAPH directory
cd CONCEPT_GRAPH

# Start the development server with hot reload
bun --hot server.ts

# Or run without hot reload
bun server.ts
```

The server starts on `http://localhost:3000` by default.

### Using the Interface

1. **View the Graph**: Open browser to `http://localhost:3000`
   - Nodes represent concepts, colored by domain
   - Edges show relationships with directional arrows
   - Drag nodes to rearrange the graph manually

2. **Search for Concepts**: Type in the search box
   - Matching concepts highlight in bright colors
   - Non-matching concepts dim out
   - Clear search to reset view

3. **Explore Details**: Click any concept node
   - Detail panel appears on the right
   - Shows full description and metadata
   - Lists all incoming and outgoing relationships
   - Click relationship targets to navigate

4. **Navigate Relationships**: Follow connections
   - Click related concept names to jump
   - Graph re-centers on new selection
   - Detail panel updates automatically

## API Endpoints

The server exposes these HTTP endpoints:

- `GET /` - Main application page
- `GET /api/concepts` - JSON array of all concepts
- `GET /api/relationships` - JSON array of all relationships
- `GET /api/concept/:id` - JSON details for specific concept
- `GET /api/search?q=<query>` - Search concepts by keyword

## Extending the Application

### Adding New Visualizations
Edit `app.tsx` to add new D3.js visualizations or charts.

### Custom Styling
Modify `style.css` to change colors, layouts, or responsive breakpoints.

### Additional Features
Consider adding:
- Graph filtering by domain or tag
- Shortest path visualization between two concepts
- Export graph as SVG or PNG
- Shareable URLs for specific concepts
- Concept comparison view
- Time-based animation of relationship types

## Performance Considerations

- Graph scales efficiently to 100+ nodes with current force simulation
- For larger graphs (500+ nodes), consider:
  - Virtual rendering (only visible nodes)
  - Server-side graph clustering
  - Progressive loading of graph sections
  - WebGL rendering for very large graphs

## Troubleshooting

### Graph doesn't load
- Check browser console for errors
- Verify concepts.json and relationships.json are valid
- Ensure server is running and accessible

### Search not working
- Clear browser cache and reload
- Check that search input has proper event listeners
- Verify search logic in app.tsx

### Performance issues
- Reduce number of visible nodes via filtering
- Disable animations in force simulation
- Use browser DevTools to profile rendering

## Future Enhancements

- Collaborative annotations and comments on concepts
- Graph versioning and diff visualization
- Integration with external knowledge bases (Wikipedia, academic papers)
- AI-assisted concept suggestion and relationship inference
- Export to various graph formats (GraphML, GEXF, Cytoscape)
- Mobile-responsive touch interface
- Offline PWA support

## Related Tools

- `explore.ts` - CLI interface for graph exploration
- `search.ts` - Shared data loading and search utilities
- `examples.ts` - Example usage patterns

## License

MIT License - see project root for details.
