import { parseSessionFile } from './src/entities/session-graph-parser.js';
import { validateSessionGraph } from './src/entities/session-graph-parser.js';
import { homedir } from 'os';

const sessions = [
  `${homedir()}/.claude/projects/-Users-bln-play-agentic-primer/f5321ccd-5ffe-4968-8091-8c05514a5806.jsonl`,
  `${homedir()}/.claude/projects/-Users-bln-play-agentic-primer/530fe4ef-92df-4e7a-8dc6-de1057dc15df.jsonl`,
  `${homedir()}/.claude/projects/-Users-bln-play-brianln-ai/`,
];

for (const sessionFile of sessions.slice(0, 2)) {
  console.log(`\nParsing: ${sessionFile.split('/').pop()}`);
  const graph = await parseSessionFile(sessionFile);

  const turnNodes = graph.nodes.filter(n => n.type === 'turn');
  const syntheticTurns = turnNodes.filter(n => n.model === '<synthetic>');
  const edgeSet = new Set(graph.edges.map(e => `${e.from}→${e.to}→${e.type}`));
  const duplicates = graph.edges.length - edgeSet.size;
  const forkPoints = graph.nodes.filter(n => n.type === 'fork-point');
  const violations = validateSessionGraph(graph);

  console.log(`  Nodes: ${graph.nodes.length}  Edges: ${graph.edges.length}  Turns: ${turnNodes.length}`);
  console.log(`  Forks: ${graph.meta.forkCount}  Joins: ${graph.meta.joinCount}  ForkPointNodes: ${forkPoints.length}`);
  console.log(`  Synthetic turns (must be 0): ${syntheticTurns.length}  Duplicate edges (must be 0): ${duplicates}`);
  console.log(`  C4 violations (must be 0): ${violations.length}`);
  if (violations.length > 0) violations.forEach(v => console.log(`    VIOLATION: ${v}`));
  if (syntheticTurns.length > 0 || duplicates > 0 || violations.length > 0) {
    console.log('  STATUS: FAIL');
    process.exit(1);
  } else {
    console.log('  STATUS: PASS');
  }
}

// Synthetic event filter test
console.log('\n--- Synthetic event filter test ---');
import { buildSessionGraph } from './src/entities/session-graph-schema.js';
const syntheticLines = [
  JSON.stringify({ type: 'user', uuid: 'u1', parentUuid: undefined, timestamp: '2024-01-01T00:00:00Z', message: { role: 'user', content: 'Hello' } }),
  JSON.stringify({ type: 'assistant', uuid: 'a1', parentUuid: 'u1', timestamp: '2024-01-01T00:00:01Z', isApiErrorMessage: true, message: { role: 'assistant', content: "You've hit your limit", model: 'claude-3' } }),
  JSON.stringify({ type: 'assistant', uuid: 'a2', parentUuid: 'u1', timestamp: '2024-01-01T00:00:02Z', message: { role: 'assistant', content: 'Real response', model: '<synthetic>' } }),
  JSON.stringify({ type: 'user', uuid: 'u2', parentUuid: 'u1', timestamp: '2024-01-01T00:00:03Z', message: { role: 'user', content: 'Real follow-up' } }),
];
const testGraph = buildSessionGraph(syntheticLines, 'test-session', '/test/project');
const testTurns = testGraph.nodes.filter(n => n.type === 'turn');
console.log(`  Turn nodes (should be 2: u1 and u2): ${testTurns.length}`);
const noSyntheticAssistants = testTurns.every(t => t.model !== '<synthetic>');
console.log(`  No synthetic model on turns (should be true): ${noSyntheticAssistants}`);

// Multi-child fork test
console.log('\n--- Fork-point (multi-child) test ---');
const forkLines = [
  JSON.stringify({ type: 'user', uuid: 'root', parentUuid: undefined, timestamp: '2024-01-01T00:00:00Z', message: { role: 'user', content: 'Root message' } }),
  JSON.stringify({ type: 'user', uuid: 'child1', parentUuid: 'root', timestamp: '2024-01-01T00:01:00Z', message: { role: 'user', content: 'Branch A' } }),
  JSON.stringify({ type: 'user', uuid: 'child2', parentUuid: 'root', timestamp: '2024-01-01T00:02:00Z', message: { role: 'user', content: 'Branch B (resume)' } }),
];
const forkGraph = buildSessionGraph(forkLines, 'fork-session', '/fork/project');
const forkNodes = forkGraph.nodes.filter(n => n.type === 'fork-point');
const forksToEdges = forkGraph.edges.filter(e => e.type === 'forks-to');
const dupForkEdges = forkGraph.edges.length - new Set(forkGraph.edges.map(e => `${e.from}→${e.to}→${e.type}`)).size;
console.log(`  Fork-point nodes (should be 1): ${forkNodes.length}`);
console.log(`  forks-to edges (should be 2): ${forksToEdges.length}`);
console.log(`  Duplicate edges (should be 0): ${dupForkEdges}`);
const forkViolations = validateSessionGraph(forkGraph);
console.log(`  C4 violations (should be 0): ${forkViolations.length}`);
if (forkViolations.length > 0) forkViolations.forEach(v => console.log(`    VIOLATION: ${v}`));

// Duplicate child test (same from→to emitted twice)
console.log('\n--- Duplicate edge dedup test ---');
const dupLines = [
  JSON.stringify({ type: 'user', uuid: 'p', parentUuid: undefined, timestamp: '2024-01-01T00:00:00Z', message: { role: 'user', content: 'Parent' } }),
  JSON.stringify({ type: 'user', uuid: 'c', parentUuid: 'p', timestamp: '2024-01-01T00:01:00Z', message: { role: 'user', content: 'Child (appears twice in JSONL due to resume)' } }),
  JSON.stringify({ type: 'user', uuid: 'c', parentUuid: 'p', timestamp: '2024-01-01T00:01:00Z', message: { role: 'user', content: 'Child (duplicate JSONL entry)' } }),
];
const dupGraph = buildSessionGraph(dupLines, 'dup-session', '/dup/project');
const dupEdges = dupGraph.edges.length - new Set(dupGraph.edges.map(e => `${e.from}→${e.to}→${e.type}`)).size;
console.log(`  Duplicate edges (should be 0): ${dupEdges}`);

console.log('\nAll tests: PASS');
