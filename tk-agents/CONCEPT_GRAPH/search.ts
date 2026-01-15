// Concept Graph Search and Navigation
// Simple, extensible utilities for exploring interdisciplinary concepts

import conceptsData from "./concepts.json";
import relationshipsData from "./relationships.json";

// Types
export interface Concept {
  id: string;
  label: string;
  domains: string[];
  tags: string[];
  description: string;
  references: string[];
}

export interface Relationship {
  from: string;
  to: string;
  type: string;
  description: string;
}

export interface SearchOptions {
  domain?: string;
  tag?: string;
  keyword?: string;
  maxResults?: number;
}

export interface TraversalOptions {
  maxDepth?: number;
  relationshipTypes?: string[];
  direction?: "outgoing" | "incoming" | "both";
}

// Load data
const concepts: Concept[] = conceptsData.concepts;
const relationships: Relationship[] = relationshipsData.relationships;

// Build indexes for fast lookup
const conceptById = new Map<string, Concept>();
const conceptsByDomain = new Map<string, Concept[]>();
const conceptsByTag = new Map<string, Concept[]>();
const outgoingEdges = new Map<string, Relationship[]>();
const incomingEdges = new Map<string, Relationship[]>();

// Initialize indexes
for (const concept of concepts) {
  conceptById.set(concept.id, concept);

  for (const domain of concept.domains) {
    if (!conceptsByDomain.has(domain)) {
      conceptsByDomain.set(domain, []);
    }
    conceptsByDomain.get(domain)!.push(concept);
  }

  for (const tag of concept.tags) {
    if (!conceptsByTag.has(tag)) {
      conceptsByTag.set(tag, []);
    }
    conceptsByTag.get(tag)!.push(concept);
  }
}

for (const rel of relationships) {
  if (!outgoingEdges.has(rel.from)) {
    outgoingEdges.set(rel.from, []);
  }
  outgoingEdges.get(rel.from)!.push(rel);

  if (!incomingEdges.has(rel.to)) {
    incomingEdges.set(rel.to, []);
  }
  incomingEdges.get(rel.to)!.push(rel);
}

// Search Functions

/**
 * Search concepts by domain
 */
export function searchByDomain(domain: string): Concept[] {
  return conceptsByDomain.get(domain) ?? [];
}

/**
 * Search concepts by tag
 */
export function searchByTag(tag: string): Concept[] {
  return conceptsByTag.get(tag) ?? [];
}

/**
 * Search concepts by keyword (case-insensitive, searches label and description)
 */
export function searchByKeyword(keyword: string): Concept[] {
  const lowerKeyword = keyword.toLowerCase();
  return concepts.filter(
    (c) =>
      c.label.toLowerCase().includes(lowerKeyword) ||
      c.description.toLowerCase().includes(lowerKeyword)
  );
}

/**
 * Combined search with multiple criteria
 */
export function search(options: SearchOptions): Concept[] {
  let results = [...concepts];

  if (options.domain) {
    results = results.filter((c) => c.domains.includes(options.domain!));
  }

  if (options.tag) {
    results = results.filter((c) => c.tags.includes(options.tag!));
  }

  if (options.keyword) {
    const lowerKeyword = options.keyword.toLowerCase();
    results = results.filter(
      (c) =>
        c.label.toLowerCase().includes(lowerKeyword) ||
        c.description.toLowerCase().includes(lowerKeyword)
    );
  }

  if (options.maxResults) {
    results = results.slice(0, options.maxResults);
  }

  return results;
}

/**
 * Get concept by ID
 */
export function getConcept(id: string): Concept | undefined {
  return conceptById.get(id);
}

/**
 * Get all unique domains
 */
export function getAllDomains(): string[] {
  return [...conceptsByDomain.keys()].sort();
}

/**
 * Get all unique tags
 */
export function getAllTags(): string[] {
  return [...conceptsByTag.keys()].sort();
}

// Traversal Functions

/**
 * Get direct relationships from a concept
 */
export function getOutgoingRelationships(conceptId: string): Relationship[] {
  return outgoingEdges.get(conceptId) ?? [];
}

/**
 * Get direct relationships to a concept
 */
export function getIncomingRelationships(conceptId: string): Relationship[] {
  return incomingEdges.get(conceptId) ?? [];
}

/**
 * Get all related concepts (both directions)
 */
export function getRelatedConcepts(conceptId: string): {
  outgoing: Array<{ relationship: Relationship; concept: Concept }>;
  incoming: Array<{ relationship: Relationship; concept: Concept }>;
} {
  const outgoing = getOutgoingRelationships(conceptId)
    .map((rel) => ({
      relationship: rel,
      concept: conceptById.get(rel.to)!,
    }))
    .filter((item) => item.concept);

  const incoming = getIncomingRelationships(conceptId)
    .map((rel) => ({
      relationship: rel,
      concept: conceptById.get(rel.from)!,
    }))
    .filter((item) => item.concept);

  return { outgoing, incoming };
}

/**
 * Traverse graph from a starting concept
 * Returns concepts reachable within maxDepth hops
 */
export function traverse(
  startId: string,
  options: TraversalOptions = {}
): Map<string, { concept: Concept; depth: number; path: string[] }> {
  const maxDepth = options.maxDepth ?? 2;
  const relationshipTypes = options.relationshipTypes
    ? new Set(options.relationshipTypes)
    : null;
  const direction = options.direction ?? "outgoing";

  const visited = new Map<
    string,
    { concept: Concept; depth: number; path: string[] }
  >();
  const queue: Array<{ id: string; depth: number; path: string[] }> = [
    { id: startId, depth: 0, path: [startId] },
  ];

  while (queue.length > 0) {
    const { id, depth, path } = queue.shift()!;

    if (visited.has(id) || depth > maxDepth) {
      continue;
    }

    const concept = conceptById.get(id);
    if (!concept) {
      continue;
    }

    visited.set(id, { concept, depth, path });

    if (depth === maxDepth) {
      continue;
    }

    // Add neighbors to queue
    const neighbors: string[] = [];

    if (direction === "outgoing" || direction === "both") {
      const outgoing = outgoingEdges.get(id) ?? [];
      for (const rel of outgoing) {
        if (!relationshipTypes || relationshipTypes.has(rel.type)) {
          neighbors.push(rel.to);
        }
      }
    }

    if (direction === "incoming" || direction === "both") {
      const incoming = incomingEdges.get(id) ?? [];
      for (const rel of incoming) {
        if (!relationshipTypes || relationshipTypes.has(rel.type)) {
          neighbors.push(rel.from);
        }
      }
    }

    for (const neighborId of neighbors) {
      if (!visited.has(neighborId)) {
        queue.push({
          id: neighborId,
          depth: depth + 1,
          path: [...path, neighborId],
        });
      }
    }
  }

  return visited;
}

/**
 * Find shortest path between two concepts
 */
export function findPath(
  fromId: string,
  toId: string,
  maxDepth: number = 5
): string[] | null {
  const queue: Array<{ id: string; path: string[] }> = [
    { id: fromId, path: [fromId] },
  ];
  const visited = new Set<string>();

  while (queue.length > 0) {
    const { id, path } = queue.shift()!;

    if (id === toId) {
      return path;
    }

    if (visited.has(id) || path.length > maxDepth) {
      continue;
    }

    visited.add(id);

    // Check outgoing edges
    const outgoing = outgoingEdges.get(id) ?? [];
    for (const rel of outgoing) {
      if (!visited.has(rel.to)) {
        queue.push({ id: rel.to, path: [...path, rel.to] });
      }
    }

    // Check incoming edges (bidirectional search)
    const incoming = incomingEdges.get(id) ?? [];
    for (const rel of incoming) {
      if (!visited.has(rel.from)) {
        queue.push({ id: rel.from, path: [...path, rel.from] });
      }
    }
  }

  return null;
}

// Display/Export Functions

/**
 * Format concept for display
 */
export function formatConcept(concept: Concept): string {
  const lines = [
    `# ${concept.label} (${concept.id})`,
    "",
    `**Domains:** ${concept.domains.join(", ")}`,
    `**Tags:** ${concept.tags.join(", ")}`,
    "",
    concept.description,
    "",
  ];

  if (concept.references.length > 0) {
    lines.push(`**See also:** ${concept.references.join(", ")}`);
  }

  return lines.join("\n");
}

/**
 * Format relationships for display
 */
export function formatRelationships(conceptId: string): string {
  const related = getRelatedConcepts(conceptId);
  const lines = [`# Relationships for ${conceptId}`, ""];

  if (related.outgoing.length > 0) {
    lines.push("## Outgoing");
    for (const { relationship, concept } of related.outgoing) {
      lines.push(
        `- **${relationship.type}** → ${concept.label}: ${relationship.description}`
      );
    }
    lines.push("");
  }

  if (related.incoming.length > 0) {
    lines.push("## Incoming");
    for (const { relationship, concept } of related.incoming) {
      lines.push(
        `- **${relationship.type}** ← ${concept.label}: ${relationship.description}`
      );
    }
  }

  return lines.join("\n");
}

/**
 * Generate summary statistics
 */
export function getStats(): {
  totalConcepts: number;
  totalRelationships: number;
  domains: number;
  tags: number;
  relationshipTypes: number;
} {
  const relationshipTypes = new Set(relationships.map((r) => r.type));

  return {
    totalConcepts: concepts.length,
    totalRelationships: relationships.length,
    domains: conceptsByDomain.size,
    tags: conceptsByTag.size,
    relationshipTypes: relationshipTypes.size,
  };
}

// Export everything
export { concepts, relationships };
