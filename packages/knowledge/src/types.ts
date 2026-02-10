/**
 * Knowledge domain types.
 * Epistemic levels, evidence links, knowledge items, relationships.
 */

export type EpistemicLevel =
  | 'MEASURED'
  | 'CALCULATED'
  | 'INFERRED'
  | 'CITED'
  | 'HYPOTHESIS';

export interface EvidenceLink {
  type: string;
  description: string;
  confidence: number;
  source?: string;
}

export type KnowledgeCategory = 'decision' | 'learning' | 'error';

export interface KnowledgeItem {
  id: string;
  category: KnowledgeCategory;
  content: string;
  reasoning?: string;
  epistemic_level: EpistemicLevel;
  confidence: number;
  evidence: EvidenceLink[];
  created: number;
  last_validated?: number;
  session_id?: string;
}

export type RelationshipType =
  | 'DEPENDS_ON'
  | 'CONTRADICTS'
  | 'SUPPORTS'
  | 'REFINES'
  | 'SUPERSEDES'
  | 'RELATES_TO';

export interface Relationship {
  id: string;
  type: RelationshipType;
  from: string;
  to: string;
  strength?: number;
  evidence?: string;
  created: number;
  metadata?: Record<string, unknown>;
}

export interface KnowledgeFilter {
  category?: KnowledgeCategory;
  epistemic_level?: EpistemicLevel;
  min_confidence?: number;
  max_confidence?: number;
  session_id?: string;
}

export interface RelationshipFilter {
  type?: RelationshipType;
  from?: string;
  to?: string;
  min_strength?: number;
}
