/**
 * Epistemic Gradients Type Definitions
 * Epic: Knowledge Architecture Phase 1 MVP
 * Date: 2026-02-03
 *
 * Type definitions for epistemic gradient system - tracking certainty levels
 * from reject (0-20%) through wonder (40-60%) to know (95-100%).
 *
 * Theoretical basis:
 * - Epistemology (Plato, Popper, Bayesian reasoning)
 * - Knowledge Graphs with confidence scores
 * - Zettelkasten knowledge maturation
 */

/**
 * Epistemic levels representing degrees of certainty
 *
 * Based on confidence ranges:
 * - reject: 0-20% (actively disbelieving)
 * - doubt: 20-40% (skeptical, needs evidence)
 * - wonder: 40-60% (open question, exploring)
 * - suspect: 60-80% (hypothesis with some evidence)
 * - believe: 80-95% (high confidence, unvalidated)
 * - know: 95-100% (validated, proven)
 */
export type EpistemicLevel = 'reject' | 'doubt' | 'wonder' | 'suspect' | 'believe' | 'know';

/**
 * Confidence ranges for each epistemic level
 */
export const EPISTEMIC_CONFIDENCE_RANGES: Record<EpistemicLevel, { min: number; max: number }> = {
  reject: { min: 0.0, max: 0.20 },
  doubt: { min: 0.20, max: 0.40 },
  wonder: { min: 0.40, max: 0.60 },
  suspect: { min: 0.60, max: 0.80 },
  believe: { min: 0.80, max: 0.95 },
  know: { min: 0.95, max: 1.0 }
};

/**
 * Evidence types that support epistemic claims
 */
export type EvidenceType =
  | 'MEASURED'      // Direct measurement or calculation
  | 'CALCULATED'    // Derived from measurements
  | 'INFERRED'      // Logical inference from evidence
  | 'CITED'         // External source reference
  | 'HYPOTHESIS'    // Theoretical claim
  | 'SPECULATION'   // Uncertain guess
  | 'VALIDATED'     // Tested and confirmed
  | 'EXPERIMENT';   // Empirical test result

/**
 * Evidence link structure
 */
export interface EvidenceLink {
  type: EvidenceType;
  source?: string;      // File path, URL, or reference
  description: string;  // What this evidence shows
  confidence?: number;  // How strong is this evidence (0-1)
  timestamp?: number;   // When was this evidence gathered
}

/**
 * Enhanced Decision with epistemic fields
 */
export interface EpistemicDecision {
  id: string;
  session_id: string;
  timestamp: number;
  decision: string;
  reasoning: string | null;
  alternatives: string | null;
  context: string | null;
  // Epistemic fields
  epistemic_level?: EpistemicLevel;
  confidence?: number;
  evidence?: string;  // JSON array of EvidenceLink[]
  last_validated?: number;
}

/**
 * Enhanced Learning with epistemic fields
 */
export interface EpistemicLearning {
  id: string;
  session_id: string;
  timestamp: number;
  learning: string;
  context: string | null;
  actionable: string | null;
  // Epistemic fields
  epistemic_level?: EpistemicLevel;
  confidence?: number;
  evidence?: string;  // JSON array of EvidenceLink[]
  last_validated?: number;
}

/**
 * Enhanced Error with epistemic fields
 */
export interface EpistemicError {
  id: string;
  session_id: string;
  timestamp: number;
  error_type: string;
  root_cause: string | null;
  suggested_fix: string | null;
  resolution_status: string | null;
  // Epistemic fields
  epistemic_level?: EpistemicLevel;
  confidence?: number;
  evidence?: string;  // JSON array of EvidenceLink[]
  last_validated?: number;
}

/**
 * Convert confidence score to epistemic level
 */
export function confidenceToEpistemicLevel(confidence: number): EpistemicLevel {
  if (confidence < 0 || confidence > 1) {
    throw new Error(`Invalid confidence: ${confidence}. Must be between 0 and 1.`);
  }

  if (confidence <= 0.20) return 'reject';
  if (confidence <= 0.40) return 'doubt';
  if (confidence <= 0.60) return 'wonder';
  if (confidence <= 0.80) return 'suspect';
  if (confidence <= 0.95) return 'believe';
  return 'know';
}

/**
 * Get suggested confidence range for epistemic level
 * Returns midpoint of range
 */
export function epistemicLevelToConfidence(level: EpistemicLevel): number {
  const range = EPISTEMIC_CONFIDENCE_RANGES[level];
  return (range.min + range.max) / 2;
}

/**
 * Validate that confidence matches epistemic level
 */
export function validateEpistemicConsistency(
  level: EpistemicLevel,
  confidence: number
): boolean {
  const range = EPISTEMIC_CONFIDENCE_RANGES[level];
  return confidence >= range.min && confidence <= range.max;
}

/**
 * Parse evidence JSON string to EvidenceLink array
 */
export function parseEvidence(evidenceJson: string | null): EvidenceLink[] {
  if (!evidenceJson) return [];

  try {
    const parsed = JSON.parse(evidenceJson);
    if (!Array.isArray(parsed)) return [];
    return parsed as EvidenceLink[];
  } catch {
    return [];
  }
}

/**
 * Serialize evidence array to JSON string
 */
export function serializeEvidence(evidence: EvidenceLink[]): string {
  return JSON.stringify(evidence);
}

/**
 * Check if knowledge item needs revalidation
 *
 * @param lastValidated - Timestamp of last validation
 * @param maxAge - Maximum age in milliseconds before revalidation needed
 * @returns true if item needs revalidation
 */
export function needsRevalidation(lastValidated: number | null | undefined, maxAge: number): boolean {
  if (!lastValidated) return true;

  const age = Date.now() - lastValidated;
  return age > maxAge;
}

/**
 * Default revalidation intervals by epistemic level (in milliseconds)
 */
export const DEFAULT_REVALIDATION_INTERVALS: Record<EpistemicLevel, number> = {
  reject: 0,                                    // Don't revalidate rejected knowledge
  doubt: 7 * 24 * 60 * 60 * 1000,              // 1 week
  wonder: 3 * 24 * 60 * 60 * 1000,             // 3 days
  suspect: 14 * 24 * 60 * 60 * 1000,           // 2 weeks
  believe: 30 * 24 * 60 * 60 * 1000,           // 1 month
  know: 90 * 24 * 60 * 60 * 1000               // 3 months
};
