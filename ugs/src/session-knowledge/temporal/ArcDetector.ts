/**
 * Thinking Arc Detector
 * Epic: agentic-primer-9ad
 * Phase: Cognitive Integration v1
 *
 * Detects patterns of conceptual evolution in sessions:
 * - Concrete → Pattern → Abstract → Philosophy
 * - Problem → Solution → Generalization
 * - Question → Investigation → Insight → Application
 *
 * Indicators:
 * - "realized", "the pattern is", "this means", "the key insight"
 * - Conceptual vocabulary shifts (specific → general)
 * - Abstraction level increases
 */

import { createClient, type Client } from '@libsql/client';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

export type ArcType =
  | 'concrete_to_abstract'  // Concrete examples → Abstract pattern
  | 'pattern_discovery'     // Noticing recurring structure
  | 'refinement'            // Iterative improvement of idea
  | 'breakthrough'          // Sudden insight/realization
  | 'synthesis';            // Combining multiple concepts

export interface ThinkingArc {
  id: string;
  sessionId: string;
  arcType: ArcType;
  startMessageId: string;
  endMessageId: string | null;
  description: string;
  breakthroughMoment: string | null;
  confidence: number;
  createdAt: number;
}

export interface ArcIndicators {
  breakthroughPhrases: string[];
  patternPhrases: string[];
  abstractionPhrases: string[];
  synthesisPhrases: string[];
  refinementPhrases: string[];
}

const INDICATORS: ArcIndicators = {
  breakthroughPhrases: [
    'realized', 'the key insight', 'aha', 'breakthrough', 'suddenly',
    'it clicked', 'finally understood', 'the real issue', 'what if'
  ],
  patternPhrases: [
    'the pattern is', 'always', 'never', 'every time', 'consistently',
    'recurring', 'systematic', 'across all', 'in every case'
  ],
  abstractionPhrases: [
    'in general', 'fundamentally', 'the principle', 'conceptually',
    'at a higher level', 'abstracting', 'the meta-', 'universally'
  ],
  synthesisPhrases: [
    'combining', 'integrating', 'bringing together', 'unified',
    'holistic', 'connects to', 'relates to', 'bridges'
  ],
  refinementPhrases: [
    'actually', 'more precisely', 'better approach', 'refined',
    'improved', 'optimized', 'clearer', 'simplified'
  ]
};

export class ArcDetector {
  private db: Client;

  constructor() {
    this.db = createClient({ url: `file:${DB_PATH}` });
  }

  /**
   * Detect thinking arcs in a session
   *
   * Analyzes the session's messages and knowledge items to find
   * patterns of conceptual evolution
   */
  async detectArcs(sessionId: string): Promise<ThinkingArc[]> {
    const arcs: ThinkingArc[] = [];

    // Get all knowledge items for this session in chronological order
    const knowledge = await this.getSessionKnowledge(sessionId);

    if (knowledge.length < 2) {
      return []; // Need at least 2 items to detect an arc
    }

    // Detect different arc types
    arcs.push(...await this.detectBreakthroughArcs(sessionId, knowledge));
    arcs.push(...await this.detectPatternArcs(sessionId, knowledge));
    arcs.push(...await this.detectAbstractionArcs(sessionId, knowledge));
    arcs.push(...await this.detectRefinementArcs(sessionId, knowledge));

    // Store detected arcs in database
    for (const arc of arcs) {
      await this.storeArc(arc);
    }

    return arcs;
  }

  /**
   * Get all knowledge items for a session
   */
  private async getSessionKnowledge(sessionId: string): Promise<Array<{
    type: string;
    content: string;
    messageId: string;
    timestamp: number;
  }>> {
    const knowledge: Array<{ type: string; content: string; messageId: string; timestamp: number }> = [];

    // Get decisions
    const decisions = await this.db.execute({
      sql: 'SELECT decision as content, message_id, timestamp FROM session_decisions WHERE session_id = ? ORDER BY timestamp ASC',
      args: [sessionId]
    });
    for (const row of decisions.rows) {
      knowledge.push({
        type: 'decision',
        content: row.content as string,
        messageId: row.message_id as string || '',
        timestamp: row.timestamp as number
      });
    }

    // Get learnings
    const learnings = await this.db.execute({
      sql: 'SELECT learning as content, message_id, timestamp FROM session_learnings WHERE session_id = ? ORDER BY timestamp ASC',
      args: [sessionId]
    });
    for (const row of learnings.rows) {
      knowledge.push({
        type: 'learning',
        content: row.content as string,
        messageId: row.message_id as string || '',
        timestamp: row.timestamp as number
      });
    }

    // Sort by timestamp
    knowledge.sort((a, b) => a.timestamp - b.timestamp);

    return knowledge;
  }

  /**
   * Detect breakthrough arcs - sudden insights or realizations
   */
  private async detectBreakthroughArcs(
    sessionId: string,
    knowledge: Array<{ type: string; content: string; messageId: string; timestamp: number }>
  ): Promise<ThinkingArc[]> {
    const arcs: ThinkingArc[] = [];

    for (let i = 0; i < knowledge.length; i++) {
      const item = knowledge[i];
      const content = item.content.toLowerCase();

      // Check for breakthrough indicators
      const breakthroughIndicators = INDICATORS.breakthroughPhrases.filter(phrase =>
        content.includes(phrase)
      );

      if (breakthroughIndicators.length > 0) {
        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType: 'breakthrough',
          startMessageId: i > 0 ? knowledge[i - 1].messageId : item.messageId,
          endMessageId: item.messageId,
          description: `Breakthrough moment: ${breakthroughIndicators.join(', ')}`,
          breakthroughMoment: item.content.slice(0, 200),
          confidence: Math.min(0.6 + breakthroughIndicators.length * 0.1, 1.0),
          createdAt: Date.now()
        });
      }
    }

    return arcs;
  }

  /**
   * Detect pattern discovery arcs - recognizing recurring structures
   */
  private async detectPatternArcs(
    sessionId: string,
    knowledge: Array<{ type: string; content: string; messageId: string; timestamp: number }>
  ): Promise<ThinkingArc[]> {
    const arcs: ThinkingArc[] = [];

    for (let i = 1; i < knowledge.length; i++) {
      const item = knowledge[i];
      const content = item.content.toLowerCase();

      // Check for pattern indicators
      const patternIndicators = INDICATORS.patternPhrases.filter(phrase =>
        content.includes(phrase)
      );

      if (patternIndicators.length > 0) {
        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType: 'pattern_discovery',
          startMessageId: knowledge[0].messageId,
          endMessageId: item.messageId,
          description: `Pattern recognized: ${patternIndicators.join(', ')}`,
          breakthroughMoment: item.content.slice(0, 200),
          confidence: Math.min(0.5 + patternIndicators.length * 0.15, 1.0),
          createdAt: Date.now()
        });
      }
    }

    return arcs;
  }

  /**
   * Detect abstraction arcs - moving from concrete to abstract
   */
  private async detectAbstractionArcs(
    sessionId: string,
    knowledge: Array<{ type: string; content: string; messageId: string; timestamp: number }>
  ): Promise<ThinkingArc[]> {
    const arcs: ThinkingArc[] = [];

    // Look for sequences where later items are more abstract than earlier ones
    for (let i = 2; i < knowledge.length; i++) {
      const item = knowledge[i];
      const content = item.content.toLowerCase();

      // Check for abstraction indicators
      const abstractionIndicators = INDICATORS.abstractionPhrases.filter(phrase =>
        content.includes(phrase)
      );

      // Also check for synthesis (often accompanies abstraction)
      const synthesisIndicators = INDICATORS.synthesisPhrases.filter(phrase =>
        content.includes(phrase)
      );

      const totalIndicators = abstractionIndicators.length + synthesisIndicators.length;

      if (totalIndicators > 0) {
        const arcType = synthesisIndicators.length > abstractionIndicators.length
          ? 'synthesis'
          : 'concrete_to_abstract';

        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType,
          startMessageId: knowledge[0].messageId,
          endMessageId: item.messageId,
          description: arcType === 'synthesis'
            ? `Synthesis: ${synthesisIndicators.join(', ')}`
            : `Abstraction: ${abstractionIndicators.join(', ')}`,
          breakthroughMoment: item.content.slice(0, 200),
          confidence: Math.min(0.5 + totalIndicators * 0.12, 1.0),
          createdAt: Date.now()
        });
      }
    }

    return arcs;
  }

  /**
   * Detect refinement arcs - iterative improvement of ideas
   */
  private async detectRefinementArcs(
    sessionId: string,
    knowledge: Array<{ type: string; content: string; messageId: string; timestamp: number }>
  ): Promise<ThinkingArc[]> {
    const arcs: ThinkingArc[] = [];

    for (let i = 1; i < knowledge.length; i++) {
      const item = knowledge[i];
      const prev = knowledge[i - 1];
      const content = item.content.toLowerCase();

      // Check for refinement indicators
      const refinementIndicators = INDICATORS.refinementPhrases.filter(phrase =>
        content.includes(phrase)
      );

      if (refinementIndicators.length > 0) {
        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType: 'refinement',
          startMessageId: prev.messageId,
          endMessageId: item.messageId,
          description: `Refinement: ${refinementIndicators.join(', ')}`,
          breakthroughMoment: item.content.slice(0, 200),
          confidence: Math.min(0.4 + refinementIndicators.length * 0.15, 1.0),
          createdAt: Date.now()
        });
      }
    }

    return arcs;
  }

  /**
   * Store an arc in the database
   */
  private async storeArc(arc: ThinkingArc): Promise<void> {
    await this.db.execute({
      sql: `
        INSERT INTO thinking_arcs (id, session_id, arc_type, start_message_id, end_message_id,
                                    description, breakthrough_moment, confidence, created_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
          confidence = excluded.confidence,
          description = excluded.description
      `,
      args: [
        arc.id,
        arc.sessionId,
        arc.arcType,
        arc.startMessageId,
        arc.endMessageId,
        arc.description,
        arc.breakthroughMoment,
        arc.confidence,
        arc.createdAt
      ]
    });
  }

  /**
   * Get all arcs for a session
   */
  async getSessionArcs(sessionId: string): Promise<ThinkingArc[]> {
    const result = await this.db.execute({
      sql: `
        SELECT id, session_id, arc_type, start_message_id, end_message_id,
               description, breakthrough_moment, confidence, created_at
        FROM thinking_arcs
        WHERE session_id = ?
        ORDER BY created_at ASC
      `,
      args: [sessionId]
    });

    return result.rows.map(row => ({
      id: row.id as string,
      sessionId: row.session_id as string,
      arcType: row.arc_type as ArcType,
      startMessageId: row.start_message_id as string,
      endMessageId: row.end_message_id as string | null,
      description: row.description as string,
      breakthroughMoment: row.breakthrough_moment as string | null,
      confidence: row.confidence as number,
      createdAt: row.created_at as number
    }));
  }

  close() {
    this.db.close();
  }
}
