/**
 * Thinking Arc Detector
 * Detects patterns of conceptual evolution in sessions.
 *
 * Arcs:
 * - Concrete -> Pattern -> Abstract -> Philosophy
 * - Problem -> Solution -> Generalization
 * - Question -> Investigation -> Insight -> Application
 *
 * Uses ISqlStorage interface -- no direct dependency on any SQL driver.
 */

import type { ISqlStorage, SqlValue } from '@agentic-primer/actors';

export type ArcType =
  | 'concrete_to_abstract'
  | 'pattern_discovery'
  | 'refinement'
  | 'breakthrough'
  | 'synthesis';

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
    'it clicked', 'finally understood', 'the real issue', 'what if',
  ],
  patternPhrases: [
    'the pattern is', 'always', 'never', 'every time', 'consistently',
    'recurring', 'systematic', 'across all', 'in every case',
  ],
  abstractionPhrases: [
    'in general', 'fundamentally', 'the principle', 'conceptually',
    'at a higher level', 'abstracting', 'the meta-', 'universally',
  ],
  synthesisPhrases: [
    'combining', 'integrating', 'bringing together', 'unified',
    'holistic', 'connects to', 'relates to', 'bridges',
  ],
  refinementPhrases: [
    'actually', 'more precisely', 'better approach', 'refined',
    'improved', 'optimized', 'clearer', 'simplified',
  ],
};

interface KnowledgeEntry {
  type: string;
  content: string;
  messageId: string;
  timestamp: number;
}

export class ArcDetector {
  constructor(private storage: ISqlStorage) {}

  async detectArcs(sessionId: string): Promise<ThinkingArc[]> {
    const arcs: ThinkingArc[] = [];
    const knowledge = await this.getSessionKnowledge(sessionId);
    if (knowledge.length < 2) return [];

    arcs.push(...this.detectBreakthroughArcs(sessionId, knowledge));
    arcs.push(...this.detectPatternArcs(sessionId, knowledge));
    arcs.push(...this.detectAbstractionArcs(sessionId, knowledge));
    arcs.push(...this.detectRefinementArcs(sessionId, knowledge));

    for (const arc of arcs) {
      await this.storeArc(arc);
    }

    return arcs;
  }

  private async getSessionKnowledge(sessionId: string): Promise<KnowledgeEntry[]> {
    const knowledge: KnowledgeEntry[] = [];

    const decisions = await this.storage.execute(
      'SELECT decision as content, message_id, timestamp FROM session_decisions WHERE session_id = ? ORDER BY timestamp ASC',
      [sessionId]
    );
    for (const row of decisions.rows) {
      const colIdx = (name: string) => decisions.columns.indexOf(name);
      knowledge.push({
        type: 'decision',
        content: row[colIdx('content')] as string,
        messageId: (row[colIdx('message_id')] as string) || '',
        timestamp: row[colIdx('timestamp')] as number,
      });
    }

    const learnings = await this.storage.execute(
      'SELECT learning as content, message_id, timestamp FROM session_learnings WHERE session_id = ? ORDER BY timestamp ASC',
      [sessionId]
    );
    for (const row of learnings.rows) {
      const colIdx = (name: string) => learnings.columns.indexOf(name);
      knowledge.push({
        type: 'learning',
        content: row[colIdx('content')] as string,
        messageId: (row[colIdx('message_id')] as string) || '',
        timestamp: row[colIdx('timestamp')] as number,
      });
    }

    knowledge.sort((a, b) => a.timestamp - b.timestamp);
    return knowledge;
  }

  private detectBreakthroughArcs(sessionId: string, knowledge: KnowledgeEntry[]): ThinkingArc[] {
    const arcs: ThinkingArc[] = [];
    for (let i = 0; i < knowledge.length; i++) {
      const item = knowledge[i];
      const content = item.content.toLowerCase();
      const hits = INDICATORS.breakthroughPhrases.filter(p => content.includes(p));
      if (hits.length > 0) {
        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType: 'breakthrough',
          startMessageId: i > 0 ? knowledge[i - 1].messageId : item.messageId,
          endMessageId: item.messageId,
          description: `Breakthrough moment: ${hits.join(', ')}`,
          breakthroughMoment: item.content.slice(0, 200),
          confidence: Math.min(0.6 + hits.length * 0.1, 1.0),
          createdAt: Date.now(),
        });
      }
    }
    return arcs;
  }

  private detectPatternArcs(sessionId: string, knowledge: KnowledgeEntry[]): ThinkingArc[] {
    const arcs: ThinkingArc[] = [];
    for (let i = 1; i < knowledge.length; i++) {
      const content = knowledge[i].content.toLowerCase();
      const hits = INDICATORS.patternPhrases.filter(p => content.includes(p));
      if (hits.length > 0) {
        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType: 'pattern_discovery',
          startMessageId: knowledge[0].messageId,
          endMessageId: knowledge[i].messageId,
          description: `Pattern recognized: ${hits.join(', ')}`,
          breakthroughMoment: knowledge[i].content.slice(0, 200),
          confidence: Math.min(0.5 + hits.length * 0.15, 1.0),
          createdAt: Date.now(),
        });
      }
    }
    return arcs;
  }

  private detectAbstractionArcs(sessionId: string, knowledge: KnowledgeEntry[]): ThinkingArc[] {
    const arcs: ThinkingArc[] = [];
    for (let i = 2; i < knowledge.length; i++) {
      const content = knowledge[i].content.toLowerCase();
      const absHits = INDICATORS.abstractionPhrases.filter(p => content.includes(p));
      const synHits = INDICATORS.synthesisPhrases.filter(p => content.includes(p));
      const total = absHits.length + synHits.length;
      if (total > 0) {
        const arcType = synHits.length > absHits.length ? 'synthesis' : 'concrete_to_abstract';
        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType,
          startMessageId: knowledge[0].messageId,
          endMessageId: knowledge[i].messageId,
          description: arcType === 'synthesis'
            ? `Synthesis: ${synHits.join(', ')}`
            : `Abstraction: ${absHits.join(', ')}`,
          breakthroughMoment: knowledge[i].content.slice(0, 200),
          confidence: Math.min(0.5 + total * 0.12, 1.0),
          createdAt: Date.now(),
        });
      }
    }
    return arcs;
  }

  private detectRefinementArcs(sessionId: string, knowledge: KnowledgeEntry[]): ThinkingArc[] {
    const arcs: ThinkingArc[] = [];
    for (let i = 1; i < knowledge.length; i++) {
      const content = knowledge[i].content.toLowerCase();
      const hits = INDICATORS.refinementPhrases.filter(p => content.includes(p));
      if (hits.length > 0) {
        arcs.push({
          id: `arc_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
          sessionId,
          arcType: 'refinement',
          startMessageId: knowledge[i - 1].messageId,
          endMessageId: knowledge[i].messageId,
          description: `Refinement: ${hits.join(', ')}`,
          breakthroughMoment: knowledge[i].content.slice(0, 200),
          confidence: Math.min(0.4 + hits.length * 0.15, 1.0),
          createdAt: Date.now(),
        });
      }
    }
    return arcs;
  }

  private async storeArc(arc: ThinkingArc): Promise<void> {
    await this.storage.execute(
      `INSERT INTO thinking_arcs (id, session_id, arc_type, start_message_id, end_message_id,
                                    description, breakthrough_moment, confidence, created_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(id) DO UPDATE SET
              confidence = excluded.confidence,
              description = excluded.description`,
      [
        arc.id, arc.sessionId, arc.arcType, arc.startMessageId, arc.endMessageId,
        arc.description, arc.breakthroughMoment, arc.confidence, arc.createdAt,
      ]
    );
  }

  async getSessionArcs(sessionId: string): Promise<ThinkingArc[]> {
    const result = await this.storage.execute(
      `SELECT id, session_id, arc_type, start_message_id, end_message_id,
                   description, breakthrough_moment, confidence, created_at
            FROM thinking_arcs WHERE session_id = ? ORDER BY created_at ASC`,
      [sessionId]
    );

    const colIdx = (name: string) => result.columns.indexOf(name);
    return result.rows.map(row => ({
      id: row[colIdx('id')] as string,
      sessionId: row[colIdx('session_id')] as string,
      arcType: row[colIdx('arc_type')] as ArcType,
      startMessageId: row[colIdx('start_message_id')] as string,
      endMessageId: row[colIdx('end_message_id')] as string | null,
      description: row[colIdx('description')] as string,
      breakthroughMoment: row[colIdx('breakthrough_moment')] as string | null,
      confidence: row[colIdx('confidence')] as number,
      createdAt: row[colIdx('created_at')] as number,
    }));
  }
}
