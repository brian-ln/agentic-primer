#!/usr/bin/env bun
/**
 * KnowledgeActor - Graph-addressable epistemic knowledge management
 *
 * Handles knowledge items (decisions, learnings, errors) with epistemic levels.
 * All knowledge is addressable via @(knowledge/type/id) and interacts via messages.
 *
 * Examples:
 *   @(knowledge/decisions/abc-123)
 *   @(knowledge/learnings/def-456)
 *   @(knowledge/errors/xyz-789)
 */

import { Actor } from '../actor.ts';
import type { MessageRouter } from '../router.ts';
import type { Message, MessageResponse, Address } from '@agentic-primer/actors';
import { createResponse, createErrorResponse, parseAddress } from '@agentic-primer/actors';
import { LibSQLKnowledgeStore } from '../../storage/LibSQLKnowledgeStore.ts';

// Epistemic types (from epistemic gradients work)
export type EpistemicLevel = 'reject' | 'doubt' | 'wonder' | 'suspect' | 'believe' | 'know';

export const EPISTEMIC_CONFIDENCE_RANGES: Record<EpistemicLevel, { min: number; max: number }> = {
  reject: { min: 0.0, max: 0.20 },
  doubt: { min: 0.20, max: 0.40 },
  wonder: { min: 0.40, max: 0.60 },
  suspect: { min: 0.60, max: 0.80 },
  believe: { min: 0.80, max: 0.95 },
  know: { min: 0.95, max: 1.0 }
};

export type EvidenceType = 'MEASURED' | 'CALCULATED' | 'INFERRED' | 'CITED' |
                            'HYPOTHESIS' | 'SPECULATION' | 'VALIDATED' | 'EXPERIMENT';

export interface EvidenceLink {
  type: EvidenceType;
  source?: string;
  description: string;
  confidence?: number;
  timestamp?: number;
}

export interface KnowledgeItem {
  id: string;
  category: 'decision' | 'learning' | 'error';
  content: string;
  reasoning?: string;
  epistemic_level: EpistemicLevel;
  confidence: number;
  evidence: EvidenceLink[];
  created: number;
  last_validated?: number;
  session_id?: string;
}

/**
 * KnowledgeActor - Manages epistemic knowledge items
 */
export class KnowledgeActor extends Actor {
  private storage: LibSQLKnowledgeStore;

  constructor(id: string, router: MessageRouter, dbPath?: string) {
    super(id, router);
    this.storage = new LibSQLKnowledgeStore(dbPath);
  }

  /**
   * Handle incoming messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'create':
          return await this.handleCreate(message, payload);

        case 'get':
          return await this.handleGet(message, payload);

        case 'query':
          return await this.handleQuery(message, payload);

        case 'update':
          return await this.handleUpdate(message, payload);

        case 'add-evidence':
          return await this.handleAddEvidence(message, payload);

        case 'update-confidence':
          return await this.handleUpdateConfidence(message, payload);

        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message);
    }
  }

  /**
   * Create new knowledge item
   */
  private async handleCreate(message: Message, payload: any): Promise<MessageResponse> {
    const {
      category,
      content,
      reasoning,
      epistemic_level,
      confidence,
      evidence = [],
      session_id
    } = payload;

    // Validate required fields
    if (!category || !content || !epistemic_level) {
      return createErrorResponse(message, 'Missing required fields: category, content, epistemic_level');
    }

    // Validate epistemic level
    if (!EPISTEMIC_CONFIDENCE_RANGES[epistemic_level as EpistemicLevel]) {
      return createErrorResponse(message, `Invalid epistemic level: ${epistemic_level}`);
    }

    // Generate ID
    const id = this.generateId();
    const timestamp = Date.now();

    // Get confidence (use provided or midpoint of range)
    const range = EPISTEMIC_CONFIDENCE_RANGES[epistemic_level as EpistemicLevel];
    const finalConfidence = confidence ?? (range.min + range.max) / 2;

    // Validate confidence matches epistemic level
    if (finalConfidence < range.min || finalConfidence > range.max) {
      return createErrorResponse(
        message,
        `Confidence ${finalConfidence} outside ${epistemic_level} range (${range.min}-${range.max})`
      );
    }

    // Create knowledge item
    const item: KnowledgeItem = {
      id,
      category,
      content,
      reasoning,
      epistemic_level: epistemic_level as EpistemicLevel,
      confidence: finalConfidence,
      evidence,
      created: timestamp,
      last_validated: timestamp,
      session_id
    };

    await this.storage.createKnowledge(item);

    // Return address for the new knowledge item
    const knowledgeAddress = `@(knowledge/${category}/${id})` as Address;

    return createResponse(message, {
      address: knowledgeAddress,
      item
    });
  }

  /**
   * Get knowledge item by ID
   */
  private async handleGet(message: Message, payload: any): Promise<MessageResponse> {
    const { id } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    const item = await this.storage.getKnowledgeById(id);

    if (!item) {
      return createErrorResponse(message, `Knowledge item not found: ${id}`);
    }

    return createResponse(message, { item });
  }

  /**
   * Query knowledge items
   */
  private async handleQuery(message: Message, payload: any): Promise<MessageResponse> {
    const { filter = {}, limit = 100 } = payload;

    const results = await this.storage.queryKnowledge(filter, limit);

    return createResponse(message, {
      count: results.length,
      items: results
    });
  }

  /**
   * Update knowledge item
   */
  private async handleUpdate(message: Message, payload: any): Promise<MessageResponse> {
    const { id, updates } = payload;

    if (!id) {
      return createErrorResponse(message, 'Missing required field: id');
    }

    const item = await this.storage.getKnowledgeById(id);

    if (!item) {
      return createErrorResponse(message, `Knowledge item not found: ${id}`);
    }

    await this.storage.updateKnowledge(id, item.category, updates);

    // Get updated item to return
    const updatedItem = await this.storage.getKnowledgeById(id);

    return createResponse(message, { item: updatedItem });
  }

  /**
   * Add evidence to knowledge item
   */
  private async handleAddEvidence(message: Message, payload: any): Promise<MessageResponse> {
    const { id, evidence } = payload;

    if (!id || !evidence) {
      return createErrorResponse(message, 'Missing required fields: id, evidence');
    }

    const item = await this.storage.getKnowledgeById(id);

    if (!item) {
      return createErrorResponse(message, `Knowledge item not found: ${id}`);
    }

    // Add evidence to array
    const updatedEvidence = [
      ...item.evidence,
      {
        ...evidence,
        timestamp: evidence.timestamp || Date.now()
      }
    ];

    // Update with new evidence array
    await this.storage.updateKnowledge(id, item.category, {
      evidence: updatedEvidence
    });

    // Get updated item to return
    const updatedItem = await this.storage.getKnowledgeById(id);

    return createResponse(message, { item: updatedItem });
  }

  /**
   * Update confidence and potentially promote epistemic level
   */
  private async handleUpdateConfidence(message: Message, payload: any): Promise<MessageResponse> {
    const { id, newConfidence, reason } = payload;

    if (!id || newConfidence === undefined) {
      return createErrorResponse(message, 'Missing required fields: id, newConfidence');
    }

    const item = await this.storage.getKnowledgeById(id);

    if (!item) {
      return createErrorResponse(message, `Knowledge item not found: ${id}`);
    }

    const oldConfidence = item.confidence;
    item.confidence = newConfidence;

    // Auto-promote epistemic level if confidence crosses thresholds
    const promoted = this.autoPromoteEpistemicLevel(item);

    // Update storage with new confidence and potentially new epistemic level
    await this.storage.updateKnowledge(id, item.category, {
      confidence: item.confidence,
      epistemic_level: item.epistemic_level
    });

    // Get updated item to return
    const updatedItem = await this.storage.getKnowledgeById(id);

    return createResponse(message, {
      item: updatedItem,
      promoted,
      oldConfidence,
      newConfidence,
      reason
    });
  }

  /**
   * Auto-promote epistemic level based on confidence
   */
  private autoPromoteEpistemicLevel(item: KnowledgeItem): boolean {
    const currentRange = EPISTEMIC_CONFIDENCE_RANGES[item.epistemic_level];

    // If confidence is above current range, promote
    if (item.confidence > currentRange.max) {
      const levels: EpistemicLevel[] = ['reject', 'doubt', 'wonder', 'suspect', 'believe', 'know'];
      const currentIndex = levels.indexOf(item.epistemic_level);

      for (let i = currentIndex + 1; i < levels.length; i++) {
        const level = levels[i];
        const range = EPISTEMIC_CONFIDENCE_RANGES[level];

        if (item.confidence >= range.min && item.confidence <= range.max) {
          item.epistemic_level = level;
          return true;
        }
      }
    }

    return false;
  }

  /**
   * Generate unique ID for knowledge items
   */
  private generateId(): string {
    return `k_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`;
  }

  /**
   * Get statistics
   */
  async getStats() {
    // Query all knowledge items from storage
    const allItems = await this.storage.queryKnowledge({}, 10000);

    return {
      total: allItems.length,
      by_category: {
        decisions: allItems.filter(k => k.category === 'decision').length,
        learnings: allItems.filter(k => k.category === 'learning').length,
        errors: allItems.filter(k => k.category === 'error').length,
      },
      by_epistemic_level: {
        reject: allItems.filter(k => k.epistemic_level === 'reject').length,
        doubt: allItems.filter(k => k.epistemic_level === 'doubt').length,
        wonder: allItems.filter(k => k.epistemic_level === 'wonder').length,
        suspect: allItems.filter(k => k.epistemic_level === 'suspect').length,
        believe: allItems.filter(k => k.epistemic_level === 'believe').length,
        know: allItems.filter(k => k.epistemic_level === 'know').length,
      }
    };
  }
}

export default KnowledgeActor;
