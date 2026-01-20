/**
 * Quality Scoring Algorithms for Signals
 * Implements criteria from SIGNAL_QUALITY_FILTERING_DESIGN.md
 */

import type { YouTubeVideoMetadata, QualityScore, TechnicalVocab } from "./types.ts";

/**
 * Technical Vocabulary Dictionary
 */
const TECHNICAL_VOCAB: TechnicalVocab = {
  "ai-ml": [
    "transformer", "attention", "embedding", "fine-tuning", "rlhf",
    "prompt engineering", "few-shot", "zero-shot", "chain-of-thought",
    "agent", "agentic", "tool use", "function calling", "reasoning",
    "context window", "tokenizer", "inference", "latency", "throughput",
  ],
  "compilers": [
    "ast", "parser", "lexer", "semantic analysis", "code generation",
    "optimization", "register allocation", "control flow", "data flow",
    "static analysis", "type checking", "ir", "intermediate representation",
  ],
  "architecture": [
    "microservices", "event-driven", "actor model", "message passing",
    "distributed systems", "consensus", "eventual consistency", "cqrs",
    "saga pattern", "circuit breaker", "load balancing", "sharding",
  ],
};

/**
 * Relevant Topics and Keywords
 */
const RELEVANT_TOPICS: Record<string, { keywords: string[], weight: number }> = {
  "ai-agents": {
    keywords: ["agent", "agentic", "autonomous", "tool use", "multi-agent"],
    weight: 3,
  },
  "llm-architecture": {
    keywords: ["llm", "language model", "transformer", "attention", "embeddings"],
    weight: 3,
  },
  "compilers": {
    keywords: ["compiler", "optimization", "code generation", "ast", "parsing"],
    weight: 2,
  },
  "claude-specific": {
    keywords: ["claude", "anthropic", "sonnet", "opus", "haiku"],
    weight: 3,
  },
};

export class QualityScoring {
  /**
   * Calculate overall quality score
   */
  calculateScore(metadata: YouTubeVideoMetadata & { hasTranscript?: boolean }): { total: number, breakdown: any } {
    const depth = this.scoreContentDepth(metadata);
    const relevance = this.scoreRelevance(metadata);
    const production = this.scoreProductionQuality(metadata);
    const topic = this.detectPrimaryTopic(metadata);
    const recency = this.scoreRecency(metadata.publishedAt, topic);

    // Weighted composite score (0-10)
    const composite = (
      depth * 0.35 +
      relevance * 0.35 +
      production * 0.20 +
      (recency + 2) * 2.5 * 0.10 // Map -2..2 to 0-10
    );

    return {
      total: composite / 10, // Normalize to 0-1
      breakdown: {
        depth,
        relevance,
        production,
        recency,
        topic,
      }
    };
  }

  getCriteria() {
    return {
      topics: RELEVANT_TOPICS,
      vocab: TECHNICAL_VOCAB,
    };
  }

  private scoreContentDepth(metadata: YouTubeVideoMetadata): number {
    let score = 5; // Baseline

    // Duration scoring
    if (metadata.duration >= 900 && metadata.duration <= 7200) {
      score += 3; // 15min - 2hr is optimal
    } else if (metadata.duration >= 600 && metadata.duration <= 900) {
      score += 1;
    } else if (metadata.duration < 300) {
      score -= 3; // Too short
    }

    // technical vocabulary density in description
    const vocabDensity = this.analyzeTechnicalVocab(metadata.description || "");
    if (vocabDensity > 0.05) score += 2;
    else if (vocabDensity > 0.02) score += 1;

    return Math.max(0, Math.min(10, score));
  }

  private scoreRelevance(metadata: YouTubeVideoMetadata): number {
    let score = 0;
    const text = `${metadata.title} ${metadata.description}`.toLowerCase();

    for (const [topic, config] of Object.entries(RELEVANT_TOPICS)) {
      const matches = config.keywords.filter(kw => text.includes(kw)).length;
      if (matches > 0) {
        score += config.weight * (matches > 2 ? 2 : 1); 
      }
    }

    return Math.max(0, Math.min(10, score));
  }

  private scoreProductionQuality(metadata: YouTubeVideoMetadata & { hasTranscript?: boolean }): number {
    let score = 5;

    // Transcript availability is crucial
    if (metadata.hasTranscript) score += 3;
    else score -= 4;

    return Math.max(0, Math.min(10, score));
  }

  private analyzeTechnicalVocab(text: string): number {
    if (!text) return 0;
    const words = text.toLowerCase().split(/\s+/);
    if (words.length === 0) return 0;
    
    let technicalTerms = 0;
    for (const category of Object.values(TECHNICAL_VOCAB)) {
      for (const term of category) {
        if (text.toLowerCase().includes(term)) {
          technicalTerms++;
        }
      }
    }

    return technicalTerms / words.length;
  }

  private detectPrimaryTopic(metadata: YouTubeVideoMetadata): string {
    const text = `${metadata.title} ${metadata.description}`.toLowerCase();
    let bestTopic = "general";
    let maxScore = 0;

    for (const [topic, config] of Object.entries(RELEVANT_TOPICS)) {
      const count = config.keywords.filter(kw => text.includes(kw)).length;
      if (count > maxScore) {
        maxScore = count;
        bestTopic = topic;
      }
    }

    return bestTopic;
  }

  private scoreRecency(publishedAt: string, topic: string): number {
    const pubDate = new Date(publishedAt);
    const now = new Date();
    const diffMonths = (now.getFullYear() - pubDate.getFullYear()) * 12 + (now.getMonth() - pubDate.getMonth());

    if (["ai-agents", "llm-architecture", "claude-specific"].includes(topic)) {
      if (diffMonths < 3) return 2;
      if (diffMonths < 6) return 1;
      if (diffMonths > 12) return -2;
    }

    return 0;
  }
}