#!/usr/bin/env bun
/**
 * SemanticMessageClassifier - Stage 2 LLM-based classification
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.4
 *
 * Uses LocalLLMClient or CloudflareLLMClient to verify candidates and extract metadata.
 * Auto-detects which client to use based on environment variables.
 * Focused prompts based on candidate categories from Stage 1.
 * Extracts structured JSON with confidence scores and detailed metadata.
 */

import { LocalLLMClient, type ChatMessage } from './LocalLLMClient';
import { CloudflareLLMClient } from './CloudflareLLMClient';

/**
 * Base classification interface
 */
export interface BaseClassification {
  confidence: number; // 0.0 to 1.0
  metadata: Record<string, any>;
}

/**
 * Decision classification with choice, alternatives, and reasoning
 */
export interface DecisionClassification extends BaseClassification {
  isDecision: true;
  choice: string; // What was decided
  alternatives?: string[]; // Other options considered
  reasoning?: string; // Why this choice was made
}

/**
 * Learning classification with insight, category, evidence, and application
 */
export interface LearningClassification extends BaseClassification {
  isLearning: true;
  insight: string; // The key insight or learning
  category?: string; // 'technical', 'architectural', 'tooling', etc.
  evidence?: string; // Supporting evidence or context
  application?: string; // How this can be applied
}

/**
 * Error classification with type, message, resolution, and prevention
 */
export interface ErrorClassification extends BaseClassification {
  isError: true;
  type?: string; // Error type (e.g., 'TypeError', 'NetworkError')
  message: string; // Error message or description
  resolution?: string; // How it was resolved
  prevention?: string; // How to prevent in future
}

/**
 * Workflow classification with approach, effectiveness, and lessons
 */
export interface WorkflowClassification extends BaseClassification {
  isWorkflow: true;
  workflow_type: 'delegation' | 'organization' | 'planning' | 'collaboration' | 'tooling';
  description: string; // What workflow/approach was used
  effectiveness: 'effective' | 'ineffective' | 'mixed';
  context?: string; // When/why this approach was used
  tools_involved?: string[]; // Skills/tools used
  outcome?: string; // What resulted
  lessons?: string; // Key takeaways
}

/**
 * Union type for all classifications
 */
export type Classification =
  | DecisionClassification
  | LearningClassification
  | ErrorClassification
  | WorkflowClassification;

/**
 * Semantic message classifier using local LLM for Stage 2 verification
 *
 * Stage 1 (vector similarity) provides candidates, Stage 2 (LLM) verifies
 * and extracts detailed metadata with focused prompts per category.
 */
export class SemanticMessageClassifier {
  private llm: LocalLLMClient | CloudflareLLMClient;

  constructor(llm?: LocalLLMClient | CloudflareLLMClient) {
    if (llm) {
      this.llm = llm;
    } else {
      // Auto-detect: use Cloudflare if credentials available (supports /ai config vars)
      const hasCloudflare = (
        process.env.CLOUDFLARE_ACCOUNT_ID || process.env.CF_ACCOUNT_ID
      ) && (
        process.env.CLOUDFLARE_GATEWAY_ID || process.env.CF_GATEWAY_NAME
      ) && (
        process.env.CLOUDFLARE_API_TOKEN || process.env.CF_AIG_TOKEN
      );

      this.llm = hasCloudflare
        ? new CloudflareLLMClient()
        : new LocalLLMClient();
    }
  }

  /**
   * Batch classify decision messages
   *
   * Process multiple messages in a single LLM call
   * Returns array of classifications (null for non-decisions)
   */
  async classifyDecisionBatch(contents: string[]): Promise<(DecisionClassification | null)[]> {
    if (contents.length === 0) return [];

    const systemPrompt = `You are a decision classifier. Analyze multiple messages and determine which describe decisions being made.

A decision message includes:
- A clear choice or selection made
- Often mentions alternatives or options considered
- May include reasoning or justification

Extract structured information for each message that is a decision.`;

    const userPrompt = `Analyze these ${contents.length} messages and determine which describe decisions:

${contents.map((content, i) => `Message ${i}:
"""
${content}
"""
`).join('\n')}

Respond with a JSON array with exactly ${contents.length} elements, one for each message in order:
[
  {
    "isDecision": true/false,
    "confidence": 0.0-1.0,
    "choice": "what was decided (if isDecision=true)",
    "alternatives": ["alternative 1", "alternative 2"] or null,
    "reasoning": "why this was chosen" or null,
    "metadata": {}
  },
  ...
]`;

    const messages: ChatMessage[] = [
      { role: 'system', content: systemPrompt },
      { role: 'user', content: userPrompt }
    ];

    try {
      const results = await this.llm.chatJSON<Array<{
        isDecision: boolean;
        confidence: number;
        choice?: string;
        alternatives?: string[];
        reasoning?: string;
        metadata?: Record<string, any>;
      }>>(messages);

      return results.map(result => {
        if (!result.isDecision || !result.choice) {
          return null;
        }
        return {
          isDecision: true,
          confidence: result.confidence || 0.0,
          choice: result.choice,
          alternatives: result.alternatives,
          reasoning: result.reasoning,
          metadata: result.metadata || {}
        };
      });
    } catch (error) {
      console.error('Error in batch decision classification:', error);
      return contents.map(() => null);
    }
  }

  /**
   * Batch classify learning messages
   */
  async classifyLearningBatch(contents: string[]): Promise<(LearningClassification | null)[]> {
    if (contents.length === 0) return [];

    const systemPrompt = `You are a learning classifier. Analyze multiple messages and determine which describe learnings or insights.

A learning message includes:
- A key insight, discovery, or realization
- Often starts with "learned that", "discovered", "found that", "realized"
- May include evidence or supporting context
- May describe how to apply the learning`;

    const userPrompt = `Analyze these ${contents.length} messages and determine which describe learnings:

${contents.map((content, i) => `Message ${i}:
"""
${content}
"""
`).join('\n')}

Respond with a JSON array with exactly ${contents.length} elements:
[
  {
    "isLearning": true/false,
    "confidence": 0.0-1.0,
    "insight": "the key learning (if isLearning=true)",
    "category": "technical/architectural/tooling/process/other" or null,
    "evidence": "supporting evidence" or null,
    "application": "how this can be applied" or null,
    "metadata": {}
  },
  ...
]`;

    const messages: ChatMessage[] = [
      { role: 'system', content: systemPrompt },
      { role: 'user', content: userPrompt }
    ];

    try {
      const results = await this.llm.chatJSON<Array<{
        isLearning: boolean;
        confidence: number;
        insight?: string;
        category?: string;
        evidence?: string;
        application?: string;
        metadata?: Record<string, any>;
      }>>(messages);

      return results.map(result => {
        if (!result.isLearning || !result.insight) {
          return null;
        }
        return {
          isLearning: true,
          confidence: result.confidence || 0.0,
          insight: result.insight,
          category: result.category,
          evidence: result.evidence,
          application: result.application,
          metadata: result.metadata || {}
        };
      });
    } catch (error) {
      console.error('Error in batch learning classification:', error);
      return contents.map(() => null);
    }
  }

  /**
   * Batch classify error messages
   */
  async classifyErrorBatch(contents: string[]): Promise<(ErrorClassification | null)[]> {
    if (contents.length === 0) return [];

    const systemPrompt = `You are an error classifier. Analyze multiple messages and determine which describe errors or problems.

An error message includes:
- An error, exception, or problem description
- Often includes error type (TypeError, NetworkError, etc.)
- May include error message or stack trace
- May describe resolution or how to prevent`;

    const userPrompt = `Analyze these ${contents.length} messages and determine which describe errors:

${contents.map((content, i) => `Message ${i}:
"""
${content}
"""
`).join('\n')}

Respond with a JSON array with exactly ${contents.length} elements:
[
  {
    "isError": true/false,
    "confidence": 0.0-1.0,
    "type": "error type" or null,
    "message": "error description (if isError=true)",
    "resolution": "how it was resolved" or null,
    "prevention": "how to prevent" or null,
    "metadata": {}
  },
  ...
]`;

    const messages: ChatMessage[] = [
      { role: 'system', content: systemPrompt },
      { role: 'user', content: userPrompt }
    ];

    try {
      const results = await this.llm.chatJSON<Array<{
        isError: boolean;
        confidence: number;
        type?: string;
        message?: string;
        resolution?: string;
        prevention?: string;
        metadata?: Record<string, any>;
      }>>(messages);

      return results.map(result => {
        if (!result.isError || !result.message) {
          return null;
        }
        return {
          isError: true,
          confidence: result.confidence || 0.0,
          type: result.type,
          message: result.message,
          resolution: result.resolution,
          prevention: result.prevention,
          metadata: result.metadata || {}
        };
      });
    } catch (error) {
      console.error('Error in batch error classification:', error);
      return contents.map(() => null);
    }
  }

  /**
   * Batch classify workflow messages
   */
  async classifyWorkflowBatch(contents: string[]): Promise<(WorkflowClassification | null)[]> {
    if (contents.length === 0) return [];

    const systemPrompt = `You are a workflow pattern classifier. Analyze multiple messages to identify insights about work organization, task breakdown, collaboration, and process insights.

A workflow message describes:
- How work was organized (parallel, sequential, delegated)
- Task breakdown or planning strategies
- Collaboration with agents or background tasks
- Tool/skill usage patterns
- Process improvements or lessons learned`;

    const userPrompt = `Analyze these ${contents.length} messages for workflow insights:

${contents.map((content, i) => `Message ${i}:
"""
${content}
"""
`).join('\n')}

Respond with a JSON array with exactly ${contents.length} elements:
[
  {
    "isWorkflow": true/false,
    "confidence": 0.0-1.0,
    "workflow_type": "delegation|organization|planning|collaboration|tooling",
    "description": "concise description",
    "effectiveness": "effective|ineffective|mixed",
    "context": "when/why used",
    "tools_involved": ["bg", "reflect", ...],
    "outcome": "what resulted",
    "lessons": "key takeaways",
    "metadata": {}
  },
  ...
]`;

    const messages: ChatMessage[] = [
      { role: 'system', content: systemPrompt },
      { role: 'user', content: userPrompt }
    ];

    try {
      const results = await this.llm.chatJSON<Array<{
        isWorkflow: boolean;
        confidence: number;
        workflow_type?: 'delegation' | 'organization' | 'planning' | 'collaboration' | 'tooling';
        description?: string;
        effectiveness?: 'effective' | 'ineffective' | 'mixed';
        context?: string;
        tools_involved?: string[];
        outcome?: string;
        lessons?: string;
        metadata?: Record<string, any>;
      }>>(messages);

      return results.map(result => {
        if (!result.isWorkflow || !result.description || !result.workflow_type) {
          return null;
        }
        return {
          isWorkflow: true,
          confidence: result.confidence || 0.0,
          workflow_type: result.workflow_type,
          description: result.description,
          effectiveness: result.effectiveness || 'mixed',
          context: result.context,
          tools_involved: result.tools_involved,
          outcome: result.outcome,
          lessons: result.lessons,
          metadata: result.metadata || {}
        };
      });
    } catch (error) {
      console.error('Error in batch workflow classification:', error);
      return contents.map(() => null);
    }
  }

  /**
   * Check if LLM is available for classification
   */
  async isAvailable(): Promise<boolean> {
    return this.llm.isAvailable();
  }

  /**
   * Get all available models from LM Studio
   */
  async listModels(): Promise<string[]> {
    return this.llm.listModels();
  }
}
