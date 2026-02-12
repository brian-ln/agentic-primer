/**
 * Confidence Decay Calculator
 * Epic: agentic-primer-9ad
 * Phase: Cognitive Integration v1
 *
 * Calculates domain-specific confidence decay for knowledge items.
 * Different domains of knowledge age differently:
 *
 * - Tech: Exponential decay, 6-12 month half-life (frameworks, APIs change fast)
 * - Science: Power law decay, 5-10 year half-life (fundamental knowledge is stable)
 * - News: Exponential decay, 1-3 month half-life (current events become history)
 * - Core: Stepped decay, version-based (programming fundamentals, stable until paradigm shift)
 */

export type Domain = 'tech' | 'science' | 'news' | 'core';

export interface DecayConfig {
  domain: Domain;
  halfLifeMs: number; // Time for confidence to drop to 50%
  decayFunction: 'exponential' | 'power-law' | 'stepped';
  minConfidence: number; // Never decay below this value
}

const DECAY_CONFIGS: Record<Domain, DecayConfig> = {
  tech: {
    domain: 'tech',
    halfLifeMs: 9 * 30 * 24 * 60 * 60 * 1000, // 9 months (average of 6-12)
    decayFunction: 'exponential',
    minConfidence: 0.2 // Tech knowledge rarely becomes completely useless
  },
  science: {
    domain: 'science',
    halfLifeMs: 7.5 * 365 * 24 * 60 * 60 * 1000, // 7.5 years (average of 5-10)
    decayFunction: 'power-law',
    minConfidence: 0.4 // Scientific knowledge tends to be foundational
  },
  news: {
    domain: 'news',
    halfLifeMs: 2 * 30 * 24 * 60 * 60 * 1000, // 2 months (average of 1-3)
    decayFunction: 'exponential',
    minConfidence: 0.1 // News becomes historical context
  },
  core: {
    domain: 'core',
    halfLifeMs: 5 * 365 * 24 * 60 * 60 * 1000, // 5 years (stepped at major shifts)
    decayFunction: 'stepped',
    minConfidence: 0.6 // Core knowledge is fundamental
  }
};

export class ConfidenceDecay {
  /**
   * Calculate confidence decay for a knowledge item
   *
   * @param baseConfidence - Initial confidence (0.0 - 1.0)
   * @param ageMs - Age in milliseconds
   * @param domain - Knowledge domain
   * @returns Current confidence after decay (0.0 - 1.0)
   */
  calculateDecay(baseConfidence: number, ageMs: number, domain: string): number {
    // Validate inputs
    if (baseConfidence < 0 || baseConfidence > 1) {
      throw new Error(`Invalid base confidence: ${baseConfidence}. Must be between 0 and 1.`);
    }

    if (ageMs < 0) {
      return baseConfidence; // Future knowledge? Return as-is
    }

    // Get decay configuration for domain
    const config = DECAY_CONFIGS[domain as Domain];
    if (!config) {
      // Unknown domain - use conservative decay (similar to 'core')
      return this.applySteppedDecay(baseConfidence, ageMs, DECAY_CONFIGS.core);
    }

    // Apply domain-specific decay function
    let decayedConfidence: number;
    switch (config.decayFunction) {
      case 'exponential':
        decayedConfidence = this.applyExponentialDecay(baseConfidence, ageMs, config);
        break;
      case 'power-law':
        decayedConfidence = this.applyPowerLawDecay(baseConfidence, ageMs, config);
        break;
      case 'stepped':
        decayedConfidence = this.applySteppedDecay(baseConfidence, ageMs, config);
        break;
      default:
        decayedConfidence = baseConfidence;
    }

    // Ensure we don't go below minimum confidence
    return Math.max(decayedConfidence, config.minConfidence);
  }

  /**
   * Exponential decay: C(t) = C₀ * 0.5^(t / halfLife)
   *
   * Used for tech and news - rapid initial decay, asymptotic approach to minimum
   */
  private applyExponentialDecay(baseConfidence: number, ageMs: number, config: DecayConfig): number {
    const halfLives = ageMs / config.halfLifeMs;
    return baseConfidence * Math.pow(0.5, halfLives);
  }

  /**
   * Power law decay: C(t) = C₀ / (1 + (t / halfLife)^α)
   *
   * Used for science - slower decay, more gradual over time
   * α = 1.5 gives good balance (faster than linear, slower than exponential)
   */
  private applyPowerLawDecay(baseConfidence: number, ageMs: number, config: DecayConfig): number {
    const alpha = 1.5;
    const normalizedAge = ageMs / config.halfLifeMs;
    return baseConfidence / (1 + Math.pow(normalizedAge, alpha));
  }

  /**
   * Stepped decay: Discrete drops at major shifts
   *
   * Used for core knowledge - stays stable until paradigm shifts
   * Steps at 0 (100%), 2yr (90%), 5yr (70%), 10yr (50%), 20yr (30%)
   */
  private applySteppedDecay(baseConfidence: number, ageMs: number, config: DecayConfig): number {
    const ageYears = ageMs / (365 * 24 * 60 * 60 * 1000);

    let decayFactor: number;
    if (ageYears < 2) {
      decayFactor = 1.0; // No decay for first 2 years
    } else if (ageYears < 5) {
      decayFactor = 0.9; // 10% decay 2-5 years
    } else if (ageYears < 10) {
      decayFactor = 0.7; // 30% decay 5-10 years
    } else if (ageYears < 20) {
      decayFactor = 0.5; // 50% decay 10-20 years
    } else {
      decayFactor = 0.3; // 70% decay after 20 years
    }

    return baseConfidence * decayFactor;
  }

  /**
   * Get decay rate (derivative) at a specific age
   *
   * Returns how fast confidence is decaying at this moment
   * Useful for understanding which knowledge is aging fastest
   */
  getDecayRate(baseConfidence: number, ageMs: number, domain: string): number {
    const config = DECAY_CONFIGS[domain as Domain];
    if (!config) {
      return 0; // Unknown domain - no decay
    }

    // Calculate decay rate based on function type
    switch (config.decayFunction) {
      case 'exponential': {
        // dC/dt = -C * ln(2) / halfLife
        const currentConfidence = this.calculateDecay(baseConfidence, ageMs, domain);
        return -currentConfidence * Math.LN2 / config.halfLifeMs;
      }
      case 'power-law': {
        // dC/dt = -C₀ * α * (t/halfLife)^(α-1) / (halfLife * (1 + (t/halfLife)^α)^2)
        const alpha = 1.5;
        const normalizedAge = ageMs / config.halfLifeMs;
        const numerator = -baseConfidence * alpha * Math.pow(normalizedAge, alpha - 1);
        const denominator = config.halfLifeMs * Math.pow(1 + Math.pow(normalizedAge, alpha), 2);
        return numerator / denominator;
      }
      case 'stepped': {
        // Stepped functions have zero derivative except at discontinuities
        return 0;
      }
      default:
        return 0;
    }
  }

  /**
   * Get configuration for a domain
   */
  getConfig(domain: string): DecayConfig | undefined {
    return DECAY_CONFIGS[domain as Domain];
  }

  /**
   * Get all available domains
   */
  getDomains(): Domain[] {
    return Object.keys(DECAY_CONFIGS) as Domain[];
  }

  /**
   * Estimate when knowledge will reach a target confidence
   *
   * Returns time in milliseconds from now
   */
  estimateTimeToConfidence(baseConfidence: number, targetConfidence: number, domain: string): number | null {
    const config = DECAY_CONFIGS[domain as Domain];
    if (!config) {
      return null;
    }

    // Can't reach target if it's below minimum
    if (targetConfidence < config.minConfidence) {
      return null;
    }

    // Can't reach target if it's above base
    if (targetConfidence > baseConfidence) {
      return null;
    }

    // Solve for time based on decay function
    switch (config.decayFunction) {
      case 'exponential': {
        // C(t) = C₀ * 0.5^(t / halfLife)
        // t = halfLife * log(C(t) / C₀) / log(0.5)
        return config.halfLifeMs * Math.log(targetConfidence / baseConfidence) / Math.log(0.5);
      }
      case 'power-law': {
        // C(t) = C₀ / (1 + (t / halfLife)^α)
        // (t / halfLife)^α = (C₀ / C(t)) - 1
        // t = halfLife * ((C₀ / C(t)) - 1)^(1/α)
        const alpha = 1.5;
        return config.halfLifeMs * Math.pow((baseConfidence / targetConfidence) - 1, 1 / alpha);
      }
      case 'stepped': {
        // For stepped decay, find the step threshold
        const ratio = targetConfidence / baseConfidence;
        if (ratio >= 1.0) return 0;
        if (ratio >= 0.9) return 2 * 365 * 24 * 60 * 60 * 1000; // 2 years
        if (ratio >= 0.7) return 5 * 365 * 24 * 60 * 60 * 1000; // 5 years
        if (ratio >= 0.5) return 10 * 365 * 24 * 60 * 60 * 1000; // 10 years
        if (ratio >= 0.3) return 20 * 365 * 24 * 60 * 60 * 1000; // 20 years
        return null; // Beyond last step
      }
      default:
        return null;
    }
  }
}
