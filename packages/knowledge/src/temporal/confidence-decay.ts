/**
 * Confidence Decay Calculator
 * Pure algorithm, no runtime-specific dependencies.
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
  halfLifeMs: number;
  decayFunction: 'exponential' | 'power-law' | 'stepped';
  minConfidence: number;
}

const DECAY_CONFIGS: Record<Domain, DecayConfig> = {
  tech: {
    domain: 'tech',
    halfLifeMs: 9 * 30 * 24 * 60 * 60 * 1000, // 9 months
    decayFunction: 'exponential',
    minConfidence: 0.2,
  },
  science: {
    domain: 'science',
    halfLifeMs: 7.5 * 365 * 24 * 60 * 60 * 1000, // 7.5 years
    decayFunction: 'power-law',
    minConfidence: 0.4,
  },
  news: {
    domain: 'news',
    halfLifeMs: 2 * 30 * 24 * 60 * 60 * 1000, // 2 months
    decayFunction: 'exponential',
    minConfidence: 0.1,
  },
  core: {
    domain: 'core',
    halfLifeMs: 5 * 365 * 24 * 60 * 60 * 1000, // 5 years
    decayFunction: 'stepped',
    minConfidence: 0.6,
  },
};

export class ConfidenceDecay {
  calculateDecay(baseConfidence: number, ageMs: number, domain: string): number {
    if (baseConfidence < 0 || baseConfidence > 1) {
      throw new Error(`Invalid base confidence: ${baseConfidence}. Must be between 0 and 1.`);
    }
    if (ageMs < 0) return baseConfidence;

    const config = DECAY_CONFIGS[domain as Domain];
    if (!config) {
      return this.applySteppedDecay(baseConfidence, ageMs, DECAY_CONFIGS.core);
    }

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

    return Math.max(decayedConfidence, config.minConfidence);
  }

  /** C(t) = C0 * 0.5^(t / halfLife) */
  private applyExponentialDecay(baseConfidence: number, ageMs: number, config: DecayConfig): number {
    const halfLives = ageMs / config.halfLifeMs;
    return baseConfidence * Math.pow(0.5, halfLives);
  }

  /** C(t) = C0 / (1 + (t / halfLife)^alpha), alpha = 1.5 */
  private applyPowerLawDecay(baseConfidence: number, ageMs: number, config: DecayConfig): number {
    const alpha = 1.5;
    const normalizedAge = ageMs / config.halfLifeMs;
    return baseConfidence / (1 + Math.pow(normalizedAge, alpha));
  }

  /** Discrete drops at paradigm shifts: 0-2yr 100%, 2-5yr 90%, 5-10yr 70%, 10-20yr 50%, 20+yr 30% */
  private applySteppedDecay(baseConfidence: number, ageMs: number, _config: DecayConfig): number {
    const ageYears = ageMs / (365 * 24 * 60 * 60 * 1000);
    let decayFactor: number;
    if (ageYears < 2) decayFactor = 1.0;
    else if (ageYears < 5) decayFactor = 0.9;
    else if (ageYears < 10) decayFactor = 0.7;
    else if (ageYears < 20) decayFactor = 0.5;
    else decayFactor = 0.3;
    return baseConfidence * decayFactor;
  }

  getDecayRate(baseConfidence: number, ageMs: number, domain: string): number {
    const config = DECAY_CONFIGS[domain as Domain];
    if (!config) return 0;

    switch (config.decayFunction) {
      case 'exponential': {
        const currentConfidence = this.calculateDecay(baseConfidence, ageMs, domain);
        return -currentConfidence * Math.LN2 / config.halfLifeMs;
      }
      case 'power-law': {
        const alpha = 1.5;
        const normalizedAge = ageMs / config.halfLifeMs;
        const numerator = -baseConfidence * alpha * Math.pow(normalizedAge, alpha - 1);
        const denominator = config.halfLifeMs * Math.pow(1 + Math.pow(normalizedAge, alpha), 2);
        return numerator / denominator;
      }
      case 'stepped':
        return 0;
      default:
        return 0;
    }
  }

  getConfig(domain: string): DecayConfig | undefined {
    return DECAY_CONFIGS[domain as Domain];
  }

  getDomains(): Domain[] {
    return Object.keys(DECAY_CONFIGS) as Domain[];
  }

  estimateTimeToConfidence(baseConfidence: number, targetConfidence: number, domain: string): number | null {
    const config = DECAY_CONFIGS[domain as Domain];
    if (!config) return null;
    if (targetConfidence < config.minConfidence) return null;
    if (targetConfidence > baseConfidence) return null;

    switch (config.decayFunction) {
      case 'exponential':
        return config.halfLifeMs * Math.log(targetConfidence / baseConfidence) / Math.log(0.5);
      case 'power-law': {
        const alpha = 1.5;
        return config.halfLifeMs * Math.pow((baseConfidence / targetConfidence) - 1, 1 / alpha);
      }
      case 'stepped': {
        const ratio = targetConfidence / baseConfidence;
        if (ratio >= 1.0) return 0;
        if (ratio >= 0.9) return 2 * 365 * 24 * 60 * 60 * 1000;
        if (ratio >= 0.7) return 5 * 365 * 24 * 60 * 60 * 1000;
        if (ratio >= 0.5) return 10 * 365 * 24 * 60 * 60 * 1000;
        if (ratio >= 0.3) return 20 * 365 * 24 * 60 * 60 * 1000;
        return null;
      }
      default:
        return null;
    }
  }
}
