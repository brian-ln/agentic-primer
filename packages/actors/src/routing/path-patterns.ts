/**
 * Path Patterns - Advanced pattern matching for hierarchical paths
 *
 * Supports *, **, and {a,b} alternatives with validation.
 * Ported from simplify/src/messaging/path-patterns.ts
 */

import { parsePath } from './path-resolver.ts';

export class PatternError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'PatternError';
  }
}

export interface PatternOptions {
  caseSensitive?: boolean;
}

/** Validate a pattern string. Throws PatternError on invalid input. */
export function validatePattern(pattern: string): void {
  if (typeof pattern !== 'string') {
    throw new PatternError('Pattern must be a string');
  }

  if (pattern.length === 0) {
    return;
  }

  let braceDepth = 0;
  let inBraces = false;
  let braceStart = -1;

  for (let i = 0; i < pattern.length; i++) {
    const char = pattern[i];
    if (char === '{') {
      if (inBraces) {
        throw new PatternError(`Nested braces not supported at position ${i}`);
      }
      braceDepth++;
      inBraces = true;
      braceStart = i;
    } else if (char === '}') {
      braceDepth--;
      inBraces = false;
      if (braceDepth < 0) {
        throw new PatternError(`Unmatched closing brace at position ${i}`);
      }
      if (i === braceStart + 1) {
        throw new PatternError(`Empty braces at position ${i}`);
      }
    }
  }

  if (braceDepth !== 0) {
    throw new PatternError('Unclosed brace in pattern');
  }

  const segments = pattern.split('/');
  for (const segment of segments) {
    if (segment.includes('***')) {
      throw new PatternError(`Invalid wildcard '***' in segment: ${segment}`);
    }
    if (segment.includes('*') && segment !== '*' && segment !== '**') {
      if (!segment.startsWith('*') && !segment.endsWith('*')) {
        throw new PatternError(`Invalid wildcard usage in segment: ${segment}`);
      }
    }
    if (segment.includes('**') && segment !== '**') {
      throw new PatternError('Wildcard ** must be alone in segment');
    }
  }

  if (segments.length > 10) {
    throw new PatternError('Pattern too deep (max 10 segments)');
  }

  const alternativeCounts = segments.map(seg => {
    const match = seg.match(/\{([^}]+)\}/);
    return match ? match[1].split(',').length : 0;
  });
  const maxAlternatives = Math.max(...alternativeCounts);
  if (maxAlternatives > 5) {
    throw new PatternError('Too many alternatives in segment (max 5)');
  }

  const totalExpansions = alternativeCounts.reduce(
    (acc, count) => acc * Math.max(1, count),
    1
  );
  if (totalExpansions > 100) {
    throw new PatternError(
      `Pattern expansion too large (${totalExpansions} combinations, max 100)`
    );
  }
}

/** Expand {a,b} alternatives into multiple patterns. */
export function expandAlternatives(pattern: string): string[] {
  validatePattern(pattern);

  const segments = pattern.split('/');
  const expandedSegments: string[][] = [];

  for (const segment of segments) {
    const match = segment.match(/\{([^}]+)\}/);
    if (match) {
      expandedSegments.push(match[1].split(',').map(alt => alt.trim()));
    } else {
      expandedSegments.push([segment]);
    }
  }

  function cartesianProduct(arrays: string[][]): string[][] {
    if (arrays.length === 0) return [[]];
    if (arrays.length === 1) return arrays[0].map(item => [item]);
    const [first, ...rest] = arrays;
    const restProduct = cartesianProduct(rest);
    return first.flatMap(item =>
      restProduct.map(restItem => [item, ...restItem])
    );
  }

  return cartesianProduct(expandedSegments).map(combo => combo.join('/'));
}

/** Match path segments against pattern segments (internal). */
function matchSegments(pathSegments: string[], patternSegments: string[]): boolean {
  let pathIndex = 0;
  let patternIndex = 0;

  while (pathIndex < pathSegments.length || patternIndex < patternSegments.length) {
    if (pathIndex >= pathSegments.length) {
      const remaining = patternSegments.slice(patternIndex);
      return remaining.length === 1 && remaining[0] === '**';
    }

    if (patternIndex >= patternSegments.length) {
      return false;
    }

    const pathSeg = pathSegments[pathIndex];
    const patternSeg = patternSegments[patternIndex];

    if (patternSeg === '**') {
      if (patternIndex === patternSegments.length - 1) {
        return true;
      }
      for (let i = pathIndex; i <= pathSegments.length; i++) {
        if (matchSegments(pathSegments.slice(i), patternSegments.slice(patternIndex + 1))) {
          return true;
        }
      }
      return false;
    } else if (patternSeg === '*') {
      pathIndex++;
      patternIndex++;
    } else if (patternSeg === pathSeg) {
      pathIndex++;
      patternIndex++;
    } else {
      return false;
    }
  }

  return pathIndex === pathSegments.length && patternIndex === patternSegments.length;
}

/** Match a path against a pattern with wildcards and alternatives. */
export function matchPattern(
  path: string,
  pattern: string,
  options: PatternOptions = {}
): boolean {
  const caseSensitive = options.caseSensitive ?? true;
  const normalizedPath = caseSensitive ? path : path.toLowerCase();
  const normalizedPattern = caseSensitive ? pattern : pattern.toLowerCase();

  validatePattern(normalizedPattern);
  const expandedPatterns = expandAlternatives(normalizedPattern);

  for (const expandedPattern of expandedPatterns) {
    if (matchSegments(parsePath(normalizedPath), parsePath(expandedPattern))) {
      return true;
    }
  }

  return false;
}

/** Compile a pattern to a reusable predicate function. */
export function compilePattern(
  pattern: string,
  options: PatternOptions = {}
): (path: string) => boolean {
  const caseSensitive = options.caseSensitive ?? true;
  const normalizedPattern = caseSensitive ? pattern : pattern.toLowerCase();
  validatePattern(normalizedPattern);
  const expandedPatterns = expandAlternatives(normalizedPattern);
  const compiledSegments = expandedPatterns.map(p => parsePath(p));

  return (path: string) => {
    const normalizedPath = caseSensitive ? path : path.toLowerCase();
    const pathSegments = parsePath(normalizedPath);
    for (const patternSegments of compiledSegments) {
      if (matchSegments(pathSegments, patternSegments)) {
        return true;
      }
    }
    return false;
  };
}

/** Filter paths by pattern. */
export function filterPaths(
  paths: string[],
  pattern: string,
  options: PatternOptions = {}
): string[] {
  return paths.filter(compilePattern(pattern, options));
}

/** Check if pattern contains wildcards (* or {). */
export function hasWildcards(pattern: string): boolean {
  return pattern.includes('*') || pattern.includes('{');
}

/** Check if pattern contains alternatives ({a,b}). */
export function hasAlternatives(pattern: string): boolean {
  return pattern.includes('{') && pattern.includes('}');
}

/** Estimate pattern complexity (0-100). */
export function estimateComplexity(pattern: string): number {
  const segments = pattern.split('/');
  let complexity = segments.length;
  complexity += (pattern.match(/\*/g) || []).length * 2;
  complexity += (pattern.match(/\*\*/g) || []).length * 5;
  const alternativesMatch = pattern.match(/\{([^}]+)\}/g);
  if (alternativesMatch) {
    for (const match of alternativesMatch) {
      complexity += match.split(',').length * 3;
    }
  }
  return Math.min(complexity, 100);
}
