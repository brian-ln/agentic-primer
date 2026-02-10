/**
 * Path Resolver - Utilities for path-based addressing
 *
 * Path parsing, validation, normalization, and pattern matching
 * for hierarchical actor addressing. Pure string manipulation, no deps.
 *
 * Ported from simplify/src/messaging/path-resolver.ts
 */

/**
 * Parse a path string into segments.
 * Splits by '/' and filters empty segments.
 */
export function parsePath(path: string): string[] {
  if (!path || typeof path !== 'string') {
    return [];
  }
  return path.split('/').filter(segment => segment.length > 0);
}

/**
 * Validate a path string.
 *
 * Rejects: directory traversal (.., .), invalid chars, excessive depth (>10),
 * overly long segments (>100 chars).
 */
export function validatePath(path: string): boolean {
  if (!path || typeof path !== 'string') {
    return false;
  }

  const segments = parsePath(path);

  if (segments.length === 0) {
    return false;
  }

  for (const segment of segments) {
    if (segment.length > 100) {
      return false;
    }

    if (segment === '.' || segment === '..') {
      return false;
    }

    if (!/^[a-zA-Z0-9_.-]+$/.test(segment)) {
      return false;
    }

    if (/^\.+$/.test(segment)) {
      return false;
    }
  }

  if (segments.length > 10) {
    return false;
  }

  return true;
}

/**
 * Strict path safety check for production routing.
 * Additional checks: no dot-prefixed segments, max 500 chars total.
 */
export function isSafePath(path: string): boolean {
  if (!validatePath(path)) {
    return false;
  }

  if (path.length > 500) {
    return false;
  }

  const segments = parsePath(path);
  for (const segment of segments) {
    if (segment.startsWith('.')) {
      return false;
    }
  }

  return true;
}

/**
 * Normalize a path string.
 * Removes duplicate slashes, strips legacy 'root/' prefix.
 */
export function normalizePath(
  path: string,
  options: { lowercase?: boolean } = {}
): string {
  if (!path || typeof path !== 'string') {
    return '';
  }

  if (path.startsWith('./') || path.startsWith('../')) {
    return path;
  }

  let cleanPath = path;
  if (cleanPath.startsWith('root/')) {
    cleanPath = cleanPath.slice(5);
  }

  const segments = parsePath(cleanPath);
  let normalized = segments.join('/');

  if (options.lowercase) {
    normalized = normalized.toLowerCase();
  }

  if (normalized && !normalized.startsWith('/')) {
    normalized = '/' + normalized;
  }

  return normalized;
}

/**
 * Match path segments against pattern segments.
 * Supports * (one segment) and ** (zero or more segments).
 */
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

/**
 * Match a path against a pattern with wildcards.
 */
export function matchPattern(path: string, pattern: string): boolean {
  return matchSegments(parsePath(path), parsePath(pattern));
}

/** Get the parent path, or null if at root level. */
export function getParentPath(path: string): string | null {
  const segments = parsePath(path);
  if (segments.length <= 1) {
    return null;
  }
  return segments.slice(0, -1).join('/');
}

/** Get the last segment (local name) of a path. */
export function getLocalName(path: string): string | null {
  const segments = parsePath(path);
  if (segments.length === 0) {
    return null;
  }
  return segments[segments.length - 1];
}

/** Join path segments. */
export function joinPath(...segments: string[]): string {
  return segments
    .flatMap(seg => parsePath(seg))
    .join('/');
}

/** Check if childPath is under parentPath. */
export function isChildOf(childPath: string, parentPath: string): boolean {
  const childSegments = parsePath(childPath);
  const parentSegments = parsePath(parentPath);

  if (parentSegments.length === 0) {
    return false;
  }

  if (childSegments.length <= parentSegments.length) {
    return false;
  }

  for (let i = 0; i < parentSegments.length; i++) {
    if (childSegments[i] !== parentSegments[i]) {
      return false;
    }
  }

  return true;
}
