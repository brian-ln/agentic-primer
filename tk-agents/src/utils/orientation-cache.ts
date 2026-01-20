/**
 * Orientation Cache - Smart caching for orientation data
 *
 * Reduces repeated file reads with 5-minute TTL
 * Stores both task data and config data
 */

import type { Task } from "../cli/orient";

export interface OrientationCache {
  tasks: Task[];
  timestamp: number;
  autoMode?: {
    enabled: boolean;
    targetCapacity: number;
    minPriority: number;
  };
}

const CACHE_TTL_MS = 5 * 60 * 1000; // 5 minutes
let cachedData: OrientationCache | null = null;

/**
 * Check if cache is still valid
 */
export function isCacheValid(): boolean {
  if (!cachedData) return false;
  const age = Date.now() - cachedData.timestamp;
  return age < CACHE_TTL_MS;
}

/**
 * Get cached orientation data if valid
 */
export function getCached(): OrientationCache | null {
  return isCacheValid() ? cachedData : null;
}

/**
 * Update cache with new data
 */
export function updateCache(
  tasks: Task[],
  autoMode?: {
    enabled: boolean;
    targetCapacity: number;
    minPriority: number;
  }
): void {
  cachedData = {
    tasks,
    timestamp: Date.now(),
    autoMode,
  };
}

/**
 * Clear cache (force refresh on next orientation)
 */
export function clearCache(): void {
  cachedData = null;
}

/**
 * Get cache age in seconds (for debugging)
 */
export function getCacheAge(): number | null {
  if (!cachedData) return null;
  return Math.floor((Date.now() - cachedData.timestamp) / 1000);
}
