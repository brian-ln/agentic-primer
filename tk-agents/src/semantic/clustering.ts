// Streaming DBSCAN Clustering for Semantic Prompt Analysis
// Implements incremental clustering as prompts arrive

export interface Point {
  id: string;
  embedding: number[];
  metadata: PromptMetadata;
}

export interface PromptMetadata {
  text: string;
  timestamp: string;
  interrupted: boolean;
  source?: string;
}

export interface Cluster {
  id: number;
  topicKeywords: string[];
  centroid: number[];
  points: Point[];
  createdAt: Date;
  lastActive: Date;
  similarityThreshold: number;
}

/**
 * Calculate cosine similarity between two vectors
 * Returns value between 0 (orthogonal) and 1 (identical)
 *
 * @param a - First embedding vector
 * @param b - Second embedding vector
 * @returns number - Cosine similarity (0-1)
 */
export function cosineSimilarity(a: number[], b: number[]): number {
  if (a.length !== b.length) {
    throw new Error("Vectors must have same dimensionality");
  }

  let dotProduct = 0;
  let magA = 0;
  let magB = 0;

  for (let i = 0; i < a.length; i++) {
    dotProduct += a[i] * b[i];
    magA += a[i] * a[i];
    magB += b[i] * b[i];
  }

  const magnitude = Math.sqrt(magA) * Math.sqrt(magB);
  if (magnitude === 0) {
    return 0;
  }

  return dotProduct / magnitude;
}

/**
 * Extract simple keywords from text using word frequency
 * TODO: Consider more sophisticated extraction (TF-IDF, RAKE)
 *
 * @param text - Input text
 * @param topN - Number of keywords to extract
 * @returns string[] - Top keywords
 */
function extractSimpleKeywords(text: string, topN: number = 5): string[] {
  // Simple stopwords list
  const stopwords = new Set([
    "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
    "of", "with", "by", "from", "as", "is", "was", "are", "were", "be",
    "been", "being", "have", "has", "had", "do", "does", "did", "will",
    "would", "should", "could", "may", "might", "can", "what", "how", "why",
    "when", "where", "who", "which", "this", "that", "these", "those", "i", "you"
  ]);

  // Tokenize and filter
  const words = text
    .toLowerCase()
    .replace(/[^\w\s]/g, " ")
    .split(/\s+/)
    .filter(w => w.length > 2 && !stopwords.has(w));

  // Count frequency
  const freq = new Map<string, number>();
  for (const word of words) {
    freq.set(word, (freq.get(word) || 0) + 1);
  }

  // Sort by frequency and return top N
  return Array.from(freq.entries())
    .sort((a, b) => b[1] - a[1])
    .slice(0, topN)
    .map(([word]) => word);
}

/**
 * Streaming DBSCAN Clustering
 * Incrementally assigns prompts to clusters as they arrive
 */
export class StreamingDBSCAN {
  private clusters: Map<number, Cluster> = new Map();
  private nextClusterId: number = 1;
  private epsilon: number; // Similarity threshold (default: 0.3, meaning ~0.7 cosine similarity)
  private minPoints: number; // Minimum cluster size

  constructor(epsilon: number = 0.3, minPoints: number = 2) {
    this.epsilon = epsilon;
    this.minPoints = minPoints;
  }

  /**
   * Add a new point and assign to cluster
   * Returns cluster ID
   *
   * @param embedding - Embedding vector
   * @param metadata - Prompt metadata
   * @returns Promise<number> - Cluster ID
   */
  async addPoint(embedding: number[], metadata: PromptMetadata): Promise<number> {
    const point: Point = {
      id: `point_${Date.now()}_${Math.random().toString(36).slice(2, 7)}`,
      embedding,
      metadata,
    };

    // Find most similar existing cluster
    let maxSimilarity = 0;
    let bestClusterId: number | null = null;

    for (const [clusterId, cluster] of this.clusters) {
      const similarity = cosineSimilarity(embedding, cluster.centroid);

      if (similarity > maxSimilarity) {
        maxSimilarity = similarity;
        bestClusterId = clusterId;
      }
    }

    // Assign to existing cluster if similarity exceeds threshold
    // Note: epsilon is distance threshold, so we use (1 - epsilon) for similarity
    const similarityThreshold = 1 - this.epsilon;

    if (maxSimilarity >= similarityThreshold && bestClusterId !== null) {
      this.addToCluster(bestClusterId, point);
      return bestClusterId;
    }

    // Create new cluster (topic shift detected)
    return this.createCluster(point);
  }

  /**
   * Add point to existing cluster
   * Updates centroid using online mean
   */
  private addToCluster(clusterId: number, point: Point): void {
    const cluster = this.clusters.get(clusterId);
    if (!cluster) {
      throw new Error(`Cluster ${clusterId} not found`);
    }

    // Update centroid (online mean)
    const n = cluster.points.length;
    cluster.centroid = cluster.centroid.map((v, i) =>
      (v * n + point.embedding[i]) / (n + 1)
    );

    cluster.points.push(point);
    cluster.lastActive = new Date();

    // Update topic keywords based on new point
    const newKeywords = extractSimpleKeywords(point.metadata.text);
    const allKeywords = [...cluster.topicKeywords, ...newKeywords];
    const keywordFreq = new Map<string, number>();
    for (const kw of allKeywords) {
      keywordFreq.set(kw, (keywordFreq.get(kw) || 0) + 1);
    }

    // Keep top 5 most frequent keywords
    cluster.topicKeywords = Array.from(keywordFreq.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 5)
      .map(([kw]) => kw);
  }

  /**
   * Create new cluster from point
   */
  private createCluster(point: Point): number {
    const clusterId = this.nextClusterId++;
    const keywords = extractSimpleKeywords(point.metadata.text);

    const cluster: Cluster = {
      id: clusterId,
      topicKeywords: keywords,
      centroid: [...point.embedding], // Copy embedding
      points: [point],
      createdAt: new Date(),
      lastActive: new Date(),
      similarityThreshold: 1 - this.epsilon,
    };

    this.clusters.set(clusterId, cluster);
    return clusterId;
  }

  /**
   * Get cluster by ID
   */
  getCluster(clusterId: number): Cluster | undefined {
    return this.clusters.get(clusterId);
  }

  /**
   * Get all clusters
   */
  getAllClusters(): Cluster[] {
    return Array.from(this.clusters.values());
  }

  /**
   * Find clusters by keyword
   */
  findClustersByKeyword(keyword: string): Cluster[] {
    const lowerKeyword = keyword.toLowerCase();
    return Array.from(this.clusters.values()).filter(cluster =>
      cluster.topicKeywords.some(kw => kw.includes(lowerKeyword))
    );
  }

  /**
   * Get cluster statistics
   */
  getStats(): {
    totalClusters: number;
    totalPoints: number;
    avgPointsPerCluster: number;
    activeClusters: number;
  } {
    const clusters = Array.from(this.clusters.values());
    const totalClusters = clusters.length;
    const totalPoints = clusters.reduce((sum, c) => sum + c.points.length, 0);
    const avgPointsPerCluster = totalClusters > 0 ? totalPoints / totalClusters : 0;

    // Active = updated in last 24 hours
    const oneDayAgo = new Date(Date.now() - 24 * 60 * 60 * 1000);
    const activeClusters = clusters.filter(c => c.lastActive > oneDayAgo).length;

    return {
      totalClusters,
      totalPoints,
      avgPointsPerCluster,
      activeClusters,
    };
  }

  /**
   * Expire old clusters (inactive for >24 hours)
   * Returns number of clusters expired
   */
  expireOldClusters(maxAgeMs: number = 24 * 60 * 60 * 1000): number {
    const cutoff = new Date(Date.now() - maxAgeMs);
    let expired = 0;

    for (const [clusterId, cluster] of this.clusters) {
      if (cluster.lastActive < cutoff) {
        this.clusters.delete(clusterId);
        expired++;
      }
    }

    return expired;
  }

  /**
   * Clear all clusters
   */
  clear(): void {
    this.clusters.clear();
    this.nextClusterId = 1;
  }
}
