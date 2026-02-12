# Embedding Storage & Compression

**Status:** Research needed
**Current:** Base64 Float32 (4KB per 768-dim embedding)

## What We Know

### Storage Format Comparison (768 dimensions)

| Format | Size | JSON-safe | Notes |
|--------|------|-----------|-------|
| JSON string | 16KB | Yes | Original, wasteful |
| **Base64 Float32** | **4KB** | **Yes** | **Current implementation** |
| Base64 Float16 | 2KB | Yes | ~0.1% quality loss |
| Base64 Int8 | 1KB | Yes | ~1-2% quality loss, needs range mapping |
| gzip(binary)→base64 | 3.8KB | Yes | Only 6% savings, adds CPU |

### Why Compression Doesn't Help Much

Embeddings are high-entropy (pseudo-random normalized floats from neural networks). gzip relies on patterns/redundancy which embeddings lack.

- gzip(Float32): 2.9KB binary, but needs base64 → 3.8KB (only 6% better)
- Not worth CPU overhead on every read/write

### Quantization Options

**Float16 (half precision)**
- 50% size reduction
- Trivial quality loss (~0.1% on similarity)
- Simple: just truncate mantissa
- Bun/Node: Need to implement manually or use library

**Int8 (8-bit quantization)**
- 75% size reduction
- Small quality loss (~1-2% on similarity)
- Requires knowing value range (calibration)
- For normalized embeddings [-1, 1]: `int8 = round(float * 127)`

**Product Quantization (PQ)**
- Used by vector databases (Faiss, etc.)
- 10-50x compression possible
- Requires training codebook on representative data
- More complex implementation

## Research Questions

### Performance Benchmarks Needed

1. **Storage size at scale**
   - 1K, 10K, 100K embeddings
   - WAL file size
   - Snapshot file size

2. **Read/write latency**
   - Base64 encode/decode time
   - Float16 conversion time
   - Int8 quantize/dequantize time
   - gzip overhead

3. **Similarity accuracy**
   - Compare cosine similarity results across formats
   - Measure recall@k degradation with quantization
   - Test with real embedding model outputs

### Industry Practices to Research

- How do vector databases store embeddings? (Pinecone, Qdrant, Weaviate, Milvus)
- What compression do embedding APIs use? (OpenAI, Cohere)
- Binary quantization (1-bit) - when is it viable?
- Matryoshka embeddings (variable dimension)
- HNSW index storage formats

### Implementation Considerations

- Should format be configurable per-model?
- Migration path for existing embeddings
- Index compatibility (can we search quantized vectors?)
- Memory vs disk trade-offs

## Current Implementation

```typescript
// embedding.ts - Base64 Float32
private encodeEmbedding(embedding: number[]): string {
  const float32 = new Float32Array(embedding);
  return Buffer.from(float32.buffer).toString('base64');
}

private decodeEmbedding(base64: string, dimensions: number): number[] {
  const buffer = Buffer.from(base64, 'base64');
  const float32 = new Float32Array(buffer.buffer, buffer.byteOffset, dimensions);
  return Array.from(float32);
}

// Stored properties:
// - embedding: base64 string
// - embeddingDimensions: number (768)
// - embeddingModel: string
// - embeddingTimestamp: number
```

## Decision

**Stay with Base64 Float32 for now.** It's:
- 4x better than JSON string
- Simple and debuggable
- No quality loss
- Easy to migrate from later

Revisit when:
- Storage becomes a bottleneck
- We have real usage patterns to benchmark
- We need to support very large embedding counts (100K+)
