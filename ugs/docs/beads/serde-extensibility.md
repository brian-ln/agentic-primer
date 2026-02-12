# Bead: SERDE Extensibility

**ID**: serde-ext
**Status**: Future consideration
**Created**: 2026-01-26

## Problem

Current graph persistence uses JSON exclusively for:
- WAL (Write-Ahead Log) events
- Snapshots
- Node/Edge serialization (toJSON)

JSON is important but not the only serialization format worth supporting.

## Considerations

### JSON Strengths
- Human readable
- Universal support
- Easy debugging
- Browser compatible

### Potential Alternatives
- **MessagePack**: Binary, faster, smaller
- **CBOR**: RFC 8949, binary JSON
- **Protocol Buffers**: Schema-driven, versioned
- **FlatBuffers**: Zero-copy access
- **BSON**: Binary JSON (MongoDB)

## Possible Approaches

1. **Codec interface**
   ```typescript
   interface Codec {
     encode(data: any): Uint8Array;
     decode(bytes: Uint8Array): any;
     contentType: string;
   }
   ```

2. **Per-stream codecs**: Different formats for WAL vs snapshots vs API

3. **Header-based versioning**: Magic bytes + version for format detection

## Questions to Answer

- What use cases need non-JSON?
- Performance requirements?
- Interop with external systems?
- Schema evolution strategy?

## Next Steps

When this becomes relevant:
1. Profile actual bottlenecks
2. Identify specific use case
3. Design codec abstraction
4. Keep JSON as default/fallback
