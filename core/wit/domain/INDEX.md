# Convergence Domain Model - File Index

Quick reference guide to all files in the domain model directory.

## Primary Files

### [domain.wit](./domain.wit)
**32 KB | 1,158 lines | WebAssembly Interface Types**

Complete WIT package defining the Convergence domain model.

**Quick Jump:**
- Line 1-159: Graph Primitives (address, node, edge, graph)
- Line 160-280: JSON and Entity interfaces
- Line 281-360: Agent interface
- Line 361-425: Task interface
- Line 426-530: Session interface
- Line 531-635: Human interface
- Line 636-710: Criteria interface
- Line 711-810: Query interface
- Line 811-880: Embedding interface
- Line 881-940: Convergence interface
- Line 941-1020: Session-state interface
- Line 1021-1100: Actor interface
- Line 1101-1158: Program interface + Worlds

**Key Sections:**
```
package convergence:domain@0.1.0

Interfaces:
  ├── Graph (4)       → address, node, edge, graph
  ├── Entities (6)    → json, entity, agent, task, session, human
  ├── Query (2)       → criteria, query
  ├── Knowledge (3)   → embedding, convergence, session-state
  └── Actor (2)       → actor, program

Worlds:
  ├── domain-graph
  ├── domain-entities
  ├── domain-query
  ├── domain-knowledge
  ├── domain-actors
  └── domain (complete)
```

### [domain.schema.json](./domain.schema.json)
**21 KB | 754 lines | JSON Schema Draft 07**

JSON Schema representation of the domain model for API validation.

**Structure:**
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "https://convergence.dev/schemas/domain/v0.1.0",
  "definitions": {
    // 57 type definitions
  }
}
```

**Definition Categories:**
- Core Types (10): address, node, edge, path, etc.
- Entity Types (15): entity-metadata, agent-config, task-spec, etc.
- Query Types (8): criterion-result, query-result, etc.
- Knowledge Types (6): embedding-model, convergence-detection, etc.
- Actor Types (8): actor-state, program-metadata, etc.
- Supporting Types (10): message-usage, notification, etc.

## Documentation Files

### [README.md](./README.md)
**11 KB | 510 lines | Primary Documentation**

Comprehensive guide to using both WIT and JSON Schema representations.

**Contents:**
1. **Overview** - What the domain model covers
2. **When to Use Each Format** - Decision guide
3. **Key Differences** - Comparison matrix
4. **Mapping Guide** - WIT ↔ JSON Schema conversions
5. **Special Cases** - Recursive types, resources, binary data
6. **Examples** - Real-world usage patterns
7. **Validation** - How to validate each format
8. **Integration Patterns** - Three architectural patterns
9. **Code Generation** - Generate bindings in 6+ languages
10. **Best Practices** - Maintenance and versioning

**Most Useful Sections:**
- Line 20-80: When to use WIT vs JSON Schema
- Line 81-150: Key differences table
- Line 151-350: Type mapping reference
- Line 351-450: Integration patterns
- Line 451-510: Best practices

### [VALIDATION.md](./VALIDATION.md)
**8.5 KB | 337 lines | Validation Report**

Validation results and compatibility verification.

**Contents:**
1. **WIT Validation** - wasm-tools results
2. **JSON Schema Validation** - Syntax verification
3. **Cross-Representation Validation** - Type mapping checks
4. **Test Cases** - 3 examples with results
5. **Issues and Resolutions** - Problems fixed during consolidation
6. **CI/CD Integration** - Automated validation steps

**Test Cases:**
- Test 1: Address serialization
- Test 2: Agent configuration
- Test 3: Variant handling

All tests: ✓ PASSED

### [SUMMARY.md](./SUMMARY.md)
**12 KB | 422 lines | Consolidation Summary**

Complete summary of the consolidation process and deliverables.

**Contents:**
1. **Overview** - High-level summary
2. **Deliverables** - All 5 files documented
3. **Source Files Consolidated** - Origin mapping
4. **Key Changes** - Fixes and modifications
5. **Statistics** - Metrics and counts
6. **Validation Summary** - All validation results
7. **Usage Examples** - Rust and TypeScript code
8. **Success Criteria** - All criteria met
9. **Next Steps** - Recommended actions
10. **Maintenance** - Version management plan

**Most Useful Sections:**
- Line 1-50: Quick overview of deliverables
- Line 150-200: Source file mapping
- Line 201-250: Key fixes and changes
- Line 380-450: Usage examples

## Supporting Files

### [examples.json](./examples.json)
**6.5 KB | 189 lines | Example Instances**

20+ JSON examples demonstrating the domain model.

**Example Categories:**
- Basic Types: address, node, edge
- Agent Examples: agent-config with full configuration
- Task Examples: task with success criteria
- Session Examples: messages, config
- Human Examples: config, approvals, notifications
- Knowledge Examples: similarity search, convergence detection
- Program Examples: metadata, invocation results

**Usage:**
```bash
# Validate an example
ajv validate -s domain.schema.json -d <(jq '.examples.agent_config' examples.json)
```

### [INDEX.md](./INDEX.md)
**This file | Quick Reference**

Navigation guide to all documentation files.

## Quick Start Guide

### For WebAssembly Component Development

1. Read: [domain.wit](./domain.wit)
2. Understand: [README.md](./README.md) - "Use WIT When" section
3. Validate: `wasm-tools component wit domain.wit`
4. Generate: `wit-bindgen rust --world domain domain.wit`
5. Examples: See SUMMARY.md usage examples

### For REST API Development

1. Read: [domain.schema.json](./domain.schema.json)
2. Understand: [README.md](./README.md) - "Use JSON Schema When" section
3. Validate: `ajv compile -s domain.schema.json`
4. Generate: `quicktype -s schema domain.schema.json -o types.ts`
5. Examples: [examples.json](./examples.json)

### For Understanding Type Mappings

1. Read: [README.md](./README.md) - "Mapping Guide" section
2. Check: [VALIDATION.md](./VALIDATION.md) - Test cases
3. Practice: Convert examples between formats

### For Integration Planning

1. Read: [README.md](./README.md) - "Integration Patterns" section
2. Review: [SUMMARY.md](./SUMMARY.md) - Usage examples
3. Plan: Choose pattern based on architecture

## File Relationships

```
domain.wit ─┬─ Defines types
            │
            ├─> domain.schema.json (Parallel representation)
            │   └─> examples.json (Validates against)
            │
            └─> Documentation
                ├─> README.md (Primary guide)
                ├─> VALIDATION.md (Verification)
                ├─> SUMMARY.md (Consolidation report)
                └─> INDEX.md (This file)
```

## Change History

### Version 0.1.0 (2026-02-05)
- Initial consolidation from 13 source files
- 17 interfaces across 5 categories
- 6 world definitions for composition
- 57 JSON Schema definitions
- Complete documentation suite
- Validated with wasm-tools and JSON parsers

## Maintenance

### When Making Changes

1. **Update both files:** domain.wit AND domain.schema.json
2. **Update examples:** Add new examples to examples.json
3. **Update docs:** Modify README.md if behavior changes
4. **Run validation:** Both WIT and JSON Schema
5. **Update VALIDATION.md:** Document new test cases
6. **Bump version:** Update both package versions together

### Version Bumping Rules

- **Patch (0.1.1):** Documentation fixes, examples
- **Minor (0.2.0):** New optional fields, new interfaces
- **Major (1.0.0):** Breaking changes to existing types

## Getting Help

### Common Questions

**Q: Which format should I use?**
A: See [README.md](./README.md) - "When to Use Each Format" section

**Q: How do I convert between formats?**
A: See [README.md](./README.md) - "Mapping Guide" section

**Q: Are the formats compatible?**
A: See [VALIDATION.md](./VALIDATION.md) - "Cross-Representation Validation"

**Q: How do I validate my data?**
A: See [README.md](./README.md) - "Validation" section

**Q: Where are usage examples?**
A: See [examples.json](./examples.json) and [SUMMARY.md](./SUMMARY.md)

### Additional Resources

- WebAssembly Component Model: https://github.com/WebAssembly/component-model
- WIT Format Spec: https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md
- JSON Schema Spec: https://json-schema.org/specification.html

## File Sizes and Stats

| File | Size | Lines | Description |
|------|------|-------|-------------|
| domain.wit | 32 KB | 1,158 | WIT package definition |
| domain.schema.json | 21 KB | 754 | JSON Schema definition |
| README.md | 11 KB | 510 | Primary documentation |
| SUMMARY.md | 12 KB | 422 | Consolidation summary |
| VALIDATION.md | 8.5 KB | 337 | Validation report |
| examples.json | 6.5 KB | 189 | Example instances |
| INDEX.md | 6 KB | ~200 | This navigation guide |
| **Total** | **97 KB** | **3,570** | **Complete documentation** |

## Quick Links

- **Start Here:** [README.md](./README.md)
- **WIT Definition:** [domain.wit](./domain.wit)
- **JSON Schema:** [domain.schema.json](./domain.schema.json)
- **Examples:** [examples.json](./examples.json)
- **Validation:** [VALIDATION.md](./VALIDATION.md)
- **Summary:** [SUMMARY.md](./SUMMARY.md)

---

**Last Updated:** 2026-02-05
**Version:** 0.1.0
**Status:** Production Ready
