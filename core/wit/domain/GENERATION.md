# Domain Model Type Generation

This document describes the process of generating TypeScript types and Zod validators from the domain model JSON Schema.

## Overview

The domain model types and validators are automatically generated from `domain.schema.json` to ensure consistency between:
1. JSON Schema definitions (source of truth)
2. TypeScript type definitions (compile-time checking)
3. Zod validation schemas (runtime validation)
4. Example data (validation testing)

## Generated Files

| File | Purpose | Generated From |
|------|---------|----------------|
| `domain.types.ts` | TypeScript type definitions | `domain.schema.json` |
| `domain.validators.ts` | Zod validation schemas | `domain.schema.json` |
| `validation-test.ts` | Validation test suite | `examples.json` |

## Generation Process

### Prerequisites

```bash
pnpm install
```

This installs:
- `json-schema-to-typescript` - Converts JSON Schema to TypeScript types
- `zod` - Runtime validation library
- `typescript` - TypeScript compiler
- `tsx` - TypeScript execution engine

### Running the Generator

```bash
pnpm tsx scripts/generate-domain-types.ts
```

The generator performs the following steps:

1. **Read Schema**: Loads `domain.schema.json` (57 type definitions)
2. **Generate TypeScript Types**: Uses `json-schema-to-typescript` to create type definitions
3. **Topological Sort**: Orders schema definitions by dependencies to avoid circular reference errors
4. **Generate Zod Schemas**: Programmatically creates Zod validators matching JSON Schema rules
5. **Write Output**: Saves generated files to `core/wit/domain/`

### Validation Testing

```bash
pnpm tsx core/wit/domain/validation-test.ts
```

The validation test:
1. Loads all 19+ examples from `examples.json`
2. Validates each example against its corresponding Zod schema
3. Performs TypeScript compile-time type checking
4. Reports success rate and any validation errors

**Current Status**: ✓ 100% pass rate (19/19 examples)

## Schema Structure

The domain schema defines 57 types organized into these categories:

### Core Primitives
- `address` - Universal reference (@(id))
- `address-scope` - Node, edge, or computed scope
- `edge-ref` - Edge reference for scoped addresses
- `property-value` - Tagged union for property values
- `node-metadata` - Temporal metadata for nodes

### Graph Model
- `node` - Graph node with properties and metadata
- `edge` - Graph edge with source, target, and weight
- `path` - Path through graph with total weight
- `traversal-options` - Graph traversal configuration
- `direction` - Traversal direction enum

### Entities
- `entity-kind` - Agent, task, session, human, model, provider, program
- `entity-lifecycle` - Draft, published, deprecated
- `entity-metadata` - Common metadata for all entities

### Agent System
- `agent-state` - Idle, thinking, executing, waiting, completed, error
- `agent-config` - Agent configuration (name, prompt, tools, model)
- `agent-harness` - Execution harness settings
- `step-result` - Result of agent execution step

### Task Management
- `task-lifecycle` - Pending, assigned, in-progress, completed, failed
- `task-priority` - P0 through P4 priority levels
- `task-config` - Task configuration and metadata
- `task-spec` - Task specification (inputs, outputs, constraints, criteria)
- `success-criterion` - Success criteria definition

### Session Management
- `session-lifecycle` - Created, active, paused, completed
- `session-config` - Session configuration (owner, model, log)
- `session-message` - Chat message with role and usage
- `session-context` - Session context capture
- `message-role` - User or assistant
- `message-usage` - Token usage tracking

### Human Interaction
- `human-state` - Available, busy, away, offline
- `human-config` - Human user configuration
- `human-preferences` - Notification and timezone preferences
- `human-permission` - Approve, assign, configure, admin
- `approval-request` - Approval request with status
- `notification` - Notification with channel and read status

### Query & Search
- `query-result` - Query result with bindings and stats
- `query-stats` - Query execution statistics
- `similarity-options` - Similarity search options
- `similarity-result` - Similarity search result with score
- `embedding-model` - Embedding model configuration

### Convergence Detection
- `convergence-detection` - Convergence detection result
- `convergence-strength` - Weak, moderate, strong
- `knowledge-artifact` - Knowledge artifact from convergence

### Program Execution
- `program-metadata` - Program metadata and configuration
- `program-runtime` - JavaScript, Python, WASM, native, container, custom
- `program-state` - Draft, published, deprecated
- `execution-mode` - Inline, worker, subprocess, container
- `invocation-result` - Program invocation result

### Actor Model
- `actor-state` - Initializing, ready, active, paused, stopping, stopped, error
- `message-pattern` - Tell, ask, stream-msg
- `supervision-strategy` - Restart, stop, resume, escalate
- `node-type` - Producer, consumer, relay, processor, hybrid

## Type Generation Details

### TypeScript Types

Generated using `json-schema-to-typescript` with these options:
- `strictIndexSignatures: true` - Strict object typing
- `enableConstEnums: true` - Use const enums where possible
- `unknownAny: false` - No `any` types
- `declareExternallyReferenced: false` - Inline all types

### Zod Validators

Programmatically generated to match JSON Schema validation rules:

| JSON Schema | Zod Equivalent |
|-------------|----------------|
| `type: "string"` | `z.string()` |
| `type: "number"` | `z.number()` |
| `type: "integer"` | `z.number().int()` |
| `type: "boolean"` | `z.boolean()` |
| `type: "null"` | `z.null()` |
| `type: "array"` | `z.array(...)` |
| `type: "object"` | `z.object({...})` |
| `enum: [...]` | `z.enum([...])` |
| `oneOf: [...]` | `z.union([...])` |
| `$ref: "#/definitions/x"` | `xSchema` |
| `format: "email"` | `z.string().email()` |
| `format: "uuid"` | `z.string().uuid()` |
| `format: "date-time"` | `z.string().datetime()` |
| `minimum: n` | `z.number().min(n)` |
| `maximum: n` | `z.number().max(n)` |
| `additionalProperties: false` | `z.object({...}).strict()` |
| `type: ["string", "null"]` | `z.string().nullable()` |
| `required: [...]` | Fields without `.optional()` |

### Dependency Ordering

The generator uses topological sorting to order schema definitions by dependencies:

1. Extract all `$ref` references from each schema
2. Build dependency graph
3. Use Kahn's algorithm for topological sort
4. Handle cycles by adding remaining nodes at the end
5. Generate schemas in dependency order to avoid "cannot access before initialization" errors

## Known Issues and Limitations

### Circular References
Some schemas have circular references (e.g., `property-value` → `address` → `address-scope` → `edge-ref` references `edge` which references `address`). These are handled by:
1. Topological sorting to minimize forward references
2. Adding cyclic nodes at the end of the generation order
3. Future: Use `z.lazy()` for true circular references if needed

### Tuple Arrays
JSON Schema tuple arrays with `items: [type1, type2]` are correctly mapped to `z.tuple([schema1, schema2])`.

### Format Validation
- `format: "email"` → `.email()`
- `format: "uuid"` → `.uuid()`
- `format: "date-time"` → `.datetime()`

Custom formats are not yet supported.

### Binary Data
The `invocation-result.output` field is defined as a byte array (`array of integers 0-255`). This is represented as `number[]` in TypeScript with Zod validation ensuring values are integers in the 0-255 range.

## Maintenance

### When to Regenerate

Regenerate types and validators whenever:
1. `domain.schema.json` is modified
2. New schema definitions are added
3. Existing definitions are changed
4. Validation rules are updated

### Validation Workflow

1. Modify `domain.schema.json`
2. Update `examples.json` with new/modified examples
3. Run generator: `pnpm tsx scripts/generate-domain-types.ts`
4. Run validation: `pnpm tsx core/wit/domain/validation-test.ts`
5. Fix any validation errors
6. Commit all changes together

### Adding New Examples

To add a new example to `examples.json`:

1. Add example data under `examples` object
2. Add mapping in `validation-test.ts` `EXAMPLE_TO_VALIDATOR_MAP`
3. Run validation test to verify

## Integration with WIT

These generated types correspond to the WIT definitions in `domain.wit`. The mapping is:

| WIT Package | JSON Schema | Generated Types |
|-------------|-------------|-----------------|
| `convergence:domain@0.1.0` | `domain.schema.json` | `domain.types.ts` |

The JSON Schema is the canonical representation for:
- JavaScript/TypeScript consumers
- Runtime validation
- Web APIs and JSON serialization

The WIT definitions are the canonical representation for:
- WebAssembly Component Model
- Cross-language interoperability
- Interface contracts

Both representations are kept in sync manually. Future work may automate WIT ↔ JSON Schema conversion.

## Future Enhancements

### Automatic WIT Generation
Generate WIT definitions from JSON Schema or vice versa to ensure perfect synchronization.

### Lazy Circular References
Use `z.lazy()` for true circular references:
```typescript
const addressSchema: z.ZodType<Address> = z.lazy(() => z.object({
  // ...
}));
```

### Custom Format Validators
Add custom Zod validators for domain-specific formats.

### Schema Versioning
Add version tracking and migration support for schema evolution.

### IDE Integration
Generate JSON Schema files for IDE autocomplete and validation in JSON/YAML files.

## Resources

- [JSON Schema Draft 07](http://json-schema.org/draft-07/schema)
- [json-schema-to-typescript](https://www.npmjs.com/package/json-schema-to-typescript)
- [Zod Documentation](https://zod.dev/)
- [WebAssembly Component Model WIT](https://component-model.bytecodealliance.org/design/wit.html)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1.0 | 2026-02-05 | Initial generation system with 57 types and 19 validated examples |
