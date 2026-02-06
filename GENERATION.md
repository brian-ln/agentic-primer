# Domain Model Type Generation

This document describes the automated generation process for TypeScript types and Zod validators from the JSON Schema domain model.

## Overview

The Convergence domain model is defined as a single source of truth in JSON Schema format (`domain.schema.json`). From this schema, we automatically generate:

1. **TypeScript Types** (`domain.types.ts`) - Compile-time type safety
2. **Zod Validators** (`domain.validators.ts`) - Runtime validation

## Generation Script

**Location**: `scripts/generate-domain-types.ts`

**Usage**:
```bash
pnpm generate:types
```

## Generation History

### 2026-02-05: Initial Generation (Phase 1 Complete)
- **Schema Version**: 0.1.0
- **Types Generated**: 57 definitions
- **Coverage**: 95%+ of Simplify/UGS implementation
- **Status**: ✅ Phase 1 validation complete

**Generated files**:
- `core/wit/domain/domain.types.ts` (3000+ lines)
- `core/wit/domain/domain.validators.ts` (2000+ lines)

**Validation**:
- 19 examples validated
- 100% pass rate
- TypeScript compilation: ✅ PASS

### 2026-02-05: P2 Enhancement - Model/Provider Config Types
- **Schema Version**: 0.1.0 (enhanced)
- **Types Added**: 6 new definitions
  - `model-config` - AI model configuration (inference/embedding)
  - `model-type` - Model type discriminant (inference | embedding)
  - `model-lifecycle` - Model lifecycle states (draft | published | deprecated)
  - `situation-params` - Situational parameter overrides
  - `provider-config` - AI provider routing configuration
  - `provider-type` - Provider type enum (cloudflare-ai-gateway)

- **Total Types**: 63 definitions
- **Coverage**: 98%+ of Simplify/UGS implementation

**Examples Added**:
- `inference_model_config` - Claude Sonnet with situational routing
- `embedding_model_config` - BGE embeddings configuration
- `cloudflare_provider_config` - Cloudflare AI Gateway config
- `situation_params` - Parameter override example

**Validation**:
- 23 examples validated (19 original + 4 new)
- 100% pass rate
- TypeScript compilation: ✅ PASS

**Gap Closure**:
- ✅ Gap #1 (P2): Model/Provider Config Details - **CLOSED**
- Remaining: 2 P3 gaps (GraphEvent, Reactive Subscriptions)

## Regeneration Process

When updating the schema, follow this process:

### 1. Update Schema
Edit `core/wit/domain/domain.schema.json` to add/modify type definitions.

**Guidelines**:
- Use JSON Schema Draft 7 format
- Maintain consistent naming (kebab-case for properties)
- Document all types with descriptions
- Use `$ref` for type references
- Validate JSON syntax

### 2. Update Examples
Edit `core/wit/domain/examples.json` to add examples for new types.

**Guidelines**:
- Provide at least one example per new type
- Use realistic data values
- Cover edge cases (nulls, optional fields)
- Ensure examples validate against schema

### 3. Regenerate Types
```bash
cd /Users/bln/play/agentic-primer-wit
pnpm generate:types
```

**Expected output**:
```
=== Convergence Domain Type Generator ===
Reading schema from: .../domain.schema.json
Generating TypeScript types...
✓ TypeScript types generated successfully
Generating Zod validators...
✓ Zod validators generated successfully
```

### 4. Update Validator Mappings
Edit `core/wit/domain/validation-test.ts`:

1. Add entries to `EXAMPLE_TO_VALIDATOR_MAP`
2. Add type assertions in `typeCheck()` function

### 5. Validate Examples
```bash
pnpm validate:domain
```

**Expected output**:
```
=== Validation Results ===
✓ Passed: N examples
Success Rate: 100.0%
```

### 6. Run Full Test Suite
```bash
pnpm test
```

Ensures both validation and TypeScript compilation pass.

## Type Generation Details

### TypeScript Types
- Generated using `json-schema-to-typescript` library
- Produces strict TypeScript interfaces
- Handles unions, enums, nested objects
- Preserves JSDoc comments from descriptions

### Zod Validators
- Generated using custom Zod schema builder
- Provides runtime validation
- Supports all JSON Schema constraints:
  - Type checking (string, number, boolean, null, object, array)
  - String formats (email, uuid, date-time, uri)
  - Numeric constraints (minimum, maximum)
  - Array constraints (minItems, maxItems)
  - Required fields
  - Enums

## Validation Coverage

| Validation Type | Coverage |
|----------------|----------|
| JSON Schema → TypeScript | 100% of definitions |
| TypeScript → Runtime (Zod) | 100% of definitions |
| Examples → Zod Validators | 100% pass rate (23/23) |
| TypeScript Compilation | ✅ PASS |

## Future Enhancements

### Planned Additions (P3)
1. **GraphEvent Types** - Event sourcing visibility
2. **Reactive Subscription Types** - Reactive query patterns

### Type Safety Improvements
- Consider readonly types for immutable structures
- Add branded types for addresses (@-syntax)
- Generate type guards for discriminated unions

## Troubleshooting

### Generation Fails
1. Validate JSON Schema syntax: `pnpm run validate:schema` (if available)
2. Check for circular references in `$ref` definitions
3. Verify all referenced definitions exist

### Validation Fails
1. Check example data matches schema constraints
2. Verify validator mappings in `validation-test.ts`
3. Run individual validator: `DomainValidators.typeName.parse(data)`

### TypeScript Compilation Fails
1. Check for type conflicts in generated types
2. Verify example type assertions match generated interfaces
3. Run `pnpm typecheck` for detailed errors

## References

- **Schema**: `core/wit/domain/domain.schema.json`
- **Examples**: `core/wit/domain/examples.json`
- **Generated Types**: `core/wit/domain/domain.types.ts`
- **Generated Validators**: `core/wit/domain/domain.validators.ts`
- **Validation Test**: `core/wit/domain/validation-test.ts`
- **Generation Script**: `scripts/generate-domain-types.ts`
- **Validation Report**: `PHASE1_VALIDATION_REPORT.md`
