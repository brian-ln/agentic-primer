# Convergence Domain Model

This directory contains two complementary representations of the complete Convergence domain model:

1. **domain.wit** - WebAssembly Interface Types (WIT) definition
2. **domain.schema.json** - JSON Schema definition

## Overview

The Convergence domain model defines the core data structures and interfaces for:

- **Graph Primitives** - Addresses, nodes, edges, and graph traversal
- **Entities** - Agents, tasks, sessions, humans, and base entity operations
- **Query System** - Success criteria evaluation and graph query DSL
- **Knowledge Management** - Embeddings, convergence detection, and session state
- **Actor/Program System** - Message-based actors and executable programs

## When to Use Each Format

### Use WIT (domain.wit) When:

- **Building WebAssembly components** that export or import domain interfaces
- **Cross-language interoperability** is required (Rust, Go, JavaScript, Python, etc.)
- **Type-safe component composition** with WASI and Component Model
- **Runtime validation** with wasm-tools component model
- **Interface versioning** and semantic compatibility matter
- **Binary serialization** with efficient wire formats

**Examples:**
- Implementing domain operations as WASM components
- Creating language bindings for domain types
- Building distributed systems with WASI preview2
- Type-safe RPC between components

### Use JSON Schema (domain.schema.json) When:

- **REST APIs** need request/response validation
- **JSON documents** are the primary data format
- **Web applications** need client-side validation
- **Configuration files** require schema validation
- **OpenAPI/Swagger** documentation generation
- **JSON-based databases** (MongoDB, CouchDB, etc.)

**Examples:**
- Validating HTTP API payloads
- Generating TypeScript types from schema
- Creating forms with automatic validation
- Documenting JSON data formats

## Key Differences

| Aspect | WIT | JSON Schema |
|--------|-----|-------------|
| **Type System** | Rich types (u32, f64, result, variant, resource) | Basic JSON types + validation rules |
| **Resources** | First-class resources with methods | Not supported (objects only) |
| **Variants** | Tagged unions with type safety | oneOf with discriminators |
| **Recursive Types** | Limited (use indirection) | Supported via $ref |
| **Binary Data** | Native list<u8> | Array of integers or base64 string |
| **Functions** | Interface methods with signatures | Not applicable |
| **Validation** | Compile-time type checking | Runtime schema validation |

## Mapping Guide

### WIT → JSON Schema Mappings

#### Basic Types

```wit
// WIT
string              → {"type": "string"}
u32, u64            → {"type": "integer", "minimum": 0}
s32, s64            → {"type": "integer"}
f32, f64            → {"type": "number"}
bool                → {"type": "boolean"}
```

#### Option Types

```wit
// WIT
option<string>      → {"type": ["string", "null"]}
option<u32>         → {"oneOf": [{"type": "integer"}, {"type": "null"}]}
```

#### Lists

```wit
// WIT
list<string>        → {"type": "array", "items": {"type": "string"}}
list<address>       → {"type": "array", "items": {"$ref": "#/definitions/address"}}
```

#### Records

```wit
// WIT
record node {
    id: string,
    data: option<string>,
}

// JSON Schema
{
  "type": "object",
  "properties": {
    "id": {"type": "string"},
    "data": {"type": ["string", "null"]}
  },
  "required": ["id"],
  "additionalProperties": false
}
```

#### Variants

```wit
// WIT
variant address-scope {
    node,
    edge(edge-ref),
    computed(string),
}

// JSON Schema
{
  "oneOf": [
    {"type": "string", "const": "node"},
    {
      "type": "object",
      "properties": {"edge": {"$ref": "#/definitions/edge-ref"}},
      "required": ["edge"]
    },
    {
      "type": "object",
      "properties": {"computed": {"type": "string"}},
      "required": ["computed"]
    }
  ]
}
```

#### Enums

```wit
// WIT
enum direction {
    outgoing,
    incoming,
    bidirectional,
}

// JSON Schema
{
  "type": "string",
  "enum": ["outgoing", "incoming", "bidirectional"]
}
```

#### Result Types

```wit
// WIT
result<node, node-error>

// JSON Schema (success case)
{"$ref": "#/definitions/node"}

// JSON Schema (error case)
{
  "type": "object",
  "properties": {
    "error": {"$ref": "#/definitions/node-error"}
  },
  "required": ["error"]
}
```

#### Resources

```wit
// WIT
resource entity {
    get-address: func() -> address;
    get-kind: func() -> entity-kind;
}

// JSON Schema - Resources become opaque references
{
  "type": "object",
  "properties": {
    "entity-id": {"type": "string", "description": "Opaque entity handle"}
  },
  "required": ["entity-id"]
}
```

### Special Cases

#### JSON Values (Recursive Types)

WIT doesn't support recursive types directly, so `json-value` is represented as:

```wit
// WIT - serialized string
type json-value = string;

// JSON Schema - native JSON
{
  "description": "Any valid JSON value"
}
```

In practice, WIT implementations should parse/serialize JSON strings, while JSON Schema allows direct validation.

#### Binary Data

```wit
// WIT
list<u8>            // Native binary

// JSON Schema
{
  "type": "array",
  "items": {"type": "integer", "minimum": 0, "maximum": 255}
}
// OR use base64 string:
{"type": "string", "contentEncoding": "base64"}
```

## Package Structure

### WIT Package

```wit
package convergence:domain@0.1.0;

interface address { ... }
interface node { ... }
interface entity { ... }
// ... more interfaces

world domain {
    export address;
    export node;
    export entity;
    // ... more exports
}
```

### JSON Schema Structure

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "https://convergence.dev/schemas/domain/v0.1.0",
  "definitions": {
    "address": { ... },
    "node": { ... },
    "entity-metadata": { ... }
  }
}
```

## Examples

### Example 1: Address

**WIT:**
```wit
record address {
    id: string,
    namespace: option<string>,
    scope: address-scope,
    version: option<string>,
}
```

**JSON Schema:**
```json
{
  "type": "object",
  "properties": {
    "id": {"type": "string"},
    "namespace": {"type": ["string", "null"]},
    "scope": {"$ref": "#/definitions/address-scope"},
    "version": {"type": ["string", "null"]}
  },
  "required": ["id", "scope"]
}
```

**JSON Instance:**
```json
{
  "id": "agent-001",
  "namespace": "convergence",
  "scope": "node",
  "version": "1.0.0"
}
```

### Example 2: Agent Configuration

**WIT:**
```wit
record agent-config {
    name: string,
    system-prompt: string,
    tools: list<address>,
    default-model: address,
    harness: agent-harness,
}
```

**JSON Instance:**
```json
{
  "name": "Research Agent",
  "system-prompt": "You are a research assistant...",
  "tools": [
    {"id": "web-search", "scope": "node"},
    {"id": "file-read", "scope": "node"}
  ],
  "default-model": {
    "id": "claude-3-5-sonnet",
    "scope": "node"
  },
  "harness": {
    "max-turns": 10,
    "reflect-on-failure": true,
    "checkpoint-every": 5
  }
}
```

### Example 3: Task with Success Criteria

**JSON Instance:**
```json
{
  "title": "Generate report",
  "description": "Create quarterly report",
  "spec": {
    "inputs": ["data/q4-metrics.csv"],
    "outputs": ["reports/q4-report.pdf"],
    "constraints": ["Must be under 10 pages"],
    "success-criteria": [
      {
        "criterion-type": "file-exists",
        "params": "{\"path\": \"reports/q4-report.pdf\"}"
      },
      {
        "criterion-type": "word-count",
        "params": "{\"min-words\": 1000, \"max-words\": 5000, \"text-source\": \"report-content\"}"
      }
    ]
  },
  "priority": "p1"
}
```

## Validation

### Validating WIT

```bash
# Install wasm-tools
cargo install wasm-tools

# Validate WIT syntax and semantics
wasm-tools component wit domain.wit

# Generate documentation
wasm-tools component wit --html domain.wit > domain.html
```

### Validating JSON Schema

```bash
# Using ajv-cli
npm install -g ajv-cli

# Validate schema itself
ajv compile -s domain.schema.json

# Validate instance against schema
ajv validate -s domain.schema.json -d instance.json
```

### Online Validators

- **WIT:** Use wasm-tools or component-model playground
- **JSON Schema:** https://www.jsonschemavalidator.net/

## Version Compatibility

Both representations use semantic versioning:

- **WIT:** `package convergence:domain@0.1.0`
- **JSON Schema:** `"$id": "https://convergence.dev/schemas/domain/v0.1.0"`

Version changes:
- **Major (0.x.0):** Breaking changes to types or interfaces
- **Minor (x.1.0):** Backward-compatible additions
- **Patch (x.x.1):** Non-breaking fixes or clarifications

## Integration Patterns

### Pattern 1: HTTP API with WASM Backend

```
HTTP Request (JSON)
  → Validate with JSON Schema
  → Deserialize to JSON
  → Convert to WIT types
  → Call WASM component
  → Convert WIT result to JSON
  → Validate with JSON Schema
  → HTTP Response (JSON)
```

### Pattern 2: Pure WASM Component Graph

```
WASM Component A (WIT)
  → Export domain interfaces
  → Import other domain interfaces
WASM Component B (WIT)
  → Compose with Component A
  → Type-safe at compile time
```

### Pattern 3: JSON Configuration + WASM Execution

```
Config File (JSON)
  → Validate with JSON Schema
  → Load into WASM component
  → Execute using WIT interfaces
  → Emit events (JSON)
  → Validate with JSON Schema
```

## Code Generation

### From WIT

```bash
# Rust bindings
wit-bindgen rust --world domain domain.wit

# JavaScript bindings
jco transpile component.wasm -o output/

# Python bindings (via componentize-py)
componentize-py bindings domain.wit
```

### From JSON Schema

```bash
# TypeScript types
quicktype -s schema domain.schema.json -o types.ts

# Python dataclasses
quicktype -s schema domain.schema.json -o models.py --python-dataclasses

# Go structs
quicktype -s schema domain.schema.json -o models.go
```

## Best Practices

1. **Keep Synchronized:** Changes to domain.wit should be reflected in domain.schema.json
2. **Version Together:** Both files should have matching version numbers
3. **Document Divergence:** Note any intentional differences (e.g., recursive types, resources)
4. **Validate Both:** CI/CD should validate both representations
5. **Test Conversions:** Ensure JSON instances can be converted to/from WIT types
6. **Use $ref:** In JSON Schema, reference definitions to avoid duplication
7. **Use `use` Statements:** In WIT, import types from other interfaces

## Limitations

### WIT Limitations

- No recursive types (use indirection or strings)
- No optional parameters in functions
- Resources cannot be serialized directly
- Limited pattern matching in variants

### JSON Schema Limitations

- No function signatures
- No compile-time type safety
- No resource/capability types
- Weaker type system than WIT

## Further Reading

- [WebAssembly Component Model](https://github.com/WebAssembly/component-model)
- [WIT Format Specification](https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md)
- [JSON Schema Specification](https://json-schema.org/specification.html)
- [WASI Preview 2](https://github.com/WebAssembly/WASI/tree/main/preview2)

## Support

For questions or issues:
- File issues in the Convergence repository
- Join the discussion on Discord
- Check the documentation at https://convergence.dev
