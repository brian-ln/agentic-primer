# @agentic-primer/protocols

Convergence domain protocol types - universal graph system primitives.

## Overview

Protocol-first domain model for convergence systems. Defines core primitives for:

- **Graph**: Address, Node, Edge
- **Entities**: Agent, Human, Information, Model, Program, Provider, Session, Task
- **Messaging**: Message, MessageEnvelope, ActorRef
- **Configuration**: ModelConfig, ProviderConfig
- **63 total types** covering the complete domain model

## Installation

```bash
npm install @agentic-primer/protocols
```

## Usage

```typescript
import {
  Address,
  Node,
  Edge,
  Agent,
  Task,
  addressSchema,
  nodeSchema
} from '@agentic-primer/protocols';

// Use types
const addr: Address = {
  id: "agent-123",
  scope: "node"
};

// Runtime validation
const result = addressSchema.safeParse(addr);
if (result.success) {
  console.log("Valid address:", result.data);
}
```

## Exports

- `@agentic-primer/protocols` - All types and validators
- `@agentic-primer/protocols/types` - Just TypeScript types
- `@agentic-primer/protocols/validators` - Just Zod validators
- `@agentic-primer/protocols/schema` - JSON Schema source
- `@agentic-primer/protocols/wit` - WASM Component Model interface

## Source of Truth

JSON Schema at `schema/domain.schema.json` is the source of truth.

All other formats are generated from the schema:
- TypeScript types (`domain.types.ts`)
- Zod validators (`domain.validators.ts`)
- WIT interface (`wit/domain.wit`)

## Protocol Specification

Version: `v0.1.0`
Schema: `https://convergence.dev/schemas/domain/v0.1.0`
WIT package: `convergence:domain@0.1.0`

## Implementations

- **SEAG** (`@agentic-primer/seag`) - Canonical TypeScript/Browser/Cloudflare Workers implementation
- **Simplify** - Reference implementation (TypeScript)
- **Signal Hub** - Cloudflare Workers event hub
- **Convergence** - Multi-model orchestration framework

## License

MIT
