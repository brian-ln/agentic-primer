# Protocol Integration Strategy

## Overview

Convergence domain protocols are now available as npm packages for integration across systems.

## Packages

### @agentic-primer/protocols

Protocol types, validators, and schemas.

**Install**:
```bash
pnpm install @agentic-primer/protocols
```

**Use**:
```typescript
import { Address, Node, Agent, Task } from '@agentic-primer/protocols';
import { addressSchema, nodeSchema } from '@agentic-primer/protocols/validators';
```

### @agentic-primer/seag (Coming Soon)

Canonical TypeScript implementation of protocols.

**Install**:
```bash
pnpm install @agentic-primer/seag
```

**Use**:
```typescript
import { Graph, Router, Actor } from '@agentic-primer/seag';
import type { Address, Node } from '@agentic-primer/protocols';
```

## Integration Paths

### 1. SEAG (Simply Graphic Actors)

**Status**: In progress
**Location**: `/Users/bln/play/agentic-primer/simplify`
**Goal**: Canonical TypeScript/Browser/Cloudflare Workers implementation

**Steps**:
1. Install @agentic-primer/protocols
2. Update Address from string to protocol type
3. Align other types (Node, Edge, Agent, Task, Session)
4. Implement protocol validators
5. Publish as @agentic-primer/seag

**Prompt**: `simplify/PROTOCOL_INTEGRATION_PROMPT.md`

### 2. Signal Hub

**Status**: Not started
**Goal**: Event hub using protocol messages

**Steps**:
1. Install @agentic-primer/protocols
2. Use MessageEnvelope for events
3. Use Address for routing
4. Implement protocol-compliant event store

### 3. Convergence Framework

**Status**: Not started
**Goal**: Multi-model orchestration using protocol types

**Steps**:
1. Install @agentic-primer/protocols
2. Use ModelConfig, ProviderConfig
3. Use Agent, Task, Session for orchestration
4. Implement protocol-compliant evaluation framework

### 4. AI Tracking System

**Status**: Not started
**Goal**: Track model usage across providers

**Steps**:
1. Install @agentic-primer/protocols
2. Use ModelConfig, ProviderConfig
3. Track usage per protocol types
4. Enable cross-system reporting

## Versioning

Protocols follow semantic versioning:

- **v0.1.0** (current) - Initial extraction from Simplify/UGS
- **v0.2.0** - After SEAG validation and refinement
- **v1.0.0** - Production-ready, stable API

## Migration Path

For existing systems:

1. **Install protocols**: `pnpm install @agentic-primer/protocols`
2. **Import types**: Use protocol types alongside existing types
3. **Create adapters**: Convert between legacy and protocol types
4. **Validate**: Use Zod validators to ensure correctness
5. **Migrate incrementally**: Replace legacy types over time
6. **Remove adapters**: Once fully migrated

## Publishing

Protocols will be published to npm registry once:

1. SEAG validation confirms protocol completeness
2. At least 2 implementations exist (SEAG + 1 other)
3. Documentation is complete
4. Version 1.0.0 released

## Package Structure

```
packages/protocols/
├── src/
│   ├── index.ts                 # Main export
│   ├── domain.types.ts          # TypeScript types (generated)
│   └── domain.validators.ts     # Zod validators (generated)
├── schema/
│   └── domain.schema.json       # JSON Schema (source of truth)
├── wit/
│   └── domain.wit               # WASM Component Model interface
├── dist/                        # Build outputs
│   ├── index.js
│   ├── index.d.ts
│   ├── domain.types.js
│   ├── domain.types.d.ts
│   ├── domain.validators.js
│   └── domain.validators.d.ts
├── package.json
├── tsconfig.json
└── README.md
```

## Import Examples

**All types and validators**:
```typescript
import {
  Address, Node, Edge, Agent, Task, Session,
  addressSchema, nodeSchema, agentSchema
} from '@agentic-primer/protocols';
```

**Just types**:
```typescript
import type { Address, Node, Agent } from '@agentic-primer/protocols/types';
```

**Just validators**:
```typescript
import { addressSchema, nodeSchema } from '@agentic-primer/protocols/validators';
```

**JSON Schema**:
```typescript
import schema from '@agentic-primer/protocols/schema';
```

**WIT interface** (for WASM components):
```wit
import convergence:domain@0.1.0;
```

## Development Workflow

### Working on Protocols

```bash
cd packages/protocols

# Make changes to JSON Schema
vim schema/domain.schema.json

# Regenerate types and validators
cd ../..
pnpm run generate:types

# Rebuild package
cd packages/protocols
pnpm run build

# Test in consuming projects
cd ../../simplify
pnpm install
```

### Adding New Types

1. Update `schema/domain.schema.json`
2. Run `pnpm run generate:types` (from root)
3. Rebuild: `cd packages/protocols && pnpm run build`
4. Update consumers (SEAG, etc.)
5. Increment version in `package.json`

## Questions?

See:
- `PHASE1_VALIDATION_REPORT.md` - Protocol coverage analysis
- `P2_GAP_CLOSURE_REPORT.md` - Model/provider config additions
- `packages/protocols/README.md` - Package documentation
- `simplify/PROTOCOL_INTEGRATION_PROMPT.md` - SEAG integration guide
