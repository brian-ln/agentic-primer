# Agentic-Primer Next Steps Prompt

## Context

You are in the **agentic-primer** repository at `/Users/bln/play/agentic-primer`.

**What just happened**:
- ✅ Extracted 63 domain protocol types from Simplify/UGS to WIT
- ✅ Created JSON Schema as source of truth (core/wit/domain/domain.schema.json)
- ✅ Generated TypeScript types and Zod validators
- ✅ Validated 98% coverage against Simplify implementation
- ✅ Closed P2 gap (model/provider configuration)
- ✅ Merged feature/phase1-wit-extraction → genesis → main
- ✅ Archived feature/event-system (experimental work preserved as tag)
- ✅ Cleaned up branches and worktrees
- ✅ Pushed everything to origin

**Current state**:
- Branch: `main` (clean, up to date with origin)
- Protocols: Available at `core/wit/domain/`
- SEAG alignment: In progress (separate session in `simplify/`)

## Why This Matters

**Protocol-First Architecture Achieved**:
- Domain model is now defined independently of any implementation
- Other systems can import protocol types and build implementations
- SEAG becomes the canonical TypeScript implementation
- Signal Hub, Convergence, AI tracking can all use same protocols
- Enables cross-system integration and multi-language bindings

**What We Have Now**:
1. **JSON Schema** - Source of truth for domain model
2. **WIT** - WASM component interface (optional future use)
3. **TypeScript types** - Strict types with no 'any'
4. **Zod validators** - Runtime validation
5. **Documentation** - Validation reports, architecture docs

## Active Work Streams

### 1. SEAG Protocol Integration (Separate Session)

**Location**: `/Users/bln/play/agentic-primer/simplify`
**Prompt file**: `PROTOCOL_INTEGRATION_PROMPT.md`
**Status**: User will load that prompt to do the integration work

**What's happening there**:
- Committing path-addressing implementation
- Creating protocol adapter layer
- Linking to protocol types via symlink
- Updating Address from string to structured type
- Merging to SEAG main

### 2. Repository Organization (This Session)

**Current repo structure**:
```
/Users/bln/play/agentic-primer/
├── core/
│   └── wit/
│       └── domain/          # Protocol definitions (JSON Schema, WIT, TS, Zod)
├── simplify/                # SEAG implementation (embedded git repo)
├── spec-kit/                # Specification tools (embedded git repo)
├── PHASE1_VALIDATION_REPORT.md
├── P2_GAP_CLOSURE_REPORT.md
└── ARCHIVED_BRANCHES.md
```

**Issues**:
1. Protocol files in `core/wit/domain/` but no package.json
2. No clear way to import protocols from other projects
3. SEAG is embedded repo (can't easily share packages)
4. No npm workspace structure yet

## Work To Do (This Session)

### Phase 1: Create Protocol Package Structure

```bash
cd /Users/bln/play/agentic-primer

# Create package for protocol types
mkdir -p packages/protocols
cd packages/protocols

# Initialize package.json
cat > package.json << 'EOF'
{
  "name": "@agentic-primer/protocols",
  "version": "0.1.0",
  "description": "Convergence domain protocol types - universal graph system primitives",
  "type": "module",
  "main": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.js"
    },
    "./types": {
      "types": "./dist/domain.types.d.ts",
      "import": "./dist/domain.types.js"
    },
    "./validators": {
      "types": "./dist/domain.validators.d.ts",
      "import": "./dist/domain.validators.js"
    },
    "./schema": "./schema/domain.schema.json",
    "./wit": "./wit/domain.wit"
  },
  "files": [
    "dist/",
    "schema/",
    "wit/",
    "README.md"
  ],
  "scripts": {
    "build": "tsc",
    "prepublishOnly": "npm run build"
  },
  "keywords": [
    "convergence",
    "protocols",
    "universal-graph",
    "actor-model",
    "domain-model",
    "wasm-component-model"
  ],
  "author": "Brian Lloyd-Newberry",
  "license": "MIT",
  "dependencies": {
    "zod": "^3.22.4"
  },
  "devDependencies": {
    "typescript": "^5.3.3"
  }
}
EOF

# Create tsconfig.json
cat > tsconfig.json << 'EOF'
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ES2022",
    "moduleResolution": "node",
    "outDir": "./dist",
    "rootDir": "./src",
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
EOF

# Copy protocol files
mkdir -p src schema wit
cp ../../core/wit/domain/domain.types.ts src/
cp ../../core/wit/domain/domain.validators.ts src/
cp ../../core/wit/domain/domain.schema.json schema/
cp ../../core/wit/domain/domain.wit wit/

# Create index.ts that re-exports everything
cat > src/index.ts << 'EOF'
/**
 * @agentic-primer/protocols
 *
 * Convergence domain protocol types and validators.
 *
 * Source of truth: schema/domain.schema.json (JSON Schema Draft 07)
 * Generated types: domain.types.ts (TypeScript)
 * Runtime validation: domain.validators.ts (Zod)
 * WASM interface: wit/domain.wit (WebAssembly Component Model)
 */

// Re-export all types
export * from './domain.types.js';

// Re-export all validators
export * from './domain.validators.js';
EOF

# Create README
cat > README.md << 'EOF'
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
EOF
```

### Phase 2: Update Root Package for Workspaces

```bash
cd /Users/bln/play/agentic-primer

# Update root package.json to enable workspaces
cat > package.json << 'EOF'
{
  "name": "agentic-primer",
  "version": "0.1.0",
  "private": true,
  "description": "Protocol-first architecture for convergence systems",
  "workspaces": [
    "packages/*"
  ],
  "scripts": {
    "build": "npm run build --workspaces",
    "test": "npm run test --workspaces"
  },
  "devDependencies": {
    "typescript": "^5.3.3"
  }
}
EOF

# Install dependencies
npm install
```

### Phase 3: Build and Verify Protocol Package

```bash
cd packages/protocols

# Install dependencies
npm install

# Build the package
npm run build

# Verify outputs
ls -la dist/
cat dist/index.d.ts | head -20

# Test import
node -e "import('@agentic-primer/protocols').then(p => console.log(Object.keys(p).slice(0, 10)))"
```

### Phase 4: Update SEAG to Use Protocol Package

**Note**: This will be done in the SEAG session using the other prompt file, but we can prepare here:

```bash
cd /Users/bln/play/agentic-primer/simplify

# Instead of symlink, SEAG can now:
# npm install ../packages/protocols

# Or add to package.json:
# "dependencies": {
#   "@agentic-primer/protocols": "file:../packages/protocols"
# }
```

### Phase 5: Commit Package Structure

```bash
cd /Users/bln/play/agentic-primer

git add packages/protocols/
git add package.json
git commit -m "feat: create @agentic-primer/protocols npm package

- Extract protocol types from core/wit/domain/ to packages/protocols/
- Create npm package with TypeScript types, Zod validators, JSON Schema, WIT
- Add workspace support to root package.json
- Prepare for publishing to npm registry

Package exports:
- @agentic-primer/protocols - All types and validators
- @agentic-primer/protocols/types - TypeScript types
- @agentic-primer/protocols/validators - Zod validators
- @agentic-primer/protocols/schema - JSON Schema
- @agentic-primer/protocols/wit - WIT interface

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

git push origin main
```

### Phase 6: Document Integration Strategy

Create `INTEGRATION_STRATEGY.md`:

```markdown
# Protocol Integration Strategy

## Overview

Convergence domain protocols are now available as npm packages for integration across systems.

## Packages

### @agentic-primer/protocols

Protocol types, validators, and schemas.

**Install**:
```bash
npm install @agentic-primer/protocols
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
npm install @agentic-primer/seag
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

1. **Install protocols**: `npm install @agentic-primer/protocols`
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

## Questions?

See:
- `PHASE1_VALIDATION_REPORT.md` - Protocol coverage analysis
- `P2_GAP_CLOSURE_REPORT.md` - Model/provider config additions
- `packages/protocols/README.md` - Package documentation
```

```bash
git add INTEGRATION_STRATEGY.md
git commit -m "docs: add protocol integration strategy

Documents how systems will integrate with @agentic-primer/protocols.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
git push origin main
```

## Success Criteria

After completing this work:

✅ `@agentic-primer/protocols` package created
✅ Protocol types available via npm install
✅ Workspace structure configured
✅ Build produces clean TypeScript declarations
✅ All exports work (types, validators, schema, wit)
✅ SEAG can install and use the package
✅ Integration strategy documented
✅ Everything committed and pushed to origin

## What Happens Next

1. **SEAG Integration** (other session):
   - User loads `PROTOCOL_INTEGRATION_PROMPT.md` in simplify/
   - SEAG adopts protocol types
   - SEAG becomes canonical implementation

2. **Package Publishing** (future):
   - Test @agentic-primer/protocols in SEAG
   - Publish to npm registry
   - Create @agentic-primer/seag package

3. **Other Systems** (future):
   - Signal Hub integrates protocols
   - Convergence integrates protocols
   - AI tracking integrates protocols

## Files Created

- `packages/protocols/package.json`
- `packages/protocols/tsconfig.json`
- `packages/protocols/src/index.ts`
- `packages/protocols/src/domain.types.ts`
- `packages/protocols/src/domain.validators.ts`
- `packages/protocols/schema/domain.schema.json`
- `packages/protocols/wit/domain.wit`
- `packages/protocols/README.md`
- `package.json` (root, with workspaces)
- `INTEGRATION_STRATEGY.md`

## Files Modified

- `package.json` (root) - Added workspaces

## Commands Summary

```bash
# Complete workflow
cd /Users/bln/play/agentic-primer

# 1. Create package structure
mkdir -p packages/protocols/src packages/protocols/schema packages/protocols/wit

# 2. Create package.json, tsconfig.json, README.md (see above)

# 3. Copy protocol files
cp core/wit/domain/domain.types.ts packages/protocols/src/
cp core/wit/domain/domain.validators.ts packages/protocols/src/
cp core/wit/domain/domain.schema.json packages/protocols/schema/
cp core/wit/domain/domain.wit packages/protocols/wit/

# 4. Create index.ts (see above)

# 5. Update root package.json with workspaces

# 6. Install and build
npm install
cd packages/protocols && npm run build

# 7. Verify
ls -la packages/protocols/dist/

# 8. Commit and push
git add packages/ package.json INTEGRATION_STRATEGY.md
git commit -m "feat: create @agentic-primer/protocols package"
git push origin main
```
