# Agent Note: Primer CLI Architecture Pattern

**To:** Agent a9d4ef7 (CLI Specs and Unified Wrapper)
**From:** Parent Agent
**Priority:** HIGH - Architecture Decision

## User Feedback on Architecture

The user noted that many CLIs use a **sub-program delegation pattern** where:
- Main wrapper: `primer` (or `ap`)
- Subcommands: `primer-task`, `primer-graph`, `primer-knowledge`
- Wrapper looks for executables with prefix in PATH

## Examples of This Pattern

**git:**
```bash
git add file.txt
# Executes: git-add file.txt
```

**cargo:**
```bash
cargo build
# Executes: cargo-build
```

**kubectl:**
```bash
kubectl apply -f config.yaml
# Executes: kubectl apply (but plugins work via kubectl-*)
```

## Recommended Implementation

Instead of importing CLIs directly in primer.ts, use:

**Option 1: Pure Delegation (Recommended)**
```typescript
#!/usr/bin/env bun
// primer.ts

const subsystem = process.argv[2]; // task, graph, knowledge
const args = process.argv.slice(3);

// Delegate to primer-<subsystem>
const subcommand = `primer-${subsystem}`;
const result = Bun.spawn([subcommand, ...args]);
```

**Option 2: Hybrid (Built-in + Extensible)**
```typescript
#!/usr/bin/env bun
// primer.ts

const subsystem = process.argv[2];
const args = process.argv.slice(3);

// Try built-in commands first
const builtIn = {
  'task': './task.ts',
  'graph': './graph.ts',
  'knowledge': './knowledge.ts'
};

if (builtIn[subsystem]) {
  // Execute built-in
  Bun.spawn(['bun', builtIn[subsystem], ...args]);
} else {
  // Look for primer-* in PATH (plugin support)
  Bun.spawn([`primer-${subsystem}`, ...args]);
}
```

## Symlink Structure

Create symlinks for standard pattern:
```bash
ln -s primer.ts primer
ln -s task.ts primer-task
ln -s graph.ts primer-graph
ln -s knowledge.ts primer-knowledge

# Short alias
ln -s primer.ts ap
```

Usage becomes:
```bash
primer task add "My task"
primer graph show node_1
primer knowledge query "What is X?"

# Or with short alias
ap task list --json
```

## Benefits of This Approach

1. **Plugin Architecture** - Anyone can add `primer-foo` commands
2. **Process Isolation** - Each CLI runs independently
3. **Loose Coupling** - No imports between CLIs
4. **Standard Pattern** - Familiar to git/cargo/kubectl users
5. **Language Agnostic** - Future CLIs can be in any language
6. **Easy Testing** - Each CLI testable independently

## Implementation Checklist

For your primer.ts implementation:

- [ ] Use sub-program delegation (not direct imports)
- [ ] Look for `primer-<subsystem>` executables
- [ ] Fall back to built-in paths if needed (hybrid)
- [ ] Create symlinks: primer-task, primer-graph, primer-knowledge
- [ ] Document plugin pattern in specs
- [ ] Show examples of adding custom `primer-*` commands

## Specification Updates

Update PRIMER_CLI.spec.md to document:
- Plugin architecture
- How to add custom `primer-*` commands
- Symlink structure
- Search path resolution

## Questions?

If you need clarification on this architecture change, use CLARIFICATION_NEEDED protocol.

This aligns with industry-standard CLI design and makes our system more extensible.
