# Specification: The Virtual World Abstraction

## The Core Axiom
**For a domain actor, there is no disk, no network, and no API.** There is only the Graph.

## 1. Uniform Addressing & Web Standards
Every entity is addressed via a **SEAG UUID**, but internally the `FileEffectActor` maps these to **FileSystemHandles** (W3C Standard). 

- **Browser:** Uses `FileSystemFileHandle`.
- **Server (Bun/Node):** Uses `node:fs` but mocks the `FileSystemHandle` interface for portability.

## 2. Persistence Lifecycle (Two-Way Sync)

### Graph-to-Disk (The "Push")
1. `FragmentNode` receives `PATCH`.
2. `DocumentActor` re-assembles (Folds) the fragments.
3. `FileEffectActor` writes the blob to the handle.

### Disk-to-Graph (The "Pull" / Reconciliation)
1. `FileEffectActor` observes external change via `fs.watch` or `FileSystemObserver`.
2. `DocumentActor` triggers a **Re-Shred**.
3. `DocumentParser` reconciles the new state with existing `FragmentNodes`.
4. Subscribers receive signals of the external change.

## 3. Structural Destructuring (The "Tree" Insight)
...
