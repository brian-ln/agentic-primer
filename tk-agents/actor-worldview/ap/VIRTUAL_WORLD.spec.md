# Specification: The Virtual World Abstraction

## The Core Axiom
**For a domain actor, there is no disk, no network, and no API.** There is only the Graph.

## 1. Uniform Addressing
Every entity, regardless of its origin (File, URL, Database Row), is addressed via a **SEAG UUID**.

- **Mapping:** The `GraphSupervisor` maintains a mapping between `SEAG_UUID` and `Origin_URI`.
- **Origins:**
    - `fs:///path/to/file`
    - `web://example.com/api`
    - `blob://s3-bucket/key`
    - `mem://volatile-cache`

## 2. Virtual Volumes (Mounting)
Instead of "opening a file", we "mount an origin" into the graph.
- Mounting a directory creates a root node with `child_of` edges to every file/folder inside.
- Mounting a URL creates a node that, when sent a `get` message, triggers the `WebEffectActor`.

## 3. The Persistence Lifecycle
Persistence is an **Effect** handled by specialized actors:

1. **Domain Actor** sends `patch` to `node_123`.
2. **Node Actor** emits `NodeUpdated` event.
3. **Persistence Supervisor** identifies that `node_123` is mapped to `fs:///path/to/file`.
4. **FileEffectActor** performs the physical write.

## 4. Structural Destructuring (The "Tree" Insight)
Documents are not blobs; they are sub-graphs.
- **Markdown:** Destructured into Header nodes, Paragraph nodes, and CodeBlock nodes.
- **CSV/JSON:** Destructured into Record nodes and Field nodes.
- **Code:** Destructured into Function nodes and Class nodes.

This allows an actor to `watch` a specific function in a source file without caring about the rest of the file.
