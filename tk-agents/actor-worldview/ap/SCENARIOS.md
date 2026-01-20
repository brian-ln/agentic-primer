# SEAG MVP: Interaction Scenarios

These scenarios demonstrate the practical power of the **Self-Evolving Actor Graph**. Use these in the [REPL](http://localhost:3000) to explore the system.

---

## Scenario 1: The "No Spoon" Configuration Update
**Goal:** Update a system setting without ever touching the filesystem directly.

1.  **Setup:** Ensure `data/config.json` exists with some JSON content.
2.  **REPL:** `mount data/config.json`
    - *Brain:* "Successfully mounted data/config.json into the graph."
3.  **REPL:** `explore seag://local/active-doc`
    - *Brain:* Discover the fragment addresses (e.g., `.../fragments/theme`).
4.  **REPL:** `set seag://local/active-doc/fragments/theme "light"`
    - *Effect:* The physical file is updated immediately.
5.  **Validation:** Open the file in your IDE to see the change.

---

## Scenario 2: Two-Way Synchronization (The Live IDE)
**Goal:** See the graph react to external physical changes in real-time.

1.  **Setup:** `mount data/demo.json`
2.  **REPL:** `explore seag://local/active-doc` (Note the current value of a fragment).
3.  **Action:** Open `data/demo.json` in your text editor and change a value manually. Save the file.
4.  **REPL:** `explore seag://local/active-doc`
    - *Observation:* The Brain will report the **new value** you just typed in your editor.
    - *Mechanism:* The `FileEffectActor` watched the disk and reconciled the actor state.

---

## Scenario 3: Knowledge Discovery (Transitive Reachability)
**Goal:** Use the graph's intelligence to find related pieces of information.

1.  **REPL:** `mount data/complex.json` (A file with nested objects).
2.  **REPL:** `explore seag://local/active-doc`
    - *Insight:* This query uses the `calculateReachability` logic in the `GraphProjector`. It doesn't just show you the file; it shows you every actor that is "reachable" through the link-graph.

---

## Scenario 4: The Audit Trail (Time Travel)
**Goal:** Verify that every interaction is durably recorded.

1.  **Action:** Perform several `mount` and `set` commands.
2.  **Validation:** Run `cat data/events.jsonl` in your terminal.
    - *Observation:* You will see every `THINK`, `APPEND`, and `WRITE_FILE` event, including the `traceId` that links them.
    - *Potential:* In the next version, the Brain will be able to "replay" these to explain why a decision was made.
