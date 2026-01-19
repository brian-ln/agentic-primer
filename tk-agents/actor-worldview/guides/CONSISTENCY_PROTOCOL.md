# SEAG Consistency Protocol

**Purpose:** Ensure alignment between Design (Specs/Models), Implementation (Code), and Validation (Tests).

## 1. The Design Quad
We maintain four artifacts for every feature:
1.  **Specification (`ap/*.spec.md`)**: The "Why" and "What". Natural language requirements.
2.  **Model (`ap/*.model.lisp`)**: The "How" (Abstract). Logical actor behavior and message flow.
3.  **Implementation (`seag/*.ts`)**: The "How" (Concrete). Executable TypeScript code.
4.  **Verification (`tests/harness/*.test.ts`)**: The "Proof". Deterministic assertions.

## 2. The Golden Rule
**The Specification is the Source of Truth.**
Code should never implement behavior not defined in the Model/Spec. If code *needs* to change, update the Model/Spec first (or concurrently).

## 3. The Update Workflow (The "Loop")

### Step 1: Design Update
- Modify `ap/<feature>.spec.md` to describe the new requirement.
- Update `ap/<feature>.model.lisp` to define the actor message flow.

### Step 2: Test Design (TDD)
- Create or update `tests/harness/<phase>_<feature>.test.ts`.
- The test should fail (or not compile) against the current implementation.

### Step 3: Implementation
- Write/Refactor `seag/<feature>.ts`.
- **Constraint:** Adhere to the Actor Model principles defined in `WORLDVIEW.md` unless explicitly optimizing (which must be documented).

### Step 4: Consistency Check
- **Names:** Do class names match Model actor names?
- **Messages:** Do `receive()` handlers match Model `(on ...)` handlers?
- **State:** Does class properties match Model `(state ...)`?

## 4. Verification Checklist
Before committing:
- [ ] Spec updated?
- [ ] Model updated?
- [ ] Tests pass?
- [ ] Docs (README/INDEX) updated?
