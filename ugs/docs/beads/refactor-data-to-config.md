# Refactor: `data` → `config` + Embedding Model Support

## Overview

Rename `data` field to `config` across all entities for semantic clarity, and add embedding model support via union types.

## Tasks

### Phase 1: Interface Updates

1. **model.ts** - Add union type for inference/embedding
   ```typescript
   interface BaseConfig {
     name: string;
     backendModel: string;
     provider: string;
   }

   interface InferenceConfig extends BaseConfig {
     modelType: 'inference';
     temperature?: number;
     maxTokens?: number;
     topP?: number;
     situations?: Record<string, SituationParams>;
   }

   interface EmbeddingConfig extends BaseConfig {
     modelType: 'embedding';
     dimensions: number;
   }

   interface Model {
     // ...
     config: InferenceConfig | EmbeddingConfig;  // was 'data'
   }
   ```

2. **Other entities** - Rename `data` → `config`
   - provider.ts
   - agent.ts
   - session.ts
   - task.ts
   - human.ts
   - information.ts
   - program.ts

### Phase 2: Manager Updates

3. **ModelManager** - Add embedding support
   - `createInferenceModel()` or keep `createModel()` with modelType param
   - `createEmbeddingModel()`
   - `embedWithModel(modelId, text)` → `{ vector, dimensions, usage }`
   - Type guards for inference vs embedding

4. **EmbeddingManager** - Use Model entities
   - Remove hardcoded MODEL constant
   - Accept modelId parameter
   - Validate model is embedding type
   - Store modelId on embedded nodes

### Phase 3: Test Updates

5. Update all `.test.ts` files
   - Change `node.data` → `node.config`
   - Add embedding model tests

### Phase 4: Spec/Doc Updates

6. Update specifications
   - `docs/specifications/*.md` - field naming

7. Create migration bead if needed for existing data

## Order of Operations

1. Update interfaces (all at once to avoid inconsistency)
2. Update managers
3. Update tests
4. Run tests, fix issues
5. Update docs

## Rollback

If issues arise, `git stash` or revert - this is a refactor, not new functionality (except embedding model support).
