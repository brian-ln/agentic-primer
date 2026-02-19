# Actor Auto-Validation

The base `Actor` class supports optional automatic payload validation using JSON Schema defined as class properties.

## Features

- **Zero-overhead when unused**: No validation overhead if schemas aren't defined
- **Opt-in/opt-out**: Enable or disable per-actor via `enableAutoValidation` flag
- **Co-located schemas**: Schemas defined directly in the actor class
- **Detailed errors**: Validation failures return descriptive error messages
- **Type-safe**: Works with TypeScript types and JSON Schema
- **Runtime configurable**: Schemas can be modified at runtime if needed

## Basic Usage

### Define Schemas in Your Actor

Define message schemas as a protected property in your actor class:

```typescript
import { Actor, type Message, type MessageResponse } from '@agentic-primer/actors';
import type { JSONSchema } from '@agentic-primer/actors';

class UserActor extends Actor {
  // Define schemas for message types this actor handles
  protected schemas = new Map<string, JSONSchema>([
    ['create-user', {
      type: 'object',
      properties: {
        name: { type: 'string', minLength: 1 },
        email: { type: 'string', pattern: '^[^@]+@[^@]+\\.[^@]+$' },
        age: { type: ['number', 'integer'], minimum: 0, maximum: 150 },
      },
      required: ['name', 'email'],
    }],
    ['update-user', {
      type: 'object',
      properties: {
        id: { type: 'string' },
        name: { type: 'string', minLength: 1 },
      },
      required: ['id'],
    }],
  ]);

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    // Payload is already validated when this is called
    switch (message.type) {
      case 'create-user':
        return this.handleCreate(message);
      case 'update-user':
        return this.handleUpdate(message);
      default:
        return createErrorResponse(message, `Unknown message type: ${message.type}`);
    }
  }

  private async handleCreate(message: Message): Promise<MessageResponse> {
    const { name, email, age } = message.payload;
    // payload is guaranteed to be valid here
    // ...
    return createResponse(message, { id: 'user-123', name, email, age });
  }
}
```

### Send Messages

When you send messages to actors with defined schemas, validation happens automatically:

```typescript
// Valid message - will be processed
await userActor.ask(address('user-actor'), 'create-user', {
  name: 'Alice',
  email: 'alice@example.com',
  age: 30,
});

// Invalid message - will return error response before reaching handleMessage()
const response = await userActor.ask(address('user-actor'), 'create-user', {
  name: '', // too short
  email: 'invalid-email', // pattern mismatch
  age: 200, // exceeds maximum
});

console.log(response.success); // false
console.log(response.error);
// "Invalid payload: $.name: String length 0 is less than minimum 1;
//  $.email: String does not match pattern: ^[^@]+@[^@]+\.[^@]+$;
//  $.age: Number 200 exceeds maximum 150"
```

## Schema Organization Patterns

### Pattern 1: Inline Schemas (Simple)

For simple actors, define schemas inline:

```typescript
class SimpleActor extends Actor {
  protected schemas = new Map([
    ['action', { type: 'object', properties: { id: { type: 'string' } } }],
  ]);
}
```

### Pattern 2: External Schema Constants (Reusable)

For shared or complex schemas, define them as constants:

```typescript
// schemas/user-schemas.ts
import type { JSONSchema } from '@agentic-primer/actors';

export const CREATE_USER_SCHEMA: JSONSchema = {
  type: 'object',
  properties: {
    name: { type: 'string', minLength: 1 },
    email: { type: 'string', pattern: '^[^@]+@[^@]+\\.[^@]+$' },
  },
  required: ['name', 'email'],
};

export const UPDATE_USER_SCHEMA: JSONSchema = {
  type: 'object',
  properties: {
    id: { type: 'string' },
    name: { type: 'string' },
  },
  required: ['id'],
};

// actors/user-actor.ts
import { CREATE_USER_SCHEMA, UPDATE_USER_SCHEMA } from '../schemas/user-schemas.ts';

class UserActor extends Actor {
  protected schemas = new Map([
    ['create-user', CREATE_USER_SCHEMA],
    ['update-user', UPDATE_USER_SCHEMA],
  ]);
}
```

### Pattern 3: Static Schemas (Shared Across Instances)

Use static properties to share schemas across all actor instances:

```typescript
class UserActor extends Actor {
  // Shared by all UserActor instances
  protected static MESSAGE_SCHEMAS = new Map<string, JSONSchema>([
    ['create-user', CREATE_USER_SCHEMA],
    ['update-user', UPDATE_USER_SCHEMA],
  ]);

  constructor(id: string, router: IMessageRouter) {
    super(id, router);
    this.schemas = UserActor.MESSAGE_SCHEMAS;
  }
}
```

### Pattern 4: Runtime Configuration

Modify schemas dynamically:

```typescript
class ConfigurableActor extends Actor {
  protected schemas = new Map<string, JSONSchema>();

  // Public API to configure validation
  addSchema(messageType: string, schema: JSONSchema): void {
    this.schemas.set(messageType, schema);
  }

  removeSchema(messageType: string): void {
    this.schemas.delete(messageType);
  }

  updateSchema(messageType: string, schema: JSONSchema): void {
    this.schemas.set(messageType, schema);
  }
}

// Usage
const actor = new ConfigurableActor('config', router);
actor.addSchema('dynamic-message', mySchema);
```

## Disabling Auto-Validation

### Disable for Entire Actor

```typescript
class ManualValidationActor extends Actor {
  constructor(id: string, router: IMessageRouter) {
    super(id, router);
    this.enableAutoValidation = false; // Disable auto-validation
  }

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    // Do your own validation here
    if (!this.isValid(message.payload)) {
      return createErrorResponse(message, 'Invalid payload');
    }
    // ...
  }
}
```

### Disable for Specific Message Types

```typescript
class SelectiveValidationActor extends Actor {
  protected schemas = new Map([
    ['validated-type', mySchema],
    // 'unvalidated-type' has no schema - won't be validated
  ]);

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    // 'validated-type' validated automatically
    // 'unvalidated-type' skips validation
  }
}
```

## Validation Error Format

Validation errors include the JSON path and description:

```text
Invalid payload: $.email: String does not match pattern: ^[^@]+@[^@]+\.[^@]+$; $.age: Number 200 exceeds maximum 150
```

Each error shows:
- `$.path`: JSONPath to the invalid field
- `description`: What validation rule failed

Multiple errors are concatenated with semicolons.

## Performance

- **No schemas defined**: Zero overhead (single `if (this.schemas)` check)
- **Schema defined**: O(1) map lookup + validation (microseconds for typical payloads)
- **Validation disabled**: Zero overhead (single `if (this.enableAutoValidation)` check)

## Migration from ActorWithIntrospection

If you're using `ActorWithIntrospection` with the `@accepts` decorator:

### Current (decorator-based):

```typescript
class ComplexActor extends ActorWithIntrospection {
  @accepts({
    description: 'Create a user',
    payload: { type: 'object', properties: { name: { type: 'string' } } },
    consequences: { category: 'write', sideEffects: [], canUndo: false, requiresConfirm: false },
  })
  async createUser(message: Message): Promise<MessageResponse> { }
}
```

### New (schema property):

```typescript
class SimpleActor extends Actor {
  protected schemas = new Map([
    ['createUser', { type: 'object', properties: { name: { type: 'string' } } }],
  ]);

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    if (message.type === 'createUser') {
      return this.handleCreate(message);
    }
  }
}
```

**When to use each:**
- **@accepts decorator**: Complex actors needing rich metadata (consequences, examples, introspection)
- **Schema property**: Simple actors just needing validation

Both can coexist in the same codebase.

## Best Practices

1. **Define schemas in actor class**: Keep schemas close to handlers, not in global state
2. **Use type unions for numbers**: JSON Schema distinguishes `integer` from `number`, use `{ type: ['number', 'integer'] }` to accept both
3. **Provide patterns for strings**: Use `pattern` for email, URLs, IDs to catch malformed data early
4. **Set reasonable limits**: Use `minLength`, `maxLength`, `minimum`, `maximum` to prevent attacks
5. **Extract complex schemas**: Put reusable schemas in a schemas/ directory
6. **Use static for shared schemas**: If all instances use the same schemas, make them static
7. **Keep schemas portable**: JSON Schema works across languages (TypeScript, Rust, Python, etc.)

## See Also

- [Actor Introspection Protocol](./src/introspection.ts) - Decorator-based validation with richer metadata
- [JSON Schema Validator](./src/schema-validator.ts) - Core validation implementation
- [Message Types](./src/message.ts) - Message and response structures
