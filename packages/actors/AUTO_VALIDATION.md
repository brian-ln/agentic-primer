# Actor Auto-Validation

The base `Actor` class supports optional automatic payload validation using JSON Schema.

## Features

- **Zero-overhead when unused**: No validation overhead if schemas aren't registered
- **Opt-in/opt-out**: Enable or disable per-actor via `enableAutoValidation` flag
- **Global registry**: Schemas registered once, applied automatically
- **Detailed errors**: Validation failures return descriptive error messages
- **Type-safe**: Works with TypeScript types and JSON Schema

## Basic Usage

### 1. Register Message Schemas

Register JSON Schema definitions for your message types at application startup:

```typescript
import { registerMessageSchema } from '@agentic-primer/actors';

// Register schemas for your message types
registerMessageSchema('create-user', {
  type: 'object',
  properties: {
    name: { type: 'string', minLength: 1 },
    email: { type: 'string', pattern: '^[^@]+@[^@]+\\.[^@]+$' },
    age: { type: ['number', 'integer'], minimum: 0, maximum: 150 },
  },
  required: ['name', 'email'],
});

registerMessageSchema('update-user', {
  type: 'object',
  properties: {
    id: { type: 'string' },
    name: { type: 'string', minLength: 1 },
  },
  required: ['id'],
});
```

### 2. Implement Your Actor

Extend `Actor` and implement `handleMessage()` (not `receive()`):

```typescript
import { Actor, type Message, type MessageResponse } from '@agentic-primer/actors';

class UserActor extends Actor {
  // Auto-validation is enabled by default
  // Set this.enableAutoValidation = false in constructor to disable

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

  private async handleUpdate(message: Message): Promise<MessageResponse> {
    const { id, name } = message.payload;
    // payload is guaranteed to be valid here
    // ...
    return createResponse(message, { id, name });
  }
}
```

### 3. Send Messages

When you send messages to actors with registered schemas, validation happens automatically:

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
console.log(response.error); // "Invalid payload: $.name: String length 0 is less than minimum 1; ..."
```

## Disabling Auto-Validation

To opt out of auto-validation for a specific actor:

```typescript
class ManualValidationActor extends Actor {
  constructor(id: string, router: IMessageRouter) {
    super(id, router);
    this.enableAutoValidation = false; // Disable auto-validation
  }

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    // Do your own validation here
    // ...
  }
}
```

## Checking Schema Registration

Query the global registry to check if schemas are registered:

```typescript
import { hasMessageSchema, getMessageSchema } from '@agentic-primer/actors';

if (hasMessageSchema('create-user')) {
  const schema = getMessageSchema('create-user');
  console.log('Schema registered:', schema);
}
```

## Generated Validators

For TypeScript-first projects, you can generate validator registrations from your type definitions:

1. Define your message payload types:

```typescript
interface CreateUserPayload {
  name: string;
  email: string;
  age: number;
}
```

2. Generate JSON Schema from TypeScript types (using tools like `typescript-json-schema` or `@anatine/zod-to-openapi`)

3. Generate registration code that calls `registerMessageSchema()` at startup

4. Import and call the registration function in your app initialization

## Migration from @accepts Decorator

If you're using `ActorWithIntrospection` and the `@accepts` decorator, you have two options:

### Option 1: Keep using @accepts (Recommended for complex actors)

```typescript
import { ActorWithIntrospection, accepts } from '@agentic-primer/actors';

class ComplexActor extends ActorWithIntrospection {
  @accepts({
    description: 'Create a new user',
    payload: { type: 'object', properties: { name: { type: 'string' } } },
    consequences: {
      category: 'write',
      sideEffects: ['database-write'],
      canUndo: false,
      requiresConfirm: false,
    },
  })
  async createUser(message: Message): Promise<MessageResponse> {
    // ...
  }
}
```

### Option 2: Migrate to base Actor with auto-validation (Simpler)

```typescript
import { Actor } from '@agentic-primer/actors';

// Register at startup
registerMessageSchema('create', createUserSchema);

class SimpleActor extends Actor {
  protected async handleMessage(message: Message): Promise<MessageResponse> {
    if (message.type === 'create') {
      return this.handleCreate(message);
    }
    // ...
  }
}
```

## Validation Error Format

Validation errors include the JSON path and description:

```
Invalid payload: $.email: String does not match pattern: ^[^@]+@[^@]+\.[^@]+$; $.age: Number 200 exceeds maximum 150
```

Each error shows:
- `$.path`: JSONPath to the invalid field
- `description`: What validation rule failed

## Performance

- **Schema lookup**: O(1) hash map lookup per message
- **No schema registered**: Single map lookup, no validation overhead
- **Schema registered**: Full JSON Schema validation (microseconds for typical payloads)
- **Validation disabled**: Zero overhead, bypasses both lookup and validation

## Best Practices

1. **Register schemas at startup**: Call `registerMessageSchema()` during application initialization, not in hot paths
2. **Use type unions for numbers**: JSON Schema distinguishes `integer` from `number`, use `{ type: ['number', 'integer'] }` to accept both
3. **Provide patterns for strings**: Use `pattern` for email, URLs, IDs to catch malformed data early
4. **Set reasonable limits**: Use `minLength`, `maxLength`, `minimum`, `maximum` to prevent attacks
5. **Keep schemas portable**: JSON Schema can be used across languages (TypeScript, Rust, Python, etc.)

## See Also

- [Actor Introspection Protocol](./src/introspection.ts) - Decorator-based validation with richer metadata
- [JSON Schema Validator](./src/schema-validator.ts) - Core validation implementation
- [Message Types](./src/message.ts) - Message and response structures
