# Shared Schemas

Core JSON Schema definitions used across all Signal Hub domains.

## Schema Index

### Core Schemas

| Schema | Description | Used By |
|--------|-------------|---------|
| [shared-message.schema.json](./shared-message.schema.json) | Universal message envelope for all communication | All domains |
| [canonical-address.schema.json](./canonical-address.schema.json) | Actor addressing format (`runtime/actor-id`) | All domains |
| [error-response.schema.json](./error-response.schema.json) | Standard error message format | All domains |

## Schema Relationships

```
SharedMessage (envelope)
    ├─→ from: CanonicalAddress
    ├─→ to: CanonicalAddress
    └─→ payload: <domain-specific>

ErrorResponse (extends SharedMessage)
    ├─→ type: "hub:error"
    └─→ payload: { code, message, details }
```

## Validation

All schemas follow JSON Schema Draft 07 specification.

### Validate with AJV (Node.js)

```bash
npm install -g ajv-cli
ajv validate -s shared-message.schema.json -d example-message.json
```

### Validate with Python

```bash
pip install jsonschema
python -c "
import json
from jsonschema import validate, Draft7Validator

with open('shared-message.schema.json') as f:
    schema = json.load(f)

with open('example-message.json') as f:
    data = json.load(f)

validate(instance=data, schema=schema)
print('✓ Valid')
"
```

## Usage in Code

### TypeScript

```typescript
import Ajv from 'ajv';
import sharedMessageSchema from './schemas/shared-message.schema.json';

const ajv = new Ajv();
const validate = ajv.compile(sharedMessageSchema);

const message = {
  type: 'hub:connect',
  from: 'browser/client-ui',
  to: 'cloudflare/signal-hub',
  payload: { version: '1.0' }
};

if (validate(message)) {
  console.log('✓ Valid message');
} else {
  console.error('✗ Invalid:', validate.errors);
}
```

### Python

```python
import json
from jsonschema import validate

with open('schemas/shared-message.schema.json') as f:
    schema = json.load(f)

message = {
    "type": "hub:connect",
    "from": "browser/client-ui",
    "to": "cloudflare/signal-hub",
    "payload": {"version": "1.0"}
}

validate(instance=message, schema=schema)  # Raises exception if invalid
```

## Cross-References

- Domain-specific schemas in `../*/schemas/` directories
- Protocol definitions in `../*/protocol.json` files
- Full specifications in `../*/README.md` files
