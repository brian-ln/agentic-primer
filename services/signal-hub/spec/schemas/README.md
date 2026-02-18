# Shared Schemas

Core JSON Schema definitions used across all Signal Hub domains.

## Schema Files

### `shared-message.schema.json`
The universal message envelope for all Signal Hub communication. Every message sent through the hub uses this format.

**Key Properties:**
- `type` - Message type identifier (e.g., `hub:connect`, `hub:send`)
- `from` - Sender's canonical address
- `to` - Recipient's canonical address
- `payload` - Message-specific payload
- `pattern` - Optional routing pattern (`request`, `notify`, `response`)
- `timestamp` - Epoch milliseconds
- `correlationId` - For request-response correlation
- `ttl` - Time-to-live in milliseconds

### `canonical-address.schema.json`
Actor addressing format across all runtimes.

**Format:** `<runtime>/<actor-id>`

**Pattern:** `^[a-z0-9-]+/[a-z0-9-]+$`

**Examples:**
- `browser/client-ui` - Browser-based UI actor
- `seag/agent-1` - SEAG inference agent
- `cloudflare/signal-hub` - Signal Hub itself

### `error-response.schema.json`
Standard error response format for all `hub:error` messages.

**Error Codes:**
- `version_mismatch` - Protocol version incompatible
- `unauthorized` - JWT invalid or missing
- `rate_limited` - Exceeded 100 msg/min
- `unknown_actor` - Target actor not registered
- `message_too_large` - Message exceeds 512KB
- `message_expired` - TTL exceeded
- `timeout` - Operation timed out
- `internal_error` - Server-side error

## Usage

These schemas are referenced by domain-specific schemas and protocol definitions using JSON Schema `$ref`:

```json
{
  "type": "object",
  "properties": {
    "actorAddress": {
      "$ref": "../../schemas/canonical-address.schema.json#/definitions/CanonicalAddress"
    }
  }
}
```

## Validation

Validate JSON against these schemas using any JSON Schema validator:

```bash
# Using ajv-cli
npx ajv validate -s shared-message.schema.json -d example-message.json
```

## Source

These schemas are derived from `@agentic-primer/protocols/shared-message` to ensure cross-runtime compatibility between Browser ↔ Cloudflare ↔ SEAG.
