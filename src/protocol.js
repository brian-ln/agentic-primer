/**
 * Universal Actor Protocol (UAP)
 *
 * All messages between actors follow this structure:
 * {
 *   protocol: "event.v1",    // Protocol version
 *   action: "append",         // Action to perform
 *   data: { ... }            // Action-specific data
 * }
 */

export const PROTOCOLS = {
  EVENT: "event.v1",
  FUNCTION: "function.v1",
  REGISTRY: "registry.v1",
  HTTP: "http.v1"
};

export const ACTIONS = {
  // Event log actions
  APPEND: "append",
  QUERY: "query",
  CHECKPOINT: "checkpoint",

  // Function registry actions
  REGISTER: "register",
  UNREGISTER: "unregister",
  LIST: "list",

  // Function execution actions
  EXECUTE: "execute",
  COMPLETE: "complete",
  ERROR: "error",

  // Pattern matching actions
  MATCH: "match",
  ROUTE: "route",

  // HTTP actions
  REQUEST: "request",
  RESPONSE: "response"
};

/**
 * Create a UAP message
 */
export function createMessage(protocol, action, data) {
  return {
    protocol,
    action,
    data,
    timestamp: new Date().toISOString()
  };
}

/**
 * Validate UAP message structure
 */
export function validateMessage(message) {
  if (!message || typeof message !== 'object') {
    return { valid: false, error: "Message must be an object" };
  }

  if (!message.protocol || typeof message.protocol !== 'string') {
    return { valid: false, error: "Message must have a protocol string" };
  }

  if (!message.action || typeof message.action !== 'string') {
    return { valid: false, error: "Message must have an action string" };
  }

  if (!message.data) {
    return { valid: false, error: "Message must have data" };
  }

  return { valid: true };
}
