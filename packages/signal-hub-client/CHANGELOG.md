# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2026-02-16

### Added

- Initial release of Signal Hub browser client
- WebSocket connection management with JWT authentication
- Automatic reconnection with exponential backoff
- Actor registration and lifecycle management
- Point-to-point messaging with tell/ask patterns
- Event-driven API (message, connected, disconnected, error, stateChange)
- Automatic heartbeat to prevent Cloudflare hibernation
- Message queuing during disconnection
- TypeScript type definitions
- Comprehensive documentation and examples
- Browser and Node.js support

### Features

- **Connection Management**
  - Connect/disconnect to Signal Hub
  - JWT authentication
  - Auto-reconnect with configurable backoff (1s to 30s)
  - Connection state tracking
  - Session management

- **Actor Management**
  - Register actors with capabilities and metadata
  - Unregister actors
  - Auto-generated browser addresses
  - Actor registry tracking

- **Messaging**
  - Fire-and-forget messaging (tell pattern)
  - Acknowledged messaging (ask pattern)
  - Flat payload structure for hub:send
  - Message TTL support
  - Trace ID support
  - Priority support

- **Event System**
  - Message events for incoming messages
  - Connection events (connected/disconnected)
  - State change events
  - Error events with codes

- **Reliability**
  - Message queue during disconnect
  - Automatic heartbeat (25s interval)
  - Heartbeat timeout detection
  - Pending ask tracking
  - Clean disconnection

### Documentation

- Comprehensive README with API reference
- Basic usage HTML example
- Node.js usage example
- Type definitions with JSDoc
- Protocol documentation references

### Technical Details

- Zero dependencies (vanilla JavaScript/TypeScript)
- ES modules for tree-shaking
- TypeScript strict mode
- Source maps and declaration maps
- Browser DOM APIs only
- Works with Node.js (with WebSocket polyfill)

[0.1.0]: https://github.com/BrianLN-AI/agentic-primer/releases/tag/signal-hub-client-v0.1.0
