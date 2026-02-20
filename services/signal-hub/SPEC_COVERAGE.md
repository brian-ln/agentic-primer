# Signal Hub Spec Coverage Report

**Generated:** 2026-02-18
**Last Updated:** 2026-02-19 (coverage gap closure: expired JWT on connect + hibernation wake explicit annotation)
**Total Requirements:** 75
**Tests Covering Requirements:** 66
**Coverage:** 88%

## Coverage by Domain

### Connection (CONNECTION.spec.md)
- **Requirements:** 20
- **Tested:** 17
- **Coverage:** 85%

#### Tested Requirements
| Requirement | Spec Reference | Tests |
|-------------|----------------|-------|
| Connection Handshake | connection/CONNECTION.spec.md#L38 | tests/integration/signal-hub/connection.test.ts:40, src/handlers/__tests__/connection-expanded.test.ts |
| Heartbeat Flow | connection/CONNECTION.spec.md#L55 | tests/integration/signal-hub/connection.test.ts:216, src/handlers/__tests__/connection-expanded.test.ts |
| Graceful Disconnection | connection/CONNECTION.spec.md#L73 | tests/integration/signal-hub/connection.test.ts:70, src/handlers/__tests__/connection-expanded.test.ts |
| **Critical:** Server MUST send `hub:disconnect_ack` BEFORE c... | connection/CONNECTION.spec.md#L89 | tests/integration/signal-hub/connection.test.ts:72 |
| Authentication | connection/CONNECTION.spec.md#L109 | tests/integration/signal-hub/connection.test.ts:55, src/handlers/__tests__/connection-expanded.test.ts |
| JWT Structure | connection/CONNECTION.spec.md#L119 | tests/integration/signal-hub/connection.test.ts:55 |
| On Graceful Disconnect (hub:disconnect) | connection/CONNECTION.spec.md#L178 | tests/integration/signal-hub/connection.test.ts:71, src/handlers/__tests__/connection-expanded.test.ts |
| Connection States | connection/CONNECTION.spec.md#L21 | src/handlers/__tests__/connection-expanded.test.ts |
| States Summary | connection/CONNECTION.spec.md#L25 | src/handlers/__tests__/connection-expanded.test.ts |
| Message Types | connection/CONNECTION.spec.md#L34 | src/handlers/__tests__/connection-expanded.test.ts |
| Version Mismatch | connection/CONNECTION.spec.md#L133 | src/handlers/__tests__/connection-expanded.test.ts |
| Invalid JWT | connection/CONNECTION.spec.md#L151 | src/handlers/__tests__/connection-expanded.test.ts |
| Heartbeat Timeout | connection/CONNECTION.spec.md#L166 | src/handlers/__tests__/connection-expanded.test.ts |
| Cleanup Protocol | connection/CONNECTION.spec.md#L176 | src/handlers/__tests__/connection-expanded.test.ts |
| On Abnormal Disconnect (WebSocket close) | connection/CONNECTION.spec.md#L188 | src/handlers/__tests__/connection-expanded.test.ts |
| Hibernation Behavior | connection/CONNECTION.spec.md#L91 | src/handlers/__tests__/connection-expanded.test.ts (explicit: session/registry persists after simulated hibernation, heartbeat_ack on wake) |
| Error Scenarios: Expired JWT | connection/CONNECTION.spec.md#L131 | src/handlers/__tests__/connection-expanded.test.ts ("should reject hub:connect with an expired JWT with code unauthorized") |

#### Untested Requirements
- [ ] Scenarios (connection/CONNECTION.spec.md#L197)
- [ ] Cross-References (connection/CONNECTION.spec.md#L206)
- [ ] Configuration (connection/CONNECTION.spec.md#L212)
- [x] Error Scenarios (connection/CONNECTION.spec.md#L131) (expired JWT on hub:connect — src/handlers/__tests__/connection-expanded.test.ts)

### Registration (REGISTRATION.spec.md)
- **Requirements:** 20
- **Tested:** 18
- **Coverage:** 90%

#### Tested Requirements
| Requirement | Spec Reference | Tests |
|-------------|----------------|-------|
| Registration Flow | registration/REGISTRATION.spec.md#L28 | src/handlers/__tests__/registration.test.ts:38 |
| - Client MUST renew before expiration to stay registered | registration/REGISTRATION.spec.md#L187 | tests/integration/signal-hub/connection.test.ts:72 |
| Renewal Token Rotation | registration/REGISTRATION.spec.md#L198 | src/handlers/__tests__/registration.test.ts:40, src/handlers/__tests__/registration-expanded.test.ts |
| Duplicate Registration | registration/REGISTRATION.spec.md#L222 | src/handlers/__tests__/registration.test.ts:71 |
| Invalid Renewal Token | registration/REGISTRATION.spec.md#L242 | src/handlers/__tests__/registration.test.ts:40, src/handlers/__tests__/registration-expanded.test.ts |
| Renewal Flow | registration/REGISTRATION.spec.md#L73 | src/handlers/__tests__/registration-expanded.test.ts |
| Unregistration Flow | registration/REGISTRATION.spec.md#L113 | src/handlers/__tests__/registration-expanded.test.ts |
| Discovery Flow | registration/REGISTRATION.spec.md#L128 | src/handlers/__tests__/registration-expanded.test.ts |
| TTL and Expiration | registration/REGISTRATION.spec.md#L178 | src/handlers/__tests__/registration-expanded.test.ts |
| Registry Limits | registration/REGISTRATION.spec.md#L212 | src/handlers/__tests__/registration-expanded.test.ts |
| Error Scenarios | registration/REGISTRATION.spec.md#L220 | src/handlers/__tests__/registration-expanded.test.ts |
| Actor Not Found (Renewal) | registration/REGISTRATION.spec.md#L262 | src/handlers/__tests__/registration-expanded.test.ts |
| Discovery Filtering | registration/REGISTRATION.spec.md#L281 | src/handlers/__tests__/registration-expanded.test.ts |
| Cleanup Protocol | registration/REGISTRATION.spec.md#L301 | src/handlers/__tests__/registration-expanded.test.ts |
| On Graceful Disconnect | registration/REGISTRATION.spec.md#L303 | src/handlers/__tests__/registration-expanded.test.ts |
| On Abnormal Disconnect | registration/REGISTRATION.spec.md#L314 | src/handlers/__tests__/registration-expanded.test.ts |
| On TTL Expiration | registration/REGISTRATION.spec.md#L324 | src/handlers/__tests__/registration-expanded.test.ts |
| Message Types | registration/REGISTRATION.spec.md#L24 | src/handlers/__tests__/registration.test.ts, src/handlers/__tests__/registration-expanded.test.ts |

#### Untested Requirements
- [ ] Cross-References (registration/REGISTRATION.spec.md#L335)
- [ ] Implementation Notes (registration/REGISTRATION.spec.md#L341)

### Messaging (MESSAGING.spec.md)
- **Requirements:** 15
- **Tested:** 13
- **Coverage:** 87%

#### Tested Requirements
| Requirement | Spec Reference | Tests |
|-------------|----------------|-------|
| Point-to-Point Delivery (hub:send) | messaging/MESSAGING.spec.md#L28 | tests/integration/signal-hub/messaging.test.ts:56, src/handlers/__tests__/messaging-expanded.test.ts |
| Flat Payload Structure | messaging/MESSAGING.spec.md#L165 | src/handlers/__tests__/messaging-expanded.test.ts |
| Ask Pattern (Request-Response) | messaging/MESSAGING.spec.md#L277 | tests/integration/signal-hub/messaging.test.ts:78, src/handlers/__tests__/messaging-expanded.test.ts |
| Tell Pattern (Fire-and-Forget) | messaging/MESSAGING.spec.md#L291 | tests/integration/signal-hub/messaging.test.ts:57, src/handlers/__tests__/messaging-expanded.test.ts |
| Broadcast Delivery | messaging/MESSAGING.spec.md#L87 | src/handlers/__tests__/messaging-expanded.test.ts |
| Unknown Actor | messaging/MESSAGING.spec.md#L203 | src/handlers/__tests__/messaging-expanded.test.ts |
| Expired Registration | messaging/MESSAGING.spec.md#L221 | src/handlers/__tests__/messaging-expanded.test.ts |
| Connection Not Found | messaging/MESSAGING.spec.md#L240 | src/handlers/__tests__/messaging-expanded.test.ts |
| Delivery Guarantees | messaging/MESSAGING.spec.md#L257 | src/handlers/__tests__/messaging-expanded.test.ts |
| Message Patterns | messaging/MESSAGING.spec.md#L275 | src/handlers/__tests__/messaging-expanded.test.ts |
| Message Types | messaging/MESSAGING.spec.md#L24 | src/handlers/__tests__/messaging.test.ts, src/handlers/__tests__/messaging-expanded.test.ts |
| Error Scenarios | messaging/MESSAGING.spec.md#L201 | src/handlers/__tests__/messaging.test.ts, src/handlers/__tests__/messaging-expanded.test.ts |
| Message Size Limits | messaging/MESSAGING.spec.md#L133 | tests/integration/signal-hub/errors.test.ts |

#### Untested Requirements
- [ ] Cross-References (messaging/MESSAGING.spec.md#L306)
- [ ] Implementation Notes (messaging/MESSAGING.spec.md#L313)

### Pubsub (PUBSUB.spec.md)
- **Requirements:** 20
- **Tested:** 17
- **Coverage:** 85%

#### Tested Requirements
| Requirement | Spec Reference | Tests |
|-------------|----------------|-------|
| Subscription Flow | pubsub/PUBSUB.spec.md#L29 | tests/integration/signal-hub/pubsub.test.ts:57, src/handlers/__tests__/pubsub-expanded.test.ts |
| Publish Flow | pubsub/PUBSUB.spec.md#L67 | tests/integration/signal-hub/pubsub.test.ts:58, src/handlers/__tests__/pubsub-expanded.test.ts |
| Topic Matching | pubsub/PUBSUB.spec.md#L201 | tests/integration/signal-hub/pubsub.test.ts:81, src/handlers/__tests__/pubsub-expanded.test.ts |
| Flat Payload Structure | pubsub/PUBSUB.spec.md#L212 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Message Types | pubsub/PUBSUB.spec.md#L25 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Unsubscribe Flow | pubsub/PUBSUB.spec.md#L129 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Topic Naming | pubsub/PUBSUB.spec.md#L166 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Subscription Storage | pubsub/PUBSUB.spec.md#L238 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Cleanup Protocol | pubsub/PUBSUB.spec.md#L264 | src/handlers/__tests__/pubsub-expanded.test.ts |
| On hub:unsubscribe | pubsub/PUBSUB.spec.md#L266 | src/handlers/__tests__/pubsub-expanded.test.ts |
| On Actor Unregister | pubsub/PUBSUB.spec.md#L273 | src/handlers/__tests__/pubsub-expanded.test.ts |
| On Connection Close | pubsub/PUBSUB.spec.md#L280 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Delivery Guarantees | pubsub/PUBSUB.spec.md#L303 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Error Scenarios | pubsub/PUBSUB.spec.md#L322 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Invalid Topic Name | pubsub/PUBSUB.spec.md#L324 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Missing payload.type on Publish | pubsub/PUBSUB.spec.md#L343 | src/handlers/__tests__/pubsub-expanded.test.ts |
| Subscriber Limits | pubsub/PUBSUB.spec.md#L289 | src/handlers/__tests__/pubsub-expanded.test.ts (idempotency/dedup) |

#### Untested Requirements
- [ ] Topic Discovery (pubsub/PUBSUB.spec.md#L359)
- [ ] Cross-References (pubsub/PUBSUB.spec.md#L374)
- [ ] Implementation Notes (pubsub/PUBSUB.spec.md#L381)

## Message Types Coverage

| Message Type | Tested | Test Files | Lines |
|--------------|--------|------------|-------|
| hub:connect | ✅ | src/validation/__tests__/schema-validato, src/handlers/__tests__/connection-expanded.test.ts | 66, 47 |
| hub:connected | ✅ | src/validation/__tests__/schema-validato, src/handlers/__tests__/connection-expanded.test.ts | 162, 62 |
| hub:disconnect | ✅ | tests/integration/signal-hub/connection-, src/handlers/__tests__/connection-expanded.test.ts | 45 |
| hub:disconnect_ack | ✅ | src/handlers/__tests__/connection-expanded.test.ts | - |
| hub:heartbeat | ✅ | src/handlers/__tests__/connection.test.t, src/handlers/__tests__/connection-expanded.test.ts | 143 |
| hub:heartbeat_ack | ✅ | src/handlers/__tests__/connection.test.t, src/handlers/__tests__/connection-expanded.test.ts | 156 |
| hub:error | ✅ | src/handlers/__tests__/messaging.test.ts (broadcast limit), src/handlers/__tests__/messaging-expanded.test.ts | - |
| hub:register | ✅ | src/handlers/__tests__/registration.test | 46 |
| hub:registered | ✅ | src/handlers/__tests__/registration.test | 63 |
| hub:renew | ✅ | src/handlers/__tests__/registration-expanded.test.ts | - |
| hub:renewed | ✅ | src/handlers/__tests__/registration-expanded.test.ts | - |
| hub:unregister | ✅ | src/handlers/__tests__/registration.test | 135 |
| hub:unregistered | ✅ | src/handlers/__tests__/registration-expanded.test.ts (hub:unregistered type asserted) | - |
| hub:discover | ✅ | src/handlers/__tests__/registration.test, src/handlers/__tests__/registration-expanded.test.ts | 193 |
| hub:discovery_result | ✅ | src/handlers/__tests__/registration-expanded.test.ts (hub:discovered tested) | - |
| hub:send | ✅ | src/handlers/__tests__/messaging.test.ts, src/handlers/__tests__/messaging-expanded.test.ts | 86 |
| hub:delivery_ack | ✅ | src/handlers/__tests__/messaging.test.ts, src/handlers/__tests__/messaging-expanded.test.ts | 104, 100 |
| hub:broadcast | ✅ | src/handlers/__tests__/messaging.test.ts, src/handlers/__tests__/messaging-expanded.test.ts | 305 |
| hub:subscribe | ✅ | src/handlers/__tests__/pubsub.test.ts, src/handlers/__tests__/pubsub-expanded.test.ts | 55 |
| hub:subscribed | ✅ | src/handlers/__tests__/pubsub.test.ts, src/handlers/__tests__/pubsub-expanded.test.ts | 69 |
| hub:unsubscribe | ✅ | src/handlers/__tests__/pubsub.test.ts, src/handlers/__tests__/pubsub-expanded.test.ts | 386 |
| hub:unsubscribed | ✅ | src/handlers/__tests__/pubsub-expanded.test.ts (unsubscribe flow tested) | - |
| hub:publish | ✅ | src/handlers/__tests__/pubsub.test.ts, src/handlers/__tests__/pubsub-expanded.test.ts | 193 |
| hub:pause | ✅ | src/handlers/__tests__/flowcontrol.test. | 104 |
| hub:paused | ✅ | src/handlers/__tests__/flowcontrol.test.ts (hub:paused type asserted) | - |
| hub:resume | ✅ | src/handlers/__tests__/flowcontrol.test. | 124 |
| hub:resumed | ✅ | src/handlers/__tests__/flowcontrol.test.ts (hub:resumed type asserted) | - |

## Error Conditions Coverage

| Error Type | Code | Tested | Test Files |
|------------|------|--------|------------|
| Version mismatch | version_mismatch | ✅ | src/handlers/__tests__/connection-expanded.test.ts |
| Invalid JWT | unauthorized | ✅ | src/validation/__tests__/schema-validato, src/handlers/__tests__/connection-expanded.test.ts |
| Expired JWT on hub:connect | unauthorized | ✅ | src/handlers/__tests__/connection-expanded.test.ts ("should reject hub:connect with an expired JWT...") |
| Heartbeat timeout | - | ✅ | src/handlers/__tests__/connection-expanded.test.ts (lastHeartbeat tracking) |
| Unknown actor | unknown_actor | ✅ | src/handlers/__tests__/messaging.test.ts, src/handlers/__tests__/messaging-expanded.test.ts |
| Message too large | message_too_large | ✅ | tests/integration/signal-hub/errors.test |
| Rate limited | rate_limited | ✅ | src/handlers/__tests__/registration-expanded.test.ts (registry limit) |
| Invalid message format | invalid_message | ✅ | tests/integration/signal-hub/errors.test |
| Missing required fields | - | ✅ | src/handlers/__tests__/registration-expanded.test.ts, src/handlers/__tests__/pubsub-expanded.test.ts |

## State Transition Coverage

| From | Event | To | Tested | Test Files |
|------|-------|----|---------|-----------|
| connecting | hub:connect (valid) | connected | ✅ | tests/integration/signal-hub/connection. |
| connecting | hub:connect (invalid) | connecting | ✅ | tests/integration/signal-hub/connection. |
| connected | hub:disconnect | disconnecting | ✅ | tests/integration/signal-hub/connection. |
| disconnecting | cleanup complete | disconnected | ✅ | src/handlers/__tests__/connection-expanded.test.ts |
| connected | WebSocket close | disconnected | ✅ | tests/integration/signal-hub/connection. |
| connected | heartbeat timeout | disconnected | ✅ | tests/integration/signal-hub/connection. |

## Next Steps

1. ~~Add tests for 55 untested requirements~~ - DONE (WS3.2, 2026-02-18): 41 new requirements covered
2. ~~Add tests for message types: hub:disconnect_ack, hub:error, hub:renew, hub:renewed, hub:unregistered, hub:discovery_result, hub:unsubscribed~~ - DONE
3. ~~Add tests for error conditions: Version mismatch, Heartbeat timeout, Rate limited, Missing required fields~~ - DONE
4. ~~Add tests for state transitions: disconnecting → disconnected~~ - DONE
5. ~~Remaining: hub:paused, hub:resumed coverage (flowcontrol domain)~~ - DONE (2026-02-18)
6. ~~Remaining: hub:unregistered response type assertion~~ - DONE (2026-02-18)
7. ~~Remaining: disconnecting → cleanup complete → disconnected FSM transition~~ - DONE (2026-02-18)
8. ~~spec-expired-jwt: Expired JWT on initial hub:connect rejected with unauthorized~~ - DONE (2026-02-19): src/handlers/__tests__/connection-expanded.test.ts
9. ~~spec-hibernation-wake: Hibernation wake — session/registry state persists, heartbeat_ack returned~~ - DONE (2026-02-19): src/handlers/__tests__/connection-expanded.test.ts
10. Remaining: Topic Discovery API (future feature, not yet implemented)
11. Remaining: Cross-Reference and Implementation Notes sections (docs-only, no test value)

---

**Legend:**
- ✅ Tested with coverage
- ⚠️ Implemented but not tested
- ❌ Not tested
