# Event System - Detailed Message Flows

This document shows the detailed message sequences and data flows through the Event System.

---

## Flow 1: HTTP Event Emission (Complete Trace)

### Step-by-Step with UAP Messages

```
┌──────────┐  ┌────────────┐  ┌───────────┐  ┌──────────┐  ┌──────────┐
│  Client  │  │HTTPServer  │  │   Loop    │  │EventLog  │  │  Disk    │
│          │  │   Actor    │  │Prevention │  │  Actor   │  │ (JSONL)  │
└────┬─────┘  └─────┬──────┘  └─────┬─────┘  └────┬─────┘  └────┬─────┘
     │              │               │              │              │
     │ POST /events │               │              │              │
     │ {type:"user.login",          │              │              │
     │  data:{userId:"alice"}}      │              │              │
     │──────────────►               │              │              │
     │              │               │              │              │
     │              │ 1. Parse JSON │              │              │
     │              │──────         │              │              │
     │              │     │         │              │              │
     │              │◄────          │              │              │
     │              │               │              │              │
     │              │ Parsed:       │              │              │
     │              │ {             │              │              │
     │              │   type: "user.login",        │              │
     │              │   data: {userId: "alice"}    │              │
     │              │ }             │              │              │
     │              │               │              │              │
     │              │ 2. Check loop prevention     │              │
     │              │───────────────────────►      │              │
     │              │               │              │              │
     │              │               │ Layer 1: Depth              │
     │              │               │ depth = 0 < 50 ✓            │
     │              │               │              │              │
     │              │               │ Layer 2: Fingerprint        │
     │              │               │ hash = sha256(...)          │
     │              │               │ not seen before ✓           │
     │              │               │              │              │
     │              │               │ Layer 3: Ancestry           │
     │              │               │ no parent, ok ✓             │
     │              │               │              │              │
     │              │               │ Layer 4: Circuit            │
     │              │               │ N/A (no function) ✓         │
     │              │               │              │              │
     │              │ {allowed: true}              │              │
     │              │◄──────────────────────       │              │
     │              │               │              │              │
     │              │ 3. Create UAP message        │              │
     │              │──────         │              │              │
     │              │     │         │              │              │
     │              │◄────          │              │              │
     │              │               │              │              │
     │              │ UAP Message:  │              │              │
     │              │ {             │              │              │
     │              │   protocol: "event.v1",      │              │
     │              │   action: "append",          │              │
     │              │   data: {     │              │              │
     │              │     type: "user.login",      │              │
     │              │     data: {userId: "alice"}  │              │
     │              │   },          │              │              │
     │              │   timestamp: "2026-01-10..." │              │
     │              │ }             │              │              │
     │              │               │              │              │
     │              │ 4. Send to EventLogActor     │              │
     │              │───────────────────────────────►             │
     │              │               │              │              │
     │              │               │              │ 5. Validate UAP
     │              │               │              │──────        │
     │              │               │              │     │        │
     │              │               │              │◄────         │
     │              │               │              │              │
     │              │               │              │ Valid ✓      │
     │              │               │              │              │
     │              │               │              │ 6. Enrich event
     │              │               │              │──────        │
     │              │               │              │     │        │
     │              │               │              │◄────         │
     │              │               │              │              │
     │              │               │              │ Enriched:    │
     │              │               │              │ {            │
     │              │               │              │   id: "evt_01KEM...",
     │              │               │              │   timestamp: "2026...",
     │              │               │              │   type: "user.login",
     │              │               │              │   data: {...},
     │              │               │              │   metadata: {
     │              │               │              │     source: "http",
     │              │               │              │     depth: 0,
     │              │               │              │     ...     │
     │              │               │              │   }         │
     │              │               │              │ }           │
     │              │               │              │             │
     │              │               │              │ 7. Format JSONL
     │              │               │              │──────       │
     │              │               │              │     │       │
     │              │               │              │◄────        │
     │              │               │              │             │
     │              │               │              │ Line:       │
     │              │               │              │ '{"id":"evt_...'
     │              │               │              │             │
     │              │               │              │ 8. Write to disk
     │              │               │              │─────────────────►
     │              │               │              │             │
     │              │               │              │          Append
     │              │               │              │          to file
     │              │               │              │             │
     │              │               │              │ Success     │
     │              │               │              │◄─────────────────
     │              │               │              │             │
     │              │               │              │ 9. Increment counter
     │              │               │              │──────       │
     │              │               │              │     │       │
     │              │               │              │◄────        │
     │              │               │              │             │
     │              │               │              │ eventCount = 1
     │              │               │              │             │
     │              │               │              │ 10. Return UAP response
     │              │               │              │◄────────────────────────
     │              │◄──────────────────────────────             │
     │              │               │              │             │
     │              │ Response:     │              │             │
     │              │ {             │              │             │
     │              │   success: true,             │             │
     │              │   eventId: "evt_01KEM...",   │             │
     │              │   eventCount: 1              │             │
     │              │ }             │              │             │
     │              │               │              │             │
     │              │ 11. Format HTTP response     │             │
     │              │──────         │              │             │
     │              │     │         │              │             │
     │              │◄────          │              │             │
     │              │               │              │             │
     │ 201 Created  │               │              │             │
     │ {            │               │              │             │
     │   "success": true,           │             │             │
     │   "eventId": "evt_01KEM...", │             │             │
     │   "eventCount": 1            │             │             │
     │ }            │               │              │             │
     │◄──────────────               │              │             │
     │              │               │              │             │
```

---

## Flow 2: Pattern Matching and Function Execution

### Complete Sequence with All Actors

```
┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐
│EventLog  │ │Pattern   │ │Function  │ │Function  │ │  Loop    │ │EventLog  │
│ Actor    │ │Matcher   │ │Registry  │ │Executor  │ │Prevention│ │  Actor   │
└────┬─────┘ └────┬─────┘ └────┬─────┘ └────┬─────┘ └────┬─────┘ └────┬─────┘
     │            │            │            │            │            │
     │ 1. Event appended       │            │            │            │
     │ type: "user.login"      │            │            │            │
     │────────────►            │            │            │            │
     │            │            │            │            │            │
     │            │ 2. Notify: new event    │            │            │
     │            │────────────►            │            │            │
     │            │            │            │            │            │
     │            │ 3. Match patterns       │            │            │
     │            │            │            │            │            │
     │            │ For each registered pattern:         │            │
     │            │ - Evaluate predicate    │            │            │
     │            │ - Check: event.type === "user.login" │            │
     │            │            │            │            │            │
     │            │ Matches:   │            │            │            │
     │            │ - patternId: "user-login"            │            │
     │            │ - priority: 10          │            │            │
     │            │ - metadata: {actions: ["send-email"]}            │
     │            │            │            │            │            │
     │            │ 4. For each matched pattern          │            │
     │            │ Get associated functions            │            │
     │            │────────────────────────►│            │            │
     │            │            │            │            │            │
     │            │            │ 5. Lookup function metadata          │
     │            │            │ functionId: "send-email"             │
     │            │            │──────      │            │            │
     │            │            │     │      │            │            │
     │            │            │◄────       │            │            │
     │            │            │            │            │            │
     │            │            │ Function:  │            │            │
     │            │            │ {          │            │            │
     │            │            │   type: "code",         │            │
     │            │            │   path: "/functions/send-email.js"  │
     │            │            │ }          │            │            │
     │            │            │            │            │            │
     │            │ Function metadata        │            │            │
     │            │◄────────────────────────│            │            │
     │            │            │            │            │            │
     │            │ 6. Create UAP execute message        │            │
     │            │──────      │            │            │            │
     │            │     │      │            │            │            │
     │            │◄────       │            │            │            │
     │            │            │            │            │            │
     │            │ UAP Message:            │            │            │
     │            │ {          │            │            │            │
     │            │   protocol: "function.v1",           │            │
     │            │   action: "execute",    │            │            │
     │            │   data: {  │            │            │            │
     │            │     functionId: "send-email",        │            │
     │            │     functionPath: "/...",            │            │
     │            │     functionType: "code",            │            │
     │            │     event: {...original event...}    │            │
     │            │   }        │            │            │            │
     │            │ }          │            │            │            │
     │            │            │            │            │            │
     │            │ 7. Send to FunctionExecutor          │            │
     │            │────────────────────────────────────►│            │
     │            │            │            │            │            │
     │            │            │            │ 8. Validate UAP         │
     │            │            │            │──────      │            │
     │            │            │            │     │      │            │
     │            │            │            │◄────       │            │
     │            │            │            │            │            │
     │            │            │            │ Valid ✓    │            │
     │            │            │            │            │            │
     │            │            │            │ 9. Load module          │
     │            │            │            │ import("/functions/send-email.js")
     │            │            │            │──────      │            │
     │            │            │            │     │      │            │
     │            │            │            │◄────       │            │
     │            │            │            │            │            │
     │            │            │            │ Module loaded           │
     │            │            │            │ fn = module.default     │
     │            │            │            │            │            │
     │            │            │            │ 10. Create context      │
     │            │            │            │ context = {             │
     │            │            │            │   emit: async (evt) => {...},
     │            │            │            │   logger: {...},        │
     │            │            │            │   config: {...}         │
     │            │            │            │ }          │            │
     │            │            │            │            │            │
     │            │            │            │ 11. Invoke function     │
     │            │            │            │ result = await fn(event, context)
     │            │            │            │──────      │            │
     │            │            │            │     │      │            │
     │            │            │            │     │      │            │
     │            │            │            │     │ Inside function:  │
     │            │            │            │     │ - Process event   │
     │            │            │            │     │ - Call context.emit()
     │            │            │            │     │                   │
     │            │            │            │     │ context.emit called:
     │            │            │            │     │ {                │
     │            │            │            │     │   type: "email.sent",
     │            │            │            │     │   data: {...}    │
     │            │            │            │     │ }                │
     │            │            │            │     │                  │
     │            │            │            │     │ 12. Enrich emitted event
     │            │            │            │     │ Add metadata:    │
     │            │            │            │     │ - source: "send-email"
     │            │            │            │     │ - triggeredBy: "evt_parent"
     │            │            │            │     │ - depth: 1       │
     │            │            │            │     │                  │
     │            │            │            │     │ 13. Check loop prevention
     │            │            │            │     │──────────────────────────►
     │            │            │            │     │                  │
     │            │            │            │     │            Depth: 1 < 50 ✓
     │            │            │            │     │            Fingerprint: new ✓
     │            │            │            │     │            Ancestry: no cycle ✓
     │            │            │            │     │            Circuit: ok ✓
     │            │            │            │     │                  │
     │            │            │            │     │ {allowed: true}  │
     │            │            │            │     │◄──────────────────────────
     │            │            │            │     │                  │
     │            │            │            │     │ 14. Append to EventLog
     │            │            │            │     │──────────────────────────►
     │            │            │            │     │                  │
     │            │            │            │     │            Write to JSONL
     │            │            │            │     │                  │──────
     │            │            │            │     │                  │     │
     │            │            │            │     │                  │◄────
     │            │            │            │     │                  │
     │            │            │            │     │ {success: true, eventId}
     │            │            │            │     │◄──────────────────────────
     │            │            │            │     │                  │
     │            │            │            │     │ emit() resolves │
     │            │            │            │     │                  │
     │            │            │            │     │ Function returns:
     │            │            │            │     │ {status: "sent"}
     │            │            │            │◄────                   │
     │            │            │            │                        │
     │            │            │            │ result = {status: "sent"}
     │            │            │            │                        │
     │            │            │            │ 15. Emit function.executed event
     │            │            │            │ {                      │
     │            │            │            │   type: "function.executed",
     │            │            │            │   data: {              │
     │            │            │            │     functionId: "send-email",
     │            │            │            │     result: {...},     │
     │            │            │            │     executionTime: 123ms
     │            │            │            │   }                    │
     │            │            │            │ }                      │
     │            │            │            │────────────────────────────────►
     │            │            │            │                        │
     │            │            │            │                  Write to JSONL
     │            │            │            │                        │
     │            │            │            │ Success                │
     │            │            │            │◄────────────────────────────────
     │            │            │            │                        │
     │            │            │            │ 16. Return UAP complete
     │            │            │            │ {                      │
     │            │            │            │   protocol: "function.v1",
     │            │            │            │   action: "complete",  │
     │            │            │            │   data: {              │
     │            │            │            │     success: true,     │
     │            │            │            │     result: {...}      │
     │            │            │            │   }                    │
     │            │            │            │ }                      │
     │            │◄────────────────────────────────────            │
     │            │            │            │                        │
```

### Events in Log After Execution

```jsonl
{"id":"evt_001","type":"user.login","data":{"userId":"alice"},"metadata":{"depth":0,"triggeredBy":null}}
{"id":"evt_002","type":"email.sent","data":{"recipient":"alice"},"metadata":{"depth":1,"triggeredBy":"evt_001","source":"send-email"}}
{"id":"evt_003","type":"function.executed","data":{"functionId":"send-email","result":{"status":"sent"}},"metadata":{"depth":1,"triggeredBy":"evt_001","source":"FunctionExecutorActor"}}
```

---

## Flow 3: Loop Detection (Fingerprinting)

### Duplicate Event Prevention

```
┌──────────┐            ┌─────────────────────────┐
│ Client   │            │ Loop Prevention         │
│          │            │ Coordinator             │
└────┬─────┘            └────┬────────────────────┘
     │                       │
     │ 1. First event        │
     │ POST /events          │
     │ {type:"test",data:{x:1}}
     │───────────────────────►
     │                       │
     │                  Generate fingerprint:
     │                  sha256("test" + '{"x":1}')
     │                  = "abc123..."
     │                       │
     │                  Check cache:
     │                  cache.has("abc123") → false
     │                       │
     │                  Add to cache:
     │                  cache.set("abc123", true)
     │                       │
     │ {allowed: true}       │
     │◄───────────────────────
     │                       │
     │ ✅ Event processed    │
     │                       │
     │                       │
     │ 2. Second event (same)│
     │ POST /events          │
     │ {type:"test",data:{x:1}}
     │───────────────────────►
     │                       │
     │                  Generate fingerprint:
     │                  sha256("test" + '{"x":1}')
     │                  = "abc123..."  (same!)
     │                       │
     │                  Check cache:
     │                  cache.has("abc123") → true ❌
     │                       │
     │ {allowed: false,      │
     │  reason: "Duplicate", │
     │  fingerprint: "abc123"}
     │◄───────────────────────
     │                       │
     │ ❌ Event rejected     │
     │ Loop prevented!       │
     │                       │
```

---

## Flow 4: Loop Detection (Ancestry Chain)

### Cycle Detection

```
┌──────────┐            ┌─────────────────────────┐
│ Events   │            │ Ancestry Tracker        │
└────┬─────┘            └────┬────────────────────┘
     │                       │
     │ Event A (root)        │
     │ id: "evt_A"           │
     │ triggeredBy: null     │
     │───────────────────────►
     │                       │
     │                  Ancestry: []
     │                  No parent → OK ✓
     │                       │
     │ {allowed: true}       │
     │◄───────────────────────
     │                       │
     │ Event B               │
     │ id: "evt_B"           │
     │ triggeredBy: "evt_A"  │
     │───────────────────────►
     │                       │
     │                  Ancestry: ["evt_A"]
     │                  Check if "evt_B" in ancestry → No ✓
     │                       │
     │ {allowed: true}       │
     │◄───────────────────────
     │                       │
     │ Event C               │
     │ id: "evt_C"           │
     │ triggeredBy: "evt_B"  │
     │───────────────────────►
     │                       │
     │                  Build ancestry:
     │                  evt_B → evt_A
     │                  Ancestry: ["evt_B", "evt_A"]
     │                  Check if "evt_C" in ancestry → No ✓
     │                       │
     │ {allowed: true}       │
     │◄───────────────────────
     │                       │
     │ Event A (again!)      │
     │ id: "evt_A"           │
     │ triggeredBy: "evt_C"  │
     │───────────────────────►
     │                       │
     │                  Build ancestry:
     │                  evt_C → evt_B → evt_A
     │                  Ancestry: ["evt_C", "evt_B", "evt_A"]
     │                       │
     │                  Check if "evt_A" in ancestry → YES ❌
     │                       │
     │                  CYCLE DETECTED!
     │                  Chain: evt_A → evt_B → evt_C → evt_A
     │                       │
     │ {allowed: false,      │
     │  reason: "Cycle",     │
     │  cycle: ["evt_A", "evt_B", "evt_C", "evt_A"]}
     │◄───────────────────────
     │                       │
     │ ❌ Loop prevented!    │
     │                       │
```

---

## Flow 5: Circuit Breaker

### Rate Limiting per Function

```
┌──────────┐            ┌─────────────────────────┐
│ Function │            │ Circuit Breaker         │
│ Executor │            │                         │
└────┬─────┘            └────┬────────────────────┘
     │                       │
     │ Execute "send-email"  │
     │───────────────────────►
     │                       │
     │                  Check counter:
     │                  count["send-email"] = 0
     │                  threshold = 100
     │                  0 < 100 ✓
     │                       │
     │                  Increment:
     │                  count["send-email"] = 1
     │                       │
     │ {allowed: true}       │
     │◄───────────────────────
     │                       │
     │ Execute "send-email"  │
     │───────────────────────►
     │                       │
     │                  count["send-email"] = 1
     │                  1 < 100 ✓
     │                  count["send-email"] = 2
     │                       │
     │ {allowed: true}       │
     │◄───────────────────────
     │                       │
     │ ... (98 more times)   │
     │                       │
     │ Execute "send-email"  │
     │ (101st time)          │
     │───────────────────────►
     │                       │
     │                  count["send-email"] = 100
     │                  100 < 100 → FALSE ❌
     │                       │
     │ {allowed: false,      │
     │  reason: "Circuit breaker tripped",
     │  count: 100,          │
     │  threshold: 100}      │
     │◄───────────────────────
     │                       │
     │ ❌ Execution blocked  │
     │                       │
     │                       │
     │ (Wait for window to reset)
     │                       │
     │ ... 60 seconds later ...
     │                       │
     │                  Window expired
     │                  count["send-email"] = 0
     │                       │
     │ Execute "send-email"  │
     │───────────────────────►
     │                       │
     │                  count["send-email"] = 0
     │                  0 < 100 ✓
     │                  count["send-email"] = 1
     │                       │
     │ {allowed: true}       │
     │◄───────────────────────
     │                       │
     │ ✅ Circuit reset      │
     │                       │
```

---

## Data Flow Summary

### Event Lifecycle

```
1. Client Emission
   ↓
2. HTTP Request Parsing
   ↓
3. Loop Prevention Check
   ↓
4. UAP Message Creation
   ↓
5. EventLogActor Processing
   ↓
6. Event Enrichment (ID, timestamp, metadata)
   ↓
7. JSONL Formatting
   ↓
8. Disk Write (append)
   ↓
9. Pattern Matching (optional)
   ↓
10. Function Execution (optional)
    ↓
11. New Event Emission (from function)
    ↓
12. Loop Prevention Check (recursive)
    ↓
13. Append to EventLog
    ↓
... (repeat for child events)
```

### Message Protocol Flow

```
HTTP JSON
    ↓
UAP Message (event.v1, append)
    ↓
EventLog Processing
    ↓
Enriched Event Object
    ↓
JSONL Line
    ↓
Disk Storage
    ↓
Pattern Matching
    ↓
UAP Message (function.v1, execute)
    ↓
Function Execution
    ↓
UAP Message (function.v1, complete)
    ↓
New Event Emission (if context.emit called)
    ↓
UAP Message (event.v1, append)
    ↓
... (recursive)
```

---

## Conclusion

These message flows demonstrate:

1. **Complete traceability**: Every step is logged and visible
2. **UAP compliance**: All inter-actor communication uses protocol
3. **Safety mechanisms**: Loop prevention at multiple layers
4. **Event sourcing**: All events persisted to append-only log
5. **Reactive architecture**: Events trigger patterns → functions → new events
6. **Error handling**: Each step has success/error responses

The system provides full observability and auditability of event processing from client request to disk storage and function execution.
