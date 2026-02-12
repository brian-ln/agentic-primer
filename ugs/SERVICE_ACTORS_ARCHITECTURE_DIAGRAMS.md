# Service Actors Architecture - Visual Reference

**Date:** 2026-02-07
**Purpose:** Visual architecture diagrams for service actor system
**Status:** Reference Guide

---

## Table of Contents

1. [System Overview](#1-system-overview)
2. [Service Layer Architecture](#2-service-layer-architecture)
3. [Service Composition Patterns](#3-service-composition-patterns)
4. [Message Flow Diagrams](#4-message-flow-diagrams)
5. [Namespace Organization](#5-namespace-organization)
6. [Service Category Breakdown](#6-service-category-breakdown)

---

## 1. System Overview

### Three-Layer Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     BUSINESS/DOMAIN LAYER                        │
│                                                                   │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │
│  │  Workflow    │  │  Task        │  │  Knowledge   │         │
│  │ Orchestrator │  │  Actor       │  │  Actor       │         │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘         │
│         │                  │                  │                  │
└─────────┼──────────────────┼──────────────────┼─────────────────┘
          │                  │                  │
          │ Uses services    │                  │
          │ (domain msgs)    │                  │
          ▼                  ▼                  ▼
┌─────────────────────────────────────────────────────────────────┐
│                        SERVICE LAYER                             │
│                                                                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │  Auth    │  │  Email   │  │   LLM    │  │  Cache   │       │
│  │ Service  │  │ Service  │  │ Service  │  │ Service  │       │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘       │
│       │             │              │             │              │
│  ┌────┴─────┐  ┌───┴──────┐  ┌───┴──────┐  ┌──┴──────┐       │
│  │  Queue   │  │  Search  │  │ Compute  │  │  Notify │       │
│  │ Service  │  │ Service  │  │ Service  │  │ Service │       │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬────┘       │
│       │             │              │             │              │
└───────┼─────────────┼──────────────┼─────────────┼─────────────┘
        │             │              │             │
        │ Composes    │              │             │
        │ system      │              │             │
        │ actors      │              │             │
        ▼             ▼              ▼             ▼
┌─────────────────────────────────────────────────────────────────┐
│                       SYSTEM LAYER                               │
│                                                                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │ Storage  │  │FileSystem│  │   HTTP   │  │WebSocket │       │
│  │  Actor   │  │  Actor   │  │  Client  │  │  Actor   │       │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘       │
│                                                                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │Scheduler │  │ Process  │  │  Logger  │  │ Metrics  │       │
│  │  Actor   │  │  Actor   │  │  Actor   │  │  Actor   │       │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘       │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
        │             │              │             │
        │ Wraps OS    │              │             │
        │ & external  │              │             │
        ▼             ▼              ▼             ▼
┌─────────────────────────────────────────────────────────────────┐
│                    EXTERNAL RESOURCES                            │
│                                                                   │
│   Database    FileSystem    HTTP APIs    WebSockets              │
│   Timers      Processes     OS Signals   External Services       │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

### Layer Responsibilities

**Business/Domain Layer:**
- Application logic and workflows
- Domain-specific rules and state machines
- Uses service-oriented messages
- Example: `auth.login`, `email.send`, `llm.generate`

**Service Layer:**
- High-level abstractions (SaaS/FaaS/MaaS/PaaS)
- Hides implementation details
- Composes multiple system actors
- Provides domain-oriented APIs

**System Layer:**
- Low-level primitives
- Thin wrappers over OS/external resources
- Technical operations (SQL, HTTP, filesystem)
- Example: `storage.query`, `http.post`, `fs.read`

**External Resources:**
- Operating system (filesystem, processes, timers)
- Databases (LibSQL, PostgreSQL)
- External APIs (Anthropic, SendGrid, Stripe)
- Network protocols (HTTP, WebSocket)

---

## 2. Service Layer Architecture

### Service Actor Internal Structure

```
┌───────────────────────────────────────────────────────────────┐
│                    AuthServiceActor                            │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  Public Interface (Message Protocol)                     │ │
│  │                                                           │ │
│  │  • auth.register  → Register new user                    │ │
│  │  • auth.login     → Authenticate user                    │ │
│  │  • auth.verify    → Verify session token                 │ │
│  │  • auth.logout    → End session                          │ │
│  │  • auth.refresh   → Refresh expired token                │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  Internal Validation & Logic                             │ │
│  │                                                           │ │
│  │  • Rate limit checking                                   │ │
│  │  • Password hashing/verification (bcrypt/argon2)         │ │
│  │  • Session token generation                              │ │
│  │  • Email verification workflow                           │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  System Actor Composition                                │ │
│  │                                                           │ │
│  │  this.storage ──→ /auth/system/storage                  │ │
│  │  this.cache   ──→ /auth/system/cache                    │ │
│  │  this.email   ──→ /email/services/email                 │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  Configuration                                           │ │
│  │                                                           │ │
│  │  • hashAlgorithm: 'bcrypt'                              │ │
│  │  • sessionDuration: 3600000                             │ │
│  │  • requireEmailVerification: true                       │ │
│  │  • rateLimitAttempts: 5                                 │ │
│  └─────────────────────────────────────────────────────────┘ │
└───────────────────────────────────────────────────────────────┘
```

### Message Flow: auth.login

```
1. Client Actor
   ↓ tell('/auth/services/auth', 'auth.login', {username, password})
   │
2. AuthServiceActor
   ↓ Internal validation
   │ • Check rate limit
   │ • Validate input format
   │
3. AuthServiceActor → StorageActor
   ↓ ask('/auth/system/storage', 'storage.query', {sql: 'SELECT...'})
   │ Fetch user by username
   │
4. StorageActor → LibSQL
   ↓ Execute SQL query
   │ Return user record
   │
5. AuthServiceActor
   ↓ Internal logic
   │ • Verify password (bcrypt)
   │ • Generate session token
   │
6. AuthServiceActor → CacheActor
   ↓ tell('/auth/system/cache', 'cache.set', {key: 'session:...', value: {...}})
   │ Store session
   │
7. AuthServiceActor
   ↓ Return response
   │ {sessionToken, userId, expiresAt}
   │
8. Client Actor
   ← Response received
```

---

## 3. Service Composition Patterns

### Pattern 1: Service → System (Direct)

Simple services that wrap 1-2 system actors.

```
┌──────────────────┐
│ CacheServiceActor│
│                  │
│  cache.get      │
│  cache.set      │
│  cache.delete   │
└────────┬─────────┘
         │ Uses
         ▼
┌─────────────────┐
│  StorageActor   │
│  (cache table)  │
└─────────────────┘
```

### Pattern 2: Service → Service → System (Composition)

Services that compose other services.

```
┌──────────────────────┐
│NotificationService   │
│                      │
│  notification.send   │
│  notification.sendMulti
└──────────┬───────────┘
           │ Composes
           ▼
┌──────────────────────┐     ┌──────────────────────┐
│  EmailServiceActor   │     │  SMSServiceActor     │
│                      │     │                      │
│  email.send         │     │  sms.send           │
└──────────┬───────────┘     └──────────┬───────────┘
           │                            │
           │ Uses                       │ Uses
           ▼                            ▼
┌──────────────────────┐     ┌──────────────────────┐
│  HTTPClientActor     │     │  HTTPClientActor     │
│  (SendGrid API)      │     │  (Twilio API)        │
└──────────────────────┘     └──────────────────────┘
```

### Pattern 3: Service → Multiple System Actors

Services that coordinate multiple system actors.

```
┌──────────────────────────────────────┐
│         AuthServiceActor              │
│                                       │
│  auth.register                        │
│  auth.login                           │
│  auth.verify                          │
└───────┬──────────┬──────────┬────────┘
        │          │          │
        │ Uses     │ Uses     │ Uses
        ▼          ▼          ▼
┌───────────┐ ┌────────┐ ┌────────────┐
│ Storage   │ │ Cache  │ │ Email      │
│ Actor     │ │ Actor  │ │ Service    │
└───────────┘ └────────┘ └────────────┘
     │            │            │
     ▼            ▼            ▼
 Database     Memory/DB    SendGrid API
```

---

## 4. Message Flow Diagrams

### Example 1: User Registration Flow

```
Client Actor                    AuthService                 StorageActor              EmailService
     │                              │                           │                         │
     │─────register-user───────────>│                           │                         │
     │  {username, email, password} │                           │                         │
     │                              │                           │                         │
     │                              │──query: check username──>│                         │
     │                              │                           │                         │
     │                              │<────rows: []──────────────│                         │
     │                              │  (username available)     │                         │
     │                              │                           │                         │
     │                              │──hash password────────────│                         │
     │                              │  (internal bcrypt)        │                         │
     │                              │                           │                         │
     │                              │──execute: INSERT user────>│                         │
     │                              │                           │                         │
     │                              │<────success───────────────│                         │
     │                              │                           │                         │
     │                              │──────sendTemplate: welcome email──────────────────>│
     │                              │  {to, templateId, variables}                        │
     │                              │                           │                         │
     │<─────response────────────────│                           │                         │
     │  {userId, sessionToken}      │                           │                         │
     │                              │                           │                         │
```

### Example 2: LLM Generation with Cache

```
Client Actor               LLMService              CacheActor             HTTPClientActor
     │                         │                       │                        │
     │───llm.generate─────────>│                       │                        │
     │  {prompt, model}        │                       │                        │
     │                         │                       │                        │
     │                         │──cache.get───────────>│                        │
     │                         │  key: hash(prompt)    │                        │
     │                         │                       │                        │
     │                         │<─────cache miss───────│                        │
     │                         │                       │                        │
     │                         │──────http.post: Claude API──────────────────>│
     │                         │  {model, messages}    │                        │
     │                         │                       │                        │
     │                         │<──────response────────────────────────────────│
     │                         │  {text, usage}        │                        │
     │                         │                       │                        │
     │                         │──cache.set───────────>│                        │
     │                         │  key: hash(prompt)    │                        │
     │                         │  value: {text, usage} │                        │
     │                         │                       │                        │
     │<────response────────────│                       │                        │
     │  {text, usage, cost}    │                       │                        │
     │                         │                       │                        │
```

### Example 3: Multi-Service Composition (Onboarding)

```
Client              OnboardingService        EmailService         PreferencesService       APIKeysService
  │                        │                      │                        │                      │
  │──start-onboarding─────>│                      │                        │                      │
  │  {userId, email}       │                      │                        │                      │
  │                        │                      │                        │                      │
  │                        │──sendTemplate────────>│                        │                      │
  │                        │  {to, templateId}    │                        │                      │
  │                        │                      │                        │                      │
  │                        │──createDefault───────────────────────────────>│                      │
  │                        │  {userId}            │                        │                      │
  │                        │                      │                        │                      │
  │                        │──generate────────────────────────────────────────────────────────────>│
  │                        │  {userId}            │                        │                      │
  │                        │                      │                        │                      │
  │                        │──sendSequence────────>│                        │                      │
  │                        │  {to, sequenceId}    │                        │                      │
  │                        │                      │                        │                      │
  │<───response────────────│                      │                        │                      │
  │  {started: true}       │                      │                        │                      │
  │                        │                      │                        │                      │
```

---

## 5. Namespace Organization

### Complete Namespace Hierarchy

```
/
├── system/                      # Global shared services
│   ├── scheduler/               # Shared time/scheduling (singleton)
│   ├── logger/                  # Shared logging (singleton)
│   └── metrics/                 # Shared telemetry (singleton)
│
├── auth/                        # Authentication namespace
│   ├── services/
│   │   └── auth-service/        # AuthServiceActor
│   └── system/
│       ├── storage/             # User/session storage
│       └── cache/               # Session cache
│
├── email/                       # Email namespace
│   ├── services/
│   │   └── email-service/       # EmailServiceActor
│   └── system/
│       ├── http/                # SendGrid/SES API
│       └── queue/               # Email queue
│
├── llm/                         # LLM inference namespace
│   ├── services/
│   │   └── llm-service/         # LLMServiceActor
│   └── system/
│       ├── http/                # Anthropic/OpenAI API
│       └── cache/               # Response cache
│
├── cache/                       # Caching namespace
│   ├── services/
│   │   └── cache-service/       # CacheServiceActor
│   └── system/
│       └── storage/             # Cache table (or Redis)
│
├── queue/                       # Queue namespace
│   ├── services/
│   │   └── queue-service/       # QueueServiceActor
│   └── system/
│       └── storage/             # Message queue table
│
├── search/                      # Search namespace
│   ├── services/
│   │   └── search-service/      # SearchServiceActor
│   └── system/
│       ├── storage/             # FTS5 index
│       └── vectors/             # Embeddings
│
├── notification/                # Notification namespace
│   ├── services/
│   │   ├── notification-service/ # NotificationServiceActor
│   │   ├── email-service/       # EmailServiceActor (delegate)
│   │   └── sms-service/         # SMSServiceActor (delegate)
│   └── system/
│       ├── http/                # Twilio/SNS API
│       └── queue/               # Notification queue
│
├── compute/                     # Compute namespace
│   ├── services/
│   │   └── compute-service/     # ComputeServiceActor
│   └── system/
│       ├── process/             # Sandbox execution
│       └── filesystem/          # Temp file storage
│
├── workflows/                   # Workflow orchestration namespace
│   ├── orchestrator/            # WorkflowOrchestrator
│   ├── tasks/                   # Task actors
│   └── system/
│       ├── storage/             # Workflow state
│       ├── filesystem/          # Workflow files
│       └── http/                # External API calls
│
└── domain/                      # Business domain namespace
    ├── logic/                   # Domain actors
    └── system/
        └── storage/             # Read-only domain data
```

### Namespace Access Control

```
┌─────────────────────────────────────────────────────────────┐
│ /workflows namespace can access:                             │
│  ✅ /workflows/system/*        (own system actors)          │
│  ✅ /auth/services/auth        (auth service)               │
│  ✅ /email/services/email      (email service)              │
│  ✅ /llm/services/llm          (LLM service)                │
│  ❌ /auth/system/*             (denied - use service)       │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ /domain namespace can access:                               │
│  ✅ /domain/system/storage     (read-only)                  │
│  ✅ /cache/services/cache      (cache service)              │
│  ❌ /workflows/system/*        (denied - namespace isolation)│
│  ❌ /email/services/email      (denied - domain is pure)    │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ /auth/services/auth can access:                             │
│  ✅ /auth/system/storage       (user/session storage)       │
│  ✅ /auth/system/cache         (session cache)              │
│  ✅ /email/services/email      (verification emails)        │
│  ✅ /cache/services/cache      (rate limiting)              │
└─────────────────────────────────────────────────────────────┘
```

---

## 6. Service Category Breakdown

### SaaS (Software as a Service) Actors

```
┌─────────────────────────────────────────────────────────────┐
│                    SaaS Services                             │
│                                                               │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐│
│  │ AuthService    │  │ EmailService   │  │ SearchService  ││
│  │                │  │                │  │                ││
│  │ • Login/logout │  │ • Send email   │  │ • Index docs   ││
│  │ • Sessions     │  │ • Templates    │  │ • Keyword      ││
│  │ • Permissions  │  │ • Campaigns    │  │ • Semantic     ││
│  └────────────────┘  └────────────────┘  └────────────────┘│
│                                                               │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐│
│  │ CacheService   │  │ QueueService   │  │ NotifyService  ││
│  │                │  │                │  │                ││
│  │ • Key-value    │  │ • Enqueue      │  │ • Multi-channel││
│  │ • TTL          │  │ • Dequeue      │  │ • Templates    ││
│  │ • Eviction     │  │ • DLQ          │  │ • Priority     ││
│  └────────────────┘  └────────────────┘  └────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

### FaaS (Function as a Service) Actors

```
┌─────────────────────────────────────────────────────────────┐
│                   FaaS Services                              │
│                                                               │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐│
│  │ComputeService  │  │TransformService│  │ValidatorService││
│  │                │  │                │  │                ││
│  │ • Execute fn   │  │ • Data pipelines│ │ • Input rules  ││
│  │ • Sandbox      │  │ • ETL          │  │ • Schema valid ││
│  │ • Timeout      │  │ • Batch        │  │ • Sanitize     ││
│  └────────────────┘  └────────────────┘  └────────────────┘│
│                                                               │
│  ┌────────────────┐                                          │
│  │ WorkerPool     │                                          │
│  │                │                                          │
│  │ • Distribute   │                                          │
│  │ • Load balance │                                          │
│  │ • Fault tolerant│                                         │
│  └────────────────┘                                          │
└─────────────────────────────────────────────────────────────┘
```

### MaaS (Model as a Service) Actors

```
┌─────────────────────────────────────────────────────────────┐
│                   MaaS Services                              │
│                                                               │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐│
│  │  LLMService    │  │EmbeddingService│  │ClassifyService ││
│  │                │  │                │  │                ││
│  │ • Generate     │  │ • Text vectors │  │ • Text class   ││
│  │ • Streaming    │  │ • Similarity   │  │ • Image class  ││
│  │ • Tool use     │  │ • Clustering   │  │ • Sentiment    ││
│  │ • Multi-provider│ │                │  │                ││
│  └────────────────┘  └────────────────┘  └────────────────┘│
│                                                               │
│  ┌────────────────┐  ┌────────────────┐                     │
│  │TranslateService│  │  VisionService │                     │
│  │                │  │                │                     │
│  │ • Languages    │  │ • Image OCR    │                     │
│  │ • Auto-detect  │  │ • Object detect│                     │
│  │                │  │ • Face detect  │                     │
│  └────────────────┘  └────────────────┘                     │
└─────────────────────────────────────────────────────────────┘
```

### PaaS (Platform as a Service) Actors

```
┌─────────────────────────────────────────────────────────────┐
│                   PaaS Services                              │
│                                                               │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐│
│  │ BuildService   │  │ DeployService  │  │ SecretService  ││
│  │                │  │                │  │                ││
│  │ • Compile      │  │ • Environments │  │ • Store secrets││
│  │ • Package      │  │ • Rollback     │  │ • Rotate keys  ││
│  │ • Artifacts    │  │ • Health check │  │ • Audit access ││
│  └────────────────┘  └────────────────┘  └────────────────┘│
│                                                               │
│  ┌────────────────┐                                          │
│  │ MonitorService │                                          │
│  │                │                                          │
│  │ • Metrics      │                                          │
│  │ • Logs         │                                          │
│  │ • Alerts       │                                          │
│  └────────────────┘                                          │
└─────────────────────────────────────────────────────────────┘
```

---

## 7. Comparison Diagrams

### Before: Low-Level System Actors

```
┌──────────────────────────────────────────────────────────┐
│            Business Actor (TaskActor)                     │
│                                                            │
│  await this.ask('/workflows/system/storage', {            │
│    sql: 'SELECT * FROM users WHERE username = ?',        │
│    params: [username]                                     │
│  });                                                       │
│                                                            │
│  const hash = await bcrypt.hash(password, 10);           │
│                                                            │
│  await this.ask('/workflows/system/cache', {             │
│    key: `session:${token}`,                              │
│    value: JSON.stringify(data),                          │
│    ttl: 3600000                                          │
│  });                                                       │
│                                                            │
│  await this.ask('/workflows/system/http', {              │
│    url: 'https://api.sendgrid.com/...',                 │
│    headers: {...},                                        │
│    body: {...}                                           │
│  });                                                       │
│                                                            │
└──────────────┬───────────────────────────────────────────┘
               │
               │ Direct access (3 system actors)
               │
               ▼
┌──────────────────────────────────────────────────────────┐
│         System Actors (Low-Level)                        │
│                                                            │
│  StorageActor    CacheActor    HTTPClientActor           │
│  (SQL)           (Key-Value)   (HTTP/API)                │
└──────────────────────────────────────────────────────────┘

Problems:
❌ Business logic mixed with infrastructure
❌ Must know SQL, HTTP APIs, crypto
❌ 80+ lines for common operations
❌ Mock 3-5 actors per test
❌ No consistency (ad-hoc implementations)
```

### After: Service-Oriented

```
┌──────────────────────────────────────────────────────────┐
│            Business Actor (TaskActor)                     │
│                                                            │
│  await this.ask('/auth/services/auth', {                 │
│    type: 'auth.login',                                   │
│    username,                                              │
│    password                                               │
│  });                                                       │
│                                                            │
└──────────────┬───────────────────────────────────────────┘
               │
               │ Service-oriented message
               │
               ▼
┌──────────────────────────────────────────────────────────┐
│              AuthServiceActor                             │
│                                                            │
│  • Validates rate limit                                  │
│  • Hashes password (bcrypt)                              │
│  • Generates session token                               │
│  • Stores session in cache                               │
│  • Sends verification email                              │
│                                                            │
└──────────────┬───────────────────────────────────────────┘
               │
               │ Composes system actors internally
               │
               ▼
┌──────────────────────────────────────────────────────────┐
│         System Actors (Low-Level)                        │
│                                                            │
│  StorageActor    CacheActor    HTTPClientActor           │
│  (SQL)           (Key-Value)   (HTTP/API)                │
└──────────────────────────────────────────────────────────┘

Benefits:
✅ Business logic separated from infrastructure
✅ Domain-oriented API (no SQL/HTTP knowledge)
✅ ~10 lines for common operations (88% reduction)
✅ Mock 1 service per test
✅ Consistent (all auth uses AuthService)
```

---

## Summary

This architecture provides:

1. **Clear Layering** - Business → Service → System → External
2. **Service Abstraction** - Hide complexity behind domain-oriented APIs
3. **Composability** - Services compose other services naturally
4. **Namespace Isolation** - Security and fault tolerance through routing
5. **Developer Experience** - 88% code reduction, simpler APIs
6. **Testability** - Easy to mock services, not system actors
7. **Evolvability** - Change implementations without breaking clients

**Next:** Implement Phase 2 (Core Services) starting with CacheServiceActor.

---

**Document End**

**Status:** Architecture reference complete
**Usage:** Visual guide for understanding service actor system
**Last Updated:** 2026-02-07
