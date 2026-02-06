# Spec-Kit Formula Examples

**Project**: GitHub spec-kit
**Analysis Date**: 2026-01-11
**Purpose**: Demonstrate artifact flow and formula usage with concrete examples

## Example 1: Simple Feature - User Authentication

### Scenario
Build a simple user authentication feature with email/password login.

### Formula Invocation

```bash
bd mol wisp spec-kit-workflow \
  --var feature_description="User authentication with email and password" \
  --var project_values="Simplicity, test-first, security-first" \
  --var tech_stack="Node.js, Express, bcrypt, JWT, PostgreSQL, Jest" \
  --var spec_dir=".specify" \
  --var tdd_approach="true" \
  --var max_projects="3" \
  --var validation_level="standard"
```

### Artifact Flow

#### Stage 1: Constitution
**Input**:
```yaml
project_values: "Simplicity, test-first, security-first"
max_projects: 3
```

**Output**: `.specify/memory/constitution.md`
```markdown
# Project Constitution

## Article I: Library-First Principle
Every feature MUST be developed as a standalone library...

## Article III: Test-First Imperative (NON-NEGOTIABLE)
All code MUST have tests written BEFORE implementation...

## Article VII: Simplicity
Initial implementation MUST use ≤3 projects...

## Article IX: Security-First
All authentication MUST use industry-standard encryption...
```

**Validation**: User approves constitution → proceed

---

#### Stage 2: Specification
**Input**:
- feature_description: "User authentication with email and password"
- constitution.md
- spec-template.md

**Output**: `specs/001-user-authentication/spec.md`
```markdown
# Feature Specification: User Authentication

**Feature Branch**: `001-user-authentication`
**Status**: Draft

## User Scenarios & Testing

### User Story 1 - Register New Account (Priority: P1)
As a new user, I want to create an account with email and password
so that I can access protected features.

**Why this priority**: Core functionality required for all other features

**Independent Test**: Can create account, verify in database, login with credentials

**Acceptance Scenarios**:
1. **Given** no existing account, **When** I provide valid email/password, **Then** account created and confirmation sent
2. **Given** valid email format, **When** password meets requirements, **Then** account created successfully

### User Story 2 - Login to Account (Priority: P1)
As a registered user, I want to login with my credentials...

### User Story 3 - Logout from Account (Priority: P2)
As a logged-in user, I want to logout...

## Requirements

### Functional Requirements
- **FR-001**: System MUST hash passwords using bcrypt before storage
- **FR-002**: System MUST validate email format
- **FR-003**: System MUST enforce password requirements (min 8 chars, uppercase, number)
- **FR-004**: System MUST generate JWT tokens on successful login
- **FR-005**: System MUST invalidate tokens on logout

### Key Entities
- **User**: email (unique), password_hash, created_at, last_login
- **Session**: user_id, token, expires_at

## Success Criteria
- **SC-001**: Users can register in <30 seconds
- **SC-002**: Login completes in <2 seconds
- **SC-003**: 99.9% of valid credentials succeed
```

**Validation**:
- ✓ No implementation details (no Express, JWT mentioned in requirements)
- ✓ Requirements testable
- ✓ Success criteria measurable
- ✓ 0 clarifications needed

**Quality Gate**: PASS → proceed to Planning

---

#### Stage 3: Clarification
**Skipped**: No [NEEDS CLARIFICATION] markers in spec

---

#### Stage 4: Planning
**Input**:
- spec.md
- constitution.md
- tech_stack: "Node.js, Express, bcrypt, JWT, PostgreSQL, Jest"
- tdd_approach: "true"

**Output 1**: `specs/001-user-authentication/plan.md`
```markdown
# Implementation Plan: User Authentication

## Technical Context
- **Language**: Node.js (ES2022)
- **Framework**: Express 4.x
- **Storage**: PostgreSQL 15
- **Authentication**: bcrypt + JWT
- **Testing**: Jest 29.x

## Constitutional Check
### Simplicity Gate (Article VII)
- ✓ Using 1 project (backend only)
- ✓ Below max_projects threshold (3)

### Anti-Abstraction Gate (Article VIII)
- ✓ Using Express directly (no custom abstractions)
- ✓ Using bcrypt directly
- ✓ Single User model representation

### Integration-First Gate (Article IX)
- ✓ Contracts defined (see contracts/auth-api.yaml)
- ✓ Integration tests specified (see quickstart.md)

**Result**: ALL GATES PASS

## Project Structure
```
src/
  models/user.js
  middleware/auth.js
  routes/auth.js
  utils/jwt.js
  utils/password.js
tests/
  integration/auth.test.js
  unit/user.test.js
  unit/password.test.js
```
```

**Output 2**: `specs/001-user-authentication/research.md`
```markdown
# Technology Research

## Password Hashing: bcrypt vs argon2
**Decision**: bcrypt
**Rationale**: Industry standard, well-tested, sufficient for use case
**Alternatives**: argon2 (more secure but overkill for MVP)

## Token Format: JWT vs Session Cookies
**Decision**: JWT
**Rationale**: Stateless, works with mobile clients
**Alternatives**: Session cookies (simpler but requires session store)
```

**Output 3**: `specs/001-user-authentication/data-model.md`
```markdown
# Data Model

## Entity: User
**Purpose**: Represents authenticated user account

**Schema**:
| Field | Type | Constraints | Purpose |
|-------|------|-------------|---------|
| id | UUID | PRIMARY KEY | Unique identifier |
| email | VARCHAR(255) | UNIQUE, NOT NULL | Login identifier |
| password_hash | VARCHAR(255) | NOT NULL | bcrypt hashed password |
| created_at | TIMESTAMP | NOT NULL, DEFAULT NOW() | Account creation time |
| last_login | TIMESTAMP | NULL | Last successful login |

**State Transitions**:
```
pending → active → suspended → deleted
```

**Validation Rules**:
- Email: RFC 5322 format
- Password: ≥8 chars, ≥1 uppercase, ≥1 number
```

**Output 4**: `specs/001-user-authentication/contracts/auth-api.yaml`
```yaml
openapi: 3.0.0
info:
  title: Authentication API
  version: 1.0.0

paths:
  /auth/register:
    post:
      summary: Register new user
      requestBody:
        content:
          application/json:
            schema:
              type: object
              properties:
                email:
                  type: string
                  format: email
                password:
                  type: string
                  minLength: 8
      responses:
        201:
          description: User created
          content:
            application/json:
              schema:
                type: object
                properties:
                  id: { type: string, format: uuid }
                  email: { type: string }
                  token: { type: string }
```

**Output 5**: `specs/001-user-authentication/quickstart.md`
```markdown
# Validation Scenarios

## Scenario 1: Register → Login → Logout
1. POST /auth/register with valid email/password
2. Verify response contains user ID and JWT token
3. POST /auth/login with same credentials
4. Verify response contains valid JWT
5. POST /auth/logout with JWT
6. Verify subsequent requests fail authentication
```

**Validation**:
- ✓ All constitutional gates pass
- ✓ Research complete
- ✓ Data model matches spec entities
- ✓ Contracts cover all requirements
- ✓ Quickstart scenarios testable

**Quality Gate**: PASS → proceed to Tasking

---

#### Stage 5: Tasking
**Input**:
- plan.md
- spec.md
- data-model.md
- contracts/

**Output**: `specs/001-user-authentication/tasks.md`
```markdown
# Tasks: User Authentication

## Phase 1: Setup (Priority: P0)
- [ ] T001 Create project structure per implementation plan
- [ ] T002 Initialize PostgreSQL database with migrations
- [ ] T003 Install dependencies (express, bcrypt, jsonwebtoken, jest, pg)
- [ ] T004 Configure Jest test environment

## Phase 2: Foundational (Priority: P0)
- [ ] T005 [P] Create User model in src/models/user.js
- [ ] T006 [P] Implement password utilities in src/utils/password.js (bcrypt wrapper)
- [ ] T007 [P] Implement JWT utilities in src/utils/jwt.js (sign/verify)

## Phase 3: User Story 1 - Register (Priority: P1)
- [ ] T008 [US1] Write integration test for POST /auth/register in tests/integration/auth.test.js
- [ ] T009 [US1] Write unit test for User.create() in tests/unit/user.test.js
- [ ] T010 [US1] Implement User.create() method in src/models/user.js
- [ ] T011 [US1] Implement POST /auth/register route in src/routes/auth.js
- [ ] T012 [US1] Add email validation middleware
- [ ] T013 [US1] Add password validation middleware
- [ ] T014 [US1] Verify integration test passes

## Phase 4: User Story 2 - Login (Priority: P1)
- [ ] T015 [US2] Write integration test for POST /auth/login
- [ ] T016 [US2] Write unit test for User.findByEmail()
- [ ] T017 [US2] Implement User.findByEmail() in src/models/user.js
- [ ] T018 [US2] Implement POST /auth/login route in src/routes/auth.js
- [ ] T019 [US2] Add authentication middleware in src/middleware/auth.js
- [ ] T020 [US2] Verify integration test passes

## Phase 5: User Story 3 - Logout (Priority: P2)
- [ ] T021 [US3] Write integration test for POST /auth/logout
- [ ] T022 [US3] Implement token blacklist in src/utils/jwt.js
- [ ] T023 [US3] Implement POST /auth/logout route
- [ ] T024 [US3] Verify integration test passes

## Dependency Graph
```
T001 → T002 → T003 → T004
T004 → T005, T006, T007 (parallel)
T005, T006, T007 → T008
T008 → T009 → T010 → T011 → T012, T013 (parallel) → T014
T014 → T015 → T016 → T017 → T018 → T019 → T020
T020 → T021 → T022, T023 (parallel) → T024
```

## Parallel Execution Examples
**Phase 2**: T005, T006, T007 can run in parallel (different files)
**Phase 3**: T012, T013 can run in parallel (different middleware files)
```

**Validation**:
- ✓ All tasks follow format
- ✓ Each user story has complete task set
- ✓ TDD: tests before implementation
- ✓ Dependencies correct
- ✓ 6 tasks marked [P] for parallel execution

**Quality Gate**: PASS → proceed to Implementation

---

#### Stage 6: Implementation
**Input**:
- tasks.md
- plan.md
- spec.md
- All design docs

**Process**:
1. Execute T001-T004 (Setup phase)
2. Execute T005, T006, T007 in parallel (Foundational)
3. Execute T008-T014 (User Story 1)
4. Execute T015-T020 (User Story 2)
5. Execute T021-T024 (User Story 3)

**Output 1**: `specs/001-user-authentication/src/` (7 files)
- models/user.js
- routes/auth.js
- middleware/auth.js
- middleware/validation.js
- utils/jwt.js
- utils/password.js
- index.js

**Output 2**: `specs/001-user-authentication/tests/` (3 files)
- integration/auth.test.js
- unit/user.test.js
- unit/password.test.js

**Output 3**: `specs/001-user-authentication/tasks.md` (updated)
```markdown
# Tasks: User Authentication

## Phase 1: Setup (Priority: P0)
- [X] T001 Create project structure per implementation plan
- [X] T002 Initialize PostgreSQL database with migrations
- [X] T003 Install dependencies
- [X] T004 Configure Jest test environment

[All other tasks marked [X]]
```

**Test Results**:
```
Test Suites: 3 passed, 3 total
Tests:       24 passed, 24 total
Coverage:    95.2% statements, 92.1% branches
```

**Validation**:
- ✓ All tasks complete
- ✓ Tests pass (100% pass rate)
- ✓ Coverage meets threshold (>90%)
- ✓ Features match spec.md

**Quality Gate**: PASS → User approval

---

### Artifact Summary

**Total Artifacts Created**: 19

| Stage | Artifacts | Count |
|-------|-----------|-------|
| Constitution | constitution.md | 1 |
| Specification | spec.md, requirements-checklist.md | 2 |
| Planning | plan.md, research.md, data-model.md, auth-api.yaml, quickstart.md | 5 |
| Tasking | tasks.md | 1 |
| Implementation | 7 src files, 3 test files | 10 |

**Total Lines of Code**: ~1,200 LOC (estimated)
- Specifications: 300 LOC
- Implementation: 600 LOC
- Tests: 300 LOC

**Time Estimate**: 60-90 minutes (with AI assistance)

---

## Example 2: Complex Feature - Real-Time Chat with Rooms

### Scenario
Build a real-time chat application with multiple rooms, user presence, and message history.

### Formula Invocation

```bash
bd mol wisp spec-kit-workflow \
  --var feature_description="Real-time chat with rooms, presence, and history" \
  --var project_values="Simplicity, test-first, integration-first, scalability" \
  --var tech_stack="Node.js, Socket.io, Redis, PostgreSQL, Jest" \
  --var spec_dir=".specify" \
  --var tdd_approach="true" \
  --var max_projects="4" \
  --var validation_level="strict"
```

### Artifact Flow Highlights

#### Stage 2: Specification
**Output**: `specs/002-realtime-chat/spec.md`
```markdown
# Feature Specification: Real-Time Chat

## User Scenarios & Testing

### User Story 1 - Join Chat Room (Priority: P1)
As a user, I want to join a chat room so I can see and send messages.

### User Story 2 - Send and Receive Messages (Priority: P1)
As a user in a room, I want to send messages that others see in real-time.

### User Story 3 - See User Presence (Priority: P2)
As a user in a room, I want to see who else is online.

### User Story 4 - View Message History (Priority: P2)
As a user joining a room, I want to see recent messages.

### User Story 5 - Create Private Rooms (Priority: P3)
As a user, I want to create private rooms with invite-only access.

## Requirements
- **FR-001**: System MUST deliver messages within 100ms (P95 latency)
- **FR-002**: System MUST persist message history [NEEDS CLARIFICATION: retention period?]
- **FR-003**: System MUST support [NEEDS CLARIFICATION: how many concurrent users per room?]
- **FR-004**: System MUST handle [NEEDS CLARIFICATION: disconnection/reconnection?]
```

**Clarifications Needed**: 3 (max allowed)

---

#### Stage 3: Clarification
**Interactive Q&A**:

**Q1**: Message retention period?
**Options**:
- A) 7 days (simple, low storage)
- B) 30 days (standard)
- C) Forever (requires archival strategy)

**User Answer**: B (30 days)

**Q2**: Concurrent users per room?
**Options**:
- A) 10 users (MVP, simple)
- B) 100 users (moderate scale)
- C) 1000+ users (requires sharding)

**User Answer**: B (100 users)

**Q3**: Disconnection/reconnection handling?
**Options**:
- A) Simple timeout (no message buffering)
- B) Automatic reconnection with message catch-up
- C) Full offline mode with sync

**User Answer**: B (reconnection with catch-up)

**Output**: `specs/002-realtime-chat/spec.md` (updated)
```markdown
- **FR-002**: System MUST persist message history for 30 days
- **FR-003**: System MUST support up to 100 concurrent users per room
- **FR-004**: System MUST handle disconnection/reconnection with automatic catch-up
```

---

#### Stage 4: Planning
**Constitutional Check**:
```markdown
### Simplicity Gate (Article VII)
- ⚠️ Using 4 projects (backend, WebSocket server, Redis, PostgreSQL)
- ❌ Exceeds max_projects threshold (4 > 3)
- ✓ JUSTIFIED: Real-time requirements necessitate Redis for pub/sub and session storage

**Complexity Tracking**:
| Component | Justification | Mitigation |
|-----------|---------------|------------|
| Redis | Required for Socket.io scaling, pub/sub | Use managed service, single instance for MVP |
```

**Output**: Multiple design documents

`data-model.md`:
```markdown
## Entity: Room
- id: UUID
- name: VARCHAR(255)
- type: ENUM('public', 'private')
- created_by: UUID (FK → User)
- created_at: TIMESTAMP

## Entity: Message
- id: UUID
- room_id: UUID (FK → Room)
- user_id: UUID (FK → User)
- content: TEXT
- created_at: TIMESTAMP
- deleted_at: TIMESTAMP (soft delete)

## Entity: RoomMembership
- room_id: UUID (FK → Room)
- user_id: UUID (FK → User)
- joined_at: TIMESTAMP
- last_seen: TIMESTAMP
```

`contracts/websocket-events.yaml`:
```yaml
events:
  client_to_server:
    - event: join_room
      payload:
        room_id: string (uuid)
      response:
        room_info: { id, name, member_count }
        recent_messages: array<Message>
        online_users: array<User>

    - event: send_message
      payload:
        room_id: string (uuid)
        content: string (max 1000 chars)
      response:
        message_id: string (uuid)
        timestamp: string (ISO 8601)

  server_to_client:
    - event: message_received
      payload:
        message: { id, user_id, username, content, timestamp }

    - event: user_joined
      payload:
        user: { id, username }
        online_count: number

    - event: user_left
      payload:
        user_id: string (uuid)
        online_count: number
```

---

#### Stage 5: Tasking
**Output**: `specs/002-realtime-chat/tasks.md` (78 tasks across 7 phases)

```markdown
## Phase 1: Setup (Priority: P0)
- [ ] T001 Create project structure with Socket.io integration
- [ ] T002 Set up PostgreSQL with migrations (rooms, messages, memberships)
- [ ] T003 Set up Redis for Socket.io adapter and session storage
- [ ] T004 [P] Install dependencies (express, socket.io, redis, pg, jest)
- [ ] T005 [P] Configure Jest with Socket.io testing utilities

## Phase 2: Foundational (Priority: P0)
- [ ] T006 [P] Create Room model in src/models/room.js
- [ ] T007 [P] Create Message model in src/models/message.js
- [ ] T008 [P] Create RoomMembership model in src/models/membership.js
- [ ] T009 [P] Implement Redis pub/sub wrapper in src/utils/pubsub.js
- [ ] T010 [P] Implement Socket.io server with Redis adapter in src/socket.js

## Phase 3: User Story 1 - Join Room (Priority: P1)
- [ ] T011 [US1] Write integration test for join_room event
- [ ] T012 [US1] Write unit test for Room.findById()
- [ ] T013 [US1] Implement Room.findById() in src/models/room.js
- [ ] T014 [US1] Implement join_room handler in src/socket/handlers/room.js
- [ ] T015 [US1] Implement RoomMembership.create() for join tracking
- [ ] T016 [US1] Emit room_info with recent messages (last 50)
- [ ] T017 [US1] Verify integration test passes

[... continues for 78 total tasks]

## Parallel Execution Examples
**Phase 2**: T006, T007, T008, T009, T010 can run in parallel (5 agents)
**Phase 3**: T012, T015 can run after T011 (parallel unit tests)
**Phase 6**: T065, T066, T067 can run in parallel (different private room features)
```

**Parallel Opportunities**: 28 tasks marked [P] (36% parallelizable)

---

#### Stage 6: Implementation
**Test-Driven Development**:

```javascript
// tests/integration/chat.test.js (created first)
describe('Real-time Chat', () => {
  test('User can join room and receive recent messages', async () => {
    const client = await createSocketClient()
    const response = await client.emit('join_room', { room_id: roomId })

    expect(response.room_info).toBeDefined()
    expect(response.recent_messages).toHaveLength(50)
    expect(response.online_users).toContain(userId)
  })

  test('Messages are delivered to all room members within 100ms', async () => {
    const [client1, client2] = await createSocketClients(2)
    await Promise.all([
      client1.emit('join_room', { room_id: roomId }),
      client2.emit('join_room', { room_id: roomId })
    ])

    const start = Date.now()
    await client1.emit('send_message', { room_id: roomId, content: 'Hello' })

    const received = await client2.waitForEvent('message_received')
    const latency = Date.now() - start

    expect(latency).toBeLessThan(100) // FR-001
    expect(received.message.content).toBe('Hello')
  })
})
```

```javascript
// src/socket/handlers/room.js (implemented after tests)
export async function handleJoinRoom(socket, { room_id }) {
  const room = await Room.findById(room_id)
  if (!room) throw new Error('Room not found')

  await socket.join(room_id)
  await RoomMembership.create({ room_id, user_id: socket.userId })

  const [recent_messages, online_users] = await Promise.all([
    Message.findByRoom(room_id, { limit: 50, order: 'DESC' }),
    RoomMembership.getOnlineUsers(room_id)
  ])

  socket.to(room_id).emit('user_joined', {
    user: socket.user,
    online_count: online_users.length + 1
  })

  return {
    room_info: { id: room.id, name: room.name, member_count: online_users.length + 1 },
    recent_messages: recent_messages.reverse(),
    online_users
  }
}
```

**Final Test Results**:
```
Test Suites: 12 passed, 12 total
Tests:       156 passed, 156 total
Coverage:    98.7% statements, 96.3% branches
Duration:    12.4s
```

**Performance Validation**:
- ✓ P95 latency: 87ms (target: <100ms)
- ✓ 100 concurrent users per room: stable
- ✓ Reconnection with catch-up: working
- ✓ 30-day message retention: configured

---

### Artifact Summary

**Total Artifacts Created**: 47

| Stage | Artifacts | Count |
|-------|-----------|-------|
| Constitution | constitution.md | 1 |
| Specification | spec.md, requirements-checklist.md | 2 |
| Clarification | spec.md (updated) | 0 (in-place update) |
| Planning | plan.md, research.md, data-model.md, websocket-events.yaml, rest-api.yaml, quickstart.md | 6 |
| Tasking | tasks.md | 1 |
| Implementation | 18 src files, 12 test files, 7 config files | 37 |

**Total Lines of Code**: ~4,800 LOC (estimated)
- Specifications: 600 LOC
- Implementation: 2,800 LOC
- Tests: 1,400 LOC

**Time Estimate**: 3-4 hours (with AI assistance, including 3 clarifications)

---

## Example 3: Artifact Reuse - Adding Feature to Existing Project

### Scenario
Add "Message Reactions" feature to existing real-time chat from Example 2.

### Key Differences

**Constitution**: Reuse existing (no new creation)
**Specification**: New spec, references existing entities from Example 2
**Planning**: References existing infrastructure (Redis, Socket.io already set up)

### Formula Invocation

```bash
bd mol wisp spec-kit-workflow \
  --var feature_description="Add emoji reactions to chat messages" \
  --var project_values="INHERIT_FROM:specs/002-realtime-chat" \
  --var tech_stack="INHERIT_FROM:specs/002-realtime-chat" \
  --var spec_dir=".specify" \
  --var tdd_approach="true" \
  --var existing_feature="002-realtime-chat"
```

### Artifact Flow (Abbreviated)

#### Stage 2: Specification
`specs/003-message-reactions/spec.md`:
```markdown
# Feature Specification: Message Reactions

**Extends**: 002-realtime-chat
**Dependencies**: Message entity from data-model.md

## User Scenarios
### User Story 1 - React to Message (Priority: P1)
As a user in a room, I want to react to messages with emojis.

## Requirements
- **FR-001**: System MUST support standard emoji reactions (👍 👎 ❤️ 😂 😮)
- **FR-002**: System MUST persist reactions with message
- **FR-003**: System MUST broadcast reactions in real-time
- **FR-004**: Users MUST be able to remove their own reactions
```

#### Stage 4: Planning
`specs/003-message-reactions/data-model.md`:
```markdown
# Data Model Extensions

## New Entity: MessageReaction
**Extends**: Existing Message entity from 002-realtime-chat

**Schema**:
| Field | Type | Constraints |
|-------|------|-------------|
| id | UUID | PRIMARY KEY |
| message_id | UUID | FK → Message (from 002-realtime-chat) |
| user_id | UUID | FK → User |
| emoji | VARCHAR(10) | CHECK (emoji IN ('👍', '👎', '❤️', '😂', '😮')) |
| created_at | TIMESTAMP | NOT NULL |

**Unique Constraint**: (message_id, user_id, emoji) - one reaction per user per emoji
```

#### Stage 5: Tasking
`specs/003-message-reactions/tasks.md`:
```markdown
## Phase 1: Setup (Priority: P0)
- [ ] T001 Create migration for message_reactions table
- [ ] T002 Install emoji validation library

## Phase 2: Foundational (Priority: P0)
- [ ] T003 Create MessageReaction model in src/models/reaction.js
- [ ] T004 Extend Message model with getReactions() method

## Phase 3: User Story 1 - React to Message (Priority: P1)
- [ ] T005 [US1] Write integration test for add_reaction event
- [ ] T006 [US1] Write unit test for MessageReaction.create()
- [ ] T007 [US1] Implement MessageReaction.create() in src/models/reaction.js
- [ ] T008 [US1] Implement add_reaction handler in src/socket/handlers/reaction.js
- [ ] T009 [US1] Broadcast reaction_added event to room
- [ ] T010 [US1] Verify integration test passes

[... 22 total tasks]
```

**Reused Infrastructure**:
- ✓ Socket.io server (from 002)
- ✓ Redis pub/sub (from 002)
- ✓ Message model (extended)
- ✓ Room handlers (no changes)

**New Code**: Only reaction-specific logic (minimal surface area)

---

### Artifact Summary

**Total Artifacts Created**: 12 (significantly fewer than new feature)

| Stage | Artifacts | Count | Notes |
|-------|-----------|-------|-------|
| Constitution | N/A | 0 | Reused from 002 |
| Specification | spec.md | 1 | References 002 |
| Planning | plan.md, data-model.md, websocket-events.yaml | 3 | Extends 002 |
| Tasking | tasks.md | 1 | 22 tasks (vs 78 in 002) |
| Implementation | 3 src files, 2 test files, 1 migration | 6 | Minimal additions |

**Total Lines of Code**: ~450 LOC
- Specifications: 120 LOC
- Implementation: 230 LOC
- Tests: 100 LOC

**Time Estimate**: 30-45 minutes (with AI assistance)

**Efficiency Gain**: 75% faster than standalone feature (infrastructure reuse)

---

## Example 4: Validation Gate Failure and Recovery

### Scenario
User attempts to create a feature that violates constitutional gates.

### Stage 4: Planning - Constitutional Compliance Failure

**Input**:
- spec.md requests "Microservices architecture with 8 separate services"
- constitution.md: Article VII requires ≤3 projects

**Output**: `specs/004-microservices/plan.md`
```markdown
## Constitutional Check
### Simplicity Gate (Article VII)
- ❌ Using 8 projects (8 microservices)
- ❌ Exceeds max_projects threshold (8 > 3)
- ❌ NOT JUSTIFIED: No compelling reason for microservices at MVP stage

**Result**: SIMPLICITY GATE FAILED
```

**Quality Gate**: FAIL → Request user decision

**User Options**:
1. **Simplify**: Reduce to ≤3 services
2. **Justify**: Provide compelling justification and update complexity tracking
3. **Override**: Update constitution max_projects threshold

**User Choice**: Option 1 (Simplify)

**Revised Plan**:
```markdown
## Constitutional Check
### Simplicity Gate (Article VII)
- ✓ Using 3 projects (API server, Worker queue, Admin panel)
- ✓ Below max_projects threshold (3)
- ✓ Microservices deferred to Phase 2 (post-MVP)

**Result**: SIMPLICITY GATE PASSED
```

**Quality Gate**: PASS → proceed to Tasking

**Lesson Learned**: Constitutional gates enforce constraints early, preventing over-engineering.

---

## Example 5: Parallel Execution Performance

### Scenario
Compare sequential vs parallel execution for Example 2 (Real-Time Chat).

### Sequential Execution (baseline)

**All 78 tasks run sequentially**:
```
Phase 1 (Setup): T001 → T002 → T003 → T004 → T005
  Estimated time: 15 minutes

Phase 2 (Foundational): T006 → T007 → T008 → T009 → T010
  Estimated time: 25 minutes

Phase 3-7 (User Stories): T011 → T012 → ... → T078
  Estimated time: 120 minutes

Total: 160 minutes
```

### Parallel Execution (optimized)

**28 tasks marked [P] run in parallel**:
```
Phase 1 (Setup): T001 → T002 → T003 → (T004 ∥ T005)
  Estimated time: 12 minutes (-20%)

Phase 2 (Foundational): T006 ∥ T007 ∥ T008 ∥ T009 ∥ T010
  Estimated time: 8 minutes (-68% due to 5-way parallelism)

Phase 3-7 (User Stories): Sequential with parallel sub-tasks
  Estimated time: 85 minutes (-29%)

Total: 105 minutes
```

**Speedup Factor**: 1.52x (160 / 105)
**Time Saved**: 55 minutes (34% reduction)

### Parallelization Breakdown

| Phase | Sequential | Parallel | Speedup | Parallel Tasks |
|-------|-----------|----------|---------|----------------|
| Setup | 15 min | 12 min | 1.25x | 2 of 5 |
| Foundational | 25 min | 8 min | 3.13x | 5 of 5 |
| US1 (Join Room) | 28 min | 20 min | 1.40x | 3 of 7 |
| US2 (Send/Receive) | 32 min | 24 min | 1.33x | 4 of 9 |
| US3 (Presence) | 18 min | 12 min | 1.50x | 2 of 6 |
| US4 (History) | 22 min | 16 min | 1.38x | 3 of 8 |
| US5 (Private Rooms) | 20 min | 13 min | 1.54x | 3 of 7 |

**Best Parallelism**: Foundational phase (5-way parallel, 3.13x speedup)
**Bottleneck**: User Story 2 (longest sequential chain)

---

## Conclusion

These examples demonstrate:

1. **Simple features** (Example 1): Fast specification-to-code (60-90 min)
2. **Complex features** (Example 2): Structured complexity management (3-4 hours)
3. **Incremental features** (Example 3): Efficient reuse of infrastructure (30-45 min)
4. **Quality gates** (Example 4): Early constraint enforcement prevents over-engineering
5. **Parallel execution** (Example 5): 1.5x speedup through intelligent task marking

**Key Patterns**:
- Constitutional governance prevents premature optimization
- Clarification limits (≤3) force informed guesses
- User story organization enables incremental delivery
- Parallel task marking optimizes execution time
- Artifact reuse accelerates follow-on features

**Artifact Flow Insights**:
- Templates constrain LLM behavior effectively
- Validation gates catch issues early (shift-left quality)
- Multi-stage decomposition manages complexity
- Test-driven development ensures quality
- Infrastructure reuse compounds efficiency gains

**Formula Effectiveness**:
- Clear artifact dependencies prevent confusion
- Quality gates with thresholds enable automation
- Parallel opportunities explicitly marked
- Validation rules enable programmatic checking
- Extensibility supports incremental features

These examples serve as reference implementations for applying the spec-kit workflow formula to real-world projects.
