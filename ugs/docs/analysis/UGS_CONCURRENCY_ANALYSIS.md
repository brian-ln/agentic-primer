# UGS Multi-Actor Concurrency Analysis

## Current State: SINGLE-USER DESIGN WITH CRITICAL GAPS ❌

### Identified Concurrency Problems:

#### 1. **Shared Actor Configuration**
- **Current**: Single global actor mode per CLI instance
- **Problem**: Agent A sets `UGS_ACTOR=human`, affects all subsequent operations
- **Impact**: Output format conflicts, race conditions between agents

#### 2. **No Context Isolation** 
- **Current**: All actors share same graph namespace
- **Problem**: Agent A adds `temp_analysis_node`, Agent B sees it in queries
- **Impact**: Data pollution, context bleeding, false results

#### 3. **Single Event Stream**
- **Current**: All changes go to one `events.wal` file
- **Problem**: No attribution - can't tell which actor made which change  
- **Impact**: No audit trail, impossible to rollback specific actor's work

#### 4. **No Session Management**
- **Current**: Each CLI invocation is independent
- **Problem**: No concept of work sessions or temporary contexts
- **Impact**: Can't maintain actor-specific state across commands

## SOLUTION: Session-Based Multi-Actor Architecture

### Core Design: SESSION CONTEXTS

Each actor gets an isolated working context:

```typescript
interface SessionContext {
  id: string;                    // Unique session identifier
  actor: 'agent' | 'human';      // Actor type with preferences
  namespace: string;             // Data isolation prefix
  startTime: number;             // Session creation
  config: Map<string, any>;      // Session-specific settings
  tempNodes: Set<string>;        // Temporary data for cleanup
  locks: Set<string>;           // Currently locked resources
}
```

### Implementation Strategy:

#### Phase 1: Session-Aware CLI
```bash
# Auto-generate session ID or explicit
./ugs session start --actor=agent --name=analysis_task
# Returns: {"sessionId": "sess_20240121_1141_001", "namespace": "sess_001"}

# All commands use session context
UGS_SESSION=sess_001 ./ugs add-node temp_analysis task status=in_progress
UGS_SESSION=sess_001 ./ugs search "analysis"  # Only sees sess_001 data

# Different session, different context
UGS_SESSION=sess_002 ./ugs session start --actor=human --name=exploration  
UGS_SESSION=sess_002 ./ugs search "analysis"  # Doesn't see sess_001 data
```

#### Phase 2: Namespaced Data Storage
```bash
# Session data gets namespace prefix
sess_001.temp_analysis_node     # Agent's temporary work
sess_002.exploration_notes      # Human's exploration
global.permanent_user_data      # Shared permanent data

# Cross-namespace queries when needed
./ugs search "user" --namespace=global      # Shared data only
./ugs search "temp" --namespace=sess_001    # Session-specific data
./ugs search "temp" --all-namespaces       # Cross-session search
```

#### Phase 3: Concurrent-Safe Operations
```typescript
class SessionManager {
  private sessions = new Map<string, SessionContext>();
  
  async executeWithSession<T>(sessionId: string, operation: () => Promise<T>): Promise<T> {
    const session = this.sessions.get(sessionId);
    if (!session) throw new Error(`Invalid session: ${sessionId}`);
    
    // Apply session-specific actor configuration
    this.applySessionConfig(session);
    
    try {
      return await operation();
    } finally {
      // Cleanup session locks
      session.locks.clear();
    }
  }
  
  private applySessionConfig(session: SessionContext): void {
    // Set actor-specific output format for this operation
    process.env.UGS_ACTOR_OVERRIDE = session.actor;
    process.env.UGS_NAMESPACE = session.namespace;
  }
}
```

### Enhanced UGS Commands for Multi-Actor:

#### Session Management:
```bash
./ugs session start --actor=agent --name=task1
./ugs session list
./ugs session switch sess_001  
./ugs session cleanup sess_002  # Remove temp data
./ugs session merge sess_001 sess_002  # Combine sessions
```

#### Context-Aware Operations:
```bash
# Session-isolated operations
./ugs --session=sess_001 add-node analysis task
./ugs --session=sess_002 add-node notes document

# Cross-session queries
./ugs search "analysis" --session=sess_001     # Only session data
./ugs search "analysis" --global              # Only permanent data  
./ugs search "analysis" --all-sessions       # Everything

# Session-specific configuration
./ugs --session=sess_001 config-set output_format=structured
./ugs --session=sess_002 config-set output_format=human
```

#### Conflict Resolution:
```bash
# Detect conflicts
./ugs conflicts check

# Resolve conflicts  
./ugs conflicts resolve --strategy=last_writer_wins
./ugs conflicts resolve --strategy=merge_properties
./ugs conflicts resolve --strategy=create_branches
```

### Event Attribution & Audit:

Enhanced event structure:
```typescript
interface UGSEvent {
  type: EventType;
  data: any;
  timestamp: number;
  sessionId: string;        // Which session made this change
  actor: 'agent' | 'human'; // Actor type
  namespace: string;        // Data namespace
  conflicts?: string[];     // Any conflicts detected
}
```

Query by context:
```bash
./ugs events --session=sess_001        # Agent's changes only
./ugs events --actor=human            # All human changes
./ugs events --namespace=global       # Shared data changes
./ugs events --conflicts             # Show conflict events
```

## Benefits of Session-Based Architecture:

1. **✅ True Concurrency**: Multiple agents + humans work simultaneously
2. **✅ Context Isolation**: Temporary work doesn't pollute shared data
3. **✅ Actor-Specific Config**: Each session has own output preferences  
4. **✅ Complete Audit Trail**: Every change attributed to session/actor
5. **✅ Conflict Detection**: Handle simultaneous edits gracefully
6. **✅ Session Recovery**: Can cleanup or restore abandoned work

## Migration Path:

### Current Single-User → Multi-Actor:
1. **Backwards Compatible**: Existing commands work (use default session)
2. **Opt-in Sessions**: New `session` commands for multi-actor scenarios  
3. **Gradual Enhancement**: Add session features incrementally
4. **Configuration Evolution**: Move from env vars to session-based config

This transforms UGS from a **single-user graph tool** into a **true multi-actor collaborative database** while maintaining full backwards compatibility.
