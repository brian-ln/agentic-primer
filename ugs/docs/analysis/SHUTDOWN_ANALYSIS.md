# UGS SHUTDOWN vs NORMAL PERSISTENCE ANALYSIS

## THE QUESTION
**Is "shutdown" actually just "snapshot"?**

## SHORT ANSWER
**YES** - shutdown primarily creates a snapshot, but there are nuances.

## DETAILED ANALYSIS

### Normal Operation Persistence:
1. **Every command** → Write to WAL (Write-Ahead Log)
2. **Auto-snapshot** → Every 1000 events (configurable)
3. **Graceful exit** → All changes already persisted in WAL

### What "shutdown" Actually Does:
1. **Forces final snapshot** → Regardless of event count
2. **Ensures clean state** → All pending changes flushed
3. **Graceful process exit** → Proper cleanup

## PERSISTENCE MODEL BREAKDOWN

### Write-Ahead Log (WAL):
- **Every operation** writes to `data/events.wal` FIRST
- **Then** applies to in-memory structures
- **Guarantees** no data loss even with crashes

### Snapshots:
- **Periodic** full state dumps to `data/snapshot.json`
- **Faster startup** → Load snapshot + replay recent WAL events
- **"shutdown"** → Forces snapshot creation immediately

## BATCH MODE vs INTERACTIVE MODE

### Batch Mode (./ugs command):
```
./ugs add-node alice person name=Alice
↓
1. Load existing data (snapshot + WAL replay)
2. Execute command
3. Write event to WAL
4. Auto-snapshot if threshold reached
5. Exit process
```
**No explicit shutdown needed** - everything auto-persisted.

### Interactive Mode (./ugs):
```
./ugs
ugs> add-node alice person name=Alice    # → WAL
ugs> add-node bob person name=Bob        # → WAL  
ugs> shutdown                            # → Force snapshot + exit
```
**"shutdown" ensures final snapshot** before exit.

## SO IS SHUTDOWN JUST SNAPSHOT?

**Mostly YES, but:**

- **shutdown** = force snapshot + clean exit
- **Normal ops** = WAL + auto-snapshots
- **Crash recovery** = Load snapshot + replay WAL

The key insight: **WAL provides durability, snapshots provide performance**.

## WHEN DOES SNAPSHOTTING HAPPEN?

1. **Auto** - Every 1000 events (or configured interval)
2. **Shutdown** - Forces immediate snapshot
3. **Never lost** - WAL ensures no data loss between snapshots

## FOR AI AGENTS

You typically **DON'T need shutdown** because:
- Every operation persists to WAL immediately
- Auto-snapshots handle performance
- Batch mode exits cleanly automatically

**Use shutdown only for:**
- Interactive sessions
- Forcing immediate snapshot
- Clean process termination
