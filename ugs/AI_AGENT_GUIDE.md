# AI AGENT GUIDE TO UGS

## DISCOVERY: What is UGS?

**UGS is NOT a daemon** - it's a CLI graph database tool with persistent storage.

### Quick Discovery Process:
1. `./ugs help` - Human-readable overview
2. `./ugs --help` - Machine-readable JSON structure  
3. `./ugs load-demo` - Test basic functionality
4. `./ugs stats` - Understand current state

## ARCHITECTURE UNDERSTANDING

### Two Operating Modes:
- **Batch Mode**: `./ugs command args` (each command runs independently)
- **Interactive Mode**: `./ugs` (starts session, type commands)

### Persistence Model:
- **Event Sourcing**: All changes logged to Write-Ahead Log
- **Automatic Persistence**: Data survives between command invocations
- **Fast Startup**: Loads from snapshot + replays recent events

## THE SHUTDOWN CONFUSION

**Why does a CLI tool have "shutdown"?**

Answer: UGS has two very different usage patterns:

### Batch Mode (Normal Usage):
```bash
./ugs add-node alice person name=Alice   # Command exits automatically
./ugs search alice                       # New process, data still there
./ugs stats                              # Another process, data persists
```
**No "shutdown" needed** - each command auto-saves and exits.

### Interactive Mode:
```bash
./ugs                    # Starts interactive session
ugs> load-demo          # Commands run in same process
ugs> stats              
ugs> add-node test test
ugs> shutdown           # Graceful exit with final snapshot
```
**"shutdown" creates final snapshot** and exits the interactive session.

## AI AGENT USAGE RECOMMENDATIONS

### For Automation (Recommended):
```bash
# Each command is independent
./ugs load-demo
./ugs search alice
./ugs path alice auth_proj
./ugs add-node new_node person name=NewUser
./ugs stats
```

### For Batch Operations:
```bash
echo "load-demo
search alice  
stats
events 5" | ./ugs
```

### For Programmatic Integration:
```bash
# JSON output for parsing
./ugs get alice | jq .properties.name
./ugs --help | jq .ugs.commands
UGS_DATA_DIR=/tmp/agent ./ugs stats
```

## STORAGE LOCATION

Default: `./data/`
- `data/events.wal` - Write-Ahead Log (all changes)
- `data/snapshot.json` - Periodic full state

Configure with:
```bash
UGS_DATA_DIR=/custom/path ./ugs stats
./ugs --data-dir /custom/path stats
```

## WHAT AN AI AGENT WOULD DISCOVER

1. **"Is this a daemon?"** - No, but data persists between runs
2. **"Why shutdown command?"** - Only for interactive mode  
3. **"How does data survive?"** - Event sourcing with WAL
4. **"What's the fastest way?"** - Use batch mode: `./ugs command`
5. **"How do I integrate?"** - JSON output + environment variables

## COMMON AI AGENT PATTERNS

```bash
# Check if data exists
./ugs stats | grep "Nodes: 0" && ./ugs load-demo

# Search and process results  
./ugs search ${QUERY} | grep "Found" 

# Add data programmatically
./ugs add-node ${ID} ${TYPE} name=${NAME},role=${ROLE}

# Get structured data
./ugs get ${ID} | jq -r .properties.name

# Custom workspace
UGS_DATA_DIR=${WORKSPACE}/graph ./ugs stats
```

## KEY INSIGHT FOR AI AGENTS

UGS combines the **convenience of a CLI tool** with the **persistence of a database**:
- No daemon to manage
- No connection setup
- Data automatically persists  
- Fast startup via snapshots
- Event sourcing ensures durability

Perfect for AI agents who need graph operations without database complexity!
