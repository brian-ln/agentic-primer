# UGS Node Property Updates - Current Methods

## Current State: No Dedicated Update Commands ❌

UGS v0.0.4 doesn't have proper `update-node` commands yet. Here are your current options:

### Method 1: Overwrite with add-node (Works but overwrites everything)
```bash
# WARNING: This replaces ALL properties, not just updates
./ugs get alice                           # See current properties
./ugs add-node alice person name='Alice Johnson',role=senior_developer,status=active
./ugs get alice                           # Node completely replaced
```

**Pros**: Works with current UGS
**Cons**: Overwrites all properties, loses data if you forget any

### Method 2: Manual Property Management
```bash
# Get current properties, then re-add with changes
./ugs get alice                           # Copy existing properties
# Manually combine old + new properties in add-node command
./ugs add-node alice person name='Alice Johnson',role=developer,team=backend,status=updated,priority=high
```

**Pros**: Preserves existing data if done carefully  
**Cons**: Error-prone, manual, requires copying properties

## What's Missing (Enhancement Task)

We need proper update commands:
```bash
# Should exist but doesn't yet:
./ugs update-node alice status=active,priority=high    # Update specific properties
./ugs set-property alice status active                 # Set single property  
./ugs remove-property alice old_field                  # Remove property
```

## Recommended Approach for Now:

### For Single Property Updates:
1. Get current node: `./ugs get alice`
2. Note all existing properties
3. Use add-node with ALL properties (existing + new): 
   `./ugs add-node alice person name='Alice Johnson',role=developer,team=backend,status=NEW_VALUE`

### For Multiple Property Updates:
1. Get current state
2. Plan your complete property set
3. Use single add-node with complete property list

## Enhancement Priority

Property updates are a **critical missing feature** that should be added alongside the adjacency lists and property indexing enhancements. This is basic CRUD functionality that UGS currently lacks.

### Implementation Notes:
- Need `update-node`, `set-property`, `remove-property` commands
- Should emit `NodeUpdated` events for audit trail
- Must update indices when properties change
- Should preserve unchanged properties (merge, don't replace)
