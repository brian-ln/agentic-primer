# Graph-Based Actor Model Configuration

## Current State: Code-Baked ❌
The actor model is hard-coded in CLI logic with environment variable fallbacks.

## Enhanced Approach: Graph-Stored Configuration ✅

### Implementation Concept:
```typescript
class UGSCLI {
  private async initializeActorModel(): Promise<OutputFormat> {
    // Priority: 1. CLI flags (highest)
    if (process.argv.includes('--human')) {
      return { isAgent: false, structured: false, verbose: true };
    }
    if (process.argv.includes('--agent')) {
      return { isAgent: true, structured: true, verbose: false };
    }
    
    // Priority: 2. Graph-stored user preferences
    const userPrefs = this.store.get('_user_prefs');
    if (userPrefs?.properties.get('output_mode') === 'human') {
      return {
        isAgent: false,
        structured: userPrefs.properties.get('structured') || false,
        verbose: userPrefs.properties.get('verbose') || true
      };
    }
    
    // Priority: 3. Graph-stored system defaults
    const systemConfig = this.store.get('_system_config');
    if (systemConfig?.properties.get('actor_mode')) {
      return {
        isAgent: systemConfig.properties.get('actor_mode') === 'agent',
        structured: systemConfig.properties.get('structured') || true,
        verbose: systemConfig.properties.get('verbose') || false
      };
    }
    
    // Priority: 4. Environment variables
    const actor = process.env.UGS_ACTOR?.toLowerCase();
    if (actor === 'human') {
      return { isAgent: false, structured: false, verbose: true };
    }
    
    // Priority: 5. Default (agent mode)
    return { isAgent: true, structured: true, verbose: false };
  }
}
```

### Configuration Commands:

#### Set User Preferences:
```bash
./ugs config-set output_mode human
./ugs config-set verbose true  
./ugs config-set default_depth 4
```

#### Query Configuration:
```bash
./ugs config-get output_mode    # Get specific setting
./ugs config-get               # Get all configuration
./ugs get _user_prefs          # Raw node data
./ugs path _system_config _user_prefs  # Config relationships
```

### Benefits of Graph-Based Configuration:

1. **Persistent**: Settings survive across sessions
2. **Versioned**: Configuration changes tracked in event log  
3. **Queryable**: Can analyze configuration with UGS commands
4. **Hierarchical**: Support complex preference inheritance
5. **Discoverable**: Configuration visible via standard UGS queries
6. **Consistent**: Uses same primitives as all other data

### Configuration Hierarchy (Priority Order):
1. **Command-line flags** (`--agent`, `--human`) - Session override
2. **Graph-stored user preferences** (`_user_prefs` node) - Personal defaults
3. **Graph-stored system defaults** (`_system_config` node) - System policy  
4. **Environment variables** (`UGS_ACTOR`) - Process-level settings
5. **Hard-coded defaults** (agent mode) - Fallback

### Example Configuration Graph:
```
_system_config (system)
├── actor_mode: "agent"
├── output_format: "structured" 
└── default_depth: 3

_user_prefs (preferences)  
├── output_mode: "human"
├── verbose: true
└── show_emojis: true

_system_config --[overrides]--> _user_prefs
```

This makes UGS **self-configuring** and **self-documenting** - the configuration system uses UGS itself to store and manage settings.
