# UGS Actor Model - Agent-First Design

## Philosophy
**UGS defaults to agent-optimized output** - structured, parseable, minimal noise.
Human-friendly mode is available but optional.

## Actor Control

### Environment Variable (Recommended)
```bash
UGS_ACTOR=agent ./ugs stats    # Structured JSON output (default)
UGS_ACTOR=human ./ugs stats    # Verbose, friendly output
```

### Command Line Flags
```bash
./ugs --agent stats            # Force agent mode
./ugs --human stats            # Force human mode
```

### Default Behavior
- **No flag, no env var**: Agent mode (structured output)
- **Explicit flags**: Override environment variable
- **UGS_ACTOR=agent**: Agent mode
- **UGS_ACTOR=human**: Human mode

## Output Differences

### Agent Mode (Default)
```json
{"nodes":3,"edges":2,"events":5,"dataDirectory":"./data"}
```
- Compact JSON
- No emojis or decoration
- Machine-parseable
- No logging noise

### Human Mode
```
📊 UGS Graph Statistics:
  Actor Mode: human
  Data Directory: ./data
  Nodes: 3
  Edges: 2
  Events: 5
  
🔗 Most Connected:
  alice: 3 total (1 in, 2 out)
```
- Verbose descriptions
- Emojis and visual elements
- Helpful context
- Progress messages

## AI Agent Benefits

### Structured Output
```bash
./ugs get alice | jq .properties.name
./ugs search oauth | jq '.results[].id'
./ugs path alice bob | jq .distance
```

### Error Handling
```json
{"error":"Node not found","code":1}
```

### Batch Processing
```bash
echo "load-demo\nstats\nsearch alice" | ./ugs | jq .
```

## Implementation Details

- **Agent mode**: `console.log(JSON.stringify(data))`
- **Human mode**: `console.log("📊 Friendly message")`
- **Errors**: Always structured in agent mode
- **Interactive**: Minimal prompts in agent mode

This design makes UGS perfect for AI agents while preserving human usability when needed.
