# Visualizing SEAG: The Logical Design

## 1. The SEAG Stack (Layered Architecture)

This diagram shows how the system is layered from the human interface down to the physical reality.

```mermaid
graph TD
    subgraph "Interface Layer (The Shadow Actors)"
        UI[Web UI Actor]
        TTY[Terminal Actor]
    end

    subgraph "Agentic Layer (Intelligence)"
        Agent[BrainAgent]
        Proxy[UserProxy]
        Logic[AnalyticAgents]
    end

    subgraph "Graph Layer (Structure)"
        INode[Information Nodes]
        ANode[Algorithmic Nodes]
        Edge[Edge Actors]
    end

    subgraph "Truth Layer (Memory)"
        Log[EventLog / InteractionLog]
        Datalog[Graph Rules / Schema]
    end

    subgraph "Infrastructure (The Spoon Manager)"
        FS[FileEffectActor]
        Web[WebEffectActor]
        DB[DatabaseAdapter]
    end

    %% Flow
    UI <-->|JSON Messages| Proxy
    Proxy <--> Agent
    Agent <--> INode
    Agent <--> ANode
    INode <--> Log
    Log -->|Project| Datalog
    INode <--> FS
    ANode <--> Web
```

---

## 2. The Interaction Loop (Sense-Think-Act)

How a message flows through the system to fulfill a request.

```mermaid
sequenceDiagram
    participant User as Human (User)
    participant UI as UI shadow Actor
    participant Proxy as UserProxy Actor
    participant Brain as BrainAgent Actor
    participant Graph as Graph Node
    participant Log as InteractionLog

    User->>UI: Types "What is X?"
    UI->>Proxy: input(text)
    Proxy->>Log: append(user_msg)
    Proxy->>Brain: think(msg_id)
    
    Note over Brain: 1. Sense (Context)
    Brain->>Log: replay(history)
    Brain->>Graph: query(knowledge)
    
    Note over Brain: 2. Think (Inference)
    Brain->>Brain: LLM/State Machine
    
    Note over Brain: 3. Act (Tool Use)
    Brain->>Graph: execute(tool_node)
    
    Brain->>Proxy: output(response)
    Proxy->>UI: signal(new_msg)
    UI->>User: Displays response
```

---

## 3. The "No Spoon" Abstraction

How physical reality is projected into the graph.

```text
PHYSICAL REALITY (The Spoon)         LOGICAL GRAPH (The Reality)
============================         ===========================

[ /src/main.ts ] <──────────────┐    ( Node: main_ts_root )
                                │         │
[ HTTP GET /api ] <─────────────┼────▶ ( Node: api_endpoint )
                                │         │
[ process: bash ] <─────────────┘    ( Node: shell_executor )
                                          │
                                          ▼
                                 [ SEAG ADDRESSING ]
                          seag://logical_system/node_uuid
```

---

## 4. Structural Destructuring (Shredding)

Turning a flat file into a live tree of actors.

```mermaid
graph LR
    File[README.md] -->|Parser| Root[Doc Root Actor]
    Root --> H1[Title Actor]
    Root --> P1[Paragraph Actor]
    Root --> Code[CodeBlock Actor]
    
    subgraph "Live Observation"
        Agent[Linter Agent] -.->|watch| Code
        User[Human] -.->|edit| File
    end
    
    File -.->|signal| Root
    Root -.->|patch| Code
    Code -.->|signal| Agent
```
