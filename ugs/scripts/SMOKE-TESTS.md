# Provider Smoke Tests

Quick reference for testing LLM provider availability and functionality.

## Overview

The Session Knowledge System works with multiple OpenAI-compatible providers:
- **LM Studio** (localhost:1234) - Free, local
- **Ollama** (localhost:11434) - Free, local
- **OpenAI** - Paid API
- Any OpenAI-compatible endpoint

## Running Tests

### Test All Providers
```bash
./scripts/smoke-test-all.sh
```

Shows summary of available providers and test results.

### Test Individual Providers

**LM Studio:**
```bash
./scripts/smoke-test-lmstudio.ts
```

**Ollama:**
```bash
./scripts/smoke-test-ollama.ts
```

**OpenAI:**
```bash
./scripts/smoke-test-openai.ts
```

## Exit Codes

- **0** - All tests passed
- **1** - Tests failed (provider available but tests failed)
- **2** - Provider unavailable (graceful skip)

## Test Coverage

Each provider test includes:
1. **Availability Check** - Can we connect to the provider?
2. **Chat Completion** - Basic text generation
3. **JSON Completion** - Structured output parsing
4. **Embeddings** - Vector generation

## Setup Requirements

### LM Studio
```bash
# 1. Download and start LM Studio
# 2. Load models from UI or CLI:
#    - Chat: llama-3.2-3b-instruct
#    - Embeddings: text-embedding-nomic-embed-text-v1.5
# 3. Ensure server is running on port 1234
```

### Ollama
```bash
# 1. Install Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# 2. Start Ollama service
ollama serve

# 3. Pull models
ollama pull llama3.2:3b
ollama pull nomic-embed-text
```

### OpenAI
```bash
# Set API key
export OPENAI_API_KEY="sk-..."

# Note: Tests will incur small costs (~$0.001)
```

## Configuration

Tests use environment variables for configuration:

```bash
# LM Studio (default)
export LLM_BASE_URL="http://localhost:1234/v1"
export LLM_MODEL="llama-3.2-3b-instruct"
export EMBEDDING_BASE_URL="http://localhost:1234/v1"
export EMBEDDING_MODEL="text-embedding-nomic-embed-text-v1.5"

# Ollama
export LLM_BASE_URL="http://localhost:11434/v1"
export LLM_MODEL="llama3.2:3b"
export EMBEDDING_BASE_URL="http://localhost:11434/v1"
export EMBEDDING_MODEL="nomic-embed-text"

# OpenAI
export OPENAI_API_KEY="sk-..."
export LLM_BASE_URL="https://api.openai.com/v1"
export LLM_MODEL="gpt-4o-mini"
export EMBEDDING_BASE_URL="https://api.openai.com/v1"
export EMBEDDING_MODEL="text-embedding-3-small"
```

See `.env.example` for complete configuration options.

## Troubleshooting

### "Provider unavailable" but it's running

**Check ports:**
```bash
# LM Studio
curl http://localhost:1234/v1/models

# Ollama
curl http://localhost:11434/v1/models
```

**Check models are loaded:**
```bash
# LM Studio
# Use the LM Studio UI to load models

# Ollama
ollama list
```

### Tests fail but provider is available

**Model not found:**
- LM Studio: Load the correct model in the UI
- Ollama: Pull the model with `ollama pull <model>`
- OpenAI: Check model name matches available models

**Network timeout:**
- Increase timeout in test script
- Check firewall settings
- Verify provider is accepting connections

### OpenAI tests skip

**API key not set:**
```bash
export OPENAI_API_KEY="sk-..."
```

**Invalid API key:**
- Check key is valid at platform.openai.com
- Ensure key has sufficient credits

## Integration with Main System

Once providers pass smoke tests, they can be used for:

1. **Knowledge Extraction** - `./extract-knowledge`
2. **Semantic Search** - `./know search "query"`
3. **Classification** - Automatic message categorization
4. **Embeddings** - Semantic similarity search

Configure your preferred provider in `.env` and all commands will use it automatically.

## Performance Benchmarks

Typical test execution times:

| Provider   | Total Time | Per Test |
|------------|-----------|----------|
| LM Studio  | ~5-8s     | ~1-2s    |
| Ollama     | ~4-7s     | ~1s      |
| OpenAI     | ~2-4s     | ~0.5s    |

*Times vary based on hardware, model size, and network latency.*

## Cost Estimates

Per smoke test run:

| Provider   | Cost      |
|------------|-----------|
| LM Studio  | $0 (free) |
| Ollama     | $0 (free) |
| OpenAI     | ~$0.001   |

OpenAI costs are minimal for testing but increase with production usage.
