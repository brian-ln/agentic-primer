#!/bin/bash
# Smoke test all providers
# Epic: agentic-primer-t49.9
#
# Runs smoke tests for:
# - LM Studio (localhost:1234)
# - Ollama (localhost:11434)
# - OpenAI (API key required)
#
# Exit codes:
# 0 - All available providers passed
# 1 - One or more available providers failed

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Color output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

echo -e "${BLUE}${BOLD}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  Multi-Provider Smoke Tests"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "${NC}"

providers_available=0
providers_passed=0
providers_failed=0
providers_skipped=0

# Test LM Studio
echo -e "${BLUE}Testing LM Studio...${NC}"
if "$SCRIPT_DIR/smoke-test-lmstudio.ts" 2>&1; then
  providers_available=$((providers_available + 1))
  providers_passed=$((providers_passed + 1))
else
  exit_code=$?
  if [ $exit_code -eq 2 ]; then
    providers_skipped=$((providers_skipped + 1))
  else
    providers_available=$((providers_available + 1))
    providers_failed=$((providers_failed + 1))
  fi
fi

# Test Ollama
echo -e "${BLUE}Testing Ollama...${NC}"
if "$SCRIPT_DIR/smoke-test-ollama.ts" 2>&1; then
  providers_available=$((providers_available + 1))
  providers_passed=$((providers_passed + 1))
else
  exit_code=$?
  if [ $exit_code -eq 2 ]; then
    providers_skipped=$((providers_skipped + 1))
  else
    providers_available=$((providers_available + 1))
    providers_failed=$((providers_failed + 1))
  fi
fi

# Test OpenAI
echo -e "${BLUE}Testing OpenAI...${NC}"
if "$SCRIPT_DIR/smoke-test-openai.ts" 2>&1; then
  providers_available=$((providers_available + 1))
  providers_passed=$((providers_passed + 1))
else
  exit_code=$?
  if [ $exit_code -eq 2 ]; then
    providers_skipped=$((providers_skipped + 1))
  else
    providers_available=$((providers_available + 1))
    providers_failed=$((providers_failed + 1))
  fi
fi

# Summary
echo -e "${BLUE}${BOLD}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "${NC}"

total_providers=3
echo -e "   Providers available: ${BOLD}$providers_available/$total_providers${NC}"
echo -e "   Providers skipped:   ${YELLOW}$providers_skipped${NC}"
echo

if [ $providers_failed -eq 0 ]; then
  echo -e "${GREEN}${BOLD}   ✅ All available providers passed ($providers_passed/$providers_available)${NC}"
  echo

  if [ $providers_skipped -gt 0 ]; then
    echo -e "${YELLOW}   To enable more providers:${NC}"
    if ! "$SCRIPT_DIR/smoke-test-lmstudio.ts" &>/dev/null; then
      echo -e "   - Start LM Studio at localhost:1234"
    fi
    if ! "$SCRIPT_DIR/smoke-test-ollama.ts" &>/dev/null; then
      echo -e "   - Install Ollama: https://ollama.ai"
    fi
    if ! "$SCRIPT_DIR/smoke-test-openai.ts" &>/dev/null; then
      echo -e "   - Set OPENAI_API_KEY environment variable"
    fi
    echo
  fi

  exit 0
else
  echo -e "${RED}${BOLD}   ❌ $providers_failed provider(s) failed${NC}"
  echo -e "   ${GREEN}$providers_passed passed${NC}, ${RED}$providers_failed failed${NC}"
  echo
  exit 1
fi
