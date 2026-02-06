#!/bin/bash

################################################################################
# Issue Validation Script
#
# Validates that a GitHub issue meets minimum requirements for processing
# by @copilot. Checks format, content, and metadata.
#
# Usage:
#   ./validate-issue.sh <issue-number> <issue-title> <issue-body> [labels...]
#
# Environment Variables:
#   DEBUG           - Enable debug output
#   QUIET           - Suppress all output except errors
#   MIN_TITLE_LEN   - Minimum title length (default: 5)
#   MIN_BODY_LEN    - Minimum body length (default: 10)
#
# Exit Codes:
#   0 - Validation successful
#   1 - Validation failed
#   2 - Invalid input format
#   3 - Missing required fields
#
# Author: GitHub Copilot
# Version: 1.0.0
################################################################################

set -euo pipefail

# Configuration
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly MIN_TITLE_LEN="${MIN_TITLE_LEN:-5}"
readonly MIN_BODY_LEN="${MIN_BODY_LEN:-10}"
readonly DEBUG="${DEBUG:-0}"
readonly QUIET="${QUIET:-0}"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

################################################################################
# Logging Functions
################################################################################

log_debug() {
  if [ "$DEBUG" == "1" ]; then
    echo -e "${BLUE}[DEBUG]${NC} $*" >&2
  fi
}

log_info() {
  if [ "$QUIET" != "1" ]; then
    echo -e "${GREEN}[INFO]${NC} $*" >&2
  fi
}

log_warn() {
  if [ "$QUIET" != "1" ]; then
    echo -e "${YELLOW}[WARN]${NC} $*" >&2
  fi
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*" >&2
}

################################################################################
# Validation Functions
################################################################################

# Validate that all required arguments are provided
validate_arguments() {
  if [ $# -lt 3 ]; then
    log_error "Usage: $0 <issue-number> <issue-title> <issue-body> [labels...]"
    return 2
  fi
}

# Validate issue number format
validate_issue_number() {
  local issue_number="$1"

  if ! [[ "$issue_number" =~ ^[0-9]+$ ]]; then
    log_error "Invalid issue number: $issue_number (must be numeric)"
    return 1
  fi

  if [ "$issue_number" -le 0 ]; then
    log_error "Invalid issue number: $issue_number (must be positive)"
    return 1
  fi

  log_debug "Issue number valid: #$issue_number"
  return 0
}

# Validate issue title
validate_title() {
  local title="$1"
  local min_len="${MIN_TITLE_LEN}"

  if [ -z "$title" ]; then
    log_error "Issue title is empty"
    return 1
  fi

  if [ ${#title} -lt "$min_len" ]; then
    log_error "Title too short (${#title} chars, minimum: $min_len)"
    log_error "Title: $title"
    return 1
  fi

  if [ ${#title} -gt 200 ]; then
    log_warn "Title is very long (${#title} chars, recommended max: 100)"
  fi

  log_debug "Title valid (${#title} chars): $title"
  return 0
}

# Validate issue body
validate_body() {
  local body="$1"
  local min_len="${MIN_BODY_LEN}"

  if [ -z "$body" ]; then
    log_error "Issue body is empty"
    return 1
  fi

  if [ ${#body} -lt "$min_len" ]; then
    log_error "Body too short (${#body} chars, minimum: $min_len)"
    return 1
  fi

  log_debug "Body valid (${#body} chars)"
  return 0
}

# Validate issue has required labels
validate_labels() {
  local -a labels=("$@")
  local required_labels=("ai-task" "enhancement" "bug")
  local has_required=0

  if [ ${#labels[@]} -eq 0 ]; then
    log_warn "No labels provided"
    return 0
  fi

  for label in "${labels[@]}"; do
    log_debug "Checking label: $label"

    for required in "${required_labels[@]}"; do
      if [ "$label" == "$required" ]; then
        has_required=1
        log_info "Found required label: $label"
        break
      fi
    done
  done

  if [ $has_required -eq 0 ]; then
    log_warn "No processing label found (ai-task, enhancement, bug)"
    log_warn "Labels provided: ${labels[*]}"
  fi

  return 0
}

# Check for common formatting issues
check_formatting() {
  local title="$1"
  local body="$2"

  # Check for all uppercase title
  if [ "$title" == "${title^^}" ] && [ ${#title} -gt 10 ]; then
    log_warn "Title is all uppercase (consider sentence case)"
  fi

  # Check for multiple consecutive newlines in body
  if [[ "$body" =~ $'\n\n\n' ]]; then
    log_warn "Body has excessive blank lines"
  fi

  # Check for minimal punctuation
  if ! [[ "$body" =~ \. ]]; then
    log_warn "Body has no periods (consider adding punctuation)"
  fi

  return 0
}

# Check for required keywords in body (based on issue type)
check_content_completeness() {
  local title="$1"
  local body="$2"

  # Check if appears to be a bug report
  if [[ "$title" =~ -i "bug|error|issue|crash|fail" ]]; then
    if ! [[ "$body" =~ -i "steps|reproduce|version|environment" ]]; then
      log_warn "Possible bug report missing standard sections (steps, version, environment)"
    fi
  fi

  # Check if appears to be a feature request
  if [[ "$title" =~ -i "feature|enhancement|add|implement" ]]; then
    if ! [[ "$body" =~ -i "use case|benefit|would like|should" ]]; then
      log_warn "Possible feature request missing use case description"
    fi
  fi

  return 0
}

# Check if similar issue already exists (simulation)
check_duplicate() {
  local title="$1"

  log_debug "Checking for potential duplicate: $title"
  log_info "Duplicate check: Would query GitHub API for similar issues"

  # In real implementation, would query GitHub for similar issues
  return 0
}

################################################################################
# Main Validation
################################################################################

main() {
  local exit_code=0

  log_info "Starting issue validation..."

  # Validate arguments
  if ! validate_arguments "$@"; then
    return 2
  fi

  local issue_number="$1"
  local issue_title="$2"
  local issue_body="$3"
  shift 3
  local -a labels=("$@")

  # Run all validations
  validate_issue_number "$issue_number" || exit_code=1
  validate_title "$issue_title" || exit_code=1
  validate_body "$issue_body" || exit_code=1
  validate_labels "${labels[@]}" || exit_code=1
  check_formatting "$issue_title" "$issue_body" || exit_code=1
  check_content_completeness "$issue_title" "$issue_body" || exit_code=1
  check_duplicate "$issue_title" || exit_code=1

  # Summary
  if [ $exit_code -eq 0 ]; then
    log_info "✓ Issue validation passed"
    echo "VALID"
    return 0
  else
    log_error "✗ Issue validation failed"
    echo "INVALID"
    return 1
  fi
}

# Run main function
main "$@"
