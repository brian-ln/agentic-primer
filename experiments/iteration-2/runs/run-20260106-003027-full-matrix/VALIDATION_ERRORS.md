# Validation Error Details

Quick reference for all syntax errors found during validation.

**Note:** Most shell script errors are shellcheck style warnings, not syntax errors.


## P1-S1-haiku - Shell Script

```
File: scripts_validate-issue.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S1-haiku/scripts_validate-issue.sh line 10:
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
^-------^ SC2034 (warning): REPO_ROOT appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- REPO_ROOT appears unused. Verify ...

File: scripts_process-completed-issue.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S1-haiku/scripts_process-completed-issue.sh line 56:
        jq '.last_updated = "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'"' "${KB_PATH}/index.json.bak" > "${KB_PATH}/index.json"
                              ^----------------------------^ SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S1-haiku/scripts_process-completed-issue.sh line 60:
        sed -i.bak2 's/"last_updated": "[^"]*"/"last_updated": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'"/g' "${KB_PATH}/index.json"
                                                                 ^----------------------------^ SC2046 (warning): Quote this to prevent word splitting.

For more information:
  https://www.shellcheck.net/wiki/SC2046 -- Quote this to prevent word splitt...


## P1-S1-opus - Markdown Errors

```
File: docs/knowledge/decisions/001-copilot-automation.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: docs/knowledge/decisions/001-copilot-bootstrap.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P1-S1-sonnet - Markdown Errors

```
File: docs-knowledge-patterns-README.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: docs-knowledge-decisions-001-use-rest-api.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P1-S3-haiku - Markdown Errors

```
File: .copilot-system-prompt.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: docs-knowledge-DECISIONS.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: docs-knowledge-INSIGHTS.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P1-S3-opus - Shell Script

```
File: scripts/analyze-logs.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-opus/scripts/analyze-logs.sh line 12:
RED='\033[0;31m'
^-^ SC2034 (warning): RED appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-opus/scripts/analyze-logs.sh line 13:
GREEN='\033[0;32m'
^---^ SC2034 (warning): GREEN appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-opus/scripts/analyze-logs.sh line 14:
YELLOW='\033[1;33m'
^----^ SC2034 (warning): YELLOW appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- GREEN appears unused. Verify use ...

File: scripts/create-improvement-pr.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-opus/scripts/create-improvement-pr.sh line 243:
    local pr_body="## Self-Improvement PR
          ^-----^ SC2034 (warning): pr_body appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- pr_body appears unused. Verify us...


## P1-S3-opus - Markdown Errors

```
File: SELF_REFLECTION.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: FILE_MANIFEST.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: docs/knowledge/insights/agent-performance.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: .github/copilot-instructions.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P1-S3-sonnet - YAML Errors

```
File: .github-workflows-knowledge-base-update.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-knowledge-base-update.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 82, in compose_node
    node = self.compose_sequence_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 111, in compose_sequence_node
    node.value.append(self.compose_node(node, index))
                      ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 127, in compose_mapping_node
    while not self.check_event(MappingEndEvent):
              ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 428, in parse_block_mapping_key
    if self.check_token(KeyToken):
       ~~~~~~~~~~~~~~~~^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 115, in check_token
    while self.need_more_tokens():
          ~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 152, in need_more_tokens
    self.stale_possible_simple_keys()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 291, in stale_possible_simple_keys
    raise ScannerError("while scanning a simple key", key.mark,
            "could not find expected ':'", self.get_mark())
yaml.scanner.ScannerError: while scanning a simple key
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-knowledge-base-update.yml", line 227, column 1
could not find expected ':'
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-knowledge-base-update.yml", line 229, column 1

File: .github-workflows-copilot-assign.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-copilot-assign.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 127, in compose_mapping_node
    while not self.check_event(MappingEndEvent):
              ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 428, in parse_block_mapping_key
    if self.check_token(KeyToken):
       ~~~~~~~~~~~~~~~~^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 115, in check_token
    while self.need_more_tokens():
          ~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 152, in need_more_tokens
    self.stale_possible_simple_keys()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 291, in stale_possible_simple_keys
    raise ScannerError("while scanning a simple key", key.mark,
            "could not find expected ':'", self.get_mark())
yaml.scanner.ScannerError: while scanning a simple key
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-copilot-assign.yml", line 133, column 1
could not find expected ':'
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-copilot-assign.yml", line 135, column 1

File: .github-workflows-validate-pr.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-validate-pr.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 41, in get_single_node
    raise ComposerError("expected a single document in the stream",
            document.start_mark, "but found another document",
            event.start_mark)
yaml.composer.ComposerError: expected a single document in the stream
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-validate-pr.yml", line 1, column 1
but found another document
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/.github-workflows-validate-pr.yml", line 301, column 1


## P1-S3-sonnet - Shell Script

```
File: scripts-validate-syntax.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-validate-syntax.sh line 24:
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
         ^--------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-validate-syntax.sh line 25:
readonly REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
         ^-------^ SC2155 (warning): Declare and assign separately to avoid masking return values.

For more information:
  https://www.shellcheck.net/wiki/SC2155 -- Declare and assign separately to ...

File: scripts-test-issue-flow.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 25:
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
         ^--------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 26:
readonly REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
         ^-------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 93:
    return $([ "$all_exist" == "true" ] && echo 0 || echo 1)
           ^-- SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 117:
    return $([ "$all_exist" == "true" ] && echo 0 || echo 1)
           ^-- SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 142:
    return $([ "$all_executable" == "true" ] && echo 0 || echo 1)
           ^-- SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 173:
    return $([ "$all_valid" == "true" ] && echo 0 || echo 1)
           ^-- SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 204:
    return $([ "$all_fields_present" == "true" ] && echo 0 || echo 1)
           ^-- SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 245:
    return $([ "$all_valid" == "true" ] && echo 0 || echo 1)
           ^-- SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 271:
    return $([ "$all_valid" == "true" ] && echo 0 || echo 1)
           ^-- SC2046 (warning): Quote this to prevent word splitting.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-test-issue-flow.sh line 341:
    local test_issue_data='{
          ^-------------^ SC2034 (warning): test_issue_data appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- test_issue_data appears unused. V...
  https://www.shellcheck.net/wiki/SC2046 -- Quote this to prevent word splitt...
  https://www.shellcheck.net/wiki/SC2155 -- Declare and assign separately to ...

File: scripts-extract-learnings.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-extract-learnings.sh line 24:
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
         ^--------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-extract-learnings.sh line 25:
readonly REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
         ^-------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-extract-learnings.sh line 313:
    local entry="- [PR #${pr_number}](${relative_insight}) - Category: ${category} ($(date +%Y-%m-%d))"
          ^---^ SC2155 (warning): Declare and assign separately to avoid masking return values.

For more information:
  https://www.shellcheck.net/wiki/SC2155 -- Declare and assign separately to ...

File: scripts-extract-learnings-v2.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P1-S3-sonnet/scripts-extract-learnings-v2.sh line 14:
YELLOW='\033[1;33m'
^----^ SC2034 (warning): YELLOW appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- YELLOW appears unused. Verify use...


## P1-S3-sonnet - Markdown Errors

```
File: docs-knowledge-decisions-001-architecture-v2.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: SOLUTION_DESIGN.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P2-S1-haiku - YAML Errors

```
File: .github/workflows/copilot-process.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/.github/workflows/copilot-process.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 127, in compose_mapping_node
    while not self.check_event(MappingEndEvent):
              ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 428, in parse_block_mapping_key
    if self.check_token(KeyToken):
       ~~~~~~~~~~~~~~~~^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 116, in check_token
    self.fetch_more_tokens()
    ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 162, in fetch_more_tokens
    self.stale_possible_simple_keys()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 291, in stale_possible_simple_keys
    raise ScannerError("while scanning a simple key", key.mark,
            "could not find expected ':'", self.get_mark())
yaml.scanner.ScannerError: while scanning a simple key
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/.github/workflows/copilot-process.yml", line 104, column 1
could not find expected ':'
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/.github/workflows/copilot-process.yml", line 107, column 1

File: .github/workflows/copilot-issue-driven.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/.github/workflows/copilot-issue-driven.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 127, in compose_mapping_node
    while not self.check_event(MappingEndEvent):
              ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 428, in parse_block_mapping_key
    if self.check_token(KeyToken):
       ~~~~~~~~~~~~~~~~^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 116, in check_token
    self.fetch_more_tokens()
    ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 162, in fetch_more_tokens
    self.stale_possible_simple_keys()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 291, in stale_possible_simple_keys
    raise ScannerError("while scanning a simple key", key.mark,
            "could not find expected ':'", self.get_mark())
yaml.scanner.ScannerError: while scanning a simple key
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/.github/workflows/copilot-issue-driven.yml", line 200, column 1
could not find expected ':'
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/.github/workflows/copilot-issue-driven.yml", line 203, column 1


## P2-S1-haiku - Shell Script

```
File: scripts/query-knowledge-base.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/query-knowledge-base.sh line 41:
KEYWORDS=$(echo "$SEARCH_QUERY" | tr '[:upper:]' '[:lower:]' | tr ' ' '\n')
^------^ SC2034 (warning): KEYWORDS appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- KEYWORDS appears unused. Verify u...

File: scripts/validate-issue.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 71:
  TEMPLATE_CHECK="fail"
  ^------------^ SC2034 (warning): TEMPLATE_CHECK appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 85:
  YAML_CHECK="skip"
  ^--------^ SC2034 (warning): YAML_CHECK appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 104:
  FIELDS_CHECK="fail"
  ^----------^ SC2034 (warning): FIELDS_CHECK appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 113:
  NUMBER_CHECK="fail"
  ^----------^ SC2034 (warning): NUMBER_CHECK appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 123:
  CONFIG_CHECK="fail"
  ^----------^ SC2034 (warning): CONFIG_CHECK appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 138:
    JSON_CHECK="skip"
    ^--------^ SC2034 (warning): JSON_CHECK appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 149:
  KB_DIR_CHECK="fail"
  ^----------^ SC2034 (warning): KB_DIR_CHECK appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S1-haiku/scripts/validate-issue.sh line 159:
  KB_INDEX_CHECK="fail"
  ^------------^ SC2034 (warning): KB_INDEX_CHECK appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- CONFIG_CHECK appears unused. Veri...


## P2-S1-haiku - Markdown Errors

```
File: docs/knowledge/decisions/workflow-architecture.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P2-S1-opus - Markdown Errors

```
File: docs/knowledge/decisions/001-issue-driven-workflow.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P2-S2-haiku - Shell Script

```
File: validate-issue.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-haiku/validate-issue.sh line 203:
  if [[ "$title" =~ -i "bug|error|issue|crash|fail" ]]; then
     ^-- SC1073 (error): Couldn't parse this test expression. Fix to allow more checks.
                       ^-- SC1072 (error): Expected test to end here (don't wrap commands in []/[[]]). Fix any mentioned problems and try again.

For more information:
  https://www.shellcheck.net/wiki/SC1072 -- Expected test to end here (don't ...
  https://www.shellcheck.net/wiki/SC1073 -- Couldn't parse this test expressi...


## P2-S2-haiku - Markdown Errors

```
File: docs/knowledge/insights/automation-learnings.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: docs/knowledge/decisions/workflow-architecture.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: bug.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: task.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: feature.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P2-S2-opus - Shell Script

```
File: scripts/validate-system.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-opus/scripts/validate-system.sh line 21:
YELLOW='\033[1;33m'
^----^ SC2034 (warning): YELLOW appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- YELLOW appears unused. Verify use...


## P2-S2-sonnet - Markdown Errors

```
File: docs/knowledge/README.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P2-S2-sonnet-TEST - YAML Errors

```
File: knowledge-base/practices/testing.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-sonnet-TEST/knowledge-base/practices/testing.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 82, in compose_node
    node = self.compose_sequence_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 110, in compose_sequence_node
    while not self.check_event(SequenceEndEvent):
              ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 392, in parse_block_sequence_entry
    raise ParserError("while parsing a block collection", self.marks[-1],
            "expected <block end>, but found %r" % token.id, token.start_mark)
yaml.parser.ParserError: while parsing a block collection
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-sonnet-TEST/knowledge-base/practices/testing.yml", line 59, column 5
expected <block end>, but found '?'
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-sonnet-TEST/knowledge-base/practices/testing.yml", line 63, column 5


## P2-S3-haiku - Shell Script

```
File: scripts-verify-copilot-system.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S3-haiku/scripts-verify-copilot-system.sh line 186:
      local count=$(find "$kb_dir/$subdir" -type f | wc -l)
            ^---^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S3-haiku/scripts-verify-copilot-system.sh line 205:
  local lines=$(wc -l < AGENT_LOG.jsonl)
        ^---^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S3-haiku/scripts-verify-copilot-system.sh line 282:
  local test_count=$(find tests -name "*.sh" | wc -l)
  ^---^ SC2168 (error): 'local' is only valid in functions.
        ^--------^ SC2155 (warning): Declare and assign separately to avoid masking return values.

For more information:
  https://www.shellcheck.net/wiki/SC2168 -- 'local' is only valid in functions.
  https://www.shellcheck.net/wiki/SC2155 -- Declare and assign separately to ...


## P2-S3-haiku - Markdown Errors

```
File: START_HERE.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P2-S3-sonnet - Shell Script

```
File: scripts-extract-patterns.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S3-sonnet/scripts-extract-patterns.sh line 230:
    while IFS='|' read -r hash author email subject date; do
                                      ^---^ SC2034 (warning): email appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- email appears unused. Verify use ...

File: scripts-bootstrap.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S3-sonnet/scripts-bootstrap.sh line 35:
readonly SCRIPT_NAME="$(basename "$0")"
         ^---------^ SC2034 (warning): SCRIPT_NAME appears unused. Verify use (or export if used externally).
         ^---------^ SC2155 (warning): Declare and assign separately to avoid masking return values.

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- SCRIPT_NAME appears unused. Verif...
  https://www.shellcheck.net/wiki/SC2155 -- Declare and assign separately to ...

File: scripts-test-workflow.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S3-sonnet/scripts-test-workflow.sh line 35:
            CLEANUP=true
            ^-----^ SC2034 (warning): CLEANUP appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- CLEANUP appears unused. Verify us...


## P2-S3-sonnet - Markdown Errors

```
File: COPILOT_SOLUTION.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P3-S1-haiku - YAML Errors

```
File: .github/ISSUE_TEMPLATE/task.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S1-haiku/.github/ISSUE_TEMPLATE/task.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 64, in compose_node
    if self.check_event(AliasEvent):
       ~~~~~~~~~~~~~~~~^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 449, in parse_block_mapping_value
    if not self.check_token(KeyToken, ValueToken, BlockEndToken):
           ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 116, in check_token
    self.fetch_more_tokens()
    ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 258, in fetch_more_tokens
    raise ScannerError("while scanning for the next token", None,
            "found character %r that cannot start any token" % ch,
            self.get_mark())
yaml.scanner.ScannerError: while scanning for the next token
found character '@' that cannot start any token
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S1-haiku/.github/ISSUE_TEMPLATE/task.yml", line 1, column 7


## P3-S1-sonnet - Markdown Errors

```
File: EXECUTIVE_SUMMARY.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: TEST_ISSUE.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P3-S3-haiku - YAML Errors

```
File: .github_workflows_issue-to-pr.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-haiku/.github_workflows_issue-to-pr.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 127, in compose_mapping_node
    while not self.check_event(MappingEndEvent):
              ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 428, in parse_block_mapping_key
    if self.check_token(KeyToken):
       ~~~~~~~~~~~~~~~~^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 116, in check_token
    self.fetch_more_tokens()
    ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/scanner.py", line 258, in fetch_more_tokens
    raise ScannerError("while scanning for the next token", None,
            "found character %r that cannot start any token" % ch,
            self.get_mark())
yaml.scanner.ScannerError: while scanning for the next token
found character '@' that cannot start any token
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-haiku/.github_workflows_issue-to-pr.yml", line 131, column 1


## P3-S3-haiku - Shell Script

```
File: scripts_validate-generated-files.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-haiku/scripts_validate-generated-files.sh line 355:
                FIX=true
                ^-^ SC2034 (warning): FIX appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- FIX appears unused. Verify use (o...

File: scripts_process-issue.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-haiku/scripts_process-issue.sh line 24:
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
^----------^ SC2034 (warning): PROJECT_ROOT appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- PROJECT_ROOT appears unused. Veri...


## P3-S3-opus - Markdown Errors

```
File: docs/knowledge/insights/001-bootstrap-learnings.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")

File: docs/knowledge/decisions/001-use-github-native-features.md
/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh: line 77: 0
0: arithmetic syntax error in expression (error token is "0")


## P3-S3-sonnet - YAML Errors

```
File: .github-ISSUE_TEMPLATE-task.yml
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import yaml, sys; yaml.safe_load(open('/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/.github-ISSUE_TEMPLATE-task.yml'))
                      ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 125, in safe_load
    return load(stream, SafeLoader)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/__init__.py", line 81, in load
    return loader.get_single_data()
           ~~~~~~~~~~~~~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/constructor.py", line 49, in get_single_data
    node = self.get_single_node()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 36, in get_single_node
    document = self.compose_document()
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 55, in compose_document
    node = self.compose_node(None, None)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 82, in compose_node
    node = self.compose_sequence_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 111, in compose_sequence_node
    node.value.append(self.compose_node(node, index))
                      ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 133, in compose_mapping_node
    item_value = self.compose_node(node, item_key)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 84, in compose_node
    node = self.compose_mapping_node(anchor)
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/composer.py", line 127, in compose_mapping_node
    while not self.check_event(MappingEndEvent):
              ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 98, in check_event
    self.current_event = self.state()
                         ~~~~~~~~~~^^
  File "/Users/bln/.local/share/mise/installs/python/3.13.11/lib/python3.13/site-packages/yaml/parser.py", line 438, in parse_block_mapping_key
    raise ParserError("while parsing a block mapping", self.marks[-1],
            "expected <block end>, but found %r" % token.id, token.start_mark)
yaml.parser.ParserError: while parsing a block mapping
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/.github-ISSUE_TEMPLATE-task.yml", line 18, column 7
expected <block end>, but found '<scalar>'
  in "/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/.github-ISSUE_TEMPLATE-task.yml", line 20, column 36


## P3-S3-sonnet - Shell Script

```
File: scripts-test-issue-flow.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-test-issue-flow.sh line 17:
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "$SCRIPT_DIR/..")"
^-------^ SC2034 (warning): REPO_ROOT appears unused. Verify use (or export if used externally).


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-test-issue-flow.sh line 237:
    local pr_number=${1:-"SIMULATED"}
          ^-------^ SC2034 (warning): pr_number appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- REPO_ROOT appears unused. Verify ...

File: scripts-extract-learnings.sh

In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-extract-learnings.sh line 102:
    export PR_TITLE=$(echo "$pr_json" | jq -r '.title')
           ^------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-extract-learnings.sh line 103:
    export PR_BODY=$(echo "$pr_json" | jq -r '.body // ""')
           ^-----^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-extract-learnings.sh line 104:
    export PR_AUTHOR=$(echo "$pr_json" | jq -r '.author.login')
           ^-------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-extract-learnings.sh line 105:
    export PR_MERGED_AT=$(echo "$pr_json" | jq -r '.mergedAt // ""')
           ^----------^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-extract-learnings.sh line 106:
    export PR_URL=$(echo "$pr_json" | jq -r '.url')
           ^----^ SC2155 (warning): Declare and assign separately to avoid masking return values.


In /Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S3-sonnet/scripts-extract-learnings.sh line 125:
    export FILE_COUNT=$(echo "$files_json" | jq '.files | length')
           ^--------^ SC2155 (warning): Declare and assign separately to avoid masking return values.

For more information:
  https://www.shellcheck.net/wiki/SC2155 -- Declare and assign separately to ...


---

*Extracted from VALIDATION_REPORT.md on Thu Jan  8 05:53:00 EST 2026*
