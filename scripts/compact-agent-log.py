#!/usr/bin/env python3
"""
compact-agent-log.py - Convert full agent logs to minimal analysis schema

Usage: ./compact-agent-log.py <input.jsonl> [output.jsonl]

Minimal Schema (one JSON per line):
  {"t":"prompt","ts":"2026-01-06T02:25:09","text":"..."}
  {"t":"tool","ts":"...","name":"WebSearch","params":{...},"tok":{"in":3,"out":2,"c_r":0,"c_w":21311}}
  {"t":"result","ts":"...","tool":"WebSearch","status":"ok","size":1234}
  {"t":"response","ts":"...","text":"...","tok":{...}}

Where:
  t = type (prompt|tool|result|response)
  ts = timestamp (ISO 8601, truncated to seconds)
  tok = tokens {in, out, c_r=cache_read, c_w=cache_write}
  params = tool parameters (minimal)
  status = ok|error
  size = result size in chars (not full content)
"""

import json
import sys
from datetime import datetime
from pathlib import Path


def compact_timestamp(ts_str):
    """Convert full timestamp to seconds precision."""
    if not ts_str:
        return None
    # 2026-01-06T02:25:09.620Z -> 2026-01-06T02:25:09
    return ts_str[:19]


def compact_tokens(usage_dict):
    """Extract minimal token info."""
    if not usage_dict:
        return None

    return {
        "in": usage_dict.get("input_tokens", 0),
        "out": usage_dict.get("output_tokens", 0),
        "c_r": usage_dict.get("cache_read_input_tokens", 0),
        "c_w": usage_dict.get("cache_creation_input_tokens", 0)
    }


def compact_tool_params(tool_name, params):
    """Extract minimal tool parameters."""
    if tool_name == "WebSearch":
        return {"query": params.get("query", "")}
    elif tool_name == "Read":
        return {"file": params.get("file_path", "")}
    elif tool_name == "Write":
        return {"file": params.get("file_path", "")}
    elif tool_name == "Edit":
        return {"file": params.get("file_path", "")}
    elif tool_name == "Bash":
        # Keep command but truncate if very long
        cmd = params.get("command", "")
        return {"cmd": cmd[:100] + "..." if len(cmd) > 100 else cmd}
    else:
        # Generic: just keep keys, not values if large
        return {k: (v[:50] + "..." if isinstance(v, str) and len(v) > 50 else v)
                for k, v in params.items()}


def extract_text_from_content(content):
    """Extract text from content array or string."""
    if isinstance(content, str):
        return content

    if isinstance(content, list):
        # Find text blocks, skip tool_use/tool_result
        texts = []
        for item in content:
            if isinstance(item, dict):
                if item.get("type") == "text":
                    texts.append(item.get("text", ""))
        return "\n".join(texts) if texts else None

    return None


def compact_log(input_file, output_file=None):
    """Convert full agent log to compact schema."""

    if output_file is None:
        # Default: same name with .compact.jsonl extension
        input_path = Path(input_file)
        output_file = input_path.parent / f"{input_path.stem}.compact.jsonl"

    output_file = Path(output_file)

    entries_written = 0
    bytes_in = 0
    bytes_out = 0

    with open(input_file, 'r') as inf, open(output_file, 'w') as outf:
        for line_num, line in enumerate(inf, 1):
            bytes_in += len(line)

            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                print(f"Warning: Invalid JSON at line {line_num}", file=sys.stderr)
                continue

            entry_type = entry.get("type")
            message = entry.get("message", {})
            ts = compact_timestamp(entry.get("timestamp"))

            # User prompt
            if entry_type == "user":
                content = message.get("content")
                if content:
                    # Check if it's tool_result
                    if isinstance(content, list) and len(content) > 0:
                        first_item = content[0]
                        if isinstance(first_item, dict) and first_item.get("type") == "tool_result":
                            # This is a tool result
                            tool_id = first_item.get("tool_use_id", "")
                            result_content = first_item.get("content", "")

                            # Determine if error or success
                            is_error = first_item.get("is_error", False)
                            status = "error" if is_error else "ok"

                            # Get size instead of content
                            size = len(str(result_content)) if result_content else 0

                            compact = {
                                "t": "result",
                                "ts": ts,
                                "tool_id": tool_id,
                                "status": status,
                                "size": size
                            }

                            outf.write(json.dumps(compact) + "\n")
                            bytes_out += len(json.dumps(compact)) + 1
                            entries_written += 1
                            continue

                    # Regular user prompt
                    text = extract_text_from_content(content)
                    if text:
                        compact = {
                            "t": "prompt",
                            "ts": ts,
                            "text": text
                        }
                        outf.write(json.dumps(compact) + "\n")
                        bytes_out += len(json.dumps(compact)) + 1
                        entries_written += 1

            # Assistant response
            elif entry_type == "assistant":
                content = message.get("content", [])
                usage = message.get("usage")
                tokens = compact_tokens(usage)

                # Check if this is tool use
                if isinstance(content, list):
                    for item in content:
                        if isinstance(item, dict):
                            if item.get("type") == "tool_use":
                                # Tool call
                                tool_name = item.get("name", "")
                                tool_input = item.get("input", {})
                                tool_id = item.get("id", "")

                                compact = {
                                    "t": "tool",
                                    "ts": ts,
                                    "name": tool_name,
                                    "params": compact_tool_params(tool_name, tool_input),
                                    "id": tool_id
                                }

                                if tokens:
                                    compact["tok"] = tokens

                                outf.write(json.dumps(compact) + "\n")
                                bytes_out += len(json.dumps(compact)) + 1
                                entries_written += 1

                # Check for text response
                text = extract_text_from_content(content)
                if text and len(text.strip()) > 0:
                    compact = {
                        "t": "response",
                        "ts": ts,
                        "text": text[:500] + "..." if len(text) > 500 else text  # Truncate long responses
                    }

                    if tokens:
                        compact["tok"] = tokens

                    outf.write(json.dumps(compact) + "\n")
                    bytes_out += len(json.dumps(compact)) + 1
                    entries_written += 1

    # Stats
    compression_ratio = (1 - bytes_out / bytes_in) * 100 if bytes_in > 0 else 0

    print(f"Compacted: {input_file} -> {output_file}")
    print(f"  Input:  {bytes_in:,} bytes")
    print(f"  Output: {bytes_out:,} bytes ({compression_ratio:.1f}% smaller)")
    print(f"  Entries: {entries_written}")

    return output_file


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None

    if not Path(input_file).exists():
        print(f"Error: Input file not found: {input_file}", file=sys.stderr)
        sys.exit(1)

    compact_log(input_file, output_file)
