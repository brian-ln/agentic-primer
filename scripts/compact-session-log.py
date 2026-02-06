#!/usr/bin/env python3
"""
Compact session and agent logs into a minimal, replay-capable format.
"""

import json
import sys
from pathlib import Path
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

def summarize_tool_use(tool_name: str, input_data: Dict) -> Dict:
    """Summarize tool use arguments without full content."""
    summary = {"tool": tool_name}
    
    if tool_name == "Read":
        summary["file"] = input_data.get("file_path", "?")
    elif tool_name in ["Write", "Edit"]:
        summary["file"] = input_data.get("file_path", "?")
        if "content" in input_data:
            summary["size"] = len(input_data["content"])
    elif tool_name in ["Grep", "Glob"]:
        summary["pattern"] = input_data.get("pattern", "?")
    elif tool_name == "Bash":
        summary["cmd"] = input_data.get("command", "?")[:100]
    elif tool_name == "WebSearch":
        summary["query"] = input_data.get("query", "?")
    elif tool_name == "WebFetch":
        summary["url"] = input_data.get("url", "?")
    elif tool_name == "Task":
        summary["task"] = input_data.get("task", "?")[:100]
    else:
        # Generic: just note the tool was used
        summary["params"] = len(str(input_data))
    
    return summary

def summarize_tool_result(content: Any) -> Dict:
    """Summarize tool result without full content."""
    if isinstance(content, str):
        size = len(content)
        if "error" in content.lower() or "failed" in content.lower():
            preview = content[:200]
            return {"status": "error", "size": size, "msg": preview}
        return {"status": "success", "size": size}
    elif isinstance(content, dict):
        return {"status": "success", "size": len(str(content))}
    elif isinstance(content, list):
        return {"status": "success", "items": len(content)}
    return {"status": "unknown"}

def process_message_content(content: Any, role: str) -> Optional[Dict]:
    """Process message content and extract minimal representation."""
    if content is None:
        return None
    
    # String content (user messages)
    if isinstance(content, str):
        # Truncate very long messages
        if len(content) > 1000:
            return {"m": content[:1000] + "... [truncated]", "full_size": len(content)}
        return {"m": content}
    
    # Array content (assistant responses, tool results)
    if isinstance(content, list) and len(content) > 0:
        item = content[0]
        item_type = item.get("type")
        
        if item_type == "thinking":
            return {"thinking": True}
        
        elif item_type == "text":
            text = item.get("text", "")
            if len(text) > 1000:
                return {"m": text[:1000] + "... [truncated]", "full_size": len(text)}
            return {"m": text}
        
        elif item_type == "tool_use":
            tool_name = item.get("name", "unknown")
            tool_input = item.get("input", {})
            return summarize_tool_use(tool_name, tool_input)
        
        elif item_type == "tool_result":
            tool_content = item.get("content", "")
            result = summarize_tool_result(tool_content)
            result["tool_result"] = True
            
            # Check for agent results
            if isinstance(tool_content, str) and "agentId" in tool_content:
                if "<task_id>" in tool_content:
                    import re
                    agent_id = re.search(r'<task_id>([^<]+)</task_id>', tool_content)
                    status = re.search(r'<status>([^<]+)</status>', tool_content)
                    if agent_id:
                        result["agent"] = agent_id.group(1)
                    if status:
                        result["agent_status"] = status.group(1)
            
            return result
    
    return None

def compact_session(session_file: Path, output_file: Path):
    """Compact session log to minimal format."""
    
    # Find related agent logs
    session_dir = session_file.parent
    session_id = session_file.stem
    agent_files = list(session_dir.glob("agent-*.jsonl"))
    
    # Collect all entries
    all_entries = []
    
    # Parse session log
    with open(session_file, 'r') as f:
        for line_num, line in enumerate(f, 1):
            try:
                entry = json.loads(line)
                entry['_source'] = 'session'
                entry['_line'] = line_num
                all_entries.append(entry)
            except json.JSONDecodeError as e:
                print(f"Error parsing session line {line_num}: {e}", file=sys.stderr)
    
    # Parse agent logs
    for agent_file in agent_files:
        with open(agent_file, 'r') as f:
            agent_id = agent_file.stem.replace('agent-', '')
            for line_num, line in enumerate(f, 1):
                try:
                    entry = json.loads(line)
                    entry['_source'] = f'agent-{agent_id}'
                    entry['_line'] = line_num
                    entry['_agent_id'] = agent_id
                    all_entries.append(entry)
                except json.JSONDecodeError as e:
                    print(f"Error parsing {agent_file.name} line {line_num}: {e}", file=sys.stderr)
    
    # Sort by timestamp
    def get_timestamp(entry):
        ts = entry.get('timestamp')
        if ts is None:
            return datetime.max.replace(tzinfo=timezone.utc)
        try:
            # Parse ISO format with Z suffix
            if isinstance(ts, str):
                return datetime.fromisoformat(ts.replace('Z', '+00:00'))
            return datetime.max.replace(tzinfo=timezone.utc)
        except:
            return datetime.max.replace(tzinfo=timezone.utc)
    
    all_entries.sort(key=get_timestamp)
    
    # Extract session metadata from first entry
    if all_entries:
        first = all_entries[0]
        metadata = {
            "v": 1,
            "format": "compact-session-log",
            "session": first.get("sessionId", "unknown"),
            "cwd": first.get("cwd", "?"),
            "branch": first.get("gitBranch", "?"),
            "started": first.get("timestamp"),
            "cc_version": first.get("version")
        }
    else:
        print("No entries found!", file=sys.stderr)
        return
    
    # Write compact log
    current_context = {
        "branch": metadata["branch"],
        "cwd": metadata["cwd"]
    }
    
    with open(output_file, 'w') as out:
        # Write header
        out.write(json.dumps(metadata) + '\n')
        
        # Process entries
        for entry in all_entries:
            timestamp = entry.get('timestamp')
            if timestamp is None:
                continue  # Skip entries without timestamps
            
            entry_type = entry.get('type')
            
            # Skip noise
            if entry_type in ['queue-operation', 'summary']:
                continue
            
            # Check for context changes
            new_branch = entry.get('gitBranch')
            new_cwd = entry.get('cwd')
            
            if new_branch and new_branch != current_context["branch"]:
                out.write(json.dumps({
                    "ctx": "branch",
                    "v": new_branch,
                    "t": timestamp
                }) + '\n')
                current_context["branch"] = new_branch
            
            if new_cwd and new_cwd != current_context["cwd"]:
                out.write(json.dumps({
                    "ctx": "cwd",
                    "v": new_cwd,
                    "t": timestamp
                }) + '\n')
                current_context["cwd"] = new_cwd
            
            # Process message
            compact = {"t": timestamp}
            
            # Add agent info if from agent
            if '_agent_id' in entry:
                compact["agent"] = entry['_agent_id']
            
            # Add role
            if entry_type == 'system':
                # Skip empty system messages
                msg = entry.get('message', {})
                if not msg or not msg.get('content'):
                    continue
                compact["r"] = "system"
            elif entry_type in ['user', 'assistant']:
                compact["r"] = entry_type
            else:
                compact["r"] = entry_type
            
            # Process content
            message = entry.get('message', {})
            content = message.get('content')
            
            content_data = process_message_content(content, entry_type)
            if content_data:
                compact.update(content_data)
            
            # Write entry
            out.write(json.dumps(compact) + '\n')
    
    # Report stats
    original_size = session_file.stat().st_size
    for agent_file in agent_files:
        original_size += agent_file.stat().st_size
    
    compact_size = output_file.stat().st_size
    reduction = (1 - compact_size / original_size) * 100
    
    print(f"Original size: {original_size:,} bytes", file=sys.stderr)
    print(f"Compact size: {compact_size:,} bytes", file=sys.stderr)
    print(f"Reduction: {reduction:.1f}%", file=sys.stderr)
    print(f"Entries processed: {len(all_entries)}", file=sys.stderr)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: compact-session-log.py <session.jsonl> <output.jsonl>", file=sys.stderr)
        sys.exit(1)
    
    session_file = Path(sys.argv[1])
    output_file = Path(sys.argv[2])
    
    if not session_file.exists():
        print(f"Session file not found: {session_file}", file=sys.stderr)
        sys.exit(1)
    
    compact_session(session_file, output_file)
    print(f"Compact log written to: {output_file}", file=sys.stderr)
