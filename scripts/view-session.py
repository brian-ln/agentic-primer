#!/usr/bin/env python3
"""
View compact session log in human-readable format.
"""

import json
import sys
from pathlib import Path
from datetime import datetime

def format_timestamp(ts_str):
    """Format timestamp to readable local time."""
    try:
        dt = datetime.fromisoformat(ts_str.replace('Z', '+00:00'))
        return dt.strftime('%H:%M:%S')
    except:
        return ts_str

def view_session(log_file: Path, show_thinking: bool = False, filter_agent: str = None):
    """View compact session log."""
    
    with open(log_file, 'r') as f:
        # Read header
        header_line = f.readline()
        header = json.loads(header_line)
        
        print("=" * 80)
        print(f"Session: {header['session'][:8]}...")
        print(f"Started: {header['started']}")
        print(f"CWD: {header['cwd']}")
        print(f"Branch: {header['branch']}")
        print(f"CC Version: {header['cc_version']}")
        print("=" * 80)
        print()
        
        # Read entries
        for line_num, line in enumerate(f, 2):
            try:
                entry = json.loads(line)
                
                # Filter by agent if requested
                if filter_agent and entry.get('agent') != filter_agent:
                    continue
                
                # Handle context changes
                if 'ctx' in entry:
                    ctx_type = entry['ctx']
                    ctx_value = entry['v']
                    ts = format_timestamp(entry['t'])
                    print(f"[{ts}] >>> Context change: {ctx_type} = {ctx_value}")
                    print()
                    continue
                
                # Extract fields
                ts = format_timestamp(entry['t'])
                role = entry.get('r', '?')
                agent = entry.get('agent')
                
                # Skip thinking unless requested
                if entry.get('thinking') and not show_thinking:
                    continue
                
                # Format role indicator
                role_indicator = {
                    'user': '👤 USER',
                    'assistant': '🤖 ASSISTANT',
                    'system': '⚙️  SYSTEM'
                }.get(role, f'❓ {role.upper()}')
                
                # Add agent indicator if present
                if agent:
                    role_indicator = f'{role_indicator} [agent:{agent}]'
                
                # Print timestamp and role
                print(f"[{ts}] {role_indicator}")
                
                # Print message
                if 'm' in entry:
                    msg = entry['m']
                    if entry.get('full_size'):
                        print(f"  {msg}")
                        print(f"  [...truncated, full size: {entry['full_size']} chars]")
                    else:
                        # Indent multi-line messages
                        for line in msg.split('\n'):
                            print(f"  {line}")
                
                # Print thinking indicator
                elif entry.get('thinking'):
                    print("  [thinking...]")
                
                # Print tool use
                elif 'tool' in entry:
                    tool = entry['tool']
                    print(f"  🔧 {tool}", end='')
                    
                    if 'file' in entry:
                        print(f" → {entry['file']}", end='')
                        if 'size' in entry:
                            print(f" ({entry['size']} bytes)", end='')
                    elif 'cmd' in entry:
                        print(f" `{entry['cmd']}`", end='')
                    elif 'pattern' in entry:
                        print(f" pattern: {entry['pattern']}", end='')
                    elif 'query' in entry:
                        print(f" query: {entry['query']}", end='')
                    elif 'url' in entry:
                        print(f" {entry['url']}", end='')
                    
                    print()
                
                # Print tool result
                elif entry.get('tool_result'):
                    status = entry.get('status', 'unknown')
                    size = entry.get('size', 0)
                    items = entry.get('items', 0)
                    
                    status_icon = {
                        'success': '✅',
                        'error': '❌',
                        'unknown': '❓'
                    }.get(status, '❓')
                    
                    print(f"  {status_icon} Result:", end='')
                    
                    if items:
                        print(f" {items} items", end='')
                    elif size:
                        print(f" {size} bytes", end='')
                    
                    if 'msg' in entry:
                        print(f"\n    {entry['msg'][:100]}", end='')
                    
                    if 'agent' in entry and 'agent_status' in entry:
                        print(f"\n    Agent {entry['agent']}: {entry['agent_status']}", end='')
                    
                    print()
                
                print()
                
            except json.JSONDecodeError as e:
                print(f"Error parsing line {line_num}: {e}", file=sys.stderr)
                continue

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='View compact session log')
    parser.add_argument('log_file', type=Path, help='Compact session log file')
    parser.add_argument('--thinking', action='store_true', help='Show thinking blocks')
    parser.add_argument('--agent', type=str, help='Filter by agent ID')
    
    args = parser.parse_args()
    
    if not args.log_file.exists():
        print(f"Log file not found: {args.log_file}", file=sys.stderr)
        sys.exit(1)
    
    view_session(args.log_file, show_thinking=args.thinking, filter_agent=args.agent)
