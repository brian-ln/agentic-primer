#!/bin/bash
# Test script for P0 CLI improvements
# Demonstrates JSON output, batch operations, --yes flag, and field projection

set -e

echo "=== P0 CLI Features Test ==="
echo ""

# Clean start
rm -f tasks.json

# 1. Initialize
echo "1. Initialize tasks.json"
bun run src/cli/task.ts init
echo ""

# 2. Test JSON output for list command
echo "2. Test JSON output for list command"
bun run src/cli/task.ts list --json
echo ""

# 3. Test field projection
echo "3. Test field projection (--fields id,state,goal)"
bun run src/cli/task.ts list --json --fields id,state,goal
echo ""

# 4. Test batch-add
echo "4. Test batch-add with example file"
bun run src/cli/task.ts batch-add --file examples/batch-add-example.json --json
echo ""

# 5. Test list with filters and JSON
echo "5. Test list with filters (priority P0) and JSON"
bun run src/cli/task.ts list --priority P0 --json --fields id,goal,priority
echo ""

# 6. Test add command with JSON
echo "6. Test add command with JSON output"
bun run src/cli/task.ts add "Test task with JSON output" --priority P2 --labels test,demo --json
echo ""

# 7. Test ready command with JSON and field projection
echo "7. Test ready command with JSON and field projection"
bun run src/cli/task.ts ready --json --fields id,state,goal,priority
echo ""

# 8. Test show command with JSON
echo "8. Test show command with JSON"
bun run src/cli/task.ts show task_2 --json --fields id,state,goal,priority,labels
echo ""

# 9. Test update command with JSON
echo "9. Test update command (start) with JSON"
bun run src/cli/task.ts update task_3 start --json
echo ""

# 10. Test search with JSON
echo "10. Test search command with JSON"
bun run src/cli/task.ts search "API" --json
echo ""

# 11. Test graph with JSON
echo "11. Test graph command with JSON"
bun run src/cli/task.ts graph task_2 --json
echo ""

# 12. Test eval and status with JSON
echo "12. Test eval command with JSON"
bun run src/cli/task.ts eval task_2 --json
echo ""

echo "13. Test status command with JSON"
bun run src/cli/task.ts status task_2 --json
echo ""

# 14. Test batch-update
echo "14. Create batch update file for testing"
cat > /tmp/test-batch-update.json << 'EOF'
[
  {
    "id": "task_4",
    "action": "start"
  },
  {
    "id": "task_5",
    "action": "start"
  }
]
EOF

bun run src/cli/task.ts batch-update --file /tmp/test-batch-update.json --json
echo ""

# 15. Test --yes flag for delete (non-interactive)
echo "15. Test delete with --yes flag (non-interactive)"
bun run src/cli/task.ts delete task_6 --yes --json
echo ""

# 16. Test backward compatibility (human-friendly output)
echo "16. Test backward compatibility (human-friendly output)"
echo "--- List command without --json:"
bun run src/cli/task.ts list
echo ""

echo "--- Ready command without --json:"
bun run src/cli/task.ts ready
echo ""

echo "=== All P0 features tested successfully! ==="
