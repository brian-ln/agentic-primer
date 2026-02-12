#!/bin/bash
# Helper script to copy files with metadata headers

SOURCE_FILE="$1"
DEST_FILE="$2"
CATEGORY="$3"
ORIGINAL_LOCATION="$4"

# Get current date
EXTRACT_DATE=$(date +"%Y-%m-%d")

# Get git commit SHA if in a git repo
if cd "$(dirname "$SOURCE_FILE")" 2>/dev/null && git rev-parse --git-dir > /dev/null 2>&1; then
    COMMIT_SHA=$(git log -1 --format="%H" -- "$(basename "$SOURCE_FILE")" 2>/dev/null || echo "N/A")
    cd - > /dev/null
else
    COMMIT_SHA="N/A"
fi

# Create metadata header
cat > "$DEST_FILE" << EOF
---
original_location: $ORIGINAL_LOCATION
extract_date: $EXTRACT_DATE
category: $CATEGORY
git_commit: $COMMIT_SHA
---

EOF

# Append original file content
cat "$SOURCE_FILE" >> "$DEST_FILE"

echo "✓ Extracted: $(basename "$SOURCE_FILE") → $CATEGORY/"
