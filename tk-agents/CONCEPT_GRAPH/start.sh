#!/bin/bash
# Quick start script for Concept Graph Web Application

echo "Starting Concept Graph Web Server..."
echo ""

cd "$(dirname "$0")"

# Check if bun is installed
if ! command -v bun &> /dev/null; then
    echo "Error: Bun is not installed"
    echo "Please install Bun from https://bun.sh"
    exit 1
fi

# Start server with hot reload
bun --hot server.ts
