-- Migration 005: Add Knowledge Graph Relationships Table
-- Epic: Graph-Addressable Knowledge System
-- Date: 2026-02-03
--
-- Creates table for storing typed relationships between knowledge nodes.
-- Enables graph traversal and confidence propagation.

-- Create relationships table
CREATE TABLE IF NOT EXISTS session_relationships (
  id TEXT PRIMARY KEY,
  type TEXT NOT NULL,
  from_address TEXT NOT NULL,
  to_address TEXT NOT NULL,
  strength REAL DEFAULT 1.0,
  evidence TEXT,
  created INTEGER NOT NULL,
  metadata TEXT,

  -- Constraints
  CHECK(type IN ('supports', 'contradicts', 'requires', 'extends', 'questions', 'related-to')),
  CHECK(strength BETWEEN 0.0 AND 1.0)
);

-- Create indexes for fast lookups
CREATE INDEX IF NOT EXISTS idx_relationships_from ON session_relationships(from_address);
CREATE INDEX IF NOT EXISTS idx_relationships_to ON session_relationships(to_address);
CREATE INDEX IF NOT EXISTS idx_relationships_type ON session_relationships(type);
CREATE INDEX IF NOT EXISTS idx_relationships_from_type ON session_relationships(from_address, type);
CREATE INDEX IF NOT EXISTS idx_relationships_to_type ON session_relationships(to_address, type);

-- Update schema version
INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('schema_version', '5-relationships', strftime('%s', 'now'));

-- Add migration record
INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('migration_005_applied', datetime('now'), strftime('%s', 'now'));
