-- Session Knowledge System - Cognitive Integration v1
-- Epic: agentic-primer-9ad
-- Phase: Cognitive Integration - Bi-temporal tracking, Confidence decay, Thinking arcs
--
-- This schema extends schema-libsql.sql with cognitive features:
-- 1. Bi-temporal tracking (valid_time + transaction_time)
-- 2. Confidence decay (domain-specific aging)
-- 3. Thinking arc detection (conceptual evolution tracking)
-- 4. Cross-system relationships (how knowledge connects)
--
-- DESIGN: All changes are ADDITIVE. Existing columns preserved for backward compatibility.

-- ============================================================================
-- ENHANCED KNOWLEDGE TABLES (Bi-temporal + Confidence Decay)
-- ============================================================================

-- Add bi-temporal columns to session_decisions
-- valid_time: When this knowledge was actually true in the real world
-- transaction_time: When we learned about it (when it entered our system)
ALTER TABLE session_decisions ADD COLUMN valid_from INTEGER;
ALTER TABLE session_decisions ADD COLUMN valid_to INTEGER; -- NULL = still valid
ALTER TABLE session_decisions ADD COLUMN transaction_from INTEGER;
ALTER TABLE session_decisions ADD COLUMN transaction_to INTEGER; -- NULL = current version

-- Add confidence decay columns
ALTER TABLE session_decisions ADD COLUMN base_confidence REAL DEFAULT 0.0;
ALTER TABLE session_decisions ADD COLUMN decay_rate REAL; -- Domain-specific decay rate
ALTER TABLE session_decisions ADD COLUMN domain TEXT; -- 'tech' | 'science' | 'news' | 'core'
ALTER TABLE session_decisions ADD COLUMN message_id TEXT; -- Link to specific message

-- Add bi-temporal columns to session_learnings
ALTER TABLE session_learnings ADD COLUMN valid_from INTEGER;
ALTER TABLE session_learnings ADD COLUMN valid_to INTEGER;
ALTER TABLE session_learnings ADD COLUMN transaction_from INTEGER;
ALTER TABLE session_learnings ADD COLUMN transaction_to INTEGER;

-- Add confidence decay columns (learnings already has some of these)
ALTER TABLE session_learnings ADD COLUMN base_confidence REAL DEFAULT 0.0;
ALTER TABLE session_learnings ADD COLUMN decay_rate REAL;
ALTER TABLE session_learnings ADD COLUMN domain TEXT;
ALTER TABLE session_learnings ADD COLUMN category TEXT; -- technical|architectural|tooling|process|performance
ALTER TABLE session_learnings ADD COLUMN evidence TEXT; -- Supporting context
ALTER TABLE session_learnings ADD COLUMN application TEXT; -- How to apply this

-- Add bi-temporal columns to session_errors
ALTER TABLE session_errors ADD COLUMN valid_from INTEGER;
ALTER TABLE session_errors ADD COLUMN valid_to INTEGER;
ALTER TABLE session_errors ADD COLUMN transaction_from INTEGER;
ALTER TABLE session_errors ADD COLUMN transaction_to INTEGER;

-- Add confidence decay columns
ALTER TABLE session_errors ADD COLUMN base_confidence REAL DEFAULT 0.0;
ALTER TABLE session_errors ADD COLUMN decay_rate REAL;
ALTER TABLE session_errors ADD COLUMN domain TEXT;
ALTER TABLE session_errors ADD COLUMN resolution TEXT; -- How the error was fixed
ALTER TABLE session_errors ADD COLUMN prevention TEXT; -- How to avoid in the future

-- Add bi-temporal columns to session_workflows
ALTER TABLE session_workflows ADD COLUMN valid_from INTEGER;
ALTER TABLE session_workflows ADD COLUMN valid_to INTEGER;
ALTER TABLE session_workflows ADD COLUMN transaction_from INTEGER;
ALTER TABLE session_workflows ADD COLUMN transaction_to INTEGER;

-- Add decay columns (workflows already has confidence)
ALTER TABLE session_workflows ADD COLUMN base_confidence REAL DEFAULT 0.0;
ALTER TABLE session_workflows ADD COLUMN decay_rate REAL;
ALTER TABLE session_workflows ADD COLUMN domain TEXT;

-- ============================================================================
-- KNOWLEDGE RELATIONSHIPS (How knowledge connects)
-- ============================================================================

CREATE TABLE IF NOT EXISTS knowledge_relationships (
  id TEXT PRIMARY KEY,
  from_type TEXT NOT NULL, -- 'decision' | 'learning' | 'error' | 'workflow'
  from_id TEXT NOT NULL,
  to_type TEXT NOT NULL,
  to_id TEXT NOT NULL,
  relationship_type TEXT NOT NULL, -- 'supports' | 'contradicts' | 'supersedes' | 'evolves_from' | 'depends_on'
  confidence REAL DEFAULT 0.0,
  created_at INTEGER NOT NULL,
  evidence TEXT, -- Why this relationship exists
  UNIQUE(from_type, from_id, to_type, to_id, relationship_type)
);

CREATE INDEX IF NOT EXISTS idx_relationships_from ON knowledge_relationships(from_type, from_id);
CREATE INDEX IF NOT EXISTS idx_relationships_to ON knowledge_relationships(to_type, to_id);
CREATE INDEX IF NOT EXISTS idx_relationships_type ON knowledge_relationships(relationship_type);

-- ============================================================================
-- THINKING ARCS (Conceptual evolution tracking)
-- ============================================================================

CREATE TABLE IF NOT EXISTS thinking_arcs (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  arc_type TEXT NOT NULL, -- 'concrete_to_abstract' | 'pattern_discovery' | 'refinement' | 'breakthrough'
  start_message_id TEXT NOT NULL,
  end_message_id TEXT,
  description TEXT NOT NULL,
  breakthrough_moment TEXT, -- Key insight or realization
  confidence REAL DEFAULT 0.0,
  created_at INTEGER NOT NULL,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_thinking_arcs_session ON thinking_arcs(session_id);
CREATE INDEX IF NOT EXISTS idx_thinking_arcs_type ON thinking_arcs(arc_type);
CREATE INDEX IF NOT EXISTS idx_thinking_arcs_created ON thinking_arcs(created_at);

-- ============================================================================
-- TEMPORAL QUERY HELPERS
-- ============================================================================

-- View: Current valid knowledge (valid_to IS NULL)
CREATE VIEW IF NOT EXISTS current_decisions AS
SELECT * FROM session_decisions
WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
  AND (transaction_to IS NULL);

CREATE VIEW IF NOT EXISTS current_learnings AS
SELECT * FROM session_learnings
WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
  AND (transaction_to IS NULL);

CREATE VIEW IF NOT EXISTS current_errors AS
SELECT * FROM session_errors
WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
  AND (transaction_to IS NULL);

CREATE VIEW IF NOT EXISTS current_workflows AS
SELECT * FROM session_workflows
WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
  AND (transaction_to IS NULL);

-- ============================================================================
-- INDEXES FOR TEMPORAL QUERIES
-- ============================================================================

CREATE INDEX IF NOT EXISTS idx_decisions_valid_from ON session_decisions(valid_from);
CREATE INDEX IF NOT EXISTS idx_decisions_valid_to ON session_decisions(valid_to);
CREATE INDEX IF NOT EXISTS idx_decisions_transaction_from ON session_decisions(transaction_from);
CREATE INDEX IF NOT EXISTS idx_decisions_domain ON session_decisions(domain);

CREATE INDEX IF NOT EXISTS idx_learnings_valid_from ON session_learnings(valid_from);
CREATE INDEX IF NOT EXISTS idx_learnings_valid_to ON session_learnings(valid_to);
CREATE INDEX IF NOT EXISTS idx_learnings_transaction_from ON session_learnings(transaction_from);
CREATE INDEX IF NOT EXISTS idx_learnings_domain ON session_learnings(domain);
CREATE INDEX IF NOT EXISTS idx_learnings_category ON session_learnings(category);

CREATE INDEX IF NOT EXISTS idx_errors_valid_from ON session_errors(valid_from);
CREATE INDEX IF NOT EXISTS idx_errors_valid_to ON session_errors(valid_to);
CREATE INDEX IF NOT EXISTS idx_errors_transaction_from ON session_errors(transaction_from);
CREATE INDEX IF NOT EXISTS idx_errors_domain ON session_errors(domain);

CREATE INDEX IF NOT EXISTS idx_workflows_valid_from ON session_workflows(valid_from);
CREATE INDEX IF NOT EXISTS idx_workflows_valid_to ON session_workflows(valid_to);
CREATE INDEX IF NOT EXISTS idx_workflows_transaction_from ON session_workflows(transaction_from);
CREATE INDEX IF NOT EXISTS idx_workflows_domain ON session_workflows(domain);

-- ============================================================================
-- METADATA UPDATE
-- ============================================================================

INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('cognitive_schema_version', '1.0', strftime('%s', 'now'));

INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('cognitive_features', 'bi-temporal,confidence-decay,thinking-arcs,relationships', strftime('%s', 'now'));
