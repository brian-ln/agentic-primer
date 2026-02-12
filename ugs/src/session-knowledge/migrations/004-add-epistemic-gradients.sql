-- Migration 004: Add Epistemic Gradients Support
-- Epic: Knowledge Architecture Phase 1 MVP
-- Date: 2026-02-03
--
-- Adds epistemic gradient fields to decisions, learnings, and errors tables
-- to support confidence tracking and knowledge maturation.

-- Add epistemic fields to session_decisions
-- Note: confidence and evidence may already exist from cognitive features migration
ALTER TABLE session_decisions ADD COLUMN epistemic_level TEXT;
ALTER TABLE session_decisions ADD COLUMN last_validated INTEGER;

-- Add epistemic fields to session_learnings
ALTER TABLE session_learnings ADD COLUMN epistemic_level TEXT;
ALTER TABLE session_learnings ADD COLUMN last_validated INTEGER;

-- Add epistemic fields to session_errors
ALTER TABLE session_errors ADD COLUMN epistemic_level TEXT;
ALTER TABLE session_errors ADD COLUMN last_validated INTEGER;

-- Create indexes for efficient queries by epistemic level
CREATE INDEX IF NOT EXISTS idx_decisions_epistemic ON session_decisions(epistemic_level);
CREATE INDEX IF NOT EXISTS idx_decisions_confidence ON session_decisions(confidence);

CREATE INDEX IF NOT EXISTS idx_learnings_epistemic ON session_learnings(epistemic_level);
CREATE INDEX IF NOT EXISTS idx_learnings_confidence ON session_learnings(confidence);

CREATE INDEX IF NOT EXISTS idx_errors_epistemic ON session_errors(epistemic_level);
CREATE INDEX IF NOT EXISTS idx_errors_confidence ON session_errors(confidence);

-- Update schema version
INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('schema_version', '4-epistemic-gradients', strftime('%s', 'now'));

-- Add migration record
INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('migration_004_applied', datetime('now'), strftime('%s', 'now'));
