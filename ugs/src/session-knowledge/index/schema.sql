-- Session Knowledge System - Database Schema
-- Epic: agentic-primer-9ad
-- Phase 1: agentic-primer-9ad.1

-- Sessions table (main index)
CREATE TABLE IF NOT EXISTS sessions (
  id TEXT PRIMARY KEY,
  created INTEGER NOT NULL,
  modified INTEGER NOT NULL,
  summary TEXT,
  summary_embedding BLOB, -- Phase 3: Vector embedding for semantic search
  message_count INTEGER DEFAULT 0,
  agent_count INTEGER DEFAULT 0,
  cost REAL DEFAULT 0.0,
  duration INTEGER DEFAULT 0,
  git_branch TEXT,
  project_path TEXT,
  content_hash TEXT,
  indexed_at INTEGER
);

-- Indexes for fast queries
CREATE INDEX IF NOT EXISTS idx_sessions_created ON sessions(created);
CREATE INDEX IF NOT EXISTS idx_sessions_modified ON sessions(modified);
CREATE INDEX IF NOT EXISTS idx_sessions_cost ON sessions(cost);
CREATE INDEX IF NOT EXISTS idx_sessions_project ON sessions(project_path);
CREATE INDEX IF NOT EXISTS idx_sessions_hash ON sessions(content_hash);

-- Session files tracking
CREATE TABLE IF NOT EXISTS session_files (
  session_id TEXT NOT NULL,
  file_path TEXT NOT NULL,
  operation TEXT CHECK(operation IN ('read', 'write', 'edit')),
  timestamp INTEGER,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_session_files_path ON session_files(file_path);
CREATE INDEX IF NOT EXISTS idx_session_files_session ON session_files(session_id);

-- Session tools tracking
CREATE TABLE IF NOT EXISTS session_tools (
  session_id TEXT NOT NULL,
  tool_name TEXT NOT NULL,
  count INTEGER DEFAULT 1,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,
  UNIQUE(session_id, tool_name)
);

CREATE INDEX IF NOT EXISTS idx_session_tools_name ON session_tools(tool_name);
CREATE INDEX IF NOT EXISTS idx_session_tools_session ON session_tools(session_id);

-- Session agents tracking
CREATE TABLE IF NOT EXISTS session_agents (
  session_id TEXT NOT NULL,
  agent_id TEXT NOT NULL,
  agent_type TEXT,
  task TEXT,
  outcome TEXT CHECK(outcome IN ('success', 'failure', 'running', 'unknown')),
  spawned_at INTEGER,
  completed_at INTEGER,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_session_agents_type ON session_agents(agent_type);
CREATE INDEX IF NOT EXISTS idx_session_agents_session ON session_agents(session_id);

-- Session decisions (Tier 3 - incremental processing)
CREATE TABLE IF NOT EXISTS session_decisions (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  decision TEXT NOT NULL,
  reasoning TEXT,
  alternatives TEXT, -- JSON array
  context TEXT,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_decisions_session ON session_decisions(session_id);
CREATE INDEX IF NOT EXISTS idx_decisions_timestamp ON session_decisions(timestamp);

-- Session learnings (Tier 3 - incremental processing)
CREATE TABLE IF NOT EXISTS session_learnings (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  learning TEXT NOT NULL,
  context TEXT,
  actionable TEXT,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_learnings_session ON session_learnings(session_id);
CREATE INDEX IF NOT EXISTS idx_learnings_timestamp ON session_learnings(timestamp);

-- Session errors (Tier 3 - incremental processing)
CREATE TABLE IF NOT EXISTS session_errors (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  tool_name TEXT,
  error_type TEXT,
  error_message TEXT,
  root_cause TEXT,
  suggested_fix TEXT,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_errors_session ON session_errors(session_id);
CREATE INDEX IF NOT EXISTS idx_errors_type ON session_errors(error_type);

-- Full-text search on summaries
CREATE VIRTUAL TABLE IF NOT EXISTS sessions_fts USING fts5(
  session_id UNINDEXED,
  summary,
  content='sessions',
  content_rowid='rowid'
);

-- Trigger to keep FTS in sync
CREATE TRIGGER IF NOT EXISTS sessions_fts_insert AFTER INSERT ON sessions BEGIN
  INSERT INTO sessions_fts(rowid, session_id, summary)
  VALUES (new.rowid, new.id, new.summary);
END;

CREATE TRIGGER IF NOT EXISTS sessions_fts_update AFTER UPDATE ON sessions BEGIN
  UPDATE sessions_fts SET summary = new.summary WHERE session_id = new.id;
END;

CREATE TRIGGER IF NOT EXISTS sessions_fts_delete AFTER DELETE ON sessions BEGIN
  DELETE FROM sessions_fts WHERE session_id = old.id;
END;

-- Metadata table
CREATE TABLE IF NOT EXISTS index_metadata (
  key TEXT PRIMARY KEY,
  value TEXT,
  updated_at INTEGER
);

-- Store last index time
INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('last_full_index', '0', strftime('%s', 'now'));

-- Store schema version
INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
VALUES ('schema_version', '2', strftime('%s', 'now'));

-- Phase 3: Message embeddings for semantic search
CREATE TABLE IF NOT EXISTS message_embeddings (
  message_id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  content TEXT NOT NULL,
  embedding BLOB NOT NULL,
  timestamp INTEGER NOT NULL,
  FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_message_embeddings_session ON message_embeddings(session_id);
CREATE INDEX IF NOT EXISTS idx_message_embeddings_timestamp ON message_embeddings(timestamp);
