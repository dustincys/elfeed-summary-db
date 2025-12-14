"""Schema for semantic search database (chunks + embeddings)."""

# Semantic search database schema - only chunks and embeddings with vector search
SEMANTIC_SCHEMA = """
-- Chunks table for elfeed entries
CREATE TABLE IF NOT EXISTS entries (
    rowid INTEGER PRIMARY KEY,
    title TEXT NOT NULL,
    entry_id TEXT NOT NULL,
    indexed_at TEXT
);

CREATE INDEX IF NOT EXISTS idx_entries_title ON entries(title);

-- Chunks table for semantic search
CREATE TABLE IF NOT EXISTS chunks (
    rowid INTEGER PRIMARY KEY,
    title TEXT NOT NULL,
    entry_id TEXT NOT NULL,
    chunk_text TEXT NOT NULL,
    chunk_type TEXT
);

CREATE INDEX IF NOT EXISTS idx_chunks_title ON chunks(title);

-- Embeddings table with libsql vector search
-- Note: F32_BLOB(384) is required for libsql vector search (all-MiniLM-L6-v2 model)
CREATE TABLE IF NOT EXISTS embeddings (
    rowid INTEGER PRIMARY KEY,
    chunk_id INTEGER NOT NULL,
    embedding_model TEXT NOT NULL,
    embedding_vector F32_BLOB(384) NOT NULL,
    embedding_dim INTEGER NOT NULL,
    created_at TEXT,
    FOREIGN KEY(chunk_id) REFERENCES chunks(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_embeddings_chunk ON embeddings(chunk_id);
CREATE INDEX IF NOT EXISTS idx_embeddings_model ON embeddings(embedding_model);

-- Vector index for fast semantic search using libsql_vector_idx
CREATE INDEX IF NOT EXISTS idx_embeddings_vector ON embeddings(libsql_vector_idx(embedding_vector));
"""
