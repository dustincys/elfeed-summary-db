"""Database schema definitions."""

# SQL schema for elfeed-summary-db
SCHEMA = """
-- Core files table
CREATE TABLE IF NOT EXISTS files (
    rowid INTEGER PRIMARY KEY,
    filename TEXT UNIQUE NOT NULL,
    md5 TEXT NOT NULL,
    last_updated TEXT NOT NULL,
    file_size INTEGER,
    indexed_at TEXT
);

CREATE INDEX IF NOT EXISTS idx_files_filename ON files(filename);
CREATE INDEX IF NOT EXISTS idx_files_md5 ON files(md5);
CREATE INDEX IF NOT EXISTS idx_files_last_updated ON files(last_updated);
CREATE INDEX IF NOT EXISTS idx_files_indexed_at ON files(indexed_at);

-- Headlines table
CREATE TABLE IF NOT EXISTS headlines (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    title TEXT NOT NULL,
    level INTEGER NOT NULL,
    todo_keyword TEXT,
    todo_type TEXT,
    archivedp INTEGER,
    commentedp INTEGER,
    begin INTEGER NOT NULL,
    end INTEGER,
    tags TEXT,
    priority TEXT,
    scheduled TEXT,
    deadline TEXT,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_headlines_filename ON headlines(filename_id);
CREATE INDEX IF NOT EXISTS idx_headlines_filename_begin ON headlines(filename_id, begin);
CREATE INDEX IF NOT EXISTS idx_headlines_todo ON headlines(todo_keyword);
CREATE INDEX IF NOT EXISTS idx_headlines_tags ON headlines(tags);

-- Tags table
CREATE TABLE IF NOT EXISTS tags (
    rowid INTEGER PRIMARY KEY,
    tag TEXT UNIQUE NOT NULL
);

-- Keywords table
CREATE TABLE IF NOT EXISTS keywords (
    rowid INTEGER PRIMARY KEY,
    keyword TEXT UNIQUE NOT NULL
);

-- File keywords table
CREATE TABLE IF NOT EXISTS file_keywords (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    keyword_id INTEGER NOT NULL,
    value TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(keyword_id) REFERENCES keywords(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_file_keywords_filename ON file_keywords(filename_id);

-- Links table
CREATE TABLE IF NOT EXISTS links (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    type TEXT,
    path TEXT,
    raw_link TEXT,
    description TEXT,
    search_option TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_links_filename ON links(filename_id);
CREATE INDEX IF NOT EXISTS idx_links_type ON links(type);

-- Linked files table (for indexing linked documents: PDFs, DOCX, PPTX, etc.)
CREATE TABLE IF NOT EXISTS linked_files (
    rowid INTEGER PRIMARY KEY,
    org_file_id INTEGER NOT NULL,
    org_link_line INTEGER NOT NULL,
    file_path TEXT NOT NULL,
    file_type TEXT NOT NULL,
    file_size INTEGER,
    md5 TEXT NOT NULL,
    last_converted TEXT,
    conversion_status TEXT NOT NULL DEFAULT 'pending',
    conversion_error TEXT,
    indexed_at TEXT,
    FOREIGN KEY(org_file_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_linked_files_org_file ON linked_files(org_file_id);
CREATE INDEX IF NOT EXISTS idx_linked_files_path ON linked_files(file_path);
CREATE INDEX IF NOT EXISTS idx_linked_files_md5 ON linked_files(md5);

-- NOTE: Chunks, embeddings, images, and image_embeddings have been moved
-- to separate databases for performance and size management:
--   - elfeed-summary-db-v3-semantic.db: chunks + embeddings (with vector search)
--   - elfeed-summary-db-v3-images.db: images + image_embeddings (with vector search)

-- Full-text search virtual table
CREATE VIRTUAL TABLE IF NOT EXISTS fts_content USING fts5(
    filename,
    title,
    content,
    tags
);
"""
