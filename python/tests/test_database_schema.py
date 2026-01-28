"""Tests for database schema completeness."""
from pathlib import Path

import pytest
from elfeed_summary_db_server.services.database import Database


@pytest.fixture
def temp_db(tmp_path):
    """Create a temporary database for testing."""
    db_path = tmp_path / "test.db"
    db = Database(db_path)
    yield db
    db.close()

def test_all_core_tables_exist(temp_db):
    """Test that all core tables are created."""
    cursor = temp_db.semantic_conn.cursor()

    # List of all expected tables
    expected_tables = [
        'entries', 'chunks', 'embeddings'
    ]

    for table_name in expected_tables:
        cursor.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name=?",
            (table_name,)
        )
        result = cursor.fetchone()
        assert result is not None, f"Table {table_name} does not exist"

def test_foreign_keys_enabled(temp_db):
    """Test that foreign keys are enabled."""
    cursor = temp_db.semantic_conn.cursor()
    cursor.execute("PRAGMA foreign_keys")
    result = cursor.fetchone()
    assert result[0] == 1, "Foreign keys should be enabled"

def test_entries_table_structure(temp_db):
    """Test entries table has correct columns."""
    cursor = temp_db.semantic_conn.cursor()
    cursor.execute("PRAGMA table_info(entries)")
    columns = {row[1] for row in cursor.fetchall()}

    expected_columns = {
        'rowid',
        'entry_id',
        'title',
        'summary',
        'content',
        'md5',
        'indexed_at',
    }
    assert expected_columns.issubset(columns), f"Missing columns in headlines table: {expected_columns - columns}"

def test_chunks_and_embeddings_tables(temp_db):
    """Test that chunks and embeddings tables exist with proper structure."""
    cursor = temp_db.semantic_conn.cursor()

    # Check chunks table
    cursor.execute("PRAGMA table_info(chunks)")
    chunk_columns = {row[1] for row in cursor.fetchall()}
    expected_chunk_cols = {
        'rowid',
        'title',
        'entry_id',
        'chunk_text',
        'chunk_type',
    }
    assert expected_chunk_cols.issubset(chunk_columns), f"Missing columns in chunks table: {expected_chunk_cols - chunk_columns}"

    # Check embeddings table
    cursor.execute("PRAGMA table_info(embeddings)")
    embedding_columns = {row[1] for row in cursor.fetchall()}
    expected_embedding_cols = {
        'rowid',
        'chunk_id',
        'embedding_model',
        'embedding_vector',
        'embedding_dim',
        'created_at',
    }
    assert expected_embedding_cols.issubset(embedding_columns), f"Missing columns in embeddings table: {expected_embedding_cols - embedding_columns}"

