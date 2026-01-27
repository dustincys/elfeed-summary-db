"""Tests for database operations."""
from pathlib import Path

import pytest
from elfeed_summary_db_server.services.database import Database


@pytest.fixture
def temp_db(tmp_path):
    """Create a temporary database for testing."""
    semantic_path = tmp_path / "elfeed-summary-db-semantic.db"
    db = Database(semantic_path)
    yield db
    db.close()

def test_database_initialization(temp_db):
    """Test that database initializes with correct schema."""
    # Check that entries table exists
    cursor = temp_db.main_conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='entries'")
    assert cursor.fetchone() is not None

    # Check that chunks table exists
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='chunks'")
    assert cursor.fetchone() is not None

    # Check that embeddings table exists
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='embeddings'")
    assert cursor.fetchone() is not None
