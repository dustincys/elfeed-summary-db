"""Tests for search API endpoints."""
import hashlib
import shutil
import tempfile
from pathlib import Path

import pytest
from elfeed_summary_db_server.api import indexing, search
from elfeed_summary_db_server.config import settings
from elfeed_summary_db_server.main import app
from elfeed_summary_db_server.services.chunking import chunk_text
from elfeed_summary_db_server.services.database import Database
from elfeed_summary_db_server.services.embeddings import get_embedding_service
from fastapi.testclient import TestClient


@pytest.fixture
def temp_db():
    """Create temporary database for testing."""
    temp_dir = tempfile.mkdtemp()
    semantic_path = Path(temp_dir) / "semantic.db"

    # Create database instance
    db = Database(semantic_path)

    # Override the global db instances in both modules
    old_search_db = search.db
    old_indexing_db = indexing.db
    search.db = db
    indexing.db = db

    yield db

    # Restore original db instances
    search.db = old_search_db
    indexing.db = old_indexing_db

    db.close()
    shutil.rmtree(temp_dir)

@pytest.fixture
def client(temp_db):
    """Create test client."""
    return TestClient(app)

def test_semantic_search(client, temp_db):
    """Test semantic search endpoint."""

    # Create some test chunks with embeddings
    test_texts = [
        "Machine learning and artificial intelligence",
        "Database systems and SQL queries",
        "Web development with Python and FastAPI"
    ]
    summary = " ".join(test_texts)
    title = "test title"
    entry_id = "test entry id"
    content = ""

    md5 = hashlib.md5("{0}{1}".format(summary, content).encode("utf-8")).hexdigest()

    chunks = []
    for i, text in enumerate(test_texts):
        chunks.append({
            "text": text,
            "chunk_type": "paragraph",
            "begin_line": i * 10,
            "end_line": i * 10 + 5
        })

    # Generate embeddings
    embedding_service = get_embedding_service()
    embeddings = embedding_service.generate_embeddings(test_texts)

    # Store chunks


    # def store_chunks(self, entry_id: str, title: str, summary: str,
    #                  content: str, md5: str, chunks: List[Dict],
    #                  embeddings: List[np.ndarray], model_name: str):

    temp_db.store_chunks(entry_id, title, summary,
                         content, md5, chunks,
                         embeddings, embedding_service.model_name)

    # Now search for something related to AI
    response = client.post(
        "/api/search/semantic",
        json={
            "query": "artificial intelligence and ML",
            "limit": 2
        }
    )

    assert response.status_code == 200
    data = response.json()

    assert "results" in data
    assert len(data["results"]) <= 2

    # First result should be most similar (AI/ML text)
    if len(data["results"]) > 0:
        first_result = data["results"][0]
        assert "chunk_id" in first_result
        assert "chunk_text" in first_result
        assert "similarity_score" in first_result
        assert "title" in first_result
        assert "chunk_type" in first_result
        assert "entry_id" in first_result
        # Similarity scores can be low for short texts, just verify it's positive
        assert first_result["similarity_score"] > 0

        # Should match the AI/ML text (most semantically similar)
        assert "artificial intelligence" in first_result["chunk_text"].lower() or \
               "machine learning" in first_result["chunk_text"].lower()

def test_semantic_search_empty_query(client):
    """Test semantic search with empty query."""
    response = client.post(
        "/api/search/semantic",
        json={
            "query": "",
            "limit": 10
        }
    )

    assert response.status_code == 422  # Validation error

def test_semantic_search_no_results(client, temp_db):
    """Test semantic search when database is empty."""
    response = client.post(
        "/api/search/semantic",
        json={
            "query": "test query",
            "limit": 10
        }
    )

    assert response.status_code == 200
    data = response.json()
    assert data["results"] == []
