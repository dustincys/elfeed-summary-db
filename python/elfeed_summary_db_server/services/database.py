"""Database service for elfeed-summary-db with multi-database architecture.

Three separate databases:
1. Main DB: Metadata, headlines, links, properties, FTS5
2. Semantic DB: Text chunks and embeddings with vector search
"""
import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

import libsql
import numpy as np
from elfeed_summary_db_server.models.semantic_schema import SEMANTIC_SCHEMA

logger = logging.getLogger(__name__)


def _row_to_dict(cursor, row):
    """Convert a database row tuple to a dict using cursor description."""
    if row is None:
        return None
    return {desc[0]: value for desc, value in zip(cursor.description, row)}


class Database:
    """Database connections and operations across three databases."""

    def __init__(self, semantic_path: Path):
        """Initialize three database connections and create schemas if needed.

        Args:
            semantic_path: Path to semantic search database (chunks, embeddings)
        """
        self.semantic_path = semantic_path

        # Connect to database
        self.semantic_conn = libsql.connect(str(semantic_path))

        # Enable foreign keys on connection
        self.semantic_conn.execute("PRAGMA foreign_keys = ON")

        # Always initialize semantic
        self._initialize_semantic_schema()

        logger.info(f"Initialized semantic={semantic_path.name}")

    def _initialize_semantic_schema(self):
        """Create semantic database tables (chunks, embeddings, vector index)."""
        cursor = self.semantic_conn.cursor()
        try:
            cursor.executescript(SEMANTIC_SCHEMA)
            self.semantic_conn.commit()
            logger.debug("Semantic schema initialized")
        except ValueError as e:
            if "vector index" in str(
                    e) and "unexpected vector column type" in str(e):
                # Vector indexes already exist, this is fine
                logger.info(
                    "Skipping vector index creation in semantic DB - already exists"
                )
                self.semantic_conn.commit()
            else:
                raise
        except Exception as e:
            logger.error(f"Error initializing semantic schema: {e}")
            raise

    def close(self):
        """Close database connection."""
        if self.semantic_conn:
            self.semantic_conn.close()
        logger.info("Closed all database connections")

    def optimize(self):
        """Optimize all databases by running ANALYZE.

        This improves query performance, especially for vector searches.
        Should be run periodically after bulk indexing operations.
        """
        # Optimize semantic DB
        cursor = self.semantic_conn.cursor()
        cursor.execute("ANALYZE")
        self.semantic_conn.commit()
        logger.info("Semantic database optimized (ANALYZE completed)")

    # -------------------------------------------------------------------------
    # Semantic DB Operations (chunks, embeddings, vector search)
    # -------------------------------------------------------------------------

    def store_chunks(self, filename: str, chunks: List[Dict],
                     embeddings: List[np.ndarray], model_name: str):
        """Store text chunks and their embeddings in semantic DB.

        Args:
            filename: Full path to org file (used instead of file_id)
            chunks: List of chunk dictionaries
            embeddings: List of embedding vectors
            model_name: Name of embedding model used
        """
        cursor = self.semantic_conn.cursor()

        # Delete existing chunks for this file
        cursor.execute("DELETE FROM chunks WHERE filename = ?", (filename, ))

        for chunk_data, embedding in zip(chunks, embeddings):
            # Insert chunk
            cursor.execute(
                """INSERT INTO chunks (title, entry_id, chunk_text, chunk_type)
                   VALUES (?, ?, ?, ?)""",
                (title, None, chunk_data["text"], chunk_data["chunk_type"]))
            chunk_id = cursor.lastrowid

            # Convert embedding to bytes
            embedding_bytes = embedding.astype(np.float32).tobytes()

            # Insert embedding
            cursor.execute(
                """INSERT INTO embeddings (chunk_id, embedding_model, embedding_vector, embedding_dim, created_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (chunk_id, model_name, embedding_bytes, len(embedding),
                 datetime.now().isoformat()))

        self.semantic_conn.commit()
