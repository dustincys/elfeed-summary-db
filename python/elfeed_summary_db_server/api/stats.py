"""Statistics API endpoints."""
import logging
import os
from typing import Any, Dict

from elfeed_summary_db_server.config import settings
from elfeed_summary_db_server.services.database import Database
from fastapi import APIRouter, HTTPException

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/stats", tags=["stats"])

# Global database instance
db = Database(settings.semantic_db_path)


@router.get("/", response_model=Dict[str, Any])
async def get_stats():
    """Get database statistics from all three databases."""
    stats = {}
    # Semantic DB stats
    semantic_cursor = db.semantic_conn.cursor()

    # Entry count
    semantic_cursor.execute("SELECT COUNT(*) FROM entries")
    stats["entries_count"] = semantic_cursor.fetchone()[0]

    # Chunk count
    semantic_cursor.execute("SELECT COUNT(*) FROM chunks")
    stats["chunks_count"] = semantic_cursor.fetchone()[0]

    # Embedding count
    semantic_cursor.execute("SELECT COUNT(*) FROM embeddings")
    stats["embeddings_count"] = semantic_cursor.fetchone()[0]

    # Database file sizes and locations

    # Semantic DB
    stats["semantic_db_path"] = str(settings.semantic_db_path)
    if os.path.exists(settings.semantic_db_path):
        stats["semantic_db_size_bytes"] = os.path.getsize(
            settings.semantic_db_path)
        stats["semantic_db_size_mb"] = round(
            stats["semantic_db_size_bytes"] / (1024 * 1024), 2)
    else:
        stats["semantic_db_size_bytes"] = 0
        stats["semantic_db_size_mb"] = 0

    # Total size
    stats["total_db_size_bytes"] = stats["semantic_db_size_bytes"]
    stats["total_db_size_mb"] = round(
        stats["total_db_size_bytes"] / (1024 * 1024), 2)

    # Legacy fields for backward compatibility
    stats["db_path"] = stats["semantic_db_path"]
    stats["db_size_bytes"] = stats["total_db_size_bytes"]
    stats["db_size_mb"] = stats["total_db_size_mb"]

    return stats


@router.get("/entries", response_model=Dict[str, Any])
async def get_entries():
    """Get all entries in the database."""
    cursor = db.semantic_conn.cursor()
    cursor.execute("""
        SELECT title, indexed_at
        FROM entries
        ORDER BY indexed_at DESC
    """)
    entries = [{
        "title": row[0],
        "indexed_at": row[1]
    } for row in cursor.fetchall()]
    return {"entries": entries, "count": len(entries)}


@router.post("/optimize", response_model=Dict[str, Any])
async def optimize_database():
    """Optimize database by running ANALYZE to update index statistics.

    This improves query performance, especially for vector searches.
    Should be run after bulk indexing operations or if queries seem slow.
    """
    try:
        db.optimize()
        return {
            "status": "success",
            "message": "Database optimized successfully"
        }
    except Exception as e:
        logger.error(f"Error optimizing database: {str(e)}")
        raise HTTPException(status_code=500,
                            detail=f"Failed to optimize database: {str(e)}")


@router.delete("/clear-database", response_model=Dict[str, Any])
async def clear_database():
    """Clear all three databases by removing their files.

    WARNING: This is destructive and cannot be undone!
    All indexed data will be permanently deleted.
    """
    global db

    try:
        # Close all database connections first
        db.close()
        logger.info("Closed all database connections")

        # Delete all three database files
        deleted_files = []
        for db_path, db_name in [(settings.semantic_db_path, "semantic")]:
            if db_path.exists():
                os.remove(db_path)
                logger.info(f"Deleted {db_name} database file: {db_path}")
                deleted_files.append(str(db_path))

        # Reinitialize all databases with fresh connections
        db = Database(settings.semantic_db_path)
        logger.info("Reinitialized all empty databases")

        return {
            "status": "success",
            "message": "All databases cleared successfully",
            "deleted_files": deleted_files
        }

    except Exception as e:
        logger.error(f"Error clearing databases: {str(e)}")
        raise HTTPException(status_code=500,
                            detail=f"Failed to clear databases: {str(e)}")
