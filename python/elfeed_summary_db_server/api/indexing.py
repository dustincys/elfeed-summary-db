"""Indexing API endpoints."""
import logging
import os
from typing import Any, Dict

import psutil
from elfeed_summary_db_server.config import settings
from elfeed_summary_db_server.models.schemas import (IndexEntryRequest,
                                                     IndexEntryResponse)
from elfeed_summary_db_server.services.chunking import chunk_text
from elfeed_summary_db_server.services.database import Database
from elfeed_summary_db_server.services.embeddings import get_embedding_service
from fastapi import APIRouter, HTTPException

logger = logging.getLogger(__name__)


def log_memory_usage(context: str = ""):
    """Log current memory usage for debugging."""
    try:
        process = psutil.Process()
        mem_info = process.memory_info()
        mem_mb = mem_info.rss / 1024 / 1024
        msg = f"[MEMORY{(' ' + context) if context else ''}] RSS: {mem_mb:.1f} MB, VMS: {mem_info.vms / 1024 / 1024:.1f} MB"
        logger.info(msg)

        # Also write to dedicated memory log file for debugging
        with open("/tmp/elfeed-summary-db-memory.log", "a") as f:
            from datetime import datetime
            timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            f.write(f"{timestamp} - PID {os.getpid()} - {msg}\n")
    except Exception as e:
        logger.warning(f"Could not get memory info: {e}")


router = APIRouter(prefix="/api", tags=["indexing"])

# Global database instance (will be improved later with dependency injection)
db = Database(settings.semantic_db_path)


@router.get("/entries")
async def get_entries() -> Dict[str, Any]:
    """Get list of all entries in the database."""
    try:
        cursor = db.semantic_conn.cursor()
        cursor.execute(
            "SELECT entry_id, title, indexed_at FROM entries ORDER BY indexed_at DESC"
        )
        rows = cursor.fetchall()

        entries = [{
            "entry_id": row[0],
            "title": row[1],
            "indexed_at": row[2]
        } for row in rows]

        return {"entries": entries}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/entry")
async def delete_entry(entry_id: str) -> Dict[str, Any]:
    """Delete an entry and all its associated data from all databases."""
    try:
        # Delete from main database (metadata)
        cursor = db.semantic_conn.cursor()
        cursor.execute("DELETE FROM entries WHERE rowid = ?", (entry_id, ))
        db.semantic_conn.commit()

        return {
            "status":
            "deleted",
            "entry_id":
            "message":
            f"Successfully deleted {entry_id} and all associated data from all tables"
        }
    except HTTPException:
        raise
    except Exception as e:
        db.semantic_conn.rollback()
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/entry", response_model=IndexEntryResponse)
async def index_entry(request: IndexEntryRequest):
    """Index an elfeed entry."""
    try:
        log_memory_usage("at start of indexing")

        # Debug: Log what we received
        logger.info(f"Indexing entry: {request.title}")
        logger.info(f"  entry_id: {request.entry_id}")
        logger.info(f"  summary: {request.summary}")
        logger.info(f"  content: {request.content}")
        logger.info(f"  md5: {request.md5}")

        cursor = db.semantic_conn.cursor()

        # Delete existing data for this file (we'll re-index everything)
        cursor.execute("DELETE FROM entries WHERE entry_id = ?", (request.entry_id, ))

        # Generate chunks from full elfeed entry content for semantic search
        if request.content:
            log_memory_usage("before chunking elfeed content")

            # Chunk the full summary with proper line tracking
            # Use configurable method and size (default: fixed chunks to reduce bloat)
            all_chunks = chunk_text(
                request.summary + request.content,
                method=settings.elfeed_chunk_method,
                chunk_size=settings.elfeed_chunk_size,
                chunk_overlap=settings.elfeed_chunk_overlap)

            # Generate embeddings
            if all_chunks:
                log_memory_usage(
                    f"before embeddings for {len(all_chunks)} elfeed chunks")
                embedding_service = get_embedding_service()
                chunk_texts = [c["text"] for c in all_chunks]
                embeddings = embedding_service.generate_embeddings(chunk_texts)

                log_memory_usage(
                    f"after embeddings, before storing {len(all_chunks)} elfeed chunks"
                )
                # Store chunks and embeddings (semantic DB uses filename not file_id)
                db.store_chunks(
                    entry_id = request.entry_id,
                    title=request.title,
                    summary=request.summary,
                    content=request.content,
                    md5=request.md5,
                    chunks=all_chunks,
                    embeddings=embeddings,
                    model_name=embedding_service.model_name)
                log_memory_usage("after storing elfeed chunks")

        log_memory_usage("before final commit")
        db.semantic_conn.commit()
        log_memory_usage("after commit, indexing complete")

        # Final garbage collection to free memory for next request
        import gc
        gc.collect()

        return IndexEntryResponse(entry_id=request.entry_id, status="indexed")

    except Exception as e:
        db.semantic_conn.rollback()
        raise HTTPException(status_code=500, detail=str(e))
