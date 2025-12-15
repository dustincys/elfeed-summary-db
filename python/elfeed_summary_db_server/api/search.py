"""Search API endpoints."""
import logging
import textwrap

import numpy as np
from fastapi import APIRouter, HTTPException

logger = logging.getLogger(__name__)

from elfeed_summary_db_server.config import settings
from elfeed_summary_db_server.models.schemas import (SearchResult,
                                                     SemanticSearchRequest,
                                                     SemanticSearchResponse)
from elfeed_summary_db_server.services.database import Database
from elfeed_summary_db_server.services.embeddings import get_embedding_service
from elfeed_summary_db_server.services.reranker import get_reranker_service

router = APIRouter(prefix="/api/search", tags=["search"])

# Global database instance
db = Database(settings.semantic_db_path)


def wrap_snippet(snippet: str, width: int = 80) -> str:
    """Wrap snippet text to specified width while preserving match markers.

    Args:
        snippet: The snippet text with >>> and <<< markers
        width: Maximum line width (default: 80)

    Returns:
        Wrapped snippet text
    """
    # Use textwrap to wrap the text, preserving existing whitespace structure
    wrapped = textwrap.fill(snippet,
                            width=width,
                            break_long_words=False,
                            break_on_hyphens=False,
                            replace_whitespace=True,
                            expand_tabs=False)
    return wrapped


@router.post("/semantic", response_model=SemanticSearchResponse)
async def semantic_search(request: SemanticSearchRequest):
    """Perform semantic search using embeddings with fast vector_top_k()."""
    import time
    start_time = time.perf_counter()
    try:
        # Get embedding service
        t1 = time.perf_counter()
        model_name = request.model or "all-MiniLM-L6-v2"
        embedding_service = get_embedding_service(model_name)
        logger.debug(
            f"Get embedding service: {(time.perf_counter()-t1)*1000:.1f}ms")

        # Generate query embedding
        t1 = time.perf_counter()
        query_embedding = embedding_service.generate_embedding(request.query)
        logger.info(
            f"Generate embedding: {(time.perf_counter()-t1)*1000:.1f}ms")

        # Convert query embedding to bytes for libsql
        query_bytes = query_embedding.astype(np.float32).tobytes()

        # Use semantic database connection
        cursor = db.semantic_conn.cursor()

        # Count total embeddings to decide between exact vs ANN search
        cursor.execute(
            "SELECT COUNT(*) FROM embeddings WHERE embedding_model = ?",
            [model_name])
        total_embeddings = cursor.fetchone()[0]

        # Determine how many candidates to retrieve for reranking
        num_candidates = request.rerank_candidates if request.rerank else request.limit

        # Use exact search for small datasets (<5000) and ANN for large datasets
        # Exact search is more accurate and faster for small-to-medium datasets
        use_exact_search = total_embeddings < 5000

        # Build query with optional filters
        t1 = time.perf_counter()

        if use_exact_search:
            # Exact search: fetch all embeddings and calculate similarities in Python
            logger.debug(
                f"Using exact search for {total_embeddings} embeddings")

            # Query semantic DB (no JOINs with main DB)
            base_query = """
                SELECT
                    e.chunk_id,
                    e.embedding_vector,
                    c.chunk_text,
                    c.chunk_type,
                    c.title
                FROM embeddings e
                JOIN chunks c ON e.chunk_id = c.rowid
            """

            params = [model_name]
            where_clauses = ["e.embedding_model = ?"]

            if request.title_pattern:
                where_clauses.append("c.title LIKE ?")
                params.append(request.title_pattern)

            base_query += " WHERE " + " AND ".join(where_clauses)
            cursor.execute(base_query, params)
            rows = cursor.fetchall()
            logger.info(
                f"Exact search query: {(time.perf_counter()-t1)*1000:.1f}ms")

            if not rows:
                return SemanticSearchResponse(results=[],
                                              query=request.query,
                                              model_used=model_name)

            # Calculate similarities for all chunks
            t1 = time.perf_counter()
            results_with_scores = []
            for row in rows:
                chunk_id = row[0]
                embedding_bytes = row[1]
                chunk_text = row[2]
                chunk_type = row[3]
                title = row[6]

                stored_embedding = np.frombuffer(embedding_bytes,
                                                 dtype=np.float32)
                similarity = float(
                    embedding_service.similarity(query_embedding,
                                                 stored_embedding))

                results_with_scores.append(
                    (similarity,
                     SearchResult(chunk_id=chunk_id,
                                  chunk_text=chunk_text,
                                  similarity_score=similarity,
                                  title=title,
                                  chunk_type=chunk_type)))

            # Sort by similarity (highest first) and take top N
            results_with_scores.sort(key=lambda x: x[0], reverse=True)
            search_results = [
                result for _, result in results_with_scores[:num_candidates]
            ]
            logger.info(
                f"Similarity calculation: {(time.perf_counter()-t1)*1000:.1f}ms"
            )

        else:
            # Use vector_top_k for larger datasets
            if request.title_pattern:
                # Build WHERE clause for filtering (exclude embedding_model - will filter in Python)
                where_parts = []
                where_params = []

                where_parts.append("c.title LIKE ?")
                where_params.append(request.title_pattern)

                # Get more candidates from vector search and filter
                # NOTE: We can't filter by embedding_model in WHERE clause as it breaks vector index usage
                # Instead, we over-fetch and filter in Python
                if where_parts:
                    where_clause = " AND ".join(where_parts)
                    cursor.execute(
                        f"""
                        SELECT
                            vt.id as chunk_id,
                            c.chunk_text,
                            c.chunk_type,
                            c.title,
                            e.embedding_vector,
                            e.embedding_model
                        FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                        JOIN embeddings e ON e.rowid = vt.id
                        JOIN chunks c ON c.rowid = e.chunk_id
                        WHERE {where_clause}
                    """, [query_bytes, num_candidates * 3] + where_params)
                else:
                    # No WHERE filters, just fetch all
                    cursor.execute(
                        """
                        SELECT
                            vt.id as chunk_id,
                            c.chunk_text,
                            c.chunk_type,
                            c.title,
                            e.embedding_vector,
                            e.embedding_model
                        FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                        JOIN embeddings e ON e.rowid = vt.id
                        JOIN chunks c ON c.rowid = e.chunk_id
                    """, [query_bytes, num_candidates * 3])

                # Filter results by embedding_model in Python
                all_rows = cursor.fetchall()
                rows = [row for row in all_rows
                        if row[5] == model_name][:num_candidates]
                logger.info(
                    f"Vector search with filters: {(time.perf_counter()-t1)*1000:.1f}ms"
                )
            else:
                # No filters: use vector_top_k with model filtering in Python
                # NOTE: We can't filter by embedding_model in WHERE clause as it breaks vector index usage
                # Instead, we over-fetch and filter in Python
                cursor.execute("""
                    SELECT
                        vt.id as chunk_id,
                        c.chunk_text,
                        c.chunk_type,
                        c.title,
                        e.embedding_vector,
                        e.embedding_model
                    FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
                    JOIN embeddings e ON e.rowid = vt.id
                    JOIN chunks c ON c.rowid = e.chunk_id
                """, [query_bytes, num_candidates * 3
                      ])  # Get 3x more results to account for filtering

                # Filter results by embedding_model in Python
                all_rows = cursor.fetchall()
                rows = [row for row in all_rows
                        if row[5] == model_name][:num_candidates]
                logger.info(
                    f"Vector search (no filters): {(time.perf_counter()-t1)*1000:.1f}ms"
                )

            # Build results from vector_top_k output (common for both filtered and unfiltered)
            if not rows:
                return SemanticSearchResponse(results=[],
                                              query=request.query,
                                              model_used=model_name)

            # vector_top_k returns results in order, but we need to calculate similarity
            search_results = []
            for row in rows:
                chunk_id = row[0]
                chunk_text = row[1]
                chunk_type = row[2]
                title = row[3]
                embedding_bytes = row[4]

                # Calculate cosine similarity
                stored_embedding = np.frombuffer(embedding_bytes,
                                                 dtype=np.float32)
                similarity = float(
                    embedding_service.similarity(query_embedding,
                                                 stored_embedding))

                search_results.append(
                    SearchResult(chunk_id=chunk_id,
                                 chunk_text=chunk_text,
                                 similarity_score=similarity,
                                 title=title,
                                 chunk_type=chunk_type))

        # Apply cross-encoder reranking if requested
        reranked = False
        if request.rerank and len(search_results) > 0:
            try:
                reranker = get_reranker_service()
                result_dicts = [r.model_dump() for r in search_results]
                reranked_dicts = reranker.rerank(
                    query=request.query,
                    results=result_dicts,
                    text_field="chunk_text",
                    score_field="similarity_score",
                    top_k=request.limit)
                search_results = [SearchResult(**d) for d in reranked_dicts]
                reranked = True
            except Exception as e:
                logger.warning(
                    f"Reranking failed, using original results: {e}")
                search_results = search_results[:request.limit]
        else:
            # Just take top N results
            search_results = search_results[:request.limit]

        total_time = time.perf_counter() - start_time
        logger.info(
            f"Semantic search TOTAL: {total_time*1000:.1f}ms (query='{request.query}', results={len(search_results)})"
        )

        return SemanticSearchResponse(results=search_results,
                                      query=request.query,
                                      model_used=model_name,
                                      reranked=reranked)

    except Exception as e:
        logger.error(f"Semantic search error: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))
