"""Pydantic models for API requests/responses."""
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class IndexEntryRequest(BaseModel):
    """Request to index an entry."""
    entry_id: str
    title: str
    summary: str
    md5: str
    content: Optional[str] = None

class IndexEntryResponse(BaseModel):
    """Response from indexing a file."""
    entry_id: str
    status: str

class SemanticSearchRequest(BaseModel):
    """Request for semantic search."""
    query: str = Field(..., min_length=1, description="Search query text")
    limit: int = Field(default=10, ge=1, le=100, description="Maximum number of results")
    model: Optional[str] = Field(default=None, description="Embedding model to use (optional)")
    title_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for title scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")
    rerank: bool = Field(default=False, description="Enable cross-encoder reranking for better accuracy")
    rerank_candidates: int = Field(default=50, ge=10, le=200, description="Number of candidates to retrieve before reranking")

class SearchResult(BaseModel):
    """Single search result."""
    chunk_id: int
    chunk_text: str
    similarity_score: float
    title: str
    chunk_type: str
    entry_id: str
    reranked: bool = False

class SemanticSearchResponse(BaseModel):
    """Response from semantic search."""
    results: List[SearchResult]
    query: str
    model_used: str
    reranked: bool = False  # True if results were reranked
