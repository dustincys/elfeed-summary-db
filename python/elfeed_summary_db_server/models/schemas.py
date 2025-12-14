"""Pydantic models for API requests/responses."""
from typing import List, Optional

from pydantic import BaseModel, Field


class SemanticSearchRequest(BaseModel):
    """Request for semantic search."""
    query: str = Field(..., min_length=1, description="Search query text")
    limit: int = Field(default=10,
                       ge=1,
                       le=100,
                       description="Maximum number of results")
    model: Optional[str] = Field(
        default=None, description="Embedding model to use (optional)")
    filename_pattern: Optional[str] = Field(
        default=None,
        description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None,
                                   description="Keyword/tag filter")
    rerank: bool = Field(
        default=False,
        description="Enable cross-encoder reranking for better accuracy")
    rerank_candidates: int = Field(
        default=50,
        ge=10,
        le=200,
        description="Number of candidates to retrieve before reranking")


class SearchResult(BaseModel):
    """Single search result."""
    chunk_id: int
    chunk_text: str
    similarity_score: float
    filename: str
    chunk_type: str
    begin_line: int
    end_line: int
    reranked: bool = False
    linked_file_path: Optional[str] = None
    linked_file_type: Optional[str] = None


class SemanticSearchResponse(BaseModel):
    """Response from semantic search."""
    results: List[SearchResult]
    query: str
    model_used: str
    reranked: bool = False  # True if results were reranked
