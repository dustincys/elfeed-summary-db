"""Pydantic models for API requests/responses."""
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class HeadlineData(BaseModel):
    """Headline data from Emacs."""
    title: str
    level: int
    todo_keyword: Optional[str] = None
    todo_type: Optional[str] = None
    archivedp: Optional[bool] = None
    commentedp: Optional[bool] = None
    begin: int
    end: Optional[int] = None
    tags: Optional[str] = None
    priority: Optional[str] = None
    scheduled: Optional[str] = None
    deadline: Optional[str] = None
    properties: Optional[Dict[str, str]] = None


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

class FulltextSearchRequest(BaseModel):
    """Request for full-text search."""
    query: str = Field(..., min_length=1, description="Search query (FTS5 syntax)")
    limit: int = Field(default=10, ge=1, le=100, description="Maximum number of results")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")

class FulltextSearchResult(BaseModel):
    """Single fulltext search result."""
    filename: str
    title: str
    content: str
    tags: str
    snippet: str
    rank: float

class FulltextSearchResponse(BaseModel):
    """Response from fulltext search."""
    results: List[FulltextSearchResult]
    query: str

class ImageSearchRequest(BaseModel):
    """Request for image search by text description."""
    query: str = Field(..., min_length=1, description="Text description of image")
    limit: int = Field(default=10, ge=1, le=100, description="Maximum number of results")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")

class ImageSearchResult(BaseModel):
    """Single image search result."""
    image_path: str
    similarity_score: float
    filename: str

class ImageSearchResponse(BaseModel):
    """Response from image search."""
    results: List[ImageSearchResult]
    query: str
    model_used: str

class HeadlineSearchRequest(BaseModel):
    """Request for headline search."""
    query: str = Field(default="", description="Search query (empty for all headlines)")
    limit: Optional[int] = Field(default=None, ge=1, le=200000, description="Maximum number of results (None = unlimited)")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")
    keyword: Optional[str] = Field(default=None, description="Keyword/tag filter")
    sort_by: str = Field(default="last_updated", description="Sort order: 'filename' (alphabetical), 'last_updated' (most recent first), 'indexed_at' (most recently indexed first)")

class HeadlineSearchResult(BaseModel):
    """Single headline search result."""
    title: str
    filename: str
    begin: int
    level: int
    tags: Optional[str] = None
    todo_keyword: Optional[str] = None

class HeadlineSearchResponse(BaseModel):
    """Response from headline search."""
    results: List[List]  # Simple list of [title, filename, begin] for performance
    query: str

class PropertySearchRequest(BaseModel):
    """Request for property search."""
    property: str = Field(..., min_length=1, description="Property name to search for")
    value: Optional[str] = Field(default=None, description="Optional property value to match")
    limit: int = Field(default=20, ge=1, le=100, description="Maximum number of results")
    filename_pattern: Optional[str] = Field(default=None, description="SQL LIKE pattern for directory/project scope")

class PropertySearchResult(BaseModel):
    """Single property search result."""
    headline_title: str
    filename: str
    begin: int
    property: str
    value: str

class PropertySearchResponse(BaseModel):
    """Response from property search."""
    results: List[PropertySearchResult]
    property: str
