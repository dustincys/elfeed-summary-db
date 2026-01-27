# Utility Scripts

This directory contains utility scripts for development, benchmarking, profiling, and database migrations.

## Benchmarks

Scripts for measuring search performance:

- **`benchmarks/benchmark_search.py`** - Benchmark various search types (semantic, fulltext, image, headline)
- **`benchmarks/benchmark_semantic_search.py`** - Detailed semantic search benchmarking

Usage:
```bash
cd python
uv run python scripts/benchmarks/benchmark_search.py
```

## Profiling

Scripts for performance analysis and debugging:

- **`profiling/debug_search_performance.py`** - Debug search performance issues
- **`profiling/profile_search.py`** - Profile search operations

Usage:
```bash
cd python
uv run python scripts/profiling/debug_search_performance.py
```
## Development

These scripts are not part of the main package but are useful for:
- Performance testing
- Debugging issues
- Database maintenance
- Schema migrations
- Understanding system behavior
