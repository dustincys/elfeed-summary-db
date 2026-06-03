# elfeed-summary-db

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Emacs](https://img.shields.io/badge/Emacs-28.1%2B-blue.svg)](https://www.gnu.org/software/emacs/)
[![Python](https://img.shields.io/badge/Python-3.10%2B-blue.svg)](https://www.python.org/)

**English** | [中文](readme_zh.md)

**Semantic search for your Elfeed RSS summaries — powered by vector embeddings.**

elfeed-summary-db indexes AI-generated summaries of your [Elfeed](https://github.com/skeeto/elfeed) RSS entries into a vector database, enabling blazing-fast semantic search. Find articles by *meaning*, not just keywords. Includes LLM tool integration via [gptel](https://github.com/karthink/gptel) so your AI assistant can search your reading history.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                        Emacs                            │
│  ┌──────────┐  ┌──────────┐  ┌───────────────────────┐ │
│  │  elfeed  │──│  parse   │──│  HTTP Client (plz)    │ │
│  │ entries  │  │  → JSON  │  │  async index + search │ │
│  └──────────┘  └──────────┘  └───────────┬───────────┘ │
│       │                                  │              │
│  ┌────┴────────┐              ┌──────────┴──────────┐  │
│  │ auto-index  │              │  Transient UI Menu  │  │
│  │ on save     │              │  Semantic Search    │  │
│  └─────────────┘              │  Ivy Integration    │  │
│                               │  gptel MCP Tools    │  │
│                               └─────────────────────┘  │
└─────────────────────────────────────┬───────────────────┘
                                      │ HTTP (localhost:8875)
┌─────────────────────────────────────┴───────────────────┐
│                 Python FastAPI Server                    │
│  ┌──────────┐  ┌────────────┐  ┌────────────────────┐  │
│  │  /health │  │ /api/entry │  │ /api/search/       │  │
│  │  /logs   │  │  (index)   │  │   semantic         │  │
│  └──────────┘  └────────────┘  └────────┬───────────┘  │
│                                         │               │
│  ┌──────────────────────────────────────┴──────────┐   │
│  │  Embedding Service (BGE-M3, CLIP)               │   │
│  │  Cross-encoder Reranker                         │   │
│  │  Chunking Service                               │   │
│  └─────────────────────┬───────────────────────────┘   │
│                        │                                │
│  ┌─────────────────────┴───────────────────────────┐   │
│  │  libsql (SQLite + Vector Search)                 │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

## Features

### 🔍 Semantic Search
- **Vector embeddings** using BGE-M3 model — search by meaning, not keywords
- **Exact search** for small collections (<5000 chunks), **ANN vector search** for larger
- **Cross-encoder reranking** for improved relevance (optional)
- **Ivy dynamic search** — results update as you type
- **Title scoping** — restrict searches to specific feeds or patterns

### 🤖 LLM Integration (gptel)
- Expose search as **MCP-style tools** for gptel
- `elfeed_semantic_search` — vector search from within LLM conversations
- `elfeed_fuzzy_summary_search` — local fuzzy search fallback
- Clickable `[[ELFEED:ID]]` links rendered as buttons in gptel buffers

### ⚡ Smart Indexing
- **Auto-index** on summary save via advice
- **Queue-based async indexing** — non-blocking, keeps Emacs responsive
- **Progress tracking** with status reports
- **Resume/cancel** stuck operations
- **Reindex database** with stale-entry cleanup
- **Configurable timeouts** and concurrency

### 🖥️ Transient UI
- Accessible via `H-v` or `M-x elfeed-summary-db`
- All commands organized in a discoverable menu
- Scope controls, server management, indexing, and search in one place

### 🌐 Web Interface
- Built-in homepage served by the FastAPI server
- API documentation auto-generated (FastAPI Swagger UI)

## Installation

### Prerequisites

- **Emacs** 28.1+
- **Python** 3.10+
- **[uv](https://github.com/astral-sh/uv)** — fast Python package manager

### 1. Python Backend

```bash
cd python
uv sync
```

This installs: FastAPI, Uvicorn, sentence-transformers, CLIP, PyTorch, libsql, and more.

### 2. Emacs Package

```elisp
;; Clone the repo
;; git clone https://github.com/dustincys/elfeed-summary-db ~/.emacs.d/private/elfeed-summary-db

;; Add to load-path and require
(add-to-list 'load-path "~/.emacs.d/private/elfeed-summary-db/elisp")
(require 'elfeed-summary-db)
```

### 3. Dependencies

The package depends on:
- `elfeed` — RSS reader
- `transient` — menu UI (Emacs 28+ built-in, or install via ELPA)
- `plz` — HTTP client (install via ELPA)
- `compat` — compatibility library
- `gptel` (optional) — for LLM tool integration
- `ivy` (optional) — for dynamic search-as-you-type
- `flx` (optional) — for fuzzy search in gptel tools

### 4. Configuration

```elisp
(use-package elfeed-summary-db
  :load-path "~/.emacs.d/private/elfeed-summary-db/elisp"
  :custom
  ;; Server settings
  (elfeed-summary-db-server-host "127.0.0.1")
  (elfeed-summary-db-server-port 8875)

  ;; Auto-start server when needed
  (elfeed-summary-db-auto-start-server t)

  ;; Auto-enable indexing on summary save
  (elfeed-summary-db-auto-enable t)

  ;; Search settings
  (elfeed-summary-db-search-default-limit 10)
  (elfeed-summary-db-search-use-reranking nil)  ; Enable for better accuracy
  (elfeed-summary-db-search-rerank-candidates 50)

  ;; Indexing settings
  (elfeed-summary-db-index-delay 0.5)   ; Delay between entries
  (elfeed-summary-db-index-timeout 240) ; Timeout per entry

  :bind
  ("H-v" . elfeed-summary-db-menu))
```

## Usage

### Starting the Server

The server auto-starts when needed (if `elfeed-summary-db-auto-start-server` is `t`). You can also start it manually:

- `M-x elfeed-summary-db-start-server` — Start the Python backend
- `M-x elfeed-summary-db-stop-server` — Stop the backend
- `M-x elfeed-summary-db-restart-server` — Restart
- `M-x elfeed-summary-db-server-status` — Check if running

### Indexing Entries

Entries are automatically indexed when their AI summary is saved (if `elfeed-summary-db-auto-enable` is `t`).

Manual indexing commands:
- `M-x elfeed-summary-db-index-elfeed` — Index **all** entries with summaries
- `M-x elfeed-summary-db-reindex-database` — Reindex existing entries, clean up stale ones
- `M-x elfeed-summary-db-index-current-entry` — Index the current entry
- `M-x elfeed-summary-db-indexing-status` — Check progress
- `M-x elfeed-summary-db-cancel-indexing` — Cancel current operation
- `M-x elfeed-summary-db-resume-indexing` — Resume stalled indexing
- `M-x elfeed-summary-db-show-failed-entries` — List entries that failed to index

### Semantic Search

| Command | Description |
|---------|-------------|
| `M-x elfeed-summary-db-semantic-search` | Search by meaning (completing-read) |
| `M-x elfeed-summary-db-semantic-search-ivy` | Dynamic search-as-you-type (Ivy) |
| `M-x elfeed-summary-db-search-at-point` | Search using text at point or region |

In the search results:
- Results show **similarity score**, **context snippet**, and **feed title**
- Press `RET` to open the selected entry in Elfeed
- In Ivy mode: `o` opens the entry, `c` copies the full summary to kill ring

### The Transient Menu

Press `H-v` or `M-x elfeed-summary-db` to open the menu:

```
elfeed-summary db [Scope: All entries]
Search and manage your elfeed entries.
┌─ Scope ───────────────────────────────────────┐
│ -a  All entries                               │
│ -t  Title pattern                             │
├─ Search ──────────────────────────────────────┤
│  v  Semantic search (vector embeddings)       │
├─ Management ──────────────────────────────────┤
│  u  Index current entry                       │
│  U  Index all entries                         │
│  r  Reindex database                          │
│  S  Server status                             │
│  R  Restart server                            │
│  L  View server logs                          │
│  W  Open web interface                        │
│  X  Clear database (destructive!)             │
├─ Actions ─────────────────────────────────────┤
│  q  Quit                                      │
└───────────────────────────────────────────────┘
```

### gptel Integration

Register the tools so your LLM can search your reading history:

```elisp
;; Once gptel is loaded:
M-x elfeed-summary-db-gptel-register-tools
```

Then in any gptel chat, the LLM can call:
- **`elfeed_semantic_search`** — "Find articles about deep learning optimizers"
- **`elfeed_fuzzy_summary_search`** — "Search for mentions of BPE tokenization"

Results include clickable buttons that open the entry in Elfeed.

To remove the tools:
```
M-x elfeed-summary-db-gptel-unregister-tools
```

### Web Interface

```elisp
M-x elfeed-summary-db-open-web-interface
```

Opens `http://127.0.0.1:8875` in your browser. The homepage shows server info and API documentation links.

## API Endpoints

The Python server exposes these endpoints:

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/health` | Health check |
| `GET` | `/` | Web homepage |
| `GET` | `/api/logs?lines=N` | Recent server logs |
| `POST` | `/api/entry` | Index an entry |
| `DELETE` | `/api/entry?entry-id=ID` | Delete an entry |
| `GET` | `/api/entries` | List all indexed entry IDs |
| `POST` | `/api/search/semantic` | Semantic vector search |
| `GET` | `/api/stats` | Database statistics |
| `DELETE` | `/api/stats/clear-database` | Clear entire database |
| `POST` | `/api/shutdown` | Graceful server shutdown |

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ELFEED_SUMMARY_DB_HOST` | `127.0.0.1` | Server host |
| `ELFEED_SUMMARY_DB_PORT` | `8875` | Server port |
| `ELFEED_SUMMARY_DB_SEMANTIC_DB_PATH` | `~/elfeed-summary-db/elfeed-summary-db-semantic.db` | SQLite DB path |

## How It Works

### Data Flow

1. **Elfeed** fetches RSS articles, and your workflow generates AI summaries (stored as `:summary` metadata)
2. **elfeed-summary-db** hooks into the save function — when a summary is saved, it parses the entry to JSON and sends it to the Python server
3. The **Python server** chunks the text, generates embeddings via BGE-M3, and stores them in a libsql database with vector index
4. On **search**, the query is embedded and compared against stored vectors using cosine similarity
5. Results are returned to Emacs and displayed in a completing-read interface (or dynamically in Ivy)

### Embedding Model

- **Default**: BGE-M3 (1024-dimensional embeddings)
- Multilingual, high-quality embeddings
- Exact cosine similarity for small collections; libsql `vector_top_k` ANN for large collections
- Optional cross-encoder reranking for improved relevance

### Database

- Uses [libsql](https://github.com/tursodatabase/libsql) — SQLite with native vector search support
- Two main tables: `chunks` (text chunks with metadata) and `embeddings` (vector embeddings)
- Vector index for fast approximate nearest neighbor search on large collections

## Project Structure

```
elfeed-summary-db/
├── README.md
├── readme.org                        # Original Org-mode README
├── elisp/
│   ├── elfeed-summary-db.el          # Main package, hooks, configuration
│   ├── elfeed-summary-db-parse.el    # Elfeed entry → JSON parsing
│   ├── elfeed-summary-db-client.el   # Async HTTP client, queue management
│   ├── elfeed-summary-db-server.el   # Server lifecycle (start/stop/health)
│   ├── elfeed-summary-db-search.el   # Semantic search UI + Ivy integration
│   ├── elfeed-summary-db-gptel-tools.el  # gptel LLM tool integration
│   └── elfeed-summary-db-ui.el       # Transient menu interface
├── python/
│   ├── pyproject.toml                # Python dependencies
│   ├── elfeed_summary_db_server/
│   │   ├── main.py                   # FastAPI app entrypoint
│   │   ├── config.py                 # Pydantic settings
│   │   ├── api/
│   │   │   ├── indexing.py           # Entry indexing endpoints
│   │   │   ├── search.py             # Semantic search endpoints
│   │   │   └── stats.py             # Statistics & management endpoints
│   │   ├── models/
│   │   │   ├── schemas.py            # Pydantic request/response models
│   │   │   └── semantic_schema.py    # SQL schema for vector DB
│   │   ├── services/
│   │   │   ├── database.py           # libsql database connections
│   │   │   ├── embeddings.py         # BGE-M3 embedding service
│   │   │   ├── chunking.py           # Text chunking strategies
│   │   │   ├── reranker.py           # Cross-encoder reranking
│   │   │   └── clip_service.py       # CLIP image embeddings
│   │   ├── templates/
│   │   └── log_handler.py            # In-memory log buffer
│   └── scripts/                      # Benchmarks and profiling
├── scripts/
│   └── setup.sh                      # Development environment setup
└── tests/
    └── elfeed-summary-db-search-test.el  # ERT tests
```

## Development

### Running Tests

**Emacs Lisp tests:**
```bash
emacs -batch -l ert \
  -l elisp/elfeed-summary-db.el \
  -l tests/elfeed-summary-db-search-test.el \
  -f ert-run-tests-batch-and-exit
```

**Python tests:**
```bash
cd python
uv run pytest
```

### Manual Server Start

```bash
cd python
uv run uvicorn elfeed_summary_db_server.main:app --reload --host 127.0.0.1 --port 8875
```

## License

MIT License.

## Author

Yanshuo Chu — [GitHub](https://github.com/dustincys)

---

<p align="center">
  <sub>Built with ❤️ for the Emacs community</sub>
</p>
