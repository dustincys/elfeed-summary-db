"""FastAPI server for elfeed-summary-db."""
import logging
import os
import signal
from contextlib import asynccontextmanager

# Disable tokenizer parallelism before any imports that use transformers
# This prevents warnings when forking subprocesses
os.environ['TOKENIZERS_PARALLELISM'] = 'false'

from pathlib import Path

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse

from elfeed_summary_db_server.api import indexing, search, stats
from elfeed_summary_db_server.log_handler import get_memory_handler

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize memory log handler
memory_handler = get_memory_handler()
logger.info("elfeed-summary-db server starting...")


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Lifespan context manager for database cleanup."""
    # Startup
    logger.info("Server starting up...")
    yield
    # Shutdown - close all database connections
    logger.info("Server shutting down, closing database connections...")
    try:
        # Close database connections from all API modules
        from elfeed_summary_db_server.api import indexing, search, stats
        for module in [indexing, search, stats]:
            if hasattr(module, 'db'):
                module.db.close()
                logger.info(f"Closed database connection in {module.__name__}")
    except Exception as e:
        logger.error(f"Error closing database connections: {e}")


app = FastAPI(title="elfeed-summary-db Server",
              version="0.1.0",
              lifespan=lifespan)

# Allow Emacs to connect from localhost
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include routers
app.include_router(indexing.router)
app.include_router(search.router)
app.include_router(stats.router)


@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "ok", "version": "0.1.0"}


@app.post("/api/shutdown")
async def shutdown():
    """Shutdown the server gracefully."""
    # Send SIGTERM to the parent process group
    os.kill(os.getpid(), signal.SIGTERM)
    return {"status": "shutting down"}


@app.get("/api/logs")
async def get_logs(lines: int = 10):
    """Get the last N lines of the server log."""
    logs = memory_handler.get_recent_logs(lines)
    return {"logs": logs, "count": len(logs)}


@app.get("/", response_class=HTMLResponse)
async def root():
    """Root endpoint - serve homepage."""
    # Load HTML template
    template_path = Path(__file__).parent / "templates" / "homepage.html"
    if template_path.exists():
        with open(template_path, "r") as f:
            return f.read()
    else:
        return {"message": "elfeed-summary-db server running"}
