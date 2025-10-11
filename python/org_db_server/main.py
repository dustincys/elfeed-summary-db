"""FastAPI server for org-db v3."""
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from org_db_server.api import indexing, search

app = FastAPI(title="org-db Server", version="0.1.0")

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

@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "ok", "version": "0.1.0"}

@app.get("/")
async def root():
    """Root endpoint."""
    return {"message": "org-db v3 server running"}
