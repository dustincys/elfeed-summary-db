#!/bin/bash
set -e

echo "Setting up elfeed summary db development environment..."

# Check Python version
python3 --version | grep -q "Python 3.1[0-9]" || {
    echo "Error: Python 3.10+ required"
    exit 1
}

# Install uv if not available
if ! command -v uv &> /dev/null; then
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
fi

# Install Python dependencies
cd python
uv sync

echo "Setup complete!"
echo "To start the server: cd python && uv run uvicorn org_db_server.main:app --reload --port 8875"
