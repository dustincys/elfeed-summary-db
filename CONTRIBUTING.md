<!-- # Contributing to org-db v3 -->

<!-- Thank you for your interest in contributing to org-db v3! This document provides guidelines for contributing to the project. -->

<!-- ## Getting Started -->

<!-- ### Prerequisites -->

<!-- - **Emacs** (for Elisp development) -->
<!-- - **Python 3.12+** (for server development) -->
<!-- - **uv** (Python package manager) -->
<!-- - **Git** -->

<!-- ### Development Setup -->

<!-- 1. **Clone the repository:** -->
<!--    ```bash -->
<!--    git clone https://github.com/yourusername/org-db-v3.git -->
<!--    cd org-db-v3 -->
<!--    ``` -->

<!-- 2. **Set up Python environment:** -->
<!--    ```bash -->
<!--    cd python -->
<!--    uv sync -->
<!--    ``` -->

<!-- 3. **Load Elisp files:** -->
<!--    ```elisp -->
<!--    (add-to-list 'load-path "/path/to/org-db-v3/elisp/") -->
<!--    (require 'org-db-v3) -->
<!--    ``` -->

<!-- 4. **Start the server:** -->
<!--    ```bash -->
<!--    cd python -->
<!--    uv run uvicorn org_db_server.main:app --host 127.0.0.1 --port 8765 -->
<!--    ``` -->

<!-- ## How to Contribute -->

<!-- ### Reporting Bugs -->

<!-- When reporting bugs, please include: -->

<!-- 1. **Description** - Clear description of the issue -->
<!-- 2. **Steps to reproduce** - Minimal steps to reproduce the behavior -->
<!-- 3. **Expected behavior** - What you expected to happen -->
<!-- 4. **Actual behavior** - What actually happened -->
<!-- 5. **Environment:** -->
<!--    - Emacs version -->
<!--    - Python version -->
<!--    - Operating system -->
<!--    - org-db version -->
<!-- 6. **Logs** - Relevant error messages or logs -->

<!-- ### Suggesting Features -->

<!-- Feature requests are welcome! Please include: -->

<!-- 1. **Use case** - Describe the problem you're trying to solve -->
<!-- 2. **Proposed solution** - Your idea for how to solve it -->
<!-- 3. **Alternatives** - Other approaches you've considered -->
<!-- 4. **Additional context** - Any other relevant information -->

<!-- ### Submitting Pull Requests -->

<!-- 1. **Fork the repository** -->

<!-- 2. **Create a feature branch:** -->
<!--    ```bash -->
<!--    git checkout -b feature/your-feature-name -->
<!--    ``` -->

<!-- 3. **Make your changes:** -->
<!--    - Write clear, concise commit messages -->
<!--    - Follow the coding style (see below) -->
<!--    - Add tests if applicable -->
<!--    - Update documentation as needed -->

<!-- 4. **Test your changes:** -->
<!--    ```bash -->
<!--    # Python tests -->
<!--    cd python -->
<!--    uv run pytest -->

<!--    # Manual testing with examples -->
<!--    # Test indexing, search, etc. -->
<!--    ``` -->

<!-- 5. **Commit your changes:** -->
<!--    ```bash -->
<!--    git add . -->
<!--    git commit -m "feat: add amazing feature" -->
<!--    ``` -->

<!-- 6. **Push to your fork:** -->
<!--    ```bash -->
<!--    git push origin feature/your-feature-name -->
<!--    ``` -->

<!-- 7. **Open a Pull Request:** -->
<!--    - Provide a clear description of the changes -->
<!--    - Reference any related issues -->
<!--    - Include screenshots/examples if relevant -->

<!-- ## Coding Guidelines -->

<!-- ### Python -->

<!-- - **Style:** Follow PEP 8 -->
<!-- - **Formatting:** Use `black` for code formatting -->
<!-- - **Type hints:** Use type hints for function signatures -->
<!-- - **Docstrings:** Use Google-style docstrings -->
<!-- - **Imports:** Group imports (standard library, third-party, local) -->

<!-- Example: -->
<!-- ```python -->
<!-- from typing import List, Optional -->

<!-- def search_headlines( -->
<!--     query: str, -->
<!--     limit: Optional[int] = None -->
<!-- ) -> List[dict]: -->
<!--     """Search for headlines matching query. -->

<!--     Args: -->
<!--         query: Search query string -->
<!--         limit: Maximum number of results (None for unlimited) -->

<!--     Returns: -->
<!--         List of headline dictionaries with title, filename, and position -->
<!--     """ -->
<!--     # Implementation -->
<!--     pass -->
<!-- ``` -->

<!-- ### Emacs Lisp -->

<!-- - **Style:** Follow standard Emacs Lisp conventions -->
<!-- - **Naming:** Use `org-db-v3-` prefix for public functions -->
<!-- - **Documentation:** Include docstrings for all interactive functions -->
<!-- - **Dependencies:** Minimize external dependencies -->

<!-- Example: -->
<!-- ```elisp -->
<!-- (defun org-db-v3-search-headlines () -->
<!--   "Search all headlines and jump to selection. -->
<!-- You can filter candidates dynamically using completing-read." -->
<!--   (interactive) -->
<!--   ;; Implementation -->
<!--   ) -->
<!-- ``` -->

<!-- ### Commit Messages -->

<!-- Follow the [Conventional Commits](https://www.conventionalcommits.org/) specification: -->

<!-- - `feat:` - New feature -->
<!-- - `fix:` - Bug fix -->
<!-- - `docs:` - Documentation changes -->
<!-- - `perf:` - Performance improvements -->
<!-- - `refactor:` - Code refactoring -->
<!-- - `test:` - Adding or updating tests -->
<!-- - `chore:` - Maintenance tasks -->

<!-- Examples: -->
<!-- ``` -->
<!-- feat: add property search functionality -->
<!-- fix: resolve vector search performance issue -->
<!-- docs: update installation instructions -->
<!-- perf: optimize headline search for 100K+ headlines -->
<!-- ``` -->

<!-- ## Project Structure -->

<!-- ``` -->
<!-- org-db-v3/ -->
<!-- ├── docs/              # Documentation -->
<!-- ├── elisp/             # Emacs Lisp code -->
<!-- ├── examples/          # Example org files -->
<!-- ├── python/            # Python server -->
<!-- │   ├── org_db_server/ # Server code -->
<!-- │   ├── scripts/       # Utility scripts -->
<!-- │   └── tests/         # Python tests -->
<!-- └── tests/             # Integration tests -->
<!-- ``` -->

<!-- ## Development Workflow -->

<!-- ### Adding a New Feature -->

<!-- 1. Check existing issues/PRs to avoid duplication -->
<!-- 2. Create an issue to discuss the feature (for large changes) -->
<!-- 3. Create a feature branch -->
<!-- 4. Implement the feature with tests -->
<!-- 5. Update documentation -->
<!-- 6. Submit a pull request -->

<!-- ### Fixing a Bug -->

<!-- 1. Create an issue describing the bug (if not exists) -->
<!-- 2. Create a bugfix branch -->
<!-- 3. Write a test that reproduces the bug -->
<!-- 4. Fix the bug -->
<!-- 5. Verify the test passes -->
<!-- 6. Submit a pull request -->

<!-- ## Testing -->

<!-- ### Python Tests -->

<!-- ```bash -->
<!-- cd python -->

<!-- # Run all tests -->
<!-- uv run pytest -->

<!-- # Run specific test file -->
<!-- uv run pytest tests/test_search.py -v -->

<!-- # Run with coverage -->
<!-- uv run pytest --cov=org_db_server -->
<!-- ``` -->

<!-- ### Manual Testing -->

<!-- 1. Index example files: `examples/` -->
<!-- 2. Test each search type (semantic, fulltext, image, headline, property) -->
<!-- 3. Test with different scope filters (directory, keyword) -->
<!-- 4. Verify performance with large datasets -->

<!-- ## Documentation -->

<!-- When adding features, update: -->

<!-- 1. **Code comments** - Inline documentation -->
<!-- 2. **Docstrings** - Function/class documentation -->
<!-- 3. **README** - User-facing documentation -->
<!-- 4. **docs/** - Detailed guides in appropriate subdirectory -->
<!-- 5. **CHANGELOG** - Record of changes (for releases) -->

<!-- ## Questions? -->

<!-- - Open an issue for questions -->
<!-- - Check existing documentation in `docs/` -->
<!-- - Review closed issues for similar questions -->

<!-- ## License -->

<!-- By contributing, you agree that your contributions will be licensed under the MIT License. -->

<!-- Thank you for contributing to org-db v3! 🎉 -->
