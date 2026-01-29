;;; elfeed-summary-db-search.el --- Semantic search for elfeed summary -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: elfeed-summary-db
;; Keywords: org-mode, database, search

;;; Commentary:

;; Semantic search functionality for elfeed summary.
;; Uses vector embeddings to find semantically similar content.

;;; Code:

(require 'json)
(require 'elfeed)
(require 'url)

;; Declare url-request variables as special for lexical binding compatibility
(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

;; Forward declarations
(declare-function elfeed-summary-db-server-url "elfeed-summary-db")
(declare-function elfeed-summary-db-ensure-server "elfeed-summary-db")
(declare-function elfeed-summary-db--scope-to-params "elfeed-summary-db-ui")

;; External package functions
(declare-function plz "plz")
(declare-function plz-error-message "plz")
(declare-function ivy-read "ivy")
(declare-function ivy-configure "ivy")

;; Require plz only when available (not in tests)
(when (require 'plz nil t)
  (defvar plz-available t))

(unless (boundp 'elfeed-summary-db-server-host)
  (defvar elfeed-summary-db-server-host "127.0.0.1"))

(unless (boundp 'elfeed-summary-db-server-port)
  (defvar elfeed-summary-db-server-port 8875))

(unless (fboundp 'elfeed-summary-db-server-url)
  (defun elfeed-summary-db-server-url ()
    "Return the base URL for the org-db server."
    (format "http://%s:%d" elfeed-summary-db-server-host elfeed-summary-db-server-port)))

(unless (fboundp 'elfeed-summary-db-ensure-server)
  (defun elfeed-summary-db-ensure-server ()
    "Ensure server is running (stub for testing)."
    nil))

(defcustom elfeed-summary-db-search-default-limit 10
  "Default number of search results to retrieve."
  :type 'integer
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-search-use-reranking nil
  "Enable cross-encoder reranking for more accurate semantic search.
When enabled, retrieves more candidates and reranks them using a
cross-encoder model for better relevance. Slower but more accurate."
  :type 'boolean
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-search-rerank-candidates 50
  "Number of candidates to retrieve before reranking.
Only used when `elfeed-summary-db-search-use-reranking' is non-nil."
  :type 'integer
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-entry-sort-order "last_updated"
  "Default sort order for entry search results.
- \"filename\": Sort alphabetically by filename
- \"last_updated\": Sort by most recently updated files first (default)
- \"indexed_at\": Sort by most recently indexed files first"
  :type '(choice (const :tag "By title (alphabetical)" "title")
                 (const :tag "By last updated (most recent first)" "last_updated")
                 (const :tag "By last indexed (most recent first)" "indexed_at"))
  :group 'elfeed-summary-db)

;;;###autoload
(defun elfeed-summary-db-semantic-search (query &optional limit)
  "Perform semantic search for QUERY.
Retrieve up to LIMIT results (default `elfeed-summary-db-search-default-limit')."
  (interactive (list (read-string "Search query: ")
                     (when current-prefix-arg
                       (read-number "Limit: " elfeed-summary-db-search-default-limit))))

  (elfeed-summary-db-ensure-server)

  (let* ((limit (or limit elfeed-summary-db-search-default-limit))
         (scope-params (when (fboundp 'elfeed-summary-db--scope-to-params)
                         (elfeed-summary-db--scope-to-params)))
         (request-body (append `((query . ,query)
                                 (limit . ,limit)
                                 (rerank . ,(if elfeed-summary-db-search-use-reranking t :json-false))
                                 (rerank_candidates . ,elfeed-summary-db-search-rerank-candidates))
                               (when scope-params
                                 (list (cons 'title_pattern (plist-get scope-params :title_pattern)))))))
    (plz 'post (concat (elfeed-summary-db-server-url) "/api/search/semantic")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as #'json-read
      :then (lambda (response)
              (elfeed-summary-db-display-search-results query response))
      :else (lambda (error)
              (message "Search error: %s" (plz-error-message error))))))

(defun elfeed-summary-db-display-search-results (query response)
  "Display search RESPONSE for QUERY using completing-read."
  (let* ((results (alist-get 'results response))
         (model-used (alist-get 'model_used response))
         (reranked (alist-get 'reranked response)))

    (if (zerop (length results))
        (message "No results found for: %s" query)

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length results))
          (let* ((result (elt results i))
                 (entry_id (alist-get 'entry_id result))
                 (chunk-text (alist-get 'chunk_text result))
                 (title (alist-get 'title result))
                 (similarity (alist-get 'similarity_score result))
                 ;; Truncate and clean chunk text for display
                 (context-width 60)
                 (display-text (replace-regexp-in-string
                                "[\n\r]+" " "
                                (if (> (length chunk-text) context-width)
                                    (concat (substring chunk-text 0 (- context-width 3)) "...")
                                  chunk-text)))
                 ;; Pad context to fixed width for alignment
                 (padded-context (format (format "%%-%ds" context-width) display-text))
                 ;; Note: File type prefix is now added by the Python API in chunk-text
                 ;; Format with fixed-width columns: score | context | filename:line
                 (candidate (format "%-6.3f | %s | %s"
                                    similarity
                                    padded-context
                                    title)))
            ;; Store metadata
            (puthash candidate
                     (list :title title
                           :text chunk-text
                           :entry_id entry_id)
                     metadata-table)
            (push candidate candidates)))

        ;; Reverse to show best results first
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                          (format "Search results (%s%s, %d found): "
                                  model-used
                                  (if reranked " + reranked" "")
                                  (length results))
                          candidates
                          nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (entry_id (read (plist-get metadata :entry_id)))
                   (entry (elfeed-db-get-entry entry_id)))
              (when entry
                (elfeed-show-entry entry)
                (message "Opened: %s" (elfeed-entry-title entry)))
              )))))))


;;;###autoload
(defun elfeed-summary-db-search-at-point ()
  "Perform semantic search using text at point or region."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'sentence t))))
    (if query
        (elfeed-summary-db-semantic-search query)
      (message "No text found at point"))))

;; Ivy-based semantic search with dynamic collection
;;
;; The ivy-based semantic search queries the API as you type, providing a truly
;; dynamic search experience. Since semantic search uses embeddings, there's a
;; slight delay (20-50ms) as you type, but results are fast with libsql vector search.
;;
;; Key features:
;; - Queries API dynamically as you type (minimum 2 characters)
;; - Caches results to avoid redundant API calls
;; - Optional reranking for improved relevance
;; - Results show similarity score, context snippet, and file location
;;
;; Usage: M-x elfeed-summary-db-semantic-search-ivy RET
;; Then start typing your query - results update as you type
;; With prefix arg (C-u): Change the number of results per query

(defcustom elfeed-summary-db-ivy-semantic-limit 20
  "Number of results to fetch per query for ivy-based semantic search.
Used when querying the API dynamically as you type."
  :type 'integer
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-ivy-semantic-min-query-length 2
  "Minimum query length before searching semantically.
Queries shorter than this will show a prompt message."
  :type 'integer
  :group 'elfeed-summary-db)

(defvar elfeed-summary-db--current-semantic-limit 20
  "Current limit for semantic search, can be set via prefix arg.")

;;;###autoload
(defun elfeed-summary-db-semantic-search-ivy (&optional limit)
  "Perform dynamic semantic search using ivy - queries API as you type.
Start typing to search - results update dynamically using vector embeddings.
LIMIT sets number of results per query (default `elfeed-summary-db-ivy-semantic-limit').
With prefix arg (C-u), prompts for custom limit."
  (interactive (list (when current-prefix-arg
                       (read-number "Results per query: " elfeed-summary-db-ivy-semantic-limit))))

  (elfeed-summary-db-ensure-server)

  ;; Set current limit for use in dynamic collection
  (setq elfeed-summary-db--current-semantic-limit (or limit elfeed-summary-db-ivy-semantic-limit))

  (if (fboundp 'ivy-read)
      (ivy-read "Semantic search (dynamic): "
                #'elfeed-summary-db--dynamic-semantic-collection
                :dynamic-collection t
                :caller 'elfeed-summary-db-semantic-search-ivy
                :action '(1
                          ("o" elfeed-summary-db--open-semantic-candidate "Open entry")
                          ("c" elfeed-summary-db--copy-semantic-text "Copy summary")))
    (user-error "Ivy is required for dynamic semantic search. Use elfeed-summary-db-semantic-search instead")))

(defun elfeed-summary-db--dynamic-semantic-collection (input)
  "Fetch semantic results matching INPUT dynamically.
Called by ivy as the user types. Returns a list of candidates."
  (if (< (length input) elfeed-summary-db-ivy-semantic-min-query-length)
      ;; Show prompt for short queries
      (list (format "Type at least %d characters to search..." elfeed-summary-db-ivy-semantic-min-query-length))

    ;; Fetch from API
    (let* ((scope-params (when (fboundp 'elfeed-summary-db--scope-to-params)
                           (elfeed-summary-db--scope-to-params)))
           (request-body (append `((query . ,input)
                                   (limit . ,elfeed-summary-db--current-semantic-limit)
                                   (rerank . ,(if elfeed-summary-db-search-use-reranking t :json-false))
                                   (rerank_candidates . ,elfeed-summary-db-search-rerank-candidates))
                                 (when scope-params
                                   (list (cons 'title_pattern (plist-get scope-params :title_pattern))))))
           ;; Synchronous request (required for dynamic collection)
           (response
            (condition-case err
                (let ((url-request-method "POST")
                      (url-request-extra-headers '(("Content-Type" . "application/json")))
                      (url-request-data (encode-coding-string
                                         (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
                                         'utf-8)))
                  (let ((buffer (url-retrieve-synchronously
                                 (concat (elfeed-summary-db-server-url) "/api/search/semantic")
                                 t nil 5)))  ; 5 second timeout
                    (if (not buffer)
                        nil
                      (unwind-protect
                          (with-current-buffer buffer
                            (goto-char (point-min))
                            (when (re-search-forward "^$" nil t)
                              (json-read)))
                        (kill-buffer buffer)))))
              (error
               (message "Semantic search error: %S" err)
               nil))))

      (if (and response (alist-get 'results response))
          (let* ((results (alist-get 'results response))
                 (candidates (if (zerop (length results))
                                 (list (format "No results found for: %s" input))
                               (elfeed-summary-db--build-ivy-semantic-candidates results))))
            candidates)
        ;; Error or no results
        (list (format "Search failed or no results for: %s" input))))))

(defun elfeed-summary-db--build-ivy-semantic-candidates (results)
  "Build ivy candidates from semantic RESULTS array.
Each candidate is a string with metadata stored as text properties."
  (let ((candidates nil))
    (dotimes (i (length results))
      (let* ((result (elt results i))
             (entry_id (alist-get 'entry_id result))
             (chunk-text (alist-get 'chunk_text result))
             (title (alist-get 'title result))
             (similarity (alist-get 'similarity_score result))
             ;; Truncate and clean chunk text for display
             (context-width 60)
             (display-text (replace-regexp-in-string
                            "[\n\r]+" " "
                            (if (> (length chunk-text) context-width)
                                (concat (substring chunk-text 0 (- context-width 3)) "...")
                              chunk-text)))
             (padded-context (format (format "%%-%ds" context-width) display-text))
             ;; Format: score | context | filename:line
             (candidate (format "%-6.3f | %s | %s:%d"
                                similarity
                                padded-context
                                (file-name-nondirectory filename)
                                begin-line)))

        ;; Store metadata as text properties
        (put-text-property 0 (length candidate) 'semantic-data
                           `(
                             :title ,title
                             :text ,chunk-text
                             :entry_id ,entry_id)
                           candidate)
        (push candidate candidates)))
    (nreverse candidates)))

(defun elfeed-summary-db--open-semantic-candidate (candidate)
  "Open file for selected semantic CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'semantic-data candidate))
         (title (plist-get data :title))
         (entry_id (plist-get data :entry_id))
         (entry (elfeed-db-get-entry entry_id)))
    (when entry
      (elfeed-show-entry entry)
      (message "Opened: %s" (elfeed-entry-title entry)))))

(defun elfeed-summary-db--copy-semantic-text (candidate)
  "Copy the chunk text to kill ring for CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'semantic-data candidate))
         (entry_id (plist-get data :entry_id))
         (entry (elfeed-db-get-entry entry_id))
         (title (elfeed-entry-title entry))
         (link (elfeed-entry-link entry))
         (fdate (elfeed-entry-date entry))
         (pdate (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time fdate)))
         (feed-title (elfeed-feed-title (elfeed-entry-feed entry)))
         (summary (elfeed-meta entry :summary)))
    (prog1
        (kill-new  (format "TITLE:\n\t\t%s\nJOURNAL:\n\t\t%s\nDATE:\n\t\t%s\nURL:\n\t\t%s\n\n%s\n"
                           title feed-title pdate link summary))
      (message "Summary copied!"))))

;; Configure ivy for semantic search if available
(with-eval-after-load 'ivy
  (ivy-configure 'elfeed-summary-db-semantic-search-ivy
                 :height 15
                 :sort-fn nil))  ; Keep relevance order from API

(defcustom elfeed-summary-db-ivy-min-query-length 2
  "Minimum query length before searching for images.
Queries shorter than this will show a prompt message."
  :type 'integer
  :group 'elfeed-summary-db)

(provide 'elfeed-summary-db-search)
;;; elfeed-summary-db-search.el ends here
