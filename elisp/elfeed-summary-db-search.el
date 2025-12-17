;;; elfeed-summary-db-search.el --- Semantic search for elfeed summary -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: elfeed-summary-db
;; Keywords: org-mode, database, search

;;; Commentary:

;; Semantic search functionality for elfeed summary.
;; Uses vector embeddings to find semantically similar content.

;;; Code:

(require 'json)
(require 'org)

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

;; Forward declare scope variable
(defvar elfeed-summary-db-search-scope)

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

(defcustom elfeed-summary-db-headline-sort-order "last_updated"
  "Default sort order for headline search results.
- \"filename\": Sort alphabetically by filename
- \"last_updated\": Sort by most recently updated files first (default)
- \"indexed_at\": Sort by most recently indexed files first"
  :type '(choice (const :tag "By filename (alphabetical)" "filename")
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
                                 (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                       (cons 'keyword (plist-get scope-params :keyword)))))))
    (plz 'post (concat (elfeed-summary-db-server-url) "/api/search/semantic")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (when (boundp 'elfeed-summary-db-search-scope)
                (setq elfeed-summary-db-search-scope '(all . nil)))
              (elfeed-summary-db-display-search-results query response))
      :else (lambda (error)
              ;; Reset scope even on error
              (when (boundp 'elfeed-summary-db-search-scope)
                (setq elfeed-summary-db-search-scope '(all . nil)))
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
                 (chunk-text (alist-get 'chunk_text result))
                 (filename (alist-get 'filename result))
                 (similarity (alist-get 'similarity_score result))
                 (begin-line (alist-get 'begin_line result))
                 (end-line (alist-get 'end_line result))
                 (linked-file-path (alist-get 'linked_file_path result))
                 (linked-file-type (alist-get 'linked_file_type result))
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
                 (candidate (format "%-6.3f | %s | %s:%d"
                                    similarity
                                    padded-context
                                    filename
                                    begin-line)))

            ;; Store metadata
            (puthash candidate
                     (list :file filename
                           :line begin-line
                           :end-line end-line
                           :text chunk-text
                           :linked-file-path linked-file-path
                           :linked-file-type linked-file-type)
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
                   (file (plist-get metadata :file))
                   (line (plist-get metadata :line)))
              (when (and file (file-exists-p file))
                (find-file file)
                (goto-char (point-min))
                (forward-line (1- line))
                (recenter)))))))))

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
;; - Multiple actions (use M-o in ivy to access):
;;   - o: Open file at match location (default)
;;   - c: Copy chunk text to kill ring
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
                          ("o" elfeed-summary-db--open-semantic-candidate "Open file")
                          ("c" elfeed-summary-db--copy-semantic-text "Copy text")))
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
                                   (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                         (cons 'keyword (plist-get scope-params :keyword))))))
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
             (chunk-text (alist-get 'chunk_text result))
             (filename (alist-get 'filename result))
             (similarity (alist-get 'similarity_score result))
             (begin-line (alist-get 'begin_line result))
             (end-line (alist-get 'end_line result))
             (linked-file-path (alist-get 'linked_file_path result))
             (linked-file-type (alist-get 'linked_file_type result))
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
                           `(:file ,filename
                                   :line ,begin-line
                                   :end-line ,end-line
                                   :text ,chunk-text
                                   :linked-file-path ,linked-file-path
                                   :linked-file-type ,linked-file-type)
                           candidate)
        (push candidate candidates)))
    (nreverse candidates)))

(defun elfeed-summary-db--open-semantic-candidate (candidate)
  "Open file for selected semantic CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'semantic-data candidate))
         (file (plist-get data :file))
         (line (plist-get data :line)))
    (when (and file (file-exists-p file))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter))))

(defun elfeed-summary-db--copy-semantic-text (candidate)
  "Copy the chunk text to kill ring for CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'semantic-data candidate))
         (text (plist-get data :text)))
    (when text
      (kill-new text)
      (message "Copied to kill ring: %s" (substring text 0 (min 50 (length text)))))))

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

(defvar elfeed-summary-db--current-image-limit 20
  "Current limit for image search, can be set via prefix arg.")

;;;###autoload
(defun elfeed-summary-db-image-search-ivy (&optional limit)
  "Perform dynamic image search using ivy - queries API as you type.
Start typing to search - results update dynamically.
LIMIT sets number of results per query (default `elfeed-summary-db-ivy-image-limit').
With prefix arg (C-u), prompts for custom limit."
  (interactive (list (when current-prefix-arg
                       (read-number "Results per query: " elfeed-summary-db-ivy-image-limit))))

  (elfeed-summary-db-ensure-server)

  ;; Set current limit for use in dynamic collection
  (setq elfeed-summary-db--current-image-limit (or limit elfeed-summary-db-ivy-image-limit))

  (if (fboundp 'ivy-read)
      (ivy-read "Image search (dynamic): "
                #'elfeed-summary-db--dynamic-image-collection
                :dynamic-collection t
                :caller 'elfeed-summary-db-image-search-ivy
                :action '(1
                          ("o" elfeed-summary-db--open-image-candidate "Open org file")
                          ("i" elfeed-summary-db--open-image-file "Open image file")
                          ("c" elfeed-summary-db--copy-image-path "Copy image path")))
    (user-error "Ivy is required for dynamic image search. Use elfeed-summary-db-image-search instead")))

(defun elfeed-summary-db--dynamic-image-collection (input)
  "Fetch images matching INPUT dynamically.
Called by ivy as the user types. Returns a list of candidates with thumbnails."
  (if (< (length input) elfeed-summary-db-ivy-min-query-length)
      ;; Show prompt for short queries
      (list (format "Type at least %d characters to search..." elfeed-summary-db-ivy-min-query-length))

    ;; Fetch from API
    (let* ((scope-params (when (fboundp 'elfeed-summary-db--scope-to-params)
                           (elfeed-summary-db--scope-to-params)))
           (request-body (append `((query . ,input)
                                   (limit . ,elfeed-summary-db--current-image-limit))
                                 (when scope-params
                                   (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                         (cons 'keyword (plist-get scope-params :keyword))))))
           ;; Synchronous request (required for dynamic collection)
           (response
            (condition-case err
                (let ((url-request-method "POST")
                      (url-request-extra-headers '(("Content-Type" . "application/json")))
                      (url-request-data (encode-coding-string
                                         (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
                                         'utf-8)))
                  (with-current-buffer
                      (url-retrieve-synchronously
                       (concat (elfeed-summary-db-server-url) "/api/search/images")
                       t nil 15)  ; 15 second timeout (image search can be slower with large DBs)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (json-read)))
              (error
               (message "Image search error: %S" err)
               nil))))

      (if (and response (alist-get 'results response))
          (let* ((results (alist-get 'results response))
                 (candidates (if (zerop (length results))
                                 (list (format "No images found for: %s" input))
                               (elfeed-summary-db--build-ivy-image-candidates results))))
            candidates)
        ;; Error or no results
        (list (format "Search failed or no results for: %s" input))))))

(defun elfeed-summary-db--build-ivy-image-candidates (results)
  "Build ivy candidates with thumbnails from RESULTS array.
Each candidate is a string with metadata stored as text properties."
  (let ((candidates nil))
    (dotimes (i (length results))
      (let* ((result (elt results i))
             (image-path (alist-get 'image_path result))
             (filename (alist-get 'filename result))
             (similarity (alist-get 'similarity_score result))
             ;; Text line - compact format for filtering
             (image-basename (file-name-nondirectory image-path))
             (org-basename (file-name-nondirectory filename))
             (text-line (format "%.3f | %-40s | %s"
                                similarity
                                (if (> (length image-basename) 40)
                                    (concat (substring image-basename 0 37) "...")
                                  image-basename)
                                org-basename))
             ;; Candidate with thumbnail on second line
             (candidate (if (and image-path (file-exists-p image-path))
                            (concat text-line "\n"
                                    (propertize " " 'display
                                                (create-image image-path nil nil
                                                              :max-width 400
                                                              :max-height 250)))
                          text-line)))

        ;; Store metadata as text properties
        (put-text-property 0 (length candidate) 'image-data
                           `(:path ,image-path :file ,filename :score ,similarity)
                           candidate)
        (push candidate candidates)))
    (nreverse candidates)))

(defun elfeed-summary-db--open-image-candidate (candidate)
  "Open org file for selected image CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'image-data candidate))
         (file (plist-get data :file))
         (image-path (plist-get data :path)))
    (when (and file (file-exists-p file))
      (find-file file)
      (goto-char (point-min))
      (when (search-forward (file-name-nondirectory image-path) nil t)
        (beginning-of-line)
        (recenter)))))

(defun elfeed-summary-db--open-image-file (candidate)
  "Open the image file directly for CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'image-data candidate))
         (image-path (plist-get data :path)))
    (when (and image-path (file-exists-p image-path))
      (find-file image-path))))

(defun elfeed-summary-db--copy-image-path (candidate)
  "Copy the image file path to kill ring for CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'image-data candidate))
         (image-path (plist-get data :path)))
    (when image-path
      (kill-new image-path)
      (message "Copied to kill ring: %s" image-path))))

;; Configure ivy for image search if available
(with-eval-after-load 'ivy
  (ivy-configure 'elfeed-summary-db-image-search-ivy
                 :height 20  ; More space for thumbnails
                 :sort-fn nil))  ; Keep relevance order from API


;; Ivy-based fulltext search with dynamic collection
;;
;; The ivy-based fulltext search queries the FTS5 index as you type, providing
;; dynamic results. FTS5 is fast (~10-100ms) so this provides good UX.

(defcustom elfeed-summary-db-ivy-fulltext-limit 20
  "Number of results to fetch per query for ivy-based fulltext search.
Used when querying the API dynamically as you type."
  :type 'integer
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-ivy-fulltext-min-query-length 2
  "Minimum query length before searching fulltext.
Queries shorter than this will show a prompt message."
  :type 'integer
  :group 'elfeed-summary-db)


(defvar elfeed-summary-db--current-fulltext-limit 20
  "Current limit for fulltext search, can be set via prefix arg.")

;;;###autoload
(defun elfeed-summary-db-fulltext-search-ivy (&optional limit)
  "Perform dynamic fulltext search using ivy - queries API as you type.
Start typing to search - results update dynamically.
LIMIT sets number of results per query (default `elfeed-summary-db-ivy-fulltext-limit').
With prefix arg (C-u), prompts for custom limit."
  (interactive (list (when current-prefix-arg
                       (read-number "Results per query: " elfeed-summary-db-ivy-fulltext-limit))))

  (elfeed-summary-db-ensure-server)

  ;; Set current limit for use in dynamic collection
  (setq elfeed-summary-db--current-fulltext-limit (or limit elfeed-summary-db-ivy-fulltext-limit))

  (if (fboundp 'ivy-read)
      (ivy-read "Fulltext search (dynamic): "
                #'elfeed-summary-db--dynamic-fulltext-collection
                :dynamic-collection t
                :caller 'elfeed-summary-db-fulltext-search-ivy
                :action #'elfeed-summary-db--open-fulltext-candidate)
    (user-error "Ivy is required for dynamic fulltext search. Use elfeed-summary-db-fulltext-search instead")))

(defun elfeed-summary-db--dynamic-fulltext-collection (input)
  "Fetch fulltext results matching INPUT dynamically.
Called by ivy as the user types. Returns a list of candidates."
  (if (< (length input) elfeed-summary-db-ivy-fulltext-min-query-length)
      ;; Show prompt for short queries
      (list (format "Type at least %d characters to search..." elfeed-summary-db-ivy-fulltext-min-query-length))

    ;; Fetch from API
    (let* ((scope-params (when (fboundp 'elfeed-summary-db--scope-to-params)
                           (elfeed-summary-db--scope-to-params)))
           (request-body (append `((query . ,input)
                                   (limit . ,elfeed-summary-db--current-fulltext-limit))
                                 (when scope-params
                                   (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                         (cons 'keyword (plist-get scope-params :keyword))))))
           ;; Synchronous request (required for dynamic collection)
           (response
            (condition-case err
                (let ((url-request-method "POST")
                      (url-request-extra-headers '(("Content-Type" . "application/json")))
                      (url-request-data (encode-coding-string
                                         (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
                                         'utf-8)))
                  (let ((buffer (url-retrieve-synchronously
                                 (concat (elfeed-summary-db-server-url) "/api/search/fulltext")
                                 t nil 5)))  ; 5 second timeout (FTS5 is fast)
                    (if (not buffer)
                        nil
                      (unwind-protect
                          (with-current-buffer buffer
                            (goto-char (point-min))
                            (when (re-search-forward "^$" nil t)
                              (json-read)))
                        (kill-buffer buffer)))))
              (error
               (message "Fulltext search error: %S" err)
               nil))))

      (if (and response (alist-get 'results response))
          (let* ((results (alist-get 'results response))
                 (candidates (if (zerop (length results))
                                 (list (format "No results found for: %s" input))
                               (elfeed-summary-db--build-ivy-fulltext-candidates results))))
            candidates)
        ;; Error or no results
        (list (format "Search failed or no results for: %s" input))))))

(defun elfeed-summary-db--build-ivy-fulltext-candidates (results)
  "Build ivy candidates from fulltext RESULTS array.
Each candidate is a string with metadata stored as text properties."
  (let ((candidates nil))
    (dotimes (i (length results))
      (let* ((result (elt results i))
             (filename (alist-get 'filename result))
             (title (alist-get 'title result))
             (snippet (alist-get 'snippet result))
             (rank (alist-get 'rank result))
             ;; Extract search term from snippet (text between >>> and <<<)
             (search-term (when (string-match ">>>\\([^<]+\\)<<<" snippet)
                            (match-string 1 snippet)))
             ;; Clean snippet for display (remove markers but keep newlines)
             (clean-snippet (replace-regexp-in-string ">>>\\|<<<" "" snippet))
             ;; Split snippet into lines and pad each to 80 chars
             (snippet-lines (split-string clean-snippet "\n"))
             (snippet-width 80)
             ;; Format: rank | snippet | filename
             (candidate (if (= (length snippet-lines) 1)
                            ;; Single line: format normally
                            (format "%8.2f | %-80s | %s"
                                    (abs rank)
                                    (truncate-string-to-width (car snippet-lines) snippet-width 0 ?\s)
                                    (file-name-nondirectory filename))
                          ;; Multi-line: first line with score and filename, rest indented
                          (concat
                           (format "%8.2f | %-80s | %s"
                                   (abs rank)
                                   (truncate-string-to-width (car snippet-lines) snippet-width 0 ?\s)
                                   (file-name-nondirectory filename))
                           (mapconcat
                            (lambda (line)
                              (format "\n         | %-80s |"
                                      (truncate-string-to-width line snippet-width 0 ?\s)))
                            (cdr snippet-lines)
                            "")))))

        ;; Store metadata as text properties
        (put-text-property 0 (length candidate) 'fulltext-data
                           `(:file ,filename :title ,title :search-term ,search-term)
                           candidate)
        (push candidate candidates)))
    (nreverse candidates)))

(defun elfeed-summary-db--open-fulltext-candidate (candidate)
  "Open file for selected fulltext CANDIDATE.
CANDIDATE is a string with metadata stored in text properties."
  (let* ((data (get-text-property 0 'fulltext-data candidate))
         (file (plist-get data :file))
         (search-term (plist-get data :search-term))
         (title (plist-get data :title)))
    (when (and file (file-exists-p file))
      (find-file file)
      (goto-char (point-min))
      ;; Try to search for the matched term first, then title
      (if (and search-term (search-forward search-term nil t))
          (progn
            (beginning-of-line)
            (recenter))
        (when (search-forward title nil t)
          (beginning-of-line)
          (recenter))))))

;; Configure ivy for fulltext search if available
(with-eval-after-load 'ivy
  (ivy-configure 'elfeed-summary-db-fulltext-search-ivy
                 :height 15
                 :sort-fn nil))  ; Keep relevance order from API

;;;###autoload
(defun elfeed-summary-db-headline-search-all (&optional sort-by)
  "Browse ALL headlines and jump to selection.
Loads all headlines from database upfront. For large databases (100K+
headlines), this can be slow. Consider using `elfeed-summary-db-headline-search'
instead, which filters dynamically as you type.

You can filter candidates using completing-read after loading.
Optional SORT-BY specifies the sort order:
  - \"filename\": Sort alphabetically by filename (default)
  - \"last_updated\": Sort by most recently updated files first
  - \"indexed_at\": Sort by most recently indexed files first
With prefix arg, prompt for sort order interactively."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Sort by: "
                            '("filename" "last_updated" "indexed_at")
                            nil t nil nil elfeed-summary-db-headline-sort-order))))

  (elfeed-summary-db-ensure-server)

  (let* ((sort-order (or sort-by elfeed-summary-db-headline-sort-order))
         (scope-params (when (fboundp 'elfeed-summary-db--scope-to-params)
                         (elfeed-summary-db--scope-to-params)))
         (request-body (append `((query . "")
                                 (sort_by . ,sort-order))
                               (when scope-params
                                 (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                       (cons 'keyword (plist-get scope-params :keyword)))))))
    (plz 'post (concat (elfeed-summary-db-server-url) "/api/search/headlines")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as (lambda () (json-parse-buffer :object-type 'alist :array-type 'list))
      :then (lambda (response)
              ;; Reset scope after search
              (when (boundp 'elfeed-summary-db-search-scope)
                (setq elfeed-summary-db-search-scope '(all . nil)))
              (elfeed-summary-db-display-headline-results response))
      :else (lambda (error)
              ;; Reset scope even on error
              (when (boundp 'elfeed-summary-db-search-scope)
                (setq elfeed-summary-db-search-scope '(all . nil)))
              (message "Search error: %s" (plz-error-message error))))))

(defun elfeed-summary-db-display-headline-results (response)
  "Display headline search RESPONSE using completing-read."
  (let ((results (alist-get 'results response)))

    (if (zerop (length results))
        (message "No headlines found in database")

      ;; Results are pre-formatted by server: (display_string filename begin)
      ;; Server handles string formatting for performance with 100K+ headlines
      ;; JSON parser returns lists directly - pass to completing-read as-is
      (let ((selection (completing-read
                        (format "Headlines (%d found): " (length results))
                        results
                        nil t)))
        (when selection
          ;; Selection is the display string, use assoc to find full data
          (let* ((data (assoc selection results))
                 (file (cadr data))
                 (begin (caddr data)))
            (when (and file (file-exists-p file))
              (find-file file)
              (goto-char begin)
              (org-show-entry)
              (recenter))))))))

(defun elfeed-summary-db--char-to-line (filename char-pos)
  "Convert character position CHAR-POS in FILENAME to line number.
Returns 1 if file doesn't exist."
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (min char-pos (point-max)))
        (line-number-at-pos))
    ;; File doesn't exist (e.g., test files), return reasonable default
    1))

;; Dynamic headline search (primary method)
;;
;; `elfeed-summary-db-headline-search' queries the database as you type, providing
;; instant filtering without loading all headlines upfront. This is much faster
;; for large databases (100K+ headlines).
;;
;; Key features:
;; - Queries API dynamically as you type (filters server-side)
;; - Fast filtering using SQL LIKE queries
;; - Customizable sort order (filename, last_updated, indexed_at)
;; - Only fetches matching results (default limit: 100 per query)
;; - Results show formatted headline and filename
;;
;; Usage: M-x elfeed-summary-db-headline-search RET
;; Then start typing to filter - results update as you type
;; With prefix arg (C-u): Choose sort order interactively
;;
;; For browsing ALL headlines: M-x elfeed-summary-db-headline-search-all
;; (loads everything upfront, slower for large databases)

(defcustom elfeed-summary-db-ivy-headline-limit 100
  "Number of headlines to fetch per query for ivy-based headline search.
Used when querying the API dynamically as you type."
  :type 'integer
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-ivy-headline-min-query-length 2
  "Minimum query length before searching headlines.
Queries shorter than this will show all headlines (up to limit)."
  :type 'integer
  :group 'elfeed-summary-db)

(defvar elfeed-summary-db--current-headline-sort "last_updated"
  "Current sort order for headline search, can be set via prefix arg.")

(defvar elfeed-summary-db--last-headline-results nil
  "Cache of last headline search results for ivy actions.")

;;;###autoload
(defun elfeed-summary-db-headline-search (&optional sort-by)
  "Browse headlines with dynamic filtering - queries API as you type.
Much faster than `elfeed-summary-db-headline-search-all' for large databases.

Start typing to filter headlines - results update dynamically.
SORT-BY specifies sort order (filename, last_updated, or indexed_at).
With prefix arg (C-u), prompts for sort order interactively.

Requires ivy. Falls back to `elfeed-summary-db-headline-search-all' if ivy not available."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Sort by: "
                            '("filename" "last_updated" "indexed_at")
                            nil t nil nil elfeed-summary-db-headline-sort-order))))

  (elfeed-summary-db-ensure-server)

  ;; Set current sort order for use in dynamic collection
  (setq elfeed-summary-db--current-headline-sort (or sort-by elfeed-summary-db-headline-sort-order))

  (if (fboundp 'ivy-read)
      (ivy-read "Headlines (dynamic filter): "
                #'elfeed-summary-db--dynamic-headline-collection
                :dynamic-collection t
                :caller 'elfeed-summary-db-headline-search
                :action #'elfeed-summary-db--open-headline-candidate)
    ;; Fallback to loading all headlines if ivy not available
    (message "Ivy not available, falling back to loading all headlines...")
    (elfeed-summary-db-headline-search-all sort-by)))

(defun elfeed-summary-db--dynamic-headline-collection (input)
  "Fetch headlines matching INPUT dynamically.
Called by ivy as the user types. Returns a list of candidates."
  ;; Even for empty input, fetch some results (up to limit)
  (let* ((scope-params (when (fboundp 'elfeed-summary-db--scope-to-params)
                         (elfeed-summary-db--scope-to-params)))
         (request-body (append `((query . ,input)
                                 (limit . ,elfeed-summary-db-ivy-headline-limit)
                                 (sort_by . ,elfeed-summary-db--current-headline-sort))
                               (when scope-params
                                 (list (cons 'filename_pattern (plist-get scope-params :filename_pattern))
                                       (cons 'keyword (plist-get scope-params :keyword))))))
         ;; Synchronous request (required for dynamic collection)
         (response
          (condition-case err
              (let ((url-request-method "POST")
                    (url-request-extra-headers '(("Content-Type" . "application/json")))
                    (url-request-data (encode-coding-string
                                       (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
                                       'utf-8)))
                (let ((buffer (url-retrieve-synchronously
                               (concat (elfeed-summary-db-server-url) "/api/search/headlines")
                               t nil 5)))  ; 5 second timeout
                  (if (not buffer)
                      nil
                    (unwind-protect
                        (with-current-buffer buffer
                          (goto-char (point-min))
                          (when (re-search-forward "^$" nil t)
                            (json-parse-buffer :object-type 'alist :array-type 'list)))
                      (kill-buffer buffer)))))
            (error
             (message "Headline search error: %S" err)
             nil))))

    (if (and response (alist-get 'results response))
        (let ((results (alist-get 'results response)))
          (if (zerop (length results))
              (list (format "No headlines found for: %s" input))
            ;; Store full results for later lookup by action function
            ;; Results format from server: (display_string filename begin)
            (setq elfeed-summary-db--last-headline-results results)
            ;; Return only display strings for ivy
            (mapcar #'car results)))
      ;; Error or no results
      (list (format "Search failed or no results for: %s" input)))))

(defun elfeed-summary-db--open-headline-candidate (candidate)
  "Open file for selected headline CANDIDATE.
CANDIDATE is a display string. Look it up in cached results."
  (let ((data (assoc candidate elfeed-summary-db--last-headline-results)))
    (if data
        (let ((file (cadr data))
              (begin (caddr data)))
          (if (and file (file-exists-p file))
              (progn
                (find-file file)
                (goto-char begin)
                (org-show-entry)
                (recenter))
            (message "File does not exist: %s" file)))
      (message "Could not find headline data for: %s" candidate))))

;; Configure ivy for headline search if available
(with-eval-after-load 'ivy
  (ivy-configure 'elfeed-summary-db-headline-search
                 :height 15
                 :sort-fn nil))  ; Keep sort order from API

;;;###autoload
(defun elfeed-summary-db-open-file ()
  "Browse all files in database and open selected file."
  (interactive)

  (elfeed-summary-db-ensure-server)

  (plz 'get (concat (elfeed-summary-db-server-url) "/api/stats/files")
    :as #'json-read
    :then (lambda (response)
            (elfeed-summary-db-display-file-list response))
    :else (lambda (error)
            (message "Error fetching files: %s" (plz-error-message error)))))

;;;###autoload
(defun elfeed-summary-db-open-linked-file ()
  "Browse all linked files (PDF, DOCX, etc.) in database and open selected file or org link."
  (interactive)

  (elfeed-summary-db-ensure-server)

  (plz 'get (concat (elfeed-summary-db-server-url) "/api/linked-files/all")
    :as #'json-read
    :then (lambda (response)
            (elfeed-summary-db-display-linked-file-list response))
    :else (lambda (error)
            (message "Error fetching linked files: %s" (plz-error-message error)))))

(defun elfeed-summary-db-display-file-list (response)
  "Display file list from RESPONSE using completing-read."
  (let* ((files (alist-get 'files response))
         (count (alist-get 'count response)))

    (if (zerop count)
        (message "No files found in database")

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length files))
          (let* ((file-info (elt files i))
                 (filename (alist-get 'filename file-info))
                 (indexed-at (alist-get 'indexed_at file-info))
                 ;; Format timestamp for display (remove microseconds if present)
                 (display-time (if indexed-at
                                   (replace-regexp-in-string "\\..*" "" indexed-at)
                                 "unknown"))
                 ;; Format: timestamp | filename
                 (candidate (format "%s | %s"
                                    display-time
                                    filename)))

            ;; Store metadata
            (puthash candidate filename metadata-table)
            (push candidate candidates)))

        ;; Keep chronological order (most recent first, already sorted from server)
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                          (format "Open file (%d files in database): " count)
                          candidates
                          nil t)))
          (when selection
            (let ((file (gethash selection metadata-table)))
              (if (and file (file-exists-p file))
                  (find-file file)
                (message "File does not exist: %s" file)))))))))

(defun elfeed-summary-db-display-linked-file-list (response)
  "Display linked file list from RESPONSE using completing-read."
  (let* ((linked-files (alist-get 'linked_files response))
         (count (alist-get 'count response)))

    (if (zerop count)
        (message "No linked files found in database")

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length linked-files))
          (let* ((file-info (elt linked-files i))
                 (file-path (alist-get 'file_path file-info))
                 (file-type (alist-get 'file_type file-info))
                 (org-filename (alist-get 'org_filename file-info))
                 (org-link-line (alist-get 'org_link_line file-info))
                 (chunk-count (alist-get 'chunk_count file-info))
                 (indexed-at (alist-get 'indexed_at file-info))
                 ;; Format timestamp for display (remove microseconds if present)
                 (display-time (if indexed-at
                                   (replace-regexp-in-string "\\..*" "" indexed-at)
                                 "unknown"))
                 ;; Format: [TYPE] filename | org-file:line | chunks | timestamp
                 (candidate (format "[%-4s] %-40s | %s:%d | %d chunks | %s"
                                    (upcase (or file-type ""))
                                    (file-name-nondirectory file-path)
                                    (file-name-nondirectory org-filename)
                                    org-link-line
                                    chunk-count
                                    display-time)))

            ;; Store metadata
            (puthash candidate
                     (list :file-path file-path
                           :org-filename org-filename
                           :org-link-line org-link-line)
                     metadata-table)
            (push candidate candidates)))

        ;; Keep chronological order (most recent first, already sorted from server)
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                          (format "Open linked file (%d files in database): " count)
                          candidates
                          nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (file-path (plist-get metadata :file-path))
                   (org-filename (plist-get metadata :org-filename))
                   (org-link-line (plist-get metadata :org-link-line)))
              ;; With prefix arg, open the linked file directly
              ;; Otherwise, open the org file and go to the link
              (if current-prefix-arg
                  (if (and file-path (file-exists-p file-path))
                      (find-file file-path)
                    (message "Linked file does not exist: %s" file-path))
                ;; Default: open org file at link location
                (when (and org-filename (file-exists-p org-filename))
                  (find-file org-filename)
                  (goto-char (point-min))
                  (forward-line (1- org-link-line))
                  (recenter))))))))))

;;;###autoload
(defun elfeed-summary-db-property-search (query)
  "Search headlines by property name and optional value.
QUERY should be in format \"PROPERTY=PATTERN\" or just \"PROPERTY\".
Examples:
  CATEGORY=org-db     Search for CATEGORY property with value matching 'org-db'
  TODO=DONE          Search for TODO property with value matching 'DONE'
  CATEGORY           Search for any headline with CATEGORY property"
  (interactive "sProperty search (PROPERTY or PROPERTY=PATTERN): ")

  (elfeed-summary-db-ensure-server)

  ;; Parse query to extract property and optional value pattern
  (let* ((parts (split-string query "=" t))
         (property (string-trim (car parts)))
         (value (when (cdr parts) (string-trim (cadr parts))))
         (scope-params (when (fboundp 'elfeed-summary-db--scope-to-params)
                         (elfeed-summary-db--scope-to-params)))
         (request-body (append `((property . ,property)
                                 (limit . 100))
                               (when value
                                 (list (cons 'value value)))
                               (when scope-params
                                 (list (cons 'filename_pattern (plist-get scope-params :filename_pattern)))))))

    (plz 'post (concat (elfeed-summary-db-server-url) "/api/search/properties")
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode (seq-filter (lambda (pair) (cdr pair)) request-body))
      :as #'json-read
      :then (lambda (response)
              ;; Reset scope after search
              (when (boundp 'elfeed-summary-db-search-scope)
                (setq elfeed-summary-db-search-scope '(all . nil)))
              (elfeed-summary-db-display-property-results query response))
      :else (lambda (error)
              ;; Reset scope even on error
              (when (boundp 'elfeed-summary-db-search-scope)
                (setq elfeed-summary-db-search-scope '(all . nil)))
              (message "Search error: %s" (plz-error-message error))))))

(defun elfeed-summary-db-display-property-results (query response)
  "Display property search RESPONSE for QUERY using completing-read."
  (let ((results (alist-get 'results response)))

    (if (zerop (length results))
        (message "No properties found for: %s" query)

      ;; Build candidates with metadata
      (let* ((candidates nil)
             (metadata-table (make-hash-table :test 'equal)))

        (dotimes (i (length results))
          (let* ((result (elt results i))
                 (headline-title (alist-get 'headline_title result))
                 (filename (alist-get 'filename result))
                 (begin (alist-get 'begin result))
                 (property (alist-get 'property result))
                 (value (alist-get 'value result))
                 ;; Fixed widths for alignment
                 (title-width 40)
                 (value-width 20)
                 (display-title (if (> (length headline-title) title-width)
                                    (concat (substring headline-title 0 (- title-width 3)) "...")
                                  headline-title))
                 (padded-title (format (format "%%-%ds" title-width) display-title))
                 (display-value (if (> (length value) value-width)
                                    (concat (substring value 0 (- value-width 3)) "...")
                                  value))
                 (padded-value (format (format "%%-%ds" value-width) display-value))
                 ;; Calculate line number from character position
                 (line-number (elfeed-summary-db--char-to-line filename begin))
                 ;; Format: property=value | headline | filename:line
                 (candidate (format "%s=%-20s | %s | %s:%d"
                                    property
                                    padded-value
                                    padded-title
                                    filename
                                    line-number)))

            ;; Store metadata
            (puthash candidate
                     (list :file filename
                           :begin begin
                           :line line-number)
                     metadata-table)
            (push candidate candidates)))

        ;; Keep original order (by filename and position)
        (setq candidates (nreverse candidates))

        ;; Let user select
        (let ((selection (completing-read
                          (format "Property results (%d found): " (length results))
                          candidates
                          nil t)))
          (when selection
            (let* ((metadata (gethash selection metadata-table))
                   (file (plist-get metadata :file))
                   (begin (plist-get metadata :begin)))
              (when (and file (file-exists-p file))
                (find-file file)
                (goto-char begin)
                (org-show-entry)
                (recenter)))))))))

(provide 'elfeed-summary-db-search)
;;; elfeed-summary-db-search.el ends here
