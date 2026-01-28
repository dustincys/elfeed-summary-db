;;; elfeed-summary-db-gptel-tools.el --- gptel tools for org-db search -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides gptel tools that allow LLMs to search your org files using
;; semantic and fulltext search capabilities of elfeed-summary-db.

;;; Code:

(require 'gptel)
(require 'elfeed-summary-db-search)
(require 'json)

(defcustom elfeed-summary-db-gptel-search-limit 5
  "Default number of search results to return to LLM tools."
  :type 'integer
  :group 'elfeed-summary-db)

(defun elfeed-summary-db-gptel--format-search-result (result)
  "Format a single search RESULT for LLM consumption."
  (let* ((chunk-id (alist-get 'chunk_id result))
         (chunk-text (alist-get 'chunk_text result))
         (similarity (alist-get 'similarity_score result))
         (title (alist-get 'title result))
         (chunk-type (alist-get 'chunk_type result))
         (entry-id (alist-get 'entry_id result))
         (summary (elfeed-meta (elfeed-db-get-entry entry-id) :summary)))
    (format "Title %s%s\nEntry: %s\nRelevance: %.3f\nSummary:\n%s\n"
            title
            entry-id
            similarity
            summary)))

(defun elfeed-summary-db-gptel--semantic-search (query &optional limit title-pattern)
  "Perform semantic search for QUERY.
Returns up to LIMIT results (default from `elfeed-summary-db-gptel-search-limit').
Optional TITLE-PATTERN restricts search to matching entries."
  (let* ((search-limit (or limit elfeed-summary-db-gptel-search-limit))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request-data `((query . ,query)
                         (limit . ,search-limit)
                         ,@(when title-pattern
                             `((title_pattern . ,title-pattern)))))
         (url-request-data (encode-coding-string
                            (json-encode request-data)
                            'utf-8))
         (response-buffer (url-retrieve-synchronously
                           (concat (elfeed-summary-db-server-url) "/api/search/semantic")
                           t nil 10)))
    (if (not response-buffer)
        (error "Failed to connect to org-db server")
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (response (json-read))
               (results (alist-get 'results response)))
          (kill-buffer)
          (if (null results)
              "No results found."
            (mapconcat #'elfeed-summary-db-gptel--format-search-result
                       results
                       "\n---\n")))))))


;;;###autoload
(defun elfeed-summary-db-gptel-register-tools ()
  "Register elfeed-summary-db search tools with gptel."
  (interactive)
  (gptel-make-tool
   :function #'elfeed-summary-db-gptel--semantic-search
   :name "elfeed_semantic_search"
   :description "Search through elfeed entries using semantic/AI similarity. Best for conceptual queries, finding related content, or when you don't know exact keywords. Returns relevant passages with context."
   :args (list
          '(:name "query"
                  :type "string"
                  :description "Natural language query describing what to search for. Can be a question, concept, or topic.")
          '(:name "limit"
                  :type "number"
                  :description "Maximum number of results to return (default 5, max 20)"
                  :optional t)
          '(:name "title_pattern"
                  :type "string"
                  :description "SQL LIKE pattern to filter files (e.g., '%project%' for files containing 'project'). Use '%' as wildcard."
                  :optional t))
   :category "org-db")

  (message "elfeed-summary-db search tools registered with gptel"))

;;;###autoload
(defun elfeed-summary-db-gptel-unregister-tools ()
  "Unregister elfeed-summary-db search tools from gptel."
  (interactive)
  ;; Remove tools by filtering gptel-tools
  (setq gptel-tools
        (seq-remove (lambda (tool)
                      (member (plist-get tool :name)
                              '("elfeed_semantic_search")))
                    gptel-tools))
  (message "elfeed-summary-db search tools unregistered from gptel"))

(provide 'elfeed-summary-db-gptel-tools)
;;; elfeed-summary-db-gptel-tools.el ends here
