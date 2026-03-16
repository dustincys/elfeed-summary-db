;;; elfeed-summary-db-gptel-tools.el --- gptel tools for elfeed-summary-db search -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides gptel tools that allow LLMs to search your elfeed entries.
;;
;; Tools included:
;;
;; 1. elfeed_semantic_search
;;    Uses elfeed-summary-db vector search server
;;
;; 2. elfeed_fuzzy_summary_search
;;    Performs local fuzzy search over AI summaries stored in elfeed metadata
;;
;; Results include clickable [[ELFEED:ID]] links that are converted
;; into buttons inside gptel buffers.

;;; Code:

(require 'gptel)
(require 'elfeed)
(require 'elfeed-summary-db-search)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup elfeed-summary-db nil
  "LLM search integration for elfeed-summary-db."
  :group 'elfeed)

(defcustom elfeed-summary-db-gptel-search-limit 5
  "Default number of search results returned to LLM tools."
  :type 'integer
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-summary-meta-key :summary
  "Metadata key used for AI summaries."
  :type 'symbol
  :group 'elfeed-summary-db)

;;; ------------------------------------------------------------
;;; Result formatting
;;; ------------------------------------------------------------

(defun elfeed-summary-db-gptel--format-search-result (result)
  "Format a single search RESULT for LLM consumption."
  (let* ((entry-id (alist-get 'entry_id result))
         (title (alist-get 'title result))
         (similarity (or (alist-get 'similarity_score result) 0))
         (entry (ignore-errors
                  (elfeed-db-get-entry (read entry-id))))
         (summary (and entry
                       (elfeed-meta entry
                                    elfeed-summary-db-summary-meta-key))))
    (format
     "Title: %s
[[ELFEED:%s]]
Relevance: %.3f
Summary:
%s"
     title
     entry-id
     similarity
     (or summary ""))))

;;; ------------------------------------------------------------
;;; Button renderer
;;; ------------------------------------------------------------

(defun elfeed-summary-db-gptel--render-buttons (start end)
  "Replace [[ELFEED:...]] markers with clickable buttons.
START and END delimit the LLM response region."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\[\\[ELFEED:\\(.*?\\)\\]\\]" end t)
      (let* ((raw (match-string 1))
             (entry-id (ignore-errors (read raw)))
             (entry (and entry-id
                         (elfeed-db-get-entry entry-id)))
             (btn-start (match-beginning 0))
             (btn-end (match-end 0)))
        (when entry
          (delete-region btn-start btn-end)
          (insert-text-button
           "🔗 Open in Elfeed"
           'follow-link t
           'help-echo (elfeed-entry-title entry)
           'action
           (lambda (_)
             (elfeed-show-entry entry)
             (message "Opened: %s"
                      (elfeed-entry-title entry)))))))))

;;; ------------------------------------------------------------
;;; Semantic search (vector)
;;; ------------------------------------------------------------

(defun elfeed-summary-db-gptel--semantic-search
    (query &optional limit title-pattern)
  "Perform semantic search for QUERY.

LIMIT controls result count.
TITLE-PATTERN optionally filters entries."
  (let* ((search-limit (or limit
                           elfeed-summary-db-gptel-search-limit))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request-data
          `((query . ,query)
            (limit . ,search-limit)
            ,@(when title-pattern
                `((title_pattern . ,title-pattern)))))
         (url-request-data
          (encode-coding-string
           (json-encode request-data)
           'utf-8))
         (response-buffer
          (url-retrieve-synchronously
           (concat (elfeed-summary-db-server-url)
                   "/api/search/semantic")
           t nil 10)))

    (if (not response-buffer)
        (error "Failed to connect to elfeed-summary-db server")

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

            (mapconcat
             #'elfeed-summary-db-gptel--format-search-result
             results
             "\n---\n")))))))

;;; ------------------------------------------------------------
;;; Fuzzy summary search (local)
;;; ------------------------------------------------------------

(defun elfeed-summary-db-gptel--fuzzy-summary-search (query &optional limit)
  "Fuzzy search AI summaries using QUERY."
  (require 'flx nil t)

  (let* ((limit (or limit 5))
         (summary-key :summary)
         (query (downcase query))
         results)

    (with-elfeed-db-visit (entry _)
      (let* ((summary (elfeed-meta entry summary-key))
             (title (elfeed-entry-title entry)))

        (when (and summary (stringp summary) (not (string-empty-p summary)))

          (let* ((clean-summary (replace-regexp-in-string "\n" " " summary))
                 (text (downcase (format "%s %s" title clean-summary)))
                 (score (cond
                         ;; best case: flx fuzzy score
                         ((and (featurep 'flx)
                               (car-safe (flx-score query text))))
                         ;; fallback: substring match
                         ((string-match-p (regexp-quote query) text) 1)
                         ;; otherwise no match
                         (t nil))))

            (when score
              (push
               `((score . ,score)
                 (title . ,title)
                 (entry-id . ,(prin1-to-string (elfeed-entry-id entry)))
                 (summary . ,clean-summary))
               results))))))

    ;; sort results
    (setq results
          (seq-take
           (sort results
                 (lambda (a b)
                   (> (alist-get 'score a)
                      (alist-get 'score b))))
           limit))

    (if (null results)
        "No fuzzy matches found."
      (mapconcat
       (lambda (r)
         (format
          "Title: %s
[[ELFEED:%s]]
Summary:
%s"
          (alist-get 'title r)
          (alist-get 'entry-id r)
          (alist-get 'summary r)))
       results
       "\n---\n"))))

;;; ------------------------------------------------------------
;;; Tool registration
;;; ------------------------------------------------------------

;;;###autoload
(defun elfeed-summary-db-gptel-register-tools ()
  "Register elfeed-summary-db tools with gptel."
  (interactive)

  ;; semantic search
  (gptel-make-tool
   :function #'elfeed-summary-db-gptel--semantic-search
   :name "elfeed_semantic_search"
   :description
   "Search elfeed entries using semantic vector similarity. Best for conceptual queries."
   :args
   (list
    '(:name "query"
            :type "string"
            :description "Natural language query.")

    '(:name "limit"
            :type "number"
            :description "Maximum results (default 5)."
            :optional t)

    '(:name "title_pattern"
            :type "string"
            :description "SQL LIKE filter on title."
            :optional t))
   :category "elfeed-summary-db")

  ;; fuzzy search
  (gptel-make-tool
   :function #'elfeed-summary-db-gptel--fuzzy-summary-search
   :name "elfeed_fuzzy_summary_search"
   :description
   "Fuzzy search AI summaries of elfeed entries. Useful when approximate matches are sufficient."
   :args
   (list
    '(:name "query"
            :type "string"
            :description "Text to match against summaries.")

    '(:name "limit"
            :type "number"
            :description "Maximum results (default 5)."
            :optional t))
   :category "elfeed-summary-db")

  (message "elfeed-summary-db gptel tools registered."))

;;; ------------------------------------------------------------
;;; Tool unregister
;;; ------------------------------------------------------------

;;;###autoload
(defun elfeed-summary-db-gptel-unregister-tools ()
  "Remove elfeed-summary-db tools from gptel."
  (interactive)

  (setq gptel-tools
        (seq-remove
         (lambda (tool)
           (member
            (plist-get tool :name)
            '("elfeed_semantic_search"
              "elfeed_fuzzy_summary_search")))
         gptel-tools))

  (message "elfeed-summary-db gptel tools removed."))

(provide 'elfeed-summary-db-gptel-tools)

;;; elfeed-summary-db-gptel-tools.el ends here
