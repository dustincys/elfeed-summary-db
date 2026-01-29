;;; elfeed-summary-db-ui.el --- Transient menu interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: elfeed-summary-db
;; Keywords: org-mode, database, search

;;; Commentary:

;; Transient menu interface for elfeed-summary db commands.
;; Provides an easy-to-use menu system for all search and management functions.

;;; Code:

(require 'transient)
(require 'elfeed-summary-db-search)
(require 'elfeed-summary-db-server)
(require 'elfeed-summary-db-client)

(defvar elfeed-summary-db-search-scope '(all . nil)
  "Current search scope. Format: (type . value)
   - (all . nil) - search all files
   - (title_pattern . \"title\") - title pattern
   Resets to (all . nil) after each search.")

(defun elfeed-summary-db--scope-description ()
  "Return current scope description for transient header."
  (pcase (car elfeed-summary-db-search-scope)
    ('all "All entries")
    ('title_pattern "title pattern")
    (_ "All files")))

(defun elfeed-summary-db--scope-to-params ()
  "Convert current scope to API filter parameters.
Returns plist with :filename_pattern and/or :keyword."
  (pcase (car elfeed-summary-db-search-scope)
    ('all nil)
    ('title_pattern
     (list :title_pattern (concat (cdr elfeed-summary-db-search-scope) "%")))
    (_ nil)))

;; Transient infix for all files scope
(transient-define-infix elfeed-summary-db--scope-all-infix ()
  "Set search scope to all files."
  :class 'transient-lisp-variable
  :variable 'elfeed-summary-db-search-scope
  :key "-a"
  :description "All entries"
  :reader (lambda (&rest _) '(all . nil)))

;; Transient infix for tag scope
(transient-define-infix elfeed-summary-db--scope-title-pattern-infix ()
  "Set search scope to titles with a specific pattern."
  :class 'transient-lisp-variable
  :variable 'elfeed-summary-db-search-scope
  :key "-t"
  :description "Title pattern"
  :reader (lambda (prompt _initial-input _history)
            (let ((title_pattern (read-string "Title pattern: ")))
              (when (and title_pattern (not (string-empty-p title_pattern)))
                (cons 'title_pattern title_pattern)))))

;;;###autoload (autoload 'elfeed-summary-db-menu "elfeed-summary-db-ui" nil t)
(transient-define-prefix elfeed-summary-db-menu ()
  [:description (lambda () (format "elfeed-summary db [Scope: %s]" (elfeed-summary-db--scope-description)))
                "Search and manage your org files."]
  [["Scope"
    ("-a" elfeed-summary-db--scope-all-infix)
    ("-t" elfeed-summary-db--scope-title-pattern-infix)]
   ["Actions"
    ("q" "Quit" transient-quit-one)]]
  ["Search"
   ["Text Search"
    ("v" "Semantic search" elfeed-summary-db--semantic-search-dispatch
     :description (lambda () (if (fboundp 'ivy-read)
                                 "Vector embeddings (dynamic)"
                               "Vector embeddings")))]
   ]
  ["Management"
   ["Indexing"
    ("u" "Update current entry" elfeed-summary-db-index-current-entry
     :description "Index current entry")
    ("U" "Update all entries" elfeed-summary-db-index-all-entries
     :description "Index all entries")
    ("r" "Reindex database" elfeed-summary-db-reindex-database
     :description "Reindex database")]
   ["Server"
    ("S" "Server status" elfeed-summary-db-server-status
     :description "Check server status")
    ("R" "Restart server" elfeed-summary-db-restart-server
     :description "Restart server")
    ("L" "View server logs" elfeed-summary-db-view-logs
     :description "View logs")
    ("W" "Open web interface" elfeed-summary-db-open-web-interface
     :description "Open web UI")
    ("X" "Clear database" elfeed-summary-db-clear-database
     :description "Clear database (destructive!)")]])

;;;###autoload
(defun elfeed-summary-db--semantic-search-dispatch ()
  "Dispatch to ivy or standard semantic search based on availability."
  (interactive)
  (if (fboundp 'ivy-read)
      (call-interactively 'elfeed-summary-db-semantic-search-ivy)
    (call-interactively 'elfeed-summary-db-semantic-search)))



;;;###autoload
(defun elfeed-summary-db-index-current-entry ()
  "Manually index the current entry."
  (interactive)
  (condition-case err
      (let ((entry (my-feed/get-current-entry)))
        (message "begin Indexing %s..." (elfeed-entry-title entry))
        (elfeed-summary-db-index-entry-async entry)
        (message "Indexing %s..." (elfeed-entry-title entry)))
    (error
     (message "Cannot index entry: %s" (error-message-string err)))))

;;;###autoload
(defun elfeed-summary-db-index-all-entries ()
  "Index all entries in the Elfeed database."
  (interactive)

  (let ((count 0))
    (maphash
     (lambda (_id entry)
       (setq count (1+ count))
       (elfeed-summary-db-index-entry-async entry))
     elfeed-db-entries)
    (message "Indexing %d entries..." count)))


;;;###autoload
(defun elfeed-summary-db-server-status ()
  "Show server status."
  (interactive)
  (elfeed-summary-db-ensure-server)
  (if (elfeed-summary-db-server-running-p)
      (message "elfeed-summary-db server is running on %s" (elfeed-summary-db-server-url))
    (message "elfeed-summary-db server is not running")))

;;;###autoload
(defun elfeed-summary-db-restart-server ()
  "Restart the elfeed-summary-db server."
  (interactive)
  (when (yes-or-no-p "Restart elfeed-summary-db server? ")
    (elfeed-summary-db-stop-server)
    (sleep-for 1)
    (elfeed-summary-db-start-server)
    (message "Server restarted")))

;;;###autoload
(defun elfeed-summary-db-view-logs ()
  "View the server log file."
  (interactive)
  (let ((log-file "/tmp/elfeed-summary-db-server.log"))
    (if (file-exists-p log-file)
        (progn
          (find-file-other-window log-file)
          (goto-char (point-max))
          (auto-revert-tail-mode 1)
          (message "Viewing server logs from %s" log-file))
      (message "Server log file not found: %s" log-file))))

;;;###autoload
(defun elfeed-summary-db-open-web-interface ()
  "Open the elfeed-summary-db server web interface in a browser."
  (interactive)
  (elfeed-summary-db-ensure-server)
  (if (elfeed-summary-db-server-running-p)
      (progn
        (browse-url (elfeed-summary-db-server-url))
        (message "Opening elfeed-summary-db web interface at %s" (elfeed-summary-db-server-url)))
    (message "elfeed-summary-db server is not running")))

;; Make elfeed-summary-db-menu available as M-x elfeed-summary-db
;;;###autoload
(defalias 'elfeed-summary-db 'elfeed-summary-db-menu)

(provide 'elfeed-summary-db-ui)
;;; elfeed-summary-db-ui.el ends here
