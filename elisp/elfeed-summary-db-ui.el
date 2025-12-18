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
   - (directory . \"/path/to/dir/\") - files under directory
   - (project . \"/path/to/project/\") - files in project root
   - (tag . \"tag-name\") - files with specific keyword/tag
   Resets to (all . nil) after each search.")

(defun elfeed-summary-db--scope-description ()
  "Return current scope description for transient header."
  (pcase (car elfeed-summary-db-search-scope)
    ('all "All files")
    ('directory (format "Directory: %s"
                        (file-name-nondirectory
                         (directory-file-name (cdr elfeed-summary-db-search-scope)))))
    ('project (format "Project: %s"
                      (file-name-nondirectory
                       (directory-file-name (cdr elfeed-summary-db-search-scope)))))
    ('tag (format "Tag: %s" (cdr elfeed-summary-db-search-scope)))
    (_ "All files")))

(defun elfeed-summary-db--scope-to-params ()
  "Convert current scope to API filter parameters.
Returns plist with :filename_pattern and/or :keyword."
  (pcase (car elfeed-summary-db-search-scope)
    ('all nil)
    ('directory
     (list :filename_pattern (concat (cdr elfeed-summary-db-search-scope) "%")))
    ('project
     (list :filename_pattern (concat (cdr elfeed-summary-db-search-scope) "%")))
    ('tag
     (list :keyword (cdr elfeed-summary-db-search-scope)))
    (_ nil)))

;; Transient infix for all files scope
(transient-define-infix elfeed-summary-db--scope-all-infix ()
  "Set search scope to all files."
  :class 'transient-lisp-variable
  :variable 'elfeed-summary-db-search-scope
  :key "-a"
  :description "All files"
  :reader (lambda (&rest _) '(all . nil)))

;; Transient infix for directory scope
(transient-define-infix elfeed-summary-db--scope-directory-infix ()
  "Set search scope to a directory."
  :class 'transient-lisp-variable
  :variable 'elfeed-summary-db-search-scope
  :key "-d"
  :description "Directory"
  :reader (lambda (prompt _initial-input _history)
            (let ((dir (read-directory-name "Limit search to directory: ")))
              (when dir
                (cons 'directory (expand-file-name dir))))))

;; Transient infix for project scope
(transient-define-infix elfeed-summary-db--scope-project-infix ()
  "Set search scope to a Projectile project."
  :class 'transient-lisp-variable
  :variable 'elfeed-summary-db-search-scope
  :key "-p"
  :description "Project"
  :reader (lambda (prompt _initial-input _history)
            (if (and (fboundp 'projectile-completing-read)
                     (fboundp 'projectile-relevant-known-projects))
                (let* ((projects (projectile-relevant-known-projects))
                       (current-project (when (fboundp 'projectile-project-root)
                                          (projectile-project-root)))
                       (project (if projects
                                    (projectile-completing-read
                                     "Select project: "
                                     projects
                                     :initial-input current-project)
                                  current-project)))
                  (if project
                      (cons 'project project)
                    (prog1 nil
                      (message "No project selected. Scope unchanged."))))
              (prog1 nil
                (message "Projectile not available. Scope unchanged.")
                (ding)))))

;; Transient infix for tag scope
(transient-define-infix elfeed-summary-db--scope-tag-infix ()
  "Set search scope to files with a specific keyword/tag."
  :class 'transient-lisp-variable
  :variable 'elfeed-summary-db-search-scope
  :key "-t"
  :description "Tag/keyword"
  :reader (lambda (prompt _initial-input _history)
            (let ((tag (read-string "Limit search to keyword/tag: ")))
              (when (and tag (not (string-empty-p tag)))
                (cons 'tag tag)))))

;;;###autoload (autoload 'org-db-menu "elfeed-summary-db-ui" nil t)
(transient-define-prefix org-db-menu ()
  [:description (lambda () (format "elfeed-summary db [Scope: %s]" (elfeed-summary-db--scope-description)))
                "Search and manage your org files."]
  [["Scope"
    ("-a" elfeed-summary-db--scope-all-infix)
    ("-d" elfeed-summary-db--scope-directory-infix)
    ("-p" elfeed-summary-db--scope-project-infix)
    ("-t" elfeed-summary-db--scope-tag-infix)]
   ["Actions"
    ("q" "Quit" transient-quit-one)]]
  ["Search"
   ["Text Search"
    ("v" "Semantic search" elfeed-summary-db--semantic-search-dispatch
     :description (lambda () (if (fboundp 'ivy-read)
                                 "Vector embeddings (dynamic)"
                               "Vector embeddings")))
    ("k" "Full-text search" elfeed-summary-db--fulltext-search-dispatch
     :description (lambda () (if (fboundp 'ivy-read)
                                 "FTS5 keywords (dynamic)"
                               "FTS5 keywords")))
    ("h" "Headline search" elfeed-summary-db-headline-search
     :description "Browse headlines")
    ("P" "Property search" elfeed-summary-db-property-search
     :description "PROPERTY=VALUE")
    ("p" "Search at point" elfeed-summary-db-search-at-point
     :description "Text at point/region")]
   ["Image Search"
    ("i" "Search images" elfeed-summary-db--image-search-dispatch
     :description (lambda () (if (fboundp 'ivy-read)
                                 "CLIP embeddings (dynamic)"
                               "CLIP embeddings")))]
   ["Files"
    ("f" "Open file from db" elfeed-summary-db-open-file
     :description "Browse org files")
    ("F" "Open linked file" elfeed-summary-db-open-linked-file
     :description "Browse linked files")]]
  ["Agenda"
   ("a" "Show agenda" elfeed-summary-db-agenda
    :description "TODOs & deadlines")]
  ["Management"
   ["Indexing"
    ("u" "Update current file" elfeed-summary-db-update-current-file
     :description "Index current file")
    ("U" "Update all open files" elfeed-summary-db-update-all-buffers
     :description "Index all open buffers")
    ("d" "Index directory" elfeed-summary-db-index-directory
     :description "Index directory recursively")
    ("r" "Reindex database" elfeed-summary-db-reindex-database
     :description "Reindex all files")]
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
(defun elfeed-summary-db--fulltext-search-dispatch ()
  "Dispatch to ivy or standard fulltext search based on availability."
  (interactive)
  (if (fboundp 'ivy-read)
      (call-interactively 'elfeed-summary-db-fulltext-search-ivy)
    (call-interactively 'elfeed-summary-db-fulltext-search)))


;;;###autoload
(defun elfeed-summary-db-update-current-file ()
  "Manually update the current file."
  (interactive)
  (if (buffer-file-name)
      (progn
        (elfeed-summary-db-index-file-async (buffer-file-name))
        (message "Indexing %s..." (buffer-file-name)))
    (message "No file associated with current buffer")))

;;;###autoload
(defun elfeed-summary-db-update-all-buffers ()
  "Update all open org buffers."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-file-name)
                   (or (string-suffix-p ".org" (buffer-file-name))
                       (string-suffix-p ".org_archive" (buffer-file-name))))
          (elfeed-summary-db-index-file-async (buffer-file-name))
          (setq count (1+ count)))))
    (message "Sent %d org file%s to server for indexing"
             count
             (if (= count 1) "" "s"))))

;;;###autoload
(defun elfeed-summary-db-server-status ()
  "Show server status."
  (interactive)
  (elfeed-summary-db-ensure-server)
  (if (elfeed-summary-db-server-running-p)
      (message "org-db server is running on %s" (elfeed-summary-db-server-url))
    (message "org-db server is not running")))

;;;###autoload
(defun elfeed-summary-db-restart-server ()
  "Restart the org-db server."
  (interactive)
  (when (yes-or-no-p "Restart org-db server? ")
    (elfeed-summary-db-stop-server)
    (sleep-for 1)
    (elfeed-summary-db-start-server)
    (message "Server restarted")))

;;;###autoload
(defun elfeed-summary-db-view-logs ()
  "View the server log file."
  (interactive)
  (let ((log-file "/tmp/org-db-server.log"))
    (if (file-exists-p log-file)
        (progn
          (find-file-other-window log-file)
          (goto-char (point-max))
          (auto-revert-tail-mode 1)
          (message "Viewing server logs from %s" log-file))
      (message "Server log file not found: %s" log-file))))

;;;###autoload
(defun elfeed-summary-db-open-web-interface ()
  "Open the org-db server web interface in a browser."
  (interactive)
  (elfeed-summary-db-ensure-server)
  (if (elfeed-summary-db-server-running-p)
      (progn
        (browse-url (elfeed-summary-db-server-url))
        (message "Opening org-db web interface at %s" (elfeed-summary-db-server-url)))
    (message "org-db server is not running")))

;; Make org-db-menu available as M-x org-db
;;;###autoload
(defalias 'org-db 'org-db-menu)

(provide 'elfeed-summary-db-ui)
;;; elfeed-summary-db-ui.el ends here
