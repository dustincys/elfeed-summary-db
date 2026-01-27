;;; elfeed-summary-db.el --- Elfeed summary db with semantic search -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yanshuo Chu
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (plz "0.7") (compat "29.1"))
;; Keywords: elfeed, database, search
;; URL: https://github.com/dustincys/elfeed-summary-db

;;; Commentary:

;; elfeed-summary-db provides semantic search
;; capabilities for elfeed entries using a Python FastAPI backend.

;;; Code:

(require 'elfeed)
(require 'elfeed-score)
(require 'plz)
(require 'transient)

(defgroup elfeed-summary-db nil
  "Elfeed summary db with semantic search."
  :group 'elfeed
  :prefix "elfeed-summary-db-")

(defcustom elfeed-summary-db-server-host "127.0.0.1"
  "Host for elfeed summary db server."
  :type 'string
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-server-port 8875
  "Port for elfeed summary db server."
  :type 'integer
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-auto-start-server t
  "Whether to auto-start the server if not running."
  :type 'boolean
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-auto-enable t
  "Whether to automatically enable elfeed-summary-db when loading the package.
When t, auto-indexing on save will be enabled for elfeed entries."
  :type 'boolean
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-debug nil
  "Enable debug messages for elfeed-summary-db auto-indexing."
  :type 'boolean
  :group 'elfeed-summary-db)

(defvar elfeed-summary-db-server-process nil
  "Process running the elfeed summary db server.")

(defvar elfeed-summary-db-server-starting nil
  "Non-nil when server is currently starting up.
Used to prevent concurrent start attempts.")

(defun elfeed-summary-db-server-url ()
  "Return the base URL for the elfeed summary db server."
  (format "http://%s:%d" elfeed-summary-db-server-host elfeed-summary-db-server-port))

(defun elfeed-summary-db-server-running-p ()
  "Check if the elfeed summary db server is running.
Returns t if server responds to health check, nil otherwise.
Uses a 3-second timeout to tolerate busy servers during heavy indexing."
  (condition-case err
      (let ((url-request-method "GET")
            (url-request-extra-headers nil)
            ;; Set connection timeout to 3 seconds (increased from 1 to tolerate busy servers)
            (url-http-attempt-keepalives nil))
        (with-timeout (3 nil)  ; 3 second timeout
          (with-current-buffer
              (url-retrieve-synchronously
               (concat (elfeed-summary-db-server-url) "/health")
               t nil 3)  ; 3 second read timeout
            (goto-char (point-min))
            (let ((status-line (buffer-substring-no-properties
                                (point) (line-end-position))))
              (when elfeed-summary-db-debug
                (message "elfeed-summary-db: Health check status: %s" status-line))
              (prog1
                  (string-match-p "200 OK" status-line)
                (kill-buffer))))))
    (error
     (when elfeed-summary-db-debug
       (message "elfeed-summary-db: Server check failed: %S" err))
     nil)))

;; Load additional modules
(require 'elfeed-summary-db-parse)
(require 'elfeed-summary-db-client)
(require 'elfeed-summary-db-server)
(require 'elfeed-summary-db-search)
(require 'elfeed-summary-db-ui)

(defun elfeed-summary-db-trigger-after-save (entry _text)
  "Advice to trigger indexing after the summary is saved to ENTRY.
_TEXT is ignored because we pull the data directly from the ENTRY object."
  (message "Summary saved for %s. Triggering async vectorization..."
           (elfeed-entry-title entry))
  ;; Pass the entry ID to your indexing function
  (elfeed-summary-db-index-entry-async entry))

(defun elfeed-summary-db-enable ()
  "Enable elfeed-summary-db.
Adds org-mode-hook for future org files and enables auto-indexing
for all currently open org buffers."
  (interactive)
  (advice-add 'my-feed/elfeed-save-summary :after #'elfeed-summary-db-trigger-after-save))

(defun elfeed-summary-db-disable ()
  "Disable elfeed-summary-db.
Removes org-mode-hook and after-save-hooks from all org buffers."
  (interactive)
  (advice-remove 'my-feed/elfeed-save-summary #'elfeed-summary-db-trigger-after-save))

;; Auto-enable if configured
(when elfeed-summary-db-auto-enable
  (elfeed-summary-db-enable))

(provide 'elfeed-summary-db)
;;; elfeed-summary-db.el ends here
