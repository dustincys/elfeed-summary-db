;;; org-db-v3-client.el --- HTTP client for org-db -*- lexical-binding: t; -*-

;;; Commentary:
;; Async HTTP client using plz.el to communicate with the server.

;;; Code:

(require 'plz)
(require 'json)
(require 'elfeed-summary-db-parse)
(require 'elfeed-summary-db-server)

;; Queue for non-blocking directory indexing
(defvar elfeed-summary-db-index-queue nil
  "Queue of files waiting to be indexed.")

(defvar elfeed-summary-db-index-timer nil
  "Timer for processing the index queue.")

(defvar elfeed-summary-db-index-total 0
  "Total number of files in current indexing operation.")

(defvar elfeed-summary-db-index-processed 0
  "Number of files processed in current indexing operation.")

(defcustom elfeed-summary-db-index-delay 0.5
  "Delay in seconds between indexing files.
Lower values = faster indexing but less responsive Emacs and higher server load.
Higher values = slower indexing but more responsive Emacs and prevents server overload.
Default 0.5s allows time for linked file processing to complete."
  :type 'number
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-index-timeout 240
  "Timeout in seconds for indexing a single file.
Files with many large linked files (PDFs, images) may take longer to process.
Default 240 seconds (4 minutes). Files that timeout will be skipped."
  :type 'number
  :group 'elfeed-summary-db)

(defvar elfeed-summary-db-index-failed-files nil
  "List of files that failed to index in the current operation.")

(defun elfeed-summary-db-index-entry-async (entry_id)
  "Index entry asynchronously by sending to server.
Disables local variables and hooks for safe and fast bulk indexing.
Skips Emacs temporary files (.#*, #*#, *~) and remote Tramp files."
  (elfeed-summary-db-ensure-server)

  ;; Skip entry without summary
  (when (elfeed-meta elfeed-show-entry :summary)
    (error "Skipping entry that has no summary: %s" (elfeed-entry-title entry)))

  (let ((json-data (elfeed-summary-db-parse-entry-to-json entry_id)))
    (plz 'post (concat (elfeed-summary-db-server-url) "/api/entry")
      :headers '(("Content-Type" . "application/json"))
      :body json-data
      :as #'json-read
      :timeout 120  ; Allow time for docling PDF/DOCX conversion
      :then (lambda (response)
              (let ((entry_id (alist-get 'entry_id response))
                    (status (alist-get 'statue response)))
                (message "Indexed %s, status %s" entry_id status)))
      :else (lambda (error)
              (message "Error indexing %s: %s" entry_id status))))
  )

(defun elfeed-summary-db-process-index-queue ()
  "Process one file from the index queue.
Waits for each request to complete before processing the next entry."
  (if elfeed-summary-db-index-queue
      (let ((entry_id (pop elfeed-summary-db-index-queue)))
        (setq elfeed-summary-db-index-processed (1+ elfeed-summary-db-index-processed))

        ;; Update progress in echo area
        (message "Indexing [%d/%d]: %s"
                 elfeed-summary-db-index-processed
                 elfeed-summary-db-index-total
                 (elfeed-entry-title (elfeed-db-get-entry entry_id)))

        (elfeed-summary-db-index-file-with-continuation entry_id))
    ;; Queue is empty, clean up
    (setq elfeed-summary-db-index-timer nil)
    (if elfeed-summary-db-index-failed-entries
        (message "Indexing complete: %d entries %s processed, %d failed (see *Messages* for details)"
                 elfeed-summary-db-index-total
                 (if (= elfeed-summary-db-index-total 1) "" "s")
                 (length elfeed-summary-db-index-failed-files))
      (message "Indexing complete: %d file%s processed"
               elfeed-summary-db-index-total
               (if (= elfeed-summary-db-index-total 1) "" "s")))))

(defun elfeed-summary-db-index-file-with-continuation (entry_id)
  "Index ENTRY_ID and continue processing queue on completion.
This ensures requests are processed sequentially, not in parallel.
Wraps processing in error handling to prevent queue stalls."
  ;; Wrap everything in condition-case to ensure queue continues even on unexpected errors
  (condition-case err
      (progn
        (elfeed-summary-db-ensure-server)

        ;; Skip entries with no summary

        (let* ((entry (elfeed-db-get-entry entry_id))
               (summary (elfeed-meta entry :summary))
               (title (elfeed-entry-title entry)))
          (if (not summary)
              (progn
                (message "Skipping entry that has no summary: %s" title)
                ;; Continue with next file in queue
                (run-with-timer elfeed-summary-db-index-delay nil #'elfeed-summary-db-process-index-queue))
            (let ((json-data (elfeed-summary-db-parse-buffer-to-json)))
              (plz 'post (concat (elfeed-summary-db-server-url) "/api/file")
                :headers '(("Content-Type" . "application/json"))
                :body json-data
                :as #'json-read
                :timeout elfeed-summary-db-index-timeout  ; Configurable timeout
                :then (lambda (response)
                        (message "Indexed %s, status %s" (alist-get 'entry_id response) (alist-get 'statue response))
                        ;; Process next file in queue after this one completes
                        (run-with-timer elfeed-summary-db-index-delay nil #'elfeed-summary-db-process-index-queue))
                :else (lambda (error)
                        ;; Track failed files
                        (push entry_id elfeed-summary-db-index-failed-entries)
                        ;; Check if it's a timeout
                        (if (and (listp error) (eq (car error) 28))
                            (message "Timeout indexing %s (exceeded %d seconds)"
                                     title
                                     elfeed-summary-db-index-timeout)
                          (message "Error indexing %s: %s"
                                   (file-name-nondirectory filename) error))
                        ;; Continue with next file even on error
                        (run-with-timer elfeed-summary-db-index-delay nil #'elfeed-summary-db-process-index-queue)))))))
    ;; Catch any unexpected errors and continue queue processing
    (error
     (message "Unexpected error processing %s: %S - continuing queue" entry_id err)
     (run-with-timer elfeed-summary-db-index-delay nil #'elfeed-summary-db-process-index-queue))))

;;;###autoload
(defun elfeed-summary-db-index-elfeed ()
  "Recursively index all entries in elfeed db.
Entries are processed one at a time using timers to keep Emacs responsive.
Skips entries with no summary."
  (interactive "Index whole elfeed")

  (when (yes-or-no-p "Index whole elfeed?")
    ;; Check if indexing is already in progress
    (when elfeed-summary-db-index-timer
      (message "Indexing already in progress, please wait or cancel first")
      (user-error "Indexing already in progress"))

    ;; 1. Collect all valid entries into a list
    (let ((all-entries '()))
      (with-elfeed-db-visit (entry _)
        (let ((summary (elfeed-meta entry :summary)))
          ;; Check your conditions (must have a summary)
          (when (and summary (stringp summary) (not (string-empty-p summary)))
            (push entry all-entries))))

      ;; 2. Reverse the list (push adds to front) and calculate count
      (setq all-entries (nreverse all-entries))

      (let ((count (length all-entries)))
        ;; 3. Set up the queue using the collected entries
        (setq elfeed-summary-db-index-queue all-entries
              elfeed-summary-db-index-total count
              elfeed-summary-db-index-processed 0
              elfeed-summary-db-index-failed-files nil) ; Reset failed list

        ;; 4. Start processing
        (setq elfeed-summary-db-index-timer t) ; Marker that indexing is active
        (run-with-timer 0 nil #'elfeed-summary-db-process-index-queue)

        (message "Starting sequential indexing of %d entr%s..."
                 count
                 (if (= count 1) "y" "ies"))))))

;;;###autoload
(defun elfeed-summary-db-reindex-database ()
  "Reindex all entries currently in the database.
Fetches the list of entries from the server and reindexes each one.
Uses non-blocking queue processing to keep Emacs responsive.
Skips entries with no summary."
  (interactive)
  (elfeed-summary-db-ensure-server)

  (plz 'get (concat (elfeed-summary-db-server-url) "/api/files")
    :as #'json-read
    :then (lambda (response)
            (let* ((entries (alist-get 'entry_id response))
                   (count (length entries))
                   (missing-entries nil)
                   (existing-entries nil)
                   (entries_with_no_summary nil))

              ;; Classify entries as existing, missing, or no summary
              (dotimes (i count)
                (let ((entry_id (elt entries i)))
                  (cond
                   ((not (summary (elfeed-meta (elfeed-db-get-entry entry_id) :summary)))
                    (push entry_id entries_with_no_summary))
                   ((elfeed-db-get-entry entry_id)
                    (push entry_id existing-entries))
                   (t
                    (push entry_id missing-entries)))))

              (if (zerop count)
                  (message "No files found in database")

                ;; Show summary and confirm
                (let ((msg (format "Reindex %d existing entr%s%s%s? "
                                   (length existing-entries)
                                   (if (= (length existing-entries) 1) "y" "ies")
                                   (if missing-entries
                                       (format ", remove %d missing file%s"
                                               (length missing-files)
                                               (if (= (length missing-files) 1) "" "s"))
                                     "")
                                   (if entries_with_no_summary
                                       (format ", skip %d entr%s"
                                               (length entries_with_no_summary)
                                               (if (= (length entries_with_no_summary) 1) "y" "ies"))
                                     ""))))
                  (when (yes-or-no-p msg)
                    ;; Delete missing files from database immediately
                    (when missing-entries
                      (message "Removing %d missing entr%s from database..."
                               (length missing-entries)
                               (if (= (length missing-entries) 1) "y" "ies"))
                      (dolist (entry_id missing-entries)
                        (elfeed-summary-db-delete-entry-async entry_id)))

                    ;; Remove remote files from database
                    (when entries_with_no_summary
                      (message "Removing %d remote entr%s from database..."
                               (length entries_with_no_summary)
                               (if (= (length entries_with_no_summary) 1) "y" "ies"))
                      (dolist (filename entries_with_no_summary)
                        (elfeed-summary-db-delete-entry-async filename)))

                    ;; Reindex existing entries using non-blocking queue
                    (when existing-entries
                      ;; Check if indexing is already in progress
                      (when elfeed-summary-db-index-timer
                        (message "Indexing already in progress, please wait or cancel first")
                        (user-error "Indexing already in progress"))

                      ;; Set up the queue
                      (setq elfeed-summary-db-index-queue existing-entries
                            elfeed-summary-db-index-total (length existing-entries)
                            elfeed-summary-db-index-processed 0
                            elfeed-summary-db-index-failed-files nil)  ; Reset failed files list

                      ;; Start processing first file (continuation handled in callback)
                      (setq elfeed-summary-db-index-timer t)  ; Marker that indexing is active
                      (run-with-timer 0 nil #'elfeed-summary-db-process-index-queue)

                      (message "Starting sequential reindex of %d entr%s..."
                               (length existing-entries)
                               (if (= (length existing-entries) 1) "y" "ies"))))))))
    :else (lambda (error)
            (message "Error fetching entry list: %s" (plz-error-message error)))))

(defun org-db-v3-delete-file-async (filename)
  "Delete FILENAME from the database asynchronously."
  (plz 'delete (concat (org-db-v3-server-url) "/api/file?filename=" (url-hexify-string filename))
    :as #'json-read
    :then (lambda (response)
            (message "Removed %s from database" filename))
    :else (lambda (error)
            (message "Error removing %s: %s" filename (plz-error-message error)))))

;;;###autoload
(defun org-db-v3-cancel-indexing ()
  "Cancel the current indexing operation."
  (interactive)
  (if org-db-v3-index-timer
      (progn
        (setq org-db-v3-index-timer nil)
        (message "Indexing cancelled: %d of %d files processed"
                 org-db-v3-index-processed
                 org-db-v3-index-total)
        (setq org-db-v3-index-queue nil
              org-db-v3-index-total 0
              org-db-v3-index-processed 0))
    (message "No indexing operation in progress")))

;;;###autoload
(defun org-db-v3-resume-indexing ()
  "Resume a stuck or paused indexing operation.
If indexing has stalled, this will restart processing the remaining queue."
  (interactive)
  (cond
   ;; No indexing operation at all
   ((not org-db-v3-index-timer)
    (message "No indexing operation to resume"))

   ;; Queue is empty but timer is set (shouldn't happen, but clean up)
   ((null org-db-v3-index-queue)
    (message "Indexing queue is empty, clearing status...")
    (setq org-db-v3-index-timer nil
          org-db-v3-index-total 0
          org-db-v3-index-processed 0))

   ;; Valid queue exists, resume processing
   (t
    (message "Resuming indexing: %d files remaining (processed %d of %d)"
             (length org-db-v3-index-queue)
             org-db-v3-index-processed
             org-db-v3-index-total)
    (run-with-timer 0 nil #'org-db-v3-process-index-queue))))

;;;###autoload
(defun org-db-v3-indexing-status ()
  "Show the status of the current indexing operation."
  (interactive)
  (if org-db-v3-index-timer
      (message "Indexing in progress: %d/%d files processed, %d remaining in queue%s"
               org-db-v3-index-processed
               org-db-v3-index-total
               (length org-db-v3-index-queue)
               (if org-db-v3-index-failed-files
                   (format ", %d failed" (length org-db-v3-index-failed-files))
                 ""))
    (message "No indexing operation in progress")))

;;;###autoload
(defun org-db-v3-show-failed-files ()
  "Show list of files that failed to index in the last operation."
  (interactive)
  (if org-db-v3-index-failed-files
      (with-output-to-temp-buffer "*org-db-v3-failed-files*"
        (princ (format "Failed to index %d file(s):\n\n" (length org-db-v3-index-failed-files)))
        (dolist (file org-db-v3-index-failed-files)
          (princ (format "  %s\n" file))))
    (message "No failed files in last indexing operation")))

;;;###autoload
(defun org-db-v3-cleanup-stale-processes ()
  "Clean up stale HTTP and curl processes from failed requests.
Only removes processes that are clearly failed or stuck.
Safe to run while indexing is in progress."
  (interactive)
  (let ((killed 0))
    (dolist (proc (process-list))
      (let ((name (process-name proc))
            (status (process-status proc)))
        (when (and (string-match-p "^127\\.0\\.0\\.1" name)
                   (eq status 'failed))
          ;; Only kill failed HTTP connection processes
          (delete-process proc)
          (setq killed (1+ killed)))))
    (if (> killed 0)
        (message "Cleaned up %d failed HTTP connection%s"
                 killed
                 (if (= killed 1) "" "s"))
      (message "No stale processes found"))))

;;;###autoload
(defun org-db-v3-clear-database ()
  "Clear the entire database by removing the database file.

WARNING: This is destructive and cannot be undone!
All indexed data will be permanently deleted.

Prompts for confirmation before proceeding."
  (interactive)
  (org-db-v3-ensure-server)

  (when (yes-or-no-p "Clear entire database? This cannot be undone! ")
    (plz 'delete (concat (org-db-v3-server-url) "/api/stats/clear-database")
      :as #'json-read
      :then (lambda (response)
              (let ((status (alist-get 'status response))
                    (message-text (alist-get 'message response))
                    (db-path (alist-get 'db_path response)))
                (if (string= status "success")
                    (message "Database cleared: %s" db-path)
                  (message "Error: %s" message-text))))
      :else (lambda (error)
              (message "Error clearing database: %s" (plz-error-message error))))))

(provide 'org-db-v3-client)
;;; org-db-v3-client.el ends here
