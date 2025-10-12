;;; org-db-v3-client.el --- HTTP client for org-db -*- lexical-binding: t; -*-

;;; Commentary:
;; Async HTTP client using plz.el to communicate with the server.

;;; Code:

(require 'plz)
(require 'json)
;; (require 'org-db-v3)
(require 'org-db-v3-parse)
(require 'org-db-v3-server)

(defvar org-db-v3-index-queue nil
  "Queue of files pending indexing.")

(defvar org-db-v3-indexing-in-progress nil
  "Whether indexing is currently in progress.")

(defun org-db-v3-add-to-queue (filename)
  "Add FILENAME to indexing queue."
  (unless (member filename org-db-v3-index-queue)
    (push filename org-db-v3-index-queue)))

(defun org-db-v3-index-file-async (filename)
  "Index FILENAME asynchronously."
  (org-db-v3-ensure-server)

  (with-current-buffer (find-file-noselect filename)
    (let ((json-data (org-db-v3-parse-buffer-to-json)))
      (plz 'post (concat (org-db-v3-server-url) "/api/index/file")
        :headers '(("Content-Type" . "application/json"))
        :body json-data
        :as #'json-read
        :then (lambda (response)
                (message "Indexed %s (%d headlines)"
                         filename
                         (alist-get 'headlines_count response))
                ;; Mark indexing as complete and process next item
                (setq org-db-v3-indexing-in-progress nil)
                (org-db-v3-process-queue))
        :else (lambda (error)
                (message "Error indexing %s: %s" filename error)
                ;; Mark indexing as complete and process next item
                (setq org-db-v3-indexing-in-progress nil)
                (org-db-v3-process-queue))))))

(defun org-db-v3-process-queue ()
  "Process the indexing queue."
  (when (and org-db-v3-index-queue
             (not org-db-v3-indexing-in-progress))
    (setq org-db-v3-indexing-in-progress t)
    (let ((filename (pop org-db-v3-index-queue)))
      (if (file-exists-p filename)
          (org-db-v3-index-file-async filename)
        ;; If file doesn't exist, mark as complete and continue
        (setq org-db-v3-indexing-in-progress nil)
        (org-db-v3-process-queue)))))

(defvar org-db-v3-idle-timer nil
  "Timer for processing the queue.")

(defun org-db-v3-start-queue-processing ()
  "Start the timer for queue processing."
  (unless org-db-v3-idle-timer
    (setq org-db-v3-idle-timer
          (run-with-timer 1 2 #'org-db-v3-process-queue))))

(defun org-db-v3-stop-queue-processing ()
  "Stop the idle timer."
  (when org-db-v3-idle-timer
    (cancel-timer org-db-v3-idle-timer)
    (setq org-db-v3-idle-timer nil)))

;;;###autoload
(defun org-db-v3-index-directory (directory)
  "Recursively index all org files in DIRECTORY."
  (interactive "DDirectory to index: ")
  (let* ((org-files (directory-files-recursively
                     directory
                     "\\.org\\(\\.gpg\\)?\\'"
                     nil
                     (lambda (dir)
                       ;; Skip hidden directories and common ignore patterns
                       (not (string-match-p "/\\." (file-name-nondirectory dir))))))
         (count (length org-files)))
    (if (zerop count)
        (message "No org files found in %s" directory)
      (when (yes-or-no-p (format "Index %d org file%s in %s? "
                                 count
                                 (if (= count 1) "" "s")
                                 directory))
        (dolist (file org-files)
          (org-db-v3-add-to-queue file))
        (message "Added %d org file%s to indexing queue"
                 count
                 (if (= count 1) "" "s"))
        ;; Start processing immediately
        (org-db-v3-process-queue)))))

;;;###autoload
(defun org-db-v3-queue-status ()
  "Show the current indexing queue status."
  (interactive)
  (message "Queue: %d files pending, %s"
           (length org-db-v3-index-queue)
           (if org-db-v3-indexing-in-progress
               "indexing in progress"
             "idle")))

(provide 'org-db-v3-client)
;;; org-db-v3-client.el ends here
