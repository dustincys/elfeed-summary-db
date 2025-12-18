;;; elfeed-summary-db-server.el --- Server management -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to start, stop, and check the elfeed summary db server.

;;; Code:

(require 'ansi-color)

;; (require 'elfeed-summary-db)

(defcustom elfeed-summary-db-python-command "uv"
  "Command to run Python (uv, python3, etc)."
  :type 'string
  :group 'elfeed-summary-db)

(defcustom elfeed-summary-db-server-directory nil
  "Directory containing the Python server code.
If nil, automatically detected relative to the elisp directory."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Custom directory"))
  :group 'elfeed-summary-db)

(defun elfeed-summary-db--get-server-directory ()
  "Get the server directory, auto-detecting if not set."
  (or elfeed-summary-db-server-directory
      (let* ((elisp-dir (file-name-directory (locate-library "elfeed-summary-db-server")))
             (project-root (file-name-directory (directory-file-name elisp-dir))))
        (expand-file-name "python" project-root))))

(defun elfeed-summary-db--port-in-use-p (port)
  "Check if PORT is already in use by another process."
  (let ((result (shell-command-to-string
                 (format "lsof -i :%d -sTCP:LISTEN 2>/dev/null || netstat -an 2>/dev/null | grep ':%d.*LISTEN'"
                         port port))))
    (not (string-empty-p (string-trim result)))))

(defun elfeed-summary-db-kill-zombie-processes ()
  "Kill any zombie elfeed summary db server processes on the configured port.
Returns t if any processes were killed, nil otherwise.
IMPORTANT: Only kills processes that are NOT managed by Emacs (not elfeed-summary-db-server-process)."
  (interactive)
  (let* ((port elfeed-summary-db-server-port)
         (lsof-output (shell-command-to-string
                       (format "lsof -i :%d -sTCP:LISTEN 2>/dev/null | tail -n +2 | awk '{print $2}'"
                               port)))
         (pids (split-string (string-trim lsof-output) "\n" t))
         ;; Get the PID of the Emacs-managed server process
         (emacs-server-pid (when (and elfeed-summary-db-server-process
                                      (process-live-p elfeed-summary-db-server-process))
                             (number-to-string (process-id elfeed-summary-db-server-process))))
         (zombie-pids (if emacs-server-pid
                          (seq-remove (lambda (pid) (string= pid emacs-server-pid)) pids)
                        pids)))
    (when zombie-pids
      (message "Found %d zombie process(es) on port %d (excluding Emacs-managed server), attempting to kill..."
               (length zombie-pids) port)
      (dolist (pid zombie-pids)
        (message "Killing zombie process %s..." pid)
        (shell-command (format "kill -9 %s 2>/dev/null" pid)))
      (sleep-for 0.5)
      (message "Zombie processes cleaned up")
      t)
    (when (and pids (not zombie-pids))
      (message "Port %d is in use by Emacs-managed server (PID %s), not killing it"
               port emacs-server-pid)
      nil)))

(defun elfeed-summary-db-start-server ()
  "Start the elfeed summary db server.
Includes protection against concurrent starts and port conflicts."
  (interactive)
  (cond
   ;; Already running
   ((elfeed-summary-db-server-running-p)
    (message "elfeed summary db server already running on %s:%d"
             elfeed-summary-db-server-host elfeed-summary-db-server-port))

   ;; Currently starting (prevent race condition)
   (elfeed-summary-db-server-starting
    (message "elfeed summary db server is already starting, please wait..."))

   ;; Port already in use by another process
   ((elfeed-summary-db--port-in-use-p elfeed-summary-db-server-port)
    (if (yes-or-no-p (format "Port %d already in use. Kill zombie processes and restart? "
                             elfeed-summary-db-server-port))
        (progn
          (elfeed-summary-db-kill-zombie-processes)
          (elfeed-summary-db-start-server))
      (message "Port %d already in use. Server may already be running outside Emacs."
               elfeed-summary-db-server-port)))

   ;; Safe to start
   (t
    (setq elfeed-summary-db-server-starting t)
    (unwind-protect
        (let* ((default-directory (elfeed-summary-db--get-server-directory))
               (process-name "elfeed-summary-db-server")
               (buffer-name "*elfeed-summary-db-server*")
               (cmd (list elfeed-summary-db-python-command "run" "uvicorn"
                          "elfeed_summary_db_server.main:app" "--reload"
                          "--host" elfeed-summary-db-server-host
                          "--port" (number-to-string elfeed-summary-db-server-port))))

          (setq elfeed-summary-db-server-process
                (make-process
                 :name process-name
                 :buffer buffer-name
                 :command cmd
                 :filter #'elfeed-summary-db-server-filter
                 :sentinel #'elfeed-summary-db-server-sentinel))

          ;; Wait for server to start with retry logic (with feedback)
          (message "Starting elfeed summary db server on %s:%d..."
                   elfeed-summary-db-server-host elfeed-summary-db-server-port)

          ;; Poll for server readiness with exponential backoff
          ;; Total max wait: 1 + 2 + 3 + 4 + 5 = 15 seconds
          (let ((retries 5)
                (retry-delays '(1 2 3 4 5))
                (started nil))
            (while (and retry-delays (not started))
              (sleep-for (car retry-delays))
              (setq retry-delays (cdr retry-delays))
              (when (elfeed-summary-db-server-running-p)
                (setq started t)))

            (if started
                (message "elfeed summary db server started on %s:%d"
                         elfeed-summary-db-server-host elfeed-summary-db-server-port)
              ;; Check if port conflict after failed start
              (with-current-buffer buffer-name
                (goto-char (point-min))
                (if (re-search-forward "Address already in use" nil t)
                    (message "Failed to start: port %d already in use" elfeed-summary-db-server-port)
                  (pop-to-buffer buffer-name)
                  (goto-char (point-max))
                  (error "Failed to start elfeed summary db server. See buffer for details"))))))
      ;; Always clear the starting flag
      (setq elfeed-summary-db-server-starting nil)))))

(defun elfeed-summary-db-stop-server ()
  "Stop the elfeed summary db server."
  (interactive)
  (when (and elfeed-summary-db-server-process
             (process-live-p elfeed-summary-db-server-process))
    ;; First try graceful shutdown via API
    (condition-case err
        (let ((url-request-method "POST")
              (url-request-extra-headers '(("Content-Type" . "application/json"))))
          (with-timeout (2 nil)
            (url-retrieve-synchronously
             (concat (elfeed-summary-db-server-url) "/api/shutdown")
             t nil 2)))
      (error
       (when elfeed-summary-db-debug
         (message "elfeed-summary-db: Graceful shutdown failed: %S" err))))

    ;; Give it a moment to shut down gracefully
    (sleep-for 0.5)

    ;; If still running, force kill
    (when (process-live-p elfeed-summary-db-server-process)
      (kill-process elfeed-summary-db-server-process))

    (setq elfeed-summary-db-server-process nil
          elfeed-summary-db-server-starting nil)
    (message "elfeed summary db server stopped")))

(defun elfeed-summary-db-server-filter (process output)
  "Filter PROCESS OUTPUT to colorize ANSI escape codes."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; Insert the text, advancing the process marker
          (goto-char (process-mark process))
          (let ((start (point)))
            (insert output)
            (ansi-color-apply-on-region start (point)))
          (set-marker (process-mark process) (point)))
        ;; If point was at end, keep it at end
        (when moving
          (goto-char (process-mark process)))))))

(defun elfeed-summary-db-server-sentinel (process event)
  "Sentinel for server PROCESS EVENT."
  (cond
   ;; Normal termination (e.g., user called elfeed-summary-db-stop-server)
   ((string-match-p "finished" event)
    (message "elfeed summary db server stopped normally")
    (setq elfeed-summary-db-server-process nil
          elfeed-summary-db-server-starting nil))

   ;; Abnormal exit during startup
   ((and (string-match-p "\\(exited\\|failed\\)" event)
         elfeed-summary-db-server-starting)
    (message "elfeed summary db server failed to start: %s" (string-trim event))
    (setq elfeed-summary-db-server-process nil
          elfeed-summary-db-server-starting nil)
    (with-current-buffer (process-buffer process)
      (pop-to-buffer (current-buffer))
      (goto-char (point-max))))

   ;; Abnormal exit after successful startup (crash)
   ((string-match-p "\\(exited\\|failed\\)" event)
    (message "elfeed summary db server crashed: %s" (string-trim event))
    (setq elfeed-summary-db-server-process nil
          elfeed-summary-db-server-starting nil))))

(defun elfeed-summary-db-ensure-server ()
  "Ensure server is running, start if needed.
Automatically cleans up zombie processes without prompting when auto-starting."
  (unless (elfeed-summary-db-server-running-p)
    (when elfeed-summary-db-auto-start-server
      ;; Check if we have a tracked server process
      (if (and elfeed-summary-db-server-process
               (process-live-p elfeed-summary-db-server-process))
          ;; We have a live process, but health check failed
          ;; This is likely because the server is busy, not dead
          (when elfeed-summary-db-debug
            (message "elfeed-summary-db: Server process alive but health check failed (likely busy)"))
        ;; No tracked process, or it's dead
        (when (elfeed-summary-db--port-in-use-p elfeed-summary-db-server-port)
          ;; Port in use but no tracked process = zombie
          (message "Cleaning up zombie processes on port %d..." elfeed-summary-db-server-port)
          (elfeed-summary-db-kill-zombie-processes))
        (elfeed-summary-db-start-server)))))

(defun elfeed-summary-db-colorize-server-buffer ()
  "Colorize ANSI escape codes in the *elfeed-summary-db-server* buffer.
Useful for colorizing output that was already in the buffer before
the filter was added."
  (interactive)
  (when-let ((buffer (get-buffer "*elfeed-summary-db-server*")))
    (with-current-buffer buffer
      (ansi-color-apply-on-region (point-min) (point-max))
      (message "Colorized *elfeed-summary-db-server* buffer"))))

(defun elfeed-summary-db-kill-emacs-hook ()
  "Hook function to cleanly shut down the elfeed summary db server when Emacs exits.
This ensures the database connection is closed properly."
  (when (and elfeed-summary-db-server-process
             (process-live-p elfeed-summary-db-server-process))
    (message "Shutting down elfeed summary db server...")
    (elfeed-summary-db-stop-server)))

;; Register the kill-emacs-hook to ensure clean shutdown
(add-hook 'kill-emacs-hook #'elfeed-summary-db-kill-emacs-hook)

(provide 'elfeed-summary-db-server)
;;; elfeed-summary-db-server.el ends here
