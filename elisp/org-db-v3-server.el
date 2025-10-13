;;; org-db-v3-server.el --- Server management -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to start, stop, and check the org-db server.

;;; Code:

;; (require 'org-db-v3)

(defcustom org-db-v3-python-command "uv"
  "Command to run Python (uv, python3, etc)."
  :type 'string
  :group 'org-db-v3)

(defcustom org-db-v3-server-directory nil
  "Directory containing the Python server code.
If nil, automatically detected relative to the elisp directory."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Custom directory"))
  :group 'org-db-v3)

(defun org-db-v3--get-server-directory ()
  "Get the server directory, auto-detecting if not set."
  (or org-db-v3-server-directory
      (let* ((elisp-dir (file-name-directory (locate-library "org-db-v3-server")))
             (project-root (file-name-directory (directory-file-name elisp-dir))))
        (expand-file-name "python" project-root))))

(defun org-db-v3--port-in-use-p (port)
  "Check if PORT is already in use by another process."
  (let ((result (shell-command-to-string
                 (format "lsof -i :%d -sTCP:LISTEN 2>/dev/null || netstat -an 2>/dev/null | grep ':%d.*LISTEN'"
                         port port))))
    (not (string-empty-p (string-trim result)))))

(defun org-db-v3-kill-zombie-processes ()
  "Kill any zombie org-db server processes on the configured port.
Returns t if any processes were killed, nil otherwise."
  (interactive)
  (let* ((port org-db-v3-server-port)
         (lsof-output (shell-command-to-string
                       (format "lsof -i :%d -sTCP:LISTEN 2>/dev/null | tail -n +2 | awk '{print $2}'"
                               port)))
         (pids (split-string (string-trim lsof-output) "\n" t)))
    (when pids
      (message "Found %d process(es) on port %d, attempting to kill..." (length pids) port)
      (dolist (pid pids)
        (message "Killing process %s..." pid)
        (shell-command (format "kill -9 %s 2>/dev/null" pid)))
      (sleep-for 0.5)
      (message "Zombie processes cleaned up")
      t)))

(defun org-db-v3-start-server ()
  "Start the org-db server.
Includes protection against concurrent starts and port conflicts."
  (interactive)
  (cond
   ;; Already running
   ((org-db-v3-server-running-p)
    (message "org-db server already running on %s:%d"
             org-db-v3-server-host org-db-v3-server-port))

   ;; Currently starting (prevent race condition)
   (org-db-v3-server-starting
    (message "org-db server is already starting, please wait..."))

   ;; Port already in use by another process
   ((org-db-v3--port-in-use-p org-db-v3-server-port)
    (if (yes-or-no-p (format "Port %d already in use. Kill zombie processes and restart? "
                             org-db-v3-server-port))
        (progn
          (org-db-v3-kill-zombie-processes)
          (org-db-v3-start-server))
      (message "Port %d already in use. Server may already be running outside Emacs."
               org-db-v3-server-port)))

   ;; Safe to start
   (t
    (setq org-db-v3-server-starting t)
    (unwind-protect
        (let* ((default-directory (org-db-v3--get-server-directory))
               (process-name "org-db-server")
               (buffer-name "*org-db-server*")
               (cmd (list org-db-v3-python-command "run" "uvicorn"
                         "org_db_server.main:app" "--reload"
                         "--host" org-db-v3-server-host
                         "--port" (number-to-string org-db-v3-server-port))))

          (setq org-db-v3-server-process
                (make-process
                 :name process-name
                 :buffer buffer-name
                 :command cmd
                 :sentinel #'org-db-v3-server-sentinel))

          ;; Wait for server to start (with feedback)
          (message "Starting org-db server on %s:%d..."
                   org-db-v3-server-host org-db-v3-server-port)
          (sleep-for 2)

          (if (org-db-v3-server-running-p)
              (message "org-db server started on %s:%d"
                       org-db-v3-server-host org-db-v3-server-port)
            ;; Check if port conflict after failed start
            (with-current-buffer buffer-name
              (goto-char (point-min))
              (if (re-search-forward "Address already in use" nil t)
                  (message "Failed to start: port %d already in use" org-db-v3-server-port)
                (pop-to-buffer buffer-name)
                (goto-char (point-max))
                (error "Failed to start org-db server. See buffer for details")))))
      ;; Always clear the starting flag
      (setq org-db-v3-server-starting nil)))))

(defun org-db-v3-stop-server ()
  "Stop the org-db server."
  (interactive)
  (when (and org-db-v3-server-process
             (process-live-p org-db-v3-server-process))
    (kill-process org-db-v3-server-process)
    (setq org-db-v3-server-process nil
          org-db-v3-server-starting nil)
    (message "org-db server stopped")))

(defun org-db-v3-server-sentinel (process event)
  "Sentinel for server PROCESS EVENT."
  (when (string-match-p "\\(finished\\|exited\\)" event)
    (message "org-db server process %s" event)
    (setq org-db-v3-server-process nil
          org-db-v3-server-starting nil)))

(defun org-db-v3-ensure-server ()
  "Ensure server is running, start if needed.
Automatically cleans up zombie processes without prompting when auto-starting."
  (unless (org-db-v3-server-running-p)
    (when org-db-v3-auto-start-server
      ;; If port is in use but server isn't responding, kill zombies automatically
      (when (org-db-v3--port-in-use-p org-db-v3-server-port)
        (message "Cleaning up zombie processes on port %d..." org-db-v3-server-port)
        (org-db-v3-kill-zombie-processes))
      (org-db-v3-start-server))))

(provide 'org-db-v3-server)
;;; org-db-v3-server.el ends here
