;;; elfeed-summary-db-parse.el --- Elfeed entry parsing to JSON -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to parse elfeed entry and convert to JSON for the server.

;;; Code:

(require 'json)
(require 'seq)  ; For seq-difference
(require 'elfeed)  ; For seq-difference

(defun elfeed-summary-db-parse-entry-to-json (entry_id)
  "Parse entry and return JSON string for server."
  (let* ((entry (elfeed-db-get-entry entry_id))
         (title (elfeed-entry-title entry))
         (summary (elfeed-meta elfeed-show-entry :summary))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (md5str (md5 (format "%S %S %S" title summary content)))
         (data `(("entry_id" . entry_id)
                 ("title" . title)
                 ("summary" . summary)
                 ("content" . content)
                 ("md5" . md5str))))
    (json-encode data)))

(provide 'elfeed-summary-db-parse)
;;; elfeed-summary-db-parse.el ends here
