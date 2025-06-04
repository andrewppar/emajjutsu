;;; emajjutsu-log.el -- create a log view like jj log -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; View jujutsu logs

;;; Code:
(require 'subr-x)
(require 'emajjutsu-display)
(require 'emajjutsu-core)

(define-derived-mode emajjutsu/log-mode fundamental-mode
  "View jujutsu repo status."
  "Major mode for viewing jujutsu status."
  (define-key emajjutsu/log-mode-map
      (kbd "C-c q") (lambda () (interactive) (kill-buffer (current-buffer)))))

(defmacro emajjutsu-log--with-buffer (&rest body)
  "Rewrite the log buffer with BODY."
  `(unwind-protect
	(progn
	  (switch-to-buffer "*emajjutsu log*")
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (emajjutsu/log-mode)
	    (progn ,@body)))
     (read-only-mode 1)))

(defconst emajjutsu-log--current-limit nil
  "The limit that was passed to emajjutsu-log/log.")

(defun emajjutsu-log/log (limit)
  "Create a buffer that displays the jj log with LIMIT.
If LIMIT is NIL it is treated as though there is none."
  (interactive)
  (setq emajjutsu-log--current-limit limit)
  (emajjutsu-log--with-buffer
   (insert
    (string-join
     (list
      (format "Directory: %s" default-directory)
      ""
      (thread-last
	(emajjutsu-core/log-tree limit)
	(string-replace (regexp-quote "@") (propertize "@" 'face emajjutsu-face/current))
	(string-replace (regexp-quote "×") (propertize "×" 'face emajjutsu-face/conflict))
	(string-replace (regexp-quote "◆") (propertize "◆" 'face emajjutsu-face/immutable))
	(seq-reduce
	 (lambda (acc change-spec)
	   (let ((change-id (plist-get change-spec :change-id)))
	     (replace-regexp-in-string
	      (regexp-quote change-id)
	      (emajjutsu-display/change change-spec :compact? t)
	      acc)))
	 (emajjutsu-core/log-changes nil))))
     "\n"))
   (goto-char (point-min))))

(defun emajjutsu-log/refresh-buffer ()
  "Refresh the log buffer."
  (emajjutsu-log/log emajjutsu-log--current-limit))

(defun emajjutsu-log/change-at-point ()
  "Get the change at point if it exists."
  (let ((line (thread-last
		(buffer-substring-no-properties
		 (line-beginning-position) (line-end-position))
		(replace-regexp-in-string (regexp-quote "│") "")
		string-trim)))
    (when (or (string-prefix-p "@" line)
	      (string-prefix-p "◆" line)
	      (string-prefix-p "○" line))
      (cadr (split-string line " " t " ")))))



(provide 'emajjutsu-log)
;;; emajjutsu-log.el ends here
