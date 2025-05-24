;;; emajjutsu.el -- jujutsu bindings for emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; Execute jj commands from within Emacs

;;; Code:
(require 'emajjutsu-status)
(require 'emajjutsu-log)

;;;###autoload
(defun emajjutsu/status (&optional change-id)
  "View the status of CHANGE-ID."
  (interactive
   (list
    (read-string "change: " "@" nil "@")))
  (let ((id (if (equal change-id "") "@" change-id)))
    (emajjutsu-status/status id)))

;;;###autoload
(defun emajjutsu/log ()
  "View the log for @."
  (interactive)
  (emajjutsu-log/log))

(defun emajjutsu--change-id-at-point ()
  "Get the change id at point if it exists."
  (cond ((equal major-mode 'emajjutsu/status-mode)
	 (emajjutsu-status/change-at-point))
	((equal major-mode 'emajjutsu/log-mode)
	 (emajjutsu-log/change-at-point))
	(t nil)))

(defmacro emajjutsu--with-buffer-refresh (&rest body)
  "Refresh buffer after executing BODY."
  `(progn
     (progn ,@body)
     (cond ((equal major-mode 'emajjutsu/status-mode)
	    (emajjutsu-status/status "@"))
	   ((equal major-mode 'emajjutsu/log-mode)
	    (emajjutsu-log/log))
	   (t nil))))

;;;###autoload
(defun emajjutsu/edit ()
  "Move the focused change."
  (interactive)
  (let ((change-id (or (emajjutsu--change-id-at-point)
		       (emajjutsu-display/change-selection))))
    (emajjutsu--with-buffer-refresh
     (emajjutsu-core/edit change-id))))
(provide 'emajjutsu)
;;; emajjutsu.el ends here
