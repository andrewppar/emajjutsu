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
(require 'emajjutsu-display)
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

(defun emajjutsu--read-limit ()
  "Read a limit from the user."
  (let* ((limit (string-to-number (read-string "limit: "))))
    (unless (= limit 0)
      limit)))

;;;###autoload
(defun emajjutsu/log (limit)
  "View the log for @.
Optionaly specify LIMIT."
  (interactive
   (list (emajjutsu--read-limit)))
  (emajjutsu-log/log limit)))

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

;;;###autoload
(defun emajjutsu/new ()
  "Add a new change.
If point is on a change in an emajjutsu buffer, use that as the base.
Otherwise prompt for a base.  Prompt for a change to put the new change before,
the empty string is treated as none."
  (interactive)
  (let ((source-change-id (or (emajjutsu--change-id-at-point)
			      (emajjutsu-display/change-selection)))
	(before-change-id (when (y-or-n-p "insert before? ")
			    (emajjutsu-display/change-selection "Insert before: "))))
    (emajjutsu--with-buffer-refresh
     (if before-change-id
	 (emajjutsu-core/new source-change-id before)
       (emajjutsu-core/new source-change-id)))))

;;;###autoload
(defun emajjutsu/describe ()
  "Describe the change at point, or specify the change."
  (interactive)
  (let ((change-id (or (emajjutsu--change-id-at-point)
		       (emajjutsu-display/change-selection)))
	(description (read-string "Describe change: ")))
    (emajjutsu--with-buffer-refresh
     (emajjutsu-core/describe change-id description))))

;;;###autoload
(defun emajjutsu/bookmark-set ()
  "Move bookmark to the change at point.

If the bookmark does not exist, create it."
  ())

(provide 'emajjutsu)
;;; emajjutsu.el ends here
