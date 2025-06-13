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
  "View the status of CHANGE-ID.
If CHANGE-ID is not provided, default to '@'."
  (interactive
   (list
    (read-string "change: " "@" nil "@")))
  (let ((id (if (equal change-id "") "@" change-id)))
    (emajjutsu-status/status id)))

(defun emajjutsu--read-limit ()
  "Prompt the user to input a limit and return it as a number.
If the input is invalid or zero, return nil."
  (let* ((limit (string-to-number (read-string "limit: "))))
    (unless (= limit 0)
      limit)))

;;;###autoload
(defun emajjutsu/log (limit)
  "View the log for @.
Optionally specify LIMIT."
  (interactive
   (list (emajjutsu--read-limit)))
  (emajjutsu-log/log limit))

(defun emajjutsu--change-id-at-point ()
  "Get the change id at point if it exists.
Return nil if no change ID is found."
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
	    (emajjutsu-status/refresh-buffer))
	   ((equal major-mode 'emajjutsu/log-mode)
	    (emajjutsu-log/refresh-buffer))
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
If point is on a change in an emajjutsu buffer, use that as the parent.
Otherwise prompt for a parent."
  (interactive)
  (let ((source-change-id (or (emajjutsu--change-id-at-point)
			      (emajjutsu-display/change-selection))))
    (emajjutsu--with-buffer-refresh
     (emajjutsu-core/new source-change-id))))

;;;###autoload
(defun emajjutsu/describe ()
  "Describe the change at point, or specify the change."
  (interactive)
  (let ((change-id (or (emajjutsu--change-id-at-point)
		       (emajjutsu-display/change-selection)))
	(description (read-string "Describe change: ")))
    (emajjutsu--with-buffer-refresh
     (emajjutsu-core/describe change-id description))))

(defun emajjutsu/diff ()
  "Get the diff of the change at point.
Only for status."
  (interactive)
  (emajjutsu-status/diff))


(defun emajjutsu/bookmarks ()
  "List bookmarks in repo."
  )

(defun emajjutsu/bookmark-delete (bookmark)
  "Delete BOOKMARK."
  (interactive
   (list
    (completing-read
     "bookmarks: "
     (mapcar
      (lambda (bookmark) (plist-get bookmark :name))
      (emajjutsu-core/bookmark-list)))))
  (emajjutsu--with-buffer-refresh
   (when (y-or-n-p (format "delete bookmark: %s?" bookmark))
     (emajjutsu-core/bookmark-delete bookmark))))

;;;###autoload
(defun emajjutsu/bookmark-set ()
  "Move bookmark to the change at point.
If the bookmark does not exist, create it."
  (interactive)
  (let* ((bookmarks (mapcar
		     (lambda (bookmark) (plist-get bookmark :name))
		     (emajjutsu-core/bookmark-list)))
	 (bookmark (completing-read "bookmarks: " bookmarks))
	 (change-id (or (emajjutsu--change-id-at-point)
			(emajjutsu-display/change-selection))))
    (emajjutsu--with-buffer-refresh
     (if (member bookmark bookmarks)
	 (emajjutsu-core/bookmark-set bookmark change-id)
       (emajjutsu-core/bookmark-create bookmark change-id)))))

(defun emajjutsu/refresh-buffer ()
  "Refresh the data on the current buffer."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (message "refreshing...")))

(defun emajjutsu/rebase-source ()
  "Rebase the change at point onto a selected change as SOURCE."
  (interactive)
  (let* ((source-change (or (emajjutsu--change-id-at-point)
			    (emajjutsu-display/change-selection)))
	 (target-change (emajjutsu-display/change-selection
			 "rebase destination: "))
	 (location (pcase (read-char
			   (string-join
			    (list
			     "Rebase Options:"
			     "'a' - select change to insert after"
			     "'b' - select change to insert before"
			     "'n' or 'q' - abort rebase"
			     (format "otherwise - rebase from %s onto %s"
				     source-change target-change)
			     "")
			    "\n"))
		     (?b :before)
		     (?a :after)
		     (?q :quit)
		     (?n :quit)
		     (_ :destination))))

    (emajjutsu--with-buffer-refresh
     (unless (equal location :quit)
       (emajjutsu-core/rebase-source
	source-change target-change location)))))

(defun emajjutsu/log->status-at-point ()
  "From a log view get the status of a particular change."
  (interactive)
  (split-window-sensibly)
  (emajjutsu-status/status (emajjutsu-log/change-at-point)))

(defun emajjutsu/push ()
  "Push the current state of the repo to remote."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (emajjutsu-core/push)))

(defun emajjutsu/fetch ()
  "Fetch from remote."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (emajjutsu-core/fetch)))

(defun emajjutsu/split (description)
  "Perform a split with DESCRIPTION on new change.
The marked files in the buffer are moved to a new parallel commit.
A description is prompted from the user."
  (interactive (list (read-string "describe new change: ")))
  (emajjutsu--with-buffer-refresh
   (cond ((equal major-mode 'emajjutsu/status-mode)
	  (emajjutsu-status/split-marks description))
	 ((equal major-mode 'emajjutsu/log-mode)
	  (emajjutsu-log/split description))
	 (t nil))))

(defun emajjutsu/squash ()
  "Peform a squash.
The marked files in the buffer are squashed into a target change."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (let* ((target-change-id (emajjutsu-display/change-selection
			     "squash into change: ")))
     (cond ((equal major-mode 'emajjutsu/status-mode)
	  (emajjutsu-status/squash-marks target-change-id))
	 ((equal major-mode 'emajjutsu/log-mode)
	  (emajjutsu-log/squash target-change-id))
	 (t nil)))))



(provide 'emajjutsu)
;;; emajjutsu.el ends here
