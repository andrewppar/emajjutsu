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
(require 'cl-lib)
(require 'emajjutsu-display)
(require 'emajjutsu-blame)
(require 'emajjutsu-bookmark)
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
(defun emajjutsu/log ()
  "Create a log entry using Emajjutsu.
This function is an interactive wrapper around `emajjutsu-log/log', providing a
convenient way to quickly create log entries in your journaling system."
  (interactive)
  (emajjutsu-log/log))

;;;###autoload
(defun emajjutsu/log-custom (limit revisions)
  "View the log for @.
Optionally specify LIMIT.
Optionally specify REVISIONS."
  (interactive
   (let ((revision-default (string-join
			    (list
			     "present(@)"
			     "ancestors(immutable_heads().., 2)"
			     "present(trunk())")
			    " | ")))
   (list (emajjutsu--read-limit)
	 (read-string "revisions: " revision-default nil revision-default))))
  (emajjutsu-log/log limit revisions))

(defun emajjutsu--change-id-at-point ()
  "Get the change id at point if it exists.
Return nil if no change ID is found."
  (cond ((equal major-mode 'emajjutsu/status-mode)
	 (emajjutsu-status/change-at-point))
	((equal major-mode 'emajjutsu/log-mode)
	 (emajjutsu-log/nearest-change))
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
  (let* ((change-id (or (emajjutsu--change-id-at-point)
			(emajjutsu-display/change-selection)))
	 (existing-description (plist-get
				(emajjutsu-core/change-status change-id)
				:description))
	 (description (read-string "Describe change: " existing-description)))
    (emajjutsu--with-buffer-refresh
     (emajjutsu-core/describe change-id description))))

(defun emajjutsu/diff ()
  "Get the diff of the change at point."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (cond ((equal major-mode 'emajjutsu/status-mode)
	  (emajjutsu-status/diff))
	 ((equal major-mode 'emajjutsu/log-mode)
	  (emajjutsu-log/diff))
	 (t nil))))


(defun emajjutsu/bookmarks ()
  "List bookmarks in repo."
  )

(defun emajjutsu/bookmark-delete (bookmark)
  "Delete BOOKMARK."
  (interactive
   (list
    (emajjutsu-bookmark/bookmark-selection)))
  (emajjutsu--with-buffer-refresh
   (when (y-or-n-p (format "delete bookmark: %s?" bookmark))
     (emajjutsu-core/bookmark-delete bookmark))))

(defun emajjutsu/bookmark-forget (bookmark)
  "Forget BOOKMARK."
  (interactive
   (list
    (emajjutsu-bookmark/bookmark-selection)))
  (emajjutsu--with-buffer-refresh
   (emajjutsu-core/bookmark-forget bookmark)))

(defun emajjutsu/bookmark-set ()
  "Move bookmark to the change at point.
If the bookmark does not exist, create it."
  (interactive)
  (let* ((bookmark (emajjutsu-bookmark/bookmark-selection))
	 (bookmarks (mapcar
		     (lambda (bookmark)
		       (plist-get bookmark :name))
		     (emajjutsu-core/bookmark-list)))
	 (change-id (or (emajjutsu--change-id-at-point)
			(emajjutsu-display/change-selection))))
    (emajjutsu--with-buffer-refresh
     (if (member bookmark bookmarks)
	 (emajjutsu-core/bookmark-set bookmark change-id)
       (emajjutsu-core/bookmark-create bookmark change-id)))))

(defun emajjutsu/bookmark-rename ()
  (interactive)
  (let* ((bookmark (emajjutsu-bookmark/bookmark-selection))
	 (new-name (read-string "new name: " bookmark)))
    (emajjutsu--with-buffer-refresh
     (emajjutsu-bookmark/rename bookmark new-name))))

(defun emajjutsu/bookmark-track ()
  (interactive)
  (let* ((bookmark (emajjutsu-bookmark/bookmark-selection t)))
    (emajjutsu--with-buffer-refresh
     (emajjutsu-bookmark/track bookmark))))

;;;###autoload
(defun emajjutsu/bookmark-list ()
  "Open a buffer with a list of bookmarks in the current repo."
  (interactive)
  (split-window-sensibly)
  (emajjutsu-bookmark/list))

(defun emajjutsu/refresh-buffer ()
  "Refresh the data on the current buffer."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (message "refreshing...")))

(defun emajjutsu--selected-changes ()
  "Return the list of marked revisions or select one.

If in `emajjutsu/log-mode', return the marked changes in the log.
Otherwise, return a list containing the change ID at point or, if
no change ID is at point, prompt the user to select a change."
  (or
   (and (equal major-mode 'emajjutsu/log-mode)
	(emajjutsu-log/marked-changes))
   (list
    (or (emajjutsu--change-id-at-point)
	(emajjutsu-display/change-selection)))))

(defun emajjutsu--rebase-between-internal (rebase-type)
  "Rebase selected revisions between two change.

REBASE-TYPE determines the type of rebase operation to perform.
REVISIONS is a list of revision identifiers to rebase.

Prompts user to select `after' and `before' change points, then rebases
each revision in REVISIONS between those points using the specified rebase type.
Returns and displays a list of result messages from each rebase operation."
  (interactive)
  (let ((revisions (emajjutsu--selected-changes))
	(after (emajjutsu-display/change-selection "after change: "))
	(before (emajjutsu-display/change-selection "before change: "))
	(results '()))
    (emajjutsu--with-buffer-refresh
     (setq
      results
      (mapcar
       (lambda (revision)
	 (emajjutsu-core/rebase-between revision after before rebase-type))
       revisions))
     (message (string-join results "\n"))
     results)))

(defun emajjutsu--rebase-internal (rebase-type location)
  "Rebase the change at point on to a selected change.

REBASE-TYPE specifies the children (if any) of the change to be rebased.
LOCATION specifies whether the rebase is before or after the selected change."
  (let* ((changes (emajjutsu--selected-changes))
	 (target-change (emajjutsu-display/change-selection
			 "rebase destination: "))
	 (response '()))

    (emajjutsu--with-buffer-refresh
     (setq response
	   (string-join
	    (mapcar
	     (lambda (change)
	       (format "%s: %s"
		       change
		       (emajjutsu-core/rebase change target-change rebase-type location)))
	     changes)
	    "\n")))
    (message response)))

(defun emajjutsu/rebase-source ()
  "Rebase the change at point, along with descendents onto a selected change."
  (interactive)
  (emajjutsu--rebase-internal :source :destination))

(defun emajjutsu/rebase-source-before ()
  "Rebase the change and all its descendants before selected change."
  (interactive)
  (emajjutsu--rebase-internal :source :before))

(defun emajjutsu/rebase-source-between ()
  "Rebase from source between two revisions."
  (interactive)
  (emajjutsu--rebase-between-internal :source))

(defun emajjutsu/rebase-revision ()
  "Rebase the change at point onto a selected change."
  (interactive)
  (emajjutsu--rebase-internal :revision :destination))

(defun emajjutsu/rebase-revision-before ()
  "Rebase the change at point before a selected change."
  (interactive)
  (emajjutsu--rebase-internal :revision :before))

(defun emajjutsu/rebase-revision-between ()
  "Rebase from source between two revisions."
  (interactive)
  (emajjutsu--rebase-between-internal :revision))

(defun emajjutsu/rebase-branch ()
  "Rebase the change and all its children at point onto a selected change."
  (interactive)
  (emajjutsu--rebase-internal :branch :destination))

(defun emajjutsu/rebase-branch-before ()
  "Rebase the change and all its children at point before a selected change."
  (interactive)
  (emajjutsu--rebase-internal :branch :before))

(defun emajjutsu/rebase-branch-between ()
  "Rebase from source between two revisions."
  (interactive)
  (emajjutsu--rebase-between-internal :branch))

(defun emajjutsu/duplicate ()
  "Duplicate the change at point and select destination."
  (interactive)
  (let* ((source-change (or (emajjutsu--change-id-at-point)
			    (emajjutsu-display/change-selection)))
	 (target-change (emajjutsu-display/change-selection
			 "duplicate into change: "))
	 (description (read-string "new description: "
				   (plist-get
				    (emajjutsu-core/change-status source-change)
				    :description))))
    (emajjutsu--with-buffer-refresh
     (if (equal (string-trim description) "")
	 (emajjutsu-core/duplicate source-change target-change)
       (emajjutsu-core/duplicate source-change target-change description)))))


(defun emajjutsu/log->item-at-point ()
  "From a log view get the status of a particular change."
  (interactive)
  (split-window-sensibly)
  (let ((line (buffer-substring-no-properties
	       (line-beginning-position) (line-end-position))))
    (if (emajjutsu-log/at-file-p)
	(find-file
	 (string-join
	  (list (emajjutsu-core/root)
		(plist-get (emajjutsu-file/parse-string line) :file))
	  "/"))
      (emajjutsu-status/status (emajjutsu-log/nearest-change)))))

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

(defun emajjutsu/restore ()
  "Peform a restore.
The marked files in the buffer are restored to their parent."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (cond
     ((equal major-mode 'emajjutsu/status-mode)
      (emajjutsu-status/restore))
     ((equal major-mode 'emajjutsu/log-mode)
      (emajjutsu-log/restore))
     (t nil))))

(defun emajjutsu/abandon ()
  "Abandon a change."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (let ((changes nil))
     (if-let ((marked-changes (and (equal major-mode 'emajjutsu/log-mode)
				   (emajjutsu-log/marked-changes))))
	 (setq changes marked-changes)
       (let* ((default (emajjutsu--change-id-at-point))
	      (prompt "select a change to abandon: ")
	      (change (emajjutsu-display/change-selection prompt  default)))
	 (push change changes)))
     (let ((change-string (cond ((equal (length changes) 1)
				 (let* ((change (car changes))
					(desc (plist-get
					       (emajjutsu-core/change-status change)
					       :description)))
				   (format "%s: %s" change desc)))
				((equal (length changes) 2)
				 (format "%s and %s"
					 (car changes)
					 (cadr changes)))
				(t
				 (format "%s, and %s"
					 (string-join (cdr changes) ", ")
					 (car changes))))))
       (when (y-or-n-p (format "Really abandon %s?" change-string))
	 (apply #'emajjutsu-core/abandon changes))))))

;;;###autoload
(defun emajjutsu/init ()
  "Create or colocate a jj repository in the current directory."
  (interactive)
  (message (emajjutsu-core/init)))

;;;###autoload
(defun emajjutsu/absorb ()
  "Absorb change in the current buffer based on the major mode.

When in `emajjutsu/status-mode', calls `emajjutsu-status/absorb'.
When in `emajjutsu/log-mode', calls `emajjutsu-log/absorb'.
Does nothing in other modes.

The buffer will be refreshed after the operation."
  (interactive)
  (emajjutsu--with-buffer-refresh
   (let ((revision (cond ((equal major-mode 'emajjutsu/status-mode)
			  (emajjutsu-status/change-at-point))
			 ((equal major-mode 'emajjutsu/log-mode)
			  (emajjutsu-log/nearest-change))
			 (t nil))))
     (emajjutsu-core/absorb revision))))

(defun emajjutsu/blame (file)
  "Show blame information for FILE using Emajjutsu.
Display line-by-line blame information, showing who last modified each line
and when.  This uses the underlying version control system to retrieve the
information.  The result is displayed in a specialized blame buffer."
  (interactive
   (list (read-file-name "file: ")))
  (emajjutsu-blame/blame-file file))

(provide 'emajjutsu)
;;; emajjutsu.el ends here
