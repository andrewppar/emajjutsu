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
(require 'cl-lib)
(require 'emajjutsu-display)
(require 'emajjutsu-core)
(require 'emajjutsu-file)
(require 'emajjutsu-table)

(define-derived-mode emajjutsu/log-mode fundamental-mode
  "JJ Log"
  "Major mode for viewing jujutsu status."
  (define-key emajjutsu/log-mode-map
      (kbd "C-c q") (lambda () (interactive) (kill-buffer (current-buffer)))))

(defmacro emajjutsu-log--with-buffer (&rest body)
  "Rewrite the log buffer with BODY."
  `(unwind-protect
	(progn
	  (switch-to-buffer (format "*emajjutsu log: %s*" (emajjutsu-core/root)))
	  (let ((inhibit-read-only t))
	    (emajjutsu/log-mode)
	    (progn ,@body)))
     (read-only-mode 1)))

(defconst emajjutsu-log--current-limit nil
  "The limit that was passed to emajjutsu-log/log.")

(defun emajjutsu-log--color-change-markers (string)
  "Propertize any markers in STRING."
  (thread-last
    string
    (string-replace (regexp-quote "@") (propertize "@" 'face emajjutsu-face/current))
    (string-replace (regexp-quote "×") (propertize "×" 'face emajjutsu-face/conflict))
    (string-replace (regexp-quote "◆") (propertize "◆" 'face emajjutsu-face/immutable))))

(defun emajjutsu-log--change-marker (change)
  "Generate a string of the change marker for CHANGE."
  (cl-destructuring-bind (&key current immutable conflict &allow-other-keys)
      change
    (cond ((equal current "true")
	   (propertize "@" 'face emajjutsu-face/current))
	  (immutable
	   (propertize "◆" 'face emajjutsu-face/immutable))
	  (conflict
	   (propertize "×" 'face emajjutsu-face/conflict))
	  (t "○"))))

(defun emajjutsu-log--pad-lines (string)
  "Add two spaces before each line in STRING."
  (let ((lines (string-split string "\n")))
    (seq-reduce
     (lambda (result line)
       (format "%s\n  %s" result line))
     (cdr lines)
     (car lines))))

(defun emajjutsu-log/log (&optional limit revisions)
  "Create a buffer that displays the jj log with LIMIT and REVISIONS.
If LIMIT is NIL it is treated as though there is none.
If REVISIONS is not specified, the default is used."
  (interactive)
  (setq emajjutsu-log--current-limit limit)
  (emajjutsu-log--with-buffer
   (erase-buffer)
   (insert
    (emajjutsu-log--pad-lines
     (string-join
      (list
       (format "Directory: %s" default-directory)
       ""
       (thread-last
	 (emajjutsu-core/log-tree limit revisions)
	 emajjutsu-log--color-change-markers
	 (seq-reduce
	  (lambda (acc change-spec)
	    (let ((commit-id (plist-get change-spec :commit-id)))
	      (replace-regexp-in-string
	       (regexp-quote commit-id)
	       (emajjutsu-display/change change-spec :compact? t)
	       acc)))
	  (emajjutsu-core/log-changes nil revisions))))
      "\n")))
   (goto-char (point-min))))

(defun emajjutsu-log/quit ()
  "Quit the current log buffer."
  (interactive)
  (setq emajjutsu-log--current-limit nil)
  (kill-buffer)
  (delete-window))

(defun emajjutsu-log/refresh-buffer ()
  "Refresh the log buffer."
  (interactive)
  (emajjutsu-log/log emajjutsu-log--current-limit))

(defconst emajjutsu-log--reverse-hex
  (mapcar #'identity "klmnopqrstuvwxyz"))

(defun emajjutsu-log--change-line-simple-p (line)
  "A predicate that indicates that LINE is for a change."
  (when (or (string-prefix-p "@" line)
	    (string-prefix-p "◆" line)
	    (string-prefix-p "×" line)
	    (string-prefix-p "M" line)
	    (string-prefix-p "○" line))
    (let ((second-element (cadr (split-string line " " t " "))))
      (and (= (length second-element) 12)
	   (seq-every-p
	    (lambda (char) (member char emajjutsu-log--reverse-hex))
	    second-element)))))

(defun emajjutsu-log--line ()
  "Get the line at point."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun emajjutsu-log--file-line-p (line)
  "Predicate to determine whether LINE represents a file."
  (and (string-prefix-p "│" line)
       (string-suffix-p "│" line)))

(defun emajjutsu-log--change-line-p (line &rest ignore-prefixes)
  "Check whether LINE has a change.
IGNORE-PREFIXES is a list of strings that may precede a change and
should be ignored for the purposes of the check."
  (emajjutsu-log--change-line-simple-p
   (string-trim
    (seq-reduce
     (lambda (result prefix-to-ignore)
       (replace-regexp-in-string (regexp-quote prefix-to-ignore) "" result))
     ignore-prefixes
     line))))

(defun emajjutsu-log/change-at-point ()
  "Get the change at point if it exists."
  (let ((line (emajjutsu-log--line)))
    ;; todo: this is configuration specific - so we may have to generalize
    ;; or find some way of reading the config... or something else
    (when (emajjutsu-log--change-line-p line "│" "*")
      (let ((change-info (thread-last
			   line
			   (replace-regexp-in-string (regexp-quote "│") "")
			   (replace-regexp-in-string (regexp-quote "*") ""))))
	(cadr (split-string change-info " " t " "))))))

(defun emajjutsu-log--nearest-change (direction)
  "Scroll in DIRECTION to find the nearest change."
  (let ((line-increment (pcase direction (:up -1) (:down 1)))
	(change-id nil)
	(last-line-return 0))
    (save-excursion
      (while (and (not change-id) (= last-line-return 0))
	(let ((line (emajjutsu-log--line)))
	  (when (not (emajjutsu-log--file-line-p line))
	    (setq change-id (emajjutsu-log/change-at-point)))
	  (setq last-line-return (forward-line line-increment)))))
    change-id))

(defun emajjutsu-log/nearest-change ()
  "Find the change-id for the current context of point."
  (emajjutsu-log--nearest-change :up))

(defun emajjutsu-log--goto-change-id (change-id)
  "Go to the line for CHANGE-ID."
  (goto-char (point-min))
  ;; we could probably make a map of change-ids to
  ;; positions and do fuzzy search or something like that
  ;; over the list...
  (re-search-forward (regexp-quote change-id)))

;;;;;;;;;
;; Inlays

(defun emajjutsu-log--hide-inlay (inlay-test-fn)
  "Remove the inlay below point that satisfies INLAY-TEST-FN."
  (let ((change-id (emajjutsu-log--nearest-change :up)))
    (save-excursion
      (emajjutsu-log--with-buffer
       (emajjutsu-log--goto-change-id change-id)
       (forward-line 1)
       (let ((found-inlay-p nil)
	     (done nil))
	 (while (not done)
	   (let ((line (emajjutsu-log--line)))
	     (cond ((emajjutsu-log--change-line-p line "│" "*")
		    (setq done t))
		   (found-inlay-p
		    (progn
		      (when (emajjutsu-table/end-p line)
			(setq done t))
		      (delete-line)))
		   (t
		    (progn
		      (setq found-inlay-p (funcall inlay-test-fn))
		      (if found-inlay-p
			  (delete-line)
			(forward-line 1))))))))))))

(defun emajjutsu-log--inlay-showing-p (inlay-test-fn)
  "Determine if there is an inlay that satisfies INLAY-TEST-FN at point."
  (save-excursion
    (let ((done nil)
	  (result nil))
      (while (and (not done) (not result))
	(setq done (or (emajjutsu-log/at-change-p) (eobp))
	      result (or result (funcall inlay-test-fn)))
	(forward-line 1))
      result)))

(defun emajjutsu-log--within-inlay-p (inlay-test-fn)
  "Test whether point is inside an inlay satisfying INLAY-TEST-FN."
  (let ((last-line-return 0)
	(result nil)
	(done nil))
    (save-excursion
      (while (and (not done) (= last-line-return 0))
	(when (emajjutsu-table/start-p (emajjutsu-log--line))
	  (setq result (funcall inlay-test-fn)
		done t))
	(setq last-line-return (forward-line -1))))
    result))

;;;;;;;;;;;;;;
;;; file inlay

(defun emajjutsu-log--file-inlay-showing-p ()
  "Test for whether the file inlay is showing."
  (emajjutsu-log--inlay-showing-p #'emajjutsu-file/at-table-start-p))

(defun emajjutsu-log--show-files (change-id)
  "Show the files for CHANGE-ID."
  (let* ((files (mapcar #'emajjutsu-file/show-spec (emajjutsu-core/change-files change-id)))
	 (title (format "Files: %s" change-id)))
    (emajjutsu-log--with-buffer

     (insert (emajjutsu-table/draw-border (cons title files)))
     (insert "\n"))))

(defun emajjutsu-log--hide-files ()
  "Hide the files for the change at point."
  (emajjutsu-log--hide-inlay #'emajjutsu-file/at-table-start-p))

;; file inlays have marks too
(defun emajjutsu-log--toggle-file-mark ()
  "Toggle the mark at the file at point, if there is one."
  (interactive)
  (when (emajjutsu-log--within-inlay-p #'emajjutsu-file/at-table-start-p)
    (let ((line (emajjutsu-log--line)))
      (when (emajjutsu-log--file-line-p line)
	(let* ((without-suffix (substring line 0 -1))
	       (pad-size (- (length without-suffix)
			    (length (string-trim without-suffix))))
	       (padding (make-string pad-size ? )))
	  (emajjutsu-log--with-buffer
	   (delete-line)
	   (insert
	    (format "│ %s%s│\n"
		    (emajjutsu-file/toggle-mark line)
		    padding))))))))

(defun emajjutsu-log--toggle-change-mark ()
  "Toggle whether there is mark on the current change."
  (let* ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (when (emajjutsu-log--change-line-p line "│" "*")
      (let* ((marked (string-prefix-p "*" line))
	     (mark (propertize "*" 'face emajjutsu-face/warning))
	     (new-line (format "%s%s\n"
			       (if marked " " mark)
			       (substring line 1))))
	(emajjutsu-log--with-buffer
	 (delete-line)
	 (insert new-line))))))

(defun emajjutsu-log/at-file-p ()
  "Return whether point is at a file."
  (emajjutsu-log--file-line-p (emajjutsu-log--line)))

(defun emajjutsu-log/at-change-p ()
  "Return whether point is at a change."
  (let ((line (emajjutsu-log--line)))
    (and (not (emajjutsu-log--file-line-p line))
	 (emajjutsu-log--change-line-p line "│" "*"))))

(defun emajjutsu-log/toggle-mark-at-point ()
  "Toggle a mark for the item at point."
  (interactive)
  (cond ((emajjutsu-log/at-file-p)
	 (emajjutsu-log--toggle-file-mark))
	((emajjutsu-log--change-line-p (emajjutsu-log--line) "│" "*")
	 (emajjutsu-log--toggle-change-mark))
	(t nil)))

(defun emajjutsu-log/marked-changes ()
  "Gather jj-changes that are marked in the current log."
  (let ((result '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line (emajjutsu-log--line)))
	  (when (string-prefix-p "*" line)
	    (when-let ((change (emajjutsu-log/change-at-point)))
	      (push change result)))
	  (forward-line 1))))
    result))

(defun emajjutsu-log/toggle-change-files ()
  "Hide or show the files for the change at point."
  (interactive)
  (let ((change-id (emajjutsu-log--nearest-change :up)))
    (save-excursion
      (emajjutsu-log--goto-change-id change-id)
      (forward-line 1)
      (if (emajjutsu-log--file-inlay-showing-p)
	  (emajjutsu-log--hide-files)
	(emajjutsu-log--show-files change-id)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; change summary inlay

(defun emajjutsu-log--hide-change-summary ()
  "Hide the change summary if it's displayed."
  (emajjutsu-log--hide-inlay #'emajjutsu-log--change-summary-start-p))

(defun emajjutsu-log--change-summary-inlay-showing-p ()
  "Test for whether the change summary inlay is showing."
  (emajjutsu-log--inlay-showing-p #'emajjutsu-log--change-summary-start-p))

(defun emajjutsu-log--change-summary-start-p ()
  "Point is at the start of a change summary."
  (let ((first-line (emajjutsu-log--line))
	(second-line (save-excursion
		       (forward-line 1)
		       (emajjutsu-log--line))))
    (and (emajjutsu-table/start-p first-line)
	 (string-prefix-p "│ Commit ID:" second-line))))

(defun emajjutsu-log--format-item (prefix item face)
  "Format a log item with a given PREFIX, ITEM, and FACE.
The PREFIX is a string prepended to the ITEM.  The ITEM is also
highlighted with the specified FACE."
  (format "%s: %s" prefix (propertize item 'face face)))

(defun emajjutsu-log--show-change-summary (change-id)
  "Insert the summary for CHANGE-ID into the log."
  (cl-destructuring-bind
	(&key commit-id change-id bookmarks author committer &allow-other-keys)
      (emajjutsu-core/change-status change-id)
    (let ((description (split-string
			(emajjutsu-core/change-full-description change-id)
			"\n"))
	  (files (mapcar
		  #'emajjutsu-file/show-spec
		  (emajjutsu-core/change-files change-id)))
	  (committer-line (emajjutsu-log--format-item
			   "Committer" committer emajjutsu-face/author))
	  (author-line (emajjutsu-log--format-item
			"Author" author emajjutsu-face/author))
	  (change-line (emajjutsu-log--format-item
			"Change ID" change-id emajjutsu-face/change-short))
	  (commit-line (emajjutsu-log--format-item
			"Commit ID" commit-id emajjutsu-face/commit-short)))
      (cl-destructuring-bind (&key local remote &allow-other-keys) bookmarks
	(let* ((bookmarks-line (emajjutsu-log--format-item
				"Bookmarks"
				(string-join (append local remote) " ")
				emajjutsu-face/bookmark))
	       (lines (thread-last files
				   (cons "")
				   (append description)
				   (cons "")
				   (cons committer-line)
				   (cons author-line)
				   (cons bookmarks-line)
				   (cons change-line)
				   (cons commit-line))))
	  (emajjutsu-log--with-buffer
	   (insert (emajjutsu-table/draw-border lines))
	   (insert "\n")))))))

(defun emajjutsu-log/toggle-change-summary ()
  "Hide or show the summary for the change at point."
  (interactive)
  (let ((change-id (emajjutsu-log--nearest-change :up)))
    (save-excursion
      (emajjutsu-log--goto-change-id change-id)
      (forward-line 1)
      (if (emajjutsu-log--change-summary-inlay-showing-p)
	  (emajjutsu-log--hide-change-summary)
	(emajjutsu-log--show-change-summary change-id)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; other log operations

(defun emajjutsu-log--files-for-change-at-point ()
  "Get the files (if they are showing) for the nearest change above point."
  (let ((change-id (emajjutsu-log--nearest-change :up))
	(files ()))
    (save-excursion
      (emajjutsu-log--goto-change-id change-id)
      (forward-line 1)
      (let ((line (buffer-substring-no-properties
		   (line-beginning-position) (line-end-position))))
	(when (emajjutsu-file/at-table-start-p)
	  (while (not (emajjutsu-table/end-p line))
	    (when-let ((file-spec (emajjutsu-file/parse-string line)))
	      (push file-spec files))
	    (forward-line 1)
	    (setq line (buffer-substring-no-properties
			(line-beginning-position) (line-end-position)))))))
    files))

(defun emajjutsu-log/split (description)
  "Split the change at point with DESCRIPTION on the new change.
The files split are the ones that are marked for that change."
  (when-let ((change-id (emajjutsu-log--nearest-change :up))
	     (files (thread-last
		      (emajjutsu-log--files-for-change-at-point)
		      (seq-filter (lambda (spec) (plist-get spec :marked)))
		      (mapcar (lambda (spec) (plist-get spec :file))))))
    (emajjutsu-log--with-buffer
     (emajjutsu-core/split change-id files description))))

(defun emajjutsu-log/squash (target-change-id)
  "Squash the marked files of the current change to TARGET-CHANGE-ID."
  (when-let ((change-id (emajjutsu-log--nearest-change :up))
	     (files (thread-last
		      (emajjutsu-log--files-for-change-at-point)
		      (seq-filter (lambda (spec) (plist-get spec :marked)))
		      (mapcar (lambda (spec) (plist-get spec :file))))))
    (emajjutsu-log--with-buffer
     (emajjutsu-core/squash files change-id target-change-id))))

(defun emajjutsu-log/restore ()
  "Revert the marked files of the curretn change."
  (when-let ((change-id (emajjutsu-log--nearest-change :up))
	     (files (thread-last
		      (emajjutsu-log--files-for-change-at-point)
		      (seq-filter (lambda (spec) (plist-get spec :marked)))
		      (mapcar (lambda (spec) (plist-get spec :file))))))
    (emajjutsu-log--with-buffer
     (emajjutsu-core/restore files change-id))))

(defun emajjutsu-log/diff ()
  "Show a diff of the file at point."
  (let* ((line (buffer-substring-no-properties
		(line-beginning-position) (line-end-position)))
	 (file (plist-get (emajjutsu-file/parse-string line) :file))
	 (change-id (emajjutsu-log--nearest-change :up)))
    (split-window-sensibly)
    (switch-to-buffer (format "*emajjutsu diff: %s*" file))
    (erase-buffer)
    (insert (emajjutsu-core/diff change-id (list file)))
    (diff-mode)
    (goto-char (point-min))))

;;(defun emajjutsu-log/add-revision-selection ()
;;  "Rewrite the log page using a revision formula specified by the user."
;;  (interactive)
;;  (let ((formula (read-string

(provide 'emajjutsu-log)
;;; emajjutsu-log.el ends here
