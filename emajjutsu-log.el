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

(define-derived-mode emajjutsu/log-mode fundamental-mode
  "JJ Log"
  "Major mode for viewing jujutsu status."
  (define-key emajjutsu/log-mode-map
      (kbd "C-c q") (lambda () (interactive) (kill-buffer (current-buffer)))))

(defmacro emajjutsu-log--with-buffer (&rest body)
  "Rewrite the log buffer with BODY."
  `(unwind-protect
	(progn
	  (switch-to-buffer (format "*emajjutsu log: %s*" default-directory))
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

(defun emajjutsu-log/log (limit)
  "Create a buffer that displays the jj log with LIMIT.
If LIMIT is NIL it is treated as though there is none."
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
	 (emajjutsu-core/log-tree limit)
	 emajjutsu-log--color-change-markers
	 (seq-reduce
	  (lambda (acc change-spec)
	    (let ((change-id (plist-get change-spec :change-id)))
	      (replace-regexp-in-string
	       (regexp-quote change-id)
	       (emajjutsu-display/change change-spec :compact? t)
	       acc)))
	  (emajjutsu-core/log-changes nil))))
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

(defun emajjutsu-log--change-line-p (line)
  "A predicate that indicates that LINE is for a change."
  (or (string-prefix-p "@" line)
      (string-prefix-p "◆" line)
      (string-prefix-p "×" line)
      (string-prefix-p "M" line)
      (string-prefix-p "○" line)))

(defun emajjutsu-log--line ()
  "Get the line at point."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun emajjutsu-log/change-at-point ()
  "Get the change at point if it exists."
  (let ((line (thread-last
		(emajjutsu-log--line)
		(replace-regexp-in-string (regexp-quote "│") "")
		(replace-regexp-in-string (regexp-quote "*") "")
		string-trim)))
    ;; todo: this is configuration specific - so we may have to generalize
    ;; or find some way of reading the config... or something else
    (when (emajjutsu-log--change-line-p line)
      (cadr (split-string line " " t " ")))))

(defun emajjutsu-log--nearest-change (direction)
  "Scroll in DIRECTION to find the nearest change."
  (let ((line-increment (pcase direction (:up -1) (:down 1)))
	(change-id nil)
	(last-line-return 0))
    (save-excursion
      (while (and (not change-id) (= last-line-return 0))
	(setq change-id (emajjutsu-log/change-at-point)
	      last-line-return (forward-line line-increment))))
    change-id))

(defun emajjutsu-log--goto-change-id (change-id)
  "Go to the line for CHANGE-ID."
  (goto-char (point-min))
  ;; we could probably make a map of change-ids to
  ;; positions and do fuzzy search or something like that
  ;; over the list...
  (re-search-forward (regexp-quote change-id)))

(defun emajjutsu-log--show-files ()
  "Show the files for the change at point."
  (when-let ((change-id (emajjutsu-log--nearest-change :up)))
    (emajjutsu-log--goto-change-id change-id)
    (end-of-line)
    (let* ((files (cons (format "Files: %s" change-id)
			(mapcar #'emajjutsu-file/show-spec
				(emajjutsu-core/change-files change-id))))
	   (max-file (apply #'max (mapcar #'length files)))
	   (top-divider (string-join (cons "╭" (nconc (make-list (+ 2 max-file) "─") (list "╮")))))
	   (bottom-divider (string-join (cons "╰" (nconc (make-list (+ 2 max-file) "─") (list "╯")))))
	   (table-string (string-join
			  (mapcar
			   (lambda (line)
			     (let ((padding (make-string (- max-file (length line)) ? )))
			       (format "│ %s%s │" line padding)))
			   files)
			  "\n")))
      (emajjutsu-log--with-buffer
       (insert "\n")
       (insert (string-join (list top-divider table-string bottom-divider) "\n"))))))

(defun emajjutsu-log--displayed-files ()
  "Get the displayed files for the change at point."
  (let ((change-id (emajjutsu-log--nearest-change :up))
	(files '()))
    (save-excursion
      (emajjutsu-log--goto-change-id change-id)
      (forward-line 1)
      (let ((line (emajjutsu-log--line)))
	(when (emajjutsu-file/table-start-p line)
	  (while (not (emajjutsu-file/table-end-p line))
	    (when (and (not (string-prefix-p "│ Files:" line))
		       (not (emajjutsu-file/table-start-p line)))
	      (push (emajjutsu-file/parse-string line) files))
	    (forward-line 1)
	    (setq line (buffer-substring-no-properties
			(line-beginning-position) (line-end-position)))))))
    files))

(defun emajjutsu-log--hide-files ()
  "Hide the files for the change at point."
  (let ((change-id (emajjutsu-log--nearest-change :up)))
    (save-excursion
      (emajjutsu-log--with-buffer
       (emajjutsu-log--goto-change-id change-id)
       (forward-line 1)
       (let ((line (emajjutsu-log--line)))
	 (when (emajjutsu-file/table-start-p line)
	   (while (not (emajjutsu-file/table-end-p line))
	     (delete-line)
	     (setq line (buffer-substring-no-properties
			 (line-beginning-position) (line-end-position))))
	   (delete-line)))))))

(defun emajjutsu-log/toggle-change-files ()
  "Hide or show the files for the change at point."
  (interactive)
  (let ((change-id (emajjutsu-log--nearest-change :up)))
    (save-excursion
      (emajjutsu-log--goto-change-id change-id)
      (forward-line 1)
      (let ((line (emajjutsu-log--line)))
	(if (emajjutsu-file/table-start-p line)
	    (emajjutsu-log--hide-files)
	  (emajjutsu-log--show-files))))))


(defun emajjutsu-log--file-line-p (line)
  "Predicate to determine whether LINE represents a file."
  (and (string-prefix-p "│" line)
       (string-suffix-p "│" line)))

(defun emajjutsu-log/toggle-file-mark ()
  "Toggle the mark at the file at point, if there is one."
  (interactive)
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
		  padding)))))))

(defun emajjutsu-log/toggle-change-mark ()
  "Toggle whether there is mark on the current change."
  (let* ((line (buffer-substring
		(line-beginning-position) (line-end-position)))
	 (test-line (string-trim
		     (replace-regexp-in-string (regexp-quote "│") ""
					       (substring line 2)))))
    (when (or (emajjutsu-log--change-line-p test-line)
	      ;; if it's already toggled
	      (and (> (length line) 2)
		   (emajjutsu-log--change-line-p test-line)))
      (let* ((marked (string-prefix-p "*" line))
	     (new-line (format "%s%s\n"
			       (if marked " " "*")
			       (substring line 1))))
	(emajjutsu-log--with-buffer
	 (delete-line)
	 (insert new-line))))))

(defun emajjutsu-log--marked-changes ()
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

(defun emajjutsu-log--files-for-change-at-point ()
  "Get the files (if they are showing) for the nearest change above point."
  (let ((change-id (emajjutsu-log--nearest-change :up))
	(files ()))
    (save-excursion
      (emajjutsu-log--goto-change-id change-id)
      (forward-line 1)
      (let ((line (buffer-substring-no-properties
		   (line-beginning-position) (line-end-position))))
	(when (emajjutsu-file/table-start-p line)
	  (while (not (emajjutsu-file/table-end-p line))
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
    (diff-mode)))

(provide 'emajjutsu-log)
;;; emajjutsu-log.el ends here
