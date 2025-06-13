;;; emajjutsu-status.el -- create a status buffer like jj st -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; View jujutsu status

;;; Code:
(require 'subr-x)
(require 'emajjutsu-core)
(require 'emajjutsu-face)
(require 'emajjutsu-display)
(require 'emajjutsu-file)
(require 'cl-lib)

(defun emajjutsu-status--files (file-specs)
  "Create a string to represent the effected FILE-SPECS."
  (string-join
   (cons
    "Working copy changes: "
    (mapcar #'emajjutsu-display/show-file-spec file-specs))
   "\n"))

(defun emajjutsu-status--parents (parent-specs)
  "Create a representation of each spec in PARENT-SPECS."
  (string-join
   (mapcar
    (lambda (parent-spec)
      (emajjutsu-display/change parent-spec :parent? t))
    parent-specs)
   "\n"))

;;;;;;

(defvar emajjutsu-status--buffer->data '()
  "A place to store all the data associated with a buffer.")

(defun emajjutsu-status--remove-buffer-data (buffer)
  "Remove BUFFER from emajjutsu-status--buffer->data."
  (setq emajjutsu-status--buffer->data
	(seq-filter
	 (lambda (buffer->data)
	   (let ((buffer-key (car buffer->data)))
	     (and (not (equal buffer-key buffer)) (buffer-live-p buffer-key))))
	 emajjutsu-status--buffer->data)))

(defun emajjutsu-status--add-buffer-data (buffer data)
  "Add BUFFER with DATA to emajjutsu-status--buffer->data."
  (emajjutsu-status--remove-buffer-data buffer)
  (push (cons buffer data) emajjutsu-status--buffer->data))


(defun emajjutsu-status--buffer->data-mark-file (buffer file mark?)
  "Add a :marked flag of MARK? to the buffer data for BUFFER and FILE."
  ;; I hate that this is not using more functional operations
  (let* ((current-data (alist-get buffer emajjutsu-status--buffer->data)))
    (dolist (file-data (plist-get current-data :files))
      (when (equal (plist-get file-data :file) file)
	(plist-put file-data :marked mark?)))))

(defun emajjutsu-status--buffer->data-change-id (buffer)
  "Get the change-id for BUFFER in EMAJJUTSU-STATUS--BUFFER->DATA."
  (thread-first
    (alist-get buffer emajjutsu-status--buffer->data)
    (plist-get :change)
    (plist-get :change-id)))


(defun emajjutsu-status/quit ()
  "Quit the current buffer."
  (interactive)
  (emajjutsu-status--remove-buffer-data (current-buffer))
  (kill-buffer)
  (delete-window))

(define-derived-mode emajjutsu/status-mode fundamental-mode
  "View jujutsu repo status."
  "Major mode for viewing jujutsu status."
  (define-key emajjutsu/status-mode-map
      (kbd "C-c q") #'emajjutsu-status/quit))

(defun emajjutsu-status--conflicts (change-id change)
  "Generate a conflict string for CHANGE-ID and CHANGE."
  (cl-destructuring-bind (&key conflict &allow-other-keys)
      change
    (if conflict
	(string-join
	 (cons
	  (format "%s There are unresolved conflicts at these paths:"
		  (propertize "Warning:" 'face emajjutsu-face/warning))
	  (emajjutsu-core/conflicts change-id))
	 "\n")
      "")))

(defmacro emajjutsu-status--with-buffer (&rest body)
  "Change the contents of the current buffer with BODY."
  `(unwind-protect
	(let ((inhibit-read-only t))
	  (emajjutsu/status-mode)
	  (progn ,@body))
     (read-only-mode 1)))

(defun emajjutsu-status/status (change-id)
  "Create a buffer to display jujutsu status for CHANGE-ID."
  (let* ((buffer (switch-to-buffer (format "*emajjutsu status: %s*" change-id)))
	 (change (emajjutsu-core/change-status change-id))
	 (files (emajjutsu-core/change-files change-id))
	 (parents (plist-get change :parents)))
    (emajjutsu-status--add-buffer-data
     buffer (list :change change :files files))
    (emajjutsu-status--with-buffer
     (erase-buffer)
     (insert
      (string-join
       (list
	(format "Directory: %s" default-directory)
	""
	(emajjutsu-status--files files)
	(emajjutsu-display/change change)
	(emajjutsu-status--parents (mapcar #'emajjutsu-core/change-status parents))
	(emajjutsu-status--conflicts change-id change))
       "\n")))))

(defun emajjutsu-status/change-at-point ()
  "Get the change id at point."
  (let ((line (buffer-substring-no-properties
	       (line-beginning-position) (line-end-position))))
    (when (or (string-prefix-p "(@)" line)
	      (string-prefix-p "(>)" line))
      (thread-first line
		    (split-string ":" t " ")
		    cadr
		    (split-string " " t " ")
		    car))))

(defun emajjutsu-status--filename-at-point ()
  "Get the filename at point."
  (thread-first
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))
    emajjutsu-file/parse-string
    (plist-get :file)))

(defun emajjutsu-status/diff ()
  "Show a diff of the file at point."
  (let* ((file (emajjutsu-status--filename-at-point))
	 (change-id (emajjutsu-status--buffer->data-change-id
		     (current-buffer))))
    (switch-to-buffer (format "*emajjutsu diff: %s*" file))
    (erase-buffer)
    (insert (emajjutsu-core/diff change-id (list file)))
    (diff-mode)))

(defun emajjutsu-status/refresh-buffer ()
  "Refresh the status on the current buffer."
  (interactive)
  (thread-last
    (current-buffer)
    emajjutsu-status--buffer->data-change-id
    emajjutsu-status/status))

(defun emajjutsu-status/follow-change-at-point ()
  "Call `emajjutsu-status/status` on the change-id for the line at point."
  (interactive)
  (emajjutsu-status/status (emajjutsu-status/change-at-point)))

(defun emajjutsu-status/visit-file-at-point ()
  "When there is a file at point, visit that file."
  (interactive)
  (when-let ((file (emajjutsu-status--filename-at-point)))
    (find-file (string-join (list (emajjutsu-core/root) file) "/"))))

(defun emajjutsu-status/action-at-point ()
  "Take action that is relevant at point."
  (interactive)
  (cond ((emajjutsu-status--filename-at-point)
	 (emajjutsu-status/visit-file-at-point))
	((emajjutsu-status/change-at-point)
	 (emajjutsu-status/follow-change-at-point))))

(defun emajjutsu-status--toggle-mark ()
  "Get line at point and toggle mark."
  (let ((line (string-trim
	       (buffer-substring
		(line-beginning-position) (line-end-position)))))
    (cl-destructuring-bind (status file)
	(string-split line " " t " ")
      (let* ((mark? (string-suffix-p "*" status))
	     (new-status (if mark?
			     (string-replace "*" " " status)
			   (format "%s*" status)))
	     (line-start (line-beginning-position)))
	(emajjutsu-status--buffer->data-mark-file
	 (current-buffer) file (not mark?))
	(save-excursion
	  (goto-char line-start)
	  (delete-region line-start (+ line-start 2))
	  (insert new-status))))))

(defun emajjutsu-status/toggle-mark ()
  "Toggle the mark on the file at point."
  (interactive)
  (when (emajjutsu-status--filename-at-point)
    (emajjutsu-status--with-buffer
     (emajjutsu-status--toggle-mark))))

(defun emajjutsu-status--buffer->data-marked-files (buffer)
  "Get filenames that are marked in BUFFER."
  (seq-reduce
   (lambda (acc filespec)
     (cl-destructuring-bind (&key marked file &allow-other-keys)
	 filespec
       (if marked (cons file acc) acc)))
   (plist-get (alist-get buffer emajjutsu-status--buffer->data) :files)
   '()))

(defun emajjutsu-status/split-marks (description)
  "Perform emajjutsu split on the marked files.
Splits are parallel by default.
DESCRIPTION is applied to the change that gets the marked files."
  (interactive)
  (let* ((buffer (current-buffer))
	 (change-id (emajjutsu-status--buffer->data-change-id buffer)))
    (when-let ((files (emajjutsu-status--buffer->data-marked-files buffer)))
      (emajjutsu-core/split change-id files description))))

(defun emajjutsu-status/squash-marks (target-change-id)
  "Squash marked files from the current change into TARGET-CHANGE-ID."
  (interactive)
  (let* ((buffer (current-buffer))
	 (change-id (emajjutsu-status--buffer->data-change-id buffer)))
    (when-let ((files (emajjutsu-status--buffer->data-marked-files buffer)))
      (emajjutsu-core/squash files change-id target-change-id))))

(provide 'emajjutsu-status)
;;; emajjutsu-status.el ends here
