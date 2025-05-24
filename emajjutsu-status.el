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
(require 'cl-lib)

(defun emajjutsu-status--files (file-specs)
  "Create a string to represent the effected FILE-SPECS."
  (string-join
   (cons
    "Working copy changes: "
    (mapcar
     (lambda (file-spec)
       (cl-destructuring-bind (&key status file &allow-other-keys)
	   file-spec
	 (let ((status-string (cl-case status
				(:added "A")
				(:modified "M")
				(:copied "C")
				(:deleted "D")
				(:renamed "R"))))
	   (propertize (format "%s %s" status-string file)
		       'face
		       (cl-case status
			 (:added emajjutsu-face/added-file)
			 (:copied emajjutsu-face/copied-file)
			 (:modified emajjutsu-face/modified-file)
			 (:deleted emajjutsu-face/deleted-file)
			 (:renamed emajjutsu-face/modified-file))))))
     file-specs))
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

(defun emajjutsu-status--buffer->data--change-id (buffer)
  "Get the change-id for BUFFER in EMAJJUTSU-STATUS--BUFFER->DATA."
  (thread-first
    (alist-get buffer emajjutsu-status--buffer->data)
    (plist-get :change)
    (plist-get :change-id)))

(defun emajjutsu-status/quit ()
  "Quit the current buffer."
  (interactive)
  (emajjutsu-status--remove-buffer-data (current-buffer))
  (kill-buffer))

(define-derived-mode emajjutsu/status-mode fundamental-mode
  "View jujutsu repo status."
  "Major mode for viewing jujutsu status."
  (define-key emajjutsu/status-mode-map
      (kbd "C-c q") #'emajjutsu-status/quit))

(defun emajjutsu-status--conflicts (change-id change)
  "Generate a conflict string for CHANGE-ID and CHANGE."
  (cl-destructuring-bind (&key conflict &allow-other-keys)
      change
    (if (equal conflict "true")
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
  (delete-other-windows)
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

(defun emajjutsu-status--change-at-point ()
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

(defun emajjutsu-status/refresh-buffer ()
  "Refresh the status on the current buffer."
  (interactive)
  (thread-last
    (current-buffer)
    emajjutsu-status--buffer->data--change-id
    emajjutsu-status/status))

(defun emajjutsu-status/follow-at-point ()
  "Call `emajjutsu-status/status` on the change-id for the line at point."
  (interactive)
  (emajjutsu-status/status (emajjutsu-status--change-at-point)))

(defun emajjutsu-status/edit-at-point ()
  "Call jujutsu edit on the change under point.
If one cannot be found the change associated with the current buffer is
used."
  (interactive)
  (if-let ((change-id (emajjutsu-status--change-at-point)))
      (emajjutsu-core/edit change-id)
    (let* ((buffer-data (alist-get (current-buffer)
				  emajjutsu-status--buffer->data nil nil #'equal))
	  (found-change-id (thread-first buffer-data
					 (plist-get :change)
					 (plist-get :change-id))))
      (when found-change-id
	(emajjutsu-core/edit found-change-id))
      (emajjutsu-status/status "@"))))

(defun emajjutsu-status--mark-change-at-point ())

(defun emajjutsu-status/describe (description)
  "Write a DESCRIPTION for the commit at point."
  (interactive
   (list (read-string "describe change: ")))
  (let ((change (emajjutsu-status--change-at-point)))
    (emajjutsu-core/describe change description)
    (emajjutsu-status/refresh-buffer)))

(defun emajjutsu-status/new ()
  "Create a new commit")

(provide 'emajjutsu-status)
;;; emajjutsu-status.el ends here
