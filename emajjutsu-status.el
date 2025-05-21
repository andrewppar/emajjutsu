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
(require 'cl-lib)

(defconst emajjutsu-face/modified-file (list :foreground "#94e2d5"))
(defconst emajjutsu-face/added-file (list :foreground "#a6e3a1"))
(defconst emajjutsu-face/copied-file (list :foreground "#a6e3a1"))
(defconst emajjutsu-face/deleted-file (list :foreground "#f38ba8"))
(defconst emajjutsu-face/conflict (list :foreground "#f38ba8"))
(defconst emajjutsu-face/empty-change (list :foreground "#a6e3a1"))
(defconst emajjutsu-face/empty-description (list :foreground "#f9e2af"))
(defconst emajjutsu-face/description (list :foreground "#cdd6f4"))
(defconst emajjutsu-face/change-short (list :weight 'bold :foreground "#f5c2e7"))
(defconst emajjutsu-face/commit-short (list :weight 'bold :foreground "#89b4fa"))
(defconst emajjutsu-face/commit-or-change (list :foreground "#6c7086"))

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

(defun emajjutsu-display--colorize-id (change-spec change-type parent?)
  "Display CHANGE-TYPE id for CHANGE-SPEC.
Bold when PARENT?."
  (let* ((id-key (cl-case change-type (:commit :commit-id) (:change :change-id)))
	 (id (plist-get change-spec id-key))
	 (short-key (cl-case change-type (:commit :short-commit) (:change :short-change)))
	 (short-id (plist-get change-spec short-key))
	 (short-face (cl-case change-type
		       (:commit emajjutsu-face/commit-short)
		       (:change emajjutsu-face/change-short)))
	 (face (if parent?
		   emajjutsu-face/commit-or-change
		 (cons :weight (cons 'bold emajjutsu-face/commit-or-change)))))
    (concat
     (propertize short-id 'face short-face)
     (propertize (substring id (length short-id)) 'face face))))

(defun emajjutsu-display--description (change-spec parent?)
  "Format the descripition for CHANGE-SPEC, bolding if not PARENT?."
  (let* ((empty-change? (equal (plist-get change-spec :emtpy) "true"))
	 (description (string-replace "\n" " " (plist-get change-spec :description)))
	 (empty-description? (equal description " "))
	 (face (cond (empty-change? emajjutsu-face/empty-change)
		     (empty-description? emajjutsu-face/empty-description)
		     (t emajjutsu-face/description)))
	 (result '()))
    (if empty-description?
	(push "(no description set)" result)
      (push description result))
    (when empty-change?
    (push "(empty)" result))
    (setq result (string-join result " "))
    (propertize result 'face (if parent?
				 face
			       (cons :weight (cons 'bold face))))))

(defun emajjutsu-status--change (change-spec parent?)
  "Format information about the CHANGE-SPEC including whether it is PARENT?."
  (cl-destructuring-bind (&key current conflict &allow-other-keys)
      change-spec
    (let* ((current-tagline (if (equal current "true") "(@)" "(>)"))
	   (parent-tagline (if parent? "Parent Commit :" "Working Copy  :"))
	   (commit-id (emajjutsu-display--colorize-id change-spec :commit parent?))
	   (change-id (emajjutsu-display--colorize-id change-spec :change parent?))
	   (description (emajjutsu-display--description change-spec parent?))
	   (conflict? (equal conflict "true")))
    (string-join
     (list current-tagline parent-tagline change-id commit-id description)
     " "))))

(defun emajjutsu-status--parents (parent-specs)
  "Create a representation of each spec in PARENT-SPECS."
  (string-join
   (mapcar
    (lambda (parent-spec)
      (emajjutsu-status--change parent-spec t))
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
  (thread-last emajjutsu-status--buffer->data
	       (alist-get buffer)
	       (alist-get :change)
	       (alist-get :change-id)))

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
	(emajjutsu-status--change change nil)
	(emajjutsu-status--parents (mapcar #'emajjutsu-core/change-status parents)))
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

(provide 'emajjutsu-status)
;;; emajjutsu-status.el ends here
