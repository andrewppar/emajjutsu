;;; emajutsu-status.el -- create a status buffer like jj st -*- lexical-binding: t -*-

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
(require 'emajutsu-core)
(require 'cl-lib)

(defconst emajutsu-face/modified-file (list :foreground "#94e2d5"))
(defconst emajutsu-face/added-file (list :foreground "#a6e3a1"))
(defconst emajutsu-face/deleted-file (list :foreground "#f38ba8"))
(defconst emajutsu-face/conflict (list :foreground "#f38ba8"))
(defconst emajutsu-face/empty-change (list :foreground "#a6e3a1"))
(defconst emajutsu-face/empty-description (list :foreground "#f9e2af"))
(defconst emajutsu-face/description (list :foreground "#cdd6f4"))
(defconst emajutsu-face/change-short (list :weight 'bold :foreground "#f5c2e7"))
(defconst emajutsu-face/commit-short (list :weight 'bold :foreground "#89b4fa"))
(defconst emajutsu-face/commit-or-change (list :foreground "#6c7086"))

(defun emajutsu-status--files (file-specs)
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
				(:deleted "D")
				(:renamed "R"))))
	   (propertize (format "%s %s" status-string file)
		       'face
		       (cl-case status
			 (:added emajutsu-face/added-file)
			 (:modified emajutsu-face/modified-file)
			 (:deleted emajutsu-face/deleted-file)
			 (:renamed emajutsu-face/modified-file))))))
     file-specs))
   "\n"))

(defun emajutsu-display--colorize-id (change-spec change-type parent?)
  "Display CHANGE-TYPE id for CHANGE-SPEC.
Bold when PARENT?."
  (let* ((id-key (cl-case change-type (:commit :commit-id) (:change :change-id)))
	 (id (plist-get change-spec id-key))
	 (short-key (cl-case change-type (:commit :short-commit) (:change :short-change)))
	 (short-id (plist-get change-spec short-key))
	 (short-face (cl-case change-type
		       (:commit emajutsu-face/commit-short)
		       (:change emajutsu-face/change-short)))
	 (face (if parent?
		   emajutsu-face/commit-or-change
		 (cons :weight (cons 'bold emajutsu-face/commit-or-change)))))
    (concat
     (propertize short-id 'face short-face)
     (propertize (substring id (length short-id)) 'face face))))

(defun emajutsu-display--description (change-spec parent?)
  "Format the descripition for CHANGE-SPEC, bolding if not PARENT?."
  (let* ((empty-change? (equal (plist-get change-spec :emtpy) "true"))
	 (description (string-replace "\n" " " (plist-get change-spec :description)))
	 (empty-description? (equal description " "))
	 (face (cond (empty-change? emajutsu-face/empty-change)
		     (empty-description? emajutsu-face/empty-description)
		     (t emajutsu-face/description)))
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

(defun emajutsu-status--change (change-spec parent?)
  "Format information about the CHANGE-SPEC including whether it is PARENT?."
  (cl-destructuring-bind (&key current conflict &allow-other-keys)
      change-spec
    (let* ((current-tagline (if (equal current "true") "(@)" "(>)"))
	   (parent-tagline (if parent? "Parent Commit :" "Working Copy  :"))
	   (commit-id (emajutsu-display--colorize-id change-spec :commit parent?))
	   (change-id (emajutsu-display--colorize-id change-spec :change parent?))
	   (description (emajutsu-display--description change-spec parent?))
	   (conflict? (equal conflict "true")))
    (string-join
     (list current-tagline parent-tagline change-id commit-id description)
     " "))))

(defun emajutsu-status--parents (parent-specs)
  "Create a representation of each spec in PARENT-SPECS."
  (string-join
   (mapcar
    (lambda (parent-spec)
      (emajutsu-status--change parent-spec t))
    parent-specs)
   "\n"))

;;;;;;

(defvar emajutsu-status--buffer->data '()
  "A place to store all the data associated with a buffer.")

(defun emajutsu-status--remove-buffer-data (buffer)
  "Remove BUFFER from emajutsu-status--buffer->data."
  (setq emajutsu-status--buffer->data
	(seq-filter
	 (lambda (buffer->data)
	   (let ((buffer-key (car buffer->data)))
	     (and (not (equal buffer-key buffer)) (buffer-live-p buffer-key))))
	 emajutsu-status--buffer->data)))

(defun emajutsu-status--add-buffer-data (buffer data)
  "Add BUFFER with DATA to emajutsu-status--buffer->data."
  (emajutsu-status--remove-buffer-data buffer)
  (push (cons buffer data) emajutsu-status--buffer->data))

(defun emajutsu-status--buffer->data--change-id (buffer)
  "Get the change-id for BUFFER in EMAJUTSU-STATUS--BUFFER->DATA."
  (thread-last emajutsu-status--buffer->data
	       (alist-get buffer)
	       (alist-get :change)
	       (alist-get :change-id)))

(defun emajutsu-status/quit ()
  "Quit the current buffer."
  (interactive)
  (emajutsu-status--remove-buffer-data (current-buffer))
  (kill-buffer))

(define-derived-mode emajutsu/status-mode fundamental-mode
  "View jujutsu repo status."
  "Major mode for viewing jujutsu status."
  (define-key emajutsu/status-mode-map
      (kbd "C-c q") #'emajutsu-status/quit))

(defmacro emajutsu-status--with-buffer (&rest body)
  "Change the contents of the current buffer with BODY."
  `(unwind-protect
	(let ((inhibit-read-only t))
	  (emajutsu/status-mode)
	  (progn ,@body))
     (read-only-mode 1)))

(defun emajutsu-status/status (change-id)
  "Create a buffer to display jujutsu status for CHANGE-ID."
  (delete-other-windows)
  (let* ((buffer (switch-to-buffer (format "*emajutsu status: %s*" change-id)))
	 (change (emajutsu-core/change-status change-id))
	 (files (emajutsu-core/change-files change-id))
	 (parents (plist-get change :parents)))
    (emajutsu-status--add-buffer-data
     buffer (list :change change :files files))
    (emajutsu-status--with-buffer
     (erase-buffer)
     (insert
      (string-join
       (list
	(format "Directory: %s" default-directory)
	""
	(emajutsu-status--files files)
	(emajutsu-status--change change nil)
	(emajutsu-status--parents (mapcar #'emajutsu-core/change-status parents)))
       "\n")))))

(defun emajutsu-status--change-at-point ()
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

(defun emajutsu-status/refresh-buffer ()
  "Refresh the status on the current buffer."
  (interactive)
  (thread-last
    (current-buffer)
    emajutsu-status--buffer->data--change-id
    emajutsu-status/status))

(defun emajutsu-status/follow-at-point ()
  "Call `emajutsu-status/status` on the change-id for the line at point."
  (interactive)
  (emajutsu-status/status (emajutsu-status--change-at-point)))

(defun emajutsu-status/edit-at-point ()
  "Call jujutsu edit on the change under point.
If one cannot be found the change associated with the current buffer is
used."
  (interactive)
  (if-let ((change-id (emajutsu-status--change-at-point)))
      (emajutsu-core/edit change-id)
    (let* ((buffer-data (alist-get (current-buffer)
				  emajutsu-status--buffer->data nil nil #'equal))
	  (found-change-id (thread-first buffer-data
					 (plist-get :change)
					 (plist-get :change-id))))
      (when found-change-id
	(emajutsu-core/edit found-change-id))
      (emajutsu-status/status "@"))))

(defun emajutsu-status--mark-change-at-point ())




(provide 'emajutsu-status)
;;; emajutsu-status.el ends here
