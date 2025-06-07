;;; emajjutsu-display.el -- format some jj objects -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; Formatters for jj objects

;;; Code:
(require 'emajjutsu-face)
(require 'emajjutsu-core)
(require 'cl-lib)

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

(defun emajjutsu-display--bookmarks (change-spec)
  "Format the bookmarks from CHANGE-SPEC."
  (let* ((bookmarks (plist-get change-spec :bookmarks))
	 (local-bookmarks (plist-get bookmarks :local))
	 (remote-bookmarks (plist-get bookmarks :remote)))
    (propertize
     (cond (local-bookmarks
	    (format "%s | " (string-join local-bookmarks " ")))
	   (remote-bookmarks
	    (format "%s | " (string-join remote-bookmarks " ")))
	   (t ""))
     'face emajjutsu-face/bookmark)))

(defun emajjutsu-display--description (change-spec parent?)
  "Format the descripition for CHANGE-SPEC, bolding if not PARENT?."
  (let* ((empty-change? (plist-get change-spec :empty))
	 (description (string-replace "\n" " " (plist-get change-spec :description)))
	 (empty-description? (equal description ""))
	 (face (cond (empty-change? emajjutsu-face/empty-change)
		     (empty-description? emajjutsu-face/empty-description)
		     (t emajjutsu-face/description)))
	 (result '()))
    (if empty-description?
	(push "(no description set)" result)
      (push description result))
    (when empty-change?
      (push "(empty)" result))
    (setq result
	  (list
	   (propertize (string-join result " ")
		      'face
		      (if parent?
			  face
			(cons :weight (cons 'bold face))))))
    (when (equal (plist-get change-spec :conflict) "true")
      (push (propertize "(conflict)" 'face emajjutsu-face/conflict) result))
    (push (emajjutsu-display--bookmarks change-spec) result)
    (setq result (string-join result " "))
    result))

(cl-defun emajjutsu-display/change (change-spec &key compact? parent?)
  "Format information about the CHANGE-SPEC.
Passing :COMPACT? t prints a smaller summary
Passing :PARENT? t ensures that the change is formatted as a parent."
  (cl-destructuring-bind (&key current author &allow-other-keys)
      change-spec
    (let* ((current-tagline (if (equal current "true") "(@)" "(>)"))
	   (parent-tagline (if parent? "Parent Commit :" "Working Copy  :"))
	   (commit-id (emajjutsu-display--colorize-id change-spec :commit parent?))
	   (change-id (emajjutsu-display--colorize-id change-spec :change parent?))
	   (description (emajjutsu-display--description change-spec parent?))
	   (author (if author
		       (propertize author 'face emajjutsu-face/author)
		     "")))
      (string-join
       (if compact?
	   (list change-id commit-id description)
	 (list current-tagline parent-tagline change-id commit-id description))
       " "))))

(defun emajjutsu-display--selectable-change (change-spec)
  "Create a display string for CHANGE-SPEC in change selection."
  (cl-destructuring-bind
	(&key change-id short-change description &allow-other-keys)
      change-spec
    (format "%s: %s"
	    (propertize short-change 'face emajjutsu-face/change-short)
	    description)))

(defun emajjutsu-display/change-selection (&optional prompt)
  "Interactively select a change-id.
Optionally supply a PROMPT."
  (let* ((selection-prompt (or prompt "Select a change: "))
	 (selection (completing-read
		     selection-prompt
		     (mapcar
		      #'emajjutsu-display--selectable-change
		      (emajjutsu-core/log-changes nil)))))
    (car (split-string selection ":"))))

(provide 'emajjutsu-display)
;;; emajjutsu-display.el ends here
