;;; emajjutsu-bookmark.el -- manage bookmarks -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; bookmark management for jj

;;; Code:
(require 'emajjutsu-face)
(require 'emajjutsu-display)

(defun emajjutsu-bookmark/quit ()
  "Quit the bookmarks buffer."
  (kill-buffer)
  (delete-window))

(defun emajjutsu-bookmark/bookmark-selection ()
  "Select a bookmark interactively."
  (completing-read
   "bookmarks: "
   (mapcar
    (lambda (bookmark) (plist-get bookmark :name))
    (emajjutsu-core/bookmark-list))))

(define-derived-mode emajjutsu/bookmark-mode fundamental-mode
  "JJ bookmarks"
  "Major mode for viewing jujutsu bookmarks."
  (define-key emajjutsu/bookmark-mode-map
      (kbd "C-c q") #'emajjutsu-bookmark/quit))

(defmacro emajjutsu-bookmark--with-buffer (&rest body)
  "Rewrite the bookmark buffer with BODY."
  `(unwind-protect
	(let ((inhibit-read-only t))
	  (emajjutsu/bookmark-mode)
	  (progn ,@body))
     (read-only-mode 1)))

(defun emajjutsu-bookmark/show (bookmark)
  "Generate string for BOOKMARK spec."
  (cl-destructuring-bind (&key name marked &allow-other-keys)
      bookmark
    (let ((bookmark-name (propertize name 'face emajjutsu-face/bookmark)))
      (format "%s%s: %s %s %s"
	      (if marked "*" " ")
	      bookmark-name
	      (emajjutsu-display/colorize-id bookmark :change nil)
	      (emajjutsu-display/colorize-id bookmark :commit nil)
	      (emajjutsu-display/description bookmark nil)))))


(defun emajjutsu-bookmark/list ()
  "List bookmarks in the current repository."
  (let ((bookmarks (emajjutsu-core/bookmark-list)))
    (switch-to-buffer
     (format "*emajjutsu bookmarks: %s*" default-directory))
    (emajjutsu-bookmark--with-buffer
     (erase-buffer)
     (dolist (bookmark bookmarks)
       (insert (format "%s\n" (emajjutsu-bookmark/show bookmark)))))))

;; (defun emajjutsu-bookmark/toggle-mark ())

(defun emajjutsu-bookmark--at-point ()
  "Get the bookmark at point."
  (car
   (split-string
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))
    ":")))

(defun emajjutsu-bookmark/delete ()
  "Delete the bookmark at point."
  (when-let ((bookmark (emajjutsu-bookmark--at-point)))
    (when (y-or-n-p (format "delete bookmark %s? " bookmark))
      (emajjutsu-core/bookmark-delete bookmark))))



(provide 'emajjutsu-bookmark)
;;; emajjutsu-bookmark.el ends here
