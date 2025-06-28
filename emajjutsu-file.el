;;; emajjutsu-file.el -- display files -*- lexical-binding: t -*-

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
(require 'cl-lib)
(require 'emajjutsu-face)

(defconst emajjutsu-file--jj-file-status
  (list "A" "M" "C" "D" "R")
  "A single letter string representing a jujutsu status.")

(defun emajjutsu-file--parse-status (status-string)
  "Parse STATUS-STRING into a status keyword and a marked boolean."
  (let ((marked-p (string-suffix-p "*" (string-trim status-string)))) (list :marked marked-p
	  :status (pcase (substring status-string 0 1)
		    ("A" :added)
		    ("M" :modified)
		    ("C" :copied)
		    ("D" :deleted)
		    ("R" :renamed)))))

(defun emajjutsu-file--parse-file (file-string status)
  "Parse FILE-STRING into a file and a display string based on STATUS."
  (if (equal status :renamed)
      (cl-destructuring-bind (base rewrite)
	  (split-string (string-trim file-string) "{")
	(let ((new-file (substring (cadr (split-string rewrite "=>")) -1)))
	  (list :file (format "%s%s" base new-file) :display file-string)))
    (list :file file-string :display file-string)))

(defun emajjutsu-file/parse-string (string)
  "Parse STRING into a file spec.
Return NIL if it cannot be parsed."
  (let* ((log-line-p (and (string-prefix-p "│" string)
			  (string-suffix-p "│" string)))
	 (stripped (or (and log-line-p (substring string 2 -2)) string))
	 (split (split-string stripped  " " t " ")))
    (unless (string-prefix-p "Files:" stripped)
      (let* ((status-string (car split))
	     (status-data (emajjutsu-file--parse-status status-string))
	     (status (plist-get status-data :status)))
	(cl-destructuring-bind (&key file display)
	    (emajjutsu-file--parse-file (string-join (cdr split)) status)
	  (plist-put (plist-put status-data :file file) :display display))))))

(defun emajjutsu-file/at-point ()
  "Get the filespect at point."
  (emajjutsu-file/parse-string
   (buffer-substring-no-properties
    (line-beginning-position) (line-end-position))))


(defun emajjutsu-file/table-start-p (string)
  "Check whether STRING represents the first line of a table."
  (string-prefix-p "╭" string))

(defun emajjutsu-file/table-end-p (string)
  "Check whether STRING represents the last line of a table."
  (string-prefix-p "╰" string))

(defun emajjutsu-file/show-spec (file-spec)
  "Show FILE-SPEC as a string with coloring.
FILE-SPEC is expected to have keys: status, file, and marked."
  (cl-destructuring-bind (&key status display marked &allow-other-keys)
      file-spec
    (let* ((face (cl-case status
		   (:added emajjutsu-face/added-file)
		   (:copied emajjutsu-face/copied-file)
		   (:modified emajjutsu-face/modified-file)
		   (:deleted emajjutsu-face/deleted-file)
		   (:renamed emajjutsu-face/modified-file)))
	   (status-string (propertize
			   (cl-case status
			     (:added "A")
			     (:modified "M")
			     (:copied "C")
			     (:deleted "D")
			     (:renamed "R"))
			   'face face))
	   (mark (if marked (propertize "*" 'face emajjutsu-face/warning) " "))
	   (display-string (propertize display 'face face)))
      (format "%s%s %s" status-string mark display-string))))

(defun emajjutsu-file/toggle-mark (string)
  "Toggle the mark on STRING."
  (when-let ((spec (emajjutsu-file/parse-string string)))
    (emajjutsu-file/show-spec
     (plist-put spec :marked (not (plist-get spec :marked))))))

(provide 'emajjutsu-file)
;;; emajjutsu-file.el ends here
