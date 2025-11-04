;;; emajjutsu-table.el -- display tables -*- lexical-binding: t -*-

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

(defun emajjutsu-table/start-p (string)
  "Check whether STRING represents the first line of a table."
  (string-prefix-p "╭" string))

(defun emajjutsu-table/end-p (string)
  "Check whether STRING represents the last line of a table."
  (string-prefix-p "╰" string))

(defun emajjutsu-table/draw-border (lines)
  "Draw a border around LINES and join them together."
  (let* ((max-length (apply #'max (mapcar #'length lines)))
	 (top (format "╭%s╮" (string-join (make-list (+ max-length 2) "─"))))
	 (bottom (format "╰%s╯" (string-join (make-list (+ max-length 2) "─")))))
    (string-join
     (cons top
	   (seq-reduce
	    (lambda (acc line)
	      (let ((padding (make-string (- max-length (length line)) ? )))
		(cons (format "│ %s%s │" line padding) acc)))
	    (reverse lines)
	    (list bottom)))
     "\n")))




(provide 'emajjutsu-table)
;;; emajjutsu-table.el ends here
