;;; emajutsu-template.el -- emacs-lisp wrapper for jj templates -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; Write jj templates

;;; Code:

(defun emj-template--atom (item)
  "Stringify ITEM."
  (format "surround(\"\\\"\", \"\\\"\", %s)" item))

(defun emj-template--list (items)
  "Create a list of ITEMS for a jujutsu template."
  (format "surround(\"[\", \"]\", separate(\",\", %s))"
	  (string-join (mapcar #'emj-template--parse items) ",")))

(defun emj-template--map (var expression list-expression)
  "Create a json list by mapping EXPRESSION containing VAR over LIST-EXPRESSION."
  (format "coalesce(surround(\"[\", \"]\", %s.map(|%s| %s).join(\",\")), \"[]\")"
	  list-expression var (emj-template--atom expression)))

(defun emj-template--object (key-value-pairs)
  "Create an object for KEY-VALUE-PAIRS."
  (format "surround(\"{\", \"}\", separate(\",\", %s))"
	  (string-join
	   (mapcar
	    (lambda (pair)
	      (format "concat(%s, \": \", %s)"
		      (emj-template--atom
		       (format "\"%s\"" (substring (format "%s" (car pair)) 1)))
		      (emj-template--parse (cadr pair))))
	    (seq-partition key-value-pairs 2))
	   ",")))

(defun emj-template--parse (template-spec)
  "Parse TEMPLATE-SPEC as a jujutsu template."
   (cond ((stringp template-spec)
	  (emj-template--atom template-spec))
	 ((and (listp template-spec) (equal (car template-spec) :map))
	  (let ((variable (cadr template-spec))
		(fn (caddr template-spec))
		(list-expr (cadddr template-spec)))
	    (emj-template--map variable fn list-expr)))
	 ((plistp template-spec)
	  (emj-template--object template-spec))
	 ((listp template-spec)
	  (emj-template--list template-spec))
	 (t nil)))

(defun emajutsu-template/build (template-spec)
  "Finalize TEMPLATE-SPEC as an jujutsu template."
  (format "'%s'" (emj-template--parse template-spec)))

(provide 'emajutsu-template)
;;; emajutsu-template.el ends here
