;;; emajjutsu-core.el -- make calls to jujutsu -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; Execute jj commands from within Emacs

;;; Code:
(require 'emajjutsu-template)
(require 'subr-x)

(defconst emajjutsu/jj
  (string-trim (shell-command-to-string "which jj")))

(defun emajjutsu-core--execute-internal (command subcommand &rest args)
  "Execute COMMAND with SUBCOMMAND and ARGS."
  (let ((default-directory (string-trim (shell-command-to-string "jj root"))))
    (when subcommand (push subcommand args))
    (push command args)
    (push emajjutsu/jj args)
    (let ((response (shell-command-to-string (string-join args " "))))
      (if (string-prefix-p "Error:" response)
	  (error response)
	response))))

(defun emajjutsu-core--parse-file-line (line)
  "Structured output of LINE representing a file."
  (let ((mod-status (cond
		      ((string-prefix-p "A" line) :added)
		      ((string-prefix-p "M" line) :modified)
		      ((string-prefix-p "C" line) :copied)
		      ((string-prefix-p "D" line) :deleted)
		      ((string-prefix-p "R" line) :renamed)
		      (t nil))))
    (list :status mod-status
	  :file (string-trim (substring line 1)))))

(defconst emajjutsu-core--commit-template
  (emajjutsu-template/build
   (list
    :change-id "change_id.short()"
    :commit-id "commit_id.short()"
    :author "coalesce(author.email(), \" \")"
    :short-change "change_id.shortest()"
    :short-commit "commit_id.shortest()"
    :current "current_working_copy"
    :empty (list :expression "empty")
    :immutable (list :expression "immutable")
    :bookmarks (list :local (list :map "x" (list :expression "stringify(x).escape_json()") "local_bookmarks")
   		     :remote (list :map "x" (list :expression "stringify(x).escape_json()") "remote_bookmarks"))
    :conflict (list :expression "conflict")
    :parents (list :map "x" "x.commit_id().shortest()" "parents")
    :description (list :expression "coalesce(description.first_line().escape_json(), \" \")"))))

(defun emajjutsu-core/change-status (commit-or-change)
  "Get information for COMMIT-OR-CHANGE.

This includes: change and commit ids and description"
  (json-parse-string
   (string-replace
    "" "\\n"
    (string-replace
     "\n" "\\n"
     (emajjutsu-core--execute-internal
      "log" nil "--no-graph"
      "-r" commit-or-change
      "-T" emajjutsu-core--commit-template)))
   :object-type 'plist
   :false-object nil
   :array-type 'list))

(defun emajjutsu-core/conflicts (commit-or-change)
  "Get conflicting file paths for COMMIT-OR-CHANGE."
  (seq-reduce
   (lambda (acc line)
     (if (equal (substring line 1 2) "C")
	 (cons (string-join (cdr (split-string line " ")) " ") acc)
       acc))
   (split-string
    (emajjutsu-core--execute-internal
     "log" nil
     "--no-graph" "--types"
     "--template" "' '"
     "-r" commit-or-change)
    "\n" t " ")
   '()))

(defun emajjutsu-core/change-files (commit-or-change)
  "Get details about the commit with COMMIT-OR-CHANGE id or unique prefix."
  (mapcar
   #'emajjutsu-core--parse-file-line
   (split-string
    (emajjutsu-core--execute-internal
     "show" "--summary" "--template" "\" \"" "-r" commit-or-change)
    "\n" t " ")))

(defun emajjutsu-core/edit (commit-or-change)
  "Swap current change to COMMIT-OR-CHANGE (using `jj edit`)."
  (emajjutsu-core--execute-internal "edit" "-r" commit-or-change))

(defun emajjutsu-core/log-tree ()
  "Get the jj string representing a log tree.

That is all of the lines connecting nodes, with only change ids at nodes."
  (emajjutsu-core--execute-internal "log" nil "--template" "'change_id.short() ++ \"\n\n\"'"))

(defun emajjutsu-core/log-changes ()
  "Create a json object for each commit in jj log."
  (let* ((template (format "%s ++ \"|||\"'"
			   (substring emajjutsu-core--commit-template
				      0 (- (length emajjutsu-core--commit-template) 1))))
	 (comma-separated-response (replace-regexp-in-string
				    (regexp-quote "|||") "," response))
	 (as-json-string (format "[%s]"
				 (substring comma-separated-response
					    0 (- (length comma-separated-response) 1)))))
    (json-parse-string as-json-string
		       :object-type 'plist
		       :array-type 'list
		       :false-object nil)))

(defun emajjutsu-core/diff (commit-or-change-id &optional filepaths)
  "Get the diff for COMMIT-OR-CHANGE-ID optionally also specify FILEPATHS.
When FILEPATHS is NIL all changes are returned."
  (let ((args (list "--git" "-r" commit-or-change-id)))
    (dolist (filepath filepaths)
      (push filepath args))
    (push "diff" args)
    (apply #'emajjutsu-core--execute-internal args)))

(defun emajjutsu-core/describe (commit-or-change-id description)
  "Set the description for COMMIT-OR-CHANGE-ID to DESCRIPTION."
  (emajjutsu-core--execute-internal
   "describe" nil "-r" commit-or-change-id "-m" (format "\"%s\"" description)))

(defun emajjutsu-core/new
    (source-commit-or-change-id &optional before-commit-or-change-id)
  "Create a new change after SOURCE-COMMIT-OR-CHANGE-ID.
Optionally ensure that it is inserted before BEFORE-COMMIT-OR-CHANGE-ID"
  (if before-commit-or-change-id
      (emajjutsu-core--execute-internal
       "new" nil
       "--insert-after" source-commit-or-change-id
       "--insert-before" before-commit-or-change-id)
    (emajjutsu-core--execute-internal
     "new" nil "--insert-after" source-commit-or-change-id)))

(provide 'emajjutsu-core)
;;; emajjutsu-core.el ends here
