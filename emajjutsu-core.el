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

(defun emajjutsu-core/root ()
  "The root of the repo at `default-directory`."
  (string-trim (shell-command-to-string "jj root")))

(defun emajjutsu-core--execute-internal (command subcommand &rest args)
  "Execute COMMAND with SUBCOMMAND and ARGS."
  (let ((default-directory (emajjutsu-core/root)))
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

(defun emajjutsu-core/log-tree (limit)
  "Get the jj string representing a log tree.

That is all of the lines connecting nodes, with only change ids at nodes.
LIMIT specifies the number of nodes to fetch."
  (if limit
      (emajjutsu-core--execute-internal
       "log" nil
       "--limit" (format "%s" limit)
       "--template" "'change_id.short() ++ \"\n\n\"'")
    (emajjutsu-core--execute-internal
     "log" nil "--template" "'change_id.short() ++ \"\n\n\"'")))

(defun emajjutsu-core/log-changes (limit)
  "Create a json object for each commit in jj log with LIMIT."
  (let* ((template (format "%s ++ \"|||\"'"
			   (substring emajjutsu-core--commit-template
				      0 (- (length emajjutsu-core--commit-template) 1))))
	 (response (if limit
		       (emajjutsu-core--execute-internal
			"log" nil "--no-graph"
			"--limit" (format "%s" limit)  "--template" template)
		     (emajjutsu-core--execute-internal
		      "log" nil "--no-graph" "--template" template)))
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
    (source-commit-or-change-id)
  "Create a new change after SOURCE-COMMIT-OR-CHANGE-ID."
  (emajjutsu-core--execute-internal "new" source-commit-or-change-id))

(defun emajjutsu-core/bookmark-list ()
  "Get a list of the bookmarks in repo."
  (let* ((base-template (emajjutsu-template/build
			 (list :name (list :expression "name.escape_json()"))))
	 (response (emajjutsu-core--execute-internal
		    "bookmark" "list" "--template"
		    (format "%s ++ \"|||\"'" (substring base-template 0 -1)))))
    (unless (string-empty-p response)
      (let ((comma-separated (replace-regexp-in-string
			      (regexp-quote "|||") "," response)))
	(json-parse-string (format "[%s]" (substring comma-separated 0 -1))
			   :object-type 'plist
			   :array-type 'list
			   :false-object nil)))))

(defun emajjutsu-core/bookmark-set (bookmark change-id)
  "Set BOOKMARK at CHANGE-ID."
  (emajjutsu-core--execute-internal
   "bookmark" "set" bookmark "-r" change-id))

(defun emajjutsu-core/bookmark-create (bookmark-name change-id)
  "Create a bookmark with BOOKMARK-NAME at CHANGE-ID."
  (emajjutsu-core--execute-internal
   "bookmark" "create" bookmark-name "-r" change-id))

(defun emajjutsu-core/bookmark-delete (bookmark)
  "Delete BOOKMARK."
  (emajjutsu-core--execute-internal "bookmark" "delete" bookmark))

(defun emajjutsu-core/rebase-source (source-change target-change location)
  "Rebase SOURCE-CHANGE (and its descendants) to TARGET-CHANGE.
LOCATION specifies where the rebase will be located with respect to
TARGET-COMMIT."
  (let ((location-flag (pcase location
			 (:after "--insert-after")
			 (:before "--insert-before")
			 (_ "--destination"))))
    (emajjutsu-core--execute-internal
     "rebase" nil "--source" source-change location-flag target-change)))

(defun emajjutsu-core/fetch ()
  "Fetch from remote."
  (message
   (emajjutsu-core--execute-internal "git" "fetch")))

(defun emajjutsu-core/push ()
  "Push current state to remote."
  (message
   (emajjutsu-core--execute-internal "git" "push" "--allow-new")))

(defun emajjutsu-core/squash (files source-change-id target-change-id)
  "Squash the FILES in SOURCE-CHANGE-ID into TARGET-CHANGE-ID."
  (let ((args (seq-concatenate
	       'list
	       files
	       (list "--from" source-change-id "--into" target-change-id "-u"))))
    (apply #'emajjutsu-core--execute-internal "squash" args)))

(defun emajjutsu-core/split (change-id files description)
  "Split CHANGE-ID into two one with FILES and one without.
The new change has DESCRIPTION."
  ;; all this is necessary because `jj split` tries to use an editor
  ;; to get a description.
  (let ((source-id (if (equal (string-trim change-id) "@")
		       (plist-get (emajjutsu-core/change-status "@") :change-id)
		     change-id)))
    (emajjutsu-core/new source-id)
    ;; current change is now the new one
    (let ((new-change-id (plist-get (emajjutsu-core/change-status "@") :change-id)))
      (emajjutsu-core/describe new-change-id description)
      (emajjutsu-core/edit source-id)
      (emajjutsu-core/squash files source-id new-change-id))))




(provide 'emajjutsu-core)
;;; emajjutsu-core.el ends here
