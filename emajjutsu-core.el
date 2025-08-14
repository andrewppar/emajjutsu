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
(require' emajjutsu-file)
(require 'subr-x)

(defconst emajjutsu/jj
  (string-trim (shell-command-to-string "which jj")))

(defun emajjutsu-core/root ()
  "The root of the repo at `default-directory`."
  (string-trim (shell-command-to-string "jj root")))

(defun emajjutsu-core--execute-internal (command subcommand args attempts)
  "Execute COMMAND with SUBCOMMAND and ARGS.
ATTEMPTS is how many times this has been attempted, a backout limit
is set for the number of attempts tried."
  (let ((default-directory (emajjutsu-core/root))
	(passed-args (seq-copy args)))
    (when subcommand (push subcommand passed-args))
    (push command passed-args)
    (push emajjutsu/jj passed-args)
    (let ((response (shell-command-to-string (string-join passed-args " "))))
      (cond ((string-prefix-p "Error:" response)
	     (error response))
	    ((string-prefix-p "Rebased" response)
	     (if (> attempts 2)
		 (error response)
	       (emajjutsu-core--execute-internal
		command subcommand args (+ attempts 1))))
	    (t response)))))

(defun emajjutsu-core--execute (command subcommand &rest args)
  "Execute COMMAND with SUBCOMMAND and ARGS."
  (emajjutsu-core--execute-internal command subcommand args 1))

(defconst emajjutsu-core--commit-template
  (emajjutsu-template/build
   (list
    :change-id "change_id.short()"
    :commit-id "commit_id.short()"
    :author "coalesce(author.email(), \" \")"
    :short-change "change_id.shortest()"
    :short-commit "commit_id.shortest()"
    :divergent (list :expression "divergent")
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
     (emajjutsu-core--execute
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
    (emajjutsu-core--execute
     "log" nil
     "--no-graph" "--types"
     "--template" "' '"
     "-r" commit-or-change)
    "\n" t " ")
   '()))

(defun emajjutsu-core/change-files (commit-or-change)
  "Get details about the commit with COMMIT-OR-CHANGE id or unique prefix."
  (mapcar
   #'emajjutsu-file/parse-string
   (split-string
    (emajjutsu-core--execute
     "show" "--summary" "--template" "\" \"" "-r" commit-or-change)
    "\n" t " ")))


(defun emajjutsu-core/changes-files
    (from-change-specifier to-change-specifier)
  "Get a list of unique files modified between two change specifiers.
FROM-CHANGE-SPECIFIER and TO-CHANGE-SPECIFIER specify the range of changes
to consider.  Returns a list of filenames that were modified in the
specified range."
  (let ((template "'change_id.short() ++ \"\n\n\"'")
	(revisions (format "%s::%s" to-change-specifier from-change-specifier)))
    (seq-reduce
     (lambda (acc* file-spec)
       (let ((name (plist-get file-spec :file)))
	 (if (member name acc*) acc* (cons name acc*))))
     (seq-mapcat
      #'emajjutsu-core/change-files
      (split-string
       (emajjutsu-core--execute "log" nil "-r" revisions "--no-graph" "--template" template)
       "\n\n" t " "))
     '())))

(defun emajjutsu-core/edit (commit-or-change)
  "Swap current change to COMMIT-OR-CHANGE (using `jj edit`)."
  (emajjutsu-core--execute "edit" "-r" commit-or-change))

(defun emajjutsu-core/log-tree (limit)
  "Get the jj string representing a log tree.

That is all of the lines connecting nodes, with only change ids at nodes.
LIMIT specifies the number of nodes to fetch."
  (if limit
      (emajjutsu-core--execute
       "log" nil
       "--limit" (format "%s" limit)
       "--template" "'commit_id.short() ++ \"\n\n\"'")
    (emajjutsu-core--execute
     "log" nil "--template" "'commit_id.short() ++ \"\n\n\"'")))

(defun emajjutsu-core/log-changes (limit)
  "Create a json object for each commit in jj log with LIMIT."
  (let* ((template (format "%s ++ \"|||\"'"
			   (substring emajjutsu-core--commit-template
				      0 (- (length emajjutsu-core--commit-template) 1))))
	 (response (if limit
		       (emajjutsu-core--execute
			"log" nil "--no-graph"
			"--limit" (format "%s" limit)  "--template" template)
		     (emajjutsu-core--execute
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
    (apply #'emajjutsu-core--execute args)))

(defun emajjutsu-core/describe (commit-or-change-id description)
  "Set the description for COMMIT-OR-CHANGE-ID to DESCRIPTION."
  (emajjutsu-core--execute
   "describe" nil "-r" commit-or-change-id "-m" (format "\"%s\"" description)))

(defun emajjutsu-core/new
    (source-commit-or-change-id)
  "Create a new change after SOURCE-COMMIT-OR-CHANGE-ID."
  (emajjutsu-core--execute "new" source-commit-or-change-id))

(defun emajjutsu-core/bookmark-list ()
  "Get a list of the bookmarks in repo."
  (let* ((base-template (emajjutsu-template/build
			 (list :name (list :expression "name.escape_json()")
			       :change-id "normal_target.change_id().short()"
			       :commit-id "normal_target.commit_id().short()"
			       :short-change "normal_target.change_id().shortest()"
			       :short-commit "normal_target.commit_id().shortest()"
			       :empty (list :expression "normal_target.empty()")
			       :description (list :expression "coalesce(normal_target.description().first_line().escape_json(), \" \")"))))
	 (response (emajjutsu-core--execute
		    "bookmark" "list" "--quiet" "--template"
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
  (emajjutsu-core--execute
   "bookmark" "set" bookmark "-r" change-id))

(defun emajjutsu-core/bookmark-create (bookmark-name change-id)
  "Create a bookmark with BOOKMARK-NAME at CHANGE-ID."
  (emajjutsu-core--execute
   "bookmark" "create" bookmark-name "-r" change-id))

(defun emajjutsu-core/bookmark-delete (bookmark)
  "Delete BOOKMARK."
  (emajjutsu-core--execute "bookmark" "delete" bookmark))

(defun emajjutsu-core/bookmark-forget (bookmark)
  "Forget BOOKMARK."
  (emajjutsu-core--execute "bookmark" "forget" bookmark))

(defun emajjutsu-core/rebase
    (revision target-change rebase-type location)
  "Rebase REVISION to TARGET-CHANGE.

REBASE-TYPE: specifies the related revisions to carry over in the rebase.
There are three options:
1. :revision - rebases only the selected revision
2. :branch - rebases the branch above the selected revision
3. :source - rebases the descendents of the selected revision.

LOCATION: specifies where the rebase is with respect to TARGET-CHANGE.
There are three options:
1. :after - rebases onto the target change and rebases all descendents
2. :before - rebases before the target change and rebases all descendents
3. :destination - makes the target change a parent."
  (let ((location-flag (pcase location
			 (:after "--insert-after")
			 (:before "--insert-before")
			 (:destination "--destination")))
	(rebase-type-flag (pcase rebase-type
			    (:revision "--revisions")
			    (:branch "--branch")
			    (:source "--source"))))
    (emajjutsu-core--execute
     "rebase" nil rebase-type-flag revision location-flag target-change)))

(defun emajjutsu-core/duplicate
    (source-change target-change &optional description)
  "Duplicate SOURCE-CHANGE with destination TARGET-CHANGE.
Add DESCRIPTION to the new change if it's passed in."
  (let ((response (emajjutsu-core--execute
		  "duplicate" source-change "-d" target-change)))
    (cl-destructuring-bind (_duplicated _old-change _as new-change &rest ignore)
	(string-split response)
      (if description
	  (emajjutsu-core/describe new-change description)
	new-change))))

(defun emajjutsu-core/init ()
  "Initialize a jj repo in the current directory."
  (message
   (shell-command-to-string
    (format "%s git init --colocate" emajjutsu/jj))))

(defun emajjutsu-core/fetch ()
  "Fetch from remote."
  (message
   (emajjutsu-core--execute "git" "fetch")))

(defun emajjutsu-core/push ()
  "Push current state to remote."
  (message
   (emajjutsu-core--execute "git" "push" "--allow-new")))

(defun emajjutsu-core/squash (files source-change-id target-change-id)
  "Squash the FILES in SOURCE-CHANGE-ID into TARGET-CHANGE-ID."
  (let ((args (seq-concatenate
	       'list
	       files
	       (list "--from" source-change-id "--into" target-change-id "-u"))))
    (apply #'emajjutsu-core--execute "squash" args)))

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

(defun emajjutsu-core/abandon (change-id &rest others)
  "Abandon the jj-changes associated with CHANGE-ID and OTHERS."
  (apply #'emajjutsu-core--execute "abandon" (cons change-id others)))

(defun emajjutsu-core/restore (files change-id)
  "Revert FILES for CHANGE-ID."
  (let ((parent (format "%s-" change-id)))
    (apply #'emajjutsu-core--execute
	   "restore" nil
	   (seq-concatenate 'list files
			    (list "--to" change-id "--from" parent)))))

(defun emajjutsu-core/absorb (revision)
  "Absorb files from REVISION."
  (emajjutsu-core--execute "absorb" nil "--from" revision))

(cl-defun emajjutsu-core/annotate (file &key (change-spec "@"))
  "Annotate FILE with current annotations.
Change what annotations to show with CHANGE-SPEC (default: \"@\")."
  (emajjutsu-core--execute "file" "annotate" file "-r" change-spec))

(provide 'emajjutsu-core)
;;; emajjutsu-core.el ends here
