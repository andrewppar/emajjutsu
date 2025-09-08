;;; emajjutsu-blame.el --- Blame functionality for Emajjutsu -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides blame functionality for the Emajjutsu package.

;;; Code:
(require 'cl-lib)
(require 'emajjutsu-core)

(defvar emajjutsu-blame--previous-left-margin nil
  "Variable to track the right margin for after quitting a blame buffer.")

(defvar emajjutsu-blame--active-buffers '()
  "A list of buffers that have blame data in their margins.")

(defun emajjutsu-blame--index (blame-data)
  "Create an indexed lookup of Git blame information from BLAME-DATA.
BLAME-DATA is a list of plists, each containing :commit, :short-commit, :line,
and :author keys.  Returns a plist mapping line numbers to formatted blame
strings, with :max-line indicating the maximum string length.  Each blame string
shows the commit hash (with short part highlighted) followed by the author name,
both with appropriate faces applied."
  (let ((index '()))
    (dolist (blame-datum blame-data)
      (cl-destructuring-bind (&key commit short-commit line author) blame-datum
	(let* ((max-line (or (plist-get index :max-line) 0))
	       (long (propertize (substring commit (length short-commit))
				 'face emajjutsu-face/commit-or-change))
	       (short (propertize short-commit
				  'face emajjutsu-face/commit-short))
	       (blame-line (format "%s%s: %s"
				   short
				   long
				   (propertize author 'face emajjutsu-face/author)))
	       (blame-length (length blame-line)))
	  (setq index (plist-put index (string-to-number line) blame-line))
	  (when (> blame-length max-line)
	    (plist-put index :max-line blame-length)))))
    index))

(defun emajjutsu-blame--set-text (text)
  "Add a left margin overlay at point with TEXT as content.
Create an overlay at point and put TEXT in the left margin with appropriate
display properties.  The overlay is marked with the emajjutsu-blame-margin
property set to t."
  (let ((ov (make-overlay (point) (point))))
    (overlay-put ov 'emajjutsu-blame-margin t)
    (overlay-put ov 'before-string
                 (propertize " " 'display
                             `((margin left-margin) ,text)))))

(defun emajjutsu-blame/blame-file (file)
  "Display a blame view for FILE in a new buffer.
Shows the content of FILE with blame annotations in the left margin.
The annotations include information about commit authors and timestamps.
The buffer is set up with appropriate margin width to accommodate the
annotations, and overlays are used to display blame information for
each line.  The original file's major mode is applied to the buffer."
  (let* ((blame-data (emajjutsu-blame--index (emajjutsu-core/annotate-blame-data file)))
	 (max-line-idx (length blame-data))
	 (current-line-idx 0))
    (switch-to-buffer (format "emajjutsu-blame: %s" file))
    (insert (emajjutsu-core/annotate-content file))
    (set-window-buffer nil (current-buffer))
    (setq-local left-margin-width (plist-get blame-data :max-line)) ;; Adjust width as needed
    (set-window-buffer (selected-window) (current-buffer))
    (remove-overlays (point-min) (point-max) 'emajjutsu-blame-margin t)
    (save-excursion
      (while (<= current-line-idx max-line-idx)
	(goto-char (point-min))
	(forward-line (- current-line-idx 1))
	(emajjutsu-blame--set-text (plist-get blame-data current-line-idx))
	(setq current-line-idx (+ current-line-idx 1))))
    ;; Force redisplay
    (redisplay t)
    (funcall (assoc-default file auto-mode-alist 'string-match))
    (goto-char (point-min))))











(provide 'emajjutsu-blame)
;;; emajjutsu-blame.el ends here
