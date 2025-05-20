;;; emajutsu.el -- jujutsu bindings for emacs -*- lexical-binding: t -*-

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
(require 'emajutsu-status)


(defun emajutsu/status (&optional change-id)
  "View the status of CHANGE-ID."
  (interactive
   (list
    (read-string "change: ")))
  (let ((id (if (equal change-id "") "@" change-id)))
    (emajutsu-status/status id)))

(provide 'emajutsu)
;;; emajutsu.el ends here
