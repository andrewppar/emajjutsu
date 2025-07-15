;;; emajjutsu-face.el -- jj colors -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; Make some faces for emajjutsu

;;; Code:
(defconst emajjutsu-face/added-file (list :foreground "#a6e3a1"))
(defconst emajjutsu-face/author (list :foreground "#f9e2af"))
(defconst emajjutsu-face/bookmark (list :foreground "#f5c2e7"))
(defconst emajjutsu-face/conflict (list :foreground "#f38ba8"))
(defconst emajjutsu-face/copied-file (list :foreground "#a6e3a1"))
(defconst emajjutsu-face/current (list :foreground "#a6e3a1"))
(defconst emajjutsu-face/deleted-file (list :foreground "#f38ba8"))
(defconst emajjutsu-face/modified-file (list :foreground "#94e2d5"))
(defconst emajjutsu-face/immutable (list :foreground "#94e2d5"))
(defconst emajjutsu-face/empty-change (list :foreground "#a6e3a1"))
(defconst emajjutsu-face/empty-description (list :foreground "#f9e2af"))
(defconst emajjutsu-face/description (list :foreground "#cdd6f4"))
(defconst emajjutsu-face/change-short (list :weight 'bold :foreground "#f5c2e7"))
(defconst emajjutsu-face/commit-short (list :weight 'bold :foreground "#89b4fa"))
(defconst emajjutsu-face/error (list :foreground "#f38ba8"))
(defconst emajjutsu-face/warning (list :weight 'bold :foreground "#f9e2af"))
(defconst emajjutsu-face/commit-or-change (list :foreground "#6c7086"))

(provide 'emajjutsu-face)
;;; emajjutsu-face.el ends here
