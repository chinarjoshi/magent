;;; magent.el --- Magit-style porcelain for AI agents -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magent contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit-section "4.0"))
;; Keywords: tools, processes
;; URL: https://github.com/chijoshi/magent

;;; Commentary:

;; Magent is a magit-style interface for orchestrating AI agent sessions
;; across repositories and worktrees.
;;
;; Use `M-x magent' to open the dashboard.

;;; Code:

(require 'magent-core)
(require 'magent-backend)
(require 'magent-ui)

(defgroup magent nil
  "Magit-style porcelain for AI agents."
  :group 'tools
  :prefix "magent-")

;;;###autoload
(defun magent ()
  "Open the magent dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*magent*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'magent-mode)
        (magent-mode))
      (unless magent--works
        (setq magent--works (or (magent-state-load) nil)))
      (magent-refresh))
    (switch-to-buffer buf)))

(provide 'magent)
;;; magent.el ends here
