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
;;
;; This file exists only as the package entry point.
;; All code lives in magent-core.el, magent-backend.el, and magent-ui.el.

;;; Code:

(require 'magent-ui)

(provide 'magent)
;;; magent.el ends here
