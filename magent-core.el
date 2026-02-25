;;; magent-core.el --- Core data model for magent -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magent contributors
;; Package-Requires: ((emacs "29.1") (magit-section "4.0"))

;;; Commentary:

;; Work struct, state management, and persistence for magent.

;;; Code:

(require 'cl-lib)

(cl-defstruct (magent-work (:constructor magent-work--internal-create)
                           (:copier nil))
  "A piece of work: one worktree with one agent session."
  (dir nil :type string :documentation "Worktree path.")
  (repo nil :type (or string null) :documentation "Repo root, derived from dir.")
  (branch nil :type (or string null) :documentation "Git branch, derived from dir.")
  (purpose "" :type string :documentation "What the agent is doing.")
  (state 'idle :type symbol :documentation "One of: working, needs-input, idle, done.")
  (session-id nil :type (or string null) :documentation "Backend session ID.")
  (pr nil :type (or string null) :documentation "PR URL.")
  (files nil :type list :documentation "Files the agent is currently touching.")
  (recent nil :type list :documentation "Recent actions/tool calls."))

(defun magent-work-create (&rest args)
  "Create a Work, deriving repo and branch from :dir."
  (let ((w (apply #'magent-work--internal-create args)))
    (when (magent-work-dir w)
      (let ((dir (expand-file-name (magent-work-dir w))))
        (setf (magent-work-dir w) dir)
        (setf (magent-work-repo w) (magent--git-repo-root dir))
        (setf (magent-work-branch w) (magent--git-branch dir))))
    w))

(defun magent--git-repo-root (dir)
  "Return the git repo root for DIR, or nil."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (condition-case nil
          (file-name-as-directory
           (string-trim
            (shell-command-to-string "git rev-parse --show-toplevel")))
        (error nil)))))

(defun magent--git-branch (dir)
  "Return the current git branch for DIR, or nil."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (condition-case nil
          (let ((branch (string-trim
                         (shell-command-to-string
                          "git rev-parse --abbrev-ref HEAD"))))
            (unless (string-empty-p branch) branch))
        (error nil)))))

;; State predicates

(defun magent-work-working-p (work)
  "Return non-nil if WORK is in working state."
  (eq (magent-work-state work) 'working))

(defun magent-work-needs-input-p (work)
  "Return non-nil if WORK needs user input."
  (eq (magent-work-state work) 'needs-input))

(defun magent-work-idle-p (work)
  "Return non-nil if WORK is idle."
  (eq (magent-work-state work) 'idle))

(defun magent-work-done-p (work)
  "Return non-nil if WORK is done."
  (eq (magent-work-state work) 'done))

(provide 'magent-core)
;;; magent-core.el ends here
