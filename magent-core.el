;;; magent-core.el --- Core data model for magent -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'json)

(defgroup magent nil
  "Magit-style porcelain for AI agents."
  :group 'tools
  :prefix "magent-")

;;; Work struct — the single core object

(cl-defstruct (magent-work (:copier nil))
  "A piece of work: one worktree with one agent session."
  (dir nil :type string)
  (repo nil :type (or string null))
  (branch nil :type (or string null))
  (purpose "" :type string)
  (state 'idle :type symbol)          ; working | idle | done
  (session-id nil :type (or string null))
  (pr nil :type (or string null))
  (files nil :type list)
  (recent nil :type list)
  (last-output nil :type (or string null))
  (start-commit nil :type (or string null)))

;;; Git helpers

(defun magent--git (dir &rest args)
  "Run git ARGS in DIR, return trimmed output or nil on error."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (with-temp-buffer
        (when (zerop (apply #'call-process "git" nil t nil args))
          (let ((out (string-trim (buffer-string))))
            (unless (string-empty-p out) out)))))))

(defun magent--git-repo-root (dir)
  "Return git repo root for DIR, or nil."
  (when-let ((root (magent--git dir "rev-parse" "--show-toplevel")))
    (file-name-as-directory root)))

(defun magent--git-branch (dir)
  "Return current git branch for DIR, or nil."
  (magent--git dir "rev-parse" "--abbrev-ref" "HEAD"))

(defun magent--git-head (dir)
  "Return current HEAD SHA for DIR, or nil."
  (magent--git dir "rev-parse" "HEAD"))

(defun magent--branch-merged-p (dir branch)
  "Return non-nil if BRANCH is merged into the default branch in DIR."
  (when (and dir branch (file-directory-p dir)
             (not (member branch '("main" "master" "HEAD"))))
    (let* ((default-directory dir)
           (default-branch (or (magent--git dir "symbolic-ref" "refs/remotes/origin/HEAD")
                               "refs/remotes/origin/main"))
           (default-branch (replace-regexp-in-string
                            "^refs/remotes/origin/" "" default-branch)))
      (with-temp-buffer
        (zerop (call-process "git" nil t nil
                             "merge-base" "--is-ancestor"
                             branch default-branch))))))

;;; State

(defun magent-work-working-p (w) (eq (magent-work-state w) 'working))
(defun magent-work-idle-p (w) (eq (magent-work-state w) 'idle))
(defun magent-work-done-p (w) (eq (magent-work-state w) 'done))

(defun magent-auto-archive-merged (works)
  "Mark WORKS whose branches have been merged as done."
  (dolist (w works)
    (when (and (not (magent-work-done-p w))
               (magent--branch-merged-p (magent-work-repo w)
                                        (magent-work-branch w)))
      (setf (magent-work-state w) 'done))))

;;; Persistence — just overrides, not the full work list.
;; Discovery rebuilds the work list each time. We only persist:
;; - start-commit (recorded when user creates work)
;; - done state (manual archive)
;; - purpose overrides (user-provided, better than JSONL prompt)

(defcustom magent-state-file
  (expand-file-name "magent/state.el" user-emacs-directory)
  "Path to persist Work overrides."
  :type 'file
  :group 'magent)

(defun magent-state-save (works)
  "Save overrides for WORKS to `magent-state-file'."
  (let ((dir (file-name-directory magent-state-file))
        (overrides nil))
    (unless (file-directory-p dir) (make-directory dir t))
    (dolist (w works)
      ;; Only persist works with user-set data
      (when (or (magent-work-done-p w)
                (magent-work-start-commit w))
        (push (list :dir (magent-work-dir w)
                    :state (magent-work-state w)
                    :start-commit (magent-work-start-commit w))
              overrides)))
    (with-temp-file magent-state-file
      (insert ";; -*- lisp-data -*-\n")
      (pp (nreverse overrides) (current-buffer)))))

(defun magent-state-load ()
  "Load overrides from `magent-state-file'. Return alist keyed by dir."
  (when (file-exists-p magent-state-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents magent-state-file)
          (let ((plists (read (current-buffer)))
                (table (make-hash-table :test 'equal)))
            (dolist (p plists)
              (puthash (plist-get p :dir) p table))
            table))
      (error nil))))

(defun magent--apply-overrides (works overrides)
  "Apply OVERRIDES hash table to WORKS list."
  (when overrides
    (dolist (w works)
      (when-let ((ov (gethash (magent-work-dir w) overrides)))
        (when (plist-get ov :start-commit)
          (setf (magent-work-start-commit w) (plist-get ov :start-commit)))
        (when (eq (plist-get ov :state) 'done)
          (setf (magent-work-state w) 'done))))))

;;; Grouping

(defun magent-group-by-repo (works)
  "Group WORKS by repo. Return alist of (repo . works)."
  (let ((groups nil))
    (dolist (w works)
      (let* ((repo (or (magent-work-repo w) "unknown"))
             (cell (assoc repo groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list w)))
          (push (cons repo (list w)) groups))))
    (nreverse groups)))

;;; Backlog

(defcustom magent-backlog-glob "*.org"
  "Glob pattern for finding backlog org files in repo roots."
  :type 'string
  :group 'magent)

(defun magent-count-todos-in-file (file)
  "Count TODO/NEXT/WAITING headings in an org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(TODO\\|NEXT\\|WAITING\\) " nil t)
        (cl-incf count))
      count)))

(defun magent-repo-todo-count (repo-dir)
  "Count total TODOs across org files in REPO-DIR root."
  (let ((total 0))
    (dolist (f (file-expand-wildcards
                (expand-file-name magent-backlog-glob repo-dir)))
      (when (file-regular-p f)
        (cl-incf total (magent-count-todos-in-file f))))
    total))

;;; Session discovery from ~/.claude/projects/

(defcustom magent-claude-projects-dir
  (expand-file-name "projects" (expand-file-name ".claude" "~"))
  "Directory where Claude Code stores project sessions."
  :type 'directory
  :group 'magent)

(defcustom magent-discover-dirs nil
  "Directories to limit session discovery. Nil means discover all."
  :type '(repeat directory)
  :group 'magent)

(defun magent--extract-text (message)
  "Extract text content from a MESSAGE alist."
  (let ((content (alist-get 'content (alist-get 'message message))))
    (cond
     ((stringp content) content)
     ((or (vectorp content) (listp content))
      (mapconcat (lambda (b)
                   (when (equal (alist-get 'type b) "text")
                     (alist-get 'text b)))
                 content ""))
     (t nil))))

(defun magent--session-metadata (jsonl-file)
  "Extract metadata from JSONL-FILE (reads head + tail, not whole file)."
  (condition-case nil
      (let* ((size (file-attribute-size (file-attributes jsonl-file)))
             (head-size (min 8192 size))
             (tail-size (min 32768 size))
             (head (with-temp-buffer
                     (insert-file-contents jsonl-file nil 0 head-size)
                     (buffer-string)))
             (tail (when (> size (+ head-size tail-size))
                     (with-temp-buffer
                       (insert-file-contents jsonl-file nil (- size tail-size) size)
                       (buffer-string))))
             (first-user nil) (last-user nil) (last-assistant nil))
        ;; Parse head for session identity
        (with-temp-buffer
          (insert head)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((msg (condition-case nil
                           (json-read-from-string
                            (buffer-substring (line-beginning-position) (line-end-position)))
                         (error nil))))
              (when msg
                (pcase (alist-get 'type msg)
                  ("user" (unless first-user (setq first-user msg)) (setq last-user msg))
                  ("assistant" (setq last-assistant msg)))))
            (forward-line 1)))
        ;; Parse tail for recent messages
        (when tail
          (with-temp-buffer
            (insert tail)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((msg (condition-case nil
                             (json-read-from-string
                              (buffer-substring (line-beginning-position) (line-end-position)))
                           (error nil))))
                (when msg
                  (pcase (alist-get 'type msg)
                    ("user" (setq last-user msg))
                    ("assistant" (setq last-assistant msg)))))
              (forward-line 1))))
        (when first-user
          (let ((prompt (magent--extract-text (or last-user first-user)))
                (output (magent--extract-text last-assistant)))
            (list (cons 'session-id (alist-get 'sessionId first-user))
                  (cons 'cwd (alist-get 'cwd first-user))
                  (cons 'branch (alist-get 'gitBranch first-user))
                  (cons 'prompt (when (stringp prompt)
                                  (truncate-string-to-width prompt 200)))
                  (cons 'last-output (when (stringp output) output))))))
    (error nil)))

(defun magent-discover-sessions ()
  "Discover Claude sessions. Returns list of Work structs.
Respects `magent-discover-dirs' for filtering."
  (let ((works nil)
        (seen (make-hash-table :test 'equal)))
    (when (file-directory-p magent-claude-projects-dir)
      (dolist (name (directory-files magent-claude-projects-dir nil "^-"))
        (let ((dir (expand-file-name name magent-claude-projects-dir)))
          (when (file-directory-p dir)
            (let* ((jsonls (directory-files dir t "\\.jsonl$"))
                   (latest (car (sort jsonls
                                      (lambda (a b)
                                        (time-less-p
                                         (file-attribute-modification-time (file-attributes b))
                                         (file-attribute-modification-time (file-attributes a))))))))
              (when latest
                (when-let* ((meta (magent--session-metadata latest))
                            (cwd (alist-get 'cwd meta))
                            (sid (alist-get 'session-id meta)))
                  (when (and (file-directory-p cwd)
                             (not (gethash cwd seen))
                             (or (null magent-discover-dirs)
                                 (cl-some (lambda (d)
                                            (string-prefix-p (expand-file-name d) cwd))
                                          magent-discover-dirs)))
                    (puthash cwd t seen)
                    (push (cons (file-attribute-modification-time (file-attributes latest))
                                (make-magent-work
                                 :dir cwd
                                 :repo (magent--git-repo-root cwd)
                                 :branch (or (alist-get 'branch meta) (magent--git-branch cwd))
                                 :purpose (or (alist-get 'prompt meta) "")
                                 :state 'idle
                                 :session-id sid
                                 :last-output (alist-get 'last-output meta)))
                          works)))))))))
    (mapcar #'cdr (sort works (lambda (a b) (time-less-p (car b) (car a)))))))

(provide 'magent-core)
;;; magent-core.el ends here
