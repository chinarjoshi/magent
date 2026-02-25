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
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
          (file-name-as-directory (string-trim (buffer-string))))))))

(defun magent--git-branch (dir)
  "Return the current git branch for DIR, or nil."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD"))
          (let ((branch (string-trim (buffer-string))))
            (unless (string-empty-p branch) branch)))))))

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

;; Persistence

(defcustom magent-state-file
  (expand-file-name "magent/state.el" user-emacs-directory)
  "Path to persist Work state."
  :type 'file
  :group 'magent)

(defun magent--work-to-plist (work)
  "Serialize WORK to a plist for persistence."
  (list :dir (magent-work-dir work)
        :repo (magent-work-repo work)
        :branch (magent-work-branch work)
        :purpose (magent-work-purpose work)
        :state (magent-work-state work)
        :session-id (magent-work-session-id work)
        :pr (magent-work-pr work)))

(defun magent--plist-to-work (plist)
  "Deserialize a PLIST to a Work struct."
  (let ((work (magent-work--internal-create
               :dir (plist-get plist :dir)
               :repo (plist-get plist :repo)
               :branch (plist-get plist :branch)
               :purpose (plist-get plist :purpose)
               :state (plist-get plist :state)
               :session-id (plist-get plist :session-id)
               :pr (plist-get plist :pr))))
    ;; Re-derive repo/branch if missing (old state files)
    (when (and (magent-work-dir work)
               (null (magent-work-repo work)))
      (setf (magent-work-repo work)
            (magent--git-repo-root (magent-work-dir work)))
      (setf (magent-work-branch work)
            (magent--git-branch (magent-work-dir work))))
    work))

(defun magent-state-save (works)
  "Save WORKS list to `magent-state-file'."
  (let ((dir (file-name-directory magent-state-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file magent-state-file
    (insert ";; -*- lisp-data -*-\n")
    (insert ";; Magent state — do not edit by hand.\n")
    (pp (mapcar #'magent--work-to-plist works) (current-buffer))))

(defun magent-state-load ()
  "Load Works from `magent-state-file'.  Return list or nil."
  (when (file-exists-p magent-state-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents magent-state-file)
          (mapcar #'magent--plist-to-work (read (current-buffer))))
      (error nil))))

(defcustom magent-backlog-glob "*.org"
  "Glob pattern for finding backlog org files in repo roots."
  :type 'string
  :group 'magent)

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

(defun magent-count-todos-in-file (file)
  "Count TODO headings in an org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(TODO\\|NEXT\\|WAITING\\) " nil t)
        (cl-incf count))
      count)))

(defun magent-repo-todo-count (repo-dir)
  "Count total TODOs across org files in REPO-DIR root."
  (let ((org-files (file-expand-wildcards
                    (expand-file-name magent-backlog-glob repo-dir)))
        (total 0))
    (dolist (f org-files)
      (when (file-regular-p f)
        (cl-incf total (magent-count-todos-in-file f))))
    total))

;;; Session discovery from ~/.claude/projects/

(require 'json)

(defcustom magent-claude-projects-dir
  (expand-file-name "projects" (expand-file-name ".claude" "~"))
  "Directory where Claude Code stores project sessions."
  :type 'directory
  :group 'magent)

(defun magent--session-metadata (jsonl-file)
  "Extract metadata from the first user message in JSONL-FILE.
Returns alist with session-id, cwd, branch, timestamp, prompt."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents jsonl-file nil 0 8192) ; read first 8K
        (goto-char (point-min))
        (let (result)
          (while (and (not result) (not (eobp)))
            (let* ((line (buffer-substring (line-beginning-position)
                                           (line-end-position)))
                   (msg (condition-case nil
                            (json-read-from-string line)
                          (error nil))))
              (when (and msg (equal (alist-get 'type msg) "user"))
                (setq result
                      (list (cons 'session-id (alist-get 'sessionId msg))
                            (cons 'cwd (alist-get 'cwd msg))
                            (cons 'branch (alist-get 'gitBranch msg))
                            (cons 'timestamp (alist-get 'timestamp msg))
                            (cons 'prompt
                                  (let ((content (alist-get 'content
                                                            (alist-get 'message msg))))
                                    (when (stringp content)
                                      (truncate-string-to-width content 200))))))))
            (forward-line 1))
          result))
    (error nil)))

(defun magent-discover-sessions (&optional filter-dirs)
  "Discover Claude sessions from `magent-claude-projects-dir'.
Scans all project directories, extracts cwd from session metadata.
If FILTER-DIRS is non-nil, only return sessions whose cwd is under
one of those directories.
Returns list of Work structs (state idle, with session-id)."
  (let ((works nil)
        (seen-cwds (make-hash-table :test 'equal)))
    (when (file-directory-p magent-claude-projects-dir)
      (dolist (proj-name (directory-files magent-claude-projects-dir nil "^-"))
        (let ((proj-dir (expand-file-name proj-name magent-claude-projects-dir)))
          (when (file-directory-p proj-dir)
            ;; Find most recent session file
            (let* ((jsonls (directory-files proj-dir t "\\.jsonl$"))
                   (sorted (sort jsonls
                                 (lambda (a b)
                                   (time-less-p (file-attribute-modification-time
                                                 (file-attributes b))
                                                (file-attribute-modification-time
                                                 (file-attributes a))))))
                   (latest (car sorted)))
              (when latest
                (when-let ((meta (magent--session-metadata latest)))
                  (let ((cwd (alist-get 'cwd meta))
                        (sid (alist-get 'session-id meta))
                        (branch (alist-get 'branch meta)))
                    (when (and cwd
                               (file-directory-p cwd)
                               (not (gethash cwd seen-cwds))
                               (or (null filter-dirs)
                                   (cl-some (lambda (d)
                                              (string-prefix-p
                                               (expand-file-name d) cwd))
                                            filter-dirs)))
                      (puthash cwd t seen-cwds)
                      (push (cons (file-attribute-modification-time
                                   (file-attributes latest))
                                  (magent-work--internal-create
                                   :dir cwd
                                   :repo (magent--git-repo-root cwd)
                                   :branch (or branch (magent--git-branch cwd))
                                   :purpose (or (alist-get 'prompt meta) "")
                                   :state 'idle
                                   :session-id sid))
                            works))))))))))
    ;; Sort by recency (most recent first), then strip timestamps
    (mapcar #'cdr
            (sort works (lambda (a b)
                          (time-less-p (car b) (car a)))))))

(provide 'magent-core)
;;; magent-core.el ends here
