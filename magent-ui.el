;;; magent-ui.el --- UI rendering for magent -*- lexical-binding: t; -*-

;;; Commentary:

;; magit-section based buffer rendering for the magent dashboard.

;;; Code:

(require 'magit-section)
(require 'magent-core)
(require 'magent-backend)
(require 'magit nil t)

;;; Faces

(defgroup magent-faces nil
  "Faces for magent."
  :group 'magent)

(defface magent-face-working
  '((t :inherit default))
  "Face for Work items in working state."
  :group 'magent-faces)

(defface magent-face-needs-input
  '((t :inherit warning :weight bold))
  "Face for Work items that need user input."
  :group 'magent-faces)

(defface magent-face-idle
  '((t :inherit shadow))
  "Face for idle Work items."
  :group 'magent-faces)

(defface magent-face-done
  '((t :inherit success))
  "Face for completed Work items."
  :group 'magent-faces)

(defface magent-face-repo
  '((t :inherit magit-section-heading))
  "Face for repo headers."
  :group 'magent-faces)

(defface magent-face-branch
  '((t :inherit magit-branch-local))
  "Face for branch names."
  :group 'magent-faces)

;;; Section types

(defclass magent-repo-section (magit-section) ()
  "Section for a repository.")

(defclass magent-work-section (magit-section) ()
  "Section for a Work item.")

(defclass magent-file-section (magit-section) ()
  "Section for a file within a Work item.")

;;; Helpers

(defun magent--state-face (state)
  "Return the face for STATE symbol."
  (pcase state
    ('working 'magent-face-working)
    ('needs-input 'magent-face-needs-input)
    ('idle 'magent-face-idle)
    ('done 'magent-face-done)
    (_ 'default)))

(defun magent--state-label (state)
  "Return display label for STATE."
  (pcase state
    ('working "working")
    ('needs-input "needs input")
    ('idle "idle")
    ('done "done")
    (_ "unknown")))

(defun magent--format-recent (work)
  "Format the most recent action for WORK."
  (if-let ((r (car (magent-work-recent work))))
      r
    ""))

;;; Section insertion

(defun magent--insert-work (work)
  "Insert a magit-section for WORK."
  (let ((state (magent-work-state work))
        (branch (or (magent-work-branch work)
                    (file-name-nondirectory
                     (directory-file-name (magent-work-dir work))))))
    (magit-insert-section (magent-work-section work)
      (let* ((state-face (magent--state-face state))
             (recent (magent--format-recent work))
             (heading (concat
                       (propertize (format "  %-25s" branch) 'face 'magent-face-branch)
                       (propertize (format "[%s]" (magent--state-label state)) 'face state-face)
                       (unless (string-empty-p recent)
                         (format "  %s" recent)))))
        (magit-insert-heading heading))
      ;; Files subsection
      (when (magent-work-files work)
        (dolist (file (magent-work-files work))
          (magit-insert-section (magent-file-section file)
            (insert (format "    %s\n" file)))))
      ;; PR info
      (when (magent-work-pr work)
        (insert (format "    PR: %s\n" (magent-work-pr work)))))))

(defun magent--insert-repo-section (repo works)
  "Insert a repo section for REPO with WORKS."
  (let ((todo-count (if (file-directory-p repo)
                        (magent-repo-todo-count repo)
                      0)))
    (magit-insert-section (magent-repo-section repo)
      (magit-insert-heading
        (concat
         (propertize (abbreviate-file-name repo) 'face 'magent-face-repo)
         (when (> todo-count 0)
           (format "  %d TODOs" todo-count))))
      (dolist (w works)
        (magent--insert-work w))
      (insert "\n"))))

;;; Buffer state

(defvar magent--works nil
  "Current list of Work structs.")

(defun magent-refresh-buffer ()
  "Refresh the *magent* buffer contents."
  (when (derived-mode-p 'magent-mode)
    (let ((inhibit-read-only t)
          (groups (magent-group-by-repo magent--works)))
      (magit-insert-section (magit-status-section)
        (magit-insert-heading "Magent\n")
        (if groups
            (dolist (group groups)
              (magent--insert-repo-section (car group) (cdr group)))
          (insert "\n  No active work. Press N to create one.\n"))))))

;;; Mode and keymap

(defvar magent-bindings
  '(("Navigation"
     ("TAB"   magit-section-toggle      "Toggle section")
     ("S-TAB" magit-section-toggle-children "Toggle all sections")
     ("n"     magit-section-forward      "Next section")
     ("p"     magit-section-backward     "Previous section"))
    ("Actions"
     ("RET"   magent-visit               "Visit (agent output or file)")
     ("i"     magent-send-input          "Send input to agent")
     ("r"     magent-resume              "Resume (work or all in repo)")
     ("k"     magent-kill-agent          "Kill agent session")
     ("D"     magent-mark-done           "Mark done / archive"))
    ("Git"
     ("c"     magent-tell-commit         "Tell agent to commit")
     ("P"     magent-tell-pr             "Tell agent to open PR")
     ("d"     magent-diff                "Diff (magit-diff in worktree)")
     ("f"     magent-fetch               "Fetch in worktree"))
    ("Tools"
     ("!"     magent-shell-command       "Shell command in worktree")
     ("$"     magent-show-process        "Show agent process buffer")
     ("l"     magent-event-log           "Agent event log")
     ("b"     magent-browse-backlog      "Browse backlog org files"))
    ("Global"
     ("N"     magent-new-work            "New work")
     ("g"     magent-refresh             "Refresh")
     ("?"     magent-help                "This help")))
  "Magent keybinding definitions. Each group is (HEADING (KEY COMMAND DESC)...).")

(defvar magent-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (dolist (group magent-bindings)
      (dolist (binding (cdr group))
        (let ((key (car binding))
              (cmd (cadr binding)))
          ;; Skip bindings already in parent map (TAB, S-TAB, n, p)
          (unless (lookup-key magit-section-mode-map (kbd key))
            (define-key map (kbd key) cmd)))))
    map)
  "Keymap for `magent-mode'.")

(define-derived-mode magent-mode magit-section-mode "Magent"
  "Major mode for the magent dashboard."
  :group 'magent
  (setq-local revert-buffer-function #'magent--revert-buffer)
  (setq buffer-read-only t))

(defun magent--revert-buffer (_ignore-auto _noconfirm)
  "Revert the magent buffer by refreshing."
  (magent-refresh))

(defun magent-refresh ()
  "Refresh the magent dashboard."
  (interactive)
  (when (derived-mode-p 'magent-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magent-refresh-buffer))))

;;; Section helpers

(defun magent--work-at-point ()
  "Return the Work struct at point, or nil."
  (when-let ((section (magit-current-section)))
    (cond
     ((magent-work-section-p section)
      (oref section value))
     ((magent-file-section-p section)
      (when-let ((parent (oref section parent)))
        (when (magent-work-section-p parent)
          (oref parent value))))
     (t nil))))

(defun magent--repo-at-point ()
  "Return the repo path at point, or nil."
  (when-let ((section (magit-current-section)))
    (cond
     ((magent-repo-section-p section)
      (oref section value))
     ((magent-work-section-p section)
      (magent-work-repo (oref section value)))
     (t nil))))

;;; Interactive commands

(defun magent-visit ()
  "Visit the thing at point -- file or agent output."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((magent-file-section-p section)
      (find-file (oref section value)))
     ((magent-work-section-p section)
      (magent-show-process))
     ((magent-repo-section-p section)
      (magent-browse-backlog)))))

(defun magent-show-process ()
  "Show the agent process buffer for Work at point."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work))
              (buf-name (format " *magent-proc-%s*" sid)))
    (if (get-buffer buf-name)
        (display-buffer buf-name)
      (message "No process buffer for this work."))))

(defun magent-shell-command ()
  "Run a shell command in the worktree of Work at point."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (let ((default-directory (magent-work-dir work)))
      (call-interactively #'shell-command))))

(defun magent-tell-commit ()
  "Tell the agent to commit its changes."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work)))
    (magent-backend-send sid "commit your changes")
    (message "Told agent to commit.")))

(defun magent-tell-pr ()
  "Tell the agent to open a PR."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work)))
    (magent-backend-send sid "open a pull request")
    (message "Told agent to open PR.")))

(defun magent-diff ()
  "Open magit-diff in the worktree at point."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (let ((default-directory (magent-work-dir work)))
      (magit-diff-unstaged))))

(defun magent-kill-agent ()
  "Kill the agent session for Work at point."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work)))
    (when (yes-or-no-p (format "Kill agent for %s? "
                               (magent-work-branch work)))
      (magent-backend-kill sid)
      (setf (magent-work-state work) 'idle)
      (magent-refresh))))

(defun magent-resume ()
  "Resume idle agent sessions.
On a Work section, resume that Work.
On a repo section, resume all idle Works under it."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((magent-repo-section-p section)
      (let ((repo (oref section value))
            (resumed 0))
        (dolist (work magent--works)
          (when (and (equal (magent-work-repo work) repo)
                     (magent-work-idle-p work)
                     (magent-work-session-id work))
            (magent-backend-resume (magent-work-session-id work))
            (setf (magent-work-state work) 'working)
            (cl-incf resumed)))
        (if (> resumed 0)
            (progn
              (message "Resumed %d agent%s." resumed (if (= resumed 1) "" "s"))
              (magent-refresh))
          (message "No idle sessions in %s." (abbreviate-file-name repo)))))
     (t
      (when-let* ((work (magent--work-at-point))
                  (sid (magent-work-session-id work)))
        (if (magent-work-idle-p work)
            (progn
              (magent-backend-resume sid)
              (setf (magent-work-state work) 'working)
              (magent-refresh))
          (message "Work is not idle.")))))))

(defun magent-browse-backlog ()
  "Open backlog org files for the repo at point."
  (interactive)
  (when-let ((repo (magent--repo-at-point)))
    (let ((org-files (file-expand-wildcards
                      (expand-file-name magent-backlog-glob repo))))
      (if org-files
          (if (= (length org-files) 1)
              (find-file (car org-files))
            (find-file (completing-read "Backlog: " org-files nil t)))
        (message "No org files found in %s" repo)))))

(defun magent-event-log ()
  "Show event log for Work at point."
  (interactive)
  (when-let* ((work (magent--work-at-point)))
    (let ((buf (get-buffer-create
                (format "*magent-log: %s*" (magent-work-branch work)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Event log for %s\n\n" (magent-work-branch work)))
          (dolist (r (reverse (magent-work-recent work)))
            (insert (format "  %s\n" r)))
          (when (null (magent-work-recent work))
            (insert "  No events recorded.\n")))
        (special-mode))
      (display-buffer buf))))

(defun magent-fetch ()
  "Run git fetch in the worktree at point."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (let ((default-directory (magent-work-dir work)))
      (if (fboundp 'magit-fetch-all)
          (magit-fetch-all (magit-fetch-arguments))
        (message "Running git fetch --all...")
        (shell-command "git fetch --all")))))

(defun magent-send-input ()
  "Send input to the agent for Work at point."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work)))
    (let ((input (read-string (format "Send to %s: "
                                      (magent-work-branch work)))))
      (magent-backend-send sid input)
      (setf (magent-work-state work) 'working)
      (magent-refresh))))

(defun magent-new-work ()
  "Create a new Work item."
  (interactive)
  (let* ((dir (read-directory-name "Worktree directory: "))
         (purpose (read-string "Purpose: "))
         (prompt (read-string "Agent prompt (or empty to skip launch): "))
         (work (magent-work-create :dir dir :purpose purpose)))
    (push work magent--works)
    (unless (string-empty-p prompt)
      (let ((sid (magent-backend-launch dir prompt)))
        (setf (magent-work-session-id work) sid)
        (setf (magent-work-state work) 'working)
        (puthash sid work magent--session-works)))
    (magent-state-save magent--works)
    (magent-refresh)))

(defun magent-mark-done ()
  "Mark Work at point as done."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (setf (magent-work-state work) 'done)
    (magent-state-save magent--works)
    (magent-refresh)))

(defun magent-help ()
  "Show magent keybindings, generated from `magent-bindings'."
  (interactive)
  (let ((buf (get-buffer-create "*magent-help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Magent keybindings\n\n")
        (dolist (group magent-bindings)
          (insert (car group) "\n")
          (dolist (binding (cdr group))
            (insert (format "  %-9s %s\n" (car binding) (caddr binding))))
          (insert "\n")))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

(provide 'magent-ui)
;;; magent-ui.el ends here
