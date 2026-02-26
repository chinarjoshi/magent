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
  '((((class color) (background dark)) :foreground "#3fb950")
    (((class color) (background light)) :foreground "#1a7f37")
    (t :inherit success))
  "Face for working state (GitHub Open green)."
  :group 'magent-faces)

(defface magent-face-needs-input
  '((((class color) (background dark)) :foreground "#e5c07b" :weight bold)
    (((class color) (background light)) :foreground "#986801" :weight bold)
    (t :inherit warning :weight bold))
  "Face for needs-input state."
  :group 'magent-faces)

(defface magent-face-idle
  '((t :inherit shadow))
  "Face for idle state."
  :group 'magent-faces)

(defface magent-face-done
  '((((class color) (background dark)) :foreground "#c678dd")
    (((class color) (background light)) :foreground "#a626a4")
    (t :inherit success))
  "Face for done state."
  :group 'magent-faces)

(defface magent-face-repo
  '((t :inherit magit-section-heading :weight bold))
  "Face for repo names."
  :group 'magent-faces)

(defface magent-face-archive
  '((t :inherit shadow))
  "Face for archived items."
  :group 'magent-faces)

;;; Section types

(defclass magent-repo-section (magit-section) ()
  "Section for a repository.")

(defclass magent-work-section (magit-section) ()
  "Section for a Work item.")

(defclass magent-file-section (magit-section) ()
  "Section for a file within a Work item.")

(defclass magent-active-section (magit-section) ()
  "Top-level section for active work.")

(defclass magent-archive-section (magit-section) ()
  "Top-level section for archived/done work.")

;;; Helpers

(defun magent--state-face (state)
  "Return the face for STATE."
  (pcase state
    ('working 'magent-face-working)
    ('needs-input 'magent-face-needs-input)
    ('idle 'magent-face-idle)
    ('done 'magent-face-done)
    (_ 'shadow)))

(defun magent--status-label (state)
  "Return display label for STATE."
  (pcase state
    ('working "working")
    ('needs-input "needs input")
    ('idle "idle")
    ('done "done")
    (_ "unknown")))

(defun magent--repo-name (repo)
  "Return just the directory name from REPO path."
  (if (or (null repo) (equal repo "unknown"))
      "unknown"
    (file-name-nondirectory (directory-file-name repo))))

(defun magent--purpose-oneliner (purpose)
  "Return a clean one-liner from PURPOSE string, or nil."
  (when (and purpose (not (string-empty-p purpose)))
    (let* ((clean (replace-regexp-in-string
                   "<[^>]+>" "" purpose))
           (clean (replace-regexp-in-string
                   "\\`[ \t\n]+" "" clean))
           (first-line (car (split-string clean "\n" t))))
      (when (and first-line (not (string-empty-p first-line)))
        (truncate-string-to-width first-line 60)))))

(defun magent--max-branch-width (works)
  "Return the max branch name length across all WORKS."
  (let ((max-w 0))
    (dolist (w works)
      (let* ((branch (or (magent-work-branch w)
                         (file-name-nondirectory
                          (directory-file-name (magent-work-dir w)))))
             (len (length branch)))
        (when (> len max-w) (setq max-w len))))
    max-w))

;;; Section insertion

(defun magent--insert-work (work col)
  "Insert a magit-section for WORK with status at column COL."
  (let* ((state (magent-work-state work))
         (branch (or (magent-work-branch work)
                     (file-name-nondirectory
                      (directory-file-name (magent-work-dir work)))))
         (oneliner (magent--purpose-oneliner (magent-work-purpose work)))
         (pad (make-string (max 1 (- col (length branch))) ?\s)))
    (magit-insert-section (magent-work-section work t)
      (magit-insert-heading
        (concat
         (propertize (format "  %s" branch) 'font-lock-face (magent--state-face state))
         pad
         (propertize (format "[%s]" (magent--status-label state))
                     'font-lock-face (magent--state-face state))
         (when oneliner
           (propertize (format "  %s" oneliner) 'font-lock-face 'shadow))))
      ;; Foldable body — first level: dir + last prompt (1-2 lines)
      (insert (propertize (format "    %s\n" (abbreviate-file-name (magent-work-dir work)))
                          'font-lock-face 'shadow))
      ;; Last prompt — max 2 lines
      (when-let ((purpose (magent-work-purpose work)))
        (unless (string-empty-p purpose)
          (let* ((clean (replace-regexp-in-string "\\`[ \t\n]+" ""
                                                   (replace-regexp-in-string "<[^>]+>" "" purpose)))
                 (lines (split-string clean "\n" t))
                 (display (mapconcat #'identity (seq-take lines 2) "\n    ")))
            (insert (propertize (format "    > %s\n"
                                        (truncate-string-to-width display 120))
                                'font-lock-face 'shadow)))))
      ;; Files
      (dolist (file (magent-work-files work))
        (magit-insert-section (magent-file-section file)
          (insert (format "    %s\n" file))))
      ;; PR
      (when (magent-work-pr work)
        (insert (propertize (format "    PR: %s\n" (magent-work-pr work))
                            'font-lock-face 'shadow)))
      ;; Last output — deeper fold
      (when-let ((output (magent-work-last-output work)))
        (unless (string-empty-p output)
          (magit-insert-section (magit-section 'output t)
            (magit-insert-heading
              (propertize "    Agent output" 'font-lock-face 'shadow))
            (let* ((lines (split-string output "\n"))
                   (display (mapconcat #'identity (seq-take lines 30) "\n")))
              (insert (propertize
                       (replace-regexp-in-string
                        "^" "      "
                        (truncate-string-to-width display 2000))
                       'font-lock-face 'shadow))
              (insert "\n"))))))))

(defun magent--insert-archive-work (work)
  "Insert a single archive line for WORK."
  (magit-insert-section (magent-work-section work)
    (magit-insert-heading
      (concat
       (propertize (format "  %s" (magent--repo-name (magent-work-repo work)))
                   'font-lock-face 'magent-face-archive)
       (propertize " - " 'font-lock-face 'shadow)
       (propertize (or (magent-work-branch work) "?")
                   'font-lock-face 'magent-face-archive)))))

(defun magent--insert-repo-section (repo works col)
  "Insert a repo section for REPO with WORKS, status at COL."
  (let ((todo-count (if (file-directory-p repo)
                        (magent-repo-todo-count repo)
                      0))
        (name (magent--repo-name repo)))
    (magit-insert-section (magent-repo-section repo)
      (magit-insert-heading
        (concat
         (propertize name 'font-lock-face 'magent-face-repo)
         (when (> todo-count 0)
           (propertize (format "  %d TODOs" todo-count) 'font-lock-face 'shadow))))
      (dolist (w works)
        (magent--insert-work w col))
      (insert "\n"))))

;;; Buffer state

(defvar magent--works nil
  "Current list of Work structs.")

(defun magent-refresh-buffer ()
  "Refresh the *magent* buffer contents."
  (when (derived-mode-p 'magent-mode)
    ;; Auto-archive merged branches
    (magent-auto-archive-merged magent--works)
    (let* ((inhibit-read-only t)
           (active (seq-filter (lambda (w) (not (magent-work-done-p w))) magent--works))
           (archive (seq-filter #'magent-work-done-p magent--works))
           (active-groups (magent-group-by-repo active))
           ;; Compute column alignment across ALL active works
           (col (+ 2 (magent--max-branch-width active))))
      (magit-insert-section (magit-status-section)
        ;; Active section
        (magit-insert-section (magent-active-section 'active)
          (magit-insert-heading
            (propertize (format "Active (%d)" (length active))
                        'font-lock-face 'magit-section-heading))
          (if active-groups
              (dolist (group active-groups)
                (magent--insert-repo-section (car group) (cdr group) col))
            (insert "\n  No active work. Press N to create one.\n\n")))
        ;; Archive section
        (when archive
          (magit-insert-section (magent-archive-section 'archive t)
            (magit-insert-heading
              (propertize (format "Archive (%d)" (length archive))
                          'font-lock-face 'magit-section-heading))
            (dolist (w archive)
              (magent--insert-archive-work w))
            (insert "\n")))))))

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
     ("d"     magent-diff                "Diff unstaged")
     ("="     magent-diff-since-start    "Diff since agent started")
     ("f"     magent-fetch               "Fetch in worktree")
     ("w"     magent-new-worktree        "New worktree + agent"))
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
          (unless (lookup-key magit-section-mode-map (kbd key))
            (define-key map (kbd key) cmd)))))
    ;; For non-Evil users
    (unless (featurep 'evil)
      (define-key map (kbd "g") #'magent-refresh)
      (define-key map (kbd "n") #'magit-section-forward)
      (define-key map (kbd "p") #'magit-section-backward))
    map)
  "Keymap for `magent-mode'.")

(define-derived-mode magent-mode magit-section-mode "Magent"
  "Major mode for the magent dashboard."
  :group 'magent
  (setq-local revert-buffer-function #'magent--revert-buffer)
  (setq buffer-read-only t))

;; Evil integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'magent-mode 'normal)
  (evil-define-key* 'normal magent-mode-map
    "q" #'quit-window
    "j" #'magit-section-forward
    "k" #'magit-section-backward
    "J" #'magit-section-forward-sibling
    "K" #'magit-section-backward-sibling
    (kbd "C-j") #'magit-section-forward
    (kbd "C-k") #'magit-section-backward
    "gg" #'evil-goto-first-line
    "G" #'evil-goto-line
    (kbd "C-d") #'evil-scroll-down
    (kbd "C-u") #'evil-scroll-up
    "/" #'evil-search-forward
    "n" #'evil-search-next
    "N" #'evil-search-previous
    "gr" #'magent-refresh
    "gR" #'magent-refresh))

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
  "Visit the thing at point.
On a file section, open the file.
On a work section, open the agent CLI via claude-code.el.
On a repo section, browse backlog."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((magent-file-section-p section)
      (find-file (oref section value)))
     ((magent-work-section-p section)
      (magent-open-agent))
     ((magent-repo-section-p section)
      (magent-browse-backlog)))))

(defun magent-open-agent ()
  "Open the agent CLI for Work at point via claude-code.el."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (let ((default-directory (magent-work-dir work))
          (sid (magent-work-session-id work)))
      (if (fboundp 'claude-code--start)
          (claude-code--start nil
                              (when sid (list "--resume" sid))
                              nil t)
        (if sid
            (let* ((buf-name (format "*magent-agent-%s*"
                                     (or (magent-work-branch work) "agent")))
                   (buf (make-comint-in-buffer
                         buf-name nil
                         magent-agent-command nil
                         "--resume" sid)))
              (switch-to-buffer-other-window buf))
          (message "No session to resume. Use N to create one."))))))

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

(defun magent--ensure-session-registered (work)
  "Ensure WORK is registered in `magent--session-works' for backend access."
  (when-let ((sid (magent-work-session-id work)))
    (unless (gethash sid magent--session-works)
      (puthash sid work magent--session-works))))

(defun magent-tell-commit ()
  "Tell the agent to commit its changes."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work)))
    (magent--ensure-session-registered work)
    (magent-backend-send sid "commit your changes")
    (setf (magent-work-state work) 'working)
    (magent-refresh)
    (message "Told agent to commit.")))

(defun magent-tell-pr ()
  "Tell the agent to open a PR."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work)))
    (magent--ensure-session-registered work)
    (magent-backend-send sid "open a pull request")
    (setf (magent-work-state work) 'working)
    (magent-refresh)
    (message "Told agent to open PR.")))

(defun magent-diff ()
  "Open magit-diff in the worktree at point."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (let ((default-directory (magent-work-dir work)))
      (if (fboundp 'magit-diff-unstaged)
          (magit-diff-unstaged)
        (message "Running git diff...")
        (shell-command "git diff")))))

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
            (magent--ensure-session-registered work)
            (magent-backend-resume (magent-work-session-id work))
            (setf (magent-work-state work) 'working)
            (cl-incf resumed)))
        (if (> resumed 0)
            (progn
              (message "Resumed %d agent%s." resumed (if (= resumed 1) "" "s"))
              (magent-refresh))
          (message "No idle sessions in %s." (magent--repo-name repo)))))
     (t
      (when-let* ((work (magent--work-at-point))
                  (sid (magent-work-session-id work)))
        (if (magent-work-idle-p work)
            (progn
              (magent--ensure-session-registered work)
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
    (magent--ensure-session-registered work)
    (let ((input (read-string (format "Send to %s: "
                                      (magent-work-branch work)))))
      (magent-backend-send sid input)
      (setf (magent-work-state work) 'working)
      (magent-refresh))))

(defun magent-diff-since-start ()
  "Show diff from when the agent started to current HEAD."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (let ((default-directory (magent-work-dir work))
          (start (magent-work-start-commit work)))
      (cond
       ((and start (fboundp 'magit-diff-range))
        (magit-diff-range (format "%s..HEAD" start)))
       (start
        (shell-command (format "git diff %s..HEAD" start)))
       (t
        (message "No start commit recorded. Use d for unstaged diff."))))))

(defun magent-new-work ()
  "Create a new Work item from an existing directory."
  (interactive)
  (let* ((dir (read-directory-name "Worktree directory: "))
         (purpose (read-string "Purpose: "))
         (prompt (read-string "Agent prompt (or empty to skip launch): "))
         (start-commit (magent--git-head dir))
         (work (magent-work-create :dir dir :purpose purpose)))
    (setf (magent-work-start-commit work) start-commit)
    (push work magent--works)
    (unless (string-empty-p prompt)
      (let ((sid (magent-backend-launch dir prompt)))
        (setf (magent-work-session-id work) sid)
        (setf (magent-work-state work) 'working)
        (puthash sid work magent--session-works)))
    (magent-state-save magent--works)
    (magent-refresh)))

(defun magent--create-worktree (repo branch)
  "Create a git worktree for BRANCH in REPO. Return the worktree dir or nil."
  (let* ((default-directory repo)
         (base (with-temp-buffer
                 (if (zerop (call-process "git" nil t nil
                                          "symbolic-ref" "--short" "HEAD"))
                     (string-trim (buffer-string))
                   "main")))
         (wt-dir (expand-file-name (concat ".worktrees/" branch) repo)))
    (let ((result (with-temp-buffer
                    (call-process "git" nil t nil
                                  "worktree" "add" "-b" branch
                                  wt-dir base)
                    (buffer-string))))
      (if (file-directory-p wt-dir)
          (progn (message "Created worktree %s from %s" branch base)
                 wt-dir)
        (message "Failed to create worktree: %s" result)
        nil))))

(defun magent-new-worktree ()
  "Create a new git worktree, then launch an agent in it."
  (interactive)
  (let* ((repo (or (magent--repo-at-point)
                   (read-directory-name "Repository: ")))
         (branch (read-string "Branch name: ")))
    (when-let ((wt-dir (magent--create-worktree repo branch)))
      (let* ((purpose (read-string "Purpose: "))
             (prompt (read-string "Agent prompt (or empty to skip): "))
             (start-commit (magent--git-head wt-dir))
             (work (magent-work-create :dir wt-dir :purpose purpose)))
        (setf (magent-work-start-commit work) start-commit)
        (push work magent--works)
        (unless (string-empty-p prompt)
          (let ((sid (magent-backend-launch wt-dir prompt)))
            (setf (magent-work-session-id work) sid)
            (setf (magent-work-state work) 'working)
            (puthash sid work magent--session-works)))
        (magent-state-save magent--works)
        (magent-refresh)))))

(defun magent-mark-done ()
  "Mark Work at point as done.
If the work directory looks like a git worktree, offer to remove it."
  (interactive)
  (when-let ((work (magent--work-at-point)))
    (setf (magent-work-state work) 'done)
    ;; Offer worktree cleanup if it's a worktree (not the main checkout)
    (let ((dir (magent-work-dir work)))
      (when (and (file-exists-p (expand-file-name ".git" dir))
                 ;; .git is a file (not directory) in worktrees
                 (not (file-directory-p (expand-file-name ".git" dir)))
                 (yes-or-no-p (format "Remove worktree %s? "
                                      (abbreviate-file-name dir))))
        (let ((default-directory (magent-work-repo work)))
          (call-process "git" nil nil nil "worktree" "remove" dir))))
    (magent-state-save magent--works)
    (magent-refresh)))

(defun magent-org-dispatch ()
  "Dispatch the org heading at point as a new agent work item.
Run this with point on a TODO heading in an org file.
Creates a worktree (if in a git repo) and launches an agent
with the heading text as the prompt."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (unless (org-at-heading-p)
    (user-error "Not on an org heading"))
  (let* ((heading (org-get-heading t t t t))
         (repo (magent--git-repo-root default-directory)))
    (if repo
        ;; In a git repo — create worktree
        (let* ((branch (read-string "Branch name: "
                                    (replace-regexp-in-string
                                     "[^a-zA-Z0-9/_-]" "-"
                                     (downcase heading)))))
          (when-let ((wt-dir (magent--create-worktree repo branch)))
            (let* ((start-commit (magent--git-head wt-dir))
                   (work (magent-work-create :dir wt-dir :purpose heading)))
              (setf (magent-work-start-commit work) start-commit)
              (push work magent--works)
              (let ((sid (magent-backend-launch wt-dir heading)))
                (setf (magent-work-session-id work) sid)
                (setf (magent-work-state work) 'working)
                (puthash sid work magent--session-works))
              (magent-state-save magent--works)
              (org-todo "NEXT")
              (when (get-buffer "*magent*")
                (with-current-buffer "*magent*" (magent-refresh)))
              (message "Dispatched: %s → %s" heading branch))))
      ;; Not in a git repo — just use current directory
      (let* ((work (magent-work-create :dir default-directory :purpose heading)))
        (push work magent--works)
        (let ((sid (magent-backend-launch default-directory heading)))
          (setf (magent-work-session-id work) sid)
          (setf (magent-work-state work) 'working)
          (puthash sid work magent--session-works))
        (magent-state-save magent--works)
        (org-todo "NEXT")
        (when (get-buffer "*magent*")
          (with-current-buffer "*magent*" (magent-refresh)))
        (message "Dispatched: %s" heading)))))

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
