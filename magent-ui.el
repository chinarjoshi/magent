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

;; Branch faces — color tells you the state at a glance
(defface magent-face-branch-working
  '((((class color) (background dark)) :foreground "#3fb950")
    (((class color) (background light)) :foreground "#1a7f37")
    (t :inherit success))
  "Face for branch names of working agents (GitHub Open green)."
  :group 'magent-faces)

(defface magent-face-branch-needs-input
  '((t :inherit warning :weight bold))
  "Face for branch names of agents needing input."
  :group 'magent-faces)

(defface magent-face-branch-idle
  '((t :inherit shadow))
  "Face for branch names of idle agents."
  :group 'magent-faces)

(defface magent-face-branch-done
  '((((class color) (background dark)) :foreground "#c678dd")
    (((class color) (background light)) :foreground "#a626a4")
    (t :inherit success))
  "Face for branch names of completed/merged work."
  :group 'magent-faces)

;; Status badge faces
(defface magent-face-status-working
  '((((class color) (background dark)) :foreground "#3fb950")
    (((class color) (background light)) :foreground "#1a7f37")
    (t :inherit success))
  "Face for [working] status badge (GitHub Open green)."
  :group 'magent-faces)

(defface magent-face-status-needs-input
  '((((class color) (background dark)) :foreground "#e5c07b" :weight bold)
    (((class color) (background light)) :foreground "#986801" :weight bold)
    (t :inherit warning :weight bold))
  "Face for [needs input] status badge."
  :group 'magent-faces)

(defface magent-face-status-idle
  '((t :inherit shadow))
  "Face for [idle] status badge."
  :group 'magent-faces)

(defface magent-face-status-done
  '((((class color) (background dark)) :foreground "#c678dd")
    (((class color) (background light)) :foreground "#a626a4")
    (t :inherit success))
  "Face for [done/merged] status badge."
  :group 'magent-faces)

;; Repo face — distinct from branch
(defface magent-face-repo
  '((t :inherit magit-section-heading :weight bold))
  "Face for repo names."
  :group 'magent-faces)

;; Archive faces
(defface magent-face-archive-repo
  '((t :inherit shadow))
  "Face for repo names in archive."
  :group 'magent-faces)

(defface magent-face-archive-branch
  '((t :inherit shadow :slant italic))
  "Face for branch names in archive."
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

(defun magent--branch-face (state)
  "Return the branch face for STATE."
  (pcase state
    ('working 'magent-face-branch-working)
    ('needs-input 'magent-face-branch-needs-input)
    ('idle 'magent-face-branch-idle)
    ('done 'magent-face-branch-done)
    (_ 'shadow)))

(defun magent--status-face (state)
  "Return the status badge face for STATE."
  (pcase state
    ('working 'magent-face-status-working)
    ('needs-input 'magent-face-status-needs-input)
    ('idle 'magent-face-status-idle)
    ('done 'magent-face-status-done)
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

(defun magent--format-recent (work)
  "Format the most recent action for WORK."
  (if-let ((r (car (magent-work-recent work))))
      r
    ""))

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
         (propertize (format "  %s" branch) 'font-lock-face (magent--branch-face state))
         pad
         (propertize (format "[%s]" (magent--status-label state))
                     'font-lock-face (magent--status-face state))
         (when oneliner
           (propertize (format "  %s" oneliner) 'font-lock-face 'shadow))))
      ;; Foldable body — first level: dir + last prompt (1-2 lines)
      (let ((dir (abbreviate-file-name (magent-work-dir work)))
            (purpose (magent-work-purpose work))
            (output (magent-work-last-output work)))
        (insert (propertize (format "    %s\n" dir) 'font-lock-face 'shadow))
        ;; Last prompt — max 2 lines
        (when (and purpose (not (string-empty-p purpose)))
          (let* ((clean (replace-regexp-in-string "<[^>]+>" "" purpose))
                 (clean (replace-regexp-in-string "\\`[ \t\n]+" "" clean))
                 (lines (split-string clean "\n" t))
                 (display (mapconcat #'identity (seq-take lines 2) "\n    ")))
            (insert (propertize (format "    > %s\n"
                                        (truncate-string-to-width display 120))
                                'font-lock-face 'shadow))))
        ;; Files
        (when (magent-work-files work)
          (dolist (file (magent-work-files work))
            (magit-insert-section (magent-file-section file)
              (insert (format "    %s\n" file)))))
        ;; PR
        (when (magent-work-pr work)
          (insert (propertize (format "    PR: %s\n" (magent-work-pr work))
                              'font-lock-face 'shadow)))
        ;; Last output — deeper fold
        (when (and output (not (string-empty-p output)))
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
                   'font-lock-face 'magent-face-archive-repo)
       (propertize " - " 'font-lock-face 'shadow)
       (propertize (or (magent-work-branch work) "?")
                   'font-lock-face 'magent-face-archive-branch)))))

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
          (unless (or (lookup-key magit-section-mode-map (kbd key))
                      (member key '("g" "n" "p")))
            (define-key map (kbd key) cmd)))))
    map)
  "Keymap for `magent-mode'.")

;; Bind g/n/p for non-Evil users
(unless (featurep 'evil)
  (define-key magent-mode-map (kbd "g") #'magent-refresh)
  (define-key magent-mode-map (kbd "n") #'magit-section-forward)
  (define-key magent-mode-map (kbd "p") #'magit-section-backward))

(define-derived-mode magent-mode magit-section-mode "Magent"
  "Major mode for the magent dashboard."
  :group 'magent
  (setq-local revert-buffer-function #'magent--revert-buffer)
  (setq buffer-read-only t))

;; Evil integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'magent-mode 'normal)
  (let ((map (evil-get-auxiliary-keymap magent-mode-map 'normal t t)))
    (dolist (group magent-bindings)
      (dolist (binding (cdr group))
        (let ((key (car binding))
              (cmd (cadr binding)))
          (unless (member key '("g" "n" "p"))
            (define-key map (kbd key) cmd))))))
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
      (cond
       ((fboundp 'claude-code--start)
        (if sid
            (claude-code--start nil (list "--resume" sid) nil t)
          (claude-code--start nil nil nil t)))
       (t
        (if sid
            (let* ((buf-name (format "*magent-agent-%s*"
                                     (or (magent-work-branch work) "agent")))
                   (buf (make-comint-in-buffer
                         buf-name nil
                         magent-agent-command nil
                         "--resume" sid)))
              (switch-to-buffer-other-window buf))
          (message "No session to resume. Use N to create one.")))))))

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
