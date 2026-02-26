;;; magent-ui.el --- UI rendering for magent -*- lexical-binding: t; -*-

;;; Commentary:

;; magit-section buffer, keybindings, interactive commands, entry point.

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

(defface magent-face-idle
  '((t :inherit shadow))
  "Face for idle state."
  :group 'magent-faces)

(defface magent-face-done
  '((((class color) (background dark)) :foreground "#c678dd")
    (((class color) (background light)) :foreground "#a626a4")
    (t :inherit success))
  "Face for done/merged state (GitHub merged purple)."
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

(defclass magent-repo-section (magit-section) ())
(defclass magent-work-section (magit-section) ())
(defclass magent-file-section (magit-section) ())
(defclass magent-active-section (magit-section) ())
(defclass magent-archive-section (magit-section) ())

;;; Helpers

(defun magent--state-face (state)
  (pcase state
    ('working 'magent-face-working)
    ('idle 'magent-face-idle)
    ('done 'magent-face-done)
    (_ 'shadow)))

(defun magent--status-label (state)
  (pcase state
    ('working "working")
    ('idle "idle")
    ('done "done")
    (_ "?")))

(defun magent--repo-name (repo)
  (if (or (null repo) (equal repo "unknown")) "unknown"
    (file-name-nondirectory (directory-file-name repo))))

(defun magent--purpose-oneliner (purpose)
  (when (and purpose (not (string-empty-p purpose)))
    (let* ((clean (replace-regexp-in-string "<[^>]+>" "" purpose))
           (clean (replace-regexp-in-string "\\`[ \t\n]+" "" clean))
           (line (car (split-string clean "\n" t))))
      (when (and line (not (string-empty-p line)))
        (truncate-string-to-width line 60)))))

(defun magent--max-branch-width (works)
  (let ((max-w 0))
    (dolist (w works max-w)
      (let ((len (length (or (magent-work-branch w) ""))))
        (when (> len max-w) (setq max-w len))))))

;;; Section insertion

(defun magent--insert-work (work col)
  "Insert a section for WORK with status aligned at COL."
  (let* ((state (magent-work-state work))
         (branch (or (magent-work-branch work) "?"))
         (oneliner (magent--purpose-oneliner (magent-work-purpose work)))
         (face (magent--state-face state))
         (pad (make-string (max 1 (- col (length branch))) ?\s)))
    (magit-insert-section (magent-work-section work t)
      (magit-insert-heading
        (concat
         (propertize (format "  %s" branch) 'font-lock-face face)
         pad
         (propertize (format "[%s]" (magent--status-label state)) 'font-lock-face face)
         (when oneliner
           (propertize (format "  %s" oneliner) 'font-lock-face 'shadow))))
      ;; Fold body: dir + last prompt
      (insert (propertize (format "    %s\n" (abbreviate-file-name (magent-work-dir work)))
                          'font-lock-face 'shadow))
      (when-let ((purpose (magent-work-purpose work)))
        (unless (string-empty-p purpose)
          (let* ((clean (replace-regexp-in-string
                         "\\`[ \t\n]+" ""
                         (replace-regexp-in-string "<[^>]+>" "" purpose)))
                 (lines (split-string clean "\n" t))
                 (display (mapconcat #'identity (seq-take lines 2) "\n    ")))
            (insert (propertize (format "    > %s\n" (truncate-string-to-width display 120))
                                'font-lock-face 'shadow)))))
      ;; Files
      (dolist (file (magent-work-files work))
        (magit-insert-section (magent-file-section file)
          (insert (format "    %s\n" file))))
      ;; PR
      (when (magent-work-pr work)
        (insert (propertize (format "    PR: %s\n" (magent-work-pr work))
                            'font-lock-face 'shadow)))
      ;; Agent output — deeper fold
      (when-let ((output (magent-work-last-output work)))
        (unless (string-empty-p output)
          (magit-insert-section (magit-section 'output t)
            (magit-insert-heading
              (propertize "    Agent output" 'font-lock-face 'shadow))
            (let ((lines (seq-take (split-string output "\n") 30)))
              (insert (propertize
                       (replace-regexp-in-string "^" "      "
                                                 (mapconcat #'identity lines "\n"))
                       'font-lock-face 'shadow))
              (insert "\n"))))))))

(defun magent--insert-archive-work (work)
  (magit-insert-section (magent-work-section work)
    (magit-insert-heading
      (concat
       (propertize (format "  %s" (magent--repo-name (magent-work-repo work)))
                   'font-lock-face 'magent-face-archive)
       (propertize " - " 'font-lock-face 'shadow)
       (propertize (or (magent-work-branch work) "?")
                   'font-lock-face 'magent-face-archive)))))

(defun magent--insert-repo-section (repo works col)
  (let ((todo-count (if (file-directory-p repo) (magent-repo-todo-count repo) 0))
        (name (magent--repo-name repo)))
    (magit-insert-section (magent-repo-section repo)
      (magit-insert-heading
        (concat
         (propertize name 'font-lock-face 'magent-face-repo)
         (when (> todo-count 0)
           (propertize (format "  %d TODOs" todo-count) 'font-lock-face 'shadow))))
      (dolist (w works) (magent--insert-work w col))
      (insert "\n"))))

;;; Buffer state

(defvar magent--works nil "Current list of Work structs.")

(defun magent-refresh-buffer ()
  (when (derived-mode-p 'magent-mode)
    (magent-auto-archive-merged magent--works)
    (let* ((inhibit-read-only t)
           (active (seq-remove #'magent-work-done-p magent--works))
           (archive (seq-filter #'magent-work-done-p magent--works))
           (groups (magent-group-by-repo active))
           (col (+ 2 (magent--max-branch-width active))))
      (magit-insert-section (magit-status-section)
        (magit-insert-section (magent-active-section 'active)
          (magit-insert-heading
            (propertize (format "Active (%d)" (length active))
                        'font-lock-face 'magit-section-heading))
          (if groups
              (dolist (g groups) (magent--insert-repo-section (car g) (cdr g) col))
            (insert "\n  No active work. Press N to create one.\n\n")))
        (when archive
          (magit-insert-section (magent-archive-section 'archive t)
            (magit-insert-heading
              (propertize (format "Archive (%d)" (length archive))
                          'font-lock-face 'magit-section-heading))
            (dolist (w archive) (magent--insert-archive-work w))
            (insert "\n")))))))

;;; Mode and keymap

(defvar magent-bindings
  '(("Navigation"
     ("TAB"   magit-section-toggle        "Toggle section")
     ("S-TAB" magit-section-toggle-children "Toggle all sections")
     ("n"     magit-section-forward        "Next section")
     ("p"     magit-section-backward       "Previous section"))
    ("Actions"
     ("RET"   magent-visit                 "Visit (agent output or file)")
     ("i"     magent-send-input            "Send input to agent")
     ("r"     magent-resume-at-point       "Resume (work or all in repo)")
     ("k"     magent-kill-agent            "Kill agent session")
     ("D"     magent-mark-done             "Mark done / archive"))
    ("Git"
     ("c"     magent-tell-commit           "Tell agent to commit")
     ("P"     magent-tell-pr               "Tell agent to open PR")
     ("d"     magent-diff                  "Diff unstaged")
     ("="     magent-diff-since-start      "Diff since agent started")
     ("f"     magent-fetch                 "Fetch in worktree")
     ("w"     magent-new-worktree          "New worktree + agent"))
    ("Tools"
     ("!"     magent-shell-command         "Shell command in worktree")
     ("$"     magent-show-process          "Show agent process buffer")
     ("l"     magent-event-log             "Agent event log")
     ("b"     magent-browse-backlog        "Browse backlog org files"))
    ("Global"
     ("N"     magent-new-work              "New work")
     ("g"     magent-refresh               "Refresh")
     ("?"     magent-help                  "This help"))))

(defvar magent-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (dolist (group magent-bindings)
      (dolist (binding (cdr group))
        (let ((key (car binding))
              (cmd (cadr binding)))
          (unless (lookup-key magit-section-mode-map (kbd key))
            (define-key map (kbd key) cmd)))))
    (unless (featurep 'evil)
      (define-key map "g" #'magent-refresh)
      (define-key map "n" #'magit-section-forward)
      (define-key map "p" #'magit-section-backward))
    map))

(define-derived-mode magent-mode magit-section-mode "Magent"
  :group 'magent
  (setq-local revert-buffer-function (lambda (&rest _) (magent-refresh)))
  (setq buffer-read-only t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'magent-mode 'normal)
  (evil-define-key* 'normal magent-mode-map
    "q" #'quit-window
    "j" #'magit-section-forward  "k" #'magit-section-backward
    "J" #'magit-section-forward-sibling  "K" #'magit-section-backward-sibling
    (kbd "C-j") #'magit-section-forward  (kbd "C-k") #'magit-section-backward
    "gg" #'evil-goto-first-line  "G" #'evil-goto-line
    (kbd "C-d") #'evil-scroll-down  (kbd "C-u") #'evil-scroll-up
    "/" #'evil-search-forward  "n" #'evil-search-next  "N" #'evil-search-previous
    "gr" #'magent-refresh  "gR" #'magent-refresh
    ;; Bind magent commands explicitly in normal state
    "i" #'magent-send-input  "r" #'magent-resume-at-point
    "c" #'magent-tell-commit  "D" #'magent-mark-done
    "P" #'magent-tell-pr  "d" #'magent-diff  "=" #'magent-diff-since-start
    "f" #'magent-fetch  "w" #'magent-new-worktree
    "!" #'magent-shell-command  "$" #'magent-show-process
    "l" #'magent-event-log  "b" #'magent-browse-backlog
    "N" #'magent-new-work  "?" #'magent-help))

(defun magent-refresh ()
  (interactive)
  (when (derived-mode-p 'magent-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magent-refresh-buffer))))

;;; Section helpers

(defun magent--work-at-point ()
  (when-let ((sec (magit-current-section)))
    (cond
     ((magent-work-section-p sec) (oref sec value))
     ((magent-file-section-p sec)
      (when-let ((p (oref sec parent)))
        (when (magent-work-section-p p) (oref p value)))))))

(defun magent--repo-at-point ()
  (when-let ((sec (magit-current-section)))
    (cond
     ((magent-repo-section-p sec) (oref sec value))
     ((magent-work-section-p sec) (magent-work-repo (oref sec value))))))

;;; Interactive commands

(defun magent-visit ()
  (interactive)
  (let ((sec (magit-current-section)))
    (cond
     ((magent-file-section-p sec) (find-file (oref sec value)))
     ((magent-work-section-p sec) (magent-open-agent))
     ((magent-repo-section-p sec) (magent-browse-backlog)))))

(defun magent-open-agent ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (let ((default-directory (magent-work-dir w))
          (sid (magent-work-session-id w)))
      (if (fboundp 'claude-code--start)
          (claude-code--start nil (when sid (list "--resume" sid)) nil t)
        (if sid
            (switch-to-buffer-other-window
             (make-comint-in-buffer
              (format "*magent-%s*" (or (magent-work-branch w) "agent"))
              nil magent-agent-command nil "--resume" sid))
          (message "No session. Use N to create one."))))))

(defun magent-show-process ()
  (interactive)
  (when-let* ((w (magent--work-at-point))
              (sid (magent-work-session-id w))
              (buf (get-buffer (format " *magent-proc-%s*" sid))))
    (display-buffer buf)))

(defun magent-shell-command ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (let ((default-directory (magent-work-dir w)))
      (call-interactively #'shell-command))))

(defun magent-tell-commit ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (magent-send w "commit your changes")
    (setf (magent-work-state w) 'working)
    (magent-refresh)
    (message "Told agent to commit.")))

(defun magent-tell-pr ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (magent-send w "open a pull request")
    (setf (magent-work-state w) 'working)
    (magent-refresh)
    (message "Told agent to open PR.")))

(defun magent-diff ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (let ((default-directory (magent-work-dir w)))
      (if (fboundp 'magit-diff-unstaged) (magit-diff-unstaged)
        (shell-command "git diff")))))

(defun magent-diff-since-start ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (let ((default-directory (magent-work-dir w))
          (start (magent-work-start-commit w)))
      (cond
       ((and start (fboundp 'magit-diff-range))
        (magit-diff-range (format "%s..HEAD" start)))
       (start (shell-command (format "git diff %s..HEAD" start)))
       (t (message "No start commit recorded."))))))

(defun magent-kill-agent ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (when (yes-or-no-p (format "Kill agent for %s? " (magent-work-branch w)))
      (magent-kill w)
      (setf (magent-work-state w) 'idle)
      (magent-refresh))))

(defun magent-resume-at-point ()
  (interactive)
  (let ((sec (magit-current-section)))
    (cond
     ((magent-repo-section-p sec)
      (let ((repo (oref sec value)) (n 0))
        (dolist (w magent--works)
          (when (and (equal (magent-work-repo w) repo)
                     (magent-work-idle-p w)
                     (magent-work-session-id w))
            (magent-resume w) (cl-incf n)))
        (if (> n 0)
            (progn (message "Resumed %d agent%s." n (if (= n 1) "" "s"))
                   (magent-refresh))
          (message "No idle sessions in %s." (magent--repo-name repo)))))
     (t
      (when-let ((w (magent--work-at-point)))
        (if (magent-work-idle-p w)
            (progn (magent-resume w) (magent-refresh))
          (message "Work is not idle.")))))))

(defun magent-browse-backlog ()
  (interactive)
  (when-let ((repo (magent--repo-at-point)))
    (let ((files (file-expand-wildcards (expand-file-name magent-backlog-glob repo))))
      (if files
          (find-file (if (= (length files) 1) (car files)
                       (completing-read "Backlog: " files nil t)))
        (message "No org files in %s" repo)))))

(defun magent-event-log ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (let ((buf (get-buffer-create (format "*magent-log: %s*" (magent-work-branch w)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Event log for %s\n\n" (magent-work-branch w)))
          (if (magent-work-recent w)
              (dolist (r (reverse (magent-work-recent w)))
                (insert (format "  %s\n" r)))
            (insert "  No events.\n")))
        (special-mode))
      (display-buffer buf))))

(defun magent-fetch ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (let ((default-directory (magent-work-dir w)))
      (if (fboundp 'magit-fetch-all)
          (magit-fetch-all (magit-fetch-arguments))
        (shell-command "git fetch --all")))))

(defun magent-send-input ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (let ((input (read-string (format "Send to %s: " (magent-work-branch w)))))
      (magent-send w input)
      (setf (magent-work-state w) 'working)
      (magent-refresh))))

(defun magent--launch-work (dir purpose prompt)
  "Create a Work in DIR, optionally launch agent with PROMPT."
  (let* ((start (magent--git-head dir))
         (w (make-magent-work
             :dir (expand-file-name dir)
             :repo (magent--git-repo-root dir)
             :branch (magent--git-branch dir)
             :purpose purpose
             :start-commit start)))
    (push w magent--works)
    (unless (string-empty-p prompt)
      (let ((sid (magent-launch dir prompt)))
        (setf (magent-work-session-id w) sid
              (magent-work-state w) 'working)))
    (magent-state-save magent--works)
    (magent-refresh)
    w))

(defun magent-new-work ()
  "Create a new Work from an existing directory."
  (interactive)
  (let ((dir (read-directory-name "Directory: "))
        (purpose (read-string "Purpose: "))
        (prompt (read-string "Agent prompt (empty to skip): ")))
    (magent--launch-work dir purpose prompt)))

(defun magent--create-worktree (repo branch)
  "Create git worktree for BRANCH in REPO. Return dir or nil."
  (let* ((default-directory repo)
         (base (or (magent--git repo "symbolic-ref" "--short" "HEAD") "main"))
         (wt-dir (expand-file-name (concat ".worktrees/" branch) repo))
         (result (with-temp-buffer
                   (call-process "git" nil t nil "worktree" "add" "-b" branch wt-dir base)
                   (buffer-string))))
    (if (file-directory-p wt-dir)
        (progn (message "Created worktree %s from %s" branch base) wt-dir)
      (message "Failed: %s" result) nil)))

(defun magent-new-worktree ()
  "Create a git worktree + branch, then optionally launch agent."
  (interactive)
  (let* ((repo (or (magent--repo-at-point) (read-directory-name "Repository: ")))
         (branch (read-string "Branch name: ")))
    (when-let ((dir (magent--create-worktree repo branch)))
      (let ((purpose (read-string "Purpose: "))
            (prompt (read-string "Agent prompt (empty to skip): ")))
        (magent--launch-work dir purpose prompt)))))

(defun magent-mark-done ()
  (interactive)
  (when-let ((w (magent--work-at-point)))
    (setf (magent-work-state w) 'done)
    (let ((dir (magent-work-dir w)))
      (when (and (file-exists-p (expand-file-name ".git" dir))
                 (not (file-directory-p (expand-file-name ".git" dir)))
                 (yes-or-no-p (format "Remove worktree %s? " (abbreviate-file-name dir))))
        (let ((default-directory (magent-work-repo w)))
          (call-process "git" nil nil nil "worktree" "remove" dir))))
    (magent-state-save magent--works)
    (magent-refresh)))

(defun magent-org-dispatch ()
  "Dispatch org heading at point as agent work."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not in org-mode"))
  (unless (org-at-heading-p) (user-error "Not on a heading"))
  (let* ((heading (org-get-heading t t t t))
         (repo (magent--git-repo-root default-directory)))
    (if repo
        (let ((branch (read-string "Branch: "
                                   (replace-regexp-in-string
                                    "[^a-zA-Z0-9/_-]" "-" (downcase heading)))))
          (when-let ((dir (magent--create-worktree repo branch)))
            (magent--launch-work dir heading heading)
            (org-todo "NEXT")
            (message "Dispatched: %s" heading)))
      (magent--launch-work default-directory heading heading)
      (org-todo "NEXT")
      (message "Dispatched: %s" heading))))

(defun magent-help ()
  (interactive)
  (let ((buf (get-buffer-create "*magent-help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Magent keybindings\n\n")
        (dolist (g magent-bindings)
          (insert (car g) "\n")
          (dolist (b (cdr g))
            (insert (format "  %-9s %s\n" (car b) (caddr b))))
          (insert "\n")))
      (special-mode) (goto-char (point-min)))
    (display-buffer buf)))

;;; Entry point + file watcher

(defvar magent--file-watcher nil)

;;;###autoload
(defun magent ()
  "Open the magent dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*magent*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'magent-mode) (magent-mode))
      (unless magent--works
        (let ((discovered (magent-discover-sessions))
              (overrides (magent-state-load)))
          (magent--apply-overrides discovered overrides)
          (setq magent--works discovered)))
      (magent-refresh))
    ;; File watcher
    (unless magent--file-watcher
      (when (and (file-directory-p magent-claude-projects-dir)
                 (fboundp 'file-notify-add-watch))
        (setq magent--file-watcher
              (file-notify-add-watch
               magent-claude-projects-dir '(change)
               (lambda (_) (magent--schedule-refresh))))))
    (switch-to-buffer buf)))

(provide 'magent-ui)
;;; magent-ui.el ends here
