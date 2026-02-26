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

(defcustom magent-discover-dirs nil
  "Directories to scan for Claude sessions on startup.
If nil, scan all projects in `magent-claude-projects-dir'."
  :type '(repeat directory)
  :group 'magent)

(defun magent--merge-discovered (existing discovered)
  "Merge DISCOVERED sessions into EXISTING works list.
Deduplicates by directory. Existing works take priority.
Updates session-id on existing idle works if discovered has a newer one."
  (let ((dirs (make-hash-table :test 'equal)))
    ;; Index existing by dir
    (dolist (w existing)
      (puthash (magent-work-dir w) w dirs))
    ;; Merge discovered
    (dolist (w discovered)
      (let* ((dir (magent-work-dir w))
             (existing-work (gethash dir dirs)))
        (if existing-work
            ;; Update session-id if existing has none
            (when (and (null (magent-work-session-id existing-work))
                       (magent-work-session-id w))
              (setf (magent-work-session-id existing-work)
                    (magent-work-session-id w)))
          ;; New work from discovery
          (puthash dir w dirs))))
    (hash-table-values dirs)))

;;;###autoload
(defun magent ()
  "Open the magent dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*magent*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'magent-mode)
        (magent-mode))
      (unless magent--works
        (let ((saved (magent-state-load))
              (discovered (magent-discover-sessions magent-discover-dirs)))
          (setq magent--works (magent--merge-discovered
                               (or saved nil) discovered))))
      (magent-refresh)
      (magent-watch-start))
    (switch-to-buffer buf)))

;;; File watcher — auto-refresh when sessions change

(defvar magent--file-watcher nil
  "File notify descriptor for watching claude projects dir.")

(defun magent--on-session-change (_event)
  "Called when a file changes in the claude projects directory."
  (magent--schedule-refresh))

(defun magent-watch-start ()
  "Start watching `magent-claude-projects-dir' for changes."
  (magent-watch-stop)
  (when (and (file-directory-p magent-claude-projects-dir)
             (fboundp 'file-notify-add-watch))
    (setq magent--file-watcher
          (file-notify-add-watch
           magent-claude-projects-dir
           '(change)
           #'magent--on-session-change))))

(defun magent-watch-stop ()
  "Stop watching for session changes."
  (when magent--file-watcher
    (file-notify-rm-watch magent--file-watcher)
    (setq magent--file-watcher nil)))

(provide 'magent)
;;; magent.el ends here
