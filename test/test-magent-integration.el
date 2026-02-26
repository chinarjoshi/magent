;;; test/test-magent-integration.el --- Integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent)

(ert-deftest magent-buffer-creates ()
  (let ((magent--works nil)
        (magent-state-file (make-temp-file "magent-state" nil ".el"))
        (magent-claude-projects-dir (make-temp-file "magent-proj" t)))
    (unwind-protect
        (progn
          (save-window-excursion (magent))
          (with-current-buffer "*magent*"
            (should (derived-mode-p 'magent-mode))
            (should (string-match-p "Active" (buffer-string)))
            (should (string-match-p "No active work" (buffer-string)))))
      (when (get-buffer "*magent*") (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

(ert-deftest magent-buffer-shows-works ()
  (let* ((magent-state-file (make-temp-file "magent-state" nil ".el"))
         (magent--works
          (list (make-magent-work
                 :dir "/tmp/wt1" :repo "/tmp/repo-a/"
                 :branch "feat/alpha" :state 'working)
                (make-magent-work
                 :dir "/tmp/wt2" :repo "/tmp/repo-a/"
                 :branch "fix/beta" :state 'idle)
                (make-magent-work
                 :dir "/tmp/wt3" :repo "/tmp/repo-b/"
                 :branch "feat/gamma" :state 'done))))
    (unwind-protect
        (progn
          (save-window-excursion (magent))
          (with-current-buffer "*magent*"
            (let ((content (buffer-string)))
              (should (string-match-p "repo-a" content))
              (should (string-match-p "feat/alpha" content))
              (should (string-match-p "\\[working\\]" content))
              (should (string-match-p "\\[idle\\]" content))
              (should (string-match-p "Archive" content))
              (should (string-match-p "feat/gamma" content)))))
      (when (get-buffer "*magent*") (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

(ert-deftest magent-buffer-state-faces ()
  (let* ((magent-state-file (make-temp-file "magent-state" nil ".el"))
         (magent--works
          (list (make-magent-work
                 :dir "/tmp/wt1" :repo "/tmp/repo/"
                 :branch "feat/working" :state 'working)
                (make-magent-work
                 :dir "/tmp/wt2" :repo "/tmp/repo/"
                 :branch "feat/idle" :state 'idle))))
    (unwind-protect
        (progn
          (save-window-excursion (magent))
          (with-current-buffer "*magent*"
            (let ((content (buffer-string)))
              (dolist (pair '(("feat/working" magent-face-working)
                              ("feat/idle" magent-face-idle)))
                (let* ((branch (car pair))
                       (expected (cadr pair))
                       (pos (string-match (regexp-quote branch) content)))
                  (should pos)
                  (should (eq (get-text-property pos 'font-lock-face content)
                              expected))))
              (dolist (pair '(("\\[working\\]" magent-face-working)
                              ("\\[idle\\]" magent-face-idle)))
                (let* ((label (car pair))
                       (expected (cadr pair))
                       (pos (string-match label content)))
                  (should pos)
                  (should (eq (get-text-property pos 'font-lock-face content)
                              expected)))))))
      (when (get-buffer "*magent*") (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

(ert-deftest magent-resume-repo-section ()
  (let* ((magent-state-file (make-temp-file "magent-state" nil ".el"))
         (resumed nil)
         (magent--works
          (list (make-magent-work
                 :dir "/tmp/wt1" :repo "/tmp/repo/"
                 :branch "feat/a" :state 'idle :session-id "sa")
                (make-magent-work
                 :dir "/tmp/wt2" :repo "/tmp/repo/"
                 :branch "feat/b" :state 'idle :session-id "sb")
                (make-magent-work
                 :dir "/tmp/wt3" :repo "/tmp/repo/"
                 :branch "feat/c" :state 'working :session-id "sc"))))
    (unwind-protect
        (cl-letf (((symbol-function 'magent-resume)
                   (lambda (w) (push (magent-work-session-id w) resumed)
                     (setf (magent-work-state w) 'working))))
          (save-window-excursion (magent))
          (with-current-buffer "*magent*"
            (goto-char (point-min))
            (re-search-forward "repo")
            (magent-resume-at-point)
            (should (eq (magent-work-state (nth 0 magent--works)) 'working))
            (should (eq (magent-work-state (nth 1 magent--works)) 'working))
            (should (eq (magent-work-state (nth 2 magent--works)) 'working))
            (should (= (length resumed) 2))
            (should (member "sa" resumed))
            (should (member "sb" resumed))
            (should-not (member "sc" resumed))))
      (when (get-buffer "*magent*") (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

;;; test/test-magent-integration.el ends here
