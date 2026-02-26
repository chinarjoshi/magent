;;; test/test-magent-integration.el --- Integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent)

(ert-deftest magent-buffer-creates ()
  "M-x magent should create the *magent* buffer in magent-mode."
  (let ((magent--works nil)
        (magent-state-file (make-temp-file "magent-state" nil ".el"))
        (magent-claude-projects-dir (make-temp-file "magent-proj" t)))
    (unwind-protect
        (progn
          (save-window-excursion
            (magent))
          (with-current-buffer "*magent*"
            (should (equal (buffer-name) "*magent*"))
            (should (derived-mode-p 'magent-mode))
            (should (string-match-p "Active" (buffer-string)))
            (should (string-match-p "No active work" (buffer-string)))))
      (when (get-buffer "*magent*")
        (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

(ert-deftest magent-buffer-shows-works ()
  "Buffer should render Work items grouped by repo."
  (let* ((magent-state-file (make-temp-file "magent-state" nil ".el"))
         (magent--works
          (list (magent-work--internal-create
                 :dir "/tmp/wt1" :repo "/tmp/repo-a/"
                 :branch "feat/alpha" :purpose "test alpha"
                 :state 'working :recent '("Edit(foo.ts)"))
                (magent-work--internal-create
                 :dir "/tmp/wt2" :repo "/tmp/repo-a/"
                 :branch "fix/beta" :purpose "test beta"
                 :state 'needs-input)
                (magent-work--internal-create
                 :dir "/tmp/wt3" :repo "/tmp/repo-b/"
                 :branch "feat/gamma" :purpose "test gamma"
                 :state 'done))))
    (unwind-protect
        (progn
          (save-window-excursion
            (magent))
          (with-current-buffer "*magent*"
            (let ((content (buffer-string)))
              ;; Active section has repo-a works
              (should (string-match-p "repo-a" content))
              (should (string-match-p "feat/alpha" content))
              (should (string-match-p "\\[working\\]" content))
              (should (string-match-p "\\[needs input\\]" content))
              ;; Archive section has the done work
              (should (string-match-p "Archive" content))
              (should (string-match-p "feat/gamma" content)))))
      (when (get-buffer "*magent*")
        (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

(ert-deftest magent-buffer-state-faces ()
  "Each work state should render with the correct face."
  (let* ((magent-state-file (make-temp-file "magent-state" nil ".el"))
         (magent--works
          (list (magent-work--internal-create
                 :dir "/tmp/wt1" :repo "/tmp/repo/"
                 :branch "feat/working" :purpose "test"
                 :state 'working)
                (magent-work--internal-create
                 :dir "/tmp/wt2" :repo "/tmp/repo/"
                 :branch "feat/needs-input" :purpose "test"
                 :state 'needs-input)
                (magent-work--internal-create
                 :dir "/tmp/wt3" :repo "/tmp/repo/"
                 :branch "feat/idle" :purpose "test"
                 :state 'idle)
                (magent-work--internal-create
                 :dir "/tmp/wt4" :repo "/tmp/repo/"
                 :branch "feat/done" :purpose "test"
                 :state 'done))))
    (unwind-protect
        (progn
          (save-window-excursion
            (magent))
          (with-current-buffer "*magent*"
            (let ((content (buffer-string)))
              ;; Branch names should have state-specific faces
              (dolist (pair '(("feat/working" magent-face-working)
                              ("feat/needs-input" magent-face-needs-input)
                              ("feat/idle" magent-face-idle)))
                (let* ((branch (car pair))
                       (expected-face (cadr pair))
                       (pos (string-match (regexp-quote branch) content)))
                  (should pos)
                  (should (eq (get-text-property pos 'font-lock-face content)
                              expected-face))))
              ;; Status badges should have state-specific faces
              (dolist (pair '(("\\[working\\]" magent-face-working)
                              ("\\[needs input\\]" magent-face-needs-input)
                              ("\\[idle\\]" magent-face-idle)))
                (let* ((label (car pair))
                       (expected-face (cadr pair))
                       (pos (string-match label content)))
                  (should pos)
                  (should (eq (get-text-property pos 'font-lock-face content)
                              expected-face)))))))
      (when (get-buffer "*magent*")
        (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

(ert-deftest magent-resume-repo-section ()
  "Resuming on a repo section should resume all idle works with session-ids."
  (let* ((magent-state-file (make-temp-file "magent-state" nil ".el"))
         (magent--session-works (make-hash-table :test 'equal))
         (resumed-ids nil)
         (magent--works
          (list (magent-work--internal-create
                 :dir "/tmp/wt1" :repo "/tmp/repo/"
                 :branch "feat/a" :purpose "A"
                 :state 'idle :session-id "sess-a")
                (magent-work--internal-create
                 :dir "/tmp/wt2" :repo "/tmp/repo/"
                 :branch "feat/b" :purpose "B"
                 :state 'idle :session-id "sess-b")
                (magent-work--internal-create
                 :dir "/tmp/wt3" :repo "/tmp/repo/"
                 :branch "feat/c" :purpose "C"
                 :state 'working :session-id "sess-c"))))
    ;; Register works in session-works for backend-resume
    (dolist (w magent--works)
      (puthash (magent-work-session-id w) w magent--session-works))
    (unwind-protect
        (cl-letf (((symbol-function 'magent-backend-resume)
                   (lambda (sid) (push sid resumed-ids) sid)))
          (save-window-excursion
            (magent))
          (with-current-buffer "*magent*"
            ;; Move point to the repo section header
            (goto-char (point-min))
            (re-search-forward "repo")
            ;; Call resume on the repo section
            (magent-resume)
            ;; The 2 idle works should now be working
            (should (eq (magent-work-state (nth 0 magent--works)) 'working))
            (should (eq (magent-work-state (nth 1 magent--works)) 'working))
            ;; The already-working one should stay working
            (should (eq (magent-work-state (nth 2 magent--works)) 'working))
            ;; Backend-resume should have been called for the 2 idle ones
            (should (= (length resumed-ids) 2))
            (should (member "sess-a" resumed-ids))
            (should (member "sess-b" resumed-ids))
            ;; The working one should NOT have been resumed
            (should-not (member "sess-c" resumed-ids))))
      (when (get-buffer "*magent*")
        (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

;;; test/test-magent-integration.el ends here
