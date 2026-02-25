;;; test/test-magent-integration.el --- Integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent)

(ert-deftest magent-buffer-creates ()
  "M-x magent should create the *magent* buffer in magent-mode."
  (let ((magent--works nil)
        (magent-state-file (make-temp-file "magent-state" nil ".el")))
    (unwind-protect
        (progn
          (save-window-excursion
            (magent))
          (with-current-buffer "*magent*"
            (should (equal (buffer-name) "*magent*"))
            (should (derived-mode-p 'magent-mode))
            (should (string-match-p "Magent" (buffer-string)))
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
              (should (string-match-p "repo-a" content))
              (should (string-match-p "repo-b" content))
              (should (string-match-p "feat/alpha" content))
              (should (string-match-p "\\[working\\]" content))
              (should (string-match-p "\\[needs input\\]" content))
              (should (string-match-p "\\[done\\]" content)))))
      (when (get-buffer "*magent*")
        (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

;;; test/test-magent-integration.el ends here
