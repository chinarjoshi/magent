;;; test/test-magent-core.el --- Tests for magent-core -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent-core)

(ert-deftest magent-core-loads ()
  (should (featurep 'magent-core)))

(ert-deftest magent-work-create ()
  (let ((w (make-magent-work :dir "/tmp/worktree" :purpose "test")))
    (should (magent-work-p w))
    (should (equal (magent-work-dir w) "/tmp/worktree"))
    (should (equal (magent-work-purpose w) "test"))
    (should (eq (magent-work-state w) 'idle))
    (should (null (magent-work-session-id w)))))

(ert-deftest magent-work-state-predicates ()
  (let ((w (make-magent-work :dir "/tmp/w")))
    (should (magent-work-idle-p w))
    (should-not (magent-work-working-p w))
    (setf (magent-work-state w) 'working)
    (should (magent-work-working-p w))
    (setf (magent-work-state w) 'done)
    (should (magent-work-done-p w))))

(ert-deftest magent-work-repo-derived ()
  (let* ((tmp (file-truename (make-temp-file "magent-test" t)))
         (worktree (expand-file-name "wt" tmp)))
    (unwind-protect
        (progn
          (make-directory worktree t)
          (let ((default-directory tmp))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "commit" "--allow-empty" "-m" "init"))
          (should (equal (magent--git-repo-root worktree)
                         (file-name-as-directory tmp))))
      (delete-directory tmp t))))

(ert-deftest magent-state-save-load-roundtrip ()
  (let* ((state-file (make-temp-file "magent-state" nil ".el"))
         (magent-state-file state-file)
         (works (list (make-magent-work
                       :dir "/tmp/a" :state 'done :start-commit "abc123")
                      (make-magent-work
                       :dir "/tmp/b" :state 'idle))))
    (unwind-protect
        (progn
          (magent-state-save works)
          (let ((overrides (magent-state-load)))
            (should (hash-table-p overrides))
            ;; Done work with start-commit persisted
            (should (gethash "/tmp/a" overrides))
            (should (eq (plist-get (gethash "/tmp/a" overrides) :state) 'done))
            (should (equal (plist-get (gethash "/tmp/a" overrides) :start-commit) "abc123"))
            ;; Idle work without start-commit not persisted
            (should-not (gethash "/tmp/b" overrides))))
      (delete-file state-file))))

(ert-deftest magent-state-load-missing-file ()
  (let ((magent-state-file "/tmp/magent-nonexistent-state.el"))
    (should (null (magent-state-load)))))

(ert-deftest magent-state-load-corrupted ()
  (let ((state-file (make-temp-file "magent-state" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file state-file (insert "not valid lisp {{{"))
          (let ((magent-state-file state-file))
            (should (null (magent-state-load)))))
      (delete-file state-file))))

(ert-deftest magent-group-works-by-repo ()
  (let ((works (list (make-magent-work :dir "/a/wt1" :repo "/a/")
                     (make-magent-work :dir "/b/wt1" :repo "/b/")
                     (make-magent-work :dir "/a/wt2" :repo "/a/"))))
    (let ((groups (magent-group-by-repo works)))
      (should (= (length groups) 2))
      (should (= (length (cdr (assoc "/a/" groups))) 2))
      (should (= (length (cdr (assoc "/b/" groups))) 1)))))

(ert-deftest magent-group-by-repo-no-repo ()
  (let ((works (list (make-magent-work :dir "/x" :repo nil)
                     (make-magent-work :dir "/y" :repo "/r/"))))
    (let ((groups (magent-group-by-repo works)))
      (should (assoc "unknown" groups))
      (should (assoc "/r/" groups)))))

(ert-deftest magent-count-org-todos ()
  (let ((tmp (make-temp-file "magent-org" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "* TODO First\n* DONE Completed\n* TODO Second\n** TODO Nested\n* Not a todo\n"))
          (should (= (magent-count-todos-in-file tmp) 3)))
      (delete-file tmp))))

(ert-deftest magent-count-todos-next-waiting ()
  (let ((tmp (make-temp-file "magent-org" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "* TODO One\n* NEXT Two\n* WAITING Three\n"))
          (should (= (magent-count-todos-in-file tmp) 3)))
      (delete-file tmp))))

(ert-deftest magent-git-repo-root-non-git ()
  (let ((tmp (make-temp-file "magent-test" t)))
    (unwind-protect
        (should (null (magent--git-repo-root tmp)))
      (delete-directory tmp t))))

;;; test/test-magent-core.el ends here
