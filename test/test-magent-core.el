;;; test/test-magent-core.el --- Tests for magent-core -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent-core)

(ert-deftest magent-core-loads ()
  "magent-core should load without error."
  (should (featurep 'magent-core)))

(ert-deftest magent-work-create ()
  "Should create a Work struct with required fields."
  (let ((w (magent-work-create
            :dir "/tmp/worktree"
            :purpose "implement auth")))
    (should (magent-work-p w))
    (should (equal (magent-work-dir w) "/tmp/worktree"))
    (should (equal (magent-work-purpose w) "implement auth"))
    (should (eq (magent-work-state w) 'idle))
    (should (null (magent-work-session-id w)))
    (should (null (magent-work-pr w)))
    (should (null (magent-work-files w)))
    (should (null (magent-work-recent w)))))

(ert-deftest magent-work-state-predicates ()
  "State predicates should reflect current state."
  (let ((w (magent-work-create :dir "/tmp/w" :purpose "test")))
    (should (magent-work-idle-p w))
    (should-not (magent-work-working-p w))
    (setf (magent-work-state w) 'working)
    (should (magent-work-working-p w))
    (setf (magent-work-state w) 'needs-input)
    (should (magent-work-needs-input-p w))
    (setf (magent-work-state w) 'done)
    (should (magent-work-done-p w))))

(ert-deftest magent-work-repo-derived ()
  "Repo should be derivable from dir when it's a git repo."
  (let* ((tmp (file-truename (make-temp-file "magent-test" t)))
         (worktree (expand-file-name "wt" tmp)))
    (unwind-protect
        (progn
          (make-directory worktree t)
          (let ((default-directory tmp))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "commit" "--allow-empty" "-m" "init"))
          (let ((w (magent-work-create :dir worktree :purpose "test")))
            (should (equal (magent-work-repo w) (file-name-as-directory tmp)))))
      (delete-directory tmp t))))

;;; test/test-magent-core.el ends here
