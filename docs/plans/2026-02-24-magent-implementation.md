# Magent Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a magit-style Emacs porcelain for orchestrating AI agent sessions across repos and worktrees.

**Architecture:** Four Elisp files — core (struct + persistence), backend (protocol + default impl), ui (magit-section buffer), and entry point (autoloads + config). Built on `magit-section` for the buffer, `cl-defstruct` for the data model, `cl-defgeneric` for backend abstraction.

**Tech Stack:** Emacs Lisp, magit-section, ERT (testing), cl-lib, json.el

**Design doc:** `docs/plans/2026-02-24-magent-design.md`

---

### Task 1: Project scaffolding

**Files:**
- Create: `magent.el`
- Create: `magent-core.el`
- Create: `magent-backend.el`
- Create: `magent-ui.el`
- Create: `test/test-magent-core.el`
- Create: `Makefile`

**Step 1: Create Makefile for running tests and byte-compilation**

```makefile
EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L . \
	--eval "(package-initialize)"

.PHONY: test compile clean

test:
	$(BATCH) -l test/test-magent-core.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile magent-core.el magent-backend.el magent-ui.el magent.el

clean:
	rm -f *.elc
```

**Step 2: Create magent-core.el with package header only**

```elisp
;;; magent-core.el --- Core data model for magent -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magent contributors
;; Package-Requires: ((emacs "29.1") (magit-section "4.0"))

;;; Commentary:

;; Work struct, state management, and persistence for magent.

;;; Code:

(require 'cl-lib)

(provide 'magent-core)
;;; magent-core.el ends here
```

**Step 3: Create empty shells for the other three files**

`magent-backend.el`:
```elisp
;;; magent-backend.el --- Backend protocol for magent -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic protocol and default implementation for agent backends.

;;; Code:

(require 'cl-lib)
(require 'magent-core)

(provide 'magent-backend)
;;; magent-backend.el ends here
```

`magent-ui.el`:
```elisp
;;; magent-ui.el --- UI rendering for magent -*- lexical-binding: t; -*-

;;; Commentary:

;; magit-section based buffer rendering for the magent dashboard.

;;; Code:

(require 'magit-section)
(require 'magent-core)
(require 'magent-backend)

(provide 'magent-ui)
;;; magent-ui.el ends here
```

`magent.el`:
```elisp
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

(provide 'magent)
;;; magent.el ends here
```

**Step 4: Create test/test-magent-core.el with a trivial passing test**

```elisp
;;; test/test-magent-core.el --- Tests for magent-core -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent-core)

(ert-deftest magent-core-loads ()
  "magent-core should load without error."
  (should (featurep 'magent-core)))

;;; test/test-magent-core.el ends here
```

**Step 5: Run test to verify setup works**

Run: `make test`
Expected: `1 test, 1 passed`

**Step 6: Commit**

```bash
git add Makefile magent.el magent-core.el magent-backend.el magent-ui.el test/
git commit -m "scaffold: project structure with four elisp modules and test harness"
```

---

### Task 2: Work struct and constructors

**Files:**
- Modify: `magent-core.el`
- Modify: `test/test-magent-core.el`

**Step 1: Write tests for Work struct**

Add to `test/test-magent-core.el`:

```elisp
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
  ;; Uses a temp git repo
  (let* ((tmp (make-temp-file "magent-test" t))
         (worktree (expand-file-name "wt" tmp)))
    (unwind-protect
        (progn
          (make-directory worktree t)
          (let ((default-directory tmp))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "commit" "--allow-empty" "-m" "init"))
          (let ((w (magent-work-create :dir worktree :purpose "test")))
            ;; repo should resolve to parent since worktree is subdir of git repo
            (should (equal (magent-work-repo w) (file-name-as-directory tmp)))))
      (delete-directory tmp t))))
```

**Step 2: Run tests to verify they fail**

Run: `make test`
Expected: FAIL — `magent-work-create` is not defined.

**Step 3: Implement Work struct in magent-core.el**

Add to `magent-core.el` before the `provide` form:

```elisp
(cl-defstruct (magent-work (:constructor magent-work--internal-create)
                           (:copier nil))
  "A piece of work: one worktree with one agent session."
  (dir nil :type string :documentation "Worktree path.")
  (repo nil :type (or string null) :documentation "Repo root, derived from dir.")
  (branch nil :type (or string null) :documentation "Git branch, derived from dir.")
  (purpose "" :type string :documentation "What the agent is doing.")
  (state 'idle :type symbol :documentation "One of: working, needs-input, idle, done.")
  (session-id nil :type (or string null) :documentation "Backend session ID.")
  (pr nil :type (or string null) :documentation "PR URL.")
  (files nil :type list :documentation "Files the agent is currently touching.")
  (recent nil :type list :documentation "Recent actions/tool calls."))

(defun magent-work-create (&rest args)
  "Create a Work, deriving repo and branch from :dir."
  (let ((w (apply #'magent-work--internal-create args)))
    (when (magent-work-dir w)
      (let ((dir (expand-file-name (magent-work-dir w))))
        (setf (magent-work-dir w) dir)
        (setf (magent-work-repo w) (magent--git-repo-root dir))
        (setf (magent-work-branch w) (magent--git-branch dir))))
    w))

(defun magent--git-repo-root (dir)
  "Return the git repo root for DIR, or nil."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (condition-case nil
          (file-name-as-directory
           (string-trim
            (shell-command-to-string "git rev-parse --show-toplevel")))
        (error nil)))))

(defun magent--git-branch (dir)
  "Return the current git branch for DIR, or nil."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (condition-case nil
          (let ((branch (string-trim
                         (shell-command-to-string
                          "git rev-parse --abbrev-ref HEAD"))))
            (unless (string-empty-p branch) branch))
        (error nil)))))

;; State predicates

(defun magent-work-working-p (work)
  "Return non-nil if WORK is in working state."
  (eq (magent-work-state work) 'working))

(defun magent-work-needs-input-p (work)
  "Return non-nil if WORK needs user input."
  (eq (magent-work-state work) 'needs-input))

(defun magent-work-idle-p (work)
  "Return non-nil if WORK is idle."
  (eq (magent-work-state work) 'idle))

(defun magent-work-done-p (work)
  "Return non-nil if WORK is done."
  (eq (magent-work-state work) 'done))
```

**Step 4: Run tests to verify they pass**

Run: `make test`
Expected: All pass.

**Step 5: Commit**

```bash
git add magent-core.el test/test-magent-core.el
git commit -m "feat(core): Work struct with state predicates and git-derived fields"
```

---

### Task 3: Persistence (save/load state)

**Files:**
- Modify: `magent-core.el`
- Modify: `test/test-magent-core.el`

**Step 1: Write tests for persistence**

Add to `test/test-magent-core.el`:

```elisp
(ert-deftest magent-state-save-load-roundtrip ()
  "Works should survive a save/load cycle."
  (let* ((state-file (make-temp-file "magent-state" nil ".el"))
         (magent-state-file state-file)
         (works (list (magent-work--internal-create
                       :dir "/tmp/a" :purpose "task A" :state 'working
                       :session-id "sess-1")
                      (magent-work--internal-create
                       :dir "/tmp/b" :purpose "task B" :state 'done
                       :pr "https://github.com/org/repo/pull/42"))))
    (unwind-protect
        (progn
          (magent-state-save works)
          (let ((loaded (magent-state-load)))
            (should (= (length loaded) 2))
            (should (equal (magent-work-dir (car loaded)) "/tmp/a"))
            (should (equal (magent-work-purpose (car loaded)) "task A"))
            (should (eq (magent-work-state (car loaded)) 'working))
            (should (equal (magent-work-session-id (car loaded)) "sess-1"))
            (should (equal (magent-work-pr (cadr loaded))
                           "https://github.com/org/repo/pull/42"))))
      (delete-file state-file))))

(ert-deftest magent-state-load-missing-file ()
  "Loading from nonexistent file should return nil, not error."
  (let ((magent-state-file "/tmp/magent-nonexistent-state.el"))
    (should (null (magent-state-load)))))
```

**Step 2: Run tests to verify they fail**

Run: `make test`
Expected: FAIL — `magent-state-save` not defined.

**Step 3: Implement persistence**

Add to `magent-core.el` before the `provide` form:

```elisp
(defcustom magent-state-file
  (expand-file-name "magent/state.el" user-emacs-directory)
  "Path to persist Work state."
  :type 'file
  :group 'magent)

(defun magent--work-to-plist (work)
  "Serialize WORK to a plist for persistence."
  (list :dir (magent-work-dir work)
        :purpose (magent-work-purpose work)
        :state (magent-work-state work)
        :session-id (magent-work-session-id work)
        :pr (magent-work-pr work)))

(defun magent--plist-to-work (plist)
  "Deserialize a PLIST to a Work struct."
  (magent-work--internal-create
   :dir (plist-get plist :dir)
   :purpose (plist-get plist :purpose)
   :state (plist-get plist :state)
   :session-id (plist-get plist :session-id)
   :pr (plist-get plist :pr)))

(defun magent-state-save (works)
  "Save WORKS list to `magent-state-file'."
  (let ((dir (file-name-directory magent-state-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file magent-state-file
    (insert ";; -*- lisp-data -*-\n")
    (insert ";; Magent state — do not edit by hand.\n")
    (pp (mapcar #'magent--work-to-plist works) (current-buffer))))

(defun magent-state-load ()
  "Load Works from `magent-state-file'. Return list or nil."
  (when (file-exists-p magent-state-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents magent-state-file)
          (mapcar #'magent--plist-to-work (read (current-buffer))))
      (error nil))))
```

**Step 4: Run tests to verify they pass**

Run: `make test`
Expected: All pass.

**Step 5: Commit**

```bash
git add magent-core.el test/test-magent-core.el
git commit -m "feat(core): state persistence with save/load roundtrip"
```

---

### Task 4: Repo grouping and backlog discovery

**Files:**
- Modify: `magent-core.el`
- Modify: `test/test-magent-core.el`

**Step 1: Write tests for grouping and backlog**

Add to `test/test-magent-core.el`:

```elisp
(ert-deftest magent-group-works-by-repo ()
  "Works should group by their repo field."
  (let ((works (list (magent-work--internal-create
                      :dir "/a/wt1" :repo "/a/" :purpose "x")
                     (magent-work--internal-create
                      :dir "/b/wt1" :repo "/b/" :purpose "y")
                     (magent-work--internal-create
                      :dir "/a/wt2" :repo "/a/" :purpose "z"))))
    (let ((groups (magent-group-by-repo works)))
      (should (= (length groups) 2))
      (should (= (length (cdr (assoc "/a/" groups))) 2))
      (should (= (length (cdr (assoc "/b/" groups))) 1)))))

(ert-deftest magent-count-org-todos ()
  "Should count TODO headings in org files."
  (let ((tmp (make-temp-file "magent-org" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "* TODO First task\n")
            (insert "* DONE Completed task\n")
            (insert "* TODO Second task\n")
            (insert "** TODO Nested task\n")
            (insert "* Not a todo\n"))
          (should (= (magent-count-todos-in-file tmp) 3)))
      (delete-file tmp))))
```

**Step 2: Run tests to verify they fail**

Run: `make test`
Expected: FAIL — functions not defined.

**Step 3: Implement grouping and backlog**

Add to `magent-core.el` before `provide`:

```elisp
(defcustom magent-backlog-glob "*.org"
  "Glob pattern for finding backlog org files in repo roots."
  :type 'string
  :group 'magent)

(defun magent-group-by-repo (works)
  "Group WORKS by repo. Return alist of (repo . works)."
  (let ((groups nil))
    (dolist (w works)
      (let* ((repo (or (magent-work-repo w) "unknown"))
             (cell (assoc repo groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list w)))
          (push (cons repo (list w)) groups))))
    (nreverse groups)))

(defun magent-count-todos-in-file (file)
  "Count TODO headings in an org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(TODO\\|NEXT\\|WAITING\\) " nil t)
        (cl-incf count))
      count)))

(defun magent-repo-todo-count (repo-dir)
  "Count total TODOs across org files in REPO-DIR root."
  (let ((org-files (file-expand-wildcards
                    (expand-file-name magent-backlog-glob repo-dir)))
        (total 0))
    (dolist (f org-files)
      (when (file-regular-p f)
        (cl-incf total (magent-count-todos-in-file f))))
    total))
```

**Step 4: Run tests to verify they pass**

Run: `make test`
Expected: All pass.

**Step 5: Commit**

```bash
git add magent-core.el test/test-magent-core.el
git commit -m "feat(core): repo grouping and org backlog TODO counting"
```

---

### Task 5: Backend protocol and default implementation

**Files:**
- Modify: `magent-backend.el`
- Create: `test/test-magent-backend.el`

**Step 1: Write tests for JSONL parsing**

Create `test/test-magent-backend.el`:

```elisp
;;; test/test-magent-backend.el --- Tests for magent-backend -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent-backend)

(ert-deftest magent-backend-loads ()
  "magent-backend should load without error."
  (should (featurep 'magent-backend)))

(ert-deftest magent-parse-jsonl-result ()
  "Should parse a result event from JSONL."
  (let ((json-str "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\",\"session_id\":\"abc-123\"}"))
    (let ((event (magent--parse-jsonl-line json-str)))
      (should (equal (alist-get 'type event) "result"))
      (should (equal (alist-get 'session_id event) "abc-123")))))

(ert-deftest magent-parse-jsonl-tool-use ()
  "Should extract tool name from assistant message with tool_use."
  (let ((json-str (json-encode
                   '((type . "assistant")
                     (message . ((content . (((type . "tool_use")
                                              (name . "Edit")
                                              (input . ((file_path . "/tmp/foo.ts"))))))))))))
    (let ((event (magent--parse-jsonl-line json-str)))
      (should (equal (alist-get 'type event) "assistant")))))

(ert-deftest magent-extract-tool-info ()
  "Should extract tool name and file from a tool_use event."
  (let ((event '((type . "assistant")
                 (message . ((content . (((type . "tool_use")
                                          (name . "Edit")
                                          (input . ((file_path . "/tmp/foo.ts")))))))))))
    (let ((info (magent--extract-tool-info event)))
      (should (equal (plist-get info :tool) "Edit"))
      (should (equal (plist-get info :file) "/tmp/foo.ts")))))

;;; test/test-magent-backend.el ends here
```

**Step 2: Run tests to verify they fail**

Run: `$(EMACS) --batch -Q -L . --eval "(package-initialize)" -l test/test-magent-backend.el -f ert-run-tests-batch-and-exit`
Expected: FAIL.

**Step 3: Update Makefile to include backend tests**

Update the `test` target in `Makefile`:

```makefile
test:
	$(BATCH) -l test/test-magent-core.el -l test/test-magent-backend.el -f ert-run-tests-batch-and-exit
```

**Step 4: Implement backend protocol and JSONL parsing**

Replace contents of `magent-backend.el` (between Commentary and provide):

```elisp
(require 'cl-lib)
(require 'json)
(require 'magent-core)

;;; Backend protocol

(cl-defgeneric magent-backend-list ()
  "Return list of active session alists.
Each alist has keys: session-id, dir, state, files, recent.")

(cl-defgeneric magent-backend-launch (dir prompt)
  "Start a new agent session in DIR with PROMPT. Return session-id.")

(cl-defgeneric magent-backend-resume (session-id)
  "Resume an idle session.")

(cl-defgeneric magent-backend-send (session-id input)
  "Send INPUT string to a running session.")

(cl-defgeneric magent-backend-kill (session-id)
  "Kill a running session.")

;;; Configuration

(defcustom magent-agent-command "claude"
  "The agent CLI command to invoke."
  :type 'string
  :group 'magent)

(defcustom magent-agent-args
  '("-p" "--dangerously-skip-permissions"
    "--output-format" "stream-json"
    "--verbose")
  "Default arguments passed to the agent CLI."
  :type '(repeat string)
  :group 'magent)

;;; JSONL parsing

(defun magent--parse-jsonl-line (line)
  "Parse a single JSONL LINE into an alist. Return nil on parse failure."
  (condition-case nil
      (json-read-from-string line)
    (error nil)))

(defun magent--extract-tool-info (event)
  "Extract tool name and file path from an assistant tool_use EVENT.
Return plist (:tool NAME :file PATH) or nil."
  (when-let* ((msg (alist-get 'message event))
              (content (alist-get 'content msg)))
    (cl-loop for block across (if (vectorp content) content
                                (vconcat content))
             when (equal (alist-get 'type block) "tool_use")
             return (let ((name (alist-get 'name block))
                          (input (alist-get 'input block)))
                      (list :tool name
                            :file (or (alist-get 'file_path input)
                                      (alist-get 'path input)
                                      (alist-get 'command input)))))))

;;; Process management

(defvar magent--processes (make-hash-table :test 'equal)
  "Map from session-id to process.")

(defvar magent--session-works (make-hash-table :test 'equal)
  "Map from session-id to Work struct for live updates.")

(defun magent--process-filter (session-id)
  "Return a process filter function for SESSION-ID."
  (lambda (_proc output)
    (let ((lines (split-string output "\n" t)))
      (dolist (line lines)
        (when-let ((event (magent--parse-jsonl-line line)))
          (magent--handle-event session-id event))))))

(defun magent--handle-event (session-id event)
  "Update Work state based on EVENT from SESSION-ID."
  (when-let ((work (gethash session-id magent--session-works)))
    (let ((type (alist-get 'type event)))
      (cond
       ;; Tool use — update files and recent
       ((equal type "assistant")
        (when-let ((info (magent--extract-tool-info event)))
          (let ((tool (plist-get info :tool))
                (file (plist-get info :file)))
            (when file
              (cl-pushnew file (magent-work-files work) :test #'equal))
            (push (format "%s(%s)" tool (or file ""))
                  (magent-work-recent work))
            ;; Keep recent to last 5
            (when (> (length (magent-work-recent work)) 5)
              (setf (magent-work-recent work)
                    (seq-take (magent-work-recent work) 5))))))
       ;; Result — agent finished
       ((equal type "result")
        (setf (magent-work-state work) 'needs-input)
        (let ((result (alist-get 'result event)))
          (when (stringp result)
            (push (truncate-string-to-width result 80) (magent-work-recent work))))))
      ;; Trigger UI refresh if buffer exists
      (when-let ((buf (get-buffer "*magent*")))
        (with-current-buffer buf
          (when (fboundp 'magent-refresh-buffer)
            (magent-refresh-buffer)))))))

(defun magent--process-sentinel (session-id)
  "Return a process sentinel for SESSION-ID."
  (lambda (_proc event)
    (when (string-match-p "\\(finished\\|exited\\|killed\\)" event)
      (remhash session-id magent--processes)
      (when-let ((work (gethash session-id magent--session-works)))
        (unless (eq (magent-work-state work) 'done)
          (setf (magent-work-state work) 'needs-input))))))

;;; Default backend methods

(cl-defmethod magent-backend-launch (dir prompt)
  "Launch agent in DIR with PROMPT using `magent-agent-command'."
  (let* ((session-id (format "magent-%s" (make-temp-name "")))
         (default-directory (expand-file-name dir))
         (args (append magent-agent-args (list prompt)))
         (proc (apply #'start-process
                      session-id
                      (format " *magent-proc-%s*" session-id)
                      magent-agent-command
                      args)))
    (set-process-filter proc (magent--process-filter session-id))
    (set-process-sentinel proc (magent--process-sentinel session-id))
    (puthash session-id proc magent--processes)
    session-id))

(cl-defmethod magent-backend-send (session-id input)
  "Send INPUT to the process for SESSION-ID."
  (when-let ((proc (gethash session-id magent--processes)))
    (when (process-live-p proc)
      (process-send-string proc (concat input "\n")))))

(cl-defmethod magent-backend-kill (session-id)
  "Kill the process for SESSION-ID."
  (when-let ((proc (gethash session-id magent--processes)))
    (when (process-live-p proc)
      (kill-process proc))
    (remhash session-id magent--processes)))

(cl-defmethod magent-backend-resume (session-id)
  "Resume a session by launching claude --resume SESSION-ID."
  (when-let ((work (gethash session-id magent--session-works)))
    (let* ((dir (magent-work-dir work))
           (default-directory (expand-file-name dir))
           (args (append (list "-p" "--dangerously-skip-permissions"
                               "--output-format" "stream-json"
                               "--verbose"
                               "--resume" session-id)
                         nil))
           (proc (apply #'start-process
                        session-id
                        (format " *magent-proc-%s*" session-id)
                        magent-agent-command
                        args)))
      (set-process-filter proc (magent--process-filter session-id))
      (set-process-sentinel proc (magent--process-sentinel session-id))
      (puthash session-id proc magent--processes)
      (setf (magent-work-state work) 'working)
      session-id)))

(cl-defmethod magent-backend-list ()
  "Return list of active session info from running processes."
  (let (result)
    (maphash (lambda (sid proc)
               (push (list (cons 'session-id sid)
                           (cons 'alive (process-live-p proc)))
                     result))
             magent--processes)
    result))
```

**Step 5: Run tests to verify they pass**

Run: `make test`
Expected: All pass.

**Step 6: Commit**

```bash
git add magent-backend.el test/test-magent-backend.el Makefile
git commit -m "feat(backend): protocol, JSONL parser, and default process-based implementation"
```

---

### Task 6: UI — magit-section buffer rendering

**Files:**
- Modify: `magent-ui.el`

**Step 1: Implement faces**

Add to `magent-ui.el`:

```elisp
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
```

**Step 2: Implement section types and rendering**

```elisp
(defclass magent-repo-section (magit-section) ()
  "Section for a repository.")

(defclass magent-work-section (magit-section) ()
  "Section for a Work item.")

(defclass magent-file-section (magit-section) ()
  "Section for a file within a Work item.")

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

(defun magent--insert-work (work)
  "Insert a magit-section for WORK."
  (let ((state (magent-work-state work))
        (branch (or (magent-work-branch work)
                    (file-name-nondirectory
                     (directory-file-name (magent-work-dir work))))))
    (magit-insert-section (magent-work-section work)
      (magit-insert-heading
        (propertize (format "  %-25s" branch)
                    'face (magent--state-face state))
        (propertize (format "[%s]" (magent--state-label state))
                    'face (magent--state-face state))
        (let ((recent (magent--format-recent work)))
          (unless (string-empty-p recent)
            (format "  %s" recent))))
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
        (propertize (abbreviate-file-name repo) 'face 'magent-face-repo)
        (when (> todo-count 0)
          (format "  %d TODOs" todo-count)))
      (dolist (w works)
        (magent--insert-work w))
      (insert "\n"))))
```

**Step 3: Implement the main buffer refresh**

```elisp
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
```

**Step 4: Commit**

```bash
git add magent-ui.el
git commit -m "feat(ui): magit-section buffer rendering with faces and section hierarchy"
```

---

### Task 7: UI — Mode, keybindings, and interactive commands

**Files:**
- Modify: `magent-ui.el`
- Modify: `magent.el`

**Step 1: Define magent-mode and keymap in magent-ui.el**

Add to `magent-ui.el`:

```elisp
(defvar magent-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    ;; Refresh
    (define-key map (kbd "g") #'magent-refresh)
    ;; Actions on Work at point
    (define-key map (kbd "RET") #'magent-visit)
    (define-key map (kbd "$") #'magent-show-process)
    (define-key map (kbd "!") #'magent-shell-command)
    (define-key map (kbd "c") #'magent-tell-commit)
    (define-key map (kbd "P") #'magent-tell-pr)
    (define-key map (kbd "d") #'magent-diff)
    (define-key map (kbd "k") #'magent-kill-agent)
    (define-key map (kbd "r") #'magent-resume)
    (define-key map (kbd "b") #'magent-browse-backlog)
    (define-key map (kbd "l") #'magent-event-log)
    (define-key map (kbd "f") #'magent-fetch)
    (define-key map (kbd "i") #'magent-send-input)
    (define-key map (kbd "N") #'magent-new-work)
    (define-key map (kbd "D") #'magent-mark-done)
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
```

**Step 2: Implement the work-at-point helper**

```elisp
(defun magent--work-at-point ()
  "Return the Work struct at point, or nil."
  (when-let ((section (magit-current-section)))
    (cond
     ((magent-work-section-p section)
      (oref section value))
     ((magent-file-section-p section)
      ;; Walk up to parent work section
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
```

**Step 3: Implement interactive commands**

```elisp
(defun magent-visit ()
  "Visit the thing at point — file or agent output."
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
  "Resume an idle agent session."
  (interactive)
  (when-let* ((work (magent--work-at-point))
              (sid (magent-work-session-id work)))
    (if (magent-work-idle-p work)
        (progn
          (magent-backend-resume sid)
          (setf (magent-work-state work) 'working)
          (magent-refresh))
      (message "Work is not idle."))))

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
      (magit-fetch-all (magit-fetch-arguments)))))

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
```

**Step 4: Implement the entry point in magent.el**

Add to `magent.el` before `provide`:

```elisp
(defgroup magent nil
  "Magit-style porcelain for AI agents."
  :group 'tools
  :prefix "magent-")

;;;###autoload
(defun magent ()
  "Open the magent dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*magent*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'magent-mode)
        (magent-mode))
      ;; Load persisted state if we haven't yet
      (unless magent--works
        (setq magent--works (or (magent-state-load) nil)))
      (magent-refresh))
    (switch-to-buffer buf)))
```

**Step 5: Commit**

```bash
git add magent-ui.el magent.el
git commit -m "feat(ui): major mode, keybindings, and interactive commands"
```

---

### Task 8: Integration test — end-to-end smoke test

**Files:**
- Create: `test/test-magent-integration.el`
- Modify: `Makefile`

**Step 1: Write integration test**

Create `test/test-magent-integration.el`:

```elisp
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
          (magent)
          (should (equal (buffer-name) "*magent*"))
          (should (derived-mode-p 'magent-mode))
          (should (string-match-p "Magent" (buffer-string)))
          (should (string-match-p "No active work" (buffer-string))))
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
          (magent)
          (let ((content (buffer-string)))
            (should (string-match-p "repo-a" content))
            (should (string-match-p "repo-b" content))
            (should (string-match-p "feat/alpha" content))
            (should (string-match-p "\\[working\\]" content))
            (should (string-match-p "\\[needs input\\]" content))
            (should (string-match-p "\\[done\\]" content))))
      (when (get-buffer "*magent*")
        (kill-buffer "*magent*"))
      (delete-file magent-state-file))))

;;; test/test-magent-integration.el ends here
```

**Step 2: Update Makefile**

```makefile
test:
	$(BATCH) -l test/test-magent-core.el -l test/test-magent-backend.el -l test/test-magent-integration.el -f ert-run-tests-batch-and-exit
```

**Step 3: Run tests**

Run: `make test`
Expected: All pass.

**Step 4: Commit**

```bash
git add test/test-magent-integration.el Makefile
git commit -m "test: end-to-end integration tests for buffer creation and rendering"
```

---

### Task 9: Final assembly and README

**Files:**
- Modify: `magent.el` (ensure autoloads are clean)
- Verify: all tests pass, byte-compilation clean

**Step 1: Byte-compile to check for warnings**

Run: `make compile`
Expected: No errors. Fix any warnings.

**Step 2: Run full test suite**

Run: `make test`
Expected: All tests pass.

**Step 3: Commit any fixes**

```bash
git add -u
git commit -m "chore: fix byte-compilation warnings"
```

