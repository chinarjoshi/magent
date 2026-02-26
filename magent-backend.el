;;; magent-backend.el --- Agent process management for magent -*- lexical-binding: t; -*-

;;; Commentary:

;; Process management for agent sessions. Launches, sends to, and
;; monitors agent CLI subprocesses via JSONL streaming.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'magent-core)

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
  "Parse a single JSONL LINE into an alist. Return nil on failure."
  (condition-case nil (json-read-from-string line) (error nil)))

(defun magent--extract-tool-info (event)
  "Extract tool name and file from an assistant tool_use EVENT."
  (when-let* ((msg (alist-get 'message event))
              (content (alist-get 'content msg)))
    (cl-loop for block across (if (vectorp content) content (vconcat content))
             when (equal (alist-get 'type block) "tool_use")
             return (let ((input (alist-get 'input block)))
                      (list :tool (alist-get 'name block)
                            :file (or (alist-get 'file_path input)
                                      (alist-get 'path input)
                                      (alist-get 'command input)))))))

;;; Process management

(defvar magent--processes (make-hash-table :test 'equal)
  "Map from session-id to process.")

(defvar magent--process-buffers (make-hash-table :test 'equal)
  "Map from session-id to partial output buffer string.")

;; Forward declaration — magent--works is defined in magent-ui.el
(defvar magent--works)

(defun magent--find-work (session-id)
  "Find the Work struct for SESSION-ID in `magent--works'."
  (cl-find session-id magent--works
           :key #'magent-work-session-id :test #'equal))

(defun magent--process-filter (session-id)
  "Return a process filter that buffers partial JSONL lines."
  (lambda (_proc output)
    (let* ((prev (gethash session-id magent--process-buffers ""))
           (combined (concat prev output))
           (lines (split-string combined "\n"))
           (partial (car (last lines)))
           (complete (butlast lines)))
      (puthash session-id partial magent--process-buffers)
      (dolist (line complete)
        (unless (string-empty-p line)
          (when-let ((event (magent--parse-jsonl-line line)))
            (magent--handle-event session-id event)))))))

(defun magent--handle-event (session-id event)
  "Update Work state from EVENT."
  (when-let ((work (magent--find-work session-id)))
    (pcase (alist-get 'type event)
      ("assistant"
       (when-let ((info (magent--extract-tool-info event)))
         (let ((file (plist-get info :file)))
           (when file (cl-pushnew file (magent-work-files work) :test #'equal))
           (push (format "%s(%s)" (plist-get info :tool) (or file ""))
                 (magent-work-recent work))
           (when (> (length (magent-work-recent work)) 5)
             (setf (magent-work-recent work)
                   (seq-take (magent-work-recent work) 5))))))
      ("result"
       (setf (magent-work-state work) 'idle)
       (when-let ((result (alist-get 'result event)))
         (when (stringp result)
           (push (truncate-string-to-width result 80)
                 (magent-work-recent work))))))
    (magent--schedule-refresh)))

(defvar magent--refresh-timer nil)

(defun magent--schedule-refresh ()
  "Coalesce rapid refreshes into one idle timer."
  (when magent--refresh-timer (cancel-timer magent--refresh-timer))
  (setq magent--refresh-timer
        (run-with-idle-timer
         0.1 nil
         (lambda ()
           (setq magent--refresh-timer nil)
           (when-let ((buf (get-buffer "*magent*")))
             (with-current-buffer buf
               (when (fboundp 'magent-refresh)
                 (magent-refresh))))))))

(defun magent--process-sentinel (session-id)
  "Return a sentinel that cleans up on process exit."
  (lambda (_proc event)
    (when (string-match-p "\\(finished\\|exited\\|killed\\)" event)
      (remhash session-id magent--processes)
      (remhash session-id magent--process-buffers)
      (when-let ((work (magent--find-work session-id)))
        (unless (magent-work-done-p work)
          (setf (magent-work-state work) 'idle)))
      (magent--schedule-refresh))))

(defun magent--start-agent (session-id dir args)
  "Start agent process for SESSION-ID in DIR with ARGS."
  (let* ((default-directory (expand-file-name dir))
         (proc (apply #'start-process
                      session-id
                      (format " *magent-proc-%s*" session-id)
                      magent-agent-command args)))
    (set-process-filter proc (magent--process-filter session-id))
    (set-process-sentinel proc (magent--process-sentinel session-id))
    (puthash session-id proc magent--processes)
    proc))

;;; Public API

(defun magent-launch (dir prompt)
  "Launch agent in DIR with PROMPT. Return session-id."
  (let ((sid (format "magent-%s" (make-temp-name ""))))
    (magent--start-agent sid dir (append magent-agent-args (list prompt)))
    sid))

(defun magent-send (work input)
  "Send INPUT to WORK's agent. Resumes via --resume if no running process."
  (let* ((sid (magent-work-session-id work))
         (proc (gethash sid magent--processes)))
    (if (and proc (process-live-p proc))
        (process-send-string proc (concat input "\n"))
      ;; No running process — resume session with this input
      (when sid
        (magent--start-agent sid (magent-work-dir work)
                             (append magent-agent-args
                                     (list "--resume" sid input)))
        (setf (magent-work-state work) 'working)))))

(defun magent-resume (work)
  "Resume WORK's agent session."
  (when-let ((sid (magent-work-session-id work)))
    (magent--start-agent sid (magent-work-dir work)
                         (append magent-agent-args
                                 (list "--resume" sid)))
    (setf (magent-work-state work) 'working)))

(defun magent-kill (work)
  "Kill WORK's agent process."
  (when-let* ((sid (magent-work-session-id work))
              (proc (gethash sid magent--processes)))
    (when (process-live-p proc) (kill-process proc))
    (remhash sid magent--processes)))

(provide 'magent-backend)
;;; magent-backend.el ends here
