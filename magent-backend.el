;;; magent-backend.el --- Backend protocol for magent -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic protocol and default implementation for agent backends.

;;; Code:

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

(provide 'magent-backend)
;;; magent-backend.el ends here
