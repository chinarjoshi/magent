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

(ert-deftest magent-handle-event-result ()
  "A result event should set state to needs-input and push result text to recent."
  (let ((magent--session-works (make-hash-table :test 'equal))
        (magent--refresh-timer nil))
    (let ((work (magent-work--internal-create
                 :dir "/tmp/wt" :purpose "test"
                 :state 'working :session-id "sess-1")))
      (puthash "sess-1" work magent--session-works)
      (magent--handle-event "sess-1"
                            '((type . "result")
                              (result . "Task completed")))
      (should (eq (magent-work-state work) 'needs-input))
      (should (member "Task completed" (magent-work-recent work))))))

(ert-deftest magent-handle-event-tool-use ()
  "An assistant tool_use event should add file to files and tool call to recent."
  (let ((magent--session-works (make-hash-table :test 'equal))
        (magent--refresh-timer nil))
    (let ((work (magent-work--internal-create
                 :dir "/tmp/wt" :purpose "test"
                 :state 'working :session-id "sess-2")))
      (puthash "sess-2" work magent--session-works)
      (magent--handle-event
       "sess-2"
       `((type . "assistant")
         (message . ((content . [((type . "tool_use")
                                  (name . "Edit")
                                  (input . ((file_path . "/tmp/foo.ts"))))])))))
      (should (member "/tmp/foo.ts" (magent-work-files work)))
      (should (cl-some (lambda (r) (string-match-p "Edit" r))
                       (magent-work-recent work))))))

(ert-deftest magent-handle-event-recent-capped ()
  "Recent should be capped at 5 items."
  (let ((magent--session-works (make-hash-table :test 'equal))
        (magent--refresh-timer nil))
    (let ((work (magent-work--internal-create
                 :dir "/tmp/wt" :purpose "test"
                 :state 'working :session-id "sess-3"
                 :recent '("act5" "act4" "act3" "act2" "act1"))))
      (puthash "sess-3" work magent--session-works)
      (magent--handle-event
       "sess-3"
       `((type . "assistant")
         (message . ((content . [((type . "tool_use")
                                  (name . "Read")
                                  (input . ((file_path . "/tmp/bar.el"))))])))))
      (should (<= (length (magent-work-recent work)) 5))
      ;; The newest entry should be the one we just added
      (should (string-match-p "Read" (car (magent-work-recent work)))))))

(ert-deftest magent-process-filter-partial-lines ()
  "Process filter should buffer partial lines until a newline arrives."
  (let ((magent--session-works (make-hash-table :test 'equal))
        (magent--process-buffers (make-hash-table :test 'equal))
        (magent--refresh-timer nil))
    (let ((work (magent-work--internal-create
                 :dir "/tmp/wt" :purpose "test"
                 :state 'working :session-id "sess-4")))
      (puthash "sess-4" work magent--session-works)
      (puthash "sess-4" "" magent--process-buffers)
      (let ((filter (magent--process-filter "sess-4"))
            ;; A result event as JSON, split across two filter calls
            (json-line (json-encode '((type . "result") (result . "Done")))))
        ;; Send partial line (no newline)
        (funcall filter nil json-line)
        ;; Nothing should be processed yet — state should remain working
        (should (eq (magent-work-state work) 'working))
        ;; Now send the newline to complete the line
        (funcall filter nil "\n")
        ;; Now the event should have been processed
        (should (eq (magent-work-state work) 'needs-input))))))

(ert-deftest magent-extract-tool-info-vector ()
  "extract-tool-info should handle content as a vector (from json-read-from-string)."
  (let ((event `((type . "assistant")
                 (message . ((content . [((type . "tool_use")
                                          (name . "Write")
                                          (input . ((file_path . "/tmp/out.py"))))]))))))
    (let ((info (magent--extract-tool-info event)))
      (should info)
      (should (equal (plist-get info :tool) "Write"))
      (should (equal (plist-get info :file) "/tmp/out.py")))))

(ert-deftest magent-parse-jsonl-invalid ()
  "parse-jsonl-line should return nil for invalid JSON, not signal an error."
  (should (null (magent--parse-jsonl-line "this is not json {")))
  (should (null (magent--parse-jsonl-line "")))
  (should (null (magent--parse-jsonl-line "{broken"))))

;;; test/test-magent-backend.el ends here
