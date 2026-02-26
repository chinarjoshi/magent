;;; test/test-magent-backend.el --- Tests for magent-backend -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent-backend)

(ert-deftest magent-backend-loads ()
  (should (featurep 'magent-backend)))

(ert-deftest magent-parse-jsonl-result ()
  (let ((event (magent--parse-jsonl-line
                "{\"type\":\"result\",\"result\":\"done\",\"session_id\":\"abc\"}")))
    (should (equal (alist-get 'type event) "result"))
    (should (equal (alist-get 'session_id event) "abc"))))

(ert-deftest magent-parse-jsonl-invalid ()
  (should (null (magent--parse-jsonl-line "not json {")))
  (should (null (magent--parse-jsonl-line "")))
  (should (null (magent--parse-jsonl-line "{broken"))))

(ert-deftest magent-extract-tool-info ()
  (let ((event '((type . "assistant")
                 (message . ((content . (((type . "tool_use")
                                          (name . "Edit")
                                          (input . ((file_path . "/tmp/foo.ts")))))))))))
    (let ((info (magent--extract-tool-info event)))
      (should (equal (plist-get info :tool) "Edit"))
      (should (equal (plist-get info :file) "/tmp/foo.ts")))))

(ert-deftest magent-extract-tool-info-vector ()
  (let ((event `((type . "assistant")
                 (message . ((content . [((type . "tool_use")
                                          (name . "Write")
                                          (input . ((file_path . "/tmp/out.py"))))]))))))
    (let ((info (magent--extract-tool-info event)))
      (should (equal (plist-get info :tool) "Write"))
      (should (equal (plist-get info :file) "/tmp/out.py")))))

(ert-deftest magent-handle-event-result ()
  "Result event should set state to idle."
  (let ((magent--works (list (make-magent-work
                              :dir "/tmp/wt" :state 'working :session-id "s1")))
        (magent--refresh-timer nil))
    (magent--handle-event "s1" '((type . "result") (result . "Done")))
    (should (eq (magent-work-state (car magent--works)) 'idle))
    (should (member "Done" (magent-work-recent (car magent--works))))))

(ert-deftest magent-handle-event-tool-use ()
  "Tool use event should add file and tool to Work."
  (let ((magent--works (list (make-magent-work
                              :dir "/tmp/wt" :state 'working :session-id "s2")))
        (magent--refresh-timer nil))
    (magent--handle-event
     "s2"
     `((type . "assistant")
       (message . ((content . [((type . "tool_use")
                                (name . "Edit")
                                (input . ((file_path . "/tmp/foo.ts"))))])))))
    (should (member "/tmp/foo.ts" (magent-work-files (car magent--works))))
    (should (cl-some (lambda (r) (string-match-p "Edit" r))
                     (magent-work-recent (car magent--works))))))

(ert-deftest magent-handle-event-recent-capped ()
  (let ((magent--works (list (make-magent-work
                              :dir "/tmp/wt" :state 'working :session-id "s3"
                              :recent '("a5" "a4" "a3" "a2" "a1"))))
        (magent--refresh-timer nil))
    (magent--handle-event
     "s3"
     `((type . "assistant")
       (message . ((content . [((type . "tool_use")
                                (name . "Read")
                                (input . ((file_path . "/tmp/bar.el"))))])))))
    (should (<= (length (magent-work-recent (car magent--works))) 5))
    (should (string-match-p "Read" (car (magent-work-recent (car magent--works)))))))

(ert-deftest magent-process-filter-partial-lines ()
  (let ((magent--works (list (make-magent-work
                              :dir "/tmp/wt" :state 'working :session-id "s4")))
        (magent--process-buffers (make-hash-table :test 'equal))
        (magent--refresh-timer nil))
    (puthash "s4" "" magent--process-buffers)
    (let ((filter (magent--process-filter "s4"))
          (json-line (json-encode '((type . "result") (result . "Done")))))
      ;; Partial — no newline
      (funcall filter nil json-line)
      (should (eq (magent-work-state (car magent--works)) 'working))
      ;; Complete — newline
      (funcall filter nil "\n")
      (should (eq (magent-work-state (car magent--works)) 'idle)))))

;;; test/test-magent-backend.el ends here
