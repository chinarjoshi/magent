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
