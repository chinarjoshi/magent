;;; test/test-magent-core.el --- Tests for magent-core -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'magent-core)

(ert-deftest magent-core-loads ()
  "magent-core should load without error."
  (should (featurep 'magent-core)))

;;; test/test-magent-core.el ends here
