;;; mir-test.el --- Tests for mir -*- lexical-binding: t; -*-

(require 'ert)
(require 'mir)

(ert-deftest mir-loads ()
  "mir.el can be required without errors."
  (should (featurep 'mir)))

(ert-deftest mir-default-a-factor-is-numeric ()
  "`mir-default-a-factor' is a positive number."
  (should (numberp mir-default-a-factor))
  (should (> mir-default-a-factor 0)))

(provide 'mir-test)
;;; mir-test.el ends here
