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

(ert-deftest mir-a-factor-mode-defcustom-exists ()
  "`mir-a-factor-mode' is defined and defaults to `priority-scaled'."
  (should (boundp 'mir-a-factor-mode))
  (should (eq mir-a-factor-mode 'priority-scaled)))

(ert-deftest mir-a-factor-function-defcustom-exists ()
  "`mir-a-factor-function' defaults to `mir--a-factor-priority-scaled'."
  (should (boundp 'mir-a-factor-function))
  (should (eq mir-a-factor-function 'mir--a-factor-priority-scaled)))

(ert-deftest mir-a-factor-priority-scaled-matches-legacy ()
  "Default function reproduces the legacy a-factor math."
  (let ((mir-scale-a-factor-by-priority t))
    (should (= (mir--a-factor-priority-scaled 'ignored 1.5 50 'review)
               (+ 1.2 (/ 50 17.543859)))))
  (let ((mir-scale-a-factor-by-priority nil))
    (should (= (mir--a-factor-priority-scaled 'ignored 1.7 50 'review)
               1.7))))

(provide 'mir-test)
;;; mir-test.el ends here
