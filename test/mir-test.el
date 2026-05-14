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

(ert-deftest mir-do-topic-review-db-uses-a-factor-function ()
  "`mir--do-topic-review-db' routes through `mir-a-factor-function'."
  (let* ((calls nil)
         (mir-a-factor-function
          (lambda (id old-af priority event)
            (push (list id old-af priority event) calls)
            3.14)))
    ;; Use a sandbox DB so we don't touch user data.
    (let* ((tmp (make-temp-file "mir-test-db-"))
           (mir-db-location tmp))
      (unwind-protect
          (progn
            (mir--init-db)
            (sqlite-execute (mir--get-db)
                            "INSERT INTO topics (id, priority, a_factor, interval, due, added, times_read, archived, title) VALUES('TID', 50.0, 2.0, 1, date('now'), datetime('now'), 0, 0, 't');")
            (let ((topic (car (mir--select-topic-db "TID"))))
              (mir--do-topic-review-db topic))
            (should (equal calls '(("TID" 2.0 50.0 review)))))
        (delete-file tmp)))))

(defconst mir-test--af-tol 0.01)

(ert-deftest mir-initial-a-factor-boundaries ()
  "Initial A-factor curve produces expected values at known points."
  ;; Sample table from design spec section 3.2:
  ;;   10 units  → 2.50
  ;;   50 units  → 1.92
  ;;   100 units → 1.67
  ;;   345 units → 1.22
  ;;   500 units → ~1.10 (clamped near min)
  (should (< (abs (- (mir--initial-a-factor 10)  2.50)) mir-test--af-tol))
  (should (< (abs (- (mir--initial-a-factor 50)  1.92)) mir-test--af-tol))
  (should (< (abs (- (mir--initial-a-factor 100) 1.67)) mir-test--af-tol))
  (should (< (abs (- (mir--initial-a-factor 345) 1.22)) mir-test--af-tol)))

(ert-deftest mir-initial-a-factor-clamps ()
  (should (>= (mir--initial-a-factor 100000) mir-a-factor-min))
  (should (<= (mir--initial-a-factor 100000) mir-a-factor-max))
  (should (= (mir--initial-a-factor 0) mir-default-a-factor))
  (should (= (mir--initial-a-factor nil) mir-default-a-factor)))

(provide 'mir-test)
;;; mir-test.el ends here
