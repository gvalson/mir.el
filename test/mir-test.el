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

(defun mir-test--column-names (db table)
  "List column names of TABLE in DB."
  (mapcar (lambda (row) (nth 1 row))
          (sqlite-select db (format "PRAGMA table_info(%s);" table))))

(ert-deftest mir-init-db-adds-content-units ()
  "`mir--init-db' creates the `content_units' column for new DBs."
  (let* ((tmp (make-temp-file "mir-test-db-"))
         (mir-db-location tmp))
    (unwind-protect
        (progn
          (mir--init-db)
          (should (member "content_units"
                          (mir-test--column-names (mir--get-db) "topics"))))
      (delete-file tmp))))

(ert-deftest mir-init-db-migrates-existing ()
  "`mir--init-db' adds `content_units' to a pre-existing schema."
  (let* ((tmp (make-temp-file "mir-test-db-"))
         (mir-db-location tmp))
    (unwind-protect
        (progn
          ;; Create the old (pre-migration) schema by hand:
          (sqlite-execute
           (mir--get-db)
           (concat "CREATE TABLE topics ("
                   "id TEXT PRIMARY KEY, priority REAL NOT NULL, "
                   "a_factor REAL NOT NULL, interval REAL NOT NULL, "
                   "added TEXT NOT NULL, last_review TEXT, "
                   "times_read INTEGER NOT NULL, "
                   "archived INT NOT NULL, archived_date TEXT, "
                   "title TEXT, due TEXT NOT NULL) STRICT;"))
          (mir--init-db)
          (should (member "content_units"
                          (mir-test--column-names (mir--get-db) "topics"))))
      (delete-file tmp))))

(ert-deftest mir-init-db-migration-idempotent ()
  "Running `mir--init-db' twice does not error."
  (let* ((tmp (make-temp-file "mir-test-db-"))
         (mir-db-location tmp))
    (unwind-protect
        (progn
          (mir--init-db)
          (mir--init-db)
          (should (member "content_units"
                          (mir-test--column-names (mir--get-db) "topics"))))
      (delete-file tmp))))

(ert-deftest mir-add-topic-persists-content-units ()
  "When provided, `content_units' is stored on the new topic row."
  (let* ((tmp (make-temp-file "mir-test-db-"))
         (mir-db-location tmp))
    (unwind-protect
        (progn
          (mir--init-db)
          (mir--add-topic-to-db "T1" 50.0 "title" nil 345)
          (let* ((row (car (mir--select-topic-db "T1")))
                 (db (mir--get-db))
                 (units (caar (sqlite-select
                               db
                               "SELECT content_units FROM topics WHERE id = ?"
                               '("T1")))))
            (should (= units 345))))
      (delete-file tmp))))

(ert-deftest mir-bump-a-factor-clamps ()
  "Bumps stay within [min, max]."
  (let* ((tmp (make-temp-file "mir-test-db-"))
         (mir-db-location tmp))
    (unwind-protect
        (progn
          (mir--init-db)
          (sqlite-execute (mir--get-db)
                          "INSERT INTO topics (id, priority, a_factor, interval, due, added, times_read, archived, title) VALUES('T1', 50.0, 4.9, 1, date('now'), datetime('now'), 0, 0, 't');")
          (mir--bump-a-factor "T1" 1.10)
          ;; 4.9 * 1.10 = 5.39 > max → clamped to 5.0
          (let ((af (nth 2 (car (mir--select-topic-db "T1")))))
            (should (= af mir-a-factor-max)))
          (sqlite-execute (mir--get-db)
                          "UPDATE topics SET a_factor = 1.10 WHERE id = 'T1';")
          (mir--bump-a-factor "T1" 0.90)
          ;; 1.10 * 0.90 = 0.99 < min → clamped to 1.05
          (let ((af (nth 2 (car (mir--select-topic-db "T1")))))
            (should (= af mir-a-factor-min))))
      (delete-file tmp))))

(ert-deftest mir-bump-a-factor-multiplies ()
  (let* ((tmp (make-temp-file "mir-test-db-"))
         (mir-db-location tmp))
    (unwind-protect
        (progn
          (mir--init-db)
          (sqlite-execute (mir--get-db)
                          "INSERT INTO topics (id, priority, a_factor, interval, due, added, times_read, archived, title) VALUES('T1', 50.0, 2.0, 1, date('now'), datetime('now'), 0, 0, 't');")
          (mir--bump-a-factor "T1" 1.05)
          (let ((af (nth 2 (car (mir--select-topic-db "T1")))))
            (should (< (abs (- af 2.10)) mir-test--af-tol))))
      (delete-file tmp))))

(provide 'mir-test)
;;; mir-test.el ends here
