;;; mir.el --- A holistic Incremental Reading Experience -*- lexical-binding: t; -*-

;; Author: Giorgi Gvalia <gvalia@pm.me>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: incremental-reading srs anki supermemo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows the user to import articles that will be automatically scheduled to be read. This is called incremental reading.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + denote
;; + denote-sequence

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'package-name)

;;;; Usage

;; mir saves all the reading material you import into `mir-archive-directory', so make sure that it is configured. Afterwards, use any of these commands to import the reading material:

;; `mir-import-buffer': Import the contents of the current buffer.
;; `mir-extract-or-import-from-region': Import the selected text from the current buffer.
;; `mir-import-from-minibuffer': Type out or yank what you want to import in the minibuffer.

;; Afterwards, use `mir-read' to start reading and `mir-read-next' to move onto the next topic. If you'd like to extract a part of a topic into a new topic, use `mir-extract-or-import-from-region'.

;;;; Tips

;; + You can customize how the scheduling works.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'denote)
(require 'denote-sequence)

;;;; Customization

(defgroup mir nil
  "Settings for `mir': an emacs-integrated incremental reading package.")

;; If this directory doesn't exist, file write commands error out.
(defcustom mir-archive-directory (expand-file-name "~/Documents/mir/")
  "The directory where all the imported material will be stored.")

(defcustom mir-db-location (concat mir-archive-directory "mir.db")
  "The location for mir's database")

(defcustom mir-default-a-factor 2
  "The default A-factor for new cards.")

(defcustom mir-default-topic-interval 1
  "How soon the newly imported topics will be shown in days. See also
`mir-default-extract-interval' which is the same but for extracts.")

(defcustom mir-default-extract-interval 2
  "How soon an extracted topic will be shown in days. See also
`mir-default-topic-interval' which is the same but for newly imported
topics.")

(defcustom mir-scale-a-factor-by-priority t
  "Whether the A-factor will be scaled based on the topic's priority. The
formula for calculating the new factor is priority/17.543859 + 1.2. This
ensures that the A-factor is between 1.2 and 6.9 for priorities ranging
from 0 to 100 (lower meaning more important topics).")

(defcustom mir-query-function #'mir-get-topics-for-today-by-priority
  "The function that is used to fetch topics for populating `mir-queue'.
The default option is to fetch all the topics that are due today sorted
by priority.")

(defcustom mir-bury-buffer-after-finishing t
  "Whether to bury the current buffer when calling `mir-read-next'. When set to a non-nil value, the user will see the currently open buffer be dismissed at the end of a reading session (when there's no more topics in the queue).")

;;;; Variables

(defvar mir-queue '()
  "The queue for topics to be read.")

(defvar mir--current-topic '()
  "The topic that's under review as of right now. mir uses this variable to
keep track of which topic should be affected by operations")

;;;; Commands

;;;###autoload
(defun mir-extract-or-import-from-region ()
  "Import the selected region as a new topic. If the region is in an
existing topic, the text is extracted as a new descendant."
  (interactive)
  (if-let* (((region-active-p))
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
      ;; if we're somehow in an existing mir topic, do an extract.
      ;; TODO: replace this with a better check.
      (if (string= default-directory mir-archive-directory)
          (mir-extract text)
        (mir-import text (mir-ask-for-priority) (mir-ask-for-name)))
    (user-error "%s" "Region not active, skipping")))

;;;###autoload
(defun mir-import-buffer (name priority)
  "Import the current buffer as a new topic."
  (interactive
   (list (mir-ask-for-name)
         (mir-ask-for-priority)))
  ;; TODO: find a way to preserve images and formatting.
  (mir-import (buffer-substring (point-min) (point-max)) priority name))

;;;###autoload
(defun mir-import-from-minibuffer (text name priority)
  "Import TEXT from the minibuffer as a new topic."
  ;; (interactive "MInscribe your material: \nMName: ")
  (interactive
   (list (read-string "Inscribe your material: " nil nil nil t)
         (mir-ask-for-name)
         (mir-ask-for-priority)))
  (mir-import text priority name))

;;;###autoload
(defun mir-read ()
  "Start a reading session or show the current topic. Use `mir-read-next'
to advance to a new topic."
  (interactive)
  (if mir--current-topic
      (mir-show-topic mir--current-topic)
    (if-let* ((next-topic (mir-queue-next)))
        (mir-show-topic next-topic)
      (user-error "%s" "Queue is empty for today: nothing to read"))))

;;;###autoload
(defun mir-read-next ()
  "Advance to the next topic when reading. Use `mir-read' to start reading.
If no new topic is available, a user error is thrown indicating so."
  (interactive)
  ;; update the db
  ;; this can only be done in a mir topic.
  (when (string= default-directory mir-archive-directory)
    (when mir--current-topic
      (mir-update-topic mir--current-topic))
    ;; set it to nil to prevent repeated invocations of the command from
    ;; updating the topic many many times
    (setq mir--current-topic nil)
    ;; get the next item in the queue
    (if-let* ((next-topic (mir-queue-next)))
        (mir-show-topic next-topic)
      (progn
        (when mir-bury-buffer-after-finishing
          (bury-buffer))
        (user-error "%s" "Queue is now empty: nothing to read")))))

;;;###autoload
(defun mir-dismiss-current-topic ()
  "Dismiss the current topic. The dismissed topic will no longer be
featured in reviews anymore and its corresponding file will have
the 'archive' tag applied to it. Does nothing if invoked outside of
`mir-archive-directory'."
  (interactive)
  ;; this can only be done in a mir topic.
  (when (and (string= default-directory mir-archive-directory)
             (yes-or-no-p "Dismiss the current topic? "))
    (progn (mir--archive-topic mir--current-topic)
           (kill-buffer)
           (setq mir--current-topic nil)
           ;; get the next item in the queue
           (if-let* ((next-topic (mir-queue-next)))
               (mir-show-topic next-topic)
             (progn
               (when mir-bury-buffer-after-finishing
                 (bury-buffer))
               (user-error "%s" "Queue is now empty: nothing to read"))))))

;;;; Functions

;;;;; Public

(defun mir-ask-for-name ()
  "Asks the user for the topic's name."
  ;; Inherits the current input method
  (read-string "Name for topic: " nil nil nil t))

(defun mir-ask-for-priority ()
  "Asks the user for a priority value. Returns a floating point value
between 0 and 100 or an user error otherwise."
  (if-let* ((p (string-to-number (completing-read "Priority (0-100): " nil)))
            (is-within-bounds (and (< p 100)
                                   (> p 0))))
      p
    (user-error "%s" "Priority must be a number between 0 and 100.")))

(defun mir-import (text priority name)
  ;; TODO: ability to add tags/keywords
  (let* ((extension (mir--get-extension-to-current-buffer))
         (file-name (mir--format-file-name name nil extension 'parent))
         (file-id (denote-extract-id-from-string file-name)))
    (mir--add-topic-to-db file-id priority)
    (write-region text nil file-name)))

(defun mir-extract (text)
  (let* ((extension (mir--get-extension-to-current-buffer))
         (name (mir-ask-for-name))
         (parent-sequence (denote-retrieve-filename-signature buffer-file-name))
         (parent-keywords (denote-extract-keywords-from-path buffer-file-name))
         (file-name (mir--format-file-name name parent-keywords extension 'child parent-sequence))
         (file-id (denote-extract-id-from-string file-name)))
    ;; maybe randomly subtract priority vals?
    (mir--add-extract-to-db file-id (nth 1 mir--current-topic))
    (write-region text nil file-name)))

(defun mir-get-topics-for-today-by-priority ()
  "Returns a list of topics due to review today, sorted by priority."
  (sqlite-select (mir--get-db)
                 "SELECT * FROM topics WHERE archived = 0 AND last_review IS NULL OR julianday('now', 'localtime') - julianday(last_review) >= interval ORDER BY priority ASC;"))

(defun mir-show-topic (topic)
  "Navigates to the file corresponding to TOPIC. Raises an user-error if
the file is not found."
  (setq mir--current-topic topic)
  (if-let* ((denote-directory mir-archive-directory)
         (id (car topic))
         (file-path (denote-get-path-by-id id)))
      (find-file file-path)
    ;; Optional: ask the user to delete the entry in db
    (user-error "%s%s%s" "Error: File with ID " id " not found!")))

(defun mir-update-topic (topic)
  (let* ((id (car topic))
         (priority (nth 1 topic))
         (old-af (nth 2 topic))
         (old-interval (nth 3 topic))
         (new-af (if mir-scale-a-factor-by-priority
                     (+ 1.2 (/ priority 17.543859))
                   old-af))
         (new-interval (* old-interval old-af))
         (new-rt (1+ (nth 6 topic))))
    (sqlite-execute (mir--get-db)
                    "UPDATE topics SET last_review = date('now', 'localtime'), a_factor=?, interval=?, times_read=? WHERE id=?;"
                    `(,new-af ,new-interval ,new-rt ,id))
    (sqlite-execute (mir--get-db)
                    "INSERT INTO topic_reviews (topic_id, review_datetime, priority, a_factor) VALUES (?, datetime('now', 'localtime'), ?, ?)"
                    `(,id ,priority ,old-af))))

(defun mir-queue-next ()
  (setq mir-queue (funcall mir-query-function))
  (when mir-queue
      (pop mir-queue)))

(defun mir-set-a-factor ()
    (interactive))

(defun mir-reschedule ()
  ;; how would this work? I think we might need to add a "scheduled"
  ;; column for this...
    (interactive))

(defun mir-set-priority ()
    (interactive))

;; TODO: extract clozes to anki
;; TODO: functions to show stats of the current topic

;; FIXME: When we have both an extract and a fresh topic with the same
;; priority, which one should be shown first?

;;;;; Private

(defun mir--get-db ()
  (sqlite-open mir-db-location))

(defun mir--init-db ()
 (sqlite-pragma (mir--get-db)
                   "foreign_keys = ON;")
 (sqlite-execute (mir--get-db)
                  (concat "CREATE TABLE IF NOT EXISTS topics ("
                          "id TEXT PRIMARY KEY, priority REAL NOT NULL, "
                          "a_factor REAL NOT NULL, interval REAL NOT NULL, "
                          "added TEXT NOT NULL, last_review TEXT, "
                          "times_read INTEGER NOT NULL, "
                          "archived INT NOT NULL, archived_date TEXT) STRICT; "))
  (sqlite-execute (mir--get-db)
                  (concat "CREATE TABLE IF NOT EXISTS topic_reviews ("
                          "topic_id TEXT NOT NULL, "
                          "review_datetime TEXT NOT NULL, "
                          "priority REAL NOT NULL, a_factor REAL NOT NULL, "
                          "FOREIGN KEY (topic_id) REFERENCES topics(id)"
                          ") STRICT;")))

(defun mir--add-topic-to-db (id priority &optional is-extract)
  (mir--init-db)
  (sqlite-execute (mir--get-db)
                    "INSERT INTO topics (id, priority, a_factor, interval, added, times_read, archived) VALUES(?, ?, ?, ?, date('now', 'localtime'), 0, 0)"
                    `(,id
                      ,priority
                      ,mir-default-a-factor
                      ,mir-default-topic-interval)))

(defun mir--add-extract-to-db (id priority)
  (mir--init-db)
  (sqlite-execute (mir--get-db)
                  "INSERT INTO topics (id, priority, a_factor, interval, added, last_review, times_read, archived) VALUES(?, ?, ?, ?, date('now', 'localtime'), date('now', 'localtime'), 1, 0)"
                  `(,id
                    ,priority
                    ,mir-default-a-factor
                    ,mir-default-extract-interval)))

(defun mir--format-file-name (name tags extension seq-type &optional parent-sequence)
  "Wrapper around `denote-format-file-name' for now."
  (let* ((denote-directory mir-archive-directory)
         (sequence (denote-sequence-get-new seq-type parent-sequence))
         (denote-use-signature sequence))
    (denote-format-file-name
     mir-archive-directory
     (denote-get-identifier (current-time))
     tags name extension sequence)))

;; Problem: importing from epubs and naming the file epub works with
;; emacs but it's not a TRUE epub.
(defun mir--get-extension-to-current-buffer ()
  "We assume that everything is .txt for now"
  ".txt")

(defun mir--archive-topic-db (id)
  ;; this does not work. I should create an archived column.
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET last_review = date('now', 'localtime'), archived = 1, archived_date = datetime('now', 'localtime') WHERE id = ?;" `(,id)))

(defun mir--archive-topic (topic)
  (let* ((id (car topic))
         (denote-directory mir-archive-directory)
         (denote-save-buffers t)
         (denote-kill-buffers t)
         (topic-file-path (denote-get-path-by-id id))
         (old-keywords (denote-extract-keywords-from-path topic-file-path))
         (new-keywords (add-to-list 'old-keywords "archive")))
    (denote-rename-file topic-file-path
                        'keep-current
                        new-keywords
                        'keep-current
                        'keep-current)
    (mir--archive-topic-db id)))

;;;; Footer

(provide 'mir)

;;; package-name.el ends here
