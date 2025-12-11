;;; mir.el --- A holistic Incremental Reading Experience -*- lexical-binding: t; -*-

;; Author: Giorgi Gvalia <gvalia@pm.me>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1") denote denote-sequence)
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

;; This package allows the user to import articles that will be
;; automatically scheduled to be read. This is called incremental
;; reading.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + denote
;; + denote-sequence

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'mir)

;;;; Usage

;; mir saves all the reading material you import into
;; `mir-archive-directory', so make sure that it is configured to the
;; folder you like. Afterwards, use any of these commands to import
;; the reading material:

;; `mir-import-buffer': Import the contents of the current buffer.
;; `mir-extract-or-import-from-region': Import the selected text from the current buffer.
;; `mir-import-from-minibuffer': Type out or yank what you want to import in the minibuffer.

;; Afterwards, use `mir-read' to start reading and `mir-read-next' to
;; move onto the next topic. If you'd like to extract a part of a
;; topic into a new topic, select the text and use
;; `mir-extract-or-import-from-region'. Note that, by default,
;; imported topics are not shown until 1 day passes after importing.
;; This is, for now, a deliberate choice which can be overridden by
;; configuring `mir-default-topic-interval'.

;;;; Tips

;; + You can customize how the scheduling works.

;;;; Credits

;; This package would not have been possible without the following
;; packages: denote[1], which provides a consistent ID scheme and file
;; naming system, and denote-sequence[2], which makes it easy to work
;; with tree-like sequences of notes.
;;
;;  [1] https://protesilaos.com/emacs/denote
;;  [2] https://protesilaos.com/emacs/denote-sequence

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

(defcustom mir-randomize-topics-ratio 0
  "The ratio of topics that should be randomized in the queue from 0.0 to
1.0. 0 means that the queue will fully follow the order given by
`mir-query-function'. 1.0 means that the ordering will be
deterministically random. A fixed seed is used here to prevent chaotic
reordering.")

(defcustom mir-bury-buffer-after-finishing t
  "Whether to bury the current buffer when calling `mir-read-next'.
  When set to a non-nil value, the user will see the currently open
  buffer be dismissed at the end of a reading session (when there's no
  more topics in the queue).")

(defcustom mir-default-file-extension ".txt"
  "The file extension of newly imported topics.")

(defcustom mir-inherit-extension-for-extracts nil
  "Whether extracts of a topic should inherit the file extension of the
current buffer. This can be handy when processing data in a specific
format but can also cause problems with files that have special
rendering such as epubs or pdfs.")

;;;; Variables

(defvar mir-queue '()
  "The queue for topics to be read.")

(defvar mir--current-topic '()
  "The topic that's under review as of right now. mir uses this variable to
keep track of which topic should be affected by operations")

(defvar mir-show-topic-hook '()
  "Hooks for `mir-show-topic'; runs after a topic is displayed.")

;;;; Commands

;;;###autoload
(defun mir-extract-or-import-from-region ()
  "Import the selected region as a new topic. If the region is in an
existing topic, the text is extracted as a new descendant."
  (interactive)
  (if-let* (((region-active-p))
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
      ;; if we're somehow in an existing mir topic, do an extract.
      ;; TODO: replace this with mir-minor-mode check
      (if (string= default-directory mir-archive-directory)
          (mir--add-extract text)
        (mir-import text (mir-ask-for-priority) (mir-ask-for-title)))
    (user-error "%s" "Region not active, skipping")))

;;;###autoload
(defun mir-import-buffer (title priority)
  "Import the current buffer as a new topic."
  (interactive
   (list (mir-ask-for-title)
         (mir-ask-for-priority)))
  (mir-import (buffer-substring (point-min) (point-max)) priority title))

;;;###autoload
(defun mir-import-from-minibuffer (text title priority)
  "Import TEXT from the minibuffer as a new topic with TITLE."
  (interactive
   (list (read-string "Inscribe your material: " nil nil nil t)
         (mir-ask-for-title)
         (mir-ask-for-priority)))
  (mir-import text priority title))

(defun mir-import-file (file)
  "Import FILE by copying it into `mir-archive-directory' and renaming it."
  (interactive "fImport file: ")
  (let* ((extension (concat "." (file-name-extension file)))
         (new-file-name
          (concat
           mir-archive-directory
           (file-name-base file)
           extension))
         ;; Might be worth it to replace "_" with " "?
         (title (file-name-base file))
         (denote-directory mir-archive-directory)
         (tags (denote-keywords-prompt))
         (sequence (denote-sequence-get-new 'parent)))
    (copy-file file new-file-name)
    (let* ((new-new-file-name
            (denote-rename-file new-file-name 'keep-current tags
                                sequence (current-time)))
           (priority (mir-ask-for-priority))
           (id (denote-retrieve-filename-identifier new-new-file-name)))
      (mir--add-topic-to-db id priority title))))

;;;###autoload
(defun mir-read ()
  ;; TODO: Read the user prefix to refresh the topic
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
      (mir--do-topic-review-db mir--current-topic))
    (mir-show-next-topic)))

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
           (mir-show-next-topic))))

(defun mir-show-topic-metadata ()
  "Print out the current topic's ID, priority, A-factor and interval."
  (interactive)
  (setq mir--current-topic (mir-queue-next))
  (if mir--current-topic
      (message (format
                "Topic ID: %s; Priority: %.3f; A-Factor: %.3f; Interval: %d;"
                (car mir--current-topic)
                (nth 1 mir--current-topic)
                (nth 2 mir--current-topic)
                (nth 3 mir--current-topic)))
    (message "No topic is currently active.")))

(define-derived-mode mir-queue-list-mode tabulated-list-mode "Mir Queue"
  "Major mode for a tabular listing of mir topics currently in the queue."
  (setq tabulated-list-format (vector '("ID" 18 t)
                                      '("Title" 35 t)
                                      '("Priority" 10 t)
                                      '("A-Factor" 8 t)
                                      '("Interval" 8 t))
        tabulated-list-revert-hook #'mir--format-queue-for-tabular-list
        tabulated-list-entries #'mir--format-queue-for-tabular-list)
  (tabulated-list-init-header))

(defun mir--format-queue-for-tabular-list ()
  "Format `mir-queue' to be displayed in `mir-queue-list-mode'."
  (mir-update-queue)
  (mapcar
     (lambda (topic)
       (let ((id (car topic))
             (title (or (nth 9 topic) "Untitled"))
             (priority (number-to-string (nth 1 topic)))
             (a-factor (number-to-string (nth 2 topic)))
             (interval (number-to-string (nth 3 topic))))
         (list id (vector id title priority a-factor interval))))
     mir-queue))

(defun mir-show-queue ()
  "Show the current queue of topics."
  (interactive)
  (switch-to-buffer "*mir-queue*")
  (mir-queue-list-mode)
  (revert-buffer nil t t))

;; -----
;; Start all topics mode

(define-derived-mode mir-topics-list-mode tabulated-list-mode "Mir Topics"
  "Major mode for a tabular listing of all mir topics."
  (setq tabulated-list-format (vector '("ID" 18 t)
                                      '("Title" 35 t)
                                      '("Priority" 10 t)
                                      '("A-Factor" 8 t)
                                      '("Interval" 8 t)
                                      '("Added" 10 t)
                                      '("Last Read" 10 t)
                                      '("Reviews" 7 t)
                                      '("Archived" 8 t)
                                      '("Arch. on" 8 t))
        tabulated-list-revert-hook #'mir--format-all-topics-for-tabular-list
        tabulated-list-entries #'mir--format-all-topics-for-tabular-list)
  (tabulated-list-init-header))

(defun mir--format-all-topics-for-tabular-list ()
  "Format all the topics to be displayed in `mir-queue-list-mode'."
  (let ((topics
         (sqlite-select (mir--get-db)
                        "SELECT * FROM topics")))
    (mapcar
     (lambda (topic)
       (let ((id (car topic))
             (title (or (nth 9 topic) "Untitled"))
             (priority (number-to-string (nth 1 topic)))
             (a-factor (number-to-string (nth 2 topic)))
             (interval (number-to-string (nth 3 topic)))
             (added (nth 4 topic))
             (last-read (or (nth 5 topic) ""))
             (reviews (number-to-string (nth 6 topic)))
             (archived (number-to-string (nth 7 topic)))
             (archived-date (or (nth 8 topic) "")))
         (list id (vector
                   id title priority a-factor interval
                   added last-read reviews archived
                   archived-date))))
     topics)))

;; How to make this work for multiple topics?
(defun mir-topics-change-priority ()
  ""
  (interactive)
  (let* ((current-topic (tabulated-list-get-entry nil))
         (id (aref current-topic 0))
         (new-priority (mir-ask-for-priority)))
    (mir--update-priority-db id new-priority)
    (message "Set new priority to %f" new-priority)))

(defun mir-show-all-topics ()
  "Show all the topics in the database"
  (interactive)
  (switch-to-buffer "*mir-topics*")
  (mir-topics-list-mode)
  (revert-buffer nil t t))

;; -----

(defun mir-set-a-factor ()
  "Set a new A-factor for the current topic."
  (interactive)
  (if (mir--refresh-current-topic)
      (let ((new-af (read-number "New A-Factor: "
                                 (nth 2 mir--current-topic)))
            (id (car mir--current-topic)))
        (mir--update-af-db id new-af)
        (message "Set new A-factor to %f" new-af))
    (user-error "%s" "No active topic.")))

(defun mir-set-priority ()
  "Set a new priority for the current topic."
  (interactive)
  (if (mir--refresh-current-topic)
      (let* ((old-priority (nth 1 mir--current-topic))
             (new-priority (mir-ask-for-priority (format "%.3f" old-priority)))
             (id (car mir--current-topic)))
        (mir--update-priority-db id new-priority))
        (message "Set new priority to %f" new-priority)
    (user-error "%s" "No active topic.")))

(defun mir-set-title ()
  "Set a new title for the current topic."
  (interactive)
  (if (mir--refresh-current-topic)
      (let ((new-title (mir-ask-for-title))
            (id (car mir--current-topic)))
        (mir--update-title-db id new-title)
        ;; TODO: actually rename the file to the new title
        (message "Title now set to '%s'" new-title))
    (user-error "%s" "No active topic.")))

(defun mir-reschedule ()
  ;; how would this work? I think we might need to add a "scheduled"
  ;; column for this...
  ;; already have `mir--update-interval-db'.
    (interactive))

(defun mir-find-parent ()
  "Display the parent topic of the current topic. This is only relevant for
topics that were made via being extracted from some other topic (which
we call the parent)."
  (interactive)
  (let ((denote-directory mir-archive-directory))
    (denote-sequence-find 'parent)))

;;;; Functions

;;;;; Public

(defun mir-ask-for-title ()
  "Asks the user for the topic's title."
  ;; Inherits the current input method
  (read-string "Name for topic: " nil nil nil t))

(defun mir-ask-for-priority (&optional old-priority)
  "Asks the user for a priority value. Returns a floating point value
between 0 and 100 or an user error otherwise. Optionally use
OLD-PRIORITY as the default value."
  (if-let* ((p (string-to-number
                (completing-read "Priority (0-100): " nil nil nil old-priority)))
            (is-within-bounds (and (<= p 100)
                                   (>= p 0))))
      p
    (user-error "%s" "Priority must be a number between 0 and 100.")))

(defun mir-import (text priority title)
  ;; TODO: ability to add tags/keywords
  (let* ((extension (mir--get-extension-to-current-buffer))
         (file-name (mir--format-file-name title nil extension 'parent))
         (file-id (denote-extract-id-from-string file-name)))
    (mir--add-topic-to-db file-id priority title)
    (write-region text nil file-name)))

(defun mir-get-topics-for-today-by-priority ()
  "Returns a list of topics due to review today, sorted by priority."
  ;; Question here: should topics that have already been reviewed get
  ;; special treatement?
  ;;
  ;; Here, "last_review IS NULL" returns all the topics that have
  ;; never been reviewed yet. Does SuperMemo prioritize already
  ;; reviewed topics over these? Does it show you all the stuff you've
  ;; already seen before beginning with the unseen stuff?
  (sqlite-select (mir--get-db)
                 "SELECT * FROM topics WHERE archived = 0 AND (last_review IS NULL OR julianday('now', 'localtime') - julianday(last_review) >= interval) ORDER BY priority ASC;"))

(defun mir-show-topic (topic)
  "Navigates to the file corresponding to TOPIC. Runs
`mir-show-topic-hook' after opening the buffer. Raises a user-error if
the file is not found."
  (setq mir--current-topic topic)
  (if-let* ((denote-directory mir-archive-directory)
            (id (car topic))
            ;; FIXME: hacky, this should not be defined here.
            (denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}[0-9]*\\)")
            (file-path (denote-get-path-by-id id)))
      (progn (find-file file-path)
             (run-hooks 'mir-show-topic-hook))
    ;; Optional: ask the user to delete the entry in db
    (user-error "%s%s%s" "Error: File with ID " id " not found!")))

(defun mir-show-next-topic ()
  "Load the next topic. Runs `mir-show-topic-hook' after loading. Returns a user error if there's nothing left in the queue."
  ;; Set the current topic to nil to prevent repeated invocations of
  ;; the command from updating the topic many many times.
  (setq mir--current-topic nil)
  ;; get the next item in the queue
  (if-let* ((next-topic (mir-queue-next)))
      (mir-show-topic next-topic)
    (progn
      (when mir-bury-buffer-after-finishing
        (bury-buffer))
      (user-error "%s" "Queue is now empty: nothing to read"))))

(defun mir-update-queue ()
    "Update the queue by calling `mir-query-function' and randomizing the
result via `mir--randomize-queue'."
    (setq mir-queue (mir--randomize-queue (funcall mir-query-function))))

(defun mir--randomize-queue (queue)
  "Shuffle QUEUE according to `mir-randomize-topics-ratio'. Uses a fixed
seed of 'mir' in order to prevent repeated invocations from randomly
redistributing the determined order."
  ;; Swap randomly determined elements A and B in the queue.
  (unless (and (<= mir-randomize-topics-ratio 1.0)
               (>= mir-randomize-topics-ratio 0.0))
    (user-error "%s"
                "`mir-randomize-topics-ratio' set to an invalid value. Make sure it's between 0.0 and 1.0"))
  (let* ((n-list (seq-length queue))
         (n-swap (round (* mir-randomize-topics-ratio n-list))))
    (random "mir")
    (dotimes (i n-swap)
      ;; Here we don't care if they end up the same. Based on:
      ;; https://www.emacswiki.org/emacs/ListModificationLibrary
      (let ((swap-a (elt queue (random n-list)))
            (swap-b (elt queue (random n-list))))
        (setcar (member swap-a queue) swap-b)
        (setcar (member swap-b queue) swap-a)))
    queue))

(defun mir-queue-next ()
  "Return the next item in the queue or nil otherwise."
  (mir-update-queue)
  (when mir-queue
      (pop mir-queue)))

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
                          "archived INT NOT NULL, archived_date TEXT, "
                          ;; should this allow null values?
                          "title TEXT) STRICT; "))
 (sqlite-execute (mir--get-db)
                 "CREATE INDEX IF NOT EXISTS idx_topics_archived ON topics(archived);")
 (sqlite-execute (mir--get-db)
                 "CREATE INDEX IF NOT EXISTS idx_topics_priority_id ON topics(priority, id);")
 (sqlite-execute (mir--get-db)
                  (concat "CREATE TABLE IF NOT EXISTS topic_reviews ("
                          "topic_id TEXT NOT NULL, "
                          "review_datetime TEXT NOT NULL, "
                          "priority REAL NOT NULL, a_factor REAL NOT NULL, "
                          "FOREIGN KEY (topic_id) REFERENCES topics(id)"
                          ") STRICT;")))

(defun mir--rescale-priority-values-db ()
  "Rescale priority values so that every topic's priority is between 0.0
and 100.0 and the distance between their values is equal. For example,
if there were 5 topics in the database, their priorities would be 0.0,
25.0, 50.0, 75.0 and 100.0 respectively. This function should be called
after adding a topic or modifying an existing topic's priority in some
way."
  (sqlite-execute (mir--get-db)
                  (concat
                   "WITH ordered AS (SELECT id, priority, "
                   "ROW_NUMBER() OVER (ORDER BY priority, id) "
                   "AS rn, COUNT(*) OVER () AS total FROM topics "
                   "WHERE archived = 0) UPDATE topics "
                   "SET priority = (SELECT (rn - 1) * "
                   "(100.0/(total-1)) FROM ordered "
                   "WHERE ordered.id = topics.id) "
                   "WHERE archived = 0;")))

(defun mir--add-topic-to-db (id priority title &optional is-extract)
  (mir--init-db)
  (sqlite-execute (mir--get-db)
                    "INSERT INTO topics (id, priority, a_factor, interval, added, times_read, archived, title) VALUES(?, ?, ?, ?, date('now', 'localtime'), 0, 0, ?)"
                    `(,id
                      ,priority
                      ,mir-default-a-factor
                      ,mir-default-topic-interval
                      ,title))
  (mir--rescale-priority-values-db))

(defun mir--add-extract-to-db (id priority title)
  (mir--init-db)
  (sqlite-execute (mir--get-db)
                  "INSERT INTO topics (id, priority, a_factor, interval, added, last_review, times_read, archived, title) VALUES(?, ?, ?, ?, date('now', 'localtime'), date('now', 'localtime'), 1, 0, ?)"
                  `(,id
                    ,priority
                    ,mir-default-a-factor
                    ,mir-default-extract-interval
                    ,title))
  (mir--rescale-priority-values-db))

(defun mir--archive-topic-db (id)
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET last_review = date('now', 'localtime'), archived = 1, archived_date = datetime('now', 'localtime') WHERE id = ?;" `(,id))
  (mir--rescale-priority-values-db))

(defun mir--do-topic-review-db (topic)
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

(defun mir--update-af-db (id a-factor)
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET a_factor=? WHERE id=?"
                  `(,a-factor ,id)))

(defun mir--update-priority-db (id priority)
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET priority=? WHERE id=?"
                  `(,priority ,id))
  (mir--rescale-priority-values-db))

(defun mir--update-interval-db (id interval)
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET interval=? WHERE id=?"
                  `(,interval ,id)))

(defun mir--update-title-db (id new-title)
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET title=? WHERE id=?"
                  `(,new-title ,id)))

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

;; Idea here: have a list of extensions that get renamed into
;; `mir-default-file-extension' (default of that would be .txt). All
;; the other file formats retain their extension.
(defun mir--get-extension-to-current-buffer ()
  "Returns the extension of the currently opened file as a string with a
leading period."
  (concat "." (file-name-extension (buffer-file-name))))

(defun mir--add-extract (text)
  (let* ((extension (if mir-inherit-extension-for-extracts
                        (mir--get-extension-to-current-buffer)
                      mir-default-file-extension))
         (title (mir-ask-for-title))
         (parent-sequence (denote-retrieve-filename-signature buffer-file-name))
         (parent-keywords (denote-extract-keywords-from-path buffer-file-name))
         (file-name (mir--format-file-name title parent-keywords extension 'child parent-sequence))
         (file-id (denote-extract-id-from-string file-name)))
    ;; maybe randomly subtract priority vals?
    (mir--add-extract-to-db file-id (nth 1 mir--current-topic) title)
    (write-region text nil file-name)))

(defun mir--archive-topic (topic)
  (let* ((id (car topic))
         (denote-directory mir-archive-directory)
         (denote-save-buffers t)
         (denote-kill-buffers t)
         (denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}[0-9]*\\)")
         (topic-file-path (denote-get-path-by-id id))
         (old-keywords (denote-extract-keywords-from-path topic-file-path))
         (new-keywords (add-to-list 'old-keywords "archive")))
    (denote-rename-file topic-file-path
                        'keep-current
                        new-keywords
                        'keep-current
                        'keep-current)
    (mir--archive-topic-db id)))

(defun mir--refresh-current-topic ()
  "Fetches the current topic from the database again, refreshing
`mir--current-topic'."
  (when-let* ((id (car mir--current-topic)))
    (setq mir--current-topic
          (car
           (sqlite-select (mir--get-db)
                          "SELECT * FROM topics WHERE id = ?"
                          `(,id))))))

;;;; Footer

(provide 'mir)

;;; package-name.el ends here
