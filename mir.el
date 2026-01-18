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

;;;; Usage

;; mir saves all the reading material you import into
;; `mir-archive-directory', so make sure that it is configured to the
;; folder you like. Afterwards, use any of these commands to import
;; the reading material:

;; `mir-import-file': Select and import an existing file.
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

(defcustom mir-query-function #'mir-get-topics-up-to-today-by-priority
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
When set to a non-nil value, the user will see the currently open buffer
be dismissed at the end of a reading session (when there's no more
topics in the queue).")

(defcustom mir-default-file-extension ".txt"
  "The file extension of newly imported topics.")

(defcustom mir-inherit-extension-for-extracts nil
  "Whether extracts of a topic should inherit the file extension of the
current buffer. This can be handy when processing data in a specific
format but can also cause problems with files that have special
rendering such as epubs or pdfs.")

(defcustom mir-save-webpage-as 'html
  "How a webpage should be imported into mir. Potential options:

\\='html: Import as a naked HTML page, similar to saving a web page in
the browser specifying \"HTML only\".

\\='singlefile: Download the complete webpage as HTML, preserving
images, styling and all else. Requires single-file-cli.

\\='txt: Extract only the readable text on the webpage and save it as a
plaintext file.")

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
(defun mir-extract-or-import-from-region (&optional prefix)
  "Import the selected region as a new topic. If the region is in an
existing topic, the text is extracted as a new descendant.

By default, the extract has the same priority as the current topic. In
order to manually select the priority, call this command with
\\[universal-argument]."
  (interactive "P")
  (if-let* (((region-active-p))
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
      ;; if we're somehow in an existing mir topic, do an extract.
      (if mir-topic-minor-mode
          (progn
            (mir--add-extract text prefix)
            (mir--extract-lower-parent-priority
             (car mir--current-topic)
             (nth 1 mir--current-topic)))
        (mir-import
         text
         (mir-ask-for-priority)
         (mir-ask-for-title)))
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
  (mir-import text priority title mir-default-file-extension))

;;;###autoload
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

(defun mir-import-url (url)
  "Import URL into mir as a new topic."
  (interactive "sURL: ")
  (unless (org-url-p url)
    (user-error "Invalid URL, check for typos and try again."))
  (cond
   ((eq mir-save-webpage-as 'html)
    (mir-import-webpage-html url))
   ((eq mir-save-webpage-as 'singlefile)
    (mir-import-webpage-singlefile url))
   ((eq mir-save-webpage-as 'txt)
    (mir-import-webpage-txt url))
   (t
    (user-error "`mir-save-webpage-as' has an invalid value, aborting."))))

;;;###autoload
(defun mir-read ()
  "Start a reading session or show the current topic. Use `mir-read-next'
to advance to a new topic."
  (interactive)
 (if-let* ((next-topic (mir-queue-next)))
        (mir-show-topic next-topic)
   (user-error "%s" "Queue is empty for today: nothing to read")))

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
                                      '("Due" 10 t)
                                      '("Added" 11 t)
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
             (due (nth 10 topic))
             (added (nth 4 topic))
             (last-read (or (nth 5 topic) ""))
             (reviews (number-to-string (nth 6 topic)))
             (archived (number-to-string (nth 7 topic)))
             (archived-date (or (nth 8 topic) "")))
         (list id (vector
                   id title priority a-factor interval
                   due added last-read reviews archived
                   archived-date))))
     topics)))

;; How to make this work for multiple topics?
(defun mir-topics-change-priority ()
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

(defun mir-reschedule (date)
  "Reschedule the current topic for a later date."
  (interactive
   (list (org-read-date nil)))
  (let ((id (car mir--current-topic)))
    (mir--update-due-db id date)))

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

(defun mir-import (text priority title &optional extension)
  ;; TODO: ability to add tags/keywords
  (let* ((file-name (mir--format-file-name
                     title
                     nil
                     (or extension mir-default-file-extension)
                     'parent))
         (file-id (denote-extract-id-from-string file-name)))
    (mir--add-topic-to-db file-id priority title)
    (write-region text nil file-name)))

(defun mir-get-topics-up-to-today-by-priority ()
  "Returns a list of topics due to review up to today, sorted by priority."
  (sqlite-select (mir--get-db)
                 "SELECT *, julianday('now', 'localtime') - julianday(due) AS days_delta FROM topics WHERE days_delta > 0 AND archived = 0 ORDER BY priority ASC;"))

;; --- mir-minor-mode things

(defcustom mir-keymap-prefix "C-c C-."
  "The prefix for `mir-topic-minor-mode' key bindings"
  :type 'string)

(defun mir--key (key)
  "Returns a valid keybinding for mir."
  (kbd (concat mir-keymap-prefix " " key)))

(defcustom mir-modeline-items '(priority ordinal)
  "What items to include in the modeline shown with `mir-topic-minor-mode'.
This variable is a list which may be populated with the following
symbols:

priority: show the item's priority value (rounded to 2 decimal points).

ordinal: show the item's position in the queue. For instance, if the
item is second in the queue, this will show \"(2)\".")

(define-minor-mode mir-topic-minor-mode
  "A minor mode for mir topics."
  :lighter " mir"
  ;; TODO modeline indicator for priority
  ;; Can use minor-mode-alist for this ^
  ;; :keymap (list (cons (mir--key "e") #'example))
  )

;; --- end mir-minor-mode things

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
             (run-hooks 'mir-show-topic-hook)
             (mir-topic-minor-mode))
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

(defun mir-queue-next ()
  "Return the next item in the queue or nil otherwise."
  (mir-update-queue)
  (when mir-queue
      (pop mir-queue)))

(defun mir--webpage-html-callback (status url dir)
  (if-let* ((dl-error (plist-get status :error)))
      (user-error "Download failed with error %s" dl-error)
    (message url)))

;; (mir-import-webpage-html "https://supermemo.guru")

(defun mir-import-webpage-html (url)
  "Import webpage located at URL as a plain HTML file. May not include any
CSS, fonts or other media."
  (let ((dir (temporary-file-directory)))
    (with-current-buffer (url-retrieve-synchronously url)
      (when (= (buffer-size) 0)
        (user-error "Could not retreive URL, check for typos and try again."))
      (set-buffer-multibyte t)
      (goto-char (point-min))
      ;; Headers and response body are separated by an empty line.
      (re-search-forward "^$")
      (let* ((html-beg (point))
             (title-beg (re-search-forward "<title[^>]*>"))
             (title-end
              (progn
                (re-search-forward "</title>")
                (match-beginning 0))))
        (mir-import
         (string-trim (buffer-substring-no-properties html-beg (point-max)))
         (mir-ask-for-priority)
         (buffer-substring-no-properties title-beg title-end)
         ".html")))))

(defcustom mir-singlefile-args '()
  "A list of arguments for single-file-cli for saving web pages to mir.")

(defun mir-import-webpage-singlefile (url)
  "Import webpage located at URL as an HTML file using single-file-cli. Use
`mir-singefile-args' to configure the command line arguments for the
single-file command."
  (let ((default-directory (temporary-file-directory)))
    (shell-command
     (concat
      "single-file "
      (mapconcat 'identity mir-singlefile-args " ")
      " "
      url))
    ;; find the downloaded file and import
    ))

;; (mir-import-webpage-singlefile "https://test.url")

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
                          "title TEXT, " "due TEXT NOT NULL "
                          ") STRICT;"))
 (sqlite-execute (mir--get-db)
                 "CREATE INDEX IF NOT EXISTS idx_topics_archived ON topics(archived);")
 (sqlite-execute (mir--get-db)
                 "CREATE INDEX IF NOT EXISTS idx_topics_priority_id ON topics(priority, id);")
 (sqlite-execute (mir--get-db)
                 "CREATE INDEX IF NOT EXISTS idx_topics_due_id ON topics(due, id);")
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

(defun mir--count-active-topics-db ()
  "Return a count of active (non-archived) topics in the database."
  (car (car (sqlite-select (mir--get-db)
                           "SELECT COUNT(*) FROM topics WHERE archived = 0;"))))

(defun mir--extract-lower-parent-priority (id old-priority)
  "Lower the priority of a parent topic with ID. This is meant to be done
after extracting something out into its own topic as a means to
encourage seeing the extracts more than the bigger parent topic. The new
priority is calculated such that the parent comes 4 topics after it was
supposed to."
  (let* ((n-topics (mir--count-active-topics-db))
         (delta (/ 100.0 n-topics))
         (new-priority (+ old-priority (* delta 4))))
    (mir--update-priority-db id new-priority)))

(defun mir--add-topic-to-db (id priority title &optional is-extract)
  (mir--init-db)
  (sqlite-execute (mir--get-db)
                    "INSERT INTO topics (id, priority, a_factor, interval, due, added, times_read, archived, title) VALUES(?, ?, ?, ?, date(julianday('now', 'localtime') + ?), datetime('now', 'localtime'), 0, 0, ?)"
                    `(,id
                      ,priority
                      ,mir-default-a-factor
                      ,mir-default-topic-interval
                      ,mir-default-topic-interval
                      ,title))
  (mir--rescale-priority-values-db))

(defun mir--add-extract-to-db (id priority title)
  (mir--init-db)
  (sqlite-execute (mir--get-db)
                  "INSERT INTO topics (id, priority, a_factor, interval, due, added, last_review, times_read, archived, title) VALUES(?, ?, ?, ?, date(julianday('now', 'localtime') + ?), datetime('now', 'localtime'), datetime('now', 'localtime'), 1, 0, ?)"
                  `(,id
                    ,priority
                    ,mir-default-a-factor
                    ,mir-default-extract-interval
                    ,mir-default-extract-interval
                    ,title))
  (mir--rescale-priority-values-db))

(defun mir--archive-topic-db (id)
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET last_review = datetime('now', 'localtime'), archived = 1, archived_date = datetime('now', 'localtime') WHERE id = ?;" `(,id))
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
    ;; one thing to note here: due to using `julianday()', the
    ;; resulting day may be 1 day later than the interval calculates
    ;; when `new-interval' is a non-integer (which, in most cases, it
    ;; should be).
    (sqlite-execute (mir--get-db)
                    "UPDATE topics SET last_review = datetime('now', 'localtime'), a_factor=?, interval=?, due=date(julianday('now', 'localtime') + ?), times_read=? WHERE id=?;"
                    `(,new-af ,new-interval ,new-interval ,new-rt ,id))
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

(defun mir--update-due-db (id due-date)
  (sqlite-execute (mir--get-db)
                  "UPDATE topics SET due=? WHERE id=?"
                  `(,due-date ,id)))

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

;; Idea here: have a list of extensions that get renamed into
;; `mir-default-file-extension' (default of that would be .txt). All
;; the other file formats retain their extension.
(defun mir--get-extension-to-current-buffer ()
  "Returns the extension of the currently opened file as a string with a
leading period."
  (concat "." (file-name-extension (buffer-file-name))))

(defun mir--add-extract (text prefix)
  (let* ((extension (if mir-inherit-extension-for-extracts
                        (mir--get-extension-to-current-buffer)
                      mir-default-file-extension))
         (title (mir-ask-for-title))
         (parent-sequence (denote-retrieve-filename-signature buffer-file-name))
         (parent-keywords (denote-extract-keywords-from-path buffer-file-name))
         (file-name (mir--format-file-name title parent-keywords extension 'child parent-sequence))
         (file-id (denote-extract-id-from-string file-name)))
    (mir--add-extract-to-db file-id
                            (if prefix
                                (mir-ask-for-priority)
                              (nth 1 mir--current-topic))
                            title)
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

;;;; Footer

(provide 'mir)

;;; package-name.el ends here
